(ns dk.simongray.mind-meld
  "A CRDT of plain maps, which devices merge in any order without conflict.

  A state is a map of collections, each a map of entries, maybe nested by
  more keys, with the :device that the state belongs to and its :clock. An
  entry is stamped with :updated-at and :device, and a removed entry is a
  tombstone with :removed?.

  A schema maps the key of each collection to its options:

  - :depth, the number of keys that lead to each entry
  - :compact?, to drop its tombstones once every device has seen them
  - :count-key, the key of a count in each entry, which only grows

      {:notes    {:depth 1 :compact? true}
       :listened {:depth 3 :count-key :seconds}}

  The registry of devices, under :devices and :retired, needs no schema.

  The comments name the source of each part, and where it departs from it:

  - Shapiro, Preguiça, Baquero and Zawirski, A comprehensive study of
    Convergent and Commutative Replicated Data Types, INRIA RR-7506, 2011,
    https://inria.hal.science/inria-00555588
  - Almeida, Approaches to Conflict-free Replicated Data Types, ACM
    Computing Surveys 57(2), 2024, https://arxiv.org/abs/2310.18220
  - Johnson and Thomas, The Maintenance of Duplicate Databases, RFC 677,
    1975, https://www.rfc-editor.org/rfc/rfc677
  - Kulkarni, Demirbas, Madappa, Avva and Leone, Logical Physical Clocks,
    OPODIS 2014, https://doi.org/10.1007/978-3-319-14472-6_2
  - Almeida, Shoker and Baquero, Delta State Replicated Data Types,
    Journal of Parallel and Distributed Computing 111, 2018,
    https://arxiv.org/abs/1603.01529"
  (:refer-clojure :exclude [merge])
  (:require [clojure.walk :as walk]))

(defn- ->inst
  "The instant `ms` milliseconds after the epoch."
  [ms]
  #?(:clj  (java.util.Date. (long ms))
     :cljs (js/Date. ms)))

(def ^:no-doc header
  "The keys of a state that hold no entries: its device and its clock."
  #{:device :clock})

(def ^:no-doc registry
  "The schema of the registry: what each device said of itself, under
  :devices, and the retired devices."
  {:devices {:depth 1}
   :retired {:depth 1}})

(defn- stamp
  [state entry at]
  (assoc entry :updated-at at :device (or (::author state) (:device state))))

(defn- stamped?
  [x]
  (and (map? x)
       (contains? x :updated-at)))

(defn later-inst
  "The later of the instants `a` and `b`, either of which can be nil."
  [a b]
  (if (and a b)
    (max-key inst-ms b a)
    (or a b)))

(defn stamp-ms
  "The stamp of the entry `e` in milliseconds, 0 for an entry without one."
  [e]
  (or (some-> (:updated-at e) inst-ms) 0))

;; Each entry is an LWW-Register, so a collection of them is an
;; LWW-element-Set (Shapiro et al. 2011, sections 3.2.1 and 3.3.3), and a
;; removal is a flag on the entry, as in RFC 677.
(defn live?
  "Whether the entry `e` is there and isn't a tombstone."
  [e]
  (and (some? e) (not (:removed? e))))

(defn live-by-stamp
  "The keys of the live entries of the map `entries`, the latest written
  first."
  [entries]
  (->> entries
       (remove (comp :removed? val))
       (sort-by (comp stamp-ms val) >)
       (mapv key)))

;; A hybrid logical clock (Kulkarni et al. 2014). Deviation: the next
;; millisecond stands in for its logical counter, so a burst of writes moves
;; the stamps ahead of the time, which at-once avoids within one call.
(defn after
  "The instant to stamp a write to `state` at `at` with: `at`, or the
  millisecond after the state's clock when that's later."
  [state at]
  (->inst (cond-> (inst-ms at)
            (:clock state) (max (inc (inst-ms (:clock state)))))))

(declare later)

(defn write-each
  "The `state` with each of the `writes`, pairs of a path and a function,
  made at `at` with one stamp: the entry at the path is replaced by what
  the function returns for it."
  [state writes at]
  (let [foreign?  (contains? state ::author)
        at        (if foreign?
                    (->inst (inst-ms at))
                    (or (::at state) (after state at)))
        put       (if foreign? later (fn [_ new] new))
        write-one (fn [s [path f]]
                    (update-in s path #(put % (stamp state (f %) at))))]
    (-> (reduce write-one state writes)
        (assoc :clock (later-inst (:clock state) at)))))

(defn write
  "The `state` with the entry at `path`, or nil, replaced by what `f`
  returns for it. It's stamped at `at`, or just after the state's clock
  when that's later."
  [state path f at]
  (write-each state [[path f]] at))

;; The alternative is the hybrid logical clock of Kulkarni et al. 2014 in
;; full, with a counter in each stamp, so that no series of writes runs ahead
;; of the time. That changes the format of every stamp, and its practical
;; gain was deemed too small.
(defn at-once
  "The `state` that `f` returns for it, with every write in `f` stamped as
  one write at `at`, rather than each a millisecond after the last, e.g.
  for the writes of an import."
  [state at f]
  (if (contains? state ::at)
    (f state)
    (dissoc (f (assoc state ::at (after state at))) ::at)))

;; Deviation: an LWW-Register needs stamps that follow causal order (Shapiro
;; et al. 2011, section 3.2.1), but these are another device's own times,
;; so an act that came later on a slower clock loses.
(defn as-device
  "The `state` that `f` returns for it, with every write in `f` made by
  `device` at exactly the instant given, as if merged from that device,
  e.g. for the changes that a sync service reports."
  [state device f]
  (dissoc (f (assoc state ::author device)) ::author))

(defn identify
  "Describe this device in the registry of `state` at `at`, with a
  `description` such as its :name and :platform, merged into what it
  said before."
  [state description at]
  (write state
         [:devices (:device state)]
         #(clojure.core/merge % description)
         at))

(defn retire
  "Retire `device` from `state` at `at`, from any device. A retired device
  can't come back by describing itself again."
  [state device at]
  (write state [:retired device] identity at))

(defn retired?
  "Whether `device` is retired in `state`."
  [state device]
  (contains? (:retired state) device))

(defn devices
  "The devices of `state` that aren't retired, as a map of id to what each
  said of itself."
  [state]
  (apply dissoc (:devices state {}) (keys (:retired state))))

(defn- latest-by-author
  "The latest stamp of each device among the entries of `state`, outside
  the registry of devices."
  [state]
  (letfn [(walk [m v]
            (cond
              (stamped? v) (update m (:device v) later-inst (:updated-at v))
              (map? v)     (reduce-kv (fn [m _ x] (walk m x)) m v)
              :else        m))]
    ;; or each acknowledgement would be news to acknowledge, and two devices
    ;; would keep writing to each other forever
    (walk {} (apply dissoc state :devices header))))

(defn acknowledge
  "Note in the registry of `state` at `at` that its device has merged the
  whole `states`, as the latest stamp of each other device under :seen.

  Only whole states count, never the changes that a service sends. The
  entry is only written when it grew, so that a sync with nothing new
  leaves the state as it was."
  [state states at]
  (let [me     (:device state)
        known  (get-in state [:devices me :seen] {})
        merged (apply merge-with later-inst known (map latest-by-author states))
        seen   (dissoc merged me)]
    (if (and (= seen known)
             (get-in state [:devices me]))
      state
      (write state [:devices me] #(assoc % :seen seen) at))))

(def default-keep-for
  "How long to keep a tombstone that every device has seen, in
  milliseconds, for a device that joins with a state of its own."
  (* 30 24 60 60 1000))

;; Stability (Shapiro et al. 2011, section 4.1, after Wuu and Bernstein,
;; PODC 1984). Deviation: :seen keeps the latest stamp of each author, not a
;; vector clock, which holds only while an author's entries arrive in the
;; order of their stamps. That's true of the devices in the registry, but
;; not of the authors of as-device, so their tombstones stay.
(defn- seen-by-all?
  "Whether every live device of `state` but the author of the entry `e`
  has merged it, by the registry of devices. It's false without another
  device, or for an author outside the registry."
  [state {:keys [device updated-at] :as e}]
  (let [judges (dissoc (devices state) device)]
    (and (contains? (:devices state) device)
         ;; a lone device gains nothing by forgetting, and a device that
         ;; joins later could bring back what it forgot
         (seq judges)
         (every? (fn [[id {:keys [seen]}]]
                   ;; this device has merged what it holds
                   (or (= id (:device state))
                       (some-> (get seen device)
                               inst-ms
                               (>= (inst-ms updated-at)))))
                 judges))))

(defn compact
  "The `state` at `at` without the tombstones of the collections that the
  `schema` marks :compact?, once every live device has merged them and the
  :keep-for of `opts` has passed.

  The :keep-for is in milliseconds, 30 days by default. A device that
  stopped syncing holds this back until it's retired, and the tombstones
  of an author outside the registry stay."
  ([schema state at]
   (compact schema state at {}))
  ([schema state at {:keys [keep-for] :or {keep-for default-keep-for} :as opts}]
   {:pre [(map? opts)]}
   (let [droppable? (fn [e]
                      (and (:removed? e)
                           (<= (+ (inst-ms (:updated-at e)) keep-for)
                               (inst-ms at))
                           (seen-by-all? state e)))
         sweep      (fn sweep [m]
                      (reduce-kv (fn [m k v]
                                   (cond
                                     (stamped? v)
                                     (cond-> m (droppable? v) (dissoc k))

                                     (map? v)
                                     (if-some [w (not-empty (sweep v))]
                                       (assoc m k w)
                                       (dissoc m k))

                                     :else
                                     m))
                                 m
                                 m))
         compact-in (fn [s k {:keys [compact?]}]
                      (cond-> s
                        (and compact? (map? (get s k))) (update k sweep)))]
     (reduce-kv compact-in state schema))))

(defn lagging
  "The live devices of `state` that haven't merged everything its device
  has, as a map of id to the latest stamp that each merged, or nil. They
  keep tombstones from being dropped until they catch up or are retired."
  [state]
  (let [me   (:device state)
        seen (get-in state [:devices me :seen] {})
        mine (assoc seen me (get (latest-by-author state) me))]
    (into {}
          (for [[id {theirs :seen}] (dissoc (devices state) me)
                :let  [behind (for [[author latest] (dissoc mine id)
                                    :when latest
                                    :let  [t (get theirs author)]
                                    :when (or (nil? t)
                                              (< (inst-ms t) (inst-ms latest)))]
                                t)]
                :when (seq behind)]
            [id (when (every? some? behind)
                  (apply min-key inst-ms behind))]))))

(defn- key-order
  "A comparison of the keys `a` and `b` of mixed types: keywords first, as
  compare orders them, then any other key by its printed form."
  [a b]
  (if (and (keyword? a) (keyword? b))
    (compare a b)
    (let [kind #(if (keyword? %) 0 1)]
      (compare [(kind a) (pr-str a)] [(kind b) (pr-str b)]))))

(defn- float-bits
  "The bits of the float `x` as 16 hex digits, the same on every platform."
  [x]
  #?(:clj  (let [h (Long/toHexString (Double/doubleToLongBits x))]
             (str (subs "0000000000000000" (count h)) h))
     :cljs (let [v   (js/DataView. (js/ArrayBuffer. 8))
                 hex #(.padStart (.toString % 16) 8 "0")]
             (.setFloat64 v 0 x)
             (str (hex (.getUint32 v 0)) (hex (.getUint32 v 4))))))

(defn- whole
  "The float `x` as an integer when it has no fraction and JavaScript holds
  it exactly, or else nil."
  [x]
  (when (and (== x (Math/floor x))
             (< (Math/abs (double x)) 9e15))
    (long x)))

(defn- in-order
  "The value `x` in a form that prints the same on both platforms: maps and
  sets sorted, a float with no fraction as a whole number, and any other
  float as its bits."
  [x]
  (cond
    (map? x)   (into (sorted-map-by key-order) x)
    (set? x)   (into (sorted-set-by key-order) x)
    (float? x) (or (whole x) (tagged-literal 'float (float-bits x)))
    :else      x))

(defn- printed
  "The entry `e` printed the same in Clojure and ClojureScript."
  [e]
  (pr-str (walk/postwalk in-order e)))

(defn- tie-key
  "What orders two entries with the same stamp: the device id of the entry
  `e`, and then the entry printed."
  [e]
  [(str (:device e)) (printed e)])

;; The merge of an LWW-Register (Shapiro et al. 2011, section 3.2.1), with
;; ties broken by device as in RFC 677. Deviation: two entries of one device
;; at one instant, e.g. from as-device, fall back to the printed entry.
(defn later
  "The later of the stamped entries `a` and `b`. On the same instant, the
  greater device id wins, and then the greater printed entry, so that
  every device picks the same one on either platform."
  [a b]
  (cond
    (nil? a)                                 b
    (nil? b)                                 a
    (> (stamp-ms a) (stamp-ms b))            a
    (< (stamp-ms a) (stamp-ms b))            b
    ;; two states that agree tie on every entry, so equal ones aren't printed
    (= a b)                                  a
    (pos? (compare (tie-key a) (tie-key b))) a
    :else                                    b))

;; A G-Counter (Shapiro et al. 2011, section 3.1.2) with an entry per device,
;; ordered as a lexicographic product of count and stamp (Almeida 2024,
;; figure 10). Like any G-Counter, it needs one writer per device id.
(defn- higher
  "Of the entries `a` and `b`, which hold a count under the key `count-key`,
  the one with the higher count, or else the later. A count only grows, so
  the higher count includes the lower one."
  [count-key a b]
  (let [count-of #(let [n (get % count-key)] (if (number? n) n 0))]
    (if (== (count-of a) (count-of b))
      (later a b)
      (max-key count-of a b))))

;; The join of a state-based CRDT (Shapiro et al. 2011, section 2.3.1): maps
;; merge pointwise and the clock by max (Almeida 2024, section 5.1, figure
;; 10). Deviation: any other value is no lattice, so problem names one.
(defn- join
  "The merge of the values `a` and `b` at the same place in two states, by
  their shape, where `pick-fn` picks one of two stamped entries."
  [pick-fn a b]
  (cond
    (nil? a)                  b
    (nil? b)                  a
    (and (map? a) (map? b))   (if (or (stamped? a) (stamped? b))
                                (pick-fn a b)
                                (merge-with (partial join pick-fn) a b))
    (and (inst? a) (inst? b)) (later-inst a b)
    :else                     a))

(defn merge
  "Merge the states `a` and `b` of two devices by the `schema`, key by key,
  by the shape of each value:

  - a stamped entry is a last-writer-wins register, and the later entry
    wins whole, even when it's a tombstone, but in a collection with a
    :count-key, the higher count wins
  - a map merges key by key, down to the entries
  - an instant is a max register
  - any other value is that of `a`

  The merge is commutative, associative and idempotent, so it's safe to
  apply in any order and any number of times."
  [schema a b]
  (let [pick-fn (fn [k]
                  (if-let [count-key (get-in schema [k :count-key])]
                    (partial higher count-key)
                    later))
        ks      (disj (into (set (keys a)) (keys b)) :device)]
    (into {:device (:device a)}
          (map (fn [k] [k (join (pick-fn k) (get a k) (get b k))]))
          ks)))

(defn- clock-problem
  [state]
  (when-some [clock (:clock state)]
    (when-not (inst? clock)
      (str "the clock is " (pr-str clock) ", not an instant"))))

(defn- entry-problem
  "A description of the first value in `v` at `path` that isn't a map, or
  of the first entry without a stamp. The entries are `depth` keys below
  `v`."
  [path depth v]
  (cond
    (not (map? v))
    (str "the entry at " (pr-str path) " is not a map")

    (pos? depth)
    (some (fn [[k x]] (entry-problem (conj path k) (dec depth) x)) v)

    (not (stamped? v))
    (str "the entry at " (pr-str path) " is not stamped")))

(defn- collection-problem
  "A description of the first collection of `state` in the `schema` that
  isn't a map, or of the first entry in one that isn't a stamped map."
  [schema state]
  (some (fn [[k {:keys [depth]}]]
          (when-some [entries (get state k)]
            (if (map? entries)
              (entry-problem [k] depth entries)
              (str k " is not a map"))))
        schema))

(defn- value-problem
  "A description of the first value in `v`, at `path`, that is neither a
  map nor in a stamped entry. Each device keeps its own such value, so the
  devices never agree on it."
  [path v]
  (cond
    (or (nil? v) (stamped? v))
    nil

    (map? v)
    (some (fn [[k x]] (value-problem (conj path k) x)) v)

    :else
    (str "the value at " (pr-str path) " is not in a stamped entry")))

(defn- stamp-problem
  "A description of the first entry in `v`, at `path`, whose stamp isn't
  an instant."
  [path v]
  (cond
    (stamped? v)
    (when-not (inst? (:updated-at v))
      (str "the entry at " (pr-str path) " is stamped "
           (pr-str (:updated-at v)) ", not an instant"))

    (map? v)
    (some (fn [[k x]] (stamp-problem (conj path k) x)) v)

    :else
    nil))

(defn- seen-problem
  [state]
  (some (fn [[id {:keys [seen]}]]
          (when-not (or (nil? seen)
                        (and (map? seen) (every? inst? (vals seen))))
            (str "what device " (pr-str id)
                 " has seen is not a map of instants")))
        (:devices state)))

(defn problem
  "A description of what in `state` breaks the rules below, given the
  `schema`, or nil. A bad value from another device spreads to every
  device, where it makes a function throw, or the devices disagree.

  - The state and its collections are maps, and the clock is an instant.
  - Besides the device and the clock, the state holds only maps, down to
    stamped entries.
  - Each entry of a collection has a stamp, which is an instant.
  - What a device has seen is a map of instants."
  [schema state]
  (if (map? state)
    (some #(% state)
          [clock-problem
           #(collection-problem (clojure.core/merge schema registry) %)
           #(value-problem [] (apply dissoc % header))
           #(stamp-problem [] %)
           seen-problem])
    "the state is not a map"))

(defn- changed-part
  "The part of `v` that isn't in `previous`, the value at the same place in
  an earlier state, or nil when nothing changed."
  [v previous]
  (cond
    (stamped? v)
    (when (not= v previous) v)

    (map? v)
    (not-empty (into {} (keep (fn [[k x]]
                                (some->> (changed-part x (get previous k))
                                         (vector k))))
                     v))

    :else
    nil))

;; A delta-state (Almeida, Shoker and Baquero 2018), found by comparing two
;; states rather than made by each write.
(defn changes-since
  "The part of `state` that isn't in the earlier state `previous`, or all
  of it for nil, e.g. for a service that takes changes rather than whole
  states. It holds every entry written or merged since, however early its
  stamp."
  [state previous]
  (let [entries (apply dissoc state header)]
    (into (select-keys state header) (changed-part entries previous))))

(comment
  (def schema
    {:notes {:depth 1 :compact? true}
     :plays {:depth 2 :count-key :n}})

  (def phone
    (-> {:device "phone"}
        (write [:notes "n1"] (constantly {:text "Buy milk"}) #inst "2026-01-01")
        (write [:plays "song" "phone"] #(update % :n (fnil inc 0))
               #inst "2026-01-01")))

  (def laptop
    (write {:device "laptop"} [:notes "n1"] (constantly {:removed? true})
           #inst "2026-01-02"))

  ;; the later write wins
  (merge schema phone laptop)
  (problem schema (assoc phone :theme "dark"))
  #_.)
