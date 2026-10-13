(ns dk.simongray.mind-meld
  "A CRDT of plain maps, which devices merge in any order without conflict.

  A state is a map of collections, each a map of entries, maybe nested by
  more keys, with the :device that the state belongs to and its :clock. An
  entry is stamped with :updated-at and :device, and a removed entry is a
  tombstone with :removed?. A field-wise entry, which write-fields writes,
  also has a stamp for each of its fields under :stamps.

  A schema maps the key of each collection to its options:

  - :depth, the number of keys that lead to each entry
  - :compact?, to drop its tombstones once every device has seen them
  - :count-key, the key of a count in each entry, which only grows

      {:notes    {:depth 1 :compact? true}
       :listened {:depth 3 :count-key :seconds}}

  The registry of devices, under :devices and :retired, needs no schema.

  The comments in the code name the source of each part, and where it
  departs from it:

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
  (:refer-clojure :exclude [merge remove])
  (:require [dk.simongray.mind-meld.check :as check]
            [dk.simongray.mind-meld.entry :as entry]
            [dk.simongray.mind-meld.registry :as registry]
            [dk.simongray.mind-meld.tree :as tree]))

(def ^:no-doc header
  "The keys of a state that hold no entries: its device and its clock."
  #{:device :clock})

(defn later-inst
  "The later of the instants `a` and `b`, either of which can be nil."
  [a b]
  (entry/later-inst a b))

(defn stamp-ms
  "The stamp of the entry `e` in milliseconds, 0 for an entry without one."
  [e]
  (entry/stamp-ms e))

(defn fields
  "The fields of the entry `e`, as an app reads them: without its stamps,
  the stamps of its fields and its removal."
  [e]
  (entry/fields e))

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
       (clojure.core/remove (comp :removed? val))
       (sort-by (comp stamp-ms val) >)
       (mapv key)))

(defn later
  "The later of the stamped entries `a` and `b`. At the same instant, the
  greater device id wins, and then the greater printed entry, so that the
  winner is the same on every device and platform."
  [a b]
  (entry/later a b))

;; A hybrid logical clock (Kulkarni et al. 2014). Deviation: the next
;; millisecond stands in for its logical counter, so a burst of writes moves
;; the stamps ahead of the time, which at-once avoids within one call.
(defn after
  "The instant to stamp a write to `state` at `at` with: `at`, or the
  millisecond after the state's clock when that's later."
  [state at]
  (entry/->inst (cond-> (inst-ms at)
                  (:clock state) (max (inc (inst-ms (:clock state)))))))

(defn- join-entries
  "The merge of the stamped entries `a` and `b`: field by field when
  either is field-wise, or else the later of the two."
  [a b]
  (cond
    ;; most entries of two states agree
    (= a b)
    a

    (or (entry/field-wise? a) (entry/field-wise? b))
    (entry/join-fields a b)

    :else
    (later a b)))

(defn- stamped
  "The entry `e` replaced by the entry `new`, stamped at `at` by `device`:
  field by field when `fields?` or `e` is field-wise, or else whole."
  [e new at device fields?]
  (if (or fields? (entry/field-wise? e))
    (entry/stamp-fields e new at device)
    (entry/stamp new at device)))

(defn- written
  "What `f` gives for the entry `e` at `path`: a map, or nil for none. It
  throws for anything else, e.g. a string from (constantly \"dark\")."
  [f e path]
  (let [new (f e)]
    (when-not (or (nil? new) (map? new))
      (throw (ex-info (str "A write to " (pr-str path)
                           " gave something other than a map or nil")
                      {:path path :value new})))
    new))

(defn- write-entries
  "The `state` with each of the `writes`, pairs of a path and a function,
  made at `at` with one stamp, field by field when `fields?`."
  [state writes at fields?]
  (let [foreign?  (contains? state ::author)
        device    (or (::author state) (:device state))
        at        (if foreign?
                    (entry/->inst (inst-ms at))
                    (or (::at state) (after state at)))
        put       (if foreign? join-entries (fn [_ new] new))
        write-one (fn [s [path f]]
                    (update-in s path #(put % (stamped % (written f % path)
                                                        at device fields?))))]
    (-> (reduce write-one state writes)
        (assoc :clock (later-inst (:clock state) at)))))

(defn write-each
  "The `state` with each of the `writes`, pairs of a path and a function,
  made at `at` with one stamp: the entry at the path is replaced by what
  the function returns for it, field by field when it's field-wise, as
  write-fields writes it."
  [state writes at]
  (write-entries state writes at false))

(defn write
  "The `state` with the entry at `path` replaced by what `f` returns for
  it, or for nil when there's none. It's stamped at `at`, or just after
  the state's clock when that's later. An entry that write-fields wrote
  stays field-wise."
  [state path f at]
  (write-entries state [[path f]] at false))

(defn write-fields
  "The `state` with the entry at `path` replaced by what `f` returns for
  it, field by field, at `at` as write stamps it: each field that changed,
  or that went, gets the stamp, so that a merge keeps the edits of two
  devices to different fields of one entry, e.g. the title of a note on
  one device and its body on another. A removal is still of the whole
  entry, by :removed?.

      (write-fields state [:notes \"n1\"] #(assoc % :title \"Milk\") at)"
  [state path f at]
  (write-entries state [[path f]] at true))

(defn remove
  "The `state` with the entry at `path` removed at `at`, as write stamps it:
  a tombstone, marked :removed?, so that an older copy of the entry can't
  bring it back. A tombstone of an entry that isn't field-wise keeps its
  fields."
  [state path at]
  (write state path #(assoc % :removed? true) at))

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

(defn merge
  "Merge the states `a` and `b` of two devices by the `schema`, key by key,
  by the shape of each value:

  - a stamped entry is a last-writer-wins register, and the later entry
    wins whole, even when it's a tombstone, but in a collection with a
    :count-key, the higher count wins
  - a field-wise entry, which write-fields writes, merges field by field,
    each field a register, and it's removed when the later of the two
    entries is
  - a map merges key by key, down to the entries
  - an instant is a max register
  - any other value is that of `a`

  The merge is commutative, associative and idempotent, so it's safe to
  apply in any order and any number of times."
  [schema a b]
  (let [pick-fn (fn [k]
                  (if-let [count-key (get-in schema [k :count-key])]
                    (partial entry/higher count-key)
                    join-entries))
        ks      (disj (into (set (keys a)) (keys b)) :device)]
    (into {:device (:device a)}
          (map (fn [k] [k (tree/join (pick-fn k) (get a k) (get b k))]))
          ks)))

;; A G-Counter is the sum of the counts of its devices (Shapiro et al. 2011,
;; section 3.1.2)
(defn sum
  "The sum of the counts of the live entries right under `path` in `state`,
  under the :count-key of their collection in `schema`, e.g. of a count
  with an entry for each device."
  [schema state path]
  (let [count-key (get-in schema [(first path) :count-key])]
    (when-not count-key
      (throw (ex-info "The collection has no :count-key"
                      {:collection (first path)})))
    (transduce (comp (filter live?)
                     (map count-key)
                     (filter number?))
               +
               0
               (vals (get-in state path)))))

(defn problem
  "A description of what in `state` breaks the rules below, given the
  `schema`, or nil. A bad value from another device spreads to every
  device, where it makes a function throw, or the devices disagree.

  - The state and its collections are maps, and the clock is an instant.
  - Besides the device and the clock, the state holds only maps, down to
    stamped entries.
  - Each entry of a collection has a stamp, which is an instant, and so
    has each field of a field-wise entry.
  - No entry of a collection with a :count-key is field-wise.
  - What a device has seen is a map of instants."
  [schema state]
  (if (map? state)
    (let [schema (clojure.core/merge schema registry/schema)]
      (some #(% state)
            [check/clock-problem
             #(check/collection-problem schema %)
             #(check/value-problem [] (apply dissoc % header))
             #(check/stamp-problem [] %)
             check/seen-problem]))
    "the state is not a map"))

(defn schema-problem
  "A description of what in `schema` breaks the rules below, or nil. A bad
  schema can make a function throw, or an option do nothing, e.g. one with
  a misspelt name.

  - The schema is a map of the key of each collection to its options.
  - The options are a map with a :depth, a whole number from 0.
  - The only options are :depth, :compact?, :count-key and :ranked?.
  - No collection is under :device, :clock, :devices or :retired, which
    the state holds itself."
  [schema]
  (check/schema-problem schema (into header (keys registry/schema))))

;; A delta-state (Almeida, Shoker and Baquero 2018), found by comparing two
;; states rather than made by each write.
(defn changes-since
  "The part of `state` that isn't in the earlier state `previous`, or all
  of it when `previous` is nil, e.g. for a service that takes changes
  rather than whole states. It holds every entry written or merged since,
  however early its stamp."
  [state previous]
  (let [entries (apply dissoc state header)]
    (into (select-keys state header) (tree/changed-part entries previous))))

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
  "Retire `device` from `state` at `at`. Any device can retire another,
  and a retired device can't come back by describing itself again."
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

(defn acknowledge
  "Note in the registry of `state` at `at` that its device has merged the
  whole `states`, as the latest stamp of each other device under :seen.

  Only whole states count, never the changes that a service sends. The
  device's entry is only written when :seen grows, so that a sync with
  nothing new leaves the state as it was."
  [state states at]
  (let [me     (:device state)
        known  (get-in state [:devices me :seen] {})
        merged (apply merge-with later-inst known
                      (map registry/latest-by-author states))
        seen   (dissoc merged me)]
    (if (and (= seen known)
             (get-in state [:devices me]))
      state
      (write state [:devices me] #(assoc % :seen seen) at))))

(def default-keep-for
  "How long to keep a tombstone that every device has seen, in
  milliseconds, so that a device that joins later with a state of its own
  still gets it."
  (* 30 24 60 60 1000))

(defn compact
  "The `state` at `at` without the tombstones of the collections that the
  `schema` marks :compact?, once every device that isn't retired has
  merged them and the :keep-for of `opts` has passed.

  The :keep-for is in milliseconds, 30 days by default. A device that
  stopped syncing holds this back until it's retired, and the tombstones
  of an author outside the registry stay. A change from such an author
  with an earlier stamp than a tombstone that was dropped brings its entry
  back, so the :keep-for should be longer than its changes can be late."
  ([schema state at]
   (compact schema state at {}))
  ([schema state at {:keys [keep-for] :or {keep-for default-keep-for} :as opts}]
   {:pre [(map? opts)]}
   (let [live       (devices state)
         droppable? (fn [e]
                      (and (:removed? e)
                           (<= (+ (inst-ms (:updated-at e)) keep-for)
                               (inst-ms at))
                           (registry/seen-by-all? state live e)))
         compact-in (fn [s k {:keys [compact?]}]
                      (cond-> s
                        (and compact? (map? (get s k)))
                        (update k tree/remove-entries droppable?)))]
     (reduce-kv compact-in state schema))))

(defn lagging
  "The devices of `state` that aren't retired and haven't merged
  everything its device has, as a map of id to the stamp up to which each
  has merged everything, or nil. They keep tombstones from being dropped
  until they catch up or are retired."
  [state]
  (let [me   (:device state)
        seen (get-in state [:devices me :seen] {})
        mine (assoc seen me (get (registry/latest-by-author state) me))]
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
