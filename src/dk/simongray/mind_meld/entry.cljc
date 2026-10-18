(ns ^:no-doc dk.simongray.mind-meld.entry
  "Stamped entries, each a last-writer-wins register, and which of two
  wins."
  (:require [clojure.walk :as walk]))

(defn ->inst
  "The instant `ms` milliseconds after the epoch."
  [ms]
  #?(:clj  (java.util.Date. (long ms))
     :cljs (js/Date. ms)))

(defn stamp
  "The entry `e` stamped as written at `at` by `device`."
  [e at device]
  (assoc e :updated-at at :device device))

(defn stamped?
  "Whether `x` is a stamped entry."
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
  "The later of the stamped entries `a` and `b`. At the same instant, the
  greater device id wins, and then the greater printed entry, so that the
  winner is the same on every device and platform."
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

(def bookkeeping
  "The keys of an entry that aren't its fields."
  #{:updated-at :device :removed? :stamps :cleared})

(defn field-wise?
  "Whether `e` is an entry whose fields each have a stamp of their own."
  [e]
  (contains? e :stamps))

(defn fields
  "The fields of the entry `e`, without its stamps and its removal."
  [e]
  (apply dissoc e bookkeeping))

(defn- order-key
  "What orders the stamps, maps of :updated-at and :device: the instant,
  and then the device."
  [{:keys [updated-at device]}]
  [(if updated-at (inst-ms updated-at) 0) (str device)])

(defn- after?
  "Whether the stamp `s` comes after the stamp `t`, which can be nil."
  [s t]
  (or (nil? t)
      (pos? (compare (order-key s) (order-key t)))))

(defn- later-stamp
  "The later of the stamps `s` and `t`, either of which can be nil."
  [s t]
  (if (after? s t) s t))

(defn cleared
  "The stamp of the last removal of the entry `e`, a map of :updated-at and
  :device, or nil: its :cleared, or else its own stamp when it's a
  tombstone that isn't field-wise."
  [e]
  (or (:cleared e)
      (when (:removed? e)
        (select-keys e [:updated-at :device]))))

(defn field-stamps
  "The stamp of each field of the entry `e` that came after its last
  removal, a map of :updated-at and :device: the stamp under :stamps, or
  else that of the entry, e.g. for each field of an entry that isn't
  field-wise."
  [e]
  (let [own (select-keys e [:updated-at :device])
        c   (cleared e)]
    (into {}
          (filter #(after? (val %) c))
          (into (zipmap (keys (fields e)) (repeat own)) (:stamps e)))))

;; A removal ends every field, and the entry keeps its stamp as :cleared,
;; so that a field written before it never comes back: an edit after it, on
;; a device that hadn't seen it, brings back only what was written after the
;; removal, whether the tombstone is still there or compaction dropped it.
;; Riak's map resets a removed field in the same way.
(defn stamp-fields
  "The entry `e` with the fields of the map `new` and its removal, stamped
  as written at `at` by `device`: each field whose value changed or that
  went gets that stamp, and the rest keep their own. When `new` is
  removed, every field goes."
  [e new at device]
  (let [stamp    {:updated-at at :device device}
        removed? (:removed? new)
        old      (fields e)
        now      (if removed? {} (fields new))
        changed  (remove #(= (find old %) (find now %))
                         (into (set (keys old)) (keys now)))]
    (cond-> (assoc now
                   :updated-at at
                   :device     device
                   :stamps     (if removed?
                                 {}
                                 (into (field-stamps e)
                                       (map #(vector % stamp))
                                       changed)))
      removed?                         (assoc :removed? true :cleared stamp)
      (and (not removed?) (cleared e)) (assoc :cleared (cleared e)))))

;; The fields of a field-wise entry are each an LWW-Register (Shapiro et al.
;; 2011, section 3.2.1), as the values of Riak's and Automerge's maps are.
;; Deviation: those keep an update that's concurrent with a removal, which
;; they know from causality. Without causality, the removal is a register of
;; its own, which every write sets, so the later of an edit and a removal
;; wins, as for the entries of a collection.
(defn- field
  "The field `k` of the entry `e`, whose stamp is `s`, as a stamped
  register, with its :value unless it went, or nil without a stamp."
  [e s k]
  (when s
    (cond-> s
      (contains? e k) (assoc :value (get e k)))))

(defn- presence-key
  "What orders the removals of two entries: the stamp, the device, and a
  removal before a write at the same stamp by the same device."
  [e]
  [(stamp-ms e) (str (:device e)) (boolean (:removed? e))])

(defn join-fields
  "The merge of the entries `a` and `b`, of which at least one is
  field-wise: each field is the later of its two unless the later removal
  of the two ended it, and the entry is removed when the later of the two
  entries is."
  [a b]
  (let [c      (later-stamp (cleared a) (cleared b))
        sa     (field-stamps a)
        sb     (field-stamps b)
        picked (keep (fn [k]
                       (let [f (later (field a (get sa k) k)
                                      (field b (get sb k) k))]
                         (when (after? f c) [k f])))
                     (into (set (keys sa)) (keys sb)))
        top    (if (pos? (compare (presence-key a) (presence-key b))) a b)]
    (cond-> (into {:updated-at (:updated-at top)
                   :device     (:device top)
                   :stamps     (into {}
                                     (map (fn [[k s]]
                                            [k (select-keys s [:updated-at
                                                               :device])]))
                                     picked)}
                  (keep (fn [[k s]] (when (contains? s :value) [k (:value s)])))
                  picked)
      (:removed? top) (assoc :removed? true)
      c               (assoc :cleared c))))

(defn stamps
  "The stamps in the entry `e`, each a map of :updated-at and :device: its
  own, those of its fields and that of its last removal."
  [e]
  (cond-> [(select-keys e [:updated-at :device])]
    (:stamps e)  (into (vals (:stamps e)))
    (:cleared e) (conj (:cleared e))))

(defn- shift
  "The stamp `s` moved `ms` milliseconds later when `pred` holds for it."
  [s pred ms]
  (if (pred s)
    (update s :updated-at #(->inst (+ (inst-ms %) ms)))
    s))

(defn shifted
  "The entry `e` with each of its stamps that `pred` holds for moved `ms`
  milliseconds later."
  [e pred ms]
  (cond-> (into e (shift (select-keys e [:updated-at :device]) pred ms))
    (:stamps e)  (update :stamps update-vals #(shift % pred ms))
    (:cleared e) (update :cleared shift pred ms)))

;; A G-Counter (Shapiro et al. 2011, section 3.1.2) with an entry per device,
;; ordered as a lexicographic product of count and stamp (Almeida 2024,
;; figure 10). Like any G-Counter, it needs one writer per device id.
(defn higher
  "Of the entries `a` and `b`, which hold a count under the key `count-key`,
  the one with the higher count, or else the later. A count only grows, so
  the higher count includes the lower one."
  [count-key a b]
  (let [count-of #(let [n (get % count-key)] (if (number? n) n 0))]
    (if (== (count-of a) (count-of b))
      (later a b)
      (max-key count-of a b))))
