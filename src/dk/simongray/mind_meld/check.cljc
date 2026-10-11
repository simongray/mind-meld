(ns ^:no-doc dk.simongray.mind-meld.check
  "Checks of a state from another device, and of a schema: each describes
  the first value that breaks its rule, or gives nil."
  (:require [dk.simongray.mind-meld.entry :as entry]))

(defn clock-problem
  "A description of the clock of `state` when it isn't an instant, or nil."
  [state]
  (when-some [clock (:clock state)]
    (when-not (inst? clock)
      (str "the clock is " (pr-str clock) ", not an instant"))))

(defn- entry-problem
  "A description of the first value in `v` at `path` that isn't a map, of
  the first entry without a stamp, or of a field-wise entry when the
  collection is `counting?`, whose counts merge whole. The entries are
  `depth` keys below `v`."
  [path depth counting? v]
  (cond
    (not (map? v))
    (str "the entry at " (pr-str path) " is not a map")

    (pos? depth)
    (some (fn [[k x]] (entry-problem (conj path k) (dec depth) counting? x))
          v)

    (not (entry/stamped? v))
    (str "the entry at " (pr-str path) " is not stamped")

    (and counting? (entry/field-wise? v))
    (str "the entry at " (pr-str path)
         " is field-wise in a collection that counts")))

(defn collection-problem
  "A description of the first collection of `state` in the `schema` that
  isn't a map, or of the first entry in one that isn't a stamped map, or
  that's field-wise where a count merges whole."
  [schema state]
  (some (fn [[k {:keys [depth count-key]}]]
          (when-some [entries (get state k)]
            (if (map? entries)
              (entry-problem [k] depth (some? count-key) entries)
              (str k " is not a map"))))
        schema))

(defn value-problem
  "A description of the first value in `v`, at `path`, that is neither a
  map nor in a stamped entry. Each device keeps its own such value, so the
  devices never agree on it."
  [path v]
  (cond
    (or (nil? v) (entry/stamped? v))
    nil

    (map? v)
    (some (fn [[k x]] (value-problem (conj path k) x)) v)

    :else
    (str "the value at " (pr-str path) " is not in a stamped entry")))

(defn- fields-problem
  "A description of the stamps of the fields of the entry `e` at `path`
  when they aren't a map of each field to a map with an instant as its
  :updated-at, or of its last removal when that isn't such a map, or nil."
  [path e]
  (let [stamps  (:stamps e)
        stamp?  #(and (map? %) (inst? (:updated-at %)))]
    (cond
      (not (and (map? stamps) (every? stamp? (vals stamps))))
      (str "the fields of the entry at " (pr-str path)
           " are not stamped with instants")

      (and (contains? e :cleared) (not (stamp? (:cleared e))))
      (str "the last removal of the entry at " (pr-str path)
           " is not stamped with an instant"))))

(defn stamp-problem
  "A description of the first entry in `v`, at `path`, whose stamp isn't
  an instant, or whose fields aren't stamped with instants when it's
  field-wise."
  [path v]
  (cond
    (entry/stamped? v)
    (cond
      (not (inst? (:updated-at v)))
      (str "the entry at " (pr-str path) " is stamped "
           (pr-str (:updated-at v)) ", not an instant")

      (entry/field-wise? v)
      (fields-problem path v))

    (map? v)
    (some (fn [[k x]] (stamp-problem (conj path k) x)) v)

    :else
    nil))

(defn seen-problem
  "A description of the first device in the registry of `state` whose
  :seen isn't a map of instants, or nil."
  [state]
  (some (fn [[id {:keys [seen]}]]
          (when-not (or (nil? seen)
                        (and (map? seen) (every? inst? (vals seen))))
            (str "what device " (pr-str id)
                 " has seen is not a map of instants")))
        (:devices state)))

(def options
  "The options of a collection in a schema."
  #{:depth :compact? :count-key :ranked?})

(defn- options-problem
  "A description of what in the options `opts` of the collection `k`
  breaks a rule, or nil."
  [k opts]
  (cond
    (not (map? opts))
    (str "the options of " k " are not a map")

    (not (nat-int? (:depth opts)))
    (str "the :depth of " k " is " (pr-str (:depth opts))
         ", not a whole number from 0")

    :else
    (when-some [unknown (first (remove options (keys opts)))]
      (str k " has the unknown option " unknown))))

(defn schema-problem
  "A description of the first collection of `schema` that breaks a rule,
  or nil, where the `reserved` keys are those that a state holds itself."
  [schema reserved]
  (if (map? schema)
    (some (fn [[k opts]]
            (if (contains? reserved k)
              (str k " is a key that the state holds itself")
              (options-problem k opts)))
          schema)
    "the schema is not a map"))
