(ns ^:no-doc dk.simongray.mind-meld.check
  "Checks of a state from another device: each describes the first value
  that breaks its rule, or gives nil."
  (:require [dk.simongray.mind-meld.entry :as entry]))

(defn clock-problem
  "A description of the clock of `state` when it isn't an instant, or nil."
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

    (not (entry/stamped? v))
    (str "the entry at " (pr-str path) " is not stamped")))

(defn collection-problem
  "A description of the first collection of `state` in the `schema` that
  isn't a map, or of the first entry in one that isn't a stamped map."
  [schema state]
  (some (fn [[k {:keys [depth]}]]
          (when-some [entries (get state k)]
            (if (map? entries)
              (entry-problem [k] depth entries)
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

(defn stamp-problem
  "A description of the first entry in `v`, at `path`, whose stamp isn't
  an instant."
  [path v]
  (cond
    (entry/stamped? v)
    (when-not (inst? (:updated-at v))
      (str "the entry at " (pr-str path) " is stamped "
           (pr-str (:updated-at v)) ", not an instant"))

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
