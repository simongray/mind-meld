(ns ^:no-doc dk.simongray.mind-meld.tree
  "Walks of a state through its maps, key by key, down to the stamped
  entries."
  (:require [dk.simongray.mind-meld.entry :as entry]))

;; The join of a state-based CRDT (Shapiro et al. 2011, section 2.3.1): maps
;; merge pointwise and the clock by max (Almeida 2024, section 5.1, figure
;; 10). Deviation: any other value is no lattice, so problem names one.
(defn join
  "The merge of the values `a` and `b` at the same place in two states, by
  their shape, where `pick-fn` picks one of two stamped entries."
  [pick-fn a b]
  (cond
    (nil? a)                  b
    (nil? b)                  a
    (and (map? a) (map? b))   (if (or (entry/stamped? a) (entry/stamped? b))
                                (pick-fn a b)
                                (merge-with (partial join pick-fn) a b))
    (and (inst? a) (inst? b)) (entry/later-inst a b)
    :else                     a))

(defn changed-part
  "The part of `v` that isn't in `previous`, the value at the same place in
  an earlier state, or nil when nothing changed."
  [v previous]
  (cond
    (entry/stamped? v)
    (when (not= v previous) v)

    (map? v)
    (not-empty (into {} (keep (fn [[k x]]
                                (some->> (changed-part x (get previous k))
                                         (vector k))))
                     v))

    :else
    nil))

(defn remove-entries
  "The map `m` without the stamped entries that `pred` holds for, nor the
  maps that they leave empty."
  [m pred]
  (reduce-kv (fn [m k v]
               (cond
                 (entry/stamped? v)
                 (cond-> m (pred v) (dissoc k))

                 (map? v)
                 (if-some [w (not-empty (remove-entries v pred))]
                   (assoc m k w)
                   (dissoc m k))

                 :else
                 m))
             m
             m))
