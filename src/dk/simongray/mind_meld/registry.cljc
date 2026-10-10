(ns ^:no-doc dk.simongray.mind-meld.registry
  "The registry of devices in a state, which records what each device has
  merged."
  (:require [dk.simongray.mind-meld.entry :as entry]))

(def schema
  "The schema of the registry: what each device said of itself, under
  :devices, and the retired devices."
  {:devices {:depth 1}
   :retired {:depth 1}})

(defn latest-by-author
  "The latest stamp of each device among the entries of `state`, outside
  the registry of devices."
  [state]
  (letfn [(walk [m v]
            (cond
              (entry/stamped? v)
              (update m (:device v) entry/later-inst (:updated-at v))

              (map? v)
              (reduce-kv (fn [m _ x] (walk m x)) m v)

              :else
              m))]
    ;; or each acknowledgement would be news to acknowledge, and two devices
    ;; would keep writing to each other forever
    (walk {} (dissoc state :devices))))

;; Stability (Shapiro et al. 2011, section 4.1, after Wuu and Bernstein,
;; PODC 1984). Deviation: :seen keeps the latest stamp of each author, not a
;; vector clock, which holds only while an author's entries arrive in the
;; order of their stamps. That's true of the devices in the registry, but
;; not of the authors of as-device, so their tombstones stay.
(defn seen-by-all?
  "Whether every one of the live `devices` of `state` but the author of the
  entry `e` has merged it, by the registry of devices. It's false without
  another device, or for an author outside the registry."
  [state devices {:keys [device updated-at] :as e}]
  (let [judges (dissoc devices device)]
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
