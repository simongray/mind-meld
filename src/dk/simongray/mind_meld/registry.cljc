(ns ^:no-doc dk.simongray.mind-meld.registry
  "The registry of devices in a state, which records what each device has
  merged.

  In version 0.5.0, the tabs of a browser wrote as replicas of its device,
  with ids such as \"phone/1\", and the states still hold what they wrote.
  The registry counts those entries as their device's."
  (:require [clojure.string :as str]
            [dk.simongray.mind-meld.entry :as entry]))

(def schema
  "The schema of the registry: what each device said of itself, under
  :devices, and the retired devices."
  {:devices {:depth 1}
   :retired {:depth 1}})

(defn replica-device
  "The device whose replica `author` was, by an id of the device's id, a
  slash and the replica's name, or nil when it has no slash."
  [author]
  (let [s (str author)]
    (when-let [i (str/last-index-of s "/")]
      (subs s 0 i))))

(defn own?
  "Whether `author` is the `device` itself or was one of its replicas."
  [device author]
  (or (= device author)
      (= (str device) (replica-device author))))

(defn device-of
  "The device that `author` writes for in `state`: the author itself when
  it's a device in the registry, or else the device whose replica it was,
  when that's in the registry or the state's own. Any other author is its
  own."
  [state author]
  (let [devices (:devices state)
        device  (replica-device author)]
    (if (and (not (contains? devices author))
             (or (contains? devices device)
                 (= (str (:device state)) device)))
      device
      author)))

(defn latest-by-author
  "The latest stamp of each device among the entries of `state`, and the
  fields of the field-wise ones, outside the registry of devices."
  [state]
  (letfn [(stamp [m {:keys [device updated-at]}]
            (update m device entry/later-inst updated-at))
          (walk [m v]
            (cond
              (entry/stamped? v)
              (reduce stamp (stamp m v) (vals (:stamps v)))

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
;; order of their stamps. That's true of the devices in the registry, and
;; was of the replicas of version 0.5.0, each of which wrote in one place at
;; a time, but not of the authors of as-device, so their tombstones stay.
(defn seen-by-all?
  "Whether every one of the `devices` of `state` that aren't retired,
  except the device that wrote the entry `e`, has merged it, by the
  registry of devices. It's false without another device, or for an
  author outside the registry."
  [state devices {:keys [device updated-at] :as e}]
  (let [writer (device-of state device)
        judges (dissoc devices writer)]
    (and (contains? (:devices state) writer)
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
