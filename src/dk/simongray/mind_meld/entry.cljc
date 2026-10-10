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
