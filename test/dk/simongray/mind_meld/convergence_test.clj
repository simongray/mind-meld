(ns dk.simongray.mind-meld.convergence-test
  "Devices that make random changes end in the same state, whatever the
  order of their merges: the merge is commutative, associative and
  idempotent, merging the changes since a state is merging the state, and
  compaction drops every tombstone but brings nothing back."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [dk.simongray.mind-meld :as crdt]
            [dk.simongray.mind-meld.list :as crdt.list]))

(def schema
  {:notes {:depth 1 :compact? true}
   :plays {:depth 2 :count-key :n}
   :queue {:depth 1 :ranked? true :compact? true}
   :moves {:depth 1 :ranked? true}})

(def devices
  ["phone" "laptop" "car"])

(def instants
  "A handful of instants close together, so that ties happen."
  (gen/fmap #(java.util.Date. (* 1000 (long %))) (gen/choose 0 5)))

(def ids
  (gen/elements ["a" "b" "c"]))

(def changes
  "Random changes to a state, as data, so that a failing case reads. The
  floats print differently on the JVM and in JavaScript."
  [(gen/tuple (gen/return :note) ids (gen/elements ["x" "y" 1.5e-4 1.55]) instants)
   (gen/tuple (gen/return :remove) ids instants)
   (gen/tuple (gen/return :play) ids (gen/choose 1 30) instants)
   (gen/tuple (gen/return :import) (gen/vector ids 1 3) instants)
   (gen/tuple (gen/return :append) (gen/vector ids 1 3) instants)
   (gen/tuple (gen/return :move) ids (gen/choose 0 3) instants)
   (gen/tuple (gen/return :dequeue) ids instants)
   (gen/tuple (gen/return :identify) (gen/elements ["Mine" "Yours"]) instants)])

(def service-change
  (gen/tuple (gen/return :service) ids instants))

(def retirement
  (gen/tuple (gen/return :retire) (gen/elements devices) instants))

(defn- queue
  "The queue of `state` with its moves."
  [state]
  (crdt.list/with-moves (:queue state) (:moves state)))

(defn- apply-change
  [state [kind x y z]]
  (case kind
    :note     (crdt/write state [:notes x] (constantly {:text y}) z)
    :remove   (crdt/write state [:notes x] #(assoc % :removed? true) y)
    :play     (crdt/write state [:plays x (:device state)] #(update % :n (fnil + 0) y) z)
    :import   (crdt/at-once state y (fn [s] (reduce #(crdt/write %1 [:notes %2] (constantly {:text "imported"}) y) s x)))
    :service  (crdt/as-device state "service" #(crdt/write % [:notes x] (constantly {:text "from a service"}) y))
    :append   (crdt.list/append state [:queue] (queue state) x y)
    :move     (crdt/write state [:moves x] (constantly {:rank (crdt.list/rank-at (queue state) x y)}) z)
    :dequeue  (crdt/write state [:queue x] #(assoc % :removed? true) y)
    :identify (crdt/identify state {:name x} y)
    :retire   (crdt/retire state x y)))

(def change
  (gen/frequency [[8 (gen/one-of changes)] [1 service-change] [1 retirement]]))

(def states
  "A device's state after a few random changes."
  (gen/fmap (fn [[device cs]] (reduce apply-change {:device device} cs))
            (gen/tuple (gen/elements devices) (gen/vector change 0 8))))

(defn- same?
  "Whether the states `a` and `b` agree on everything but whose device they
  are."
  [a b]
  (= (dissoc a :device) (dissoc b :device)))

(defspec merge-is-commutative 200
  (prop/for-all [a states b states]
    (same? (crdt/merge schema a b) (crdt/merge schema b a))))

(defspec merge-is-associative 200
  (prop/for-all [a states b states c states]
    (= (crdt/merge schema (crdt/merge schema a b) c)
       (crdt/merge schema a (crdt/merge schema b c)))))

(defspec merge-is-idempotent 200
  (prop/for-all [a states b states]
    (let [m (crdt/merge schema a b)]
      (= m (crdt/merge schema m m) (crdt/merge schema m a) (crdt/merge schema m b)))))

(defspec merging-the-changes-is-merging-the-state 200
  (prop/for-all [a     states
                 more  (gen/vector change 0 8)
                 b     states]
    (let [a' (reduce apply-change a more)]
      (= (crdt/merge schema b a')
         (crdt/merge schema (crdt/merge schema b a) (crdt/changes-since a' a))))))

(def acquainted
  "The states of the devices once each has described itself and they have
  all merged, so that each one knows the others."
  (let [known (reduce #(crdt/merge schema %1 %2)
                      (map #(crdt/identify {:device %} {} (java.util.Date. 0)) devices))]
    (mapv #(assoc known :device %) devices)))

;; A service's change at an instant older than a tombstone that compaction
;; already dropped comes back, so the devices make every change themselves.
(def schedule
  "What happens next: a device syncs, or makes a change."
  (gen/vector (gen/tuple (gen/choose 0 (dec (count devices)))
                         (gen/frequency [[3 (gen/return :sync)] [4 (gen/one-of changes)]]))
              0 20))

(defn- sync-at
  "The `states` after the device `i` merges the others, acknowledges them
  and compacts at `at`, with no tombstone kept longer than it must be."
  [states i at]
  (let [others (map states (remove #{i} (range (count states))))
        merged (reduce #(crdt/merge schema %1 %2) (states i) others)
        acked  (crdt/acknowledge merged others at)]
    (assoc states i (crdt/compact schema acked at {:keep-for 0}))))

(defn- run
  "The :states of the devices after the `steps` and three rounds in which
  every device syncs: one for the entries to reach everyone, one for what
  each acknowledged to reach everyone, and one for what the compaction
  left. Under :everything is the merge of every state they went through,
  every tombstone included."
  [steps]
  (let [rounds (for [_ (range 3) i (range (count devices))] [i :sync])]
    (reduce (fn [{:keys [states everything]} [n [i step]]]
              (let [at     (java.util.Date. (* 1000 (+ 100 n)))
                    states (if (= :sync step)
                             (sync-at states i at)
                             (update states i apply-change step))]
                {:states     states
                 :everything (crdt/merge schema everything (states i))}))
            {:states acquainted :everything (first acquainted)}
            (map-indexed vector (concat steps rounds)))))

(defn- shown
  "What `state` shows: its live notes, its queue in order and its counts."
  [state]
  {:notes (into {} (filter (comp crdt/live? val)) (:notes state))
   :queue (mapv key (crdt.list/ranked (queue state)))
   :plays (:plays state)})

(defn- tombstones
  "The tombstones of `state` in the collections that compaction drops from."
  [state]
  (filter :removed? (mapcat (comp vals state) [:notes :queue])))

(defspec devices-that-sync-converge 100
  (prop/for-all [steps schedule]
    (apply = (map #(dissoc % :device) (:states (run steps))))))

(defspec compaction-drops-every-tombstone-and-brings-nothing-back 100
  (prop/for-all [steps schedule]
    (let [{:keys [states everything]} (run steps)]
      (and (every? empty? (map tombstones states))
           (every? #(= (shown everything) (shown %)) states)))))
