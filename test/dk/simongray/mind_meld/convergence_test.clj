(ns dk.simongray.mind-meld.convergence-test
  "Devices that make random changes end in the same state, whatever the
  order of their merges: the merge is commutative, associative and
  idempotent, merging the changes since a state is merging the state, and
  compaction drops every tombstone but brings nothing back, also when two
  replicas of a device share its store and lose no count."
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [dk.simongray.mind-meld :as crdt]
            [dk.simongray.mind-meld.list :as crdt.list]))

(def schema
  {:notes {:depth 1 :compact? true}
   :cards {:depth 1 :compact? true}
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
   (gen/tuple (gen/return :identify) (gen/elements ["Mine" "Yours"]) instants)
   (gen/tuple (gen/return :field) ids (gen/elements [:title :body :done]) (gen/elements ["x" "y" 1.55 nil]) instants)
   (gen/tuple (gen/return :unfield) ids (gen/elements [:title :body :done]) instants)
   (gen/tuple (gen/return :remove-card) ids instants)
   (gen/tuple (gen/return :restore-card) ids instants)
   (gen/tuple (gen/return :plain-card) ids (gen/elements ["x" "y"]) instants)])

(def service-change
  (gen/one-of [(gen/tuple (gen/return :service) ids instants)
               (gen/tuple (gen/return :service-field) ids (gen/elements ["x" "y"]) instants)]))

(def retirement
  (gen/tuple (gen/return :retire) (gen/elements devices) instants))

(defn- queue
  "The queue of `state` with its moves."
  [state]
  (crdt.list/with-moves (:queue state) (:moves state)))

(defn- apply-change
  [state [kind x y z w]]
  (case kind
    :field         (crdt/write-fields state [:cards x] #(assoc % y z) w)
    :unfield       (crdt/write-fields state [:cards x] #(dissoc % y) z)
    :remove-card   (crdt/write-fields state [:cards x] #(assoc % :removed? true) y)
    :restore-card  (crdt/write-fields state [:cards x] #(dissoc % :removed?) y)
    :plain-card    (crdt/write state [:cards x] #(assoc % :tag y) z)
    :service-field (crdt/as-device state "service" #(crdt/write-fields % [:cards x] (fn [c] (assoc c :title y)) z))
    :note     (crdt/write state [:notes x] (constantly {:text y}) z)
    :remove   (crdt/write state [:notes x] #(assoc % :removed? true) y)
    :play     (crdt/write state [:plays x (crdt/author state)] #(update % :n (fnil + 0) y) z)
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
  "What `state` shows: its live notes and cards, its queue in order and its
  counts."
  [state]
  {:notes (into {} (filter (comp crdt/live? val)) (:notes state))
   ;; a card keeps the stamps of the fields that went and of its last
   ;; removal, which compaction drops with a tombstone, so it shows its fields
   :cards (into {}
                (keep (fn [[k v]] (when (crdt/live? v) [k (crdt/fields v)])))
                (:cards state))
   :queue (mapv key (crdt.list/ranked (queue state)))
   :plays (:plays state)})

(defn- tombstones
  "The tombstones of `state` in the collections that compaction drops from."
  [state]
  (filter :removed? (mapcat (comp vals state) [:notes :cards :queue])))

(defspec devices-that-sync-converge 100
  (prop/for-all [steps schedule]
    (apply = (map #(dissoc % :device) (:states (run steps))))))

(defspec compaction-drops-every-tombstone-and-brings-nothing-back 100
  (prop/for-all [steps schedule]
    (let [{:keys [states everything]} (run steps)]
      (and (every? empty? (map tombstones states))
           (every? #(= (shown everything) (shown %)) states)))))

;; Two replicas of the phone share its store, as two tabs share a browser's
;; storage. Each changes its own state, and rebases onto the store when it
;; saves or syncs, unless the store holds what it last read or wrote there.
(def writers
  ["laptop" "car" "phone/1" "phone/2"])

(def acquainted-replicas
  "The world of the laptop, the car and two replicas of the phone, once
  each device has described itself and they have all merged, with the
  state that each replica last read from the store or wrote there."
  (let [[phone laptop car] acquainted]
    {:states {"laptop"  laptop
              "car"     car
              "phone/1" (crdt/with-replica phone "1")
              "phone/2" (crdt/with-replica phone "2")}
     :bases  {"phone/1" phone
              "phone/2" phone}
     :store  phone}))

(def replica-schedule
  "What happens next: a device or a replica syncs or makes a change, or a
  replica saves."
  (gen/vector (gen/tuple (gen/elements writers)
                         (gen/frequency [[3 (gen/return :sync)]
                                         [2 (gen/return :save)]
                                         [4 (gen/one-of changes)]]))
              0 24))

(defn- replica?
  [who]
  (some? (re-find #"/" who)))

(defn- synced
  "The state `s` of `who` in the `world` after a sync at `at`: merged with
  the states of the other devices, the phone's from its store,
  acknowledged and compacted."
  [{:keys [states store]} who s at]
  (let [others (cond-> (vec (for [[w x] states
                                  :when (and (not= w who) (not (replica? w)))]
                              x))
                 (not (replica? who)) (conj store))
        merged (reduce #(crdt/merge schema %1 %2) s others)]
    (crdt/compact schema (crdt/acknowledge merged others at) at {:keep-for 0})))

(defn- replica-step
  "The `world` after `who` takes the `step` at `at`. A change stays in the
  state of whoever makes it, and the plays add up under :played. A
  replica rebases onto the store when it saves or syncs, and saves into
  it after."
  [{:keys [states bases store] :as world} who step at]
  (let [own  (states who)
        save (fn [s]
               (-> world
                   (assoc-in [:states who] s)
                   (assoc-in [:bases who] s)
                   (assoc :store s)))]
    (cond
      (and (replica? who) (#{:sync :save} step))
      (let [rebased (crdt/rebase schema own (bases who) store)]
        (save (cond->> rebased (= :sync step) (#(synced world who % at)))))

      (= :sync step) (assoc-in world [:states who] (synced world who own at))
      (= :save step) world

      :else
      (cond-> (assoc-in world [:states who] (apply-change own step))
        (= :play (first step))
        (update-in [:played (second step)] (fnil + 0) (nth step 2))))))

(defn- run-replicas
  "The world after the `steps` and four rounds in which every device and
  replica syncs, with the merge of everything it went through."
  [steps]
  (let [rounds (for [_ (range 4) w writers] [w :sync])]
    (reduce (fn [{:keys [world everything]} [n [who step]]]
              (let [world (replica-step world who step (java.util.Date. (* 1000 (+ 100 n))))]
                {:world      world
                 :everything (crdt/merge schema everything (get-in world [:states who]))}))
            {:world acquainted-replicas :everything (:store acquainted-replicas)}
            (map-indexed vector (concat steps rounds)))))

(defn- counts-every-play?
  "Whether the state `s` counts every play of the :played of the `world`."
  [world s]
  (every? (fn [[song n]] (== n (crdt/sum schema s [:plays song])))
          (:played world)))

;; more runs than the others, since a wrong design can take hundreds to fail
(defspec replicas-that-share-a-store-converge 1000
  (prop/for-all [steps replica-schedule]
    (let [{:keys [world everything]} (run-replicas steps)
          all (conj (vals (:states world)) (:store world))]
      (and (apply = (map #(dissoc % :device :replica) all))
           (every? empty? (map tombstones all))
           (every? #(= (shown everything) (shown %)) all)
           (every? (partial counts-every-play? world) all)))))
