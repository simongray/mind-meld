(ns dk.simongray.mind-meld-test
  "The CRDT of plain maps on a schema of its own: entries and their stamps,
  writes and the clock, merge by the later entry and by the higher count,
  the changes since a state, the registry of devices, compaction and the
  checks by the schema."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.mind-meld :as crdt]))

(def t0 #inst "2026-01-01T00:00:00Z")
(def t1 #inst "2026-01-01T01:00:00Z")
(def t2 #inst "2026-01-01T02:00:00Z")
(def t3 #inst "2026-01-01T03:00:00Z")

(def schema
  {:notes {:depth 1 :compact? true}
   :plays {:depth 2 :count-key :n}})

(deftest entries-and-their-stamps
  (let [old  {:text "old" :updated-at t0 :device "phone"}
        new  {:text "new" :updated-at t1 :device "laptop"}
        tied (assoc old :updated-at t1)]
    (is (= t1 (crdt/later-inst t0 t1) (crdt/later-inst t1 t0)))
    (is (= t0 (crdt/later-inst t0 nil) (crdt/later-inst nil t0)))
    (is (= (inst-ms t1) (crdt/stamp-ms new)))
    (is (= 0 (crdt/stamp-ms {})) "an entry without a stamp")
    (is (= new (crdt/later old new) (crdt/later new old)))
    (is (= old (crdt/later old nil) (crdt/later nil old)))
    (is (= tied (crdt/later tied new) (crdt/later new tied)) "at the same instant, the greater device id")
    (is (= ["n2" "n1"] (crdt/live-by-stamp {"n1" old "n2" new "n3" (assoc new :removed? true)}))
        "the live entries, the latest first")))

(deftest writing-and-the-clock
  (let [s (crdt/write {:device "phone"} [:notes "n1"] (constantly {:text "x"}) t1)]
    (is (= {:text "x" :updated-at t1 :device "phone"} (get-in s [:notes "n1"])))
    (is (= t1 (:clock s)))
    (is (= (crdt/after s t0) (get-in (crdt/write s [:notes "n2"] (constantly {}) t0) [:notes "n2" :updated-at]))
        "an earlier time is stamped just after the clock")
    (testing "at once, many writes share one stamp"
      (let [many (crdt/at-once s t2 (fn [s] (reduce #(crdt/write %1 [:notes %2] (constantly {}) t2) s (range 100))))]
        (is (= #{t2} (set (map :updated-at (vals (dissoc (:notes many) "n1"))))))
        (is (nil? (crdt/problem schema many)))))
    (testing "each of many writes, with one stamp"
      (let [both (crdt/write-each s [[[:notes "n2"] (constantly {:text "y"})] [[:notes "n1"] #(assoc % :text "z")]] t2)]
        (is (= #{t2} (set (map :updated-at (vals (:notes both))))))
        (is (= "z" (get-in both [:notes "n1" :text])))
        (is (= t2 (:clock both)))))
    (testing "a live entry and a tombstone"
      (is (crdt/live? (get-in s [:notes "n1"])))
      (is (not (crdt/live? (get-in (crdt/write s [:notes "n1"] #(assoc % :removed? true) t2) [:notes "n1"]))))
      (is (not (crdt/live? nil))))))

(deftest merging-by-the-schema
  (let [phone  (-> {:device "phone"}
                   (crdt/write [:notes "n1"] (constantly {:text "old"}) t0)
                   (crdt/write [:plays "song" "phone"] (constantly {:n 9}) t1))
        laptop (-> {:device "laptop"}
                   (crdt/write [:notes "n1"] (constantly {:text "new"}) t1)
                   (crdt/write [:plays "song" "phone"] (constantly {:n 5}) t2))
        merged (crdt/merge schema phone laptop)]
    (is (= "new" (get-in merged [:notes "n1" :text])) "the later entry")
    (is (= 9 (get-in merged [:plays "song" "phone" :n])) "the higher count, though it's the earlier entry")
    (is (= 5 (get-in (crdt/merge {} phone laptop) [:plays "song" "phone" :n])) "without a :count-key, the later entry")
    (is (= "phone" (:device merged)))
    (is (= (dissoc merged :device) (dissoc (crdt/merge schema laptop phone) :device)))
    (testing "two entries of one device at one instant: the same winner on every platform"
      (let [entry (fn [v] {:device "x" :notes {"n1" {:v v :updated-at t0 :device "x"}}})
            v     #(get-in (crdt/merge schema %1 %2) [:notes "n1" :v])]
        ;; the JVM prints 1.5E-4 and JavaScript 0.00015, so the printed floats
        ;; once ordered these two differently
        (is (= 1.55 (v (entry 1.5e-4) (entry 1.55)) (v (entry 1.55) (entry 1.5e-4))))))))

(deftest the-changes-since-a-state
  (let [before (crdt/write {:device "phone"} [:notes "n1"] (constantly {:text "x"}) t1)
        after  (crdt/write before [:notes "n2"] (constantly {:text "y"}) t2)]
    (is (= {:device "phone" :clock t2 :notes {"n2" (get-in after [:notes "n2"])}} (crdt/changes-since after before)))
    (is (= after (crdt/changes-since after nil)) "all of it, without an earlier state")
    (is (= {:device "phone" :clock t2} (crdt/changes-since after after)) "nothing changed")
    (testing "an entry merged since, however early its stamp"
      (let [merged (crdt/merge schema after (crdt/write {:device "laptop"} [:notes "n3"] (constantly {}) t0))]
        (is (= ["n3"] (keys (:notes (crdt/changes-since merged after)))))))))

(deftest the-registry-of-devices
  (let [laptop (crdt/identify {:device "laptop"} {:name "Laptop"} t0)
        phone  (-> {:device "phone"}
                   (crdt/identify {:name "Phone"} t0)
                   (crdt/identify {:platform "iOS"} t1)
                   (as-> s (crdt/merge schema s laptop)))
        names  #(update-vals (crdt/devices %) (fn [d] (select-keys d [:name :platform])))]
    (is (= {"phone" {:name "Phone" :platform "iOS"} "laptop" {:name "Laptop"}} (names phone))
        "what each device said of itself, merged")
    (let [retired (crdt/retire phone "laptop" t2)]
      (is (crdt/retired? retired "laptop"))
      (is (not (crdt/retired? retired "phone")))
      (is (= ["phone"] (keys (names retired))))
      (is (= ["phone"] (keys (names (crdt/identify (crdt/merge schema laptop retired) {:name "Back"} t3))))
          "a retired device can't come back by describing itself again"))))

(deftest devices-that-lag-behind
  (let [note   (fn [s id at] (crdt/write s [:notes id] (constantly {}) at))
        phone  (note (crdt/identify {:device "phone"} {} t0) "p1" t1)
        laptop (note (crdt/identify {:device "laptop"} {} t0) "l1" t2)
        tablet (-> (crdt/identify {:device "tablet"} {} t0)
                   (as-> s (crdt/merge schema s phone) (crdt/merge schema s laptop))
                   (crdt/acknowledge [phone laptop] t2))
        laptop (note laptop "l2" t3)
        phone  (-> (note phone "p2" t3)
                   (as-> s (crdt/merge schema s laptop) (crdt/merge schema s tablet))
                   (crdt/acknowledge [laptop tablet] t3))]
    (is (= {"tablet" t1 "laptop" nil} (crdt/lagging phone))
        "the tablet has merged everything up to t1, though the laptop's entries up to t2, and the laptop nothing")
    (let [tablet (crdt/acknowledge (crdt/merge schema tablet phone) [phone] t3)]
      (is (= {"laptop" nil} (crdt/lagging (crdt/merge schema phone tablet))) "until it catches up"))))

(deftest compacting-by-the-schema
  (let [far    #inst "2026-02-01T03:00:00Z"  ; 31 days after t3
        phone  (-> {:device "phone"}
                   (crdt/identify {:name "Phone"} t0)
                   (crdt/write [:notes "n1"] (constantly {:removed? true}) t1)
                   (crdt/write [:drafts "d1"] (constantly {:removed? true}) t1)
                   (crdt/as-device "service:x" #(crdt/write % [:notes "n2"] (constantly {:removed? true}) t1)))
        laptop (-> (crdt/identify {:device "laptop"} {:name "Laptop"} t0)
                   (as-> s (crdt/merge schema s phone))
                   (crdt/acknowledge [phone] t2))
        phone  (crdt/acknowledge (crdt/merge schema phone laptop) [laptop] t3)
        later  (crdt/compact schema phone far)]
    (is (nil? (get-in later [:notes "n1"])) "a tombstone that every device has seen goes")
    (is (some? (get-in later [:drafts "d1"])) "a collection that the schema doesn't compact keeps its tombstones")
    (is (some? (get-in later [:notes "n2"]))
        "so does an author outside the registry, whose stamps say nothing of what the devices have seen")
    (is (= phone (crdt/compact schema phone t3)) "a young tombstone stays")
    (is (= {} (crdt/lagging phone)))))

(deftest what-another-device-may-not-send
  (is (nil? (crdt/problem schema {:device "x" :clock t0 :notes {"n1" {:updated-at t0 :device "x"}}})))
  (doseq [[state found] [[{:clock "now"} #"clock"]
                         [{:notes [1]} #":notes is not a map"]
                         [{:notes {"n1" {:text "x"}}} #"not stamped"]
                         [{:plays {"song" {:n 5 :updated-at t0 :device "x"}}} #"\[:plays \"song\" :n\] is not a map"]
                         [{:theme "dark"} #"\[:theme\] is not in a stamped entry"]
                         [{:devices {"x" {:seen {"y" 5} :updated-at t0 :device "x"}}} #"seen"]
                         [{:retired {"x" {}}} #"not stamped"]]]
    (is (re-find found (str (crdt/problem schema state))) (pr-str state)))
  (is (= "the state is not a map" (crdt/problem schema [1 2]))))

(deftest what-a-schema-may-not-hold
  (is (nil? (crdt/schema-problem schema)))
  (is (nil? (crdt/schema-problem {:queue {:depth 1 :ranked? true :compact? true} :settings {:depth 0}})))
  (doseq [[s found] [[[:notes] #"schema is not a map"]
                     [{:notes [1]} #"options of :notes are not a map"]
                     [{:notes {:compact? true}} #":depth of :notes is nil"]
                     [{:notes {:depth -1}} #"not a whole number"]
                     [{:plays {:depth 2 :count :n}} #":plays has the unknown option :count"]
                     [{:clock {:depth 0}} #":clock is a key that the state holds"]
                     [{:devices {:depth 1}} #":devices is a key that the state holds"]]]
    (is (re-find found (str (crdt/schema-problem s))) (pr-str s))))

