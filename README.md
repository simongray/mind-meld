mind-meld
=========

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/mind-meld.svg)](https://clojars.org/dk.simongray/mind-meld)

> _My mind to your mind. My thoughts to your thoughts._

This is a Clojure and ClojureScript library for keeping an app's data the
same on all of a user's devices without a server to merge it, e.g. the
notes of a notes app, the queue of a podcast app, or the settings of any
app. Each device keeps the data as a plain map and can change it offline,
and any two of those maps merge without conflict, in any order. It's a
[CRDT](https://crdt.tech/), a conflict-free replicated data type, for
[local-first](https://www.inkandswitch.com/essay/local-first/) apps.

**NOTE:** It's the companion of
[like-minded](https://github.com/simongray/like-minded): like-minded gets
the states from each device to the others, and mind-meld merges them
without conflict.

- **No server to run.** The state is a Clojure map that prints as EDN,
  so the devices can sync it through storage the user already has.
- **Lists that don't get scrambled.** A queue or a playlist has the same
  order on every device.
- **Counts that only go up.** A count such as the seconds listened on
  each device keeps its highest value.
- **Deletions that don't pile up.** Removed entries can be dropped once
  every device has seen the removal.

The same code runs on the JVM, in Node and in the browser, with the same
results on each.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software. There, it syncs a listener's
> subscriptions, queue and play progress between their devices. Like
> podcast-clj, it was developed with assistance from frontier LLMs.

Getting started
---------------

It requires Clojure 1.11+ and Java 11+. Add it from
[Clojars](https://clojars.org/dk.simongray/mind-meld) to the `:deps` in
your `deps.edn`:

```clojure
dk.simongray/mind-meld {:mvn/version "0.5.0"}
```

For unreleased changes, depend on the latest commit on `master` instead.
In shadow-cljs, a Git dependency only works through `deps.edn`, so set
`:deps true` in your `shadow-cljs.edn` too.

Writing and merging
-------------------

A state is a map with the id of its device, and a schema names its
collections. Each device writes its changes with `crdt/write` and the
time each was made:

```clojure
(require '[dk.simongray.mind-meld :as crdt])

(def schema
  {:notes {:depth 1 :compact? true}})

(def phone
  (crdt/write {:device "phone"}
              [:notes "milk"]
              (constantly {:text "Buy milk"})
              #inst "2026-10-09T09:00:00Z"))

(def laptop
  (crdt/write {:device "laptop"}
              [:notes "bread"]
              (constantly {:text "Buy bread"})
              #inst "2026-10-09T10:00:00Z"))
```

The `:depth` of the notes is 1, since one key, the note's id, leads to
each of them. With `:compact? true`, removed notes can be dropped for
good once every device has seen the removal, as described
[below](#tombstones-and-devices). To catch a mistake in a schema, e.g. in
a test, `crdt/schema-problem` describes an unknown option or a missing
`:depth`.

To sync, a device merges the states of the others into its own:

```clojure
(crdt/merge schema phone laptop)
;; => {:device "phone"
;;     :clock  #inst "2026-10-09T10:00:00.000-00:00"
;;     :notes  {"milk"  {:text       "Buy milk"
;;                       :updated-at #inst "2026-10-09T09:00:00.000-00:00"
;;                       :device     "phone"}
;;              "bread" {:text       "Buy bread"
;;                       :updated-at #inst "2026-10-09T10:00:00.000-00:00"
;;                       :device     "laptop"}}}
```

The merge gives the same result in any order and any number of times, so
the devices can sync through anything that stores a file. Where two
devices changed the same entry, the later change wins. A removal is a
change too: the entry stays as a [tombstone](https://crdt.tech/glossary),
marked `:removed?`, so that an older copy of it can't bring it back. Say
the laptop has synced and removes the milk note:

```clojure
(crdt/remove (crdt/merge schema laptop phone)
             [:notes "milk"]
             #inst "2026-10-09T11:00:00Z")
```

If the phone, still offline, edits the note at 10:30, the note stays
removed on both devices once they sync, since the removal at 11:00 is
later. Use `crdt/live?` to tell whether an entry is there and isn't a
tombstone.

Other functions cover imports and the rest of a sync:

- With `crdt/at-once`, a series of writes gets one stamp, e.g. those of
  an import. Without it, writes given the same time are stamped a
  millisecond apart, since a write is always stamped later than the
  state's clock.
- With `crdt/as-device`, you can write what another device or a service
  did, at the time it happened there.
- For a service that takes changes rather than whole states,
  `crdt/changes-since` gives what changed since an earlier state.
- To check a state from another device before you merge it,
  `crdt/problem` describes what in it would break the merge.

Fields that change apart
------------------------

An entry is one register, so where two devices changed the same entry,
one change is lost, even when they changed different fields, e.g. the
title of a note on the phone and its text on the laptop. For such
entries, write with `crdt/write-fields`, which stamps each field that
changes:

```clojure
(def note
  (crdt/write-fields {:device "phone"}
                     [:notes "milk"]
                     (constantly {:title "Milk" :text "2 litres"})
                     #inst "2026-10-09T09:00:00Z"))

(def phone
  (crdt/write-fields note
                     [:notes "milk"]
                     #(assoc % :title "Oat milk")
                     #inst "2026-10-09T10:00:00Z"))

(def laptop
  (crdt/write-fields (assoc note :device "laptop")
                     [:notes "milk"]
                     #(assoc % :text "3 litres")
                     #inst "2026-10-09T10:30:00Z"))

(crdt/fields (get-in (crdt/merge schema phone laptop) [:notes "milk"]))
;; => {:title "Oat milk" :text "3 litres"}
```

The entry keeps the stamp of each field under `:stamps`, and
`crdt/fields` gives the fields alone. Once written field by field, an
entry stays so, whether `crdt/write` or `crdt/write-fields` writes it
next.

A removal is still of the whole entry, and the later of a removal and an
edit wins. A removal ends every field, so an edit after it, on a device
that hadn't seen it, brings back only what was written after the
removal. Every device needs a version of mind-meld that knows these
entries, from 0.2.0, since an older one merges them whole.

Sets and counts
---------------

A set is a collection one level deeper, with an entry for each member,
e.g. the tags of each note under `[:tags note-id tag]`. A member is in
the set while its entry is live, and `crdt/remove` takes it out:

```clojure
(def schema
  {:notes {:depth 1 :compact? true}
   :tags  {:depth 2 :compact? true}
   :plays {:depth 2 :count-key :n}})

(defn tag
  "The `state` with the note `id` tagged `t` at `at`."
  [state id t at]
  (crdt/write state [:tags id t] (constantly {}) at))

(def tagged
  (-> {:device "phone"}
      (tag "milk" "dairy" #inst "2026-10-09T09:00:00Z")
      (tag "milk" "shopping" #inst "2026-10-09T09:00:00Z")
      (crdt/remove [:tags "milk" "shopping"] #inst "2026-10-09T10:00:00Z")))

(crdt/live-by-stamp (get-in tagged [:tags "milk"]))
;; => ["dairy"]
```

A count can't be one entry that every device writes, since two devices
that each add 1 to 5 would both write 6. Give each device an entry of
its own, with `crdt/author` in the path, and a `:count-key` in the
schema, so that a merge keeps the higher count of each device. Then
`crdt/sum` adds them up:

```clojure
(defn play
  "The `state` after its device played `song` at `at`."
  [state song at]
  (crdt/write state
              [:plays song (crdt/author state)]
              #(update % :n (fnil inc 0))
              at))

(def phone
  (-> {:device "phone"}
      (play "intro" #inst "2026-10-09T09:00:00Z")
      (play "intro" #inst "2026-10-09T09:10:00Z")))

(def laptop
  (play {:device "laptop"} "intro" #inst "2026-10-09T09:05:00Z"))

(crdt/sum schema (crdt/merge schema phone laptop) [:plays "intro"])
;; => 3
```

Ordered lists
-------------

An ordered collection, e.g. a queue, is a map of ids to entries with a
`:rank`, a string that sorts in the order of the list. A move goes in a
second collection that holds only the new ranks, so a move never brings
back an item that another device removed:

```clojure
(require '[dk.simongray.mind-meld.list :as crdt.list])

(def schema
  {:queue       {:depth 1 :ranked? true}
   :queue-moves {:depth 1 :ranked? true}})

(defn entries
  "The queue of `state` with its moves."
  [state]
  (crdt.list/with-moves (:queue state) (:queue-moves state)))

(defn queue
  "The ids in the queue of `state`, in order."
  [state]
  (mapv key (crdt.list/ranked (entries state))))

(defn move
  "The `state` with `id` moved to `index` of its queue at `at`."
  [state id index at]
  (crdt/write state
              [:queue-moves id]
              (constantly {:rank (crdt.list/rank-at (entries state) id index)})
              at))

(def phone
  (crdt.list/append {:device "phone"} [:queue] {} ["a" "b"]
                    #inst "2026-10-09T09:00:00Z"))

(def laptop
  (crdt.list/append {:device "laptop"} [:queue] {} ["c" "d"]
                    #inst "2026-10-09T09:05:00Z"))

(def both
  (crdt/merge schema phone laptop))

(queue both)
;; => ["c" "d" "a" "b"]

(queue (move both "d" 0 #inst "2026-10-09T10:00:00Z"))
;; => ["d" "c" "a" "b"]
```

The ids that a device appends together share a block of ranks of its own,
so the two devices' items don't mix. With `:ranked? true`,
`crdt.list/problem` checks the ranks of a collection, as `crdt/problem`
checks the rest.

The ranks come from `dk.simongray.mind-meld.rank`, which you can also use
on its own. There's always room for another rank between any two, and the
ranks stay short at the ends of a list, however long it grows.

Tombstones and devices
----------------------

Tombstones pile up, so `crdt/compact` drops those of the collections with
`:compact? true` once every device has merged them and they're older than
the `:keep-for` option you pass it, 30 days by default. The state holds a
registry of the devices, which records what each has merged:

- Each device describes itself once with `crdt/identify`, e.g. with its
  name and platform.
- After it merges the whole states of other devices, it records this
  with `crdt/acknowledge`.
- A device that stops syncing holds compaction back. You can find such
  devices with `crdt/lagging`, and retire one for good with
  `crdt/retire`.

Replicas of a device
--------------------

Some devices write one state in several places at once, e.g. the tabs and
workers of a web app, which share the browser's storage. They can't share
one id, since one of them would lose the counts of the other, and the
registry assumes that an author's entries arrive in the order of their
stamps. Give each of them a replica of its own with `crdt/with-replica`:

```clojure
(def tab
  (crdt/with-replica {:device "phone"} "1"))

(crdt/author tab)
;; => "phone/1"
```

The replica stamps what it writes, and a count with `crdt/author` in its
path gets an entry for each replica. The registry counts what a replica
writes as its device's, so the other devices still see one device. Each
replica is an author that they keep track of, though, so reuse the names
of replicas that are gone rather than make a new one for each tab.

The replicas share the device's storage, and each catches up with what
the others saved there with `crdt/rebase`, never `crdt/merge`:

```clojure
(crdt/rebase schema tab saved stored)
```

Here `saved` is the state that the tab last read from the storage or
wrote there, and `stored` the state there now. Since the registry counts
every replica as having merged what its device holds, a whole merge
would bring back what another replica compacted meanwhile. A rebase
takes only what the tab changed since, and moves those changes after the
clock of `stored`, as if made when they're saved.

Principles
----------

- **Standards first.** Each part follows a published design, e.g. the
  [last-writer-wins](https://crdt.tech/glossary) register and element set
  of [Shapiro et al.](https://inria.hal.science/inria-00555588) The code
  cites its sources, and says where it departs from them.
- **Merge by shape.** The merge goes key by key, down through maps to the
  stamped entries, so a state can grow new collections, and an older
  version of an app still merges them.
- **A clock that only moves forward.** A write is stamped after anything
  the device has merged, as a
  [hybrid logical clock](https://cse.buffalo.edu/tech-reports/2014-04.pdf)
  does, but with no counter, so a stamp stays a plain instant.
- **The same winner everywhere.** Of two entries with the same stamp, the
  device id decides, and then the entry as printed, which is the same on
  every platform.
- **Forget only what everyone has seen.** A tombstone is only dropped once
  every device in the registry that isn't retired has merged it, so none
  of those devices can bring back what the others forgot.
- **One codebase.** The library is written in `.cljc`, with no
  dependencies but Clojure.

Development
-----------

```bash
clojure -X:test                       # the tests on the JVM
npm install                           # once, for ClojureScript
clojure -M:cljs compile test          # the tests in Node
clojure -M:cljs compile browser-test  # the tests in a browser
node test/browser-run.mjs             # run them in a headless Chrome
```

License
-------

The mind-meld project is licensed under the [MIT licence](LICENSE).
