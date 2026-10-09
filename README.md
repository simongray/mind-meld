mind-meld
=========

This is a Clojure and ClojureScript library for keeping an app's data the
same on all of a user's devices without a server, e.g. the notes of a
notes app, the queue of a podcast app, or the settings of any app. Each
device keeps the data as a plain map and changes it offline, and any two
of those maps merge without conflict, in any order. It's a CRDT, a
conflict-free replicated data type.

- **Plain data.** The state is a map that prints as EDN, so you can keep
  it anywhere a file can go, e.g. a folder that Dropbox syncs, a WebDAV
  server or IndexedDB.
- **The later change wins.** Each entry is stamped with the time and the
  device of its last change. A removal leaves a tombstone, so an older
  copy of the entry can't bring it back.
- **Ordered lists.** A queue keeps its order on every device, the items
  that two devices add at the same time don't mix, and a move never brings
  back an item that another device removed.
- **Counts.** A count that only grows, e.g. the seconds listened on one
  device, keeps the higher of two counts.
- **Tidying up.** A registry of the devices notes what each has seen, so
  that tombstones are dropped once every device has them.

The same code runs on the JVM, in Node and in the browser, with the same
results on each.

> This library was spun out of [podcast-clj](https://github.com/simongray/podcast-clj),
> a library for making podcast software, where it syncs a listener's
> subscriptions, queue and play progress between their devices. Like
> podcast-clj, it was developed with assistance from frontier LLMs.

Getting started
---------------

Add it to the `:deps` in your `deps.edn` as a Git dependency, with the SHA
of the latest commit on `master` (requires Clojure 1.11+ & Java 11+):

```clojure
io.github.simongray/mind-meld {:git/sha "…"}
```

For ClojureScript, also set `:deps true` in your `shadow-cljs.edn`, since
shadow-cljs only reads Git dependencies from `deps.edn`.

A state is a map with the id of its device, and a schema names its
collections. Each device writes its changes with `crdt/write`, at the time
it made them:

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
each of them. With `:compact? true`, the tombstones of removed notes are
dropped once every device has seen them, as described
[below](#tombstones-and-devices).

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
the devices can sync through anything that stores a document. Where two
devices changed the same entry, the later change wins. A removal is a
change too, which leaves a tombstone, e.g. when the laptop has synced and
removes the milk note:

```clojure
(crdt/write (crdt/merge schema laptop phone)
            [:notes "milk"]
            #(assoc % :removed? true)
            #inst "2026-10-09T11:00:00Z")
```

If the phone, still offline, edits the note at 10:30, the note stays
removed on both devices once they sync. Ask `crdt/live?` whether an entry
is there and isn't a tombstone.

A few more functions cover the rest of a sync:

- `crdt/at-once` gives a series of writes one stamp, e.g. those of an
  import. Without it, writes made at one time are stamped a millisecond
  apart, since a write is always stamped later than the state's clock.
- `crdt/as-device` writes what another device or a service did, at a time
  of its own.
- `crdt/changes-since` gives what changed since an earlier state, for a
  service that takes changes rather than whole states.
- `crdt/problem` describes what in a state would break the merge, so you
  can check a state from another device before you merge it.

Ordered lists
-------------

An ordered collection, e.g. a queue, is a map of ids to entries with a
`:rank`, a string that sorts by place. A move goes in a second collection
that holds only the new ranks, so a move never brings back an item that
another device removed:

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

The ids that a device appends together share a block of ranks named after
the device, so the two devices' items don't mix. With `:ranked? true`,
`crdt.list/problem` checks the ranks of a collection, as `crdt/problem`
checks the rest.

The ranks come from `dk.simongray.mind-meld.rank`, which you can also use
on its own. There's always room for another rank between any two, and the
ranks stay short at the ends of a list, however long it grows.

Tombstones and devices
----------------------

Tombstones pile up, so `crdt/compact` drops those of the collections with
`:compact? true` once every device has merged them and they're 30 days
old, by default. To know what each device has merged, the state keeps a
registry of the devices:

- Each device describes itself once with `crdt/identify`, e.g. with its
  name and platform.
- After it merges the whole states of other devices, it notes what they
  held with `crdt/acknowledge`.
- A device that stops syncing holds compaction back. `crdt/lagging` lists
  such devices, and `crdt/retire` removes one for good.

Principles
----------

- **Standards first.** Each part follows a published CRDT, e.g. the
  last-writer-wins register and element set of Shapiro et al. The code
  cites its sources, and says where it departs from them.
- **Merge by shape.** The merge goes key by key, down through maps to the
  stamped entries, so a state can grow new collections, and an older
  version of an app still merges them.
- **A clock that only moves forward.** A write is stamped after anything
  the device has merged, as a hybrid logical clock does, but with no
  counter, so a stamp stays a plain instant.
- **The same winner everywhere.** Of two entries with the same stamp, the
  device id decides, and then the printed entry, which prints the same on
  every platform.
- **Forget only what everyone has seen.** A tombstone is only dropped once
  every device in the registry has merged it, so no device can bring back
  what the others forgot.
- **One codebase.** The library is written in `.cljc`, with no
  dependencies but Clojure.

Development
-----------

```bash
clojure -X:test               # the tests on the JVM
npm install                   # once, for the Node tests
clojure -M:cljs compile test  # the tests in Node
```

License
-------

The mind-meld project is licensed under the [MIT licence](LICENSE).
