(ns xylophone.paredit
  "Paredit operations in terms of zippers."
  (:refer-clojure :exclude [split])
  (:require
   [clojure.zip :as z]
   [xylophone.zip :as x]))

;; ---------------------------------------------------------------------
;; Deleting & Killing

(defn forward-delete
  "Remove the sibiling on the immediate right of loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo bar baz quux))
        (z/down)
        (z/right)
        (forward-delete)
        (z/root))
    ;; => (foo bar quux)"
  [loc]
  (if-let [loc (z/right loc)]
    (z/remove loc)
    loc))


(defn forward-kill
  "Remove all sibilings on the right of loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo bar baz quux))
        (z/down)
        (forward-kill)
        (z/root))

    ;; => (foo)"
  [loc]
  (reduce
   (fn [loc _]
     (z/remove (z/right loc)))
   loc
   (z/rights loc)))


(defn backward-delete
  "Remove the sibiling on the immediate left of loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo bar baz quux))
        (z/down)
        (z/rightmost)
        (backward-delete)
        (z/root))

    ;; => (foo bar quux)"
  [loc]
  (if-let [loc (z/left loc)]
    (z/next (z/remove loc))
    loc))



(defn backward-kill
  "Remove all sibilings on the left of loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo bar baz quux))
        (z/down)
        (z/rightmost)
        (backward-kill)
        (z/root))

    ;; => (quux)"
  [loc]
  (reduce
   (fn [loc _]
     (-> (z/left loc)
         (z/remove)
         (z/next)))
   loc
   (z/lefts loc)))


;; ---------------------------------------------------------------------
;; Depth-Changing

(defn wrap
  ;; This docstring could be better.
  "Create a new branch with wrapper at loc with the node at loc as
  it's child. wrapper must be a valid branch. 

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(+ 1 2))
        (wrap '(+ 3))
        (z/root))
    ;; => (+ 3 (+ 1 2))"
  [loc wrapper]
  (let [branch? (x/branch-fn loc)
        zipper (x/zipper-fn loc)]
    (when-not (branch? wrapper)
      (throw (ex-info "Wrapper must be a branch"
                      {:given wrapper
                       :expected `(~'(:zip/branch? (meta loc)) ~wrapper)})))
    (z/replace loc 
               (-> (zipper wrapper)
                   (z/append-child (z/node loc))
                   (z/root)))))

(defn splice-children
  "Given a branch inject it's children into it's parent node at the
  loc's position and discard the loc. This is a no-op if called from
  the root node.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (splice-children)
        (z/root))
    ;; => (foo bar baz quux)"
  [loc]
  (if (x/root? loc)
    loc
    (let [loc (reduce
               (fn [l c]
                 (z/insert-left l c))
               loc
               (z/children loc))]
      (z/remove loc))))

(defn splice
  "Like splice-children but operates on the parent of loc. This is a
  no-op if called on the root node. 

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (splice)
        (z/root))
    ;; => (foo bar baz quux)"
  [loc]
  (if (x/root? loc)
    loc
    (-> (z/up loc)
        (splice-children)
        (x/backward (count (z/rights loc))))))

(defn raise
  "Remove all sibilings to the left and right of loc and splice loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz quux)))
        (z/down)
        (z/right)
        (z/down)
        (z/right)
        (raise)
        (z/root))
    ;; => (foo baz)"
  [loc]
  (-> loc backward-kill forward-kill splice))


(-> (x/seq-zip '(foo (bar baz quux)))
    (z/down)
    (z/right)
    (z/down)
    (z/right)
    (raise)
    (z/root))

(defn splice-killing-forward
  "Remove all sibilings to the right of loc and splice loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (splice-killing-forward)
        (z/root))
    ;; => (foo bar quux)"
  [loc]
  (-> loc forward-kill splice))

(defn splice-killing-backward
  "Remove all sibilings to the left of loc and splice loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (z/right)
        (splice-killing-backward)
        (z/root))
    ;; => (foo baz quux)"
  [loc]
  (-> loc backward-kill splice))


;; ---------------------------------------------------------------------
;; Barfage & Slurpage


(defn forward-slurp
  "Move the node to immediate right of the parent of loc into the
  rightmost position of loc. This is a no-op if loc is the root node
  or the parent of loc has no sibling to the right.

  Maintains loc position.
 
  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (forward-slurp)
        (z/root))
    ;; => (foo (bar baz quux))"
  [loc]
  (if-let [ploc (z/up loc)]
    (if-let [prloc (z/right ploc)]
      (-> (z/remove prloc)
          (z/insert-right (z/node prloc)))
      loc)
    loc))


(defn forward-barf
  "Move the rightmost sibling of loc to immediate right of the parent
  of loc. This is a no-op if loc is the root location.

  Maintains loc position while loc is not an only child. If loc is an
  only child the parent of loc will be maintained.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (forward-barf)
        (z/root))

    ;; => (foo (bar) baz quux)"
  [loc]
  (if (z/up loc)
    (if (x/only-child? loc)
      (-> (z/remove loc)
          (z/insert-right (z/node loc)))
      (let [idx (x/index loc)
            rloc (z/rightmost loc)]
        (-> (z/remove rloc)
            (z/up)
            (z/insert-right (z/node rloc))
            (z/down)
            (x/forward idx))))
    loc))


(defn backward-slurp
  "Move the node to immediate left of the parent of loc into the
  leftmost position of loc. This is a no-op if loc is the root node or
  no the part of loc has no sibling to the left.

  Maintains loc position.
 
  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (backward-slurp)
        (z/root))
    ;; => ((foo bar baz) quux)"
  [loc]
  (if-let [ploc (z/up loc)]
    (if-let [plloc (z/left ploc)]
      (-> (z/remove plloc)
          (z/next)
          (z/down)
          (z/insert-left (z/node plloc))
          (x/forward (x/index loc)))
      loc)
    loc))


(defn backward-barf
  "Move the rightmost sibling of loc to immediate right of the parent
  of loc. This is a no-op if loc is the root location.

  Maintains loc position while loc is not an only child. If loc is an
  only child the parent of loc will be maintained.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (z/right)
        (backward-barf)
        (z/root))

    ;; => (foo bar (baz) quux)"
  [loc]
  (if (z/up loc)
    (if (x/only-child? loc)
      (-> (z/remove loc)
          (z/insert-left (z/node loc)))
      (let [idx (x/index loc)
            loc (z/leftmost loc)]
        (-> (z/remove loc)
            (z/insert-left (z/node loc))
            (z/down)
            (x/forward (dec idx)))))
    loc))


;; ---------------------------------------------------------------------
;; Misc 

(defn split
  "Similar to paredit-split-sexp.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (z/right)
        (split)
        (z/root))

    ;; => (foo (bar) (baz) quux)"
  [loc]
  (assert (x/child? loc) "Cannot split at top")
  (let [b (-> loc z/up x/empty x/make-root)
        l (z/node
           (reduce
            (fn [loc child]
              (z/append-child loc child))
            b
            (z/lefts loc)))
        r (z/node
           (reduce
            (fn [loc child]
              (z/append-child loc child))
            (z/append-child b (z/node loc))
            (z/rights loc)))]
    (-> loc
        (z/up)
        (z/insert-left l)
        (z/insert-right r)
        (z/remove)
        (z/next))))
