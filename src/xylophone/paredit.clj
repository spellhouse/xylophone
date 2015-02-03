(ns xylophone.paredit
  "Paredit operations in terms of zippers."
  (:refer-clojure :exclude [split])
  (:require
   [clojure.zip :as zip]
   [xylophone.zip :as xip]))


;; ---------------------------------------------------------------------
;; Deleting & Killing

(defn forward-delete
  "Remove the sibiling on the immediate right of loc."
  [loc]
  (if-let [loc (zip/right loc)]
    (zip/remove loc)
    loc))

(defn forward-kill
  "Remove all sibilings on the right of loc."
  [loc]
  (reduce
   (fn [loc _]
     (zip/remove (zip/right loc)))
   loc
   (zip/rights loc)))

(defn backward-delete
  "Remove the sibiling on the immediate left of loc."
  [loc]
  (if-let [loc (zip/left loc)]
    (zip/next (zip/remove loc))
    loc))

(defn backward-kill
  "Remove all sibilings on the left of loc."
  [loc]
  (reduce
   (fn [loc _]
     (-> (zip/left loc)
         (zip/remove)
         (zip/next)))
   loc
   (zip/lefts loc)))


;; ---------------------------------------------------------------------
;; Depth-Changing

(defn wrap
  "
  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])

    (-> (x/seq-zip '(+ 1 2))
        (wrap '(+ 3))
        (z/root))
    ;; => (+ 3 (+ 1 2))"
  [loc wrapper]
  (let [branch? (xip/branch-fn loc)
        zipper (xip/zipper-fn loc)]
    (when-not (branch? wrapper)
      (throw (ex-info "Wrapper must be a branch"
                      {:given wrapper
                       :expected
                       `(~'(:zip/branch? (meta loc)) ~wrapper)})))
    (zip/replace loc 
                 (-> (zipper wrapper)
                     (zip/append-child (zip/node loc))
                     (zip/root)))))

(defn splice-children
  "Given a branch inject it's children into it's parent node at the
  loc's position and discard the loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])


    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (splice)
        (z/root))
    ;; => (foo bar baz quux)"
  [loc]
  (if (xip/root? loc)
    loc
    (let [loc (reduce
               (fn [l c]
                 (zip/insert-left l c))
               loc
               (zip/children loc))]
      (zip/remove loc))))

(defn splice
  "Given a branch inject it's children into it's parent node at the
  loc's position and discard the loc.

  Example:

    (require
     '[clojure.zip :as z]
     '[xylophone.zip :as x])


    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (splice)
        (z/root))
    ;; => (foo bar baz quux)"
  [loc]
  (if (xip/root? loc)
    loc
    (-> (zip/up loc)
        (splice-children)
        (xip/backward (count (zip/rights loc))))))

(defn raise
  "Remove all sibilings to the left and right of loc and splice loc."
  [loc]
  (-> loc backward-kill forward-kill splice))

(defn splice-killing-forward
  "Remove all sibilings to the right of loc and splice loc."
  [loc]
  (-> loc forward-kill splice))

(defn splice-killing-backward
  "Remove all sibilings to the left of loc and splice loc."
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
  (if-let [ploc (zip/up loc)]
    (if-let [prloc (zip/right ploc)]
      (-> (zip/remove prloc)
          (zip/insert-right (zip/node prloc)))
      loc)
    loc))


(defn forward-barf
  "Move the rightmost sibling of loc to immediate right of the parent
  of loc. This is a no-op if loc is the root location.

  Maintains loc position while loc is not an only child. If loc is an
  only child the parent of loc will be maintained.

  Example:

    (-> (x/seq-zip '(foo (bar baz) quux))
        (z/down)
        (z/right)
        (z/down)
        (forward-barf)
        (z/root))

    ;; => (foo (bar) baz quux)"
  [loc]
  (if (zip/up loc)
    (if (xip/only-child? loc)
      (-> (zip/remove loc)
          (zip/insert-right (zip/node loc)))
      (let [idx (xip/index loc)
            rloc (zip/rightmost loc)]
        (-> (zip/remove rloc)
            (zip/up)
            (zip/insert-right (zip/node rloc))
            (zip/down)
            (xip/forward idx))))
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
  (if-let [ploc (zip/up loc)]
    (if-let [plloc (zip/left ploc)]
      (-> (zip/remove plloc)
          (zip/next)
          (zip/down)
          (zip/insert-left (zip/node plloc))
          (xip/forward (xip/index loc)))
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

    (-> (xip/seq-zip '(foo (bar baz) quux))
        (zip/down)
        (zip/right)
        (zip/down)
        (zip/right)
        (backward-barf)
        (zip/root))

    ;; => (foo bar (baz) quux)"
  [loc]
  (if (zip/up loc)
    (if (xip/only-child? loc)
      (-> (zip/remove loc)
          (zip/insert-left (zip/node loc)))
      (let [idx (xip/index loc)
            loc (zip/leftmost loc)]
        (-> (zip/remove loc)
            (zip/insert-left (zip/node loc))
            (zip/down)
            (xip/forward (dec idx)))))
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
  (assert (xip/child? loc) "Cannot split at top")
  (let [b (-> loc zip/up xip/empty xip/make-root)
        l (zip/node
           (reduce
            (fn [loc child]
              (zip/append-child loc child))
            b
            (zip/lefts loc)))
        r (zip/node
           (reduce
            (fn [loc child]
              (zip/append-child loc child))
            (zip/append-child b (zip/node loc))
            (zip/rights loc)))]
    (-> loc
        (zip/up)
        (zip/insert-left l)
        (zip/insert-right r)
        (zip/remove)
        (zip/next))))
