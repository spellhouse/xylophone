(ns xylophone.zip
  "Utilites for working with zippers as returned by clojure.zip."
  (:refer-clojure :exclude [empty])
  (:require
   [clojure.zip :as zip]))

;; The version of seq-zip included with Clojure will throw a
;; NullPointerException when calling remove on a node that is an only
;; child. This version resolves the issue.
;;
;; SEE: http://dev.clojure.org/jira/browse/CLJ-941
(defn seq-zip
  "Returns a zipper for nested sequences, given a root sequence"
  {:added "1.0"}
  [root]
  (zip/zipper seq?
              identity
              (fn [node children]
                (with-meta (or children ()) (meta node)))
              root))


;; The version of xml-zip included with Clojure uses a branch?
;; predicate which is far too permissive - (complement string?). This
;; version of xml-zip uses a stricter predicate. 
(defn xml-zip
  "Returns a zipper for xml elements (as from xml/parse),
  given a root element"
  {:added "1.0"}
  [root]
  (zip/zipper (fn [x]
                (and (map? x)
                     (contains? x :tag)))
              (comp seq :content)
              (fn [node children]
                (assoc node :content (and children (apply vector children))))
              root))

(defn branch-fn
  "Given a zipper location return it's branch? function."
  [loc]
  (:zip/branch? (meta loc)))

(defn children-fn
  "Given a zipper location return it's children function."
  [loc]
  (:zip/children (meta loc)))

(defn make-node-fn
  "Given a zipper location return it's make-node function."
  [loc]
  (:zip/make-node (meta loc)))

(defn zipper-fn
  "Given a zipper location return it's zipper function."
  [loc]
  (let [{:keys [zip/branch? zip/children zip/make-node]} (meta loc)]
    (fn [root]
      (zip/zipper branch? children make-node root))))

(defn make-root
  "Return a zipper where the root-node is loc. loc must be a branch."
  [loc]
  (assert (zip/branch? loc) "Cannot make root from a node")
  ((zipper-fn loc) (zip/node loc)))

(defn empty
  "Remove all children from a loc. loc must be a branch"
  [loc]
  (assert (zip/branch? loc) "Cannot empty a node")
  (if (seq (zip/children loc))
    (reduce
     (fn [loc _]
       (zip/remove loc))
     (zip/rightmost (zip/down loc))
     (zip/children loc))
    loc))

(defn index
  "Return the integer position of loc within it's parent."
  [loc]
  (count (zip/lefts loc)))

(defn only-child?
  "True if loc is the only child of it's parent."
  [loc]
  (not (or (seq (zip/lefts loc))
           (seq (zip/rights loc)))))

(defn root?
  "True if loc is the root location in the zipper.."
  [loc]
  (nil? (zip/up loc)))

(defn child?
  "True if loc is not a root location."
  [loc]
  (not (nil? (zip/up loc))))

(defn forward
  "Like clojure.zip/right but maintains loc if there is no sibling to
  the right. Optionally an integer can be passed specifying the numer
  of moves right to make."
  ([loc]
     (or (zip/right loc) loc))
  ([loc n]
     (if (or (>= 0 n)
             (not (zip/right loc)))
       loc
       (recur (zip/right loc) (dec n)))))

(defn backward
  "Like clojure.zip/left but maintains loc if there is no sibling to
  the left. Optionally an integer can be passed specifying the numer
  of moves left to make."
  ([loc]
     (or (zip/left loc) loc))
  ([loc n]
     (if (or (>= 0 n)
             (not (zip/left loc)))
       loc
       (recur (zip/left loc) (dec n)))))
