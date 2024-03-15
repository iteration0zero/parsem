(ns parsem.scratch
  (:require [parsem.setup :refer :all]
            [parsem.parser :as p]))

(def ->p
  identity)

(def p-and-gen-p
  (mbind p/pitem
    (fn [parsed-item]
      (munit (mbind (munit (p/pchar parsed-item))
               ->p)))))
(defn typep [])

(comment
  (p-and-gen-p
    "a"))