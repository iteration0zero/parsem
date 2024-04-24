(ns parsem.util
  (:require [parsem.setup :refer :all]
            [parsem.parser :as p]))

(defn mlet [bnds f]
  (mbind (p/applyp (p/redp p/pitem
                     (munit {})
                     (fn [acc]
                       (fn [[k v]]
                         (munit
                           (mbind acc
                             (fn [acc-v]
                               (mbind (v acc-v)
                                      (fn [v]
                                        (munit (assoc acc-v k v))))))))))
           bnds)
    (fn [v]
      (mbind v
        f))))


(comment
  (in-ns 'parsem.util)
  ((mlet [[:a (fn [_] p/pitem)]
          [:b (fn [_] p/pitem)]]
     (fn [{:keys [a b]}]
       (munit [a b])))
   [1 2])
  (let [a 1
        b 2]
    [a b]))