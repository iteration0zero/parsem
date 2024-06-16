(ns parsem.util
  (:require [parsem.setup :refer :all]
            [parsem.parser :as p]
            [parsem.monad :as m]
            [parsem.setup :as setup]
            [parsem.sequence :as sequence]))

(defn mlet [bnds f]
  (mbind (p/applyp (p/redp p/pitem
                     (munit {})
                     (mbind p/pitem
                            (fn [acc-p]
                              (munit (mbind p/pitem
                                            (fn [[k valuep-f]]
                                              (munit (mbind acc-p
                                                            (fn [acc-val]
                                                              (mbind (valuep-f acc-val)
                                                                     (fn [value]
                                                                       (munit (assoc acc-val k value)))))))))))))
                   bnds)
    (fn [bnd-p]
      (mbind bnd-p
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