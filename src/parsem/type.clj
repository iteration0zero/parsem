(ns parsem.type
  (:require [parsem.setup :refer :all]
            [parsem.util :refer :all]
            [parsem.parser :as p]))

(def prod
  (fn [types]
    (fn [ts]
      (if (empty? ts)
        (munit [])
        (mbind (p/applyp
                (mbind (p/typep types)
                       (fn [v]
                           (mbind v
                                  (fn [v]
                                      (munit v)))))
                (first ts))
          (fn [p]
            (mbind p
              (fn [v]
                (mbind ((prod types)
                        (rest ts))
                  (fn [v']
                    (munit (cons v v'))))))))))))

(def sum
  (fn [types]
    (fn [ts]
      (if (empty? ts)
        mzero
        (mbind (p/applyp
                (mbind (p/typep types)
                       (fn [v]
                           (mbind v
                                  (fn [v]
                                      (munit v)))))
                (first ts))
          (fn [p]
            (mplus p
              ((sum types)
               (rest ts)))))))))


(defn -> [types]
  (fn [arg type]
    (fn [v]
      (letfn [(ttp []
                (p/mapp p/pitem
                  (mplus (mbind p/pitem
                           (fn [v']
                             (p/applyp (ttp) v')))
                    (mbind p/pitem
                      (fn [v']
                        (munit (if (= arg v')
                                 v
                                 v')))))))]
        (mbind (p/applyp (ttp) type)
               (fn [type']
                   (mbind (p/applyp
                           (mbind (p/typep types)
                                  (fn [v]
                                      (mbind v
                                             (fn [v]
                                                 (munit v)))))
                           type')
                          (fn [v]
                              (munit v)))))))))


(defn bind [_]
  (fn [t f]
    (mbind t f)))

(def base-types
  {:base/item (fn [_]
                (munit p/pitem))
   :base/pred (fn [_]
                (mbind p/pitem
                  (fn [pred]
                    (munit (p/ppred pred)))))
   :base/set (fn [_]
               (mbind p/pitem
                 (fn [set]
                   (munit (p/pset set)))))
   :base/string (fn [_]
                  (mbind (p/listp p/pitem)
                    (fn [s]
                      (munit (p/pstring s)))))
   :base/type (fn [types]
                (munit (p/typep types)))})

(def function-types
  {:f/identity [:-> :x :x]
   :f/self-apply [:-> :x [:x :x]]
   :f/first [:-> :x [:-> :y :x]]
   :f/second [:-> :x [:-> :y :y]]})

(def type-combinators
  {:c/list (fn [types]
             (mlet [[:type (fn [_] p/pitem)]
                    [:parsed-type (fn [{:keys [type]}]
                                      (p/applyp
                                       (mbind (p/typep types)
                                              (fn [v]
                                                  (mbind v
                                                         (fn [v]
                                                             (munit v)))))
                                       type))]]
               (fn [{:keys [parsed-type]}]
                 (munit (p/listp parsed-type)))))
   :c/sum (fn [types]
            (mlet [[:ts (fn [_] (p/listp p/pitem))]]
              (fn [{:keys [ts]}]
                (munit ((sum types) ts)))))
   :c/prod (fn [types]
             (mlet [[:ts (fn [_] (p/listp p/pitem))]]
               (fn [{:keys [ts]}]
                 (munit ((prod types) ts)))))
   :c/-> (fn [types]
           (mlet [[:arg (fn [_] p/pitem)]
                  [:type (fn [_] p/pitem)]]
             (fn [{:keys [arg type]}]
               (munit ((-> types) arg type)))))
   :c/bind (fn [types]
             (mlet [[:type (fn [_] p/pitem)]
                    [:parsed-type (fn [{:keys [type]}]
                                      (p/applyp
                                       (mbind (p/typep types)
                                              (fn [v]
                                                  (mbind v
                                                         (fn [v]
                                                             (munit v)))))
                                       type))]
                    [:ftype (fn [_] p/pitem)]
                    [:parsed-ftype (fn [{:keys [ftype]}]
                                       (p/applyp
                                        (mbind (p/typep types)
                                               (fn [v]
                                                   (mbind v
                                                          (fn [v]
                                                              (munit v)))))
                                        ftype))]]
               (fn [{:keys [parsed-type parsed-ftype]}]
                 (munit ((bind types) parsed-type parsed-ftype)))))
   :c/unit (fn [types]
             (mlet [[:arg (fn [_] p/pitem)]]
               (fn [{:keys [arg]}]
                 (munit arg))))
   :c/apply (fn [types]
              (mlet [[:type (fn [_] p/pitem)]
                     [:sequence (fn [_] p/pitem)]
                     [:parsed-type (fn [{:keys [type sequence]}]
                                     (p/applyp (p/typep types) type))]]
                (fn [{:keys [parsed-type sequence]}]
                  (munit
                   (p/applyp
                    parsed-type
                    sequence)))))})

(def dtype-grammar
  {:dtype/keyword [:base/pred keyword?]})

(def to-dtype-combinators
    {:to-dtype/keyword (fn [_]
                           (mlet [[:name-seq (fn [_] p/pitem)]]
                                 (fn [{:keys [name-seq]}]
                                     (munit (keyword (apply str name-seq))))))})

(defn g->types [g]
  (reduce (fn [acc [k v]]
            (assoc acc
              k
              (fn [types]
                  (p/applyp
                   (mbind (p/typep types)
                          (fn [v]
                            (mbind v
                                   (fn [v]
                                     (munit v)))))
                   v))))
          {}
    g))

(def aux-combinators
  {:c/seq
   (fn [types]
     (mlet [[:item (fn [_] p/pitem)]]
       (fn [{:keys [item]}]
         (munit (munit [item])))))
   :c/cons
   (fn [types]
     (mlet [[:item (fn [_] p/pitem)]
            [:sequence (fn [_] p/pitem)]]
       (fn [{:keys [item sequence]}]
         (munit (munit (cons item sequence))))))
   :c/filter
   (fn [types]
     (mlet
       [[:ftype (fn [_]
                  p/pitem)]
        [:type (fn [_]
                 p/pitem)]
        [:parsed-type (fn [{:keys [type]}]
                          (p/applyp
                           (mbind (p/typep types)
                                  (fn [v]
                                   (mbind v
                                         (fn [v]
                                            (munit v)))))
                           type))]
        [:parsed-ftype (fn [{:keys [ftype]}]
                           (p/applyp
                            (mbind (p/typep types)
                                   (fn [v]
                                       (mbind v
                                              (fn [v]
                                                  (munit v)))))
                            ftype))]]
       (fn [{:keys [parsed-ftype parsed-type]}]
         (munit
           (mbind parsed-type
             (fn [v]
               (p/applyp (p/filterp
                           p/pitem
                           (mbind p/pitem
                             (fn [v]
                               (p/applyp parsed-ftype
                                 v))))
                 v)))))))
   :c/map
   (fn [types]
     (mlet
         [[:mtype (fn [_]
                    p/pitem)]
          [:type (fn [_]
                   p/pitem)]
          [:parsed-type (fn [{:keys [type]}]
                            (p/applyp
                             (mbind (p/typep types)
                                    (fn [v]
                                       (mbind v
                                               (fn [v]
                                                   (munit v)))))
                             type))]
          [:parsed-mtype (fn [{:keys [mtype]}]
                             (p/applyp
                              (mbind (p/typep types)
                                     (fn [v]
                                       (mbind v
                                              (fn [v]
                                                  (munit v)))))
                              mtype))]]
       (fn [{:keys [parsed-mtype parsed-type]}]
         (munit
           (mbind parsed-type
             (fn [v]
               (p/applyp (p/mapp
                           p/pitem
                           (mbind p/pitem
                             (fn [v]
                               (p/applyp parsed-mtype
                                 v))))
                 v)))))))})

(def type-grammar
  {:type/type [:c/prod [:type/type-name] [:type/type-args]]
   :type/type-name [:dtype/keyword]
   :type/type-args [:c/filter [:type/type] [:c/list [:base/item]]]})

(def grammar-grammar
     {:grammar/nt [:dtype/keyword]
      :grammar/rule [:c/prod [:grammar/nt]
                     [:c/filter [:type/type]
                      [:c/bind [:base/item]
                       [:c/-> :x
                        [:c/unit [:c/seq :x]]]]]]
      :grammar/grammar [:c/filter [:grammar/rule] [:c/list [:base/item]]]})

(def to-grammar-grammar
    {:to-grammar/nt [:c/bind [:base/item]
                     [:c/-> :x
                      [:c/unit [:to-dtype/keyword :x]]]]
     :to-grammar/type [:c/bind [:base/item]
                       [:c/-> :x
                        [:c/bind [:c/list [:base/item]]
                         [:c/-> :y
                          [:c/unit [:c/cons :x :y]]]]]]
     :to-grammar/rule [:c/bind [:to-grammar/nt]
                       [:c/-> :x
                        [:c/bind [:to-grammar/type]
                         [:c/-> :y
                          [:c/unit [:c/cons :x [:y]]]]]]]})

(def parser-grammar
  {:parser/parser [:c/bind [:base/item]
                   [:c/-> :x
                    [:c/bind [:base/item]
                     [:c/-> :y
                      [:c/bind [:c/cons :x :y]
                       [:c/-> :z [:c/unit :z]]]]]]]})


(def bnf-to-idl-grammar
    {:bnf-to-idl/identifier [:c/bind [:bnf/identifier]
                             [:c/-> :x
                              [:c/bind [:c/apply [:to-grammar/nt] :x]
                               [:c/-> :y
                                [:c/unit [:c/seq :y]]]]]]})


(def idl-grammar
    [:idl/specification [:c/prod [:idl/definition] [:c/list [:idl/definition]]]])

(comment
  (in-ns 'parsem.type)
  (let [ts (merge base-types
                  type-combinators
                  aux-combinators
                  to-dtype-combinators
                  (g->types dtype-grammar)
                  (g->types type-grammar)
                  (g->types grammar-grammar)
                  (g->types parser-grammar)
                  (g->types to-grammar-grammar)
                  (g->types bnf-to-idl-grammar))]
    (def p0 (mbind (p/applyp
                    (mbind (p/typep ts)
                           (fn [v]
                               (mbind v
                                      (fn [v]
                                          (munit v)))))
                    [:c/bind [:base/item]
                     [:c/-> :x
                      [:c/unit :x]]])
                   (fn [v]
                       (mbind v
                              (fn [v]
                                  (munit v))))))
    (def p1 (mbind (p/applyp
                    (mbind (p/typep ts)
                           (fn [v]
                               (mbind v
                                      (fn [v]
                                          (munit v)))))
                    [:type/type])
                   (fn [v]
                       (mbind v
                              (fn [v]
                                  (munit v))))))
    (def p2 (mbind (p/applyp
                    (mbind (p/typep ts)
                           (fn [v]
                               (mbind v
                                      (fn [v]
                                          (munit v)))))
                    [:parser/parser])
                   (fn [v]
                       (mbind v
                              (fn [v]
                                  (munit v))))))
    (def p3 (mbind (p/applyp
                    (mbind (p/typep ts)
                           (fn [v]
                               (mbind v
                                      (fn [v]
                                          (munit v)))))
                    [:bnf/identifier])
                   (fn [v]
                       (mbind v
                              (fn [v]
                                  (munit v))))))

    #_((ffirst ((mlet [[:v (fn [_] p1)]
                       [:v (fn [{:keys [v]}]
                             (p/applyp p2 v))]]
                  (fn [{:keys [v]}]
                    (munit v)))
                [:type/type]))
       [:parser/parser])
    (p0 [:a])))




