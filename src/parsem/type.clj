(ns parsem.type
  (:require [parsem.setup :refer :all]
            [parsem.util :refer :all]
            [parsem.parser :as p]))

(def prod
  (fn [types]
    (fn [ts]
      (if (empty? ts)
        (munit [])
        (mbind (p/applyp (p/typep types) (first ts))
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
        (mbind (p/applyp (p/typep types) (first ts))
          (fn [p]
            (mplus p
              ((sum types)
               (rest ts)))))))))

(defn -> [types]
  (fn [[arg type]]
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
                                 v')))))))
              (rf [type]
                (mplus (p/applyp (mbind (p/pset #{:c/unit})
                                   (fn [_]
                                     (prn "-> type:" type)
                                     (mbind (p/applyp (p/typep types)
                                                      type)
                                            (fn [v]
                                                (prn "v unit:" v)
                                                (mbind v
                                                       (fn [v]
                                                           (prn "v unit:" v)
                                                           (munit v)))))))
                         type)
                  (mbind (p/applyp (p/typep types) type)
                    (fn [v]
                      (prn "v:" v)
                      (mbind v
                             (fn [v]
                               (munit v)))))))
              (of [type]
                (mbind (p/applyp (ttp) type)
                  (fn [v]
                    (mbind (update-s identity)
                      (fn [s]
                        (rf v))))))]
        (of type)))))


(defn bind [types]
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
                                    (p/applyp (p/typep types) type))]]
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
               (munit ((-> types) [arg type])))))
   :c/bind (fn [types]
             (mlet [[:type (fn [_] p/pitem)]
                    [:parsed-type (fn [{:keys [type]}]
                                    (p/applyp (p/typep types) type))]
                    [:ftype (fn [_] p/pitem)]
                    [:parsed-ftype (fn [{:keys [ftype]}]
                                     (p/applyp (p/typep types) ftype))]]
               (fn [{:keys [parsed-type parsed-ftype]}]
                 (munit ((bind types) parsed-type parsed-ftype)))))
   :c/unit (fn [types]
             (mlet [[:type (fn [_] p/pitem)]
                    [:parsed-type (fn [{:keys [type]}]
                                    (p/applyp (p/typep types) type))]]
               (fn [{:keys [parsed-type]}]
                 (munit (munit parsed-type)))))
   :c/apply (fn [types]
              (mlet [[:type (fn [_] p/pitem)]
                     [:parsed-type (fn [{:keys [type]}]
                                     (p/applyp (p/typep types) type))]
                     [:sequence (fn [_] p/pitem)]]
                (fn [{:keys [parsed-type sequence]}]
                  (p/applyp parsed-type sequence))))})

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
                (p/applyp (p/typep types) v))))
    {}
    g))

(def annotate
  (fn [types]
    (mlet [[:a (fn [_] p/pitem)]
           [:type (fn [_] p/pitem)]
           [:parsed-type (fn [{:keys [type]}]
                           (p/applyp (p/typep types) type))]]
     (fn [{:keys [a parsed-type]}]
       (munit
         (mbind parsed-type
           (fn [v]
             (munit [a v]))))))))

(def ast-combinators
  {:ast/annotate annotate})

(def aux-combinators
  {:c/seq
   (fn [types]
     (mlet [[:type (fn [_] p/pitem)]
            [:parsed-type (fn [{:keys [type]}]
                            (p/applyp (p/typep types) type))]]
       (fn [{:keys [parsed-type]}]
         (munit
           (mbind parsed-type
             (fn [v]
               (munit [v])))))))
   :c/cons
   (fn [types]
     (mlet [[:item (fn [_] p/pitem)]
            [:sequence (fn [_] p/pitem)]]
       (fn [{:keys [item sequence]}]
         (munit (cons item sequence)))))
   :c/filter
   (fn [types]
     (mlet
       [[:ftype (fn [_]
                  p/pitem)]
        [:type (fn [_]
                 p/pitem)]
        [:parsed-type (fn [{:keys [type]}]
                        (p/applyp (p/typep types) type))]
        [:parsed-ftype (fn [{:keys [ftype]}]
                         (p/applyp (p/typep types) ftype))]]
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
                          (p/applyp (p/typep types) type))]
          [:parsed-mtype (fn [{:keys [mtype]}]
                           (p/applyp (p/typep types) mtype))]]
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

(def base-grammar
  {:s  [:prod [:np] [:vp] [:np]]
   :np [:n]
   :vp [:v]
   :n (into [:string] "I")
   :v (into [:string] "eat")})

(def type-grammar
  {:type/type [:c/prod [:type/type-name] [:type/type-args]]
   :type/type-name [:dtype/keyword]
   :type/type-args [:c/filter [:type/type] [:c/list [:base/item]]]})

(def grammar-grammar
     {:grammar/nt [:dtype/keyword]
      :grammar/rule [:c/prod [:grammar/nt] [:c/filter [:type/type] [:c/seq [:base/item]]]]
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
                          [:c/unit [:c/cons :x :y]]]]]]})

(def parser-grammar
  {:parser/parser [:c/bind [:base/item]
                   [:c/-> :x
                    [:c/bind [:base/item]
                     [:c/-> :y
                      [:c/unit [:c/cons :x :y]]]]]]})

(comment
  (in-ns 'parsem.type)
  (let [ts (merge base-types
                  type-combinators
                  ast-combinators
                  aux-combinators
                  to-dtype-combinators
                  (g->types base-grammar)
                  (g->types dtype-grammar)
                  (g->types type-grammar)
                  (g->types grammar-grammar)
                  (g->types parser-grammar)
                  (g->types to-grammar-grammar))]
    (def p0 (p/type->p ts
              [:c/list [:base/item]]))
    (def p1 (p/type->p ts
              [:type/type]))
    (def p2 (p/type->p ts
              [:parser/parser]))
    (def p3 (p/type->p ts
                       [:grammar/grammar]))
    (def p4 (p/type->p ts
              [:to-grammar/type]))

    (p4 [:prod [:base/item] [:base/item]])

    #_((mbind (p/applyp (p/typep ts)
                        (ffirst (p4 [(seq "test") :prod [:base/item] [:base/item]])))
              (fn [v]
                  (mbind v
                         (fn [v]
                             (munit v)))))
       [])
    #_(p4 [(seq "test")])
    #_((ffirst ((mlet [[:v (fn [_] p1)]
                       [:v (fn [{:keys [v]}]
                             (p/applyp p2 v))]
                       [:v (fn [{:keys [v]}]
                             (p/applyp (p/typep ts) v))]
                       [:v (fn [{:keys [v]}]
                             v)]
                       [:p (fn [{:keys [v]}]
                             (p/applyp (p/typep ts) v))]]
                  (fn [{:keys [p]}]
                    (munit p)))
                [:type/type]))
       [:parser/parser])))



