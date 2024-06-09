(ns parsem.type
  (:require [parsem.setup :refer :all]
            [parsem.util :refer :all]
            [parsem.parser :as p]
            [parsem.bnf :as bnf]))

(defn type->value [types type]
  (p/applyp
    (p/typep types)
    type))

(defn parse [parser]
  (mbind parser
    (fn [value]
      (munit value))))

(defn prod [ctx arg-types]
      (if (empty? arg-types)
        (munit [])
        (mbind (type->value ctx (first arg-types))
          (fn [parser]
            (mbind parser
              (fn [value]
                (mbind (prod ctx (rest arg-types))
                  (fn [value']
                    (munit (cons value value'))))))))))

(defn sum [ctx arg-types]
    (if (empty? arg-types)
      mzero
      (mbind (type->value ctx (first arg-types))
        (fn [parser]
          (mplus parser
            (sum ctx (rest arg-types)))))))

(defn -> [ctx arg type]
  (fn [value]
    (letfn [(ttp []
              (p/mapp p/pitem
                (mplus (mbind p/pitem
                         (fn [value']
                           (p/applyp (ttp) value')))
                  (mbind p/pitem
                    (fn [value']
                      (munit (if (= arg value')
                               value
                               value')))))))]
      (mbind (p/applyp (ttp) type)
             (fn [type']
               (mbind (type->value ctx type')
                      (fn [parser]
                        (parse parser))))))))

(defn bind [_ t f]
  (mbind t f))

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
   :base/type (fn [ctx]
                (munit (p/typep ctx)))
   :base/fetch (fn [_]
                 (munit mfetch))})

(def type-combinators
    {:c/list     (fn [ctx]
                     (mlet [[:type (fn [_] p/pitem)]
                            [:parsed-type (fn [{:keys [type]}]
                                              (type->value ctx type))]]
                           (fn [{:keys [parsed-type]}]
                               (munit (p/listp parsed-type)))))
     :c/sum      (fn [ctx]
                     (mlet [[:arg-types (fn [_] (p/listp p/pitem))]]
                           (fn [{:keys [arg-types]}]
                               (munit (sum ctx arg-types)))))
     :c/prod     (fn [ctx]
                     (mlet [[:arg-types (fn [_] (p/listp p/pitem))]]
                           (fn [{:keys [arg-types]}]
                               (munit (prod ctx arg-types)))))
     :c/->       (fn [ctx]
                     (mlet [[:arg (fn [_] p/pitem)]
                            [:type (fn [_] p/pitem)]]
                           (fn [{:keys [arg type]}]
                               (munit (-> ctx arg type)))))
     :c/bind     (fn [ctx]
                   (mlet [[:type (fn [_] p/pitem)]
                          [:parsed-type (fn [{:keys [type]}]
                                            (type->value ctx type))]
                          [:ftype (fn [_] p/pitem)]
                          [:parsed-ftype (fn [{:keys [ftype]}]
                                             (type->value ctx ftype))]]
                         (fn [{:keys [parsed-type parsed-ftype]}]
                             (munit (bind ctx parsed-type parsed-ftype)))))
     :c/plus     (fn [ctx]
                   (mlet [[:type1 (fn [_] p/pitem)]
                          [:type2 (fn [_] p/pitem)]
                          [:parsed-type1 (fn [{:keys [type1]}]
                                             (type->value ctx type1))]
                          [:parsed-type2 (fn [{:keys [type2]}]
                                             (type->value ctx type2))]]
                         (fn [{:keys [parsed-type1 parsed-type2]}]
                             (munit (mplus parsed-type1
                                           parsed-type2)))))
     :c/unit     (fn [ctx]
                   (mlet [[:arg (fn [_] p/pitem)]]
                     (fn [{:keys [arg]}]
                       (munit (munit arg)))))
     :c/identity (fn [ctx]
                     (mlet [[:item (fn [_] p/pitem)]]
                       (fn [{:keys [item]}]
                         (munit item))))
     :c/apply (fn [ctx]
                (mlet [[:type (fn [_] p/pitem)]
                       [:sequence (fn [_] p/pitem)]
                       [:parsed-type (fn [{:keys [type sequence]}]
                                       (type->value ctx type))]]
                  (fn [{:keys [parsed-type sequence]}]
                    (munit (p/applyp
                            parsed-type
                            sequence)))))})

(def dtype-grammar
    {:dtype/keyword [:base/pred keyword?]})

(def to-dtype-combinators
    {:to-dtype/keyword (fn [_]
                         (mlet [[:name-seq (fn [_] p/pitem)]]
                           (fn [{:keys [name-seq]}]
                             (munit (keyword (apply str name-seq))))))})

(defn grammar->ctx [grammar]
  (reduce (fn [acc [key value]]
            (assoc acc
              key
              (fn [ctx]
                (type->value ctx value))))
          {}
    grammar))

(def aux-combinators
  {:c/seq
   (fn [_]
     (mlet [[:item (fn [_] p/pitem)]]
       (fn [{:keys [item]}]
         (munit (munit [item])))))
   :c/cons
   (fn [_]
     (mlet [[:item (fn [_] p/pitem)]
            [:sequence (fn [_] p/pitem)]]
       (fn [{:keys [item sequence]}]
         (munit (munit (cons item sequence))))))
   :c/reduce
   (fn [ctx]
     (mlet
       [[:acctype (fn [_] p/pitem)]
        [:rfptype (fn [_] p/pitem)]
        [:parsed-acctype (fn [{:keys [acctype]}]
                           (type->value ctx acctype))]
        [:parsed-rfptype (fn [{:keys [rfptype]}]
                           (type->value ctx rfptype))]]
       (fn [{:keys [parsed-acctype parsed-rfptype]}]
           (munit
            (p/redp
              p/pitem
              parsed-acctype
              parsed-rfptype)))))
   :c/filter
   (fn [ctx]
     (mlet
       [[:ftype (fn [_]
                  p/pitem)]
        [:parsed-ftype (fn [{:keys [ftype]}]
                         (type->value ctx ftype))]]
       (fn [{:keys [parsed-ftype]}]
         (munit
           (p/filterp
             p/pitem
             (mbind p/pitem
               (fn [value]
                 (p/applyp parsed-ftype
                   value))))))))
   :c/map
   (fn [ctx]
     (mlet
         [[:mtype (fn [_]
                    p/pitem)]
          [:parsed-mtype (fn [{:keys [mtype]}]
                           (type->value ctx mtype))]]
       (fn [{:keys [parsed-mtype]}]
         (munit
           (p/mapp
             p/pitem
             parsed-mtype)))))
   :c/prn
   (fn [_]
     (mlet
      [[:val (fn [_] p/pitem)]]
      (fn [{:keys [val]}]
        (munit (munit (prn val))))))})


(def function-grammar
  {:f/apply
   [:c/bind [:base/item]
    [:c/-> :f
     [:c/bind [:base/item]
      [:c/-> :s
       [:c/bind [:c/seq :s]
        [:c/-> :ss
         [:c/apply
          [:c/bind [:base/item]
           :f]
          :ss]]]]]]]
   :f/f->p
   [:c/-> :f
    [:c/bind
     [:c/apply
      [:base/type]
      [:c/bind [:base/item]
       [:c/-> :arg
        [:c/apply
         [:f/apply]
         [:f :arg]]]]]
     [:c/-> :p
      [:c/unit :p]]]]})


(def sequence-grammar
  {:s/first
   [:-> :s
    [:c/apply [:base/item]
     :s]]
   :s/second
   [:-> :s
    [:c/bind
     [:f/apply [:s/rest]
      :s]
     [:-> :s_r
      [:c/bind [:base/item]
       [:c/-> :second
        [:c/unit :second]]]]]]
   :s/rest
   [:-> :s
    [:c/apply
     [:c/bind [:base/item]
      [:c/-> :first
       [:c/bind [:base/fetch]
        [:c/-> :rest
         [:c/unit :rest]]]]]
     :s]]})

#_(def test-grammar
    {:select [:c/bind [:c/list
                       [:c/bind [:base/item]
                        [:c/-> :i
                         [:c/apply
                          [:c/prod [:base/item] [:base/item]]
                          :i]]]]
              [:c/-> :keys
               [:c/unit
                [:c/bind [:base/item]
                 [:c/-> :sequence
                  [:c/reduce
                   [:c/unit []]
                   [:c/-> :acc
                    [:c/-> :item
                     [:c/reduce
                      [:c/unit :acc]
                      [:c/-> :iacc
                       [:c/-> :key
                        [:c/bind [:f/apply
                                  [:f/first]
                                  :key]
                         [:c/-> :keyname
                          [:c/bind [:f/apply
                                    [:f/second]
                                    :key]
                           [:c/-> :keytype
                            [:c/plus
                             [:c/apply
                              [:c/bind [:c/sum :keytype]
                               [:c/-> :i
                                [:c/cons [:c/cons :keyname [:c/seq :i]]
                                 :iacc]]]
                              :item]
                             [:c/unit :iacc]]]]]]]]
                      :keys]]]
                   :sequence]]]]]]})

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

(def test-grammar
  {:test/test [:c/bind [:base/item]
               [:c/-> :f
                [:c/bind [:base/item]
                 [:c/-> :s
                  [:c/bind [:c/seq :s]
                   [:c/-> :ss
                    [:c/cons :f :ss]]]]]]]})

(comment
  (in-ns 'parsem.type)
  (let [ctx (merge base-types
                   type-combinators
                   aux-combinators
                   to-dtype-combinators
                   (grammar->ctx dtype-grammar)
                   (grammar->ctx type-grammar)
                   (grammar->ctx grammar-grammar)
                   (grammar->ctx sequence-grammar)
                   (grammar->ctx function-grammar)
                   (grammar->ctx test-grammar)
                   (grammar->ctx bnf/bnf-g))]
    (def p0 (mbind (type->value ctx
                    [:c/bind [:c/apply [:select]
                              [[:lhs [:bnc/mascii]]]]
                     [:c/-> :seq-selector
                      [:c/bind [:c/apply [:base/type]
                                :seq-selector]
                       [:c/-> :cmpld-seq-selector
                        [:c/bind [:c/parser :cmpld-seq-selector]
                         [:c/-> :res
                          [:c/unit :res]]]]]]])
                   (fn [parser]
                     (parse parser))))
    (def p1 (mbind (type->value ctx
                    [:bnf/identifier])
                   (fn [parser]
                     (parse parser))))
    (def p2 (mbind (type->value ctx
                    [:c/bind [:c/apply [:select]
                              [[:lhs [:bnc/mascii]]]]
                     [:c/-> :seq-selector
                      [:c/bind [:c/apply [:base/type]
                                :seq-selector]
                       [:c/-> :cmpld-seq-selector
                        [:c/unit :cmpld-seq-selector]]]]])
                   (fn [parser]
                     (parse parser))))
    (def p3 (mbind (type->value ctx
                    [:c/reduce
                     [:c/identity []]
                     [:c/bind [:base/item]
                      [:c/-> :acc
                       [:c/bind [:c/apply [:base/type]
                                 [:c/bind [:base/item]
                                  [:c/-> :item
                                   [:c/bind [:c/cons :item :acc]
                                    [:c/-> :v
                                     [:c/unit :v]]]]]]
                        [:c/-> :p
                         [:c/unit :p]]]]]])
                   (fn [parser]
                       (parse parser))))
    (def p4 (mbind (type->value ctx
                                [:c/prod
                                 [:base/item]
                                 [:base/item]])
                   (fn [parser]
                     (parse parser))))
    (def p5 (mbind (type->value ctx
                                [:c/bind
                                 [:c/apply
                                  [:f/apply]
                                  [[:f/f->p]
                                   [:c/-> :i
                                    [:c/unit :i]]]]
                                 [:c/-> :p
                                  [:c/bind [:c/identity :p]
                                   [:c/-> :res
                                    [:c/unit :res]]]]])
                   (fn [parser]
                     (parse parser))))
    (def p6 (mbind (type->value ctx
                                [:c/apply
                                 [:f/apply]
                                 [[:c/-> :i
                                   [:c/unit :i]]
                                  :a]])
                   (fn [parser]
                     (parse parser))))
    (p5 [:1 :2 :3 :d :e :f :g :h])))



