(ns parsem.type
  (:require [parsem.setup :refer :all]
            [parsem.util :refer :all]
            [parsem.parser :as p]))

(defn type->value [types]
  (fn [type]
    (p/applyp
     (mbind (p/typep types)
            (fn [type-args->type-value]
                (mbind type-args->type-value
                       (fn [type-value]
                           (munit type-value)))))
     type)))

(defn parse [parser]
  (mbind
   parser
   (fn [value]
       (munit value))))

(def prod
  (fn [types]
    (fn [arg-types]
      (if (empty? arg-types)
        (munit [])
        (mbind ((type->value types) (first arg-types))
          (fn [parser]
            (mbind (parse parser)
              (fn [value]
                (mbind ((prod types)
                        (rest arg-types))
                  (fn [value']
                    (munit (cons value value'))))))))))))

(def sum
  (fn [types]
    (fn [arg-types]
      (if (empty? arg-types)
        mzero
        (mbind ((type->value types) (first arg-types))
          (fn [parser]
            (mplus parser
              ((sum types)
               (rest arg-types)))))))))


(defn -> [types]
  (fn [arg type]
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
                 (mbind ((type->value types) type')
                        (fn [parser]
                          (parse parser)))))))))


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
  {:f/apply [:c/-> :f
             [:c/unit
              [:c/-> :s
               [:c/apply
                [:c/bind [:base/item]
                 :f]
                [:c/seq :s]]]]]})

(def type-combinators
  {:c/list (fn [types]
             (mlet [[:type (fn [_] p/pitem)]
                    [:parsed-type (fn [{:keys [type]}]
                                    ((type->value types) type))]]
               (fn [{:keys [parsed-type]}]
                 (munit (p/listp parsed-type)))))
   :c/sum (fn [types]
            (mlet [[:ts (fn [_] (p/listp p/pitem))]]
              (fn [{:keys [arg-types]}]
                (munit ((sum types) arg-types)))))
   :c/prod (fn [types]
             (mlet [[:ts (fn [_] (p/listp p/pitem))]]
               (fn [{:keys [arg-types]}]
                 (munit ((prod types) arg-types)))))
   :c/-> (fn [types]
           (mlet [[:arg (fn [_] p/pitem)]
                  [:type (fn [_] p/pitem)]]
             (fn [{:keys [arg type]}]
               (munit ((-> types) arg type)))))
   :c/bind (fn [types]
             (mlet [[:type (fn [_] p/pitem)]
                    [:parsed-type (fn [{:keys [type]}]
                                    ((type->value types) type))]
                    [:ftype (fn [_] p/pitem)]
                    [:parsed-ftype (fn [{:keys [ftype]}]
                                     ((type->value types) ftype))]]
               (fn [{:keys [parsed-type parsed-ftype]}]
                 (munit ((bind types) parsed-type parsed-ftype)))))
   :c/unit (fn [types]
             (mlet [[:arg (fn [_] p/pitem)]]
               (fn [{:keys [arg]}]
                 (munit (munit arg)))))
   :c/parser (fn [types]
               (mlet [[:parser (fn [_] p/pitem)]]
                 (fn [{:keys [parser]}]
                   (munit parser))))
   :c/apply (fn [types]
              (mlet [[:type (fn [_] p/pitem)]
                     [:sequence (fn [_] p/pitem)]
                     [:parsed-type (fn [{:keys [type]}]
                                     (p/applyp (p/typep types) type))]]
                (fn [{:keys [parsed-type sequence]}]
                  (prn "apply")
                  (prn "parsed-type: " parsed-type)
                  (prn "sequence: " sequence)
                  (munit
                   (p/applyp
                     (mbind parsed-type
                            (fn [parser]
                              (prn "apply parser")
                              (prn parser)
                              (parse parser)))
                     sequence)))))})


(def dtype-grammar
  {:dtype/keyword [:base/pred keyword?]})

(def to-dtype-combinators
    {:to-dtype/keyword (fn [_]
                           (mlet [[:name-seq (fn [_] p/pitem)]]
                                 (fn [{:keys [name-seq]}]
                                     (munit (keyword (apply str name-seq))))))})

(defn grammar->ctxlsdtypes [grammar]
  (reduce (fn [accumulated-val [key value]]
            (assoc accumulated-val
              key
              (fn [types]
                ((type->value types) value))))
          {}
    grammar))

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
   :c/reduce
   (fn [types]
     (mlet
       [[:acctype (fn [_] p/pitem)]
        [:rfptype (fn [_] p/pitem)]
        [:type (fn [_] p/pitem)]
        [:parsed-acctype (fn [{:keys [acctype]}]
                           ((type->value types) acctype))]
        [:parsed-rfptype (fn [{:keys [rfptype]}]
                           ((type->value types) rfptype))]
        [:parsed-type (fn [{:keys [type]}]
                        ((type->value types) type))]]
       (fn [{:keys [parsed-acctype parsed-rfptype parsed-type]}]
           (munit
            (mbind parsed-type
                   (fn [value]
                     (p/applyp
                       (p/redp
                         p/pitem
                         parsed-acctype
                         parsed-rfptype)
                      value)))))))
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
             (fn [value]
               (p/applyp (p/filterp
                           p/pitem
                           (mbind p/pitem
                             (fn [value]
                               (p/applyp parsed-ftype
                                 value))))
                 value)))))))
   :c/map
   (fn [types]
     (mlet
         [[:mtype (fn [_]
                    p/pitem)]
          [:type (fn [_]
                   p/pitem)]
          [:parsed-type (fn [{:keys [type]}]
                          ((type->value types) type))]
          [:parsed-mtype (fn [{:keys [mtype]}]
                           ((type->value types) mtype))]]
       (fn [{:keys [parsed-mtype parsed-type]}]
         (munit
           (mbind parsed-type
             (fn [value]
               (p/applyp (p/mapp
                           p/pitem
                           (mbind p/pitem
                             (fn [value]
                               (p/applyp parsed-mtype
                                 value))))
                 value)))))))})

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
                                [:c/bind [:c/seq :y]
                                 [:c/-> :z
                                  [:c/unit :z]]]]]]]})

(def idl-grammar
    [:idl/specification [:c/prod [:idl/definition] [:c/list [:idl/definition]]]])

(comment
  (in-ns 'parsem.type)
  (let [types (merge base-types
                     type-combinators
                     aux-combinators
                     to-dtype-combinators
                     (grammar->ctxlsdtypes dtype-grammar)
                     (grammar->ctxlsdtypes type-grammar)
                     (grammar->ctxlsdtypes grammar-grammar)
                     (grammar->ctxlsdtypes parser-grammar)
                     (grammar->ctxlsdtypes to-grammar-grammar)
                     (grammar->ctxlsdtypes bnf-to-idl-grammar)
                     (grammar->ctxlsdtypes function-types))]
    (def p0 (mbind ((type->value types)
                    [:c/bind
                     [:c/apply
                      [:base/type]
                      [:base/item]]
                     [:c/-> :x
                      [:c/bind [:c/parser :x]
                       [:c/-> :y
                        [:c/bind [:c/parser :y]
                         [:c/-> :z
                          [:c/unit :z]]]]]]])
                   (fn [parser]
                     (prn "parser:" parser)
                     (parse parser))))
    (def p1 (mbind ((type->value types)
                    [:base/item])
                   (fn [parser]
                     (parse parser))))
    (def p2 (mbind ((type->value types)
                    [:parser/parser])
                   (fn [parser]
                     (parse parser))))
    (def p3 (mbind ((type->value types)
                    [:bnf/identifier])
                   (fn [parser]
                     (parse parser))))

    (p0 [[:a]])))




