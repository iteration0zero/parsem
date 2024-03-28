(ns parsem.bnf
  (:require [parsem.type :as type]
            [parsem.parser :as parser]
            [parsem.setup :refer :all]))

(def bnf-g
  {:bnf/alpha [:base/pred
               (into #{}
                 (map char (range 65 123)))]
   :bnf/num [:base/pred
             (into #{}
               (map char (range 48 57)))]
   :space [:base/pred
           #{\space}]
   :tab [:base/pred
          #{\tab}]
   :newline [:base/pred
             #{\newline}]
   :bnf/alphanum [:c/prod
                  [:bnf/alpha]
                  [:c/list
                   [:c/sum
                    [:bnf/alpha]
                    [:bnf/num]]]]
   :bnf/ascii [:base/pred
               (into #{}
                 (concat (map char (range 65 123))
                         (map char (range 48 57))
                         [\! \? \. \, \- \_ \; \: \= \& \( \) \{ \} \[ \]]))]
   :bnf/asciie [:base/pred
                (into #{}
                  (concat (map char (range 65 123))
                          (map char (range 48 57))
                          [\! \? \. \, \- \_ \; \: \= \& \( \) \{ \} \[ \| \| \< \> \^ \] \+ \* \/ \% \~ \,]))]
   :bnf/mascii [:c/prod
                [:bnf/ascii]
                [:c/list
                 [:bnf/ascii]]]
   :bnf/masciie [:c/prod
                 [:bnf/asciie]
                 [:c/list
                  [:bnf/asciie]]]
   :bnf/identifier [:c/prod
                    [:base/string \<]
                    [:bnf/mascii]
                    [:base/string \>]]
   :bnf/terminal [:c/prod
                  [:base/string \"]
                  [:bnf/masciie]
                  [:base/string \"]]
   :bnf/sep [:c/prod
             [:c/sum
              [:space]
              [:tab]
              [:newline]]
             [:c/list
              [:c/sum
               [:space]
               [:tab]
               [:newline]]]]
   :bnf/sep-l [:c/list
               [:c/sum
                [:space]
                [:tab]
                [:newline]]]
   :bnf/eor (into [:base/string] "--EOR--")
   :bnf/bor (into [:base/string] "--BOR--")
   :bnf/definition_op (into [:base/string] "::=")
   :bnf/alternative_op (into [:base/string] "::+")
   :bnf/op [:c/sum
            [:bnf/definition_op]
            [:bnf/alternative_op]]
   :bnf/prod [:c/prod
              [:bnf/syntactic_unit_alt]
              [:bnf/sep-l]
              [:c/sum
               [:bnf/prod]
               [:bnf/syntactic_unit_alt]]]
   :bnf/sum [:c/prod
             [:c/sum
              [:bnf/prod]
              [:bnf/syntactic_unit_alt]]
             [:bnf/sep-l]
             [:base/string \|]
             [:bnf/sep-l]
             [:bnf/syntactic_unit]]
   :bnf/group [:c/prod
               [:base/string \{]
               [:bnf/sep-l]
               [:bnf/syntactic_unit]
               [:bnf/sep-l]
               [:base/string \}]]
   :bnf/opt [:c/prod
             [:base/string \[]
             [:bnf/sep-l]
             [:bnf/syntactic_unit]
             [:bnf/sep-l]
             [:base/string \]]]
   :bnf/star [:base/string \*]
   :bnf/rep [:base/string \+]
   :bnf/mod [:c/sum
             [:bnf/star]
             [:bnf/rep]]
   :bnf/mod_unit [:c/prod
                  [:c/sum
                   [:bnf/identifier]
                   [:bnf/terminal]
                   [:bnf/group]
                   [:bnf/opt]]
                  [:bnf/mod]]
   :bnf/syntactic_unit_alt [:c/sum
                            [:bnf/mod_unit]
                            [:bnf/identifier]
                            [:bnf/terminal]
                            [:bnf/group]
                            [:bnf/opt]]
   :bnf/syntactic_unit [:c/sum
                        [:bnf/sum]
                        [:bnf/prod]
                        [:bnf/syntactic_unit_alt]]
   :bnf/lhs [:bnf/identifier]
   :bnf/rhs [:bnf/syntactic_unit]
   :bnf/rule [:c/prod
              [:bnf/bor]
              [:bnf/sep-l]
              [:bnf/lhs]
              [:bnf/sep]
              [:bnf/op]
              [:bnf/sep]
              [:bnf/rhs]
              [:bnf/sep-l]
              [:bnf/eor]]
   :bnf/rules [:c/list
               [:c/prod
                [:bnf/sep-l]
                [:bnf/rule]
                [:bnf/sep-l]]]})

(comment
  (in-ns 'parsem.bnf)
  (let [ts (merge type/base-types
                  type/type-combinators
                  type/aux-combinators
                  (type/g->types type/dtype-grammar)
                  (type/g->types type/type-grammar)
                  (type/g->types type/grammar-grammar)
                  (type/g->types type/parser-grammar)
                  (type/g->types bnf-g)
                  (type/g->types type/bnf-to-idl-grammar))]
      (def p1 (mbind (parser/applyp
                      (mbind (parser/typep ts)
                             (fn [v]
                                 (mbind v
                                        (fn [v]
                                            (munit v)))))
                      [:bnf/identifier])
                     (fn [v]
                         (mbind v
                                (fn [v]
                                    (munit v))))))

      (p1 (seq "<definition>"))
      #_(def p4 (parser/redp parser/pitem
                             {:open #{}
                              :closed #{}}
                             (fn [acc i]
                               (mplus (mbind (applyp (pset ) [i])
                                        (fn [v]
                                            (munit (conj acc v))))
                                 (munit acc)))))
      #_(slurp "resources/idl_sl.ebnf")
      #_((parser/type->p ts [:bnf/rules])
         (seq (slurp "resources/idl.ebnf")))))
