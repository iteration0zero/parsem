(ns parsem.bnf
  (:require [parsem.type :as type]
            [parsem.parser :as parser]
            [parsem.setup :refer :all]))

(def bnf-g
  {:bnf/alpha [:pred
               (into #{}
                 (map char (range 65 123)))]
   :bnf/num [:pred
             (into #{}
               (map char (range 48 57)))]
   :space [:pred
           #{\space}]
   :tab [:pred
          #{\tab}]
   :newline [:pred
             #{\newline}]
   :bnf/alphanum [:prod
                  [:bnf/alpha]
                  [:list
                   [:sum
                    [:bnf/alpha]
                    [:bnf/num]]]]
   :bnf/ascii [:pred
               (into #{}
                 (concat (map char (range 65 123))
                         (map char (range 48 57))
                         [\! \? \. \, \- \_ \; \: \= \& \( \) \{ \} \[ \]]))]
   :bnf/asciie [:pred
                (into #{}
                  (concat (map char (range 65 123))
                          (map char (range 48 57))
                          [\! \? \. \, \- \_ \; \: \= \& \( \) \{ \} \[ \| \| \< \> \^ \] \+ \* \/ \% \~ \,]))]
   :bnf/mascii [:prod
                [:seq [:bnf/ascii]]
                [:list
                 [:bnf/ascii]]]
   :bnf/masciie [:prod
                 [:seq [:bnf/asciie]]
                 [:list
                  [:bnf/asciie]]]
   :bnf/identifier [:prod
                    [:string \<]
                    [:bnf/mascii]
                    [:string \>]]
   :bnf/terminal [:prod
                  [:string \"]
                  [:bnf/masciie]
                  [:string \"]]
   :bnf/sep [:prod
             [:sum
              [:space]
              [:tab]
              [:newline]]
             [:list
              [:sum
               [:space]
               [:tab]
               [:newline]]]]
   :bnf/sep-l [:list
               [:sum
                [:space]
                [:tab]
                [:newline]]]
   :bnf/eor (into [:string] "--EOR--")
   :bnf/bor (into [:string] "--BOR--")
   :bnf/definition_op (into [:string] "::=")
   :bnf/alternative_op (into [:string] "::+")
   :bnf/op [:sum
            [:bnf/definition_op]
            [:bnf/alternative_op]]
   :bnf/prod [:prod
              [:bnf/syntactic_unit_alt]
              [:bnf/sep-l]
              [:sum
               [:bnf/prod]
               [:bnf/syntactic_unit_alt]]]
   :bnf/sum [:prod
             [:sum
              [:bnf/prod]
              [:bnf/syntactic_unit_alt]]
             [:bnf/sep-l]
             [:string \|]
             [:bnf/sep-l]
             [:bnf/syntactic_unit]]
   :bnf/group [:prod
               [:string \{]
               [:bnf/sep-l]
               [:bnf/syntactic_unit]
               [:bnf/sep-l]
               [:string \}]]
   :bnf/opt [:prod
             [:string \[]
             [:bnf/sep-l]
             [:bnf/syntactic_unit]
             [:bnf/sep-l]
             [:string \]]]
   :bnf/star [:string \*]
   :bnf/rep [:string \+]
   :bnf/mod [:sum
             [:bnf/star]
             [:bnf/rep]]
   :bnf/mod_unit [:prod
                  [:sum
                   [:bnf/identifier]
                   [:bnf/terminal]
                   [:bnf/group]
                   [:bnf/opt]]
                  [:bnf/mod]]
   :bnf/syntactic_unit_alt [:sum
                            [:bnf/mod_unit]
                            [:bnf/identifier]
                            [:bnf/terminal]
                            [:bnf/group]
                            [:bnf/opt]]
   :bnf/syntactic_unit [:sum
                        [:bnf/sum]
                        [:bnf/prod]
                        [:bnf/syntactic_unit_alt]]
   :bnf/lhs [:bnf/identifier]
   :bnf/rhs [:bnf/syntactic_unit]
   :bnf/rule [:prod
              [:bnf/bor]
              [:bnf/sep-l]
              [:bnf/lhs]
              [:bnf/sep]
              [:bnf/op]
              [:bnf/sep]
              [:bnf/rhs]
              [:bnf/sep-l]
              [:bnf/eor]]
   :bnf/rules [:prod
               [:bnf/sep-l]
               [:bnf/rule]
               [:list
                [:prod
                 [:bnf/sep-l]
                 [:bnf/rule]]]
               [:bnf/sep-l]]})

(comment
  (in-ns 'parsem.bnf)
  (let [ts (merge type/base-types
             type/type-combinators
             type/ast-combinators
             type/aux-combinators
             (type/g->types bnf-g))]
    ((parser/type->p ts [:bnf/rules])
     (slurp "resources/idl.ebnf"))))