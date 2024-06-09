(ns parsem.parser
  (:require [parsem.setup :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSERS                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn empty-check [f]
    (fn [s]
        (if (not (empty? s))
            (f s))))

;; Takes a parser and an input sequence
;; and returns a parser which uses the monadic state operations mfetch and mset
;; to temporarily set the state to the specified input sequence,
;; apply the parser to it, reset the state and return the obtained result.
(defn applyp [parser sequence]
  (mbind mfetch
    (fn [sequence']
      (mbind (mset sequence)
        (fn [_]
          (mbind parser
            (fn [value]
              (mbind (mset sequence')
                (fn [_]
                  (munit value))))))))))


;; A parser which consumes one item of an input sequence.
(def pitem
  (mbind (update-s (empty-check rest))
         (comp munit first)))

;; A parser which checks if an item is contained in a set.
(defn pset [set]
  (mbind pitem
    (fn [value]
      (if (get set value)
        (munit value)
        mzero))))

;; A parser which checks if a given predicate is applicable.
(defn ppred [pred]
  (mbind pitem
    (fn [value]
      (if (pred value)
        (munit value)
        mzero))))

;; A parser which checks for equivalence of an item with a character.
(defn pchar [character]
  (pset #{character}))

;; A parser for alphabetic characters
(def alphap
  (pset (into #{} (map char (range 65 123)))))

;; A parser for numeric characters
(def nump
  (pset (into #{} (map char (range 48 57)))))

;; A parser which takes
;; an association 'types' of 'type-name' -> 'types' -> 'type-parser'
;; and returns the respective 'type-parser'.
(defn typep [types]
  (mbind (ppred types)
    (fn [type-name]
      ((types type-name) types))))

;; A parser which returns a list of results of 'parsera',
;; possibly empty.
(defn listp [parsera]
  (mplus
    (mbind
      parsera
      (fn [value]
        (mbind
          (listp parsera)
          (fn [value']
            (munit (cons value value'))))))
    (munit [])))


;; A parser which parses the characters of
;; a given input string s.
(defn pstring [s]
  (if (not (empty? s))
    (mbind
      (pchar (first s))
      (fn [value]
        (mbind (pstring (rest s))
               (fn [value']
                 (munit (cons value value'))))))
    (munit '())))

(defn redp [elemp acc rfp]
  (mplus
    (mbind elemp
      (fn [value]
        (mbind (applyp rfp [acc])
               (fn [rfp']
                 (mbind (applyp rfp' [value])
                        (fn [value']
                          (redp elemp value' rfp)))))))
    (munit acc)))

(defn filterp [elemp predp]
  (redp elemp
    []
    (mbind pitem
           (fn [acc]
             (munit (mbind pitem
                           (fn [value]
                             (mplus (mbind (applyp predp [value])
                                           (fn [value]
                                             (munit (conj acc value))))
                                    (munit acc)))))))))

(defn mapp [parsera fp]
  (redp parsera
    []
    (mbind pitem
           (fn [acc]
             (munit (mbind pitem
                           (fn [value]
                             (mbind (applyp fp [value])
                                    (fn [value]
                                      (munit (conj acc value)))))))))))

(comment
  (in-ns 'parsem.parser)
  ((mapp pitem
     (mbind pitem
       (fn [v]
         (munit v))))
   "test")
  (let [ts (types base-types)
        ts (types ts type-combinators)
        gts (g->types ts test-g)]
    ((type->p gts [:s])
     "Ieat")))




