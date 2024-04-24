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
      (munit ((types type-name) types)))))

(defn type->p [types type]
  (mbind (applyp (typep types) type)
         (fn [v]
             (mbind v
                    (fn [v]
                        (munit v))))))

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

(defn redp [parsera acc rfp]
  (mplus
    (mbind parsera
      (fn [value]
        (mbind ((rfp acc) value)
          (fn [v]
            (redp parsera v rfp)))))
    (munit acc)))

(defn filterp [parsera predp]
  (redp parsera
    []
    (fn [acc]
      (fn [i]
        (mplus (mbind (applyp predp [i])
                 (fn [v]
                   (munit (conj acc v))))
          (munit acc))))))

(defn mapp [parsera fp]
  (redp parsera
    []
    (fn [acc]
      (fn [i]
        (mbind (applyp fp [i])
          (fn [v]
            (munit (conj acc v))))))))

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




