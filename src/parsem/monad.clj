(ns parsem.monad
  (:require [clojure.spec.alpha :as spec])
  (:import (clojure.lang ISeq)))

(defprotocol IMonad
  (unit [_ a])
  (bind [_ a f]))

(defprotocol IMonadZeroPlus
  (zero [_])
  (plus [_ a b]))

(defprotocol IMonadDetPlus
  (dplus [_ a b]))

(defprotocol IMonadStateOp
  (mfetch [_])
  (mset [_ s])
  (mupdate [_ f]))

(def mex
  (reify
    IMonad
    (unit [_ a] a)
    (bind [_ a f]
      (if (= a :f)
        :f
        (f a)))
    IMonadZeroPlus
    (zero [_] :f)
    (plus [_ a b]
      (if (= a :f)
        b
        a))))

(def mseq
  (reify
    IMonad
    (unit [_ a] [a])
    (bind [this as f]
      (into [] (mapcat f as)))
    IMonadZeroPlus
    (zero [_] [])
    (plus [this as bs]
      (concat as bs))))

(defn mstatep [m]
  (reify
    IMonad
    (unit [_ a]
      (fn [s]
          (if (sequential? s)
            (unit m [a s])
            (zero m))))
    (bind [_ a f]
      (fn [s]
        (if (sequential? s)
          (bind m
            (a s)
            (fn [[v s']]
              ((f v) s')))
          (zero m))))
    IMonadZeroPlus
    (zero [_]
      (fn [s]
        (zero m)))
    (plus [_ a b]
      (fn [s]
        (if (sequential? s)
          (plus m
                (a s)
                (b s))
          (zero m))))
    IMonadDetPlus
    (dplus [this a b]
      (fn [s]
        (if (sequential? s)
          (let [r (a s)]
            (if (= ((zero this) s) r)
              (b s)
              r))
          (zero m))))
    IMonadStateOp
    (mfetch [this]
      (mupdate this identity))
    (mset [this s]
      (mupdate this (fn [_] s)))
    (mupdate [_ f]
      (fn [s]
        (if (sequential? s)
          (if (f s)
            (unit m [s (f s)])
            (zero m))
          (zero m))))))
