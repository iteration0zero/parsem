(ns parsem.sequence)

(defprotocol ISequence
  (first [this])
  (rest [this])
  (eos? [this]))

(extend-protocol
  ISequence
  clojure.lang.ISeq
  (first [this]
    (.first this))
  (rest [this]
    (.more this))
  (eos? [this]
    (nil? (.first this)))
  java.lang.String
  (first [this]
    (.charAt this 0))
  (rest [this]
    (.substring this 1))
  (eos? [this]
    (= 0 (.length this))))

(defmulti create-sequence
  class)

(defmethod create-sequence
  :default
  [e]
  (if (seqable? e)
    (seq e)))

(comment
 (in-ns 'parsem.sequence)
 (let [s1 (create-sequence [:test])]
   (eos? s1)))



