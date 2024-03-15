(ns parsem.test.core
  (:require [clojure.test :refer :all]
            [parsem.parser :as p]))

(testing "Sample test"
  (is (seqable? (p/applyp p/alphap 5))))

(comment
  (p/applyp p/alphap \a))