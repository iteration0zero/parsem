(ns parsem.setup
  (:require [parsem.monad :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defines the parser monad as the parameterised state monad
;; by the seq/list monad.
;; Therefore defines parsers as stateful, non-deterministic computations.
(def parser-monad (m/mstatep m/mseq))

;; Monadic operations
(def munit (partial m/unit parser-monad))
(def mbind (partial m/bind parser-monad))
(def mzero (m/zero parser-monad))
(def mplus (partial m/dplus parser-monad))


;; Monadic state operations
(def update-s (partial m/mupdate parser-monad))
(def mfetch (m/mfetch parser-monad))
(def mset (partial m/mset parser-monad))
