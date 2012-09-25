(ns avrem.bitmatch
  (:use clojure.test)
  (:use midje.sweet)
  (require [clojure.math.numeric-tower :as math]))

; Stolen debug macro
;(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
(defmacro dbg[x] x)

(defn byte-align-mask [mask matchpattern args masklen]
  (let [fillBits (rem (- 8 (rem masklen 8)) 8)]
    [(/ (+ masklen fillBits) 8)
     (bit-shift-left mask fillBits) 
     (bit-shift-left matchpattern fillBits)
     (dbg (into [] (map (fn [[pat len pos]]
            [pat len (- (+ masklen fillBits) (+ pos len))]) args)))]))

(defn to-bitmask [len]
  (- (math/expt 2 len) 1))

(defn makemask [pat len]
  (if (keyword? pat)
    0
    (to-bitmask len)))

(defn makematch [pat len]
  (if (keyword? pat)
    0
    (bit-and pat (to-bitmask len)))) ; Only take the specified bits into account

(defn store-arg [args pat len pos]
  (if (keyword? pat)
    (into args [[pat len pos]])
    args))

; Make a bitmask to extract the interesting elements and ignore those that are not interesting
; based on the pattern described by all-patterns
(defn maskm [all-patterns]
  (loop [masklen 0
         mask 0
         matchpattern 0
         args []
         remain all-patterns]
    (let [pattern (first remain)
          len (first (rest remain))]
      (if pattern
        (recur (+ masklen len) 
               (bit-or (bit-shift-left mask len) (makemask pattern len))
               (bit-or (bit-shift-left matchpattern len) (makematch pattern len))
               (store-arg args pattern len masklen)
               (rest (rest remain)))
        (dbg (byte-align-mask mask matchpattern args masklen)))))) ; Fill up to byte boundary


(defmacro bitvector [bytevector length]
    (dbg (loop [len length
           bits []]
      (if (> len 0)
        (recur (- len 1)
               (cons `(bit-shift-left (nth ~bytevector ~(- len 1)) ~(* 8 (- length len))) bits))
        `(reduce bit-or 0 [~@bits])))))


; Create a map containing the specified arguments mapped to their corresponding data
(defn make-args [bits args]
  (loop [argmap {}
         rem-args args]
    (if (empty? rem-args)
      argmap
      (let [[pat len pos] (first rem-args)]
        (recur (assoc argmap pat (bit-and (to-bitmask len) (bit-shift-right bits pos)))
               (rest rem-args))))))

  
(defmacro maskmacro [matchpat bytevector]
  (let [[length mask pattern args] (maskm matchpat)]
    (dbg [length mask pattern args])
    `(let [bits# (dbg (bitvector ~bytevector ~length))]
         (if (= (bit-and bits# ~mask) ~pattern)
           (dbg (make-args bits#  ~args))
           ))))

(defmacro match-and-execute [bytevector additional-argument & clauses]
  (dbg [bytevector additional-argument clauses])
  ; Must be at least two elements in the clauses to execute
  (when (second clauses)
    (let [arg-spec (first clauses)
          fun-to-call (second clauses)
          rest-clauses (nnext clauses)]
      `(if-let [argument# (maskmacro ~arg-spec ~bytevector)]
         (~fun-to-call argument# ~additional-argument)
         (match-and-execute ~bytevector ~additional-argument ~@rest-clauses)))))

; Tests
;(defn testfn [x y] (print x) x)

(fact "First clause match"
      (match-and-execute [255] :state
                         [:arg 8] #(identity {:first %1, :second %2})) =>
      '{:first {:arg 255}, :second :state})

(fact "Second clause match"
      (match-and-execute [255] :state
                         [2 2] #(identity [:unexpected %1 %2]),
                         [:arg 8] #(identity {:first %1, :second %2})) =>
      '{:first {:arg 255}, :second :state})

(fact "No clause match"
      (match-and-execute [255] :state
                         [2 2] #(identity [:unexpected %1 %2]),
                         [3 3 :_ 4] #(identity {:first %1, :second %2})) => nil)

(fact "Basic, single byte"
      (maskmacro [:arg 8] [255]) => '{:arg 255}
      (maskmacro [:arg 4] [255]) => '{:arg 15}
      (maskmacro [:_ 4 :arg 4] [255]) => '{:arg 15 :_ 15}
      (maskmacro [:_ 4 :arg 2 3 2] [255]) => '{:arg 3 :_ 15}
      (maskmacro [2 2] [255]) => 'nil
      (maskmacro [0 8] [255]) => 'nil
      (maskmacro [] [255]) => '{}
      (maskmacro [0 0] [255]) => '{}
      (maskmacro [3 2] [255]) => '{}
      (maskmacro [2 2] [128]) => '{}
      (maskmacro [2 2] [135]) => '{}
      (maskmacro [2 2 :arg 6] [135]) => '{:arg 7}
      (maskmacro [2 2, 2 2, 2 2, 2 2] [(+ 128 32 8 2)]) => '{}
      (maskmacro [1 2, 1 2, 1 2, 1 2] [(+ 64 16 4 1)]) => '{}
      (maskmacro [2 2 :arg 6] [135]) => '{:arg 7}
      (maskmacro [:arg1 3, :arg2 3, :arg3 2] [(+ 32 8 3)]) => '{:arg1 1, :arg2 2, :arg3 3})
;(fact (maskmacro [1 24] [1]) => '{}) will produce an exception, OK?

(fact "Multi byte"
      (maskmacro [:arg 16] [255 255]) => '{:arg 65535}
      (maskmacro [:arg 16] [1 2]) => '{:arg 258}
      (maskmacro [:arg 16] [255 255]) => '{:arg 65535}
      (maskmacro [0 7 :arg 7]  [1 2]) => '{:arg 64}
      (maskmacro [0 10 :arg 3 0 3] [0 40]) => '{:arg 5}
      (maskmacro [0 10 :arg 3 0 3] [0 42]) => 'nil
      (maskmacro [:arg 16] [1 2]) => '{:arg 258})

