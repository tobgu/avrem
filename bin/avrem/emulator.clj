(ns avrem.emulator
  (:use avrem.hexparser)
  (:use avrem.instructions)
  (:use avrem.bitmatch))

; Stolen debug macro
(defmacro local-dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
;(defmacro local-dbg[x] x)

(defn make-zero-vector [size]
  (vector (take size (repeat 0))))

(defn execute-op [program state]
  (match-and-execute (subvec program (:program-counter state)) state
                     [0x3 6 :arg 2] add-lsl
                     [0x6 4] ori-sbr
                     [:_ 8] the-rest))

(defn run [file-name]
  (let [program (read-hex-file file-name)]
    (loop [state {:memory (make-zero-vector 10),
                  :registers (make-zero-vector 32),
                  :stack (make-zero-vector 10),
                  :stack-pointer 0,
                  :program-counter 0}]
;      (local-dbg [state])
      (let [prog-count (:program-counter state)
            prog-size (count program)]
        (println prog-count prog-size)
        (if (< prog-count prog-size)
          (recur (execute-op program state))
          :done)))))


(defn run-emulator-example []
  (run "./resources/blink.hex"))