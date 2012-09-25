(ns avrem.hexparser
  (:import (java.io BufferedReader FileReader)))

; Parses hex data at a given address. Adds it to
; the program which is then returned.
(defn parse-data [program address data]
  (if (.isEmpty data)
    program
    (recur (assoc program 
                  address 
                  (Integer/parseInt (.substring data 0 2) 16)) 
           (+ 1 address)
           (.substring data 2))))

; Parses a line in an Intel HEX file into a vector representing the
; program
; TODO: Error checking (checksums, length, etc.)
(defn parse-hex-line [program line]
  (let [byteCount (.substring line 1  3)
        address (.substring line 3  7)
        recordType (.substring line 7  9)
        data (.substring line 9 (- (.length line) 2))]
    (println byteCount ":" address ":" recordType ":" data)
    (parse-data program (Integer/parseInt address 16) data)))

; Open Intel HEX file specified by file-name.
; Parses the file and returns a vector with bytes (as shorts) of
; the program.
(defn read-hex-file [file-name]
  (with-open [reader (BufferedReader. (FileReader. file-name))]
    (loop [program (vector-of :short), 
           lines (line-seq reader)]
      (if (not-empty lines)
        (recur 
          (parse-hex-line program (first lines))
          (rest lines))
        program))))

(defn run-example []
  (read-hex-file "./resources/blink.hex"))
