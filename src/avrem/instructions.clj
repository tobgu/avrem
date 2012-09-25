(ns avrem.instructions)

(defn inc-program-counter [state]
  (update-in state [:program-counter] inc))

(defn add-lsl [arguments state]
  (println "add-lsl: " arguments)
  (inc-program-counter state))

(defn ori-sbr [arguments state]
  (println "ori-sbr: " arguments)
  (inc-program-counter state))

(defn the-rest [arguments state]
  (println "the-rest: " arguments)
  (inc-program-counter state))