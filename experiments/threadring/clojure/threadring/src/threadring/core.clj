(ns threadring.core
  (:gen-class))

(set! *warn-on-reflection* true)

(defn pass-message
  [the-agent
   ^Integer value
   first-agent]
  (if (zero? value)
    (do (println (:num the-agent))
      (shutdown-agents)
      (System/exit 0))
    ; Pass to the next agent if we're not at the end of the line,
    ; otherwise back to the first.
    (send (if (:next the-agent) (:next the-agent) first-agent)
          pass-message (dec value) first-agent))
  the-agent)

(defn pass-messages
  [^Integer num-agents
   ^Integer num-messages]
  (let [first-agent (reduce (fn [next-agent agent-num]
                              (agent {:next next-agent :num agent-num}))
                            nil
                            (range num-agents 0 -1))]
    (send first-agent pass-message num-messages first-agent)))

(defn -main [& args]
  (let [num-messages (if (empty? args)
                       1000
                       (Integer/valueOf (first args)))]
    (pass-messages 503 num-messages)))
