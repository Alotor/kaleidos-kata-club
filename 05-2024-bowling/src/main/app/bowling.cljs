(ns app.bowling)

(defrecord Game [players scores])

(defn new-game []
  (Game. {} {}))

(defn add-player [game player-name]
  (let [id (js/Symbol)]
    [(update game :players assoc player-name id)
     id]))

(defn throw-ball
  [game player-id round throw score]
  ;; Validate that player exists
  
  ;; Todo validate that there are enough pins to score

  ;; validate that the throw number is 1 or 2, and 3 if last round

  ;; validate round number

  (-> game
      (assoc-in [:scores player-id round throw] score)))

(defn all-throws
  [game player-id throws]
  (->> (range 0 21)
       (reduce
        (fn [game idx]
          (let [r (inc (quot idx 2))
                t (inc (rem idx 2))
                [r t] (if (= idx 20) [10 3] [r t])]
            (throw-ball game player-id r t (nth throws idx 0))))
        game)))

(defn strike-normal-bonus
  "A strike earns 10 points plus the sum of your next two shots."
  [game player-id round]
  (let [t1 (get-in game [:scores player-id (inc round) 1] 0)
        t2 (get-in game [:scores player-id (inc round) 2] 0)

        t2 (cond
             ;; Last frame with a strike will have its second shot in the second position
             (and (= t1 10) (= round 9))
             t2
             
             (= t1 10)
             (get-in game [:scores player-id (+ round 2) 1] 0)

             :else
             t2)]
    (+ t1 t2)))

(defn spare-normal-bonus
  "A spare earns 10 points plus the sum of your next one shot."
  [game player-id round]
  (get-in game [:scores player-id (inc round) 1] 0))

(defn score
  [game player-id]
  (->> (range 1 11)
       (reduce
        (fn [result round]
          (let [t1 (get-in game [:scores player-id round 1] 0)
                t2 (get-in game [:scores player-id round 2] 0)
                t3 (get-in game [:scores player-id round 3] 0)

                strike? (= t1 10)
                spare?  (and (not strike?) (= (+ t1 t2) 10))

                current
                (if (= round 10)
                  (+ t1 t2 t3)
                  (cond-> (+ t1 t2)
                    strike? (+ (strike-normal-bonus game player-id round))
                    spare?  (+ (spare-normal-bonus game player-id round))))]

            ;; (prn "?ROUND" round (+ result current) current)
            (+ result current)))
        0)))
