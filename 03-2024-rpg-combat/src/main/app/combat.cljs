
(ns app.combat
  (:refer-clojure :exclude [object? char?]))

(def levels
  {2	1000
   3	3000
   4	6000
   5	10000
   6	15000
   7	21000
   8	28000
   9	36000
   10	45000
   })

;; status - :dead :alive

(def max-health-1 1000)
(def max-health-6 1500)

(def char-status #{:alive :dead})
(def magic-obj-status #{:intact :destroyed})
(def magic-obj-type #{:healing :weapon})

(def default-char-status :alive)
(def default-obj-status :intact)

(defrecord Character [name level health max-health status factions exp])
(defrecord MagicObject [name type health max-health damage status])

(defn create-char
  ([name]
   (->Character name 1 max-health-1 max-health-1 default-char-status #{} 0))
  ([name start-health]
   (let [start-health (if (> start-health max-health-1) max-health-1 start-health)]
     (->Character name 1 start-health max-health-1 default-char-status #{} 0))))

(defn create-magic-obj
  ([name type max-health]
   (->MagicObject name type max-health max-health 0 default-obj-status))
  ([name type max-health damage]
   (assert (= type :weapon))
   (->MagicObject name type max-health max-health damage default-obj-status)))

(defn char?
  [char]
  (instance? Character char))

(defn object?
  [obj]
  (instance? MagicObject obj))

(defn healing-object?
  [obj]
  (and (object? obj) (= :healing (:type obj))))

(defn weapon?
  [obj]
  (and (object? obj) (= :weapon (:type obj))))

(defn kill [char] (assoc char :status :dead))
(defn health [{:keys [health]}] health)
(defn alive? [{:keys [status]}] (= status :alive))
(defn dead? [{:keys [status]}] (= status :dead))
(defn destroyed? [{:keys [status]}] (= status :destroyed))

;; Level
(defn level [{:keys [level]}] level)

(defn level-up
  ([char]
   (if (= (:level char) 10)
     char
     (let [char (update char :level inc)]
       (cond-> char
         (>= (:level char) 6)
         (assoc :max-health 1500)))))
  
  ([char to-level]
   (->> (range (:level char) to-level)
        (reduce #(level-up %1) char))))


;; factions
(defn factions [{:keys [factions]}]
  (vec factions))

(defn faction? [{:keys [factions]} check-faction]
  (contains? factions check-faction))

(defn join-faction [char faction]
  (-> char
      (update :factions conj faction)))

(defn leave-faction [char faction]
  (-> char
      (update :factions disj faction)))

(defn allies? [c1 c2]
  (or (= c1 c2) ;; always is ally of itself
      (boolean (some (:factions c2) (:factions c1)))))


(declare damage)

;; Actions
(defn attack
  ([source target]
   (assert (weapon? source))
   (let [[source target] (attack source target (:damage source))] 
     [(damage source 1) target]))
  
  ([source target amount]
   (if (or (= source target)
           (allies? source target)
           (and (object? source) (not (weapon? source))))
     [source target]
     ;; target 1, source 1... target - 5 1
     (let [amount (if (>= (- (:level target) 5) (:level source))
                    (- amount (* amount 0.5))
                    amount)

           amount (if (>= (- (:level source) 5) (:level target))
                    (+ amount (* amount 0.5))
                    amount)

           target (-> target
                      (update :health - amount)
                      (update :exp + amount))

           target (cond-> target
                    (<= (:health target) 0)
                    (-> (assoc :health 0)
                        (assoc :status :dead)))

           target (cond-> target
                    (and (not (dead? target))
                         (>= (:exp target) (get levels (inc (:level target)))))
                    (level-up))
           ]
       [source target]))))

(defn heal [source target amount]
  (cond
    (or (object? target)
        (dead? source)
        (and (not (healing-object? source))
             (not (allies? source target))))
    [source target]

    (= source target)
    (let [source (update source :health #(min (+ % amount) (:max-health source)))]
      [source source])

    :else
    (let [amount (if (object? source) (min amount (:max-health source)) amount)]
      [source (update target :health #(min (+ % amount) (:max-health target)))])))

(defn damage
  [obj amount]
  (let [obj (update obj :health #(max 0 (- % amount)))]
    (cond-> obj
      (<= (:health obj) 0)
      (assoc :status :destroyed))))
