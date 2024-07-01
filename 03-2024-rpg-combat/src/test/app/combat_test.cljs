
(ns app.combat-test
  (:require
   [app.combat :as combat]
   [cljs.test :refer [deftest testing is]]))

(deftest create-char-test
  (testing "Create a new character"
    (testing "Health, starting at 1000"
      (let [char (combat/create-char "Pikachu")]
        (is (= 1000 (combat/health char)))))

    (testing "May be alive or death, starting alive"
      (let [char (combat/create-char "Pikachu")]
        (is (= true (combat/alive? char))))

      (let [char (-> (combat/create-char "Pikachu")
                     (combat/kill))]
        (is (= true (combat/dead? char))))

      (let [char (-> (combat/create-char "Pikachu")
                     (combat/kill)
                     (combat/kill))]
        (is (= true (combat/dead? char)))))))

(deftest deal-damage-test
  (testing "Characters can deal damage to other characters"
    (testing "Damage is substracted from health"
      (let [c1 (combat/create-char "Pikachu")
            c2 (combat/create-char "Charmander")
            [c1 c2] (combat/attack c1 c2 100)]
        (is (= (combat/health c1) 1000))
        (is (= (combat/health c2) 900))))

    (testing "When damage received exceeds current Health, Health becomes 0 and the character dies"
      (let [c1 (combat/create-char "Pikachu")
            c2 (combat/create-char "Charmander")
            [c1 c2] (combat/attack c1 c2 1000)]
        (is (= (combat/health c1) 1000))
        (is (= (combat/health c2) 0))
        (is (= (combat/dead? c2) true))))

    (testing "* Health cannot be less than 0"
      (let [c1 (combat/create-char "Pikachu")
            c2 (combat/create-char "Charmander")
            [c1 c2] (combat/attack c1 c2 100000)]
        (is (= (combat/health c1) 1000))
        (is (= (combat/health c2) 0))
        (is (= (combat/dead? c2) true))))

    (testing "A Character cannot Deal Damage to itself"
      (let [c1 (combat/create-char "Pikachu")
            [c1 c2] (combat/attack c1 c1 100000)]
        (is (= (= c1 c2) true))
        (is (= (combat/health c1) 1000))))))

(deftest heal-test
  (testing "A Character can Heal themselves."
    (let [c1 (combat/create-char "Pikachu" 500)
          [c1 c2] (combat/heal c1 c1 500)]
      (is (= (= c1 c2) true))
      (is (= (combat/health c1) 1000))))

  (testing "Dead characters cannot heal"
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/kill))
          c2 (combat/create-char "Charmander" 500)
          [c1 c2] (combat/heal c1 c2 500)]
      (is (= (combat/health c2) 500)))))

(deftest levels-test
  (testing "All characters have a level, starting at 1"
    (let [c1 (combat/create-char "Pikachu")]
      (is (= (combat/level c1) 1))))

  (testing "A character cannot have a health above 1000 until they reach level 6, when the max increases to 1500"
    (let [c1 (combat/create-char "Pikachu")
          [c1 _] (combat/heal c1 c1 10000)]
      (is (= (combat/level c1) 1))
      (is (= (combat/health c1) 1000)))

    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/level-up 6))
          [c1 _] (combat/heal c1 c1 10000)]
      (is (= (combat/level c1) 6))
      (is (= (combat/health c1) 1500))))

  (testing "When dealing damage:"
    (testing "If the target is 5 or more levels above the attacker, Damage is reduced by 50%"
      (let [c1 (combat/create-char "Pikachu")
            c2 (-> (combat/create-char "Charmander")
                   (combat/level-up 6))
            [c1 c2] (combat/attack c1 c2 100)]
        (is (= (combat/level c1) 1))
        (is (= (combat/level c2) 6))
        (is (= (combat/health c1) 1000))
        (is (= (combat/health c2) 950))))
    
    (testing "If the target is 5 or more lelels below the attacker, Damage is increased by 50%"
      (let [c1 (-> (combat/create-char "Pikachu")
                   (combat/level-up 6))
            c2 (combat/create-char "Charmander")
            [c1 c2] (combat/attack c1 c2 100)]
        (is (= (combat/level c1) 6))
        (is (= (combat/level c2) 1))
        (is (= (combat/health c1) 1000))
        (is (= (combat/health c2) 850))))))

(deftest factions-test
  (testing "Newly created Characters belong to no Faction."
    (let [c1 (combat/create-char "Pikachu")]
      (is (= (combat/factions c1) []))))
  (testing "A Character may Join or Leave one or more Factions."
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/join-faction "f1")
                 (combat/join-faction "f2"))]
      (is (= (combat/faction? c1 "f1") true))
      (is (= (combat/faction? c1 "f2") true))
      (is (= (count (combat/factions c1)) 2))

      (let [c1 (combat/leave-faction c1 "f1")]
        (is (= (combat/faction? c1 "f1") false))
        (is (= (combat/faction? c1 "f2") true))
        (is (= (count (combat/factions c1)) 1)))))
  
  (testing "Players belonging to the same Faction are considered Allies."
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/join-faction "electric"))
          c2 (-> (combat/create-char "Pidgey")
                 (combat/join-faction "flying"))
          c3 (-> (combat/create-char "Zapdos")
                 (combat/join-faction "electric")
                 (combat/join-faction "flying"))]

      (is (= (combat/allies? c1 c2) false))
      (is (= (combat/allies? c1 c3) true))
      (is (= (combat/allies? c2 c3) true))))

  (testing "Allies cannot Deal Damage to one another."
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/join-faction "pokemon"))
          c2 (-> (combat/create-char "Pidgey")
                 (combat/join-faction "pokemon"))

          [c1 c2] (combat/attack c1 c2 1000)]
      (is (= (combat/health c2) 1000))))
  
  (testing "Allies can Heal one another and non-allies cannot."
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/join-faction "pokemon"))
          c2 (-> (combat/create-char "Pidgey" 500)
                 (combat/join-faction "pokemon"))

          [c1 c2] (combat/heal c1 c2 500)]
      (is (= (combat/health c2) 1000)))

    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/join-faction "electric"))
          c2 (-> (combat/create-char "Pidgey" 500)
                 (combat/join-faction "flying"))

          [c1 c2] (combat/heal c1 c2 500)]
      (is (= (combat/health c2) 500)))))

(deftest magical-objects-test
  (testing "As well as Characters there are also Magical Objects"
    (testing "Magical Objects have Health. The maximum amount of Health is fixed at the time the object is created"
      (let [o1 (combat/create-magic-obj "healing item" :healing 10)]
        (is (= (combat/health o1) 10))))
    
    (testing "When reduced to 0 Health, Magical Objects are Destroyed"
      (let [o1 (-> (combat/create-magic-obj "healing item" :healing 10)
                   (combat/damage 10))]
        (is (= (combat/health o1) 0))
        (is (= (combat/destroyed? o1) true))))
    
    (testing "Magical Objects cannot be Healed by Characters"
      (let [c1 (combat/create-char "Pikachu")
            o1 (-> (combat/create-magic-obj "healing item" :healing 10)
                   (combat/damage 5))
            [c1 o1] (combat/heal c1 o1 10)]
        (is (= (combat/health o1) 5))))
    
    ;;(testing "Magical Objects do not belong to Factions; they are neutral")
    )

  (testing "Characters can gain health from a Healing Magical Object."
    (testing "Characters can gain any amount of health from the Object, up to its maximum and theirs"
      (let [c1 (combat/create-char "Pikachu" 500)
            o1 (combat/create-magic-obj "healing item" :healing 10)
            [o1 c1] (combat/heal o1 c1 100)]
        (is (= (combat/health c1) 510))))
    
    (testing "Healing Magical Objects cannot deal Damage"
      (let [c1 (combat/create-char "Pikachu")
            o1 (combat/create-magic-obj "healing item" :healing 10)
            [o1 c1] (combat/attack o1 c1 100)]
        (is (= (combat/health c1) 1000)))))

  (testing "Characters can deal Damage by using a Magical Weapon."
    (testing "These Magical Objects deal a fixed amount of damage when they are used; The amount of damage is fixed at the time the weapon is created"
      (let [c1 (combat/create-char "Pikachu")
            o1 (combat/create-magic-obj "sword" :weapon 10 100)
            [o1 c1] (combat/attack o1 c1)]
        (is (= (combat/health c1) 900))))
    
    (testing "Every time the weapon is used, the Health is reduced by 1"
      (let [c1 (combat/create-char "Pikachu")
            o1 (combat/create-magic-obj "sword" :weapon 10 100)
            [o1 c1] (combat/attack o1 c1)]
        (is (= (combat/health c1) 900))
        (is (= (combat/health o1) 9))))

    (testing "** Check the weapon can break"
      (let [c1 (combat/create-char "Pikachu")
            o1 (combat/create-magic-obj "sword" :weapon 1 100)
            [o1 c1] (combat/attack o1 c1)]
        (is (= (combat/health c1) 900))
        (is (= (combat/health o1) 0))
        (is (= (combat/destroyed? o1) true))))
    
    (testing "Magical Weapons cannot give Health to a Character"
      (let [c1 (combat/create-char "Pikachu" 500)
            o1 (combat/create-magic-obj "healing item" :weapon 10)
            [o1 c1] (combat/heal o1 c1 100)]
        (is (= (combat/health c1) 500))))))

(deftest changing-level-test
  (testing "Leveling"
    (testing "Level 1 Characters that survive 1000 damage points gain a level, (this may be counted over several battles)"
      (let [c1 (combat/create-char "Pikachu")
            c2 (combat/create-char "Charmander")
            [c1 c2] (combat/attack c1 c2 500)
            [c2 _]  (combat/heal c2 c2 500)
            [c1 c2] (combat/attack c1 c2 500) ;; enought to level!
            ]
        (is (= (combat/level c2) 2))))
    
    (testing "a character cannot gain a level while receiving damage, it happens directly afterwards (if the player is still alive)"
      (let [c1 (combat/create-char "Pikachu")
            c2 (combat/create-char "Charmander")
            [c1 c2] (combat/attack c1 c2 500)
            [c1 c2] (combat/attack c1 c2 500) ;; enought to level! but is dead
            ]
        (is (= (combat/dead? c2) true))
        (is (= (combat/level c2) 1))))
    
    (testing "Level 2 Characters need to survive an additional 2000 damage points to gain a level, Level 3 Characters need to survive an additional 3000, and so on."
      (let [c1 (combat/create-char "Pikachu")
            c2 (combat/create-char "Charmander")

            [c1 c2] (combat/attack c1 c2 500)
            [c2 _]  (combat/heal c2 c2 500)

            [c1 c2] (combat/attack c1 c2 500) ;; level 1
            [c2 _]  (combat/heal c2 c2 500)

            [c1 c2] (combat/attack c1 c2 500)
            [c2 _]  (combat/heal c2 c2 500)
            [c1 c2] (combat/attack c1 c2 500)
            [c2 _]  (combat/heal c2 c2 500)
            [c1 c2] (combat/attack c1 c2 500)
            [c2 _]  (combat/heal c2 c2 500)
            [c1 c2] (combat/attack c1 c2 500) ;; level 2
            [c2 _]  (combat/heal c2 c2 500)]
        (is (= (combat/dead? c2) false))
        (is (= (combat/level c2) 3)))))
  
  (testing "Level with factions"
    (testing "Level 1 Characters that have ever been part of 3 distinct factions gain a level"
      )

    (testing "Level 2 Characters need to join an additional 3 distinct factions to gain a level, Level 3 Characters need to join an additional 3, and so on."
      ))

  (testing "The maximum Level for Characters is 10"
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/level-up 20))]
      (is (= (combat/level c1) 10))))

  (testing "Characters cannot lose a level they have gained"
    (let [c1 (-> (combat/create-char "Pikachu")
                 (combat/level-up 5)
                 (combat/level-up 1))]
      (is (= (combat/level c1) 5))))

  )
