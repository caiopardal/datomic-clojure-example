(ns db-fns-test
	(:require [clojure.test :refer :all]
						[datomic.api :as d]
						[db-fns :refer :all]))
	
	(defn make-db []
		(let [url "datomic:mem://test"]
			(d/delete-database url)
			(d/create-database url)
			(let [conn (d/connect url)
						schema (load-file "resources/datomic/schema.edn")]
				(d/transact conn schema)
				conn)))

(defn easy-setup []
  (let [conn  (make-db)
        ;some fns to make setup easier
        add!  (fn [uname fname lname]
                (add-user! conn uname fname lname))
        send! (fn [from text & to]
                (let [db (d/db conn)]
                  (apply send-message!
                         conn
                         (find-user-id db from)
                         text
                         (map (partial find-user-id db) to))))]
    [conn add! send!]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-add-user!
  (let [conn (make-db)]

    (is (= 0 (get-n-users (d/db conn))))

    (add-user! conn "cpardal" "Caio" "Pardal")

    (try
      (add-user! conn "" "Caio" "Pardal")
      (is false)
      (catch AssertionError e
        (is true)))

    (is (= 1 (get-n-users (d/db conn))))))


(deftest test-send-message!
  (let [conn  (make-db)
        pardal  (add-user! conn "pardal"  "Caio"   "Pardal")
        andre (add-user! conn "andre" "Andre"  "Nieri")
        joao  (add-user! conn "joao"  "Joao" "Fernandes")]
    (is (= 3 (get-n-users (d/db conn))))

    (is (= 0 (get-n-messages (d/db conn))))

    (send-message! conn pardal "We rock, you know that?" joao)
    (is (= 1 (get-n-messages (d/db conn))))

    (send-message! conn andre "Oh yeahhhhh! Let's fucking go!" pardal joao)
    (is (= 2 (get-n-messages (d/db conn))))))


(deftest test-find-user-id
  (let [conn (make-db)
        db0  (d/db conn)
        pardal (add-user! conn "pardal"  "Caio"   "Pardal")
        db1  (d/db conn)
        joao (add-user! conn "joao"  "Joao" "Fernandes")
        db2  (d/db conn)]

    (is (= nil  (find-user-id db0 "pardal")))
    (is (= pardal (find-user-id db1 "pardal")))
    (is (= pardal (find-user-id db1 "Caio "))) ; Broken test on purpose. The right answer is "pardal"
    (is (= nil  (find-user-id db1 "joao")))
    (is (= joao (find-user-id db2 "joao")))))


(deftest test-my-inbox
  (let [[conn add! send!] (easy-setup)]

    (add! "pardal" "Caio"   "Pardal")
    (add! "joao" "Joao" "Fernandes")
    (add! "andre" "Andre"  "Nieri")

    (send! "pardal" "Hey, how are you?"
           "joao")
    (send! "joao" "I'm fine, thanks! How about you?"
           "pardal")

    (send! "joao" "We are about to change some lives!!!"
           "pardal" "andre")

    (send! "andre" "Let's rock this shit!"
           "andre")

    (let [db (d/db conn)]

      (is (= (my-inbox db "testing...")
             #{}))

      (is (= (my-inbox db "pardal")
             #{["joao" "We are about to change some lives!!!"]
               ["joao" "I'm fine, thanks! How about you?"]}))

      (is (= (my-inbox db "andre")
             #{["joao" "We are about to change some lives!!!"]
               ["andre"  "Let's rock this shit!"]})))))


(deftest test-everyone-ive-messaged-with
  (let [[conn add! send!] (easy-setup)]

    (add! "pardal" "Caio"   "Pardal")
    (add! "joao" "Joao" "Fernandes")
    (add! "abee"  "Aunt"   "Bee")
    (add! "lou"   "Thelma" "Lou")
    (add! "andre" "Andre"  "Nieri")
    (add! "floyd" "Floyd"  "Lawson")

    (send! "pardal" "Hey, how are you?"
           "joao")

    (send! "joao" "I'm fine, thanks! How about you?"
           "pardal")

    (send! "andre" "I don't want to stay here anymore! Let's go!!!"
           "joao")

    (send! "abee" "Now don't forget"
           "pardal")

    (send! "joao" "We are about to change some lives!!!"
           "pardal" "abee" "lou" "andre" "floyd")

    (send! "floyd" "Let's rock this shit!"
           "floyd")

    (let [db (d/db conn)]
      (is (= (everyone-ive-messaged-with db "pardal")
             #{"joao" "abee"}))

      (is (= (everyone-ive-messaged-with db "lou")
             #{"joao"}))

      (is (= (everyone-ive-messaged-with db "joao")
             #{"floyd"  "abee"  "lou"  "pardal"  "andre"})))))
