(ns lift2.lifts
  (:import (java.util.concurrent LinkedBlockingQueue))
  (require [clojure.core.async :as as]))

(defn equality-check [key state value]
  `(= (~key ~state) ~value))

(defn comparator-fn? [form]
  (#{'= '> '< '<= '>=} form))

(def nlifts 2)
(def nfloors 3)

(defrecord Lift [nlift at-floor assigned-direction stopping-at people])

;; ':floors' is a vector of vectors of people waiting on each floor (represented by the floor numbers they wish to move to)
(def building
  {:floors (apply vector (for [_ (range nfloors)] (ref [])))
   :lifts (apply vector (for [nlift (range nlifts)] (agent (Lift. (inc nlift) 0 nil #{} []))))})

(defn direction-required [floor going-to]
  (cond
    (< floor going-to) :up
    (> floor going-to) :down))

(defn send-or-fail [queue message]
  (.put queue message))

(defn divide
  "Returns a vector of sequences, the first element are those that satisfy the predicate, the second element are those that do not"
  [pred coll]
  [(filter pred coll) (remove pred coll)])

(defn is-going
  "Takes a direction #{:up :down nil} and a floor number and returns a function that takes a floor and returns a truthy
  or falsy value to indicate if that floor is in the direction relative to the original floor. Any floor is considered
  to be falsey for a nil direction"
  [direction relative-floor]
  (if direction
    (fn [floor] (= direction (direction-required relative-floor floor)))
    (constantly false)))

(defn half-way? [floor] (odd? (/ floor 1/2)))

(defn calculate-new-floor [current-floor direction]
  ((if (= :up direction) + -) current-floor 1/2))

(defn opposite-direction [direction] (if (= direction :up) :down :up))

(defn remove-people [people-waiting people-to-remove]
  (apply vector (remove (into #{} people-to-remove) people-waiting)))

(defn lift-behaviour [lift]
  (let [{:keys [nlift at-floor assigned-direction stopping-at people]} lift
        [people-disembarking people-staying] (divide (partial = at-floor) people)
        new-stopping-at (disj stopping-at at-floor)
        stopping-here? (stopping-at at-floor)]
    (if stopping-here?
      (Thread/sleep 2000)
      (Thread/sleep 1000))
    (dosync
      (let [;; Lift behaviour without interaction from people
            [stopping-at-current-direction stopping-at-opposite-direction] (divide (is-going assigned-direction at-floor) new-stopping-at)
            new-assigned-direction (cond
                                     (half-way? at-floor) assigned-direction
                                     (seq stopping-at-current-direction) assigned-direction
                                     (seq stopping-at-opposite-direction) (opposite-direction assigned-direction))
            ;; Mix in the people behaviour
            people-waiting-ref (get-in building [:floors at-floor])
            people-waiting (if people-waiting-ref @people-waiting-ref [])
            new-assigned-direction (if new-assigned-direction
                                     new-assigned-direction
                                     (when (seq people-waiting)
                                       (direction-required at-floor (first people-waiting))))
            people-joining (filter (is-going new-assigned-direction at-floor) people-waiting)
            ;; Lifts new state
            new-floor (if new-assigned-direction
                        (if (stopping-at at-floor)
                          (if (or (seq people-joining) (seq people-disembarking))
                            at-floor
                            (calculate-new-floor at-floor new-assigned-direction))
                          (calculate-new-floor at-floor new-assigned-direction))
                        at-floor)
            new-lift-state (assoc lift
                                  :assigned-direction new-assigned-direction
                                  :stopping-at (clojure.set/union new-stopping-at (into #{} people-joining))
                                  :people (concat people-staying people-joining)
                                  :at-floor new-floor)]
        (when (seq people-joining)
          (alter people-waiting-ref remove-people people-joining))
        (if (or (seq people-disembarking) (seq people-joining))
          (println "Lift" nlift "has waited at floor" (str at-floor) "while" (count people-joining) "people join and" (count people-disembarking) "people leave")
          (when (:assigned-direction new-lift-state)
            (if (half-way? (:at-floor new-lift-state))
              (println "Lift" nlift "is moving" (:assigned-direction new-lift-state) "from floor" (str at-floor) "to" (str (if (= (:assigned-direction new-lift-state) :up)
                                                                                                                             (inc at-floor)
                                                                                                                             (dec at-floor))))
              (println "Lift" nlift "is at floor" (str (:at-floor new-lift-state))))))
        (when (:assigned-direction new-lift-state) (send-off *agent* #'lift-behaviour))
        new-lift-state))))

(defn add-stop [lift floor]
  ;; TODO: need to tell lift it might need to start moving
  (if (:assigned-direction lift)
    (assoc lift :stopping-at (conj (:stopping-at lift) floor))
    (do
      (send-off *agent* #'lift-behaviour)
      (assoc lift :stopping-at (conj (:stopping-at lift) floor) :assigned-direction (direction-required (:at-floor lift) floor)))))

(defn start-lifts []
  (dorun
    (map #(send-off % lift-behaviour) (:lifts building))))

(defn splice [seq item position]
  (lazy-cat (take position seq) [item] (drop position seq)))

(defn lift-is [& {rat-floor :at-floor rdirection :direction rstopping-at :stopping-at}]
  (fn [lift-agent]
    (let [{:keys [at-floor assigned-direction stopping-at] :as as} @lift-agent]
      (and (if rat-floor
             (if (coll? rat-floor)
               (eval (apply list (splice rat-floor at-floor 1)))
               (= rat-floor at-floor))
             true)
           (if rdirection
             (= rdirection assigned-direction)
             true)
           (if rstopping-at
             (stopping-at rstopping-at)
             true)))))

(defn sieve
  "Takes a collection and a number of predicates. Returns the collection reordered with elements that pass the first
  predicate first, followed by elements that pass the second predicate and so on. All elements of the collection are
  eventually returned, so those on the end may possibly pass none of the supplied predicates."
  [coll & preds]
  (if (seq preds)
    (let [[pass fail] (divide (first preds) coll)]
      (println "sieving." (count pass) "passed" (count fail) "to check next")
      (lazy-cat pass (apply sieve fail (rest preds))))
    coll))

(defn select-lift [lifts floor direction-required]
  (dosync
    (first (sieve lifts
                  (lift-is :at-floor floor :direction direction-required)
                  (lift-is :at-floor floor :direction nil)
                  (if (= direction-required :up)
                    (lift-is :at-floor [< floor] :direction :up :stopping-at floor)
                    (lift-is :at-floor [> floor] :direction :down :stopping-at floor))
                  (if (= direction-required :up)
                    (lift-is :at-floor [< floor] :direction :up)
                    (lift-is :at-floor [> floor] :direction :down))))))

(defn add-person
  "Simluate a person who wants to go from 'floor' to 'going-to' pressing the appropriate 'up' or 'down' button to summon
  a lift. When a lift arrives that is going in the correct direction the person will enter the lift (regardless of
  whether it was the one originanly summoned) and then press the appripriate floor button on the lift panel."
  [floor going-to]
  (let [direction-required (direction-required floor going-to)]
    (when direction-required
      (dosync
        (let [ppl-waiting-at-this-floor (get-in building [:floors floor])]
          (when-not (some #(= direction-required (direction-required floor %1)) @ppl-waiting-at-this-floor)
            (let [lift (select-lift (:lifts building) floor direction-required)]
              (send-off lift add-stop floor)))
          (alter ppl-waiting-at-this-floor conj going-to))))))
