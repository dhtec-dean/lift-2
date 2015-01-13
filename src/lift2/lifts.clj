(ns lift2.lifts
  (:import (java.util.concurrent LinkedBlockingQueue))
  (require [clojure.core.async :as as]))

(defn equality-check [key state value]
  `(= (~key ~state) ~value))

(defn comparator-fn? [form]
  (#{'= '> '< '<= '>=} form))

(def nlifts 1)
(def nfloors 2)

(defrecord Lift [queue at-floor assigned-direction stopping-at people])

;; ':floors' is a vector of vectors of people waiting on each floor (represented by the floor numbers they wish to move to)
(def building
  {:floors (apply vector (for [_ (range nfloors)] (ref [])))
   :lifts (apply vector (for [_ (range nlifts)] (ref (Lift. (LinkedBlockingQueue.) 0 nil #{} []))))})

(defn direction-required [floor going-to]
  (cond
    (< floor going-to) :up
    (> floor going-to) :down))

(defn send-or-fail [queue message]
  (.put queue message))

(defn add-stop [lift floor]
  ;; TODO: need to tell lift it might need to start moving
  (if (:assigned-direction lift)
    (assoc lift :stopping-at floor)
    (assoc lift :stopping-at floor :assigned-direction (direction-required (:at-floor lift) floor))))

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

(defn opposite-direction [direction] (if (= direction :up) :down :up))

(defn remove-people [people-waiting people-to-remove]
  (apply vector (remove (into #{} people-to-remove) people-waiting)))

(defn half-way? [floor] (odd? (/ floor 1/2)))

(defn calculate-new-floor [current-floor direction]
  ((if (= :up direction) + -) current-floor 1/2))

(defn lift-behaviour [lift]
  (let [{:keys [queue at-floor assigned-direction moving? stopping-at people]} lift
        [people-staying people-disembarking] (divide (partial = at-floor) people)
        new-stopping-at (disj stopping-at at-floor)
        stopping-here? (stopping-at at-floor)]
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
            ;; Time to wait until next motion
            pause (when new-assigned-direction
                    (cond
                      stopping-here? 2
                      (half-way? new-floor) 5
                      :else 0))]
        (when pause (send-or-fail queue pause))
        (when (seq people-joining)
          (alter people-waiting remove-people people-joining))
        (assoc lift
               :assigned-direction new-assigned-direction
               :stopping-at (clojure.set/union new-stopping-at (into #{} people-joining))
               :people (concat people-staying people-joining))))))

(defn start-lifts []
  (dorun
    (for [lift (:lifts building)]
      (as/thread
        (loop []
          (when-let [pause (as/<!! (:chan @lift))]
            (Thread/sleep (* pause 1000))
            (dosync
              (alter lift lift-behaviour))
            (recur)))))))

(defmacro lift-is [& requirements]
  (when-not (even? (count requirements)) (throw (IllegalArgumentException. "Must supply an even number of forms to lift-is")))
  (let [state (gensym 'state)
        body (for [[key value] (partition 2 requirements)]
               (if (coll? value)
                 (if (comparator-fn? (first value))
                   (concat [(first value)] [(list key state)] (rest value))
                   (let [expressions (for [v (rest value)]
                                       (equality-check key state v))]
                     `(~(first value) ~@expressions)))
                 (equality-check key state value)))]
    `(fn [~state]
       (and ~@body))))

#_(defn add-stop
  ([state floor] (assoc state :stopping-at (conj (:stopping-at state) floor)))
  ([state floor direction] (assoc state :stopping-at (conj (:stopping-at state) floor) :direction direction)))

(defn valid-to-request [current-state requested-floor requested-direction]
  (let [floor-comparator (if (= requested-direction :up) <= >=)
        {:keys [direction at-floor]} current-state]
    (or (and (= direction requested-direction)
             (floor-comparator at-floor requested-floor))
        (= direction :unassigned))))

(defn pn-swap!
  "Like swap! but returns the previous and new versions of the atom in a vector, [p n]"
  [atom f & args]
  (loop []
    (let [previous @atom
          new (apply f previous args)]
      (if (compare-and-set! atom previous new)
        [previous new]
        (recur)))))

(defn sieve
  "Takes a collection and a number of predicates. Returns the collection reordered with elements that pass the first
  predicate first, followed by elements that pass the second predicate and so on. All elements of the collection are
  eventually returned, so those on the end may possibly pass none of the supplied predicates."
  [coll & preds]
  (if (seq preds)
    (let [[pass fail] (divide (first preds) coll)]
      (lazy-cat pass (apply sieve fail (rest preds))))
    coll))

(defn select-lift [lifts floor direction-required]
  (dosync
    (first (sieve lifts
                  ;; TODO: Add predicates to select most appropriate lift
                  ))))

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
              (alter lift add-stop floor)))
          (alter ppl-waiting-at-this-floor conj going-to))))))
