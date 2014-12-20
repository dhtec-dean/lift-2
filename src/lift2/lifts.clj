(ns lift2.lifts
  (require [clojure.core.async :as async]))

(defn equality-check [key state value]
  `(= (~key ~state) ~value))

(defn comparator-fn? [form]
  (#{'= '> '< '<= '>=} form))

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

(defn add-stop
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

(defn configure-building [lifts]
  {:floor-state (atom {})
   :lifts       (for [lift (range lifts)]
                  (let [state (atom {:at-floor 0
                                     :direction :unassigned
                                     :stopping-at #{}
                                     :people []
                                     :stationary true})]
                    {:lift-number  lift
                     :stopping-at (fn [] (:stopping-at @state))
                     :request-stop (fn [floor direction]
                                     ;; TODO: Need to be able to force a lift to accept request, even if it is 'inconvenient'
                                     ;; TODO: The logic here needs to be fixed!
                                     (let [[old-state new-state]
                                           (pn-swap! state (fn [current-state]
                                                          (if-let [new-direction (valid-to-request current-state floor direction)]
                                                            (add-stop current-state floor new-direction)
                                                            current-state)))]
                                       (when (not= old-state new-state)
                                         (println "Lift" lift "has had a stop at floor" floor "added")
                                         (when (not= (:direction old-state) (:direction new-state))
                                           (println "Lift" lift "is starting to move" (:direction new-state) "from floor" (:at-floor old-state))
                                           ;; TODO: Start thread to move the lift
                                           )
                                         true)))
                     :at-floor (fn [] (:at-floor @state))
                     :direction (fn [] (:direction @state))
                     :person-enters (fn [going-to]
                                      (swap! state (fn [{:keys [people stopping-at] :as current-state}]
                                                     (assoc current-state :people (conj people going-to) :stopping-at (conj stopping-at going-to)))))}))})

(def building (configure-building 3))

(defn divide
  "Returns a vector of sequences, the first element are those that satisfy the predicate, the second element are those that do not"
  [pred coll]
  [(filter pred coll) (remove pred coll)])

(defn sieve
  "Takes a collection and a number of predicates. Returns the collection reordered with elements that pass the first
  predicate first, followed by elements that pass the second predicate and so on. All elements of the collection are
  eventually returned, so those on the end may possibly pass none of the supplied predicates."
  [coll & preds]
  (if (seq preds)
    (let [[pass fail] (divide (first preds) coll)]
      (lazy-cat pass (apply sieve fail (rest preds))))
    coll))

(defn summon
  "Simluate a person who wants to go from 'floor' to 'going-to' pressing the appropriate 'up' or 'down' button to summon
  a lift. When a lift arrives that is going in the correct direction the person will enter the lift (regardless of
  whether it was the one originanly summoned) and then press the appripriate floor button on the lift panel."
  [floor going-to]
  (when-not (= floor going-to)
    (let [direction-required (if (> going-to floor) :up :down)
          floor-comparator (if (= direction-required :up) < >)]
      (loop []
        (let [ideal-lift (first (sieve (:lifts building)
                                       (lift-is :at-floor floor
                                                :direction (or direction-required :unassigned)
                                                :stationary true)
                                       (lift-is :at-floor (floor-comparator floor)
                                                :direction (or direction-required :unassigned))))]
          (when-not ((:request-stop ideal-lift) floor direction-required)
            (recur)))))))
