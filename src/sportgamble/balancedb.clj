(ns sportgamble.balancedb)

(def transactions (atom []))

(defn calculate[total transaction]
    (let [ammount (:value transaction)]
        (+ total ammount)
    )
)

(defn balance []
    (reduce calculate 0 @transactions)
)

(defn register [transaction]
  (swap! transaction conj (merge transaction {:id (+ (count @transaction) 1)})))