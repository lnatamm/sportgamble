(ns sportgamble.balancedb)

(defonce transactions (atom []))

(defn calculate[total transaction]
    (let [amount (:value transaction)]
        (+ total amount)
    )
)

(defn getTransactions[]
    @transactions
)

(defn balance []
    (reduce calculate 0 @transactions)
)

(defn register [request]
    (let [
            transaction (assoc (:body request) :value (:value request))]
            (swap! transactions conj transaction)
            transaction))