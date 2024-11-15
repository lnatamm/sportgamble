(ns sportgamble.core
  (:require	[clj-http.client	:as	http-client])
  (:gen-class))

(def key "c525251008cb6c3a48e1722f260dea29")

(def api-host "https://api.the-odds-api.com")

(def money 0)

(defn printOptions[]
  (println "Seu saldo atual e" money)
  (println "1 - Depositar")
  (println "2 - Sacar")
  (println "3 - Escolher um evento")
  (println "0 - Encerrar")
  (println)
)

(defn validRange[x]
  (or (= x 0) (= x 1) (= x 2) (= x 3))
)

(defn input
  ([x]
    (cond
      (not (validRange x))
        (do
          (println (format "%d e uma opcao invalida\n" x))
          (printOptions)
          (input)
        )
      :else x
    )
  )
  ([]
    (input (read))
  )
)

(defn readNumber
  ([x]
    (cond 
      (not (number? x))
        (do
          (println (format "%s nao e um numero\n" x))
          (println "Insira o valor a ser depositado")
          (readNumber)
        )
      :else x
    )
  )
  ([]
    (readNumber (read))
  )
)

(defn deposit[ammount]
  (def money (+ money ammount))
)

(defn withdrawl[ammount]
  (cond
    (> ammount money)
      (do
        (println "Saldo insuficiente, para cancelar a acao digite 0.")
        (println "Insira o valor a ser sacado")
        (withdrawl (readNumber))
      )
      :else (def money (- money ammount))
  )
)

(defn executeOrder[op]
  (cond
    (= op 1)
      (do
        (println "Insira o valor a ser depositado")
        (deposit (readNumber))
      )
    (= op 2)
      (do
        (println "Insira o valor a ser sacado")
        (withdrawl (readNumber))
      )
    (= op 3)
    :else
  )
)

(defn -main
  [& args]
  (printOptions)
  (def x (input))
  (executeOrder x)
  (if (not= x 0) (recur args))
)