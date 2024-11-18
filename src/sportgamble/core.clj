(ns sportgamble.core
  (:require	[clj-http.client	:as	http-client])
  (:gen-class))

(def key "c525251008cb6c3a48e1722f260dea29")

(def api-host "https://api.the-odds-api.com")

(def money 0)

(defn printOptions[op]
  (cond
    (= op 1)
      (do
        (println "Seu saldo atual e" money)
        (println "1 - Depositar")
        (println "2 - Sacar")
        (println "3 - Escolher um evento")
        (println "0 - Encerrar")
        (println)
      )
    (= op 2)
      (do
        (println "1 - Futebol")
        (println "2 - Basquete")
        (println "0 - Voltar")
        (println)
      )
    :else (println)
  )

)

(defn validRange[x op]
  (cond
    (= op 1) (or (= x 0) (= x 1) (= x 2) (= x 3)) 
    (= op 2) (or (= x 0) (= x 1) (= x 2))
    (= op 3) (println "Selecionou Opcao Errada!")
    :else (false)
  )
)

(defn input
  ([x op]
    (cond
      (or (not (validRange x op)))
        (do
          (println (format "%s e uma opcao invalida\n" x))
          (printOptions op)
          (input op)
        )
      :else x
    )
  )
  ([op]
    (input (read) op)
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

(defn defineSport[x]
  (cond 
    (= x 1) "Futebol"
    (= x 2) "Basquete"
    (= x 0) 0
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
      (do
        (printOptions 2)
        (def sport (defineSport (input 2)))
        (if 
          (number? sport) (println "Retornando\n")
          (println (format "Voce escolheu %s\n" sport))
        )
      )
    :else (println "Encerrando o Programa")
  )
)

(defn -main
  [& args]
  (printOptions 1)
  (def x (input 1))
  (executeOrder x)
  (if (not= x 0) (recur args))
)