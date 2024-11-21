;;Exemplo requisicao odd individual: https://api.the-odds-api.com/v4/sports/americanfootball_nfl/events/3544bdfabe61cc6d4389984a5ca83955/odds?apiKey=c525251008cb6c3a48e1722f260dea29&regions=us&markets=h2h&oddsFormat=decimal
;;Exemplo requisicao odd geral: https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds?apiKey=c525251008cb6c3a48e1722f260dea29&regions=us&markets=h2h&oddsFormat=decimal
;;Exemplo requisicao eventos: https://api.the-odds-api.com/v4/sports/americanfootball_nfl/events?apiKey=c525251008cb6c3a48e1722f260dea29
(ns sportgamble.core ;;branch jv Branch Levi
  (:require [cheshire.core	:refer	:all])
  (:require	[clj-http.client	:as	http-client])
  (:gen-class))

(def key "c525251008cb6c3a48e1722f260dea29")

(def api-host "https://api.the-odds-api.com")

;Átomo de saldo inicializado com 0
(def money (atom 0))

;; "op" é um parâmetro utilizado para indicar o tipo de menu a ser impresso
;; 1: Menu Principal. 2: Menu de seleção de esportes
(defn printOptions[op]
  (cond
    (= op 1)
      (do
        (println "Seu saldo atual e" @money)
        (println "1 - Depositar")
        (println "2 - Sacar")
        (println "3 - Apostar em um evento")
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
    (= op 3)
      (do
        (println "1 - h2h (Head to Head/Moneyline)")
        (println "2 - spreads (Points Handicaps)")
        (println "3 - totals (Over/Under)")
        (println "0 - Voltar")
      )
    :else (println)
  )
)

;; "op" é um parâmetro utilizado para indicar o tipo de validação a ser feita
;; 1: (0, 3). 2: (0, 2)
(defn validRange[x op]
  (cond
    (or (= op 1) (= op 3)) (or (= x 0) (= x 1) (= x 2) (= x 3)) 
    (= op 2) (or (= x 0) (= x 1) (= x 2))
    (= op 4) (println "Selecionou Opcao Errada!")
    :else (false)
  )
)

;; "op é o parâmetro que irá ser passado para a função de validação"
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
  (swap! money + ammount)
)

(defn withdrawl[ammount]
  (cond
    (> ammount @money)
      (do
        (println "Saldo insuficiente, para cancelar a acao digite 0.")
        (println "Insira o valor a ser sacado")
        (withdrawl (readNumber))
      )
      :else (swap! money - ammount)
  )
)

(defn defineSport[x]
  (cond 
    (= x 1) "Futebol"
    (= x 2) "Basquete"
    (= x 0) 0
  )
)

(defn defineMarket[x]
  (cond
    (= x 1) "h2h"
    (= x 2) "spreads"
    (= x 3) "totals"
    (= x 0) 0
  )
)

(defn translateToSportAPIKey[sport]
  (if (= sport "Futebol") "soccer_epl" "basketball_nba")
)

(defn getGamesFromAPI[sportAPIKey market]
  (def requisition (format "%s/v4/sports/%s/odds?apiKey=%s&regions=us&markets=%s&oddsFormat=decimal" api-host sportAPIKey key market))
  (parse-string (:body (http-client/get requisition)))
)

(defn printGames[games]
  ;;Aqui também precisamos printar as odds
  (dorun (map #(println (format "Liga: %s\nData:%s\nJogo: %s vs %s\n" (get % "sport_title") (get % "commence_time") (get % "home_team") (get % "away_team"))) @games))
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
      ;;Precisamos diminuir o nesting
      (do
        (println "Escolha um mercado")
        (printOptions 3)
        (def market (defineMarket (input 3)))
        (if
          (number? market) (println "Retornando\n")
          (do
            (println (format "Voce escolheu o mercado %s\n" market))
            (println "Escolha o evento")
            (printOptions 2)
            (def sport (defineSport (input 2)))
            (if 
              (number? sport) (println "Retornando\n")
              (do
                (println (format "Voce escolheu %s\n" sport))
                (def games (atom (getGamesFromAPI (translateToSportAPIKey sport) market)))            
                (printGames games)
                (println (get (nth @games 0) "id"))
              )
            )
          )
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