(ns sportgamble.handler
  (:require [compojure.core	:refer	:all]
						[compojure.route	:as	route]
						[cheshire.core	:as	json]
            [clj-http.client	:as	http]
            [cheshire.core :refer	:all]
            [sportgamble.balancedb :as balancedb]
            [sportgamble.betsdb :as betsdb]
            [ring.middleware.json	:refer	[wrap-json-body]]
            [ring.middleware.defaults	:refer	[wrap-defaults api-defaults]]
            ))

(def api-host "https://api.the-odds-api.com")

(def apiKey "e89035ce2ff24af8e6d1be59aea9821a")

(defn as-json [content]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string content)})

;;Retorna todos os jogos de um determinado esporte e mercado
(defn getGamesFromAPI
  ([sportAPIKey]
    (def requisition (format "%s/v4/sports/%s/odds?apiKey=%s&regions=us&oddsFormat=decimal" api-host sportAPIKey apiKey))
    (:body (http/get requisition))
  )
  ([sportAPIKey market]
    (def requisition (format "%s/v4/sports/%s/odds?apiKey=%s&regions=us&oddsFormat=decimal&markets=%s" api-host sportAPIKey apiKey market))
    (:body (http/get requisition))
  )
)

;;Define o vencedor entre 2 times
(defn defineWinner [team1 team2]
  (def team1Score (Integer/parseInt (get team1 "score")))
  (def team2Score (Integer/parseInt (get team2 "score")))
  (cond
    (> team1Score team2Score) (get team1 "name")
    (< team1Score team2Score) (get team2 "name")
    :else "Draw"
  )
)

;;Retorna o resultado de um jogo caso o mesmo esteja completo
(defn getGameResultFromAPI[sportAPIKey eventId]
  (def requisition (format "%s/v4/sports/%s/scores/?apiKey=%s&eventIds=%s&daysFrom=3" api-host sportAPIKey apiKey eventId))
  (def game (nth (parse-string (:body (http/get requisition))) 0))
  (if 
    (not (get game "completed")) "incomplete"
    (do
      (defineWinner (nth (get game "scores") 0) (nth (get game "scores") 1))
    )
  )
)

(defn getGameFromAPI[sportAPIKey eventId]
  (def requisition (format "%s/v4/sports/%s/scores/?apiKey=%s&eventIds=%s&daysFrom=3" api-host sportAPIKey apiKey eventId))
  (parse-string (:body (http/get requisition)))
)

(defroutes app-routes
  (GET "/" [] "Hello World")
  (POST "/transacoes" request (-> (balancedb/register (:body request)) (as-json)))
  (GET "/transacoes" [] (-> (balancedb/getTransactions) (as-json)))
  (GET "/saldo" [] (-> (balancedb/balance) (as-json)))
  (GET "/eventos" sportAPIKey (getGamesFromAPI (:sportAPIKey (:params sportAPIKey))))
  (GET "/totals" sportAPIKey (getGamesFromAPI (:sportAPIKey (:params sportAPIKey)) "totals"))
  (POST "/apostas" request (-> (betsdb/saveBet (:body request)) (as-json)))
  (GET "/apostas" [] (-> (betsdb/getBets) (as-json)))
  (GET "/resultados" [sportAPIKey eventId] (getGameResultFromAPI sportAPIKey eventId))
  (GET "/jogo" [sportAPIKey eventId] (getGameFromAPI sportAPIKey eventId))
  (GET "/liquidar" [] (do (betsdb/liquidateBets) (http/get "http://localhost:3000/apostas")))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true}) ;; Garante que as chaves sejam keywords no map
      (wrap-defaults api-defaults)))