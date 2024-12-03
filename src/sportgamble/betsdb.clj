(ns sportgamble.betsdb
  (:require [cheshire.core	:refer	:all])
  (:require	[clj-http.client	:as	http-client])
  (:gen-class)
)

(def api-host "http://localhost:3000")

(defonce bets (atom []))

(defn getBets[]
  @bets
)

(defn getResult [sportAPIKey eventId]
  (:body (http-client/get (str api-host (format "/resultados?sportAPIKey=%s&eventId=%s" sportAPIKey eventId))))
)
(defn getGame [sportAPIKey eventId]
  (:body (http-client/get (str api-host (format "/jogo?sportAPIKey=%s&eventId=%s" sportAPIKey eventId))))
)

;;Atualiza o status da bet
(defn updateBetStatus [game-id status]
  (swap! bets 
    (fn[bets] 
      (doall 
        (map (fn[bet] (if (= (:game-id bet) game-id) (assoc bet :status status) bet)) bets)
      )
    )
  )
)

;; Função que é chamada para cada bet quando o usuário consulta suas apostas
(defn checkResult [bet]
  (def sportAPIKey (get bet :sport-key))
  (def eventId (get bet :game-id))
  (def betOutcome (get bet :selected-outcome))
  (def betOdd (get bet :odds))
  (def betValue (get bet :bet-value))
  (def betStatus (get bet :status))
  (def market (get bet :market))
  (def winner (getResult sportAPIKey eventId))
  (def game (getGame sportAPIKey eventId))
  (def selectedPoint (:selected-point bet))
  
  (if (and (not (= winner "incomplete")) (= betStatus "Pendente"))
    (do
      (def team1Score (Integer/parseInt (get (nth (get game "scores") 0) "score")))
      (def team2Score (Integer/parseInt (get (nth (get game "scores") 1) "score")))
      (def totals (+ team1Score team2Score))
      (cond
        (= market "h2h") 
        (if 
          (= winner betOutcome)
            (do
              (println "Aposta vencedora no mercado h2h!")
              (updateBetStatus eventId "Ganhou")
              (http-client/post (str api-host "/transacoes") {:form-params {:value (* betOdd betValue)} :content-type :json})
            )
          (updateBetStatus eventId "Perdeu")
        )
        (= market "totals")
        (cond
          (= betOutcome "Over")
          (if
            (> totals selectedPoint)
              (do
                (println "Aposta vencedora no mercado totals (Over)!")
                (updateBetStatus eventId "Ganhou")
                (http-client/post (str api-host "/transacoes") {:form-params {:value (* betOdd betValue)} :content-type :json})
              )
            (updateBetStatus eventId "Perdeu")
          )
          (= betOutcome "Under")
          (if 
            (< totals selectedPoint)
              (do
                (println "Aposta vencedora no mercado totals (Under)!")
                (updateBetStatus eventId "Ganhou")
                (http-client/post (str api-host "/transacoes") {:form-params {:value (* betOdd betValue)} :content-type :json})
              )
            (updateBetStatus eventId "Perdeu")
          )
        )
      )
    )
  )
)


;;Liquida as apostas
(defn liquidateBets []
  (dorun (map checkResult @bets))
)

;;Função para salvar uma aposta no átomo
(defn saveBet [request]
  ;; Salva a aposta incluindo as informações das equipes
  (swap! bets conj {:game-id (:game-id request)
                    :market (:market request)
                    :selected-outcome (:selected-outcome request)
                    :bet-value (:bet-value request)
                    :odds (:odds request)
                    :home_team (:home-team request)  ;; Adiciona o time da casa
                    :away_team (:away-team request)  ;; Adiciona o time visitante
                    :selected-point (:selected-point request)
                    :sport-key (:sport-key request)
                    :status "Pendente"})  ;; Status inicial da aposta como "pendente"
)