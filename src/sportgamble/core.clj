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

(def bets (atom []))  ;; Lista de apostas feitas

(defn saveBet [game-id market selected-outcome bet-value odds]
  (if (<= bet-value @money)  ;; Verifica se o valor da aposta é menor ou igual ao saldo
    (do
      ;; Salva a aposta
      (swap! bets conj {:game-id game-id
                        :market market
                        :selected-outcome selected-outcome
                        :bet-value bet-value
                        :odds odds
                        :status "pending"})  ;; Status inicial da aposta como "pendente"
      
      ;; Atualiza o saldo, subtraindo o valor da aposta
      (swap! money - bet-value))
    (println "Saldo insuficiente para fazer a aposta. Tente um valor menor.")
  )
)

;; Fazer função para pegar resultado dos jogos

(defn printGames [games market]
  (dorun
    (map
      (fn [game]
        (let [home-team (get game "home_team")
              away-team (get game "away_team")
              commence-time (get game "commence_time")
              bookmakers (get game "bookmakers")
              draftkings (first (filter #(= "draftkings" (get % "key")) bookmakers)) ;; Filtra o bookmaker "draftkings"
              markets (get draftkings "markets")]
          
          ;; Imprime as informações do jogo
          (println (format "Liga: %s" (get game "sport_title")))
          (println (format "Data: %s" commence-time))
          (println (format "Jogo: %s vs %s" home-team away-team))
          (println "Odds DraftKings:")

          (dorun
            (map
              (fn [market-item] 
                (let [market-name (get market-item "key")
                      outcomes (get market-item "outcomes")] 
                  (when (= market-name market) 
                    (println (format "Mercado: %s" market-name))
                    (dorun
                      (map
                        (fn [outcome]
                          (println (format "%s: %s" (get outcome "name") (get outcome "price"))))
                        outcomes)))))
              markets))

          (println "-----------------------------------")))
      games)))



(defn executeOrder[op]
  (cond
    (= op 1) ;; Opção de Depositar
    (do
      (println "Digite o valor para depositar:")
      (def amount (readNumber))
      (deposit amount)
      (println (format "Deposito de %d realizado com sucesso!" amount))
    )
    
    (= op 2) ;; Opção de Sacar
    (do
      (println "Digite o valor para sacar:")
      (def amount (readNumber))
      (withdrawl amount)
      (println (format "Saque de %d realizado com sucesso!" amount))
    )
    
(= op 3)  ;; Opção de Apostar em um evento
(do
      (println "Escolha um mercado")
      (printOptions 3)  ;; Mostra as opções de mercado de aposta (h2h, spreads, totals)
      (def market (defineMarket (input 3)))  ;; Lê a escolha do mercado
      (if
        (number? market) (println "Retornando\n")  ;; Se for número, sai
        (do
          (println (format "Voce escolheu o mercado %s\n" market))
          (println "Escolha o evento (digite o numero do evento):")
          
          (printOptions 2)  ;; Mostra as opções de esporte (Futebol ou Basquete)
          (def sport (defineSport (input 2)))  ;; Lê a escolha do esporte

          (if 
            (number? sport) (println "Retornando\n")  ;; Se for número, sai
            (do
              (println (format "Voce escolheu %s\n" sport))
              (def games (atom (getGamesFromAPI (translateToSportAPIKey sport) market)))  ;; Chama a API para pegar os jogos
              (println "Eventos disponiveis:")
              (dorun (map-indexed (fn [idx game] 
                                    (println (str (+ idx 1) " - " 
                                                  (get game "home_team") " vs " 
                                                  (get game "away_team") " - Data: " 
                                                  (get game "commence_time"))))
                                @games))  ;; Exibe todos os jogos com índice para o usuário escolher

              (println "Digite o numero do evento que deseja apostar:")
              (def event-choice (read))  ;; O usuário escolhe o evento digitando o número
              
              ;; Pega o jogo escolhido baseado no número
              (def selected-game (nth @games (dec event-choice)))
              
              ;; Pega as odds para o mercado
              (def bookmakers (get selected-game "bookmakers"))
              (def draftkings (first (filter #(= "draftkings" (get % "key")) bookmakers)))  ;; Filtra o bookmaker "draftkings"
              (def markets (get draftkings "markets"))
              
              ;; Pega as odds para o mercado
              (def market-odds (first (filter #(= market (get % "key")) markets)))  ;; Filtra pelo mercado
              (def outcomes (get market-odds "outcomes"))
              
              ;; Solicita ao usuário escolher um resultado para apostar
              (println "Escolha o resultado para apostar:")
              (doseq [outcome outcomes]  ;; Exibe as opções de resultado
                (println (str (get outcome "name") " - Odd: " (get outcome "price"))))
              (println "Digite o numero do resultado que deseja apostar:")
              (def selected-outcome (read))  ;; Usuário escolhe o resultado

              ;; Encontrar a odd correspondente ao resultado escolhido
              (def selected-odd (get (nth outcomes (dec selected-outcome)) "price"))
              
              ;; Pergunta o valor da aposta
              (println "Digite o valor da aposta:")
              (def bet-value (readNumber))  ;; O usuário insere o valor da aposta
              
              ;; Salva a aposta
              (saveBet (:id selected-game) market (get (nth outcomes (dec selected-outcome)) "name") bet-value selected-odd)
              
              ;; Verifica se a aposta foi realmente salva
              (if (<= bet-value @money)  ;; Se o valor da aposta for suficiente e a aposta foi salva
                (println (format "Aposta de %d realizada no evento '%s vs %s' com a odd %.2f. Seu saldo agora e: %s" bet-value 
                                 (get selected-game "home_team") (get selected-game "away_team") selected-odd @money))
                (println "Aposta nao foi realizada devido a saldo insuficiente.")
              )
            )
          )
        )
    )
    
    :else (println "Encerrando o Programa")
  )
)
)

(defn -main
  [& args]
  (printOptions 1)
  (def x (input 1))
  (executeOrder x)
  (if (not= x 0) (recur args))
)