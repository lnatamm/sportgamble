(ns sportgamble.core
  (:require [cheshire.core	:refer	:all])
  (:require	[clj-http.client	:as	http-client])
  (:gen-class))

(def soccer "soccer_epl")
(def basketball "basketball_nba")
(def api-host "http://localhost:3000")

(def bet-id (atom 0))


(defn getBalance[]
  (Double/parseDouble (:body (http-client/get (str api-host "/saldo"))))
)

(defn getBets[]
  (:body (http-client/get (str api-host "/apostas")))
)

(defn getGames[sportAPIKey]
    (:body (http-client/get (str api-host "/eventos?sportAPIKey=" sportAPIKey))) 
)

(defn getTotals[sportAPIKey]
  (:body (http-client/get (str api-host "/totals?sportAPIKey=" sportAPIKey)))
)

;; "op" é um parâmetro utilizado para indicar o tipo de menu a ser impresso
;; 1: Menu Principal. 2: Menu de seleção de esportes
(defn printOptions[op]
  (cond
    (= op 1)
      (do
        (println "Seu saldo atual e" (getBalance))
        (println "1 - Depositar")
        (println "2 - Sacar")
        (println "3 - Consultar Eventos")
        (println "4 - Realizar uma Aposta")
        (println "5 - Consultar Apostas")
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
        (println "2 - totals (Over/Under)")
        (println "0 - Voltar")
      )
    :else (println)
  )
)

;; "op" é um parâmetro utilizado para indicar o tipo de validação a ser feita
;; 1: (0, 3). 2: (0, 2)
(defn validRange[x op]
  (cond
    (= op 1) (or (= x 0) (= x 1) (= x 2) (= x 3) (= x 4) (= x 5)) 
    (or (= op 2) (= op 3)) (or (= x 0) (= x 1) (= x 2))
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

(defn inputRange
  ([x r]
    (if
      (> x (count (range r)))
        (do
          (println (format "%d e uma opcao invalida\n" x))
          (inputRange r)
        )
      x
    )
  )
  ([r]
    (inputRange (read) r)
  )
)

(defn inputBet
  ([x]
    (cond
      (> x (getBalance))
        (do
          (println (format "%d e maior que o saldo em conta" x))
          (inputBet)
        )
      (< x 0)
        (do
          (println "Nao e possivel apostar um valor menor ou igual a 0")
          (inputBet)
        )
      :else x
    )
  )
  ([]
    (inputBet (read))
  )
)

;;Lê uma entrada até receber um número
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
    (= x 2) "totals"
    (= x 0) 0
  )
)

;;Traduz o nome do esporte para sua respectiva chave na API
(defn translateToSportAPIKey[sport]
  (if (= sport "Futebol") soccer basketball)
)

;;Retorna o tipo de esporte pela chave da API
(defn getSport[sport_key]
  (if (= sport_key soccer) "Futebol" "Basquete")
)

;;Printa todas as apostas
(defn printBets []
  (def bets (parse-string (getBets)))
  (doall
   (map (fn [bet]
          (let [home-team (get bet "home_team")
                away-team (get bet "away_team")
                market (get bet "market")
                selected-outcome (get bet "selected-outcome")
                selected-point (get bet "selected-point")
                bet-value (get bet "bet-value")
                odds (get bet "odds")
                status (get bet "status")
                sport_key (getSport (get bet "sport-key"))]
            (println (str "Jogo: " home-team " vs " away-team
                          ", Esporte: " sport_key
                          ", Mercado: " market
                          ", Resultado Apostado: " (str selected-outcome " " selected-point)
                          ", Valor Apostado: " bet-value
                          ", Odds: " odds
                          ", Status: " status))))
        bets)))

(defn deposit[amount]
  (http-client/post  (str api-host "/transacoes") {:form-params {:value amount} :content-type :json})
)

(defn postBet[game-id market selected-outcome bet-value odds home-team away-team selected-point sport-key]
  (swap! bet-id inc)
  (http-client/post  (str api-host "/apostas") 
    {
      :form-params 
      {
        :bet-id @bet-id :game-id game-id :market market :selected-outcome selected-outcome 
        :bet-value bet-value :odds odds :home-team home-team :away-team away-team 
        :selected-point selected-point :sport-key sport-key
      } 
      :content-type :json
    }
  )
)

(defn withdrawl[amount]
  (cond
    (> amount (getBalance))
      (do
        (println "Saldo insuficiente, para cancelar a acao digite 0.")
        (println "Insira o valor a ser sacado")
        (withdrawl (readNumber))
      )
    :else (do (http-client/post  (str api-host "/transacoes") {:form-params {:value (* -1 amount)} :content-type :json}) (println (format "Saque de %d realizado com sucesso!" amount)))
  )
)

(defn executeOrder [op]
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
    )

    (= op 3)
    (do
      (println "Digite o esporte")
      (printOptions 2)
      (def value (input 2))
      (if (= value 0) (println "Retornando")
        (do
          (def sportAPIKey (translateToSportAPIKey (defineSport value)))
          (def games (parse-string (getGames sportAPIKey)))
          (println "Eventos disponiveis:")
          (dorun (map-indexed (fn [idx game] 
                                (println (str (+ idx 1) " - " 
                                              (get game "home_team") " vs " 
                                              (get game "away_team") " - Data: " 
                                              (get game "commence_time"))))
                            games))  ;; Exibe todos os jogos com índice para o usuário escolher
        )
      )
    )
    
    (= op 4) ;; Opção de Apostar em um evento
    (do
      (println "Digite o esporte")
      (printOptions 2)
      (def sport (defineSport (input 2)))  ;; Lê a escolha do esporte
      (if
        (number? sport) (println "Retornando\n")  ;; Se for número, sai
        (do

          (def sportAPIKey (translateToSportAPIKey sport))
          (def games (parse-string (getGames sportAPIKey)))
          (println (format "Voce escolheu %s\n" sport))

          (println "Eventos disponiveis:")
          (dorun (map-indexed (fn [idx game] 
                                (println (str (+ idx 1) " - " 
                                              (get game "home_team") " vs " 
                                              (get game "away_team") " - Data: " 
                                              (get game "commence_time"))))
                            games))  ;; Exibe todos os jogos com índice para o usuário escolher
          
          (def market 0)
          (println "Digite o numero do evento que deseja apostar:")
          (def event-choice (inputRange (count games)))  ;; O usuário escolhe o evento digitando o número
          (if (not= event-choice 0)
            (do
              (println "Escolha um mercado")
              (printOptions 3)  ;; Mostra as opções de mercado de aposta (h2h, spreads, totals)
              (def market (defineMarket (input 3)))  ;; Lê a escolha do mercado
            )
          )
          (if 
            (or (number? market)) (println "Retornando") ;; Se for número, sai
            (do
              (println (format "Voce escolheu o mercado %s\n" market))
              (if (= market "totals") (def games (parse-string (getTotals sportAPIKey))))
              ;; Pega o jogo escolhido baseado no número
              (def selected-game (nth games (dec event-choice)))
              ;; Pega as odds para o mercado
              (def bookmakers (get selected-game "bookmakers"))
              
              ;; Filtro condicional com base no esporte (basquete ou futebol)
              (def bookmaker-key (if (= sport "Basquete") "draftkings" "bovada"))
              (def bookmaker (first (filter #(= bookmaker-key (get % "key")) bookmakers)))  ;; Filtra pelo bookmaker correto
              (def markets (get bookmaker "markets"))
              
              ;; Pega as odds para o mercado
              (def market-odds (first (filter #(= market (get % "key")) markets)))  ;; Filtra pelo mercado
              (def outcomes (get market-odds "outcomes"))
              
              ;; Verifica se o mercado é "totals"
              (if (= market "totals")
                (do
                  ;;(println games)
                  ;; Exibe as opções de "Over" e "Under" com os pontos e odds
                  (println "Escolha o resultado para apostar:")
                  (doall
                    (map-indexed (fn [idx outcome] 
                          (println (str (+ idx 1) " - "
                                        (get outcome "name") " " 
                                        (get outcome "point") " - Odd: " 
                                        (get outcome "price")))) 
                        outcomes))
                  (println "Digite o numero do resultado que deseja apostar:")
                  (def selected-outcome (inputRange (count outcomes)))  ;; Usuário escolhe o resultado
                  (if (= selected-outcome 0)
                      (println "Retornando")
                      (do
                        ;; Encontrar a odd correspondente ao resultado escolhido
                        (def selected-odd (get (nth outcomes (dec selected-outcome)) "price"))
                        (def selected-point (get (nth outcomes (dec selected-outcome)) "point"))
                      )
                  )
                )
                (do
                  ;; Caso o mercado não seja "totals", você pode manter o código anterior para "h2h"
                  (println "Escolha o resultado para apostar:")
                  (doall
                    (map-indexed (fn [idx outcome] 
                          (println (str (+ idx 1) " - "
                                        (get outcome "name") " - Odd: " 
                                        (get outcome "price")))) 
                        outcomes))
                  (println "Digite o numero do resultado que deseja apostar:")
                  (def selected-outcome (inputRange (count outcomes)))  ;; Usuário escolhe o resultado
                  (if (= selected-outcome 0)
                      (println "Retornando")
                      (do
                        ;; Encontrar a odd correspondente ao resultado escolhido
                        (def selected-odd (get (nth outcomes (dec selected-outcome)) "price"))
                        (def selected-point "")
                      )
                  )
                )
              )
              (if (not= selected-outcome 0)
                (do
                  ;; Pergunta o valor da aposta
                  (println "Digite o valor da aposta:")
                  (def bet-value (inputBet))  ;; O usuário insere o valor da aposta
                  
                  ;; Verifica se a aposta foi realmente salva
                  (if (not= bet-value 0)  ;; Verifica se o valor da aposta é suficiente          Vai ser um GET/saldo em JSON
                      (do
                        ;; Salva a aposta
                        (postBet (get selected-game "id") market (get (nth outcomes (dec selected-outcome)) "name") bet-value selected-odd
                                  (get selected-game "home_team") (get selected-game "away_team") selected-point (get selected-game "sport_key"))
                        (println (format "Aposta de %d realizada no evento '%s vs %s' com a odd %.2f." bet-value (get selected-game "home_team") (get selected-game "away_team") selected-odd  ))
                        (http-client/post  (str api-host "/transacoes") {:form-params {:value (* -1 bet-value)} :content-type :json})
                      )
                      (println "Retornando")
                  )
                )
              )
            )
          )
        )
      )
    )
    
    (= op 5) ;; Opção de Ver Apostas
      (do
        (println "Exibindo todas as apostas realizadas:")
        (http-client/get (str api-host "/liquidar"))
        (printBets)
        (println "Fim da visualizacao de apostas.")
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