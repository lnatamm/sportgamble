(ns sportgamble.handler
  (:require [compojure.core	:refer	:all]
						[compojure.route	:as	route]
						[cheshire.core	:as	json]
            [clj-http.client	:as	http]
            [sportgamble.balancedb :as balancedb]
            [ring.middleware.json	:refer	[wrap-json-body]]
            [ring.middleware.defaults	:refer	[wrap-defaults api-defaults]]
            ))

;Átomo de saldo inicializado com 0
(def money (atom 0))

(defn as-json [content]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string content)})

(defn deposit[ammount]
  (swap! money + ammount)
)

(defroutes app-routes
  (GET "/" [] "Hello World")
  (POST "/transacoes" ammount (-> (balancedb/register (:body ammount)) (as-json)))
  (GET "/saldo" [] (as-json {:balance (balancedb/balance)}))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes api-defaults))