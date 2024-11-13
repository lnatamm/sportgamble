(ns sportgamble.core
  (:gen-class))

;; (defn-	valores-em	[argumento]
;;   (cond
;;     ;;	.startsWith	e	.substring	são	as	novidades	aqui!
;;     (.startsWith	argumento	"--de=")
;;     {:de	(.substring	argumento	5)}

;;     (.startsWith	argumento	"--para=")
;;     {:para	(.substring	argumento	7)}
;;     :else	{}
;;   )
;;  )

(defn printOptions[]
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

(defn -main
  [& args]
  (printOptions)
  (def x (input))
  (if (not= x 0) (-main))
)
