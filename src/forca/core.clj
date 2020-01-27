(ns forca.core
  (:gen-class))

(def total-de-vidas 6)

(defn perdeu [] (println "Você perdeu"))
(defn ganhou [] (println "Você ganhou"))

(defn letras-faltantes [palavra acertos]
    (remove (fn [letra] (contains? acertos (str letra))) palavra))

(defn acertou-a-palavra-toda? [palavra acertos] 
    (empty? (letras-faltantes palavra acertos)))

(defn le-letra! [] (read-line))

(defn acertou? [chute palavra] 
    (.contains palavra chute))

(defn imprime-forca [vidas palavra acertos]
    (println "Vidas " vidas)
    (doseq [letra (seq palavra)]
        (if (contains? acertos (str letra))
            (print letra) 
            (print "_")))
    (println))

(defn jogo [vidas palavra acertos]
    (imprime-forca vidas palavra acertos)
    (cond 
        (= vidas 0) (perdeu)
        (acertou-a-palavra-toda? palavra acertos) (ganhou)
        :else
        (let [chute (le-letra!)]
            (if (acertou? chute palavra)
                (do
                    (println "Acertou a letra!")
                    (recur vidas palavra (conj acertos chute)))
                (do
                    (println "Errou a letra! Perdeu Vida!")
                    (recur (dec vidas) palavra acertos))))))

(def palavra-secreta "MELANCIA")

(defn comeca-o-jogo [] 
    (jogo total-de-vidas palavra-secreta #{}))

(defn fib [x] 
    (if (= x 1) 1
        (if (= x 2) 1
            (+ (fib(- x 1)) (fib(- x 2))))))

(defn fib2 [x] 
    (loop [a 1 b 1 numero 2]
        (if (= numero x) b
            (recur b (+ a b) (inc numero)))))

(defn conta [] 
    (* 7 (Integer/parseInt (le-letra!))))

(defn soma [n]
    (loop [contador 1 soma 0]
        (if (> contador n) soma 
        (recur (inc contador) (+ soma contador)))))

(defn -main [& args]
    (comeca-o-jogo))

; (require '[forca.core :as forca] :reload')