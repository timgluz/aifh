;; Chapter1 - Logic with NeuralNetworks

(ns vol3-clojure-examples.examples.example-logic
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :as m])
  (:import [java.lang Integer]))

(defn int->binary-vector
  "returns vector of binary values for a number"
  [^Long number ^Long n-elements]
  (let [init-vec (fn [size]
                    (take size (repeatedly #(identity 0))))
        add-padding (fn [n-elements binary-vec]
                      (if (= n-elements (count binary-vec))
                        binary-vec
                        ;;add padding of zeroes
                        (concat
                          (init-vec (- n-elements (count binary-vec)))
                          binary-vec)))]
    (->> (int number)
      (Integer/toBinaryString)
      (map #(Integer/parseInt (str %)))
      (add-padding n-elements)
      vec
      doall)))

(defn generate-truth-table
  "generates truth table for size of n elements in rows"
  [^Long n]
  (vec
    (for [x (range (math/expt 2 n))]
      (int->binary-vector x n))))

(defn calc-neuron
  [the-neuron input-vec]
  (let [res (+ (:bias the-neuron)
              (m/mmul (:weights the-neuron) input-vec))]
    (if (<= 0.5 res) 1 0)))

(defn run
  "trains Neuron weights for truth table"
  [the-neuron training-dt]
  (loop [left-rows training-dt]
    (println
      (first left-rows) "- "
      (calc-neuron the-neuron (first left-rows)))
    (when-not (empty? left-rows)
      (recur (rest left-rows)))))

(comment

  (require '[vol3-clojure-examples.examples.example-logic :as ex] :reload) 
  (ex/int->binary-vector 7 3) 
  (def training-dt (generate-truth-table 2))
  ;;process_and - aka returns AND values
  (def and-neuron {:weights [1.0 1.0] :bias -1.5})
  (ex/run and-neuron training-dt)

  ;;process or
  (def or-neuron {:weights [1, 1] :bias -0.5})
  (ex/run or-neuron training-dt)

  ;;process_not
  (def not-neuron {:weights [-1] :bias 0.5})
  (ex/run not-neuron [[0] [1]])

  ;;process xor
  ;; it doesnt look beautiful as it's replicates imperative flow
  ;; as the python example had.
  (def hidden1 [{:weights [1 1] :bias -0.5}
                {:weights [1 1] :bias -1.5}])
  (def hidden2 {:weights [-1] :bias 0.5})
  (def output {:weights [1 1] :bias -1.5})

  (def hidden1-results
    (doall (vec
      (map (fn [dt-row] [(ex/calc-neuron (first hidden1) dt-row)
                         (ex/calc-neuron (second hidden1) dt-row)])
           training-dt))))

  (identity hidden1-results)
  (def hidden2-results
    (doall (vec
              (map (fn [dt-row]
                     [(first dt-row)
                      (ex/calc-neuron hidden2 [(second dt-row)])])
                   hidden1-results))))
  (identity hidden2-results)
  (ex/run output hidden2-results)
  )
