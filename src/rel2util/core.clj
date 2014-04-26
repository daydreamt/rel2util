(ns rel2util.core
  (:use [clojure.string :only [split trim]]
        [incanter.core :only [factorial]])
  (:require [loom.alg :as alg]
            [loom.alg-generic :as ag]
            [loom.graph :as g]
            [loom.io]))

(defn is-fully-connected? [graph]
  "Does not check for duplicate edges."
  (let [e (count (g/edges graph))
        n (count (g/nodes graph))]
    (= e (/ (* n (dec n)) 2))))

(defn read-graph [path]
  (let [lines (split (slurp path) #"\n")
        edges (set (map #(split (trim %) #" ") lines))
        ;; The set here removes duplicate edges
        nodes (set (concat (map first edges)
                           (map last edges)))]
    (apply g/digraph edges)))

(defn edges-to [graph x]
  "Returns all edges to node x"
  (filter #(= x (second %)) (g/edges graph)))

(defn edges-from [graph x]
  (filter #(= x (first %)) (g/edges graph)))

(defn- nodes-with-edge-to [graph x]
  (set (map first (edges-to graph x))))

(defn- nodes-with-edge-from [graph x]
  (set (map second (edges-from graph x))))

(defn equivalent? [graph a b]
  (and (= (conj (nodes-with-edge-to graph a) a)
          (conj (nodes-with-edge-to graph b) b))
       (= (conj (nodes-with-edge-from graph a) a)
          (conj (nodes-with-edge-from graph b) b))))

(defn get-equivalent-pairs [graph]
  (set (for [a (g/nodes graph) b (g/nodes graph)
             :when (and (< (.compareTo (str a) (str b)) 0)
                        (equivalent? graph a b))]
         [a, b])))

(defn get-node-groups [graph]
  "Given a graph, returns two node groups, one to keep, and one that has equivalences"
  (loop [pairs (get-equivalent-pairs graph) tokeep #{} toremove #{} hm {}]
    (if (empty? pairs)
      {:keep tokeep :remove toremove :hm hm}
      (let [[f, s] (first pairs)]
        (cond (not (contains? tokeep f)) (recur (rest pairs) (conj tokeep f) (conj toremove s) (assoc hm s f))
              (not (contains? tokeep s)) (recur (rest pairs) (conj tokeep s) (conj toremove f) (assoc hm f s))
              :else (recur (rest pairs) tokeep (conj (conj toremove s) f) hm))))))

(defn get-simpler-graph [graph]
  (apply g/remove-nodes g1 (into [] (:remove (get-node-groups g1)))))


(defn get-util-function-values [graph]
  (let [graph2 (get-simpler-graph graph)
        eq-mapping (:hm (get-node-groups graph))]
    (apply merge (map (fn [node] {node (g/out-degree graph2 (get eq-mapping node node))}) (g/nodes graph)))))

(defn make-util-function [path]
  "Returns a utility function from a file describing a relation."
  (let [graph (read-graph path)
        hm (get-util-function-values graph)
        f (fn [x] (get hm x "not found"))]
    f))
