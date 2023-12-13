(ns corrector.core
  (:gen-class)
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(defn words [text]
  (re-seq #"\w+" (string/lower-case text)))

(def WORDS (frequencies (words (slurp "big.txt"))))
(def N (reduce + (vals WORDS)))

(defn P
  "Probability of `word`."
  [word]
  (/ (get WORDS word) N))

(defn known [words]
  (set (filter #(contains? WORDS %) words)))

(defn split-string-at [s n]
  (map #(apply str %) (split-at n s)))

;; convert edits1 to clojure
(defn edits1 "All edits that are one edit away from `word`."
  [word]
  (let [letters    "abcdefghijklmnopqrstuvwxyz"
        splits     (for [i (range (inc (count word)))] (split-at word i))
        deletes    (set (for [[L R] splits :when R] (str L (subs R 1))))
        transposes (set (for [[L R] splits :when (< 1 (count R))]
                     (str L + (second R) + (first R) + (subs R 2) )))
        replaces   (set (for [[L R] splits :when R
                         c          (seq letters)]
                     (str L c (subs R 1))))
        inserts    (set (for [[L R] splits :when R
                             c      (seq letters)]
                         (str L c R)))]
    (set/union deletes transposes replaces inserts)))

;; this python
(defn edits2
  "All edits that are two edits away from `word`."
  [word]
  (for [e1 (edits1 word)
        e2 (edits1 e1)]
    e2))

(defn candidates "Generate possible spelling corrections for word."
  [word]
  (or (known [word])
      (known (edits1 word))
      (known (edits2 (word)))
      [word]))

(defn correction "Most probable spelling correction for word."
  [word]
  (first (sort-by P (candidates word))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (doseq [w ["speling" "correct" "helo"]]
    (println (correction w))))
