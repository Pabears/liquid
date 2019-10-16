(ns dk.salza.liq.datastructures.sub-editor
  "Work in progress"
  (:require [clojure.string :as str]))

(defn sub-editor
  ([text]
   (let [after (if (string? text) (seq text) text)] ; Creating with a list '("a" "b") should also work
     {:type :sub-editor
      ::before ()     ; The list of characters before the cursor in reverse order
      ::after after   ; The list of characters after the cursor in normal order
      ::point 0       ; The current point (cursor position). Starts at 0, the begining of the slider
      ::linenumber 1  ; Meta information for fast retrievel of the current line number
      ::totallines (inc (count (filter #(= % \newline) after))) ; Only to make the end function perform optimal
      ::dirty false
      ::marks {}}))   ; A map of named positions, like "selection" -> 18.
                      ; The positions are pushed, when text is insertet
                      ; strictly before the mark.
  ([] (sub-editor "")))

(defn sub-editor?
  "Returns true if the input has shape/properties
  like a slider."
  [se]
  (and (map? se) (= (se :type) :sub-editor)))

(defn is-newline?
  "Check if \\n or {:char \\n}"
  [c]
  (not (or (and (char? c) (not= c \newline))
           (and (string? c) (not= c "\n"))
           (and (map? c) (not= (c :char) "\n")))))

(defn beginning?
  "True if and only if the point is at the
  beginning of the slider."
  [se]
  (empty? (se ::before)))

(defn end?
  "True if and only if the point is at the
  end of the slider."
  [se]
  (empty? (se ::after)))

(defn forward-char
  "Moves the point to the right (forward) the given amount of times."
  ([se]
   (let [c (first (se ::after))]
     (if (end? se)
         se
         (assoc se
           ::before (conj (se ::before) c)
           ::after (rest (se ::after))
           ::point (inc (se ::point))
           ::linenumber (+ (se ::linenumber)
                           (cond (is-newline? c) 1
                                 (sub-editor? c) (- (c ::totallines) 1)
                                 true 0))))))
  ([se amount]
   (loop [s se n amount]
     (if (<= n 0)
       s
       (recur (forward-char s) (dec n))))))
  
(defn backward-char
  "Moves the point to the left the given amount of times.
  So moving one character left is achieved with
  (backward-char se 1)."
  ([se]
   (let [c (first (se ::before))]
     (if (beginning? se)
         se
         (assoc se
           ::before (rest (se ::before))
           ::after (conj (se ::after) c)
           ::point (dec (se ::point))
           ::linenumber (- (se ::linenumber)
                           (cond (is-newline? c) 1
                                 (sub-editor? c) (- (c ::totallines) 1)
                                 true 0))))))
  ([se amount]
   (loop [s se n amount]
     (if (<= n 0)
       s
       (recur (backward-char s) (dec n))))))
