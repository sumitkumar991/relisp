(ns relisp.core
  (:gen-class)
  (:require [clojure.string :as cls]))
(def env
  {
   :+ + :- - :* * :/ /
   :> > :< < :>= >= :<= <=
   :nil nil :not not

   })
(def regex-strings
  {
   :inte #"(-?\s*?(?:0|[1-9])[0-9]*)?"
   :doublee #"^([-]?\s*(?:0|[1-9])\d*(?:\.\d)+?\d*(?:[eE][+-]?\d+)?)(?:.|\n)*?$" ; fails for "323e4"
   :stringg #"([\"])(?:(?=(\\?))\2.)*?\1"
   })
(defn get-string
  [input]
  (get (re-find (:stringg regex-strings) input) 0))
(defn parse-string
  [input-str]
  (if (= \" (first input-str))
    (let [x (get-string input-str)]
      (if x
        [x (subs input-str (count x))]
        [nil input-str]))
    [nil input-str]))

(defn parse-null
  "Parses null values to nil"
  [input-str]
  (if (nil? input-str)
    [nil nil]
    (if (cls/starts-with? input-str "null")
      ["null" (subs input-str 4)]
      [nil input-str])))

(def esc-list '(\space \backspace \newline \formfeed \tab \return))
(defn parse-space
  "Removes whitespaces from left of strings & some ignore symbols"
  [input-str]
  (if (empty?
        input-str)
    input-str
    (if (some
          #(= (char (first input-str))  %)
          esc-list)
      (parse-space (subs input-str 1))
      input-str)))

(defn parse-parens
  "parses , ( ) symbols"
  [input-str]
  (if (empty? input-str)
    [nil nil]
    (if (some #(= (first input-str) %) '(\( \)))
      [(first input-str) (subs input-str 1)]
      [nil input-str])))
(defn parse-boolean
  "Parses the string value to bool value"
  [input-str]
  (if (nil? input-str)
    nil
    (cond
      (cls/starts-with? input-str "true") [true (subs input-str 4)]
      (cls/starts-with? input-str "false") [false (subs input-str 5)]
      :else [nil input-str])))

(defn parse-int
  "Parses the string to int"
  [input-str]
  (let [[ss result] (re-find (:inte regex-strings) input-str)]
    (if result
      (if (< (count result) 17)
        [(Long/parseLong (cls/replace result " " "")) (subs input-str (count result))]
        [(BigInteger. (cls/replace result " " "")) (subs input-str (count result))])
      [nil input-str])))

(defn parse-double
  "Parses double value in string"
  [input-str]
  (let [[ss result] (re-find (:doublee regex-strings) input-str)]
    (if result
      [(Double/parseDouble (cls/replace result " " "")) (subs input-str (count result))]
      [nil input-str])))
(defn parse-number
  "Parses given string for numeric types"
  [input-str]
  (let [[x y] (parse-double input-str)]
    (if x
      [x y]
      (let [[a b] (parse-int input-str)]
        (if a [a b]
              [a b])))))
(defn parse-keyword
  "Returns  the string for possible keyword"
  [input-str]
  (let [x (get env (keyword input-str))]
    x))
(defn get-next-token
  "Gets the next argument in the argument list"
  [input-str]
  (let [x (subs input-str 0 (cls/index-of input-str " "))]
    [(parse-keyword x),(subs input-str (inc (count x)))]))

(def factory-parsers (list parse-parens parse-boolean parse-number parse-string))

(defn parse-values
  "Tries all parsers & return when a parser can parse the value"
  ([input-str]
    (if (or (empty? input-str) (nil? input-str))
      [nil input-str]
      (parse-values factory-parsers input-str)))
  ([[p & parsers] input-str]
   (if (nil? p)
     [nil input-str]
     (let [[result rem] (p input-str)]
       (if (not (nil? result))
         [result rem]
         (parse-values parsers rem))))))

(defn parse
  ""
  [input-str]
  (let [remain (parse-space input-str)]
    (let [[res rem] (parse-values remain)]
      (if (= res \()
        (let [[func remm] (get-next-token rem)]
          (if (nil? func)
            (do
              (println "Syntax error: expected function after '('")
              (System/exit 0))
            (let [[result remaining]
                  (loop [args [] remain remm]
                    (if (= \) (first remain))
                      [(apply func args) (subs remain 1)]
                      (let [[arg rem-str] (parse remain)]
                        (cond
                          (nil? arg) (do (println "Unexpected Termination missing ')'") (System/exit 0))
                          :else (recur (conj args arg) rem-str))
                        )))]
              [result remaining])))
        ;it is an argument
        [res rem])))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (parse "(+ 6  5)"))
  )
