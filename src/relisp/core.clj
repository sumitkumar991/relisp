(ns relisp.core
  (:gen-class)
  (:require [clojure.string :as cls]))
(def env
  {
   :+ + :- - :* * :/ /
   :> > :< < :>= >= :<= <=
   := =
   ;:and and :or or
   :logand bit-and :logior bit-or :eq == :not not
   :1+ inc :1- dec
   :floor (fn [x y] [(int (/ x y)) (rem x y)])
   :ceiling (fn cel ([x] (cel x 1)) ([x y] [(let [r (/ x y)] (cond (integer? r) r :else (int (inc r)))) (rem x y)]) )
   :max max :min min :round #(int (+ 0.5 %))
   :nil "null"
   :null nil?
   ; list functions
   :list list :car first :cdr rest :cddr (comp rest rest) :cons cons :last butlast
   ;:append conj :car first :cdr rest
   })
(def global-env {})                                         ;keeps all globals as k/v pairs
(def rest-str #(if (empty? %) nil (subs % 1)) )

(def MAX-LEN 30)

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

(defn find-next-exp
  [input-str]
  (let [sp-removed (parse-space input-str)]
    (if (= (first sp-removed) \()
      (loop [temp-str (rest-str sp-removed) count 1 indd 1]
        (cond
          (empty? temp-str) (do (println "error: no matching parenthesis")
                                (System/exit 0))
          (= count 0) [(subs sp-removed 0 indd) (subs sp-removed indd)]
          (= (first temp-str) \() (recur (subs temp-str 1) (inc count) (inc indd))
          (= (first temp-str) \)) (recur (subs temp-str 1) (dec count) (inc indd))
          :else (recur (subs temp-str 1) count (inc indd)))
        )
      (let [x (subs sp-removed 0 (or (cls/index-of sp-removed " ") (cls/index-of sp-removed ")") (count sp-removed)))]
        [x (subs sp-removed (count x))])
      )))

(defn get-all-args
  "vector of all args upto closing )"
  [input-str]
  (let [sp-removed (parse-space input-str)]
    (loop [arg-list [] remaining-str sp-removed]
      (let [[exp remm] (find-next-exp remaining-str)]
        (if (or (= exp "") (= (first remaining-str) \)))
          [arg-list remaining-str]
          (recur (conj arg-list exp) remm))
        ))))

(defn parse-keyword
  "Returns the string for possible keyword"
  [input]
  (let [x (get env (keyword input))]
    x))
(defn parse-key
  "Parses string for keywords"
  [input-str]
  (let [[x xs] (find-next-exp input-str)]
    (let [res (get env (keyword x))]
      (if (nil? res)
        [nil input-str]
        [res (subs input-str (count x))]))
    ))
(defn check-env
  "Check global environment for variable values"
  [input curr-env]
  (get curr-env input))

(declare parse-spl-form)

(defn get-next-token
  "Parses & checks if next token is spl form or keyword or is a global variable in order"
  [input-str]
  (let [[x xs] (find-next-exp input-str)] ;todo: assuming name cannot be longer than fixed
    (let [form (parse-spl-form x)]
      (if (nil? form)
        (let [result (parse-keyword x)]
          (if (nil? result)
            (let [res-var (check-env x global-env)]
              (if (nil? res-var)
                [x (subs input-str (count x))]
                [res-var (subs input-str (count x))]))
            [result (subs input-str (count x))]))
        [form (subs input-str (count form))]
        ))))

(def spl-forms '("if" "lambda" "define" "funcall"))
(defn parse-spl-form
  [input]
  (let [form  ((fn [[x & xs]]
                 (if (nil? x)
                    nil
                    (if (= input x)
                      x
                      (recur xs)))
                   ) spl-forms)]
    (if (nil? form)
      nil
      form)))

(defn parse-vars
  "Checks the global env for variables"
  [input-str curr-env]
  (let [[token remain] (find-next-exp input-str)]
    (let [x (check-env token curr-env)]
      (if (nil? x)
        (let [y (check-env token global-env)]
          (if (nil? y)
            [nil input-str]
            [y remain]))
        [x remain]))))

(def factory-parsers (list parse-parens parse-key parse-boolean parse-number parse-string parse-vars get-next-token)) ;order of parse-vars & get-next-token is !imp

(defn parse-values
  "Tries all parsers & return when a parser can parse the value"
  ([input-str curr-env]
    (if (or (empty? input-str) (nil? input-str))
      [nil input-str]
      (parse-values factory-parsers input-str curr-env)))
  ([[p & parsers] input-str curr-env]
   (if (nil? p)
     [nil input-str]
     (if (or (= p parse-vars))
       (let [[result rem] (p input-str curr-env)]
         (if (not (nil? result))
           [result rem]
           (recur parsers rem curr-env)))
       (let [[result rem] (p input-str)]
         (if (not (nil? result))
           [result rem]
           (recur parsers rem curr-env)))
       )
     )))

(declare parse)
(defn eval-exp
  "evaluation of spl form"
  [func input-str curr-env]
  (let [[args rem-str] (get-all-args input-str)]
    (case func
      "if" (do (let [evaled-args (map #(get % 0 ) (map parse args))]
                 [(if (nth evaled-args 0) (nth evaled-args 1) (nth evaled-args 2)) rem-str]))

      "define" (do (def global-env (assoc global-env (args 0) ((parse (args 1)) 0)))
                   [nil (subs rem-str 1)])

      "funcall" (let [[[arg-list lam-body] remmm] (parse (args 0))]
                  (let [evaled-args (map #(nth % 0) (map parse (rest args)))]
                    (if (not= (count arg-list) (count evaled-args))
                      (do (println (str "Invalid arity (" (count evaled-args) ") for function required ("  (count arg-list) ")"))
                          (System/exit 1))
                      (let [local-env (zipmap arg-list  evaled-args)]
                        [(get (parse lam-body local-env) 0) rem-str]))
                    ))

      "lambda" (let [arg-list (cls/split (cls/trim (subs (args 0) 1 (cls/index-of (args 0) ")"))) #" ")]
                  [[arg-list (args 1)] rem-str])                                            ;returns vector of [args quote-body]
      (let [final-args (map #(get % 0) (map (fn [elem] (parse elem curr-env)) args)) ]
        [(apply func final-args) rem-str])))
)
(defn parse
  "parses whole string"
  ([input-str]
    (parse input-str global-env))
  ([input-str curr-env]
   (let [sp-result (parse-space input-str)]
     (let [[val-res val-rem] (parse-values sp-result curr-env)]
       (if (= val-res \()
         (let [[func remm] (get-next-token val-rem)]
           (eval-exp func remm curr-env)
           )
         [(if (= val-res "null") nil val-res) val-rem]                   ;it's an argument
         )))))

(defn interpret
  "Returns the evaluated expression value"
  [input-str]
  (get (parse input-str) 0))

(defn -main
  "Driver function for repl application"
  [& args]
  (loop []
    (print "relisp=> ")
    (flush)
    (let [input (read-line)]
      (if (= input "exit")
        (do (println "Bye, See u soon")
            (System/exit 0))
        (do (println (interpret input))
          (recur))
        ))
    )
  ;(parse "(define abc (lambda (x y) (+ x y)))")
  ;(println (interpret "(funcall abc 2 56)"))
  )
