(ns clojure-options.help
  (:require [clojure.string :refer [join blank? split]]))

(defonce ^:dynamic ^{:doc (join "\n  " ["A template for the help summary. The '%s' will be fill in"
                                        "such: the first will be the program name, the second will"
                                        "be program description and the thirst will be the usage"
                                        "summary."])}
  *help-template* 
  "%s

  %s

Options:
%s")

(defn- normalize-options
  "Given the 'all-options' output of 'parse-spec-to-options', will return
  a list of option specs with the tokens included."
  [options-hash]
  (vals
   (reduce (fn [options [token option]]
             (let [old-option ((:key option) options)]
               (assoc options (:keyword option)
                      (merge old-option
                              (assoc option :tokens
                                    (conj (:tokens old-option)
                                          token))))))
           {}
           options-hash)))

(defn- parameter-string
  "Returns a printable type descriptor for a option tag type."
  [tag]
  (case tag
    String "STRING"
    long "NUM"
    double "NUM"
    nil nil
    :else nil))

(def ^:private token-string
  (memoize
   (fn [{:keys [tokens free tag default]}]
     (str "  "
          (join " " (remove nil?
                            (if-not free
                              (concat (map #(str (if (> (count %) 1) "--" "-") %)
                                           (sort-by count tokens))
                                      (list (parameter-string tag)
                                            default))
                              (concat tokens (list (or (parameter-string tag) "STRING")
                                                   default)))))
          "  "))))

(defn- maplist [fn list]
  (map fn (take-while not-empty (iterate rest list))))

(defmacro ^:private dolist [[name sequence] & body]
  `(maplist (fn [~name] ~@body) ~sequence))
  
(defn- blank-str
  "Returns a string of spaces of 'length' length"
  [length]
  (apply str (repeat length \space)))

(defn- tokens-to-lines
  "Given a list of 'tokens', will join them into a string separated by newlines and
  spaces, such that no line is ever longer than 'line-length' and that each line but
  the first is prefixed by 'prefix-length' of blank space."
  [tokens line-length & [prefix-length]]
  (assert (and (coll? tokens) (every? string? tokens)))
  (let [line-length (- line-length (or prefix-length 0))]
    (loop [tokens tokens
           lines []]
      (if (not-empty tokens)
        (let [next-line (first (drop-while #(and (< line-length (reduce + ( map count %)))
                                                 (> (count %) 1))
                                           (iterate butlast tokens)))]
          (recur (drop (count next-line) tokens)
                 (conj lines (join " " next-line))))
        (join (str "\n" (blank-str (or prefix-length 0))) lines)))))

(defn- option-usage-string
  "From an option descriptor, creates a string of the form:

  '  -a --alpha STRING  This is an option'"
  [option & [description-start max-length]]
  (let [token-string (token-string option)
        description-start (max (or description-start 25) (count token-string))
        max-length (or max-length 100)]
    (str 
     token-string
     (blank-str (- description-start (count token-string)))
     (when (:doc option)
       (if (> (+ description-start (count (:doc option))) max-length)
         (tokens-to-lines (split (:doc option) #" ") max-length description-start)
         (:doc option)))
     "\n")))

(defn options-string
  "Create a basic usage summary."
  [options & [max-line-length]]
  (assert (map? options))
  (let [options (normalize-options options)
        description-start (apply max
                                 (map (comp count token-string)
                                      options))]
    (join ""
          (map #(option-usage-string % description-start max-line-length)
               options))))

(defn usage-summary
  "Returns a simplified usage summary."
  [program-name options & [max-line-length]]
  (assert (and (map? options)
               (every? string? (keys options))))
  (let [options (normalize-options options)]
    (str "Usage: "
         program-name
         " "
         (tokens-to-lines (map (fn [option]
                                 (str "["
                                      (if-not (:free option)
                                        (str (join " | "
                                                   (map #(str (if (> (count %) 1) "--" "-") %)
                                                        (:tokens option)))
                                             (when (:tag option)
                                               (str "=<" (last (sort-by count (:tokens option))) ">")))
                                        (str "<" (first (:tokens option)) ">"))
                                      "]"))
                               options)
                          (or max-line-length 100000000)
                          (count (str "Usage: " program-name " "))))))

(defn help
  "Returns a full program description and usage summary."
  [program-name program-description options & [max-line-length]]
  (format *help-template*
          (usage-summary program-name options max-line-length)
          program-description
          (options-string options max-line-length)))
