(ns clojure-options.core
  (:require [clojure-options.help :refer :all]
            [clojure.string :refer [join]]))

;;; parameters
(defonce ^:const ^{:doc "The name of the executable used to launch this application."}
  +program-name+
  "Executable Name")

(defonce ^:const ^{:doc "A short plaintext description of the application."}
  +program-description+
  "The following are the available options for this application.")

(defonce ^:const ^{:doc (join "\n  " ["If truthy, the parser will insert an implicit 'help' option."
                                      "If a string or a symbol, the tokens for the option will"
                                      "be generated from it, else the tokens will be \"h\" and \"help\"."])}
  +help-option?+
  true)

(defn- alpha-numeric?
  "Returns true if char is an alphanumeric character."
  [char]
  (assert (char? char) (str char " is not alphanumeric"))
  (or (< 47 (int char) 58)
      (< 64 (int char) 91)
      (< 96 (int char) 123)))


;;Two kinds of exceptions:
;;Those generated from bad input
;;Those generated from bad code
;;The former type should not stop the parser, should handleable by the program, and should have a reasonable default response
;;The latter type should break from the parser and generate an error message right away.
;; 'ExceptionInfo' exceptions with the ':type' value ':parser-error', are used for the former type.

(defmacro ^:private try-reducer [new-tokens new-output errors]
  `(let [{tokens# :tokens output# :output errors# :errors}
         (try
           {:tokens ~new-tokens
            :output ~new-output
            :errors ~errors}
           (catch clojure.lang.ExceptionInfo ei#
             (if (= (:type (ex-data ei#)) :parser-error)
               {:tokens ~new-tokens
                :output ~'output
                :errors (conj ~errors (.getMessage ei#))}
               (throw ei#))))]
     (recur tokens# output# errors#)))

(defn throw-parser-error
  "Throws an parser error exception. The parser will collect these exceptions and throw all 
  of them at once when it is done parsing."
  [message]
  (throw (ex-info message {:type :parser-error})))

(defn- reduce-parsed-options
  "A function that parses a list of command line tokens according to a set of 
  conditions and feeds the resulting option pairs to a reducer function provided
  by the caller.
  -- reducer: a reducing function which collects the parsed command line tokens,
       This function must take two arguments, the first is an intermediary form of
       the output and the second is the next option/value pair. The option/value
       pair is of the following form: 'option' which refers to the token parsed
       and 'value' which is the parameter value assigned to that option. If 'value'
       is equal to :boolean, then 'option' is a boolean option with no parameter 
       and if it is :fee, then 'option' is a free argument with no parameter.
  -- val (optional): a starting value for the output. This will be passed to the
       first call to 'reducer'. Defaults to 'nil' if not provided.
  -- tokens: a tokenized command line with the executable name removed
  -- boolean-options: a set of parameters that do not require values; these are
       either true or false depending on whether they were passed in on the cli
  -- parameters-options: a set of parameters that do require values; there are
       either false if not passed or the value of the next token in the list
  'reduce-parsed-options' is intended as a backend for convenient option parsing mechanisms
  such as 'getopt' or 'with-cli-options'. It is an adaptation of 'map-parsed-options' from
  the 'unix-options' cl library.
  In the case of a bad option being detected, 'reduce-parsed-options' will attempt to finish
  parsing the tokens before throwing an exception. The output can had by calling 'ex-data'
  on the exception."
  ([reducer tokens boolean-options parameter-options]
   (reduce-parsed-options reduce nil tokens boolean-options parameter-options))
  ([reducer val tokens boolean-options parameter-options]
   (assert (and (coll? tokens) (every? string? tokens))
           (str "Invalid collection of tokens: " tokens))
   (assert (and (set? boolean-options) (every? string? boolean-options))
           (str "Invalid set of boolean options: " boolean-options))
   (assert (and (set? parameter-options) (every? string? parameter-options))
           (str "Invalid set of parameter options: " parameter-options))
   (loop [tokens tokens
          output val
          errors []]
     (let [token (first tokens)]
       (cond (empty? token)
             (if (empty? errors)
               output
               (throw (ex-info (join "\n" (map (partial str "Warning: ") errors)) {:type :parser-error :output output})))
             (= token "--")
             (let [[output errors]
                   (reduce (fn [[output errors] option]
                             (try
                               [(reducer output option) errors]
                               (catch clojure.lang.ExceptionInfo ei
                                 (if (= (:type (ex-data ei)) :parser-error)
                                   [output (conj errors (.getMessage ei))]))))
                           [output errors]
                           (map vector (rest tokens) (repeat :free)))]
               (if (empty? errors)
                 output
                 (throw (ex-info (join "\n" (map (partial str "Warning: ") errors))
                                 {:type :parser-error :output output}))))
             (= token "-")
             (try-reducer (rest tokens) (reducer output [token :free]) errors)
             (and (= (first token) \-)
                  (not (= (second token) \-)))
                                        ;short options are broken up and iterated over separately
             (let [option (str (second token))]
               (cond (parameter-options option)
                     (if (<= (count token) 2)
                       (try-reducer (drop 2 tokens) (reducer output [option (second tokens)]) errors)
                       (try-reducer (rest tokens) (reducer output [option (subs token 2)]) errors))
                     (boolean-options option)
                     (if (<= (count token) 2)
                       (try-reducer (rest tokens) (reducer output [option :boolean]) errors)
                       (try-reducer (cons (str "-" (subs token 2)) (rest tokens))
                                    (reducer output [option :boolean]) errors))
                     :else
                     (if (<= (count token) 2)
                       (recur (rest tokens) output (conj errors (str "Invalid option: '-" option "'")))
                       (recur (cons (str "-" (subs token 2)) (rest tokens))
                              output
                              (conj errors (str "Invalid option: '-" option "'"))))))
             (and (= (first token) \-)
                  (= (second token) \-))
             (cond (some #{\=} token)
                   (let [[option parameter] (clojure.string/split token #"=")]
                     (cond (parameter-options (subs option 2))
                           (try-reducer (rest tokens) (reducer output [(subs option 2) parameter]) errors)
                           (boolean-options (subs option 2))
                           (recur (rest tokens) output (conj errors (str "Used '=' with option that doesn't take a parameter: '" option "'")))
                           :else (recur (rest tokens) output (conj errors (str "Invalid option: '" option "'")))))
                   (parameter-options (subs token 2))
                   (if (#{"--" "-" nil} (second tokens))
                     (try-reducer (rest tokens) (reducer output [(subs token 2) nil]) (conj errors (str "No parameter for option '" token "'")))
                     (try-reducer (drop 2 tokens) (reducer output [(subs token 2) (second tokens)]) errors))
                   (boolean-options (subs token 2))
                   (try-reducer (rest tokens) (reducer output [(subs token 2) :boolean]) errors)
                   :else (recur (rest tokens) output (conj errors (str "Invalid option: '" token "'"))))
             :else (try-reducer (rest tokens) (reducer output [token :free]) errors))))))
  
(defn getopts
  "A traditional command-line option parser of the same general format as
  getopt from the Unix cli. Return the parsed command-line arguments as a list
  with \"--\" separating the valid options from the free arguments. For example:

     tokens = [\"-afgo.txt\" \"--alpha\" \"stay.txt\"
               \"--file\" \"return.txt\" \"loop.txt\"]

     (getopts tokens \"af:j\" [\"alpha\" \"file=\"])
      =>  [\"a\" \"f\" \"go.txt\" \"alpha\" \"file\" \"return.txt\" \"--\"
           \"stay.txt\" \"loop.txt\"]"
  [tokens short-options long-options]
  (let [[boolean-options
         parameter-options]
        ;;first parse short-options and long-options into boolean and parameter option sets
        (loop [short-options short-options
               boolean-options #{}
               parameter-options #{}]
          (let [option (first short-options)]
            (if (and (not (nil? option))
                     (alpha-numeric? option))
              (if (= (second short-options) \:)
                (recur (subs short-options 2)
                       boolean-options
                       (conj parameter-options (str option)))
                (recur (subs short-options 1)
                       (conj boolean-options (str option))
                       parameter-options))
              (loop [long-options long-options
                     boolean-options boolean-options
                     parameter-options parameter-options]
                (if (not-empty long-options)
                  (let [option (first long-options)]
                    (if (= (last option) \=)
                      (recur (rest long-options)
                             boolean-options
                             (conj parameter-options (subs option 0 (dec (count option)))))
                      (recur (rest long-options)
                             (conj boolean-options option)
                             parameter-options)))
                  [boolean-options parameter-options])))))]
    (let [[options free]
          (reduce-parsed-options
           (fn [[options free] [option value]]
             (cond (= value :free)
                   [options (conj free option)]
                   (= value :boolean)
                   [(conj options option) free]
                   :else
                   [(conj (conj options option) value) free]))
           [[] []]
           tokens boolean-options parameter-options)]
      (concat options ["--"] free))))

(defn- map-tag-to-parser
  "Maps a type hint to an appropriate parser function."
  [tag]
  (case tag
    String identity
    long '(fn [str#]
            (try (Long. str#)
                 (catch NumberFormatException nfe#
                   (throw-parser-error (str "Invalid long integer: " str#)))))
    double '(fn [str#]
            (try (Double. str#)
                 (catch NumberFormatException nfe#
                   (throw-parser-error (str "Invalid floating point number: " str#)))))
    
    nil '(constantly true)
    identity))
                              
(defn- parse-spec-to-options
  "This function parses a user provided option spec of the form:
  
  [alpha ^String beta & [in-file out-file]]
  
  to a number of data structures needed for correctly parsing cli tokens.
  The output is a hash with these keys:
  -- :boolean-options - A list of strings to be passed to 'reduce-parsed-options'
  -- :parameter-options - A list of strings to be passed to 'reduce-parsed-options'
  -- :all-options - A map where the keys are tokens from 'boolean-options' and
       'parameter-options' the values are more detailed option specifications. The 
       specifications are derived from meta data provided in the original option
       spec. The type hint options are used to derive parsers for the cli token
       parameters.
  -- :free-options - Either a symbol to be used as the name of the variable to 
       contain a list of all of the free tokens, or a list of such symbols to be
       bound individually to the same free tokens."
  [spec]
  (loop [spec spec
         boolean-options #{}
         parameter-options #{}
         all-options {}]
    (let [option (first spec)
          {:keys [tag parser]} (meta option)]
      (cond (nil? option)
            {:boolean-options boolean-options
             :parameter-options parameter-options
             :all-options all-options
             :free-options 'free-options}
            (= option '&)
            {:boolean-options boolean-options
             :parameter-options parameter-options
             :all-options (if (coll? (second spec))
                            (reduce #(assoc %1 (name %2)
                                            (let [{:keys [tag parser]} (meta %2)]
                                              (assoc (meta %2)
                                                :keyword (keyword %2)
                                                :free true
                                                :tag (when tag (keyword (name tag)))
                                                :parser (or parser
                                                            (map-tag-to-parser
                                                             (or tag String))))))
                                    all-options (second spec))
                            all-options)
             :free-options (or (second spec) 'free-options)}
            :else
            (let [token (name option)
                  ;;make sure that there are no duplicate short tokens by skipping older ones
                  short-token (->> (int (first token))
                                   (iterate inc)
                                   (map (comp str char))
                                   (remove all-options)
                                   first)
                  option (assoc (meta option)
                           :keyword (keyword option)
                           :free false
                           :tag (when tag (keyword (name tag)))
                           :parser (or parser (map-tag-to-parser tag)))
                  all-options (assoc all-options token option short-token option)]
              (if (nil? tag)
                (recur (rest spec)
                       (conj (conj boolean-options token) short-token)
                       parameter-options
                       all-options)
                (recur (rest spec)
                       boolean-options
                       (conj (conj parameter-options token) short-token)
                       all-options)))))))



(defn rotate-array-map [map quantity]
  (let [map (if (> quantity 1) 
              (rotate-array-map map (dec quantity)) 
              map)
        [key val] (first map)]
    (assoc (dissoc map key)
      key val)))

(defmacro let-cli-options
  "This macro binds cli options to variables according to the provided spec.
  In the simplest case, a list of variable names are provided and this macro
  will figure out how to bind the provided tokens to them. In more complex
  situations, type hints, doc strings and free token bindings can be provided
  to provide more flexibility.

     (let-cli-options [alpha ^String beta] tokens ...)"
  [spec tokens & body]
  (let [help-token (if (or (string? +help-option?+) (symbol? +help-option?+))
                     (symbol +help-option?+)
                     (with-meta 'help {:doc "Show usage summary"}))
        spec (if +help-option?+ (cons help-token spec) spec)
        {boolean-options# :boolean-options
         parameter-options# :parameter-options
         all-options# :all-options
         free-options# :free-options}
        (parse-spec-to-options spec)
        spec (take-while #(not (= '& %)) spec)
        reduce-parsed-options# reduce-parsed-options]
    `(try
       (let [~(assoc (zipmap spec (map keyword spec)) free-options# :free)
             (~reduce-parsed-options# (fn [output# [option# value#]]
                                       (if (= value# :free)
                                         (assoc output#
                                                :free
                                                (conj (or (:free output#) []) option#))
                                         (assoc output#
                                                (:keyword (~all-options# option#))
                                                value#)))
                                     {}
                                     ~tokens
                                     ~boolean-options#
                                     ~parameter-options#)]
         (let [~@(mapcat #(vector % `(if ~% (try (~(or (:parser (all-options# (name %))) identity) ~%)
                                                 (catch Exception e#
                                                   (throw-parser-error (str "Error parsing parameter: "
                                                                            (.getMessage e#)))))
                                         ~(:default (all-options# (name %)))))
                         (concat spec (if (coll? free-options#) free-options# [free-options#])))] 
           ~(if +help-option?+
              `(if ~help-token
                 (print (help +program-name+ +program-description+ (rotate-array-map ~all-options# 2)))
                 ~@body)
              `(do ~@body))))
       (catch clojure.lang.ExceptionInfo ei#
         (if (= (:type (ex-data ei#)) :parser-error)
           (do
             (print (str (.getMessage ei#) "\n"))
             (print (help +program-name+ +program-description+ (rotate-array-map ~all-options# 2))))
           (throw ei#))))))

(defmacro defmain
  "This macro defines the main entry point to an application. Given a 'spec'
  consisting of a list of variable names, and a function body, this macro will
  return a function that when, passed a list of tokens passed in at the command
  line, will bind them to the specified variables. The behavior of the parser
  can be customized with the application of meta-data to the variable names.

  Meta data options:
  --:tag (aka. type hint) - Specifies a type and a parser for a variable, no :tag
                            means that the variable does not take a parameter.
  --:doc - Documentation string to appear in help text.
  --:default - Default value for the parameter, the default, default value  for
               all parameters is nil."
  [spec & body]
  `(defn ~'-main [~'args] ; I want 'args' to appear in auto-generated usage docs
     (let [args# ~'args]  ; but I don't want to actually capture it
       (let-cli-options ~spec args# ~@body))))
