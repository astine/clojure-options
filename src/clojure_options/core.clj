(ns clojure-options.core
  (:require [clojure.string :refer [join]]))

(defn- alpha-numeric?
  "Returns true if char is an alphanumeric character."
  [char]
  (assert (char? char) (str char " is not alphanumeric"))
  (or (< 47 (int char) 58)
      (< 64 (int char) 91)
      (< 96 (int char) 123)))

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
   (assert (set? boolean-options))
   (assert (set? parameter-options))
     (loop [tokens tokens
            output val
            errors []]
       (let [token (first tokens)]
           (cond (empty? token)
                 (if (empty? errors)
                   output
                   (throw (ex-info (join "\n" errors) {:output output})))
                 (= token "--")
                 (if (empty? errors)
                   (reduce reducer output (map vector (rest tokens) (repeat :free)))
                   (throw (ex-info (join "\n" errors) {:output (reduce reducer output (map vector (rest tokens) (repeat :free)))})))
                 (= token "-")
                 (recur (rest tokens) (reducer output [token :free]) errors)
                 (and (= (first token) \-)
                      (not (= (second token) \-)))
                                        ;short options are broken up and iterated over seperately
                 (let [option (str (second token))]
                   (cond (parameter-options option)
                         (if (<= (count token) 2)
                           (recur (drop 2 tokens) (reducer output [option (second tokens)]) errors)
                           (recur (rest tokens) (reducer output [option (subs token 2)]) errors))
                         (boolean-options option)
                         (if (<= (count token) 2)
                           (recur (rest tokens) (reducer output [option :boolean]) errors)
                           (recur (cons (str "-" (subs token 2)) (rest tokens))
                                  (reducer output [option :boolean]) errors))
                         :else (recur (cons (str "-" (subs token 2)) (rest tokens))
                                      output
                                      (conj errors (str "Warning: Invalid option '-" option "'")))))
                 (and (= (first token) \-)
                      (= (second token) \-))
                 (cond (some #{\=} token)
                       (let [[option parameter] (clojure.string/split token #"=")]
                         (cond (parameter-options (subs option 2))
                               (recur (rest tokens) (reducer output [option parameter]) errors)
                               (boolean-options (subs token 2))
                               (recur (rest tokens) output (conj errors (str "Warning: Used '=' with option that doesn't take a parameter: '" option "'")))
                               :else (recur (rest tokens) output (conj errors (str "Warning: Invalid option '" option "'")))))
                       (parameter-options (subs token 2))
                       (if (#{"--" "-" nil} (second tokens))
                         (recur (rest tokens) (reducer output [(subs token 2) nil]) (conj errors (str "Warning: No parameter for option '" token "'")))
                         (recur (drop 2 tokens) (reducer output [(subs token 2) (second tokens)]) errors))
                       (boolean-options (subs token 2))
                       (recur (rest tokens) (reducer output [(subs token 2) :boolean]) errors)
                       :else (recur (rest tokens) output (conj errors (str "Warning: Invalid option '" token "'"))))
                 :else (recur (rest tokens) (reducer output [token :free]) errors))))))
  
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
                             (conj parameter-options option))
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

(defn- map-tag-to-parser [tag]
  (case tag
    String identity
    long '(fn [str] (Long. str))
    double '(fn [str] (Double. str))
    nil '(constantly true)
    identity))
                              
(defn- parse-spec-to-options
  "This function parses an option spec."
  [spec]
  (loop [spec spec
         boolean-options #{}
         parameter-options #{}
         all-options {}]
    (let [option (first spec)
          {:keys [tag]} (meta option)]
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
                                            (assoc (meta %2)
                                              :keyword (keyword %2)
                                              :parser (map-tag-to-parser (:tag (meta %2)))))
                                    all-options (second spec))
                            all-options)
             :free-options (second spec)}
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
                           :parser (map-tag-to-parser tag))
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

(defmacro let-cli-options
  "This macro binds cli options to variables"
  [spec tokens & body]
  (let [{boolean-options# :boolean-options
         parameter-options# :parameter-options
         all-options# :all-options
         free-options# :free-options}
        (parse-spec-to-options spec)
        spec (take-while #(not (= '& %)) spec)]
    `(let [~(assoc (zipmap spec (map keyword spec)) free-options# :free)
           (reduce-parsed-options (fn [output# [option# value#]]
                                    (if (= value# :free)
                                      (assoc output#
                                        :free
                                        (conj (:free output#) option#))
                                      (assoc output#
                                        (:keyword (~all-options# option#))
                                        value#)))
                                  {}
                                  ~tokens
                                  ~boolean-options#
                                  ~parameter-options#)]
       (let [~@(mapcat #(vector % `(when ~% (~(:parser (all-options# (name %))) ~%)))
                       (concat spec (when (coll? free-options#) free-options#)))] 
         ~@body))))
