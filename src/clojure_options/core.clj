(ns clojure-options.core
  (:require [clojure.set :refer :all]))

(defn map-parsed-options
  "A function that parses a list of command line tokens according to a set of 
  conditions and allows the user to operate on them as a series of key/value pairs.
  -- tokens: a tokenized command line with the executable name removed
  -- boolean-options: a set of parameters that do not require values; these are
       either true or false depending on whether they were passed in on the cli
  -- parameters-options: a set of parameters that do require values; there are
       either false if not passed or the value of the next token in the list
  'map-parsed-options' is intended as a backend for convenient option parsing mechanisms
  such as 'getopt' or 'with-cli-options'."
  [tokens boolean-options parameter-options]
  (loop [tokens tokens
         options []
         free-options []]
    (let [token (first tokens)]
      (cond (empty? token)
            {:options options
             :free-options free-options}
            (= token "--")
            (recur []
                   options
                   (reduce conj free-options (rest tokens)))
            (= token "-")
            (recur (rest tokens)
                   options
                   (conj free-options token))
            (and (= (first token) \-)
                 (not (= (second token) \-)))
                                        ;short options are broken up and iterated over seperately
            (let [option (str (second token))]
              (cond (parameter-options option)
                    (if (<= (count token) 2)
                      (recur (drop 2 tokens)
                             (conj options [option (second tokens)])
                             free-options)
                      (recur (rest tokens)
                             (conj options [option (subs token 2)])
                             free-options))
                    (boolean-options option)
                    (if (<= (count token) 2)
                      (recur (rest tokens)
                             (conj options [option true])
                             free-options)
                      (recur (cons (str "-" (subs token 2)) (rest tokens))
                             (conj options [option true])
                             free-options))
                    :else {:options options
                           :free-options free-options
                           :error (str "Warning: Invalid option '-" option "'")}))
            (and (= (first token) \-)
                 (= (second token) \-))
            (cond (some #{\=} token)
                  (let [[option parameter] (clojure.string/split token #"=")]
                    (cond (parameter-options (subs option 2))
                          (recur (rest tokens)
                                 (conj options [option parameter])
                                 free-options)
                          (boolean-options (subs token 2))
                          {:options options
                           :free-options free-options
                           :error (str "Warning: Used '=' with option that doesn't take a parameter: '" option "'")}
                          :else {:options options
                                 :free-options free-options
                                 :error (str "Warning: Invalid option '" option "'")}))
                  (parameter-options (subs token 2))
                  (if (#{"--" "-" nil} (second tokens))
                    {:options options
                     :free-options free-options
                     :error (str "Warning: No parameter for option '" token "'")}
                    (recur (drop 2 tokens)
                           (conj options [(subs token 2) (second tokens)])
                           free-options))
                  (boolean-options (subs token 2))
                  (recur (rest tokens)
                         (conj options [(subs token 2) true])
                         free-options)
                  :else {:options options
                         :free-options free-options
                         :error (str "Warning: Invalid option '" token "'")})
            :else (recur (rest tokens)
                         options
                         (conj free-options token))))))

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
        (loop [short-options short-options
               boolean-options #{}
               parameter-options #{}]
          (let [option (first short-options)]
            (if (and (not (nil? option))
                     (or (< 47 (int option) 58) ;if option alphanumeric
                         (< 64 (int option) 91)
                         (< 96 (int option) 123)))
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
                  [boolean-options parameter-options])))))
        {:keys [options free-options error] :as result}
        (map-parsed-options tokens boolean-options parameter-options)]
    (concat (mapcat (fn [option]
                      (if (string? (second option)) option (take 1 option)))
                    options)
            ["--"]
            free-options)))
                              
