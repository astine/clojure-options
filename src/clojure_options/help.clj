(ns clojure-options.help
  (:require [clojure.string :refer [join blank? split]]))

(defn- normalize-options
  [options-hash]
  (vals
   (reduce (fn [options [token option]]
             (let [old-option ((:keyword option) options)]
               (assoc options (:keyword option)
                      (merge old-option
                              (assoc option :tokens
                                    (conj (:tokens old-option)
                                          token))))))
           {}
           options-hash)))

(defn- parameter-string
  [tag]
  (case tag
    String "STRING"
    long "NUM"
    double "NUM"
    nil nil
    :else nil))

(def ^:private token-string
  (memoize
   (fn [{:keys [tokens tag default]}]
     (str "  "
          (join " " (remove nil?
                            (concat (map #(str (if (> (count %) 1) "--" "-") %)
                                         (sort-by count tokens))
                                    (list (parameter-string tag)
                                          default))))
          "  "))))

(defn- maplist [fn list]
  (map fn (take-while not-empty (iterate rest list))))

(defmacro ^:private dolist [[name sequence] & body]
  `(maplist (fn [~name] ~@body) ~sequence))
  
(defn- blank-str [length]
  (apply str (repeat length \space)))

(defn- option-usage-string
  [option & [description-start max-length]]
  (let [token-string (token-string option)
        description-start (max (or description-start 25) (count token-string))
        max-length (or max-length 100)]
    (str 
     token-string
     (blank-str (- description-start (count token-string)))
     (if (> (+ description-start (count (:doc option))) max-length)
       (let [length (- max-length description-start)]
         (loop [desc (split (:doc option) #" ")
                lines []]
           (if (not-empty desc)
             (let [next-line (first (drop-while #(and (< length (reduce + (map count %)))
                                                      (> (count %) 1))
                                                (iterate butlast desc)))]
               (recur (subvec desc (count next-line))
                      (conj lines (join " " next-line))))
             (join (str "\n" (blank-str description-start)) lines))))
       (:doc option))
     "\n")))

(defn options-string
  "Create a very basic usage summary."
  [options & [max-line-length]]
  (assert (map? options))
  (let [options (normalize-options options)
        description-start (apply max
                                 (map (comp count token-string)
                                      options))]
    (join ""
          (map #(option-usage-string % description-start max-line-length)
               options))))
