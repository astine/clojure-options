(ns clojure-options.core-test
  (:require [clojure.test :refer :all]
            [clojure-options.core :refer :all]))

(def token-set-one
  ["-afgo.txt" "--alpha" "stay.txt" "--file" "return.txt" "loop.txt" "--" "foo" "bar" "baz"])

(def token-set-two
  ["-afgo.txt" "--alpha=stay.txt" "--file" "return.txt" "--" "foo" "bar" "baz"])

(def token-set-three
  ["-f12.3" "--alpha" "123" "--file" "12.3" "loop.txt" "--" "foo" "bar" "baz"])

(def token-set-four
  ["-h"])

(deftest getopts-test
  (testing "getopts"
    (is (= (getopts token-set-one "af:j" ["alpha" "file="])
           ["a" "f" "go.txt" "alpha" "file" "return.txt" "--" "stay.txt" "loop.txt" "foo" "bar" "baz"]))
    (is (= (getopts token-set-one "af:j" ["alpha=" "file="])
           ["a" "f" "go.txt" "alpha" "stay.txt" "file" "return.txt" "--" "loop.txt" "foo" "bar" "baz"]))
    (is (= (getopts token-set-one "a:f:j" ["alpha" "file="])
           ["a" "fgo.txt" "alpha" "file" "return.txt" "--" "stay.txt" "loop.txt" "foo" "bar" "baz"]))
    (is (= (getopts token-set-two "a:f:j" ["alpha=" "file="])
           ["a" "fgo.txt" "alpha" "stay.txt" "file" "return.txt" "--" "foo" "bar" "baz"])))
  (testing "getopts parsing exceptions"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Warning: Used '=' with option that doesn't take a parameter: '--alpha'"
                          (getopts token-set-two "a:f:j" ["alpha" "file="])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Warning: Invalid option: '--alpha'"
                          (getopts token-set-two "a:f:j" ["file="])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Warning: Invalid option: '-a'"
                          (getopts token-set-two "f:j" ["alpha=" "file="])))
    ))

(deftest let-cli-options-test
  (testing "let-cli-options"
    (is (= (let-cli-options [alpha ^String file & free] token-set-one [alpha file free])
           [true "return.txt" ["stay.txt" "loop.txt" "foo" "bar" "baz"]]))
    (is (= (let-cli-options [^String alpha ^String file & free] token-set-two [alpha file free])
           ["stay.txt" "return.txt" ["foo" "bar" "baz"]]))
    (is (= (let-cli-options [^long alpha ^double file & free] token-set-three [alpha file free])
           [123 12.3 ["loop.txt" "foo" "bar" "baz"]])))
  (testing "let-cli-options help"
    (is (= (with-out-str (let-cli-options [^{:doc "This is an option"} ^long alpha ^{:default "text.txt" :tag String} file & free] token-set-four [alpha file free]))
           "Usage: Executable Name [-a | --alpha=<alpha>] [-f | --file=<file>] [-h | --help]

  The following are the available options for this application.

Options:
  -a --alpha NUM             This is an option
  -f --file STRING text.txt  
  -h --help                  Show usage summary
"))
    (is (= (with-out-str (let-cli-options [^{:doc "This is an option"} ^String alpha & free] token-set-one [alpha free]))
           "Warning: Invalid option: '--file'
Usage: Executable Name [-a | --alpha=<alpha>] [-h | --help]

  The following are the available options for this application.

Options:
  -a --alpha STRING  This is an option
  -h --help          Show usage summary
"))
    (is (= (with-out-str (let-cli-options [^{:doc "This is an option"} ^long alpha ^long file & free] token-set-one [alpha free]))
           "Error parsing parameter: Invalid long integer: stay.txt
Usage: Executable Name [-a | --alpha=<alpha>] [-f | --file=<file>] [-h | --help]

  The following are the available options for this application.

Options:
  -a --alpha NUM  This is an option
  -f --file NUM   
  -h --help       Show usage summary
"))
    ))

(defmain [alpha beta gamma ^String epsilon]
  [alpha beta gamma epsilon])

(deftest defmain-test
  (testing "defmain"
    (is (= (-main ["-a" "-b"]) [true true nil nil]))
    (is (= (-main ["-ab"]) [true true nil nil]))
    (is (= (-main ["-eab"]) [nil nil nil "ab"]))
    (is (= (-main ["-a" "--epsilon" "foo"]) [true nil nil "foo"]))
    (is (= (-main []) [nil nil nil nil]))))
