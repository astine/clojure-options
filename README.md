# clojure-options

This is a clojure adaptation of my common lisp unix-options library. It is a cli option parsing library. It exists because I don't like the current interface of tools.cli. 

The basic usage is simple. Provide a list of variable names that you want bound and the clojure-options will figure out how to parse the tokens passed in on the command line to populate those variables. EG:

```clojure
(defmain [config verbose]
  [config verbose])

(-main "-c") => [true nil]

(-main "--verbose") => [nil true]

(-main "-cv") => [true true]
```

More control can be had by providing meta data to control how the variable names are interperated by the parser, or by setting some constants.

Other features include a usage summery generator and automatic response to invalid command line options.

## Install

Add this to the dependencies in your `project.clj`:

```clojure
[clojure-options "0.1.0"]
```

## Usage

 _defmain_ `(defmain spec & body)`
  The primary interface for binding cli options. Given a list of function 
  arguments and a function body it will define a function named '-main' that 
  takes a tokenized list of command line options, parses them, and binds them
  to the variable names in the function body.

```clojure
 tokens = ["-afgo.txt" "--alpha" "stay.txt"
           "--file" "return.txt" "loop.txt"]

 (defmain [alpha ^String file & free-tokens]
   [alpha file free-tokens])

 (apply -main tokens)
 => [true "return.txt" ("loop.txt" "stay.txt")]
```

 _let-cli-options_ `(let-cli-options spec tokens & body)`
  A simple interface for binding cli options. Simply provide a list of variable
  names that you want let-bound and clojure-options will figure out the right
  way to parse the cli tokens.

```clojure
 tokens = ["-afgo.txt" "--alpha" "stay.txt"
           "--file" "return.txt" "loop.txt"]

 (let-cli-options [alpha ^String file & free-tokens] tokens
   [alpha file free-tokens])
  => [true "return.txt" ("loop.txt" "stay.txt")]
```

 _getopts_ `(getopts tokens short-options long-options`
  A traditional command-line option parser of the same general format as
  getopt from the Unix cli. Return the parsed command-line arguments as a list
  with "--" separating the valid options from the free arguments. For example:

```clojure
 tokens = ["-afgo.txt" "--alpha" "stay.txt"
           "--file" "return.txt" "loop.txt"]

 (getopts tokens "af:j" ["alpha" "file="])
  =>  ["a" "f" "go.txt" "alpha" "file" "return.txt" "--"
       "stay.txt" "loop.txt"]
```

### Spec

The `spec` is a list of variables that will be bound by either `let-cli-options` or `defmain`. This is how `clojure-options` figures out how to interperate the token list. Generally, a short option "-a" and a long option "--alpha" is generated for each variable name provided and any token passed in matching either form is bound to the variable name. If two variables are passed in starting with the same first letter, the short option of the second pushed to the next letter of the alphabet. So "-a" -> "-b". 

```clojure
[alpha america delta]
["-a" "--delta"]
--
alpha = true
america = nil
delta = true
```

Any free tokens (those not bound to a specific option) will collected in a list which will be bound to a list named by the first variable name after `&`. If `&` is not present, then the variable name will default to `free`. If the first value after `&` is a list, then the free option list will be destructured and bound to the variables names in the list.

```clojure
[& [file1 file2]]
["--" "foo.txt" "bar.txt"]
--
file1 = "foo.txt"
file2 = "bar.txt"
```

### Metadata

Metadata can be added to the variables in a `spec` to derive more control over the parsing and binding process. He are some valid metadata fields:

field   |  usage
--------|------
tag     | (ie a type hint eg: ^String) If not blank, specifies that the corresponing option takes a parameter. The tag value specifies the type. Currently, `String`, `long`, and `double` are supported. A default parser exists for each of these types. If left blank, The option is a boolean option and will either be set to `true` or `nil` depending on if it is present in the tokens list.
doc     | A documentation string for use in usage printouts,
default | The default value for this option, when it's not present in the token list. This `default` defaults to `nil`.
parser  | A function that will be used to parse cli options. Usually defaults to `identity` or is picked based on the value of `tag`

### Constants

In addition there are a few constants that can be set for more flexibility. Remember to add the `^:const` directive so that there values will be available at macro expansion time. EG:

```clojure
(def ^:const *program-description* "This program is obviously awesome")
```

Some available constants are:

constant              | default value   | description 
----------------------|-----------------|------------
+program-name+        |"Executable Name"|"The following are the available options for this application."
+program-description+ |"The following are the available options for this application."|"A short plaintext description of the application."
+help-option?+        | true |"If truthy, the parser will insert an implicit 'help' option. If a string or a symbol, the tokens for the option will be generated from it, else the tokens will be \"h\" and \"help\"."

## License

Copyright © 2015 Andrew Stine

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
