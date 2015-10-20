# clojure-options

This is a clojure adaptation of my common lisp unix-options library. It is currently incomplete but the 'getopts' function works.

## Usage

getopts
  A traditional command-line option parser of the same general format as
  getopt from the Unix cli. Return the parsed command-line arguments as a list
  with "--" separating the valid options from the free arguments. For example:

     tokens = ["-afgo.txt" "--alpha" "stay.txt"
               "--file" "return.txt" "loop.txt"]

     (getopts tokens "af:j" ["alpha" "file="])
      =>  ["a" "f" "go.txt" "alpha" "file" "return.txt" "--"
           "stay.txt" "loop.txt"]

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
