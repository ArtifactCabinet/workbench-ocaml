# Books
+ The OCaml system <https://ocaml.org/manual/>
+ Real World OCaml <https://dev.realworldocaml.org/>
+ Cornell CS 3110 <https://www.cs.cornell.edu/courses/cs3110/2021sp/textbook/

# Types

## Std containers
+ Lists -> immutable singly-linked lists
+ Arrays -> mutable vectors
+ Strings -> immutable vectors
+ Set/Map -> immutable trees
+ Hashtbl -> automatically growing hash
+ Buffer -> extensible strings
+ Queue -> FIFO
+ Stack ->LIFO

# How to compile an OCaml program

| Purpose         | The C lang | Byte-code | Native code |
| --------------- | ---------- | --------- | ----------- |
| Source code     | *.c        | *.ml      | *.ml        |
| Header files    | *.h        | *.mli     | *.mli       |
| Object files    | *.o        | *.cmo     | *.cmx2      |
| Library files   | *.a        | *.cma     | *.cmxa3     |
| Binary programs | prog       | prog      | prog.opt    |

+ ocamlc => byte-code compiler
+ ocamlopt => native code compiler

Use `Tuareg` inside Emacs then you can interactively run partial of the `.ml` files.

REPL =>`utop`

# Other related links

* [Xavier Leroy](https://xavierleroy.org/)

# Misc

## `Core` or `Std` or `Batteries`?

<https://ocamlverse.github.io/content/standard_libraries.html>

Just use `std` for normal tasks to avoid bloating.

Use `Core/Base` for bigger applications.

`Batteries` is community driven.

The source code of inria std is actually here and its quite elegant: https://github.com/ocaml/ocaml/tree/trunk/stdlib
