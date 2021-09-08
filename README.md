# ipoptml
OCaml bindings for [IPOPT](https://coin-or.github.io/Ipopt/index.html).

This implementation is in a early stage of development.

## Installation
### Install IPOPT

On Ubunut 20.04 do:
```
sudo apt-get install coinor-libipopt-dev
```

On OS X with homebrew do (untested):
```
brew install ipopt --with-openblas
```

For details on other ways of installing IPOPT, see
[here](https://coin-or.github.io/Ipopt/INSTALL.html)

### Install this package
Install `opam` libraries: 
```
opam install dune ctypes ctypes-foreign
```

Build the package:
```
dune build
```

Run the tests:
```
dune runtest
```

Install for the current user:
```
dune install
```

The module `Ipoptml` should now be availible for the current user.
