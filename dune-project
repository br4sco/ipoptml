(lang dune 2.9)
(generate_opam_files true)
(name ipoptml)
(maintainers "Oscar Eriksson <oerikss@kth.se>")

(package
 (name ipoptml)
 (synopsis "OCaml bindings for Ipopt")
 (description "OCaml bindings for the constrained Non-Linear Program solver Ipopt.")
 (depends dune ctypes ctypes.foreign))
 