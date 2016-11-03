# Add opam dependencies here
OPAM_DEPENDS="ocamlfind ounit re"

opam init -a
opam install ${OPAM_DEPENDS} -y
eval `opam config env -y`

make
make test