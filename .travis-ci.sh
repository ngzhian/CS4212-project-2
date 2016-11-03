# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ounit re"
	 
echo "yes" | sudo add-apt-repository ppa:avsm/ocaml41+opam11
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
make test