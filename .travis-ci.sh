# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ounit re"
	 

sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`

make
make test