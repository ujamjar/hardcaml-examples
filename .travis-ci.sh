# install OCaml + OPAM
case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
4.02.1,1.2.0) ppa=avsm/ocaml42+opam12 ;;
4.02.3,1.2.0) ppa=avsm/ocaml42+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac
	 
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
opam init 
eval `opam config env`

opam install oasis
opam pin add -n lambda-term git://github.com/andrewray/lambda-term#move-focus
opam pin add -n hardcaml git://github.com/ujamjar/hardcaml
opam pin add -n hardcaml-waveterm git://github.com/ujamjar/hardcaml-waveterm
opam pin add -n hardcaml-framework git://github.com/ujamjar/hardcaml-framework

opam pin add -n $OPAMPKG -k git .
opam depext -y $DEPPKGS $OPAMPKG

ocaml -version
opam install $DEPPKGS $OPAMPKG

