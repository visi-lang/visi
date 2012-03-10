#!/bin/sh
# Derived from build-iphone-haskell.sh
echo "my_cabal.sh running with ACTION=$ACTION"

# Do this to get c2hs
export PATH=$PATH:~/Library/Haskell/bin

DIST="$TARGET_BUILD_DIR/dist"
CABAL=cabal
HASKELL_LIB=/Library/Frameworks/GHC.framework/Versions/7.4.1-x86_64/usr/lib/ghc-7.4.1
USER_LIB=${HOME}/Library/Haskell/ghc-7.4.1/lib/



case "$ACTION" in
  "")
    mkdir -p "$DIST" || exit 1
    $CABAL --extra-include-dirs="$(pwd)/Visi" --extra-include-dirs="$(pwd)/Visi/include" --distpref="$DIST" configure || exit 1
    $CABAL --distpref="$DIST" --verbose build || exit 1
    libtool -v -static -o WholeVisi.o $DIST/build/HSVisi-0.1.o -L${HASKELL_LIB}/pretty-1.1.1.0 -L${USER_LIB}parsec-3.1.2/lib -L${USER_LIB}/c2hs-0.16.3/lib -L${USER_LIB}text-0.11.1.13/lib -L${USER_LIB}mtl-2.0.1.0/lib -L${USER_LIB}transformers-0.2.2.0/lib -L${HASKELL_LIB}/containers-0.4.2.1 -L${HASKELL_LIB}/deepseq-1.3.0.0 -L${HASKELL_LIB}/bytestring-0.9.2.1 -L${HASKELL_LIB}/array-0.4.0.0 -L${HASKELL_LIB}/base-4.5.0.0 -L${HASKELL_LIB}/integer-gmp-0.4.0.0 -L${HASKELL_LIB}/ghc-prim-0.2.0.0 -L${HASKELL_LIB} -lHSpretty-1.1.1.0 -lHSparsec-3.1.2 -lHStext-0.11.1.13 -lHSmtl-2.0.1.0 -lHStransformers-0.2.2.0 -lHScontainers-0.4.2.1 -lHSdeepseq-1.3.0.0 -lHSbytestring-0.9.2.1 -lHSarray-0.4.0.0 -lHSbase-4.5.0.0 -lHSinteger-gmp-0.4.0.0 -lHSghc-prim-0.2.0.0 -lHSrts 2>&1 | grep -v "libtool: warning same member name"  || exit 1
    ;;
  clean)
    $CABAL --distpref="$DIST" clean || exit 1
    ;;
  *)
    echo "my_cabal.sh: Unknown action '$1'"
    exit 1
    ;;
esac

 
