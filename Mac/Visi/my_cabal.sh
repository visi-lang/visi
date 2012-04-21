#!/bin/sh
# Derived from build-iphone-haskell.sh
echo "my_cabal.sh running with ACTION=$ACTION"

# Do this to get c2hs
export PATH=$PATH:~/Library/Haskell/bin

DIST="$TARGET_BUILD_DIR/dist"
CABAL=cabal
# HASKELL_LIB=/Library/Frameworks/GHC.framework/Versions/7.4.1-x86_64/usr/lib/ghc-7.4.1
# USER_LIB=${HOME}/Library/Haskell/ghc-7.4.1/lib/


function depends_on( )
{
  if [ "$1" == 'builtin_rts' ] ; then
    return
  fi
  
  local from=$1
  local known=$2
  if [ "`expr \"$known\" : \"$from\"`" != "0" ] ; then
    return
  fi
  local known="$from\n$known"
  local all=$(ghc-pkg field $from depends | grep -o " .*" | grep -o "[^ ]*$" | grep -o "[^-]*.[^-]*" | grep -v "^-")
  local x=""
  for x in $all ; do
    known="${known}\n$(depends_on $x $known)"
  done
  local k2=""
  for x in $known ; do
    k2="$k2\n$x"
  done
  
  echo $k2 | sort | uniq
}

pkg_ver () {
  local p=$1
  local n=$(ghc-pkg field $p name | grep -o "[^ ]*$")
  local v=$(ghc-pkg field $p version | grep -o "[^ ]*$")
  echo "${n}-${v}"
}

case "$ACTION" in
  "")
    mkdir -p "$DIST" || exit 1
    $CABAL --extra-include-dirs="$(pwd)/Visi" --extra-include-dirs="$(pwd)/Visi/include" --distpref="$DIST" configure || exit 1
    $CABAL --distpref="$DIST" --verbose build || exit 1
    rm -f WholeVisi.o

    if [ $(diff  $DIST/build/autogen/cabal_macros.h  $DIST/build/autogen/cabal_macros.h_cache 2>&1 | wc -l) == "0" ] ; then
     echo "using cached libtool thing"
    else
        root=$(cat $DIST/build/autogen/cabal_macros.h | grep package | grep -o '[^ ]* \*' | grep -o '[^ ]*')

        dep=""
        dep2=""

        for x in $root; do
          dep2=$(depends_on "$x" "$dep")
          dep3=""
          for y in $dep2; do
            dep3="${dep3}\n${y}"
          done
          dep=$(echo $dep3 | sort | uniq)
        done

        d=""

        dep_rts="rts ${dep}"

        for x in ${dep_rts}; do
            z=$(ghc-pkg field $x library-dirs | grep -o "[^ ]*$")
            q=$z
            d="$d\n$q"
        done

        bigl=""

        for x in $(echo $d | sort | uniq); do
            bigl="${bigl} -L${x}"
        done

        pkgs=""
        for x in $dep; do
           pkgs="${pkgs}\n$(pkg_ver $x)"
        done

        small=""

        for x in $(echo $pkgs | sort | uniq); do
            small="${small} -lHS${x}"
        done

        cp $DIST/build/autogen/cabal_macros.h $DIST/build/autogen/cabal_macros.h_cache

        echo "libtool -v -static -o WholeVisi.o $DIST/build/HSVisi-0.1.o $bigl $small -lHSrts_thr 2>&1 | (grep -v 'libtool: warning same member name')" >  $DIST/build/autogen/cabal_link_cmd.sh

    fi

    sh $DIST/build/autogen/cabal_link_cmd.sh || exit 1

    ;;
  clean)
    rm -f $DIST/build/autogen/cabal_macros.h_cache
    rm -f $DIST/build/autogen/cabal_link_cmd.sh
    $CABAL --distpref="$DIST" clean || exit 1
    ;;
  *)
    echo "my_cabal.sh: Unknown action '$1'"
    exit 1
    ;;
esac

 
