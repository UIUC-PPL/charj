#!/bin/bash
set -e
set -o pipefail
if [[ -z "${CHARM_HOME}" ]]; then
    >&2 echo "Expected to find CHARM_HOME environment variable."
    >&2 echo "Please define it then try again."
    exit -1
fi
XLAT="${CHARM_HOME}/src/xlat-i"
if [[ ! -f "${XLAT}/interfaceBuilder.h" ]]; then
    >&2 echo "Expected to find XI Builder under CHARM_HOME."
    >&2 echo "Please make sure your copy of Charm++ is up-to-date."
    exit -1
fi
echo "--- RUNNING SBT TASK ---"
sbt assembly
echo "--- MOVING JARFILE TO BIN/ ---"
mkdir -p bin
cp target/scala-*/charj-assembly-*.jar bin/charj.jar
echo "--- BUILDING OBJ FILES ---"
mkdir -p obj
# TODO ensure that we use the same compiler/flags as the one used to build Charm++
INCLUDES="-I${XLAT} -I${XLAT}/sdag -I${XLAT}/sdag/constructs -I${CHARM_HOME}/include"
CFLAGS="${CFLAGS} ${INCLUDES} -w"
for f in ${XLAT}/*.C ${XLAT}/sdag/*.C ${XLAT}/sdag/constructs/*.C; do
    o="obj/$(basename $f .C).o"
    if [[ "${o}" == "obj/xi-main.o" ]]; then
        cmd="cc $f -c -o $o $CFLAGS -DXI_LIBRARY=1"
    else
        cmd="cc $f -c -o $o $CFLAGS"
    fi
    echo $cmd
    eval $cmd
done
echo "--- DONE ---"
export CHARJ_HOME=$(pwd)
echo "Add CHARJ_HOME=$(pwd) to your environment."
echo "Consider aliasing 'charjc' and 'java -jar ${CHARJ_HOME}/bin/charj.jar'."
echo ""
echo "To test, run an example program like:"
echo "java -jar $CHARJ_HOME/bin/charj.jar ${CHARJ_HOME}/src/test/charj/parallel/hello.cp"