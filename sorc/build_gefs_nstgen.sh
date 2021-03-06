#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

progname=gefs_nstgen

if [ -f ../modulefiles/gefs/gefs_$target.ver ]; then
    source ../modulefiles/gefs/gefs_$target.ver
fi
source ../modulefiles/gefs/${progname}.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
#
cd ${progname}.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-""}
export OMPFLAGM=${OMPFLAGM:-""}

export INCSM="-I ${SFCIO_INC4} -I ${NEMSIO_INC}"

export LIBSM="${NEMSIO_LIB} ${BACIO_LIB4} ${SFCIO_LIB4} ${W3NCO_LIB4}"


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
