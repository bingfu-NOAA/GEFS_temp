#!/bin/ksh
#

# EXPORT list here
set -x
export IOBUF_PARAMS=
export FORT_BUFFERED=TRUE
export MKL_CBWR=AVX
ulimit -s unlimited
ulimit -a

export ATP_ENABLED=0
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=134217728

export MPICH_ABORT_ON_ERROR=1
export MPICH_ENV_DISPLAY=1
export MPICH_VERSION_DISPLAY=1
export MPICH_CPUMASK_DISPLAY=1

export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export MP_SHARED_MEMORY=no
export MEMORY_AFFINITY=core:6

export total_tasks=6
export OMP_NUM_THREADS=6
export taskspernode=4

#Date and Cycle
#export cyc=00
#export PDY=20160415
#export cyc_fcst=00
#export job=Aa2016041500100
export FORECAST_SEGMENT=hr

# export for development runs only begin
export envir=${envir:-dev}
export RUN_ENVIR=${RUN_ENVIR:-dev}

export gefsmpexec_mpmd=" mpirun.lsf"
export APRUNC="mpirun.lsf"

export APRUN_RECENT="mpirun.lsf"
export APRUN_CHGRES="mpirun.lsf"
export aprun_gec00="mpirun.lsf"

export NTHREADS_SIGCHGRS=6

. $GEFS_ROCOTO/parm/setbase
. $GEFS_ROCOTO/parm/gefs_config
. $GEFS_ROCOTO/parm/gefs_dev.parm


# CALL executable job script here
$SOURCEDIR/jobs/JGEFS_ENSAVG_NEMSIO

