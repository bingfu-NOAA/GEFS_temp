#ifort -FR -c checkCTP2.f90
#ifort -o checkCTP2.exe checkCTP2.o -L/gpfs/hps/nco/ops/nwprod/lib/g2/v2.5.0/intel/ -lg2_v2.5.0_4 -lbacio_4 -lw3nco_4
basetmp1=$basetmp1
date=$date
sorc_dir=$basetmp1/tmpnwprd_rerun/SubX_reforecast_${date}/sorc
exec_dir=$basetmp1/tmpnwprd_rerun/SubX_reforecast_${date}/exec
mkdir -p $exec_dir
lib_g2=/gpfs/hps/nco/ops/nwprod/lib/g2/v2.5.0/intel/include/g2_v2.5.0_4
lib_lg2=-L/gpfs/hps/nco/ops/nwprod/lib/g2/v2.5.0/intel
lib_bacio=-L/gpfs/hps/nco/ops/nwprod/lib/bacio/v2.0.1/intel
lib_lip=-L/gpfs/hps/nco/ops/nwprod/lib/ip/v3.0.0/intel
lib_w3=-L/gpfs/hps/nco/ops/nwprod/lib/w3nco/v2.0.6/intel
lib_sp=-L/gpfs/hps/nco/ops/nwprod/lib/sp/v2.0.2/intel
ifort -O -I $lib_g2 $sorc_dir/gefs_daily_ave_1mem_p1.f90 $sorc_dir/prlevel.f $sorc_dir/printinfr.f90 $lib_lg2 -lg2_v2.5.0_4 $lib_bacio -lbacio_v2.0.1_4 $lib_lip -lip_v3.0.0_4 $lib_w3 -lw3nco_v2.0.6_4 $lib_sp -lsp_v2.0.2_4 -ljasper -lpng -lz -o $exec_dir/gefs_daily_ave_1mem_p1.exe 
