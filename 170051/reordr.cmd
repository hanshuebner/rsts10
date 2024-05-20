; REORDR.CMD - Build regular and I/D flavors of REORDR
;
; First build the regular version
;
INPUT:[170,51]REORDR/FP=INPUT:[170,51]REORDR,D:[1,1]BP2OTS/LB
/
UNITS = 13
ASG = SY:5:6:7:8:9:10:11:12
EXTTSK =  512 
; Set the size of the I/O buffers
GBLDEF=RECSIZ:20000
/
;
; Now build the I/D version
;
INPUT:[170,51]RDRIDS/FP/ID=INPUT:[170,51]REORDR,D:[1,1]BP2OTS/LB
/
UNITS = 13
ASG = SY:5:6:7:8:9:10:11:12
EXTTSK =  512 
; Set the size of the I/O buffers
GBLDEF=RECSIZ:40000
//
