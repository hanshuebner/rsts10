;
; TKB .CMD FILE FOR PAT
; ON A MAPPED RSX-11M SYSTEM
; LINKING TO FCSRES
; CREATED BY SGNBLDBLD.CMD VERSION 1.62
;
[1,54]PATRES/-FP/CP/MM,[1,34]PATRES/-SP=
[1,24]PATRESBLD/MP
TASK=...PAT
STACK=128
UNITS=4
ASG=TI:1
ASG=TI:2
PAR=GEN:0:40000
EXTSCT=$$ERB0:100
TSKV=$ERVCT:10
LIBR=FCSRES:RO
/