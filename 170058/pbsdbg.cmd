;
;	PBSDBG.CMD - TKB command file to build PBSDBG.TSK
;
;	COPYRIGHT (C) 1983
;	DIGITAL EQUIPMENT CORP., MAYNARD, MASS.
;
;	Task-build using RMS resident library RMSRES
;
SYSTEM:PBSDBG/FU,LST:PBSDBG/MA/CR/-SP=SYSTEM:PBSDBG/MP
TASK=PBSDBG
UNITS=12
EXTSCT=PATCH:1000
LIBR=RMSRES:RO
;include global symbols
;@SYSTEM:SYMBOL
GBLPAT=PBSDBG:..RUN:240
//
