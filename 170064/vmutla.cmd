; VMUTLA.CMD
;
; MAC COMMAND FILE TO ASSEMBLE THE TKB VIRTUAL WORK FILE ROUTINES 
;
SYSTEM:ALBLK ,LST:ALBLK /CR/-SP=RSX:COMMON,TKB:MACFLM,ALBLK
SYSTEM:ALSVB ,LST:ALSVB /CR/-SP=RSX:COMMON,TKB:MACFLM,ALSVB
SYSTEM:ALVRT ,LST:ALVRT /CR/-SP=RSX:COMMON,TKB:MACFLM,ALVRT
SYSTEM:CVRL  ,LST:CVRL  /CR/-SP=RSX:COMMON,TKB:MACFLM,CVRL
SYSTEM:CVRS  ,LST:CVRS  /CR/-SP=RSX:COMMON,TKB:MACFLM,WRKST,CVRL
SYSTEM:EXTSK ,LST:EXTSK /CR/-SP=RSX:COMMON,TKB:MACFLM,EXTSK
SYSTEM:FNDPG ,LST:FNDPG /CR/-SP=RSX:COMMON,TKB:MACFLM,FNDPG
SYSTEM:GTCOR ,LST:GTCOR /CR/-SP=RSX:COMMON,TKB:MACFLM,GTCOR
SYSTEM:GTCOS ,LST:GTCOS /CR/-SP=RSX:COMMON,TKB:MACFLM,WRKST,GTCOR
SYSTEM:INIDM ,LST:INIDM /CR/-SP=RSX:COMMON,TKB:MACFLM,INIDM
SYSTEM:INIVM ,LST:INIVM /CR/-SP=RSX:COMMON,TKB:MACFLM,INIVM
SYSTEM:INIVS ,LST:INIVS /CR/-SP=RSX:COMMON,TKB:MACFLM,WRKST,INIVM
SYSTEM:MRKPG ,LST:MRKPG /CR/-SP=RSX:COMMON,TKB:MACFLM,MRKPG
SYSTEM:RDPAG ,LST:RDPAG /CR/-SP=RSX:COMMON,TKB:MACFLM,FCSPR,RDPAG
SYSTEM:RDPAS ,LST:RDPAS /CR/-SP=RSX:COMMON,TKB:MACFLM,FCSPR,WRKST,RDPAG
SYSTEM:RQVCB ,LST:RQVCB /CR/-SP=RSX:COMMON,TKB:MACFLM,RQVCB
SYSTEM:VMDAT ,LST:VMDAT /CR/-SP=RSX:COMMON,TKB:MACFLM,VMDAT
SYSTEM:VMDAS ,LST:VMDAS /CR/-SP=RSX:COMMON,TKB:MACFLM,WRKST,VMDAT
SYSTEM:VMUTL ,LST:VMUTL /CR/-SP=RSX:COMMON,TKB:MACFLM,VMUTL
;
;	PIP UTILITY ROUTINES
;
SYSTEM:GTKNM ,LST:GTKNM /CR/-SP=RSX:COMMON,TKB:PIPMAC,GTKNM
