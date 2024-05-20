$!******************************************************************************
$!
$! UTLBLD.COM - command file to build the RSX UTL libraries.
$!
$!******************************************************************************
$!
$!
$ _ON ERROR THEN GOTO EXIT
$ _SET ECHO
$ _SET JOB/PRIORITY=-16
$!
$ _ASSIGN/USER/REPLACE D:         BL
$ _ASSIGN/USER/REPLACE D:$        LIB
$ _ASSIGN/USER/REPLACE D:[170,0]  CMN
$ _ASSIGN/USER/REPLACE D:[170,90] RMS
$!
$! Set up logicals to point to local (test) account
$!
$ _ASSIGN/USER/REPLACE D:[170,53] RSX
$ _ASSIGN/USER/REPLACE D:[170,60] FCS
$ _ASSIGN/USER/REPLACE D:[170,61] UTL
$ _ASSIGN/USER/REPLACE D:[170,62] MAC
$ _ASSIGN/USER/REPLACE D:[170,63] LBR
$ _ASSIGN/USER/REPLACE D:[170,64] TKB
$ _ASSIGN/USER/REPLACE D:[170,65] RNO
$ _ASSIGN/USER/REPLACE D:[170,66] PAT
$ _ASSIGN/USER/REPLACE D:[170,67] CRF
$!
$ _ASSIGN/USER/REPLACE D:[170,61] SYSTEM
$ _ASSIGN/USER/REPLACE D:[170,61] LST
$ _ASSIGN/USER/REPLACE D:[170,53] LB
$!
$ _SET PROT/DEFAULT 60
$!
$! Delete old versions
$!
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.BAK
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.OBJ
$ _DELETE/NOLOG/NOWARN  SYSTEM:*.LST
$!
$! Set protections
$!
$ _SET FILE/PROT=40 SYSTEM:CSIMAC.MAC
$ _SET FILE/PROT=40 SYSTEM:GCLMAC.MAC
$ _SET FILE/PROT=40 SYSTEM:EGCMMA.MAC
$ _SET FILE/PROT=40 SYSTEM:HLPMAC.MAC
$ _SET FILE/PROT=40 SYSTEM:TPMAC.MAC
$ _SET FILE/PROT=40 SYSTEM:DSMAC.MAC
$ _SET FILE/PROT=40 SYSTEM:TKBRTO.CMD
$!
$RUN LIB:MAC
@SYSTEM:CMLASM
@SYSTEM:TKBUTL
$EOD
$!
$RUN LIB:CRF
@SYSTEM:CMLCRF
@SYSTEM:TKBCRF
$EOD
$!
$RUN LIB:LBR
SYSTEM:SYSLIB.OLB/CR:167.:1024.:35.:OBJ
SYSTEM:UTLNEP.OLB/CR:167.:1024.:35.:OBJ
@SYSTEM:CMLLIB
@SYSTEM:TKBLIB
@SYSTEM:UTLNEP
$EOD
$!
$ _DELETE/NOLOG SYSTEM:*.OBJ
$!
$RUN LIB:LBR
SYSTEM:SYSLIB.OBJ=SYSTEM:SYSLIB/EX
SYSTEM:UTLNEP.OBJ=SYSTEM:UTLNEP/EX
$EOD
$!
$!
$! Set protections
$!
$ _SET FILE/PROT=40 SYSTEM:SYSLIB.OBJ
$ _SET FILE/PROT=40 SYSTEM:UTLNEP.OBJ
$ _SET FILE/PROT=40 SYSTEM:LOAD.DGE
$ _SET FILE/PROT=40 SYSTEM:AUTOT.DGE
$ _SET FILE/PROT=40 SYSTEM:AUTO.DGE
$ _SET FILE/PROT=40 SYSTEM:AUTOX.DGE
$ _SET FILE/PROT=40 SYSTEM:AUTOY.DGE
$ _SET FILE/PROT=40 SYSTEM:OVCTC.DGE
$ _SET FILE/PROT=40 SYSTEM:OVCTR.DGE
$ _SET FILE/PROT=40 SYSTEM:OVCTL.DGE
$!
$EXIT:
$ _IF $SEVERITY .EQ. 1 THEN _WRITE 0 "UTLBLD completed successfully"
$ _IF $SEVERITY .NE. 1 THEN _WRITE 0 "UTLBLD completed with errors"
$!
$ _SET JOB/PRIORITY=-8
$ _exit

