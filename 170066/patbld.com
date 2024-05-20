$!******************************************************************************
$!
$! PATBLD.COM - command file to build the RSX PAT libraries.
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
$ _ASSIGN/USER/REPLACE D:[170,66] SYSTEM
$ _ASSIGN/USER/REPLACE D:[170,66] LST
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
$RUN LIB:MAC
@PAT:PATASM
$EOD
$!
$RUN LIB:CRF
@PAT:PATCRF
$EOD
$!
$RUN LIB:LBR
@PAT:PATLIB
$EOD
$!
$ _DELETE/NOLOG SYSTEM:*.OBJ
$!
$! Set protections
$!
$ _SET FILE/PROT=40 SYSTEM:PAT.OLB
$!
$RUN LIB:TKB
@PAT:PATBLD
$!
$RUN LIB:CRF
SYSTEM:PAT.MAP
$EOD
$!
$RUN LIB:MAKSIL
PAT/DEBUG
SYSTEM:PAT.TSK
YES
SYSTEM:PAT.STB
SYSTEM:PAT.SIL
$EOD
$!
$ _DELETE/NOLOG SYSTEM:PAT.TSK
$ _DELETE/NOLOG SYSTEM:PAT.STB
$ _RENAME SYSTEM:PAT.SIL SYSTEM:PAT.TSK
$!
$EXIT:
$ _IF $SEVERITY .EQ. 1 THEN _WRITE 0 "PATBLD completed successfully"
$ _IF $SEVERITY .NE. 1 THEN _WRITE 0 "PATBLD completed with errors"
$!
$ _SET JOB/PRIORITY=-8
$ _exit
