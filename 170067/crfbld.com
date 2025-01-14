$!******************************************************************************
$!
$! CRFBLD.COM - command file to build the RSX CRF libraries.
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
$ _ASSIGN/USER/REPLACE D:[170,67] SYSTEM
$ _ASSIGN/USER/REPLACE D:[170,67] LST
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
@CRF:CRFASM
$EOD
$!
$RUN LIB:CRF
@CRF:CRFCRF
$EOD
$!
$RUN LIB:LBR
@CRF:CRFLIB
$EOD
$!
$ _DELETE/NOLOG SYSTEM:*.OBJ
$!
$! Set protections
$!
$ _SET FILE/PROT=40 SYSTEM:CRF.OLB
$!
$RUN LIB:TKB
@CRF:CRFBLD
$RUN LIB:CRF
SYSTEM:CRF.MAP
$EOD
$!
$RUN LIB:MAKSIL
CRF/DEBUG
SYSTEM:CRF.TSK
YES
SYSTEM:CRF.STB
SYSTEM:CRF.SIL
$EOD
$!
$ _DELETE/NOLOG SYSTEM:CRF.TSK
$ _DELETE/NOLOG SYSTEM:CRF.STB
$ _RENAME SYSTEM:CRF.SIL SYSTEM:CRF.TSK
$!
$EXIT:
$ _IF $SEVERITY .EQ. 1 THEN _WRITE 0 "CRFBLD completed successfully"
$ _IF $SEVERITY .NE. 1 THEN _WRITE 0 "CRFBLD completed with errors"
$!
$ _SET JOB/PRIORITY=-8
$ _exit

