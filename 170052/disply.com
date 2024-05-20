$!
$! DISPLY.COM - Build the DISPLY.TSK program
$!
$! Modification History:
$!
$! Date		Edit		Reason
$! ====		====		======
$! 01-Jun-90	001 DLS		Creation
$!
$!
$! Set up defaults for symbols
$!
$!	P1 = Source account and DISPLY.TSK destination account
$!
$ _IF P1 .EQS. "" THEN $P1 = "D:[170,52]"
$
$! Set up private logical names
$!
$ _SET NOON
$ _DEASSIGN SYSTEM:
$ _SET ON
$ _ASSIGN 'P1' SYSTEM
$ _ASSIGN D:[1,1] LB
$!
$! Set up system logical names
$!
$ _DEASSIGN/SYSTEM CMN
$ _ASSIGN/SYSTEM D:[170,0] CMN
$!
$! Assemble the MACRO subroutine
$!
$ _MACRO SYSTEM:DSPMUL/OBJECT=SYSTEM:DSPMUL/LIST=SYSTEM:DSPMUL
$!
$! Assemble the BP2 components
$!
$ _RUN D:$BP2IC2
OLD SYSTEM:DISPLY
COM SYSTEM:DISPLY/OBJ/CHAIN/NOWARN/DOUBLE/LIST/CROSS:NOKEY
BUI SYSTEM:DISPLY,SYSTEM:DSPMUL
SCRATCH
EXIT
$!
$! Now task-build the DISPLY program
$!
$ _TKB @SYSTEM:DISPLY
$ _SET FILE/PROT=232 SYSTEM:DISPLY.TSK
$ _EXIT
