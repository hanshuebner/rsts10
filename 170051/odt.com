$!
$! ODT.COM - Build the Octal Debugging Tool (ODT) program
$!
$! Modification History:
$!
$! Date		Edit		Reason
$! ====		====		======
$! 22-Mar-90	001 JFM		Creation
$!
$!
$! Set up defaults for symbols
$!
$!	P1 = Account statistics lives in
$!	P2 = Account where utilities can be found (not used)
$!
$ IF P1.EQS."" THEN $P1 = "D:[170,51]"
$ IF P2.EQS."" THEN $P2 = "D:$"
$
$! Set up private logical names
$!
$ ASSIGN 'P1' SYSTEM
$ ASSIGN D:[1,1] LB
$!
$! Set up system logical names
$!
$ DEASSIGN/SYSTEM CMN
$ ASSIGN/SYSTEM D:[170,0] CMN
$!
$! Assemble the MACRO subroutine
$!
$ MACRO SYSTEM:SETMUL/OBJECT=SYSTEM:SETMUL/LIST=SYSTEM:SETMUL
$!
$! Assemble the BP2 components
$!
$ RUN D:$BP2IC2
OLD SYSTEM:ODT
COM SYSTEM:ODT/OBJ/CHAIN/NOWARN/DOUBLE/LIST/CROSS:NOKEY
BUI SYSTEM:ODT,SYSTEM:SETMUL
SCRATCH
EXIT
$!
$! Now task-build the ODT program
$!
$ TKB @SYSTEM:ODT
$ EXIT
