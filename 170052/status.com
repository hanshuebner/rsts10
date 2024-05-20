$!
$! STATUS.COM - Build the Statistics package
$!
$! Modification History:
$!
$! Date		Edit		Reason
$! ====		====		======
$! 31-Jul-84	001 KPH		Creation
$!
$!
$!
$! Set up defaults for symbols
$!
$!	P1 = Account statistics lives in
$!	P2 = Account where utilities can be found
$!
$ IF P1.EQS."" THEN $P1 = "D:[170,52]"
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
$ SHOW LOG/SYS/ALL
$!
$! Assemble the MACRO subroutine
$!
$ MACRO SYSTEM:DSKPEK/OBJECT=SYSTEM:DSKPEK/LIST=SYSTEM:DSKPEK
$!
$! Assemble the BP2 components
$!
$ RUN D:$BP2IC2
OLD SYSTEM:STATUS
COM SYSTEM:STATUS/OBJ/CHAIN/NOWARN/LIST/CROSS:NOKEY
BUI SYSTEM:STATUS,SYSTEM:DSKPEK
OLD SYSTEM:QSTATS
COM SYSTEM:QSTATS/OBJ/CHAIN/NOWARN/LIST/CROSS:NOKEY
SCRATCH
EXIT
$!
$! Now task-build the statistics package
$!
$ TKB @SYSTEM:STATUS
$ TKB @SYSTEM:QSTATS
$ EXIT
