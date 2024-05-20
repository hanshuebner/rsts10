$ SET NOON

$!+
$! TKBUTL.COM - Build the System Utility Manager UTLMGR
$!-

$RUN $UTILTY
REMOVE LOGICAL LIB
ADD LOGICAL D:$ LIB
REMOVE LOGICAL CMN
ADD LOGICAL D:[170,0] CMN
REMOVE LOGICAL CUI
ADD LOGICAL D:[170,19] CUI
LIST LOGICAL
EXIT
$EOD

$ASSIGN D:[1,1] LB
$ASSIGN D:[170,101] SYSTEM
$ASSIGN D:[170,101] LST

$SHOW LOGICAL

$RUN LIB:TKB
@SYSTEM:UTLMGR
$EOD

$RUN LIB:PIP
SYSTEM:UTLMGR.TSK<232>/RE/W
$EOD
$!
$! UTLMGR build complete
$!
$EXIT