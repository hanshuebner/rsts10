$JOB/CCL/NOLIMIT/PRIORITY:-16 [170,30]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: SY
$ASSIGN D: DK
$ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NOWARN/NOLOG
$SIZE 20
$R SY:MACRO
DEFALT,LSD:DEFALT/C=#COMMON,#KERNEL,DEFALT
$SIZE 20
$R SY:MACRO
ODT,LSD:ODT/C=#COMMON,#KERNEL,ODT
$EOJ