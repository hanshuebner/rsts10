$JOB/CCL/NOLIMIT/PRIORITY:-16 [170,40]
$ASSIGN  LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: SY
$ASSIGN D: DK
$ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NOWARN/LOG
$SIZE 24
$R SY:MACRO
TBL,LSD:TBL/C=#COMMON,#KERNEL,CONFIG.MAC,#KBDEF,CHECK,TBL
$EOJ
