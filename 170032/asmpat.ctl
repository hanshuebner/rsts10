$JOB/CCL/NOLIMIT/PRIORITY:-16 [170,32]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: SY
$ASSIGN D: DK
$ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NOWARN/LOG
$SIZE 28
$R SY:MACRO
ONLPAT,LSD:ONLPAT/C=#COMMON,#KERNEL,#RTCOM,[170,42]INIPFX,ONLPAT
$SIZE 28
$R SY:MACRO
ONLINE,LSD:ONLINE/C=#COMMON,#KERNEL,#RTCOM,[170,42]INIPFX,ONLINE
$EOJ
