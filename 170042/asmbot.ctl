$JOB/CCL/NOLIMIT/PRIORITY:-16 [170,42]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: SY
$ASSIGN D: DK
$ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NO/NOLOG
$SIZE 20
$R SY:MACRO
BLDBOT=BLDBOT
$EOJ
