$JOB/CCL/NOLIMIT [170,46]
!ASSIGN LST: NL
!ASSIGN NL: LST
DEASSIGN NL
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NO/NOLOG
$SIZE 20
$R SY:MACRO
PIP,PIP/C=PIP
$EOJ
