$JOB/CCL/NOLIMIT [170,36]
ASSIGN LST: NL
ASSIGN NL: LST
DEASSIGN NL
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
$SWITCH RT11
$R SY:LINK
SRCCOM.SV/Z/K:28.,LST:SRCCOM/W,SY:SRCCOM=SRCCOM
$R SY:SILUS
SRCCOM.SAV/S,LST:SRCCOM=SRCCOM.SV
$R SY:PIP
SRCCOM.SV,SRCCOM.OBJ,SRCCOM.STB/DE/NOLOG
$EOJ
