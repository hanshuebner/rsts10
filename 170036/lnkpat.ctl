$JOB/CCL/NOLIMIT [170,36]
ASSIGN LST: NL
ASSIGN NL: LST
DEASSIGN NL
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
$SWITCH RT11
$R SY:LINK
PATCH.SV/Z/K:28.,LST:PATCH/W,SY:PATCH=PATCH
$R SY:LINK
PAT.SV/Z/K:28.,LST:PAT/W,SY:PAT=PAT
$R SY:SILUS
PATCH.SAV/S,LST:PATCH=PATCH.SV
$R SY:SILUS
PAT.SAV/S,LST:PAT=PAT.SV
$R SY:PIP
PAT.SV,PAT.OBJ,PAT.STB/DE/NOLOG
PATCH.SV,PATCH.OBJ,PATCH.STB/DE/NOLOG
$EOJ
