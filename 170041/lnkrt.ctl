$JOB/CCL/NOLIMIT [170,41]
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
$SWITCH RT11
$SIZE 20
$R SY:LINK
RT11/Z,RT11/W,SY:RT11=RT11/X/H:#177776/U:#4000/C
#ERR.STB
PATCH
$SIZE 20
$R SY:LINK
RT11OD/Z,RT11OD/W,SY:RT11OD=RT11/X/H:#177776/U:#4000/C
#ERR.STB,#RTSODT
PATCH
$R PIP
RT11.OBJ/DE/NOLOG
$EOJ
