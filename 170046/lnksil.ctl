$JOB/CCL/NOLIMIT/PRI:-16 [170,46]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: SY
$ASSIGN D: DK
$SWITCH RT11
$SIZE 20
$R SY:LINK
SILUS.SV/Z/K:28.,LSD:SILUS/W,SY:SILUS=SILUS/X,RSTSLB
SILUSO.SV/Z/K:28.,LSD:SILUSO/W,SY:SILUSO=SILUS/X,RSTSLB/I/T
O.ODT
O.ODT

$R SY:SILUS
SILUS.SAV/S,LSD:SILUS=SILUS.SV/N
SILUS
SILUSO.SAV/S,LSD:SILUSO=SILUSO.SV/N
SILUSO
$R SY:PIP
SILUS.SV,SILUS.STB,SILUS.OBJ/DE/NOLOG
SILUSO.SV,SILUSO.STB/DE/NOLOG
$EOJ
