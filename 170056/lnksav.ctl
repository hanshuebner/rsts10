$JOB/CCL/NOLIMIT/PRI:-16 [170,56]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: SY
$ASSIGN D: DK
$ASSIGN <48>
$SWITCH RT11
$SIZE 28
$R SY:LINK
SAVRES.SV,LSD:SAVRES,SY:SAVRES=SROOT/X/T/U:#1000/B:1500/C
INIFIP/C
INIFIM/C
[170,46]RSTSLB/C
ONLSAV/C
GGBOOT,BLDBOT/O:1/C
DIA/O:1/C
SAV/O:1/C
RES/O:1/C
IMA/O:1
INITGO
PATCH
$EOD
$R SY:SILUS
SAVRES.SAV/S,LSD:SAVRES=SAVRES.SV/N
SAVRES
$EOD
$R SY:PIP
SAVRES.SV,SAVRES.STB/DE/NOLOG
$EOJ
