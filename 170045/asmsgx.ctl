$JOB/CCL/NOLIMIT/PRIORITY:-16 [170,45]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D: DK
$ASSIGN D: SY
$ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NOWARN/LOG
$SIZE 20
$R SY:MACRO
SYSGEN,SYSGEN/C=SGNRT
$SIZE 20
$R SY:MACRO
SGNSRT,SGNSRT/C=SGNSRT
$SIZE 20
$R SY:MACRO
SGNTTY,SGNTTY/C=SGNTTY
$SIZE 20
$R SY:MACRO
SGNDSK,SGNDSK/C=SGNDSK
$SIZE 20
$R SY:MACRO
SGNPRF,SGNPRF/C=SGNPRF
$SIZE 20
$R SY:MACRO
SGNOPT,SGNOPT/C=SGNOPT
$SIZE 20
$R SY:MACRO
SGNBAT,SGNBAT/C=SGNBAT
$SIZE 20
$R SY:MACRO
SGNANS,SGNANS/C=SGNANS
$SIZE 20
$R SY:MACRO
SGNDIR,SGNDIR/C=SGNDIR
$EOJ

