$JOB/CCL/ERROR:FATAL/NOLIMIT [170,137]
ASSIGN LST: NL
ASSIGN NL: LST
DEASSIGN NL
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NOWARN/LOG
$SIZE 28
$R SY:MACRO
LINK0,LST:LINK0/C=LNKCND,LINK0
LINK1,LST:LINK1/C=LNKCND,LINK1
LINK2,LST:LINK2/C=LNKCND,LINK2
LINK3,LST:LINK3/C=LNKCND,LINK3
LINK4,LST:LINK4/C=LNKCND,LINK4
LINK5,LST:LINK5/C=LNKCND,LINK5
LINK6,LST:LINK6/C=LNKCND,LINK6
LINK7,LST:LINK7/C=LNKCND,LINK7
LINK8,LST:LINK8/C=LNKCND,LINK8
LNKEM,LST:LNKEM/C=LNKCND,LNKEM
LNKLB1,LST:LNKLB1/C=LNKCND,LNKLB1
$R SY:LIBR
LNKLB1=LNKLB1
$EOJ