!
!	Modified 30-Jan-86 by Vogel - change to V9 batch format
!	1-Feb-90  Sconce  SYRC included in assembly of VE
!	14-Feb-90 Sconce  M18, DI added;  debug LA variant added
!		
SET NOON
ASSIGN SY: SY
ASSIGN SY: LST
ASSIGN SY: DK
SWITCH RT11
R SY:PIP
*.BAK/DE:NOWARN/LOG
$
SIZE 20
R SY:MACRO
RC,LST:RC/C=COMMON,SYRC,SYSA,SYPP,RC
$
SIZE 20
R SY:MACRO
EC,LST:EC/C=COMMON,SYRC,EC
$
SIZE 20
R SY:MACRO
SC,LST:SC/C=COMMON,SYRC,SC
$
SIZE 20
R SY:MACRO
SU,LST:SU/C=COMMON,SYRC,SYPP,SU
$
SIZE 20
R SY:MACRO
LA,LST:LA/C=COMMON,SYRC,SYPP,LA
$
SIZE 20
R SY:MACRO
LAD,LST:LAD/C=COMMON,SYRC,SYPP,SYDBG,LA
$
SIZE 20
R SY:MACRO
TR,LST:TR/C=COMMON,SYRC,SYSA,SYPP,TR
$
SIZE 20
R SY:MACRO
ED,LST:ED/C=COMMON,SYRC,SYPP,ED
$
SIZE 20
R SY:MACRO
RX,LST:RX/C=COMMON,RX
$
SIZE 20
R SY:MACRO
VE,LST:VE/C=COMMON,SYRC,VE
$
SIZE 20
R SY:MACRO
IO,LST:IO/C=COMMON,SYRC,SYSA,SYPP,IO
$
SIZE 20
R SY:MACRO
PU,LST:PU/C=COMMON,SYRC,SYSA,SYPP,PU
$
SIZE 20
R SY:MACRO
MX,LST:MX/C=COMMON,SYRC,SYSA,SYPP,MX
$
SIZE 20
R SY:MACRO
SN,LST:SN/C=COMMON,SYRC,SN
$
SIZE 20
R SY:MACRO
DBG,LST:DBG/C=COMMON,SYRC,DBG
$
SIZE 20
R SY:MACRO
M18,LST:M18/C=COMMON,SYRC,M18
$
SIZE 20
R SY:MACRO
DI,LST:DI/C=COMMON,SYRC,DI
$
