$JOB/CCL/NOLIMIT [170,46]
ASSIGN D: SY
ASSIGN D: DK
ASSIGN <40>
$SWITCH RT11
$R SY:PIP
*.BAK/DE:NOWARN/NOLOG
$R SY:MACRO
NUMS,SY:NUMS/C=#COMMON,NUMS
VAL,SY:VAL/C=#COMMON,VAL
VALDBL,SY:VALDBL/C=#COMMON,VALDBL
DECNUM,SY:DECNUM/C=#COMMON,DECNUM
RADS,SY:RADS/C=#COMMON,RADS
CVTSS,SY:CVTSS/C=#COMMON,CVTSS
INSTR,SY:INSTR/C=#COMMON,INSTR
EQUSS,SY:EQUSS/C=#COMMON,EQUSS
ASCR50,SY:ASCR50/C=#COMMON,ASCR50
GETUNQ,SY:GETUNQ/C=#COMMON,GETUNQ
INPLIN,SY:INPLIN/C=#COMMON,INPLIN
REGSCR,SY:REGSCR/C=#COMMON,REGSCR
REGSAV,SY:REGSAV/C=#COMMON,REGSAV
INPUTL,SY:INPUTL/C=#COMMON,INPUTL
INPUTX,SY:INPUTX/C=#COMMON,INPUTX
TTYIN,SY:TTYIN/C=#COMMON,#RTCOM,TTYIN
ASCNUM,SY:ASCNUM/C=#COMMON,ASCNUM
DIVIDE,SY:DIVIDE/C=#COMMON,DIVIDE
DIVDBL,SY:DIVDBL/C=#COMMON,DIVDBL
PKGLOC,SY:PKGLOC/C=#COMMON,#RTCOM,PKGLOC
FLAGS,SY:FLAGS/C=#COMMON,FLAGS
$R SY:LIBR
RSTSLB/A/X,SY:RSTSLB=NUMS,VAL,VALDBL,DECNUM,RADS,CVTSS/C
INSTR,EQUSS,ASCR50,GETUNQ,INPLIN,REGSCR/C
REGSAV,INPUTL,INPUTX,TTYIN,ASCNUM,DIVIDE/C
DIVDBL,PKGLOC,#RTSODT,FLAGS,#ERR.STB
,TT:=RSTSLB
$EOJ
