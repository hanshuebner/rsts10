$JOB/CCL/NOLIMIT/PRIORITY:-16 [1,236]
$ASSIGN LSD: NL
$ASSIGN NL: LSD
$DEASSIGN NL
$ASSIGN D:[170,50] A
$ASSIGN D: DK
$ASSIGN [170,42]
$RUN $TKB
DSKINT/DA/SQ,DSKINT/-SP/MA,DSKINT=D:[1,170]ERR.STB,A:DSKCUI,A:DSKINT,A:ONLDSI
D:[170,46]RSTSLB/LB,D:[170,19]CUIOLB/LB
//
$RUN $MAKSIL
DSKINT
DSKINT
YES
DSKINT.STB
DSKINT.SIL
$RUN $PIP
DSKINT.TSK,DSKINT.STB/DE
DSKINT.TSK<124>/RE=DSKINT.SIL
$EOJ
