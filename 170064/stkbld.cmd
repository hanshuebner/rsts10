;
; TKB .CMD FILE FOR TKB
; ON AN RSX-11M-PLUS SYSTEM
; USING SYSLIB
; CREATED BY SYSGEN3.CMD VERSION 03.04
;
; REG	01-AUG-87	UPDATE FOR RSX CHANGES
;
TKB:STK/-FP/CP/MM,TKB:STK/-SP/CR,TKB:STK=TKB:TKBBLD/MP
TASK=...STK
IDENT=M43.00		; Release V4.0
STACK=170
UNITS=8
ASG=TI:1
ASG=TI:2
ASG=OV:8
PAR=GEN:0:70000
PRI=50
EXTSCT=P3B$D:1000	; ALLOCATE PHASE 3 FCS BUFFER
GBLDEF=D$FAPR:5		; DEFAULT INITIAL APR FOR PRIVILEGED TASKS
GBLDEF=M$XAPR:1		; MAXIMUM INITIAL APR FOR PRIVILEGED TASKS (BIASED)
GBLDEF=N$MPAG:24	; SET PAGE LIST THRESHOLD
GBLDEF=T$KINC:3100	; TASK EXTENSION INCREMENT
GBLDEF=T$KMAX:177700	; MAXIMUM TASK SIZE
GBLDEF=P$LNTH:74	; SET NUMBER OF LINES PER PAGE
GBLDEF=W$KEXT:31	; SET DEFAULT WORKFILE EXTEND SIZE
GBLDEF=W$KLUN:10	; SET WORKFILE LUN
GBLDEF=..RWLG:1		; DELETE READ/WRITE LONG REFERENCE
GBLPAT=P3LBSR:$MXLBF:1000 ; ESTABLISH SIZE OF LIBRARY SEARCH BUFFER
;GBLPAT=TASKB:$DFTSK:140002 ; MAKE /FP THE DEFAULT
GBLPAT=TASKB:$DFTSK:040002 ; MAKE /CP, /FP THE DEFAULT
GBLPAT=TASKB:$DFTSO:000010 ; MAKE -SG THE DEFAULT
GBLPAT=TASKB:$DFTSO+2:000004 ; MAKE SB THE DEFAULT (STK VERSION OF TKB)
;
; MODIFING TASK FILE SWITCH DEFAULTS
;
;	GBLPAT=TASKB:"SWITCH":"XXXXXX"
;
;	WHERE "SWITCH" REPRESENTS THE SWITCH WORD AND "XXXXXX"
;	REPRESENTS THE REVISED BIT SETTING. NOTE THAT
;	THE STATE OF BIT POSITIONS NOT LISTED IN THE TABLE 
;	MUST NOT BE ALTERED.
;
;
; SWITCH WORD: $DFSWT
; INITIAL CONTENTS: 0
;
;	BIT	CONDITION IF SET TO 1
;
;	15	AB
;	11	SQ
;	 4	FU
;	 3	-RO
;
; SWITCH WORD: $DFTSK
; INITIAL CONTENTS: 100002
;
;	BIT	CONDITION IF SET
;
;	15	-CP,-AL
;	14	FP
;	13	EA
;	12	-HD
;	11	CM
;	10	DA
;	9	PI
;	8	PR
;	7	TR
;	6	PM
;	5	SL	
;	4	-SE
;	3	MU
;	2	AC
;	1	-AL
;	0	XH
;
; *NOTE WELL*  
; TO TURN ON /XH  AS THE DEFAULT MODIFY $DFTSK
;	TO TURN ON /-XH AS THE DEFAULT MODIFY $DFTSO
;
; SWITCH WORD: $DFTSO
; INITIAL CONTENTS: 000010
;
;	BIT	CONDITION IF SET TO 1
;
;	 8	-XH
;	 3	-SG
;
; SWITCH WORD: $DFTSO+2
; INITIAL CONTENTS: 000000
;
;	BIT	CONDITION IF SET TO 1
;
;	 2	SB
;
; MODIFYING MAP FILE SWITCH DEFAULTS
;
; SWITCH WORD: $DFLBS
; INITIAL CONTENTS: 120000
;
; 	BIT	CONDITION IF SET
;
;	15	-MA
;
; SWITCH WORD:	$DFMAP
; INITIAL CONTENTS: 2040
;
;	BIT	CONDITION IF SET
;
;	10	SH
;	 8	-SP
;	 6	CR
;	 5	WI
;
/
