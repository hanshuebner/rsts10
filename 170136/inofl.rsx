;  DEC/CMS REPLACEMENT HISTORY, Element INOFL.RSX
;  *3    18-AUG-1986 11:22:59 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:27:19 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:38:57 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element INOFL.RSX
	.NLIST							;Edit Level 01
	.ENABL	LC,GBL
	.LIST
	.TITLE	INOFL - Initialize output file
	.SBTTL	INOFL - Initialize output file
	.SBTTL
	.SBTTL		.IDENT	/V05.05/
	.SBTTL
	.IDENT	/V05.05/
;****************************************************************************
;*									    *
;*                   COPYRIGHT (c)  1983, 1986                              *
;*          BY DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS.                *
;*                   ALL RIGHTS RESERVED.                                   *
;* 									    *
;*  THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND  COPIED  *
;*  ONLY  IN  ACCORDANCE  WITH  THE  TERMS  OF  SUCH  LICENSE AND WITH THE  *
;*  INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR  ANY  OTHER  *
;*  COPIES  THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE AVAILABLE TO ANY  *
;*  OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF  THE  SOFTWARE  IS  HEREBY  *
;*  TRANSFERRED.							    *
;* 									    *
;*  THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE  WITHOUT  NOTICE  *
;*  AND  SHOULD  NOT  BE  CONSTRUED  AS  A COMMITMENT BY DIGITAL EQUIPMENT  *
;*  CORPORATION.							    *
;* 									    *
;*  DIGITAL ASSUMES NO RESPONSIBILITY FOR THE USE OR  RELIABILITY  OF  ITS  *
;*  SOFTWARE ON EQUIPMENT THAT IS NOT SUPPLIED BY DIGITAL.		    *
;*									    *
;****************************************************************************


;++
;  Facility:	MACRO-11  The PDP-11 macro assembler for RT/RSX/VMS and RSTS/E
;
;    Author:	Too many people to list here
;
;   Created:	From the dust and dirt
;
;  Abstract:	INOFL - Initialize output file
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;	001	Jrw	12-Apr-83	Correct exit status handling.
;--


;	System Library "MCALLS"

	.MCALL	CSI$,	GCML$,	GCMLD$
	.MCALL	CLOSE$,CSI$1,CSI$2,EXIT$S,NMBLK$,CSI$SW,CSI$SV
	.MCALL	FDAT$R,FDOP$R

	CSI$
	GCMLD$			;Get gcml symbols & release space

;
; LOCAL DATA
;
; DEFAULT NAME BLOCKS
;

	PURE	DPURE,D

LSTNAM::NMBLK$	,LST,,SY	;LISTING FILE
OBJNAM::NMBLK$	,OBJ,,SY	;OBJECT FILE


;
; SWITCH CONTROL LIST
;

LSTSWT:				;++003 LISTING SWITCHES
	CSI$SW	LI,LIMSK,,SET,,LIVAL ;++003 /LI SWITCH
	CSI$SW	NL,NLMSK,,SET,,NLVAL ;++003 /NL SWITCH
	.IF NDF	XSPOOL		;++015
	CSI$SW	SP,SPMSK,$SWTCH,CLEAR,NEG, ;++003 /SP SWITCH
	.ENDC			;++015
	.IF NDF	XCREF		;++003
	CSI$SW	CR,CRMSK,,SET,,CRVAL ;++003 /CR SWITCH
	.ENDC			;++003
	.WORD	0		;++003 END OF LISTING SWITCH TABLE


OBJSWT:				;++003 OBJECT (& SOURCE) SWITCHES
	CSI$SW	EN,ENMSK,,SET,,ENVAL ;++003 /EN SWITCH
	CSI$SW	DS,DSMSK,,SET,,DSVAL ;++003 /DS SWITCH
	.WORD	0		;++003 END OF SOURCE & OBJECT SWT TABLE

;
; VALUE CONTROL LIST
; If any xxVALS are modified - be sure to change PROCSI.RSX too!
;

	.IF NDF	XCREF		;++003
CRVALS=6.			;++022 NUMBER OF /CR SWITCH VALUES
	.ENDC			;++003
DSVALS=11.			;++022
	.IRP	$X,<ABS,PIC,CDR,FPT,PNC,LC,CRF>  ;++022
	.IIF DF XED'$X,DSVALS=DSVALS-1 ;++003
	.ENDM			;++003
ENVALS=DSVALS			;++003 NUMBER OF "EN" VALUES
LIVALS=15.			;NUMBER "LI" VALUES
	.IIF DF XLCTTM,LIVALS=LIVALS-1 ;++003
NLVALS=LIVALS			;++003 NUMBER OF "NL" VALUES


;++017
;
; VALUE TABLES
;
;--017

LIVAL:				;++003 /LI SWITCH VALUE TABLE
TMP=0				;++003
	.REPT	LIVALS		;++003
	CSI$SV	ASCII,LIADDR+TMP,3 ;++003
TMP=TMP+4			;++003
	.ENDR			;++003
	.WORD	0		;++003 END OF /LI VALUE TABLE


NLVAL:				;++003 /NL SWITCH VALUE TABLE
TMP=0				;++003
	.REPT	NLVALS		;++003
	CSI$SV	ASCII,NLADDR+TMP,3 ;++003
TMP=TMP+4			;++003
	.ENDR			;++003
	.WORD	0		;++003 END OF /NL VALUE TABLE


	.IF NDF XCREF		;++003
CRVAL:				;++003 /CR SWITCH VALUE TABLE
TMP=0				;++003
	.REPT	CRVALS		;++003
	CSI$SV	ASCII,CRADDR+TMP,3 ;++003
TMP=TMP+4			;++003
	.ENDR			;++003
	.WORD	0		;++003 END OF /CR VALUE TABLE
	.ENDC			;++003


ENVAL:				;++003 /EN SWITCH VALUE TABLE
TMP=0				;++003
	.REPT	ENVALS		;++003
	CSI$SV	ASCII,ENADDR+TMP,3 ;003
TMP=TMP+4			;++003
	.ENDR			;++003
	.WORD	0		;++003 END OF /EN VALUE TABLE


DSVAL:				;++003 /DS SWITCH VALUE TABLE
TMP=0				;++003
	.REPT	DSVALS		;++003
	CSI$SV	ASCII,DSADDR+TMP,3 ;++003
TMP=TMP+4			;++003
	.ENDR			;++003
	.WORD	0		;++003 END OF /DS VALUE TABLE
	.PAGE
	PURE	PUREI,I		;++017


;+
; **-$INOFL-INITIALIZE OUTPUT FILES
;
; INPUTS:
;
;	R0=GET COMMAND LINE RETURN STATUS (CARRY BIT).
;		IF R0 LT 0, THEN ERROR RETURN.
;		IF R0 GE 0, THEN NORMAL RETURN.
;	R4=ADDRESS OF GET COMMAND LINE BLOCK.
;
; OUTPUTS:
;
;	R0=PL IF OUTPUT FILES INITIALIZED.
;	R0=MI IF ERRORS WERE ENCOUNTERED.
;-

$INOFL::MOV	#CMLBLK,R4	;GET ADDRESS OF GET COMMAND LINE BLOCK
	MOV	R4,R0		;++015 COPY ADDR OF GCML BLOCK INTO R0
	GCML$	,#$MACPR,#6	;GET THE COMMAND LINE
	BCC	40$		;Branch if we have one.
	MOVB	G.ERR(R4),R0	;GET ERROR STATUS
	CMPB	#GE.EOF,R0	;END OF FILE?
	BEQ	30$		;IF EQ YES
	MOV	#CMLM3,R1	;ASSUME INDIRECT OPEN FAILURE
	CMPB	#GE.OPR,R0	;OPEN FAILURE?
	BEQ	50$		;IF EQ YES
	MOV	#CMLM4,R1	;ASSUME BAD INDIRECT FILE
	CMPB	#GE.BIF,R0	;BAD INDIRECT FILE?
	BEQ	50$		;IF EQ YES
	MOV	#CMLM5,R1	;ASSUME INDIRECT DEPTH EXCEEDED
	CMPB	#GE.MDE,R0	;MAX DEPTH EXCEEDED?
	BEQ	50$		;IF EQ YES
	CALL	$CMDER		;COMMAND I/O ERROR
30$:				;++018


	.IF NDF	YQCMO		;++018

	CLOSE$	#CMOFDB		;++005 CLOSE COMMAND OUTPUT FILE

	.ENDC			;++013 'YQCMO'

	MOV	$EXSTS,R0	;R0 = Exit status code
	JMP	$EXST		;Exit with this status.

;;	MOV	$EXSTS,R0	;GET STATUS
;;	BGE	35$		;IF GE STATUS HAS BEEN SET
;;	MOV	#EX$SUC,R0	;ELSE, SET SUCCESS STATUS
;;35$:	JMP	$EXST		;EXIT (POSSIBLY WITH STATUS)

;
; COMMAND LINE SUCESSFULLY READ
;

40$:
;;;	CALL	SAVREG		;++014 SAVE REGISTERS
	MOV	#$FDBLS,$NXFDB	;++022 INIT AVAILABLE FDB LIST POINTER
	CLR	$XFDB		;++022 CLEAR TABLE SLOT FOR EXTRA FDB
	MOV	#CSIBLK,R0	;++017 PUT ADDR OF CSIBLK IN R0
	MOV	G.CMLD(R4),C.CMLD(R0)  ;++017 SET LINE LENGTH
	MOV	G.CMLD+2(R4),C.CMLD+2(R0)  ;++017 SET LINE ADDRESS
	CSI$1	R0		;++017 ANALYZE COMMAND LINE SYNTAX
	BCC	70$		;++017 BRANCH IF SYNTAX IS O.K.
45$:	MOV	#CSIM2,R1	;++017 COMMAND SYNTAX ERROR
50$:	CALL	OUTERM		;++013 SEND ERR MSG AND CMD LINE TO CMO
60$:
;;;	MOV	#-1,R0		;SET ERROR STATUS
	CALLR	CONT
	RETURN			;

;
; SYNTAX CORRECT OPEN OUTPUT FILES
;

70$:	MOV	C.CMLD(R0),G.CMLD(R4)  ;++017 RESET COMMAND LINE LENGTH
	BEQ	60$		;++013 DON'T PROCESS NULL LINE
	BITB	#CS.EQU,C.STAT(R0)  ;++017 CHECK FOR EQUAL SIGN SEEN
	BEQ	45$		;++017 IF NOT, DECLARE SYNTAX ERROR
	MOVB	#CS.OUT,C.TYPR(R0)  ;++017 SPECIFY OUTPUT REQUEST
	MOV	#BINCHN,R1	;SET CHANNEL NUMBER
	MOV	#OBJSWT,R2	;SET ADDRESS OF SWITCH LIST
	CALL	$GTFDB		;++022 GET NEXT AVAILABLE FDB
	MOV	R0,FDBTBL+BINCHN  ;++017 AND STORE IT IN FDB TABLE
	FDAT$R	R0,#R.VAR,,#OBJLEN,,#-1  ;++017 SET FILE DATA ATTRIBUTES
	FDOP$R	R0,#3,,#OBJNAM	;++017 SET FILE OPEN PARAMETERS
	CALL	OPENCH		;OPEN OBJECT CHANNEL
	BMI	60$		;++014 BRANCH TO LEAVE ON ERROR
	TSTB	IOFTBL+BINCHN	;++014 OBJECT FILE CREATED?
	BEQ	72$		;++017 BRANCH IF NOT
	INCB	OBJFIL		;++014 INDICATE OBJECT FILE CREATED
	CALL	90$		;++014 CLOSE FILE FOR LATER RE-OPEN
	BR	75$		;++017
72$:	CLR	FDBTBL+BINCHN	;++017 INDICATE NO OBJECT FILE
	SUB	#2,$NXFDB	;++022 RESTORE NEXT FDB POINTER
75$:	CALL	$GTFDB		;++022 GET NEXT AVAILABLE FDB
	MOV	R0,FDBTBL+LSTCHN  ;++022 STORE ADDR IN FDB TABLE
	FDAT$R	R0,#R.VAR,#FD.CR,#LSTLEN,,#$LSEXT  ;++018 SET DATA ATTRIBS
	FDOP$R	R0,#4,,#LSTNAM	;++017 SET FILE OPEN PARAMETERS
	MOV	#LSTCHN,R1	;SET CHANNEL NUMBER
	CLRB	IOFTBL+1(R1)	;CLEAR SPOOLED DEVICE FLAG
	MOV	#LSTSWT,R2	;SET ADDRESS OF SWITCH LIST
	CALL	OPENCH		;OPEN CHANNEL
	BMI	60$		;++014 BRANCH TO LEAVE ON ERROR
	TSTB	IOFTBL+LSTCHN	;++014 LISTING FILE CREATED?
	BNE	78$		;++017 BRANCH IF TRUE
	CLR	FDBTBL+LSTCHN	;++017 INDICATE NO LISTING FILE
	SUB	#2,$NXFDB	;++022 RESTORE NEXT FDB POINTER
	BR	95$		;++017 BRANCH TO RETURN
78$:				;++017
	INCB	LSTFIL		;++014 INDICATE LISTING FILE CREATED


	.IF DF	RSX11M		;++022

.IF	NDF,R$RSTS	;IF NOT RSTS/E

	TSTB	IOFTBL+1+LSTCHN	;OUTPUTTING TO A SPOOLED DEVICE?
	BNE	79$		;IF NE YES
	CALL	90$		;++022 CLOSE LISTING FILE
79$:				;REF LABEL

.ENDC;	NDF,R$RSTS	;IF NOT RSTS/E

	.ENDC			;++017


	.IF NDF	XCREF		;++022

80$:	CALL	$OPCRF		;++022 OPEN CRF FILE IF NEEDED
	BMI	60$		;++022 IF MI ERROR
	TSTB	IOFTBL+CRFCHN	;++022 WAS A FILE CREATED?
	BEQ	95$		;++022 IF EQ NO
	INCB	CRFFIL		;++022 FLAG EXISTENCE OF CRF FILE

	.IFF			;++022

	BR	95$		;++022 BRANCH TO LEAVE

	.ENDC			;++022


90$:	MOV	FDBTBL(R1),R0	;++014 GET FDB ADDRESS IN R0
	MOV	R0,R3		;++014 POINT R3 TO THE ...
	ADD	#F.FNB+N.FID,R3	;++013 ... FILE ID
	MOV	(R3)+,-(SP)	;++014 SAVE ...
	MOV	(R3)+,-(SP)	;++014 ... THE ...
	MOV	(R3)+,-(SP)	;++014 ... FILE ID
	CLOSE$			;++014 CLOSE THE OUTPUT FILE
	CLRB	IOFTBL(R1)	;++014 SET FILE CLOSED STATUS
	MOV	(SP)+,-(R3)	;++014 RESTORE ...
	MOV	(SP)+,-(R3)	;++014 ... THE ...
	MOV	(SP)+,-(R3)	;++014 ... FILE ID
95$:	CLR	R0		;++017 SET FILE SUCCESS STATUS
	RETURN			;Return to MAIN control loop in MROOT


	.END
