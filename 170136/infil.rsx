;  DEC/CMS REPLACEMENT HISTORY, Element INFIL.RSX
;  *3    18-AUG-1986 11:21:59 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:24:24 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:38:04 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element INFIL.RSX
	.NLIST							;Edit Level 02
	.ENABL	LC,GBL
	.LIST
	.TITLE	INFIL - CSI Processing and output file initialization
	.SBTTL	INFIL - CSI Processing and output file initialization
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
;  Abstract:	INFIL - CSI Processing and output file initialization
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;	001	Jrw	11-Nov-84	Update for CSI$4
;	002	Jrw	14-Nov-84	Add BISB of FL.AEX in F.FLG in FDB
;--


;	System Library "MCALLS"

	.MCALL	CSI$,	GCMLD$
	CSI$
	GCMLD$			;Get gcml symbols & release space

	.MCALL	CSI$1,CSI$4
	.MCALL	OFNB$R,OFNB$W,OFNB$U,OFNB$A,FDBF$R
	.MCALL	GTIM$S,GLUN$S


	.GLOBL	G.CMLD

	PURE	TSTBYT,D

SYSTR:	.ASCII	/SY0:/		;++022 DEVICE STRING FOR SY0:
SYSTRL=.-SYSTR			;++022 STRING LENGTH

LFSTR:	.ASCII	/MACTMP/	;++022 FILE STRING FOR TMP LST FILE
LFSTRL=.-LFSTR			;++022 STRING LENGTH


	PURE	DPURE,D
$SPOPT::.WORD	SPMSK		;Default to /SP

	PURE	PUREI,I		;++017


;+
; **-OPENCH-OPEN CHANNEL
;
; INPUTS:
;
;	R1=CHANNEL NUMBER.
;	R2=ADDRESS OF SWITCH LIST.
;
; OUTPUTS:
;
;	R0=PL IF OPERATION SUCESSFUL.
;	R0=MI IF ERRORS WERE ENCOUNTERED.
;-

OPENCH::MOV	#CSIBLK,R5	;++017 PUT ADDR OF CSI BLOCK IN R5
	CSI$4	R5,,R2		;GET SPECIFIED FILE
	BCS	JMP1		;IF CS ILLEGAL SWITCH ERROR
	MOV	#CSIM3,R3	;ASSUME WILD CARD SPECIFIED
	BITB	#CS.WLD,C.STAT(R5);ANY WILD CARDS SPECIFIED?
	BEQ	20$		;IF EQ NO
	JMP	OPSWT2		;ELSE, DECLARE ERROR
20$:
	MOV	R1,-(SP)	;++017 SAVE R1
	MOV	R5,-(SP)	;++017 SAVE R5
	JMP	PROCSI		;++017 PROCESS COMMAND LINE SWITCHES

;++017
;
; THE FOLLOWING LABEL IS REQUIRED TO RELOAD THIS SEGMENT AFTER
; PROCESSING OF COMMAND LINE SWITCHES IS COMPLETE.
;
;--017

	.ENABL	LSB		;++022

$OPSWT::MOV	(SP)+,R5	;++017 RESTORE R5
	MOV	(SP)+,R1	;++017 RESTORE R1
	TST	R0		;++017 TEST RESULTS
	BPL	50$		;IF PL NO ERROR DETECTED
JMP1:	JMP	OPSWT1		;ELSE, DECLARE SWITCH ERROR
50$:				;
	MOV	FDBTBL(R1),R0	;GET ADDRESS OF CHANNEL FDB
	CLR	F.FNB+N.FID(R0)	;++017 INSURE THAT OLD FILE ID'S ARE GONE.
	MOV	#CSIBLK+C.DSDS,R2  ;++014 PNT R2 TO CSI FILE DESCRIPTOR
	CMPB	#CS.INP,C.TYPR(R5);INPUT FILE?
	BNE	10$		;IF NE NO
	BITB	#CS.NMF!CS.DVF,C.STAT(R5);FILE OR DEVICE SPECIFIED?
	BNE	52$		;IF NE THEN OK
	CALLR	OPSWT2		;IF EQ THEN GIVE ERROR

52$:	MOV	(PC)+,R3	;++015 ASSUME A NORMAL SOURCE INPUT...
	.RAD50	/MAC/		;++015 ...FILE IS BEING OPENED
	BIT	#MLMSK,C.MKW1(R5) ;++015 WAS /ML SWITCH SPECIFIED?
	BEQ	5$		;++015 NO, SKIP
	MOV	(PC)+,R3	;++015 A USER MACRO LIBRARY FILE...
	.RAD50	/MLB/		;++015 ... IS BEING OPENED
5$:	MOV	R3,SRCNAM+N.FTYP  ;++015 SPECIFY DEFAULT FILE TYPE
	CALL	OPENRD		;++014 OPEN SOURCE FILE FOR READ
	MOV	#CSIM6,R3	;ASSUME OPEN FAILURE
	BCS	OPSWT2		;IF CS OPEN FAILURE
	BR	15$		;
10$:	BITB	#CS.NMF!CS.DVF,C.STAT(R5);FILE OR DEVICE SPECIFIED?
	BEQ	20$		;IF EQ NO
100$:				;++022
	CALL	FPARSE		;++014 PARSE THE FILE NAME SPEC
	MOV	#CSIM5,R3	;++015 ASSUME OPEN FAILURE
	BCS	OPSWT2		;++014 SKIP ON ERROR
	CMP	#"NL,F.DVNM(R0)	;OUTPUT TO NULL DEVICE?
	BEQ	20$		;IF EQ YES -- DON'T CREATE ANY
	CMP	R1,#LSTCHN	;++022 IS THIS LISTING FILE?
	BNE	$OPNWT		;++022 IF NE NO
	SUB	#6*2,SP		;ALLOCATE GLUN BUFFER ON STACK
	MOV	SP,R3		;STORE ADDRESS IN R3
	GLUN$S	F.LUN(R0),R3	;GET LUN INFORMATION

	BIT	#SPMSK,C.MKW1(R5) ;Was /[-]SP specified?
	BNE	103$		;Branch if so, don't override option
	BIS	$SPOPT,$SWTCH	;Else setup correct spooling default
103$:	BIT	#FD.OSP,G.LUCW(R3)  ;IS OUTPUT DEVICE SPOOLED?
	BEQ	105$		;IF EQ NO
	INCB	IOFTBL+1+LSTCHN	;ELSE, SET SPOOLED DEVICE FLAG
	BIS	#SPMSK,$SWTCH	;FAKE APPEARANCE OF /-SP		; CD058
105$:	ADD	#6*2,SP		;RESTORE STACK


	.IF NDF	XCREF		;++022

	BIT	#CRMSK,C.MKW1(R5)  ;++022 WAS /CR SWITCH SPECIFIED?
	BEQ	120$		;++022 IF NE NO
	BITB	#FD.SQD,F.RCTL(R0)  ;++022 YES, IS LST DEVICE SEQUENTIAL?
	BNE	110$		;IF NE YES
	BITB	#FD.DIR,F.RCTL(R0)  ;++022 OR, IS IT NON-DIRECTORY?
	BEQ	110$		;IF EQ YES
	TSTB	IOFTBL+1+LSTCHN	;IS IT A SPOOLED DEVICE?
	BEQ	120$		;IF EQ NO

;
; IF A CREF IS DESIRED AND THE LISTING FILE DOESN'T GO TO A
; DIRECTORY DEVICE, THE DEVICE SY0: WILL BE USED. THE ORIGINAL
; SPECIFICATION IS SAVED FOR EVENTUAL OUTPUT OF BOTH THE
; LISTING AND CREF ON THAT DEVICE BY CRF.
;

110$:	MOV	F.FNB+N.DVNM(R0),$LSTGT  ;++022 SAVE LST FILE DEVICE NAME
	MOVB	F.FNB+N.UNIT(R0),$LSTGT+2  ;++022 AND UNIT NUMBER
	MOV	#SYSTRL,C.DEVD(R5)  ;++022 SUBSTITUTE DESCRIPTOR FOR
	MOV	#SYSTR,C.DEVD+2(R5)  ;++022 DEVICE SY0:
	CLRB	IOFTBL+1+LSTCHN	;CLEAR SPOOLED DEVICE FLAG
	TST	F.FNB+N.FNAM(R0)  ;++022 WAS A FILE NAME SPECIFIED?
	BNE	100$		;++022 IF NE YES
	MOV	#LFSTRL,C.FILD(R5)  ;++022 SUBSTITUTE DESCRIPTOR FOR
	MOV	#LFSTR,C.FILD+2(R5)  ;++022 TEMP FILE NAME
	BR	100$		;++022 REPEAT THE PARSE

	.ENDC			;++022

;
; CALCULATE THE PROPER DEVICE BUFFER FOR LISTING FILE.
;

120$:	MOV	F.VBSZ(R0),-(SP)  ;++022 GET DEVICE BUFFER SIZE
	CMP	(SP),#LSTLEN	;++022 COMPARE W/ MAX LINE SIZE
	BGE	130$		;++022 IF GE DEVICE BUFFER IS BIG ENOUGH
	MOV	#LSTLEN,(SP)	;++022 ELSE, OVERRIDE W/ LINE LENGTH
130$:	MOV	(SP)+,$LSTVZ	;++022 STORE RESULT
	FDBF$R	R0,,$LSTVZ	;++022 USE BUFFER OVERRIDE IN NECESSARY


$OPNWT::			;++022 REF LABEL FOR OPEN OUTPUT FILE
	OFNB$W			;++014 OPEN OUTPUT FILE (VIA FNB)
	MOV	#CSIM5,R3	;ASSUME OPEN FAILURE
	BCS	OPSWT2		;IF CS OPEN FAILURE

15$:	MOVB	#IO.OPN,IOFTBL(R1)  ;++014 SET FILE OPEN STATUS
	CLR	F.DSPT(R0)	;++018 CLEAR DESCRIPTOR AND DEFAULT FILE ...
	CLR	F.DFNB(R0)	;++018 ... BLK ADDR FOR POSSIBLE DELET$
20$:	RETURN			;++017

	.DSABL	LSB		;++022

;++017
;
; **-OPSWT1-DECLARE SWITCH ERROR AND RESTART
; **-OPSWT2-DECLARE ERROR AND RESTART
;
;--017

OPSWT1::MOV	#CSIM4,R3	;++017 DECLARE SWITCH ERROR
OPSWT2::CALL	$OPTER		;++018 SEND MESSAGES AND CLEANUP FILES
	MOV	#-1,R0		;SET ERROR STATUS
	RETURN			;

;++014
;
; *-OPENRD-* OPEN SOURCE FILE FOR READ
;
;--014

OPENRD:				;++014
	CALL	FPARSE		;++014 PARSE THE FILE NAME
	BCS	1$		;++014 SKIP ON ERROR
	OFNB$R			;++014 OPEN SOURCE FOR READ (VIA FNB)
1$:				;++014
	RETURN			;++014


;++014
;
; *-FPARSE-* SETUP AND PARSE A FILE NAME BLOCK
;
;--014

FPARSE:	TST	F.FNB+N.FID(R0)	;++014 CHECK FOR A FILE ID
	BNE	1$		;++014 SKIP IF ONE IS ALREADY THERE
	MOV	R1,-(SP)	;++014 SAVE R1
	MOV	R0,R1		;++014 PUT ADDR OF THE FILE NAME ...
	ADD	#F.FNB,R1	;++014 ...BLOCK IN R1
	MOV	F.DFNB(R0),R3	;++014 PNT R3 TO DEFAULT FILENAME BLK
	BISB	#FL.AEX,F.FLG(R0) ;Flag that logical name expansion is not
				;needed again.
	CALL	.PARSE		;++014 PARSE FILENAME--BUILD FNB
	MOV	(SP)+,R1	;++014 RESTORE R1
1$:	RETURN			;++014

;+
; **-$GTFDB-*-GET NEXT AVAILABLE FDB
;
; INPUTS:
;	$NXFDB=POINTS TO NEXT AVAILABLE FDB IN LIST
;
; OUTPUTS:
;	R0=FDB ADDRESS
;	$NXFDB=UPDATED TO NEXT ENTRY IN LIST
;
;	THE FDB ALLOCATED IS ZEROED BY THIS ROUTINE.
;
;-

$GTFDB::MOV	@$NXFDB,R0	;++022 GET FDB ADDR IN R0
	MOV	R1,-(SP)	;SAVE R1					; CD053
	MOV	#<S.FDB/2>,R1	;PUT FDB SIZE (IN WORDS) IN R1			; CD053
10$:	CLR	(R0)+		;CLEAR OUT THE FDB				; CD053
	SOB	R1,10$		;						; CD053
	MOV	(SP)+,R1	;RESTORE R1					; CD053
	MOV	@$NXFDB,R0	;RETURN WITH FDB ADDRESS IN R0			; CD053
	ADD	#2,$NXFDB	;++022 UPDATE NEXT FDB POINTER
	RETURN			;++022


;++011
;
; *-FINP1-* FINISH PASS 1 (INITIALIZE FOR PASS 2)
;
;--011
FINP1::
	CALL	CLOSRC		;++022 CLOSE SOURCE FILE
	TSTB	OBJFIL		;++022 WAS OBJECT FILE CREATED?
	BEQ	100$		;++022 IF EQ NO
	OFNB$U	FDBTBL+BINCHN	;++022 RE-OPEN FILE
	BCS	500$		;++022 IF CS ERROR
	MOVB	#IO.OPN,IOFTBL+BINCHN  ;++022 SET FILE OPEN STATUS
100$:				;++022


	.IF DF	RSX11M		;++022

	.IF	NDF,R$RSTS	;IF NOT RSTS
	TSTB	LSTFIL		;++022 WAS LISTING FILE CREATED?
	BEQ	300$		;++022 IF EQ NO
	TSTB	IOFTBL+LSTCHN	;LISTING FILE KEPT OPEN?
	BNE	200$		;IF NE YES
	MOV	FDBTBL+LSTCHN,R0  ;++022 GET LST FILE FDB ADDR
	FDBF$R	R0,,$LSTVZ	;++022 USE BUFFER OVERRIDE IF NECESSARY
	OFNB$A	R0		;++022 RE-OPEN LISTING FILE
	BCS	500$		;++022 IF CS ERROR
	MOVB	#IO.OPN,IOFTBL+LSTCHN  ;++022 SET FILE OPEN STATUS
200$:				;++022

	.ENDC	;NDF,R$RSTS	;END IF NOT RSTS
	.ENDC			;++022


	.IF NDF	XCREF		;++022

	TSTB	CRFFIL		;++022 WAS CREF FILE CREATED?
	BEQ	300$		;++022 IF EQ NO
	OFNB$U	FDBTBL+CRFCHN	;++022 RE-OPEN CREF FILE
	BCS	500$		;++022 IF CS ERROR
	MOVB	#IO.OPN,IOFTBL+CRFCHN  ;++022 SET FILE OPEN STATUS
	MOV	#LINBUF,R1	;++022 POINT R1 TO A SCRATCH BUFFER
	MOV	R1,BUFTBL+CRFCHN  ;++022 MODIFY BUFFER POINTER TABLE
	MOV	#^RMAC,(R1)+	;++022 STORE NAME OF CREF INPUT SOURCE
	CLR	(R1)+		;++022
	MOV	#1,(R1)+	;++022 STORE IDENT CODE OF SOURCE, TOO
	GTIM$S	R1		;++022 GET TIME PARAMETERS


	.IF NDF	XLCTTM		;++022

	CLR	G.TIMI+2(R1)	;++022 ASSUME WIDE LISTING MODE DESIRED
	BIT	#LC.TTM,LCMASK	;++022 IS WIDE LISTING MODE DESIRED?
	BNE	250$		;++022 IF NE YES
	INC	G.TIMI+2(R1)	;++022 ELSE, SET NARROW CRF LISTING FLAG
250$:				;++022

	.IFF			;++022

	MOV	#1,G.TIMI+2(R1)	;++022 SET NARROW CRF LISTING FLAG

	.ENDC			;++022


	MOV	#18.,@CNTTBL+CRFCHN  ;++022 SET BUFFER LENGTH
	$WRITE	CRF		;++022 OUTPUT THE CREF HEADER RECORD
	MOV	#CRFBUF,BUFTBL+CRFCHN  ;++022 RESTORE CREF BUFFER ADDR
	MOV	#12.,@CNTTBL+CRFCHN  ;++022 AND RECORD SIZE

	.ENDC			;++022


300$:	CSI$1	#CSIBLK		;++022 RE-SCAN THE COMMAND LINE
	RETURN			;++022


500$:	MOV	#CSIM5,R3	;++022 GET OPEN FAILURE MESSAGE
	JMP	RESTRT		;++022 REPORT ERROR, CLOSE FILES,
				;++022 AND RESTART ASSEMBLER


	.END