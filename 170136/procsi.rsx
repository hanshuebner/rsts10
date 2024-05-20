;  DEC/CMS REPLACEMENT HISTORY, Element PROCSI.RSX
;  *3    18-AUG-1986 11:25:26 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:38:36 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:42:43 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element PROCSI.RSX
	.NLIST							;Edit Level 00
	.ENABL	LC,GBL
	.LIST
	.TITLE	PROCSI - Handle CSI scan of command line
	.SBTTL	PROCSI - Handle CSI scan of command line
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
;  Abstract:	PROCSI - Handle CSI scan of command line
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;--


;	System Library "MCALLS"

	.MCALL	CSI$
	CSI$

	PURE	PUREI,I

PROCSI::			;++003 PROCESS SWTICHES
	MOV	C.MKW1(R5),R2	;GET SWITCHES SPECD
	MOV	#SWCLT,R1	;SWITCH CONTROL TABLE ADDR
PRO:	BIT	(R1)+,R2	;SWITCH SPECD?
	BEQ	NXTCSI		;BRANCH IF NOT
	MOV	(R1)+,R0	;GET ASCII SWITCH NAME
	MOV	(R1)+,R3	;GET SIZE OF VALUE TABLE IN BYTES
	MOV	(R1)+,R4	;ADDR OF VALUE TABLE
	MOV	#LINBUF,R5	;ASM AREA FOR CHAR STRING
	MOVB	#' ,(R5)+	;
NXTCHR:	MOVB	(R4),(R5)+	;MOVE CHAR FROM TABLE TO LINBUF
	BNE	CHROK		;BRANCK IF NOT NULL
	DEC	R5
	MOVB	#' ,(R5)+	;
CHROK:	CLRB	(R4)+		;REINIT VALTAB AS WE GO


	.IF NDF	PDPV45		;++014


	DEC	R3		;++014 DECREMENT CHAR COUNT
	BNE	NXTCHR		;++014 SKIP BACK IF COUNT NOT ZERO

	.IFF			;++014

	SOB	R3,NXTCHR	;REPEAT


	.ENDC			;++014 (PDPV45)


	CLRB	(R5)
	CALL	PROSW		;NOW ASM PROCESSES SWITCHES
	BNE	NXCSI		;BRANCH IF NO ERROR
	SEC			;SET CARRY
	BR	NXEXT		;
NXTCSI:	ADD	#6,R1
NXCSI:	TST	(R1)		;MORE SWITCHES IN CNTRL TABLE
	BNE	PRO		;YES, BRANCH
NXEXT:	ROR	R0		;SAVE CARRY INDICATOR
	JMP	$OPSWT		;RELOAD SEGMENT IF NECESSARY

;
; SWITCH PROCESSING STORAGE
;
; DEFINE SWITCH MASKS
;

LIMSK==1			;"LI" MASK
NLMSK==2			;"NL" MASK
PAMSK==4			;"PA" MASK
	.IF NDF XCREF		;++003
CRMSK==10			;"CR" MASK
	.IFTF			;++003
ENMSK==20			;"EN" MASK
DSMSK==40			;"DS" MASK
SPMSK==100			;"SP" MASK
	.IF NDF	XSML		;++007
MLMSK==200			;++007 "ML" MASK
	.ENDC			;++007

;
; SPECIFY NUMBER OF SWITCH VALUES
; If any xxVALS change - be sure to modify INOFL.RSX too!
;

LIVALS=15.
	.IIF DF XLCTTM,LIVALS=LIVALS-1 ;++003
NLVALS=LIVALS			;++003
	.IFT			;++003
CRVALS=6.			;++022 NUMBER OF /CR VALUES
	.IFTF			;++003
ENVALS=11.			;++022
	.IRP	$X,<ABS,PIC,CDR,FPT,PNC,LC,CRF>  ;++022
	.IIF DF XED'$X,ENVALS=ENVALS-1 ;++003
	.ENDM			;++003
DSVALS=ENVALS			;++003

;
; SWITCH CONTROL PROCESSING TABLE
;

	PURE	DPURE,D		;++017
SWCLT:	.WORD	LIMSK,"LI,LIVALS*4,LIADDR
	.WORD	NLMSK,"NL,NLVALS*4,NLADDR
	.IFT			;++003
	.WORD	CRMSK,"CR,CRVALS*4,CRADDR
	.IFTF			;++003
	.WORD	ENMSK,"EN,ENVALS*4,ENADDR
	.WORD	DSMSK,"DS,DSVALS*4,DSADDR
	.WORD	0

;
; SWITCH VALUE STORAGE
;

	.PSECT	IMPURE,D,RW	;++017
LIADDR::.BLKW	LIVALS*2	;
NLADDR::.BLKW	NLVALS*2	;
	.IFT			;++003
CRADDR::.BLKW	CRVALS*2	;
	.ENDC			;++003
ENADDR::.BLKW	ENVALS*2	;
DSADDR::.BLKW	DSVALS*2	;


	.END