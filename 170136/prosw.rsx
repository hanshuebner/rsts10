;  DEC/CMS REPLACEMENT HISTORY, Element PROSW.RSX
;  *3    18-AUG-1986 11:25:41 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:39:36 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:43:10 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element PROSW.RSX
	.NLIST							;Edit Level 00
	.ENABL	LC,GBL
	.LIST
	.TITLE	PROSW - MACRO-11/RSX switch parse
	.SBTTL	PROSW - MACRO-11/RSX switch parse
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
;  Abstract:	PROSW - MACRO-11/RSX switch parse
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;--



;
; LOCAL MACROS
;
;THE MACRO "GENSWT" IS USED TO SPECIFY  A COMMAND
;STRING SWITCH (1ST ARGUMENT) AND THE ADDRESS OF
;THE ROUTINE TO BE CALLED WHEN ENCOUNTERED (2ND ARG).
;
	.MACRO	GENSWT	MNE,ADDR,?LABEL
LABEL:	.ASCIZ	/MNE/
.	=	LABEL+2		;TRIM TO ONE WORD
	.WORD	ADDR
	.ENDM

	PURE	SWTSEC,D,GBL	;++017
SWTBAS::			;REF LABEL
	.IF NDF	XCREF
	GENSWT	CR,CRFSET
	.ENDC

	GENSWT	DS,DSABL
	GENSWT	EN,ENABL
	GENSWT	LI,LIST
	GENSWT	NL,NLIST
SWTTOP::			;REF LABEL

	PURE	PUREI,I

;+
; **PROSW-PROCESS SWITCH
;
; INPUTS:
;
;	R0=TWO CHARACTER ASCII SWITCH.
;-

PROSW::	SAVREG			;SAVE REGISTERS
	SETXPR			;SET EXPRESSION-TYPE REGISTERS
	MOV	R0,(R1)+	;SET "SYMBOL"
	CALL	XCTLIN		;ZERO LINE-ORIENTED FLAGS
	SCANW	SWTROL		;SCAN FOR SWITCH
	BEQ	1$		;  NOT FOUND, EXIT ZERO
	CLR	(R3)		;CLEAR "MODE"
	MOV	(R1),(R4)	;ADDRESS TO "VALUE"
	MOV	#LINBUF,CHRPNT	;POINT TO START OF LINE
	SETNB			;SET R5
	INC	EXMFLG		;FLAG EXEC MODE
	CALL	PROPC		;PROCESS AS OP-CODE
	CLR	R0		;ASSUME ERROR
	BIS	ERRBTS,R5	;ERROR OR NOT TERMINATOR?
	BNE	1$		;  YES, ERROR
	COM	R0		;OK, SET .NE. ZERO
1$:	RETURN


	.END
