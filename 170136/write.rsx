;  DEC/CMS REPLACEMENT HISTORY, Element WRITE.RSX
;  *3    18-AUG-1986 11:28:32 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:59:52 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:47:23 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element WRITE.RSX
	.NLIST							;Edit Level 00
	.ENABL	LC,GBL
	.LIST
	.TITLE	WRITE - MACRO/RSX Output record routines
	.SBTTL	WRITE - MACRO/RSX Output record routines
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
;  Abstract:	WRITE - MACRO/RSX Output record routines
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;--


;	System Library "MCALLS"

.IF NDF	RSX11M

	.MCALL	PUT$

.IFF

	.MCALL	PUT$S

.ENDC

.IF DF	YQCMO		;'YQCMO' MEANS QIO DIRECT TO 'CO'

	.MCALL	DIR$,QIO$,WTSE$S

.ENDC

	PURE	PUREI,I

$WRITE::			;WRITE LINE
	CALL	SAVREG		;++014 SAVE REGISTERS
	MOV	BUFTBL(R0),R1	;++005 PUT ADDR OF BUFFER IN R1
	MOV	@CNTTBL(R0),R2	;++005 PUT BYTE COUNT IN R2
	.IF DF	YQCMO		;++005
	CMP	R0,#CMOCHN	;++005 IS OUTPUT TO 'CO'
	BEQ	$QCMO		;++005 YES, BRANCH TO Q I/O DIRECTLY
	.ENDC			;++014 (YQCMO)
	MOV	R0,R3		;++005 ELSE, COPY CHANNEL NUMBER



.IF NDF	RSX11M


	PUT$	FDBTBL(R3),R1,R2 ;++005 OUTPUT LINE

.IFF

	PUT$S	FDBTBL(R3),R1,R2 ;++014 OUTPUT LINE


.ENDC


	BCS	1$
	RETURN
1$:	MOV	#OUTM1,R3	;OUTPUT I/O ERROR
	JMP	RESTRT		;++014 SEND ERROR MESSAGE AND RE-START


.IF DF	YQCMO


;+
;
; **-$QCMO-SEND A LINE TO DEVICE 'CO' DIRECTLY VIA QUEUE I/O
; INPUTS:
;	R1=ADDRESS OF MESSAGE
;	R2=BYTE COUNT OF MESSAGE
;
;-

$QCMO::	MOV	R1,CMODPB+Q.IOPL ;++005 PUT BUFFER ADDR IN DPB
	MOV	R2,CMODPB+Q.IOPL+2 ;++005 PUT BYTE COUNT IN DPB
	DIR$	#CMODPB		;++005 INVOKE QIO DIRECTIVE
	BCS	1$		;++005 SKIP IF IT FAILED
	WTSE$S	#1		;++005 WAIT FOR I/O DONE
1$:	RETURN			;++005

	.PSECT	MIXED,D,RW
 
CMODPB:	QIO$	IO.WVB,2,1,,,,<,,40,,,> ;++005 DPB TO SEND TO LUN 2 (CO)

.ENDC


	.END
