;  DEC/CMS REPLACEMENT HISTORY, Element READ.RSX
;  *3    18-AUG-1986 11:26:09 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:41:32 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:43:48 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element READ.RSX
	.NLIST							;Edit Level 00
	.ENABL	LC,GBL
	.LIST
	.TITLE	READ  - Read from source file
	.SBTTL	READ  - Read from source file
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
;  Abstract:	READ  - Read from source file
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;--


;	System Library "MCALLS"

.IF NDF	RSX11M

	.MCALL	GET$

.IFF

	.MCALL	GET$S

.ENDC

	PURE	PUREI,I

$READ::				;READ LINE
	CALL	SAVREG		;++014 SAVE REGISTERS
	MOV	R0,R4		;++010 COPY SOFTWARE CHANNEL NUMBER
	MOV	FDBTBL(R4),R0	;++010 GET ADDR OF FDB IN R0


.IF NDF	RSX11M


	GET$			;++010 GET NEXT RECORD FROM FILE

.IFF

	GET$S			;++014 GET NEXT RECORD


.ENDC


	MOV	F.NRBD(R0),@CNTTBL(R4)  ;++010 SET BYTE COUNT
	BCC	100$		;IF CC NO ERRORS DETECTED
	CMPB	#IE.RBG,F.ERR(R0)  ;WAS ERROR RECORD TOO BIG?
	BNE	5$		;IF NE NO
	BISB	#IO$RBG,IOFTBL(R4)  ;SET RECORD TOO BIG STATUS
	BR	100$		;RETURN WITH PART OF RECORD
5$:	CMPB	#IE.EOF,F.ERR(R0) ;EOF ?
	BNE	10$		;NO -BAD
	BISB	#IO$EOF,IOFTBL(R4)  ;++014 SET END-OF-FILE STATUS
100$:	RETURN			;
 
 
10$:	MOV	#INPM1,R3	;INPUT FILE I/O ERROR
READ1:	CALLR	RESTRT		;


	.END
