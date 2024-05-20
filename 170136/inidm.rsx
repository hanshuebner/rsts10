;  DEC/CMS REPLACEMENT HISTORY, Element INIDM.RSX
;  *3    18-AUG-1986 11:22:06 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:24:45 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:38:13 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element INIDM.RSX
	.NLIST							;Edit Level 01
	.ENABL	LC,GBL
	.LIST
	.TITLE	INIDM	- Initialize dynamic storage
	.SBTTL	INIDM	- Initialize dynamic storage
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
;    Author:	Joe Worrall
;
;   Created:	11-Jun-82
;
;  Abstract:	INIDM	This module resets dynamic storage to that of
;			the installed task size of MACRO-11/RSX.
;
;
;  Prefix files:	CONFIG.MAC
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;	001	Jrw	15-Mar-83	Conditionalize for IAS-11
;--


;	System Library "MCALLS"

	.MCALL	DIR$,	EXTK$S,	GPRT$,	GTSK$

; Local data

	PURE	DPURE,D

LIMIT:	.LIMIT			;Task D-SPACE address limits

; 'Get task paramters' DPB

TDPB:	GTSK$	TBUF

.IF	NDF,I$$IAS	;If not IAS

; 'Get partition paramters' DPB

PDPB:	GPRT$	,TBUF

.ENDC;	NDF,I$$IAS	;If not IAS

	.PSECT	MIXED,RW,D

; Parameter buffer

TBUF:	.BLKW	16.

	PURE	PUREI,I

.SBTTL	$INIDM - Initialize dynamic storage

;+
; $INIDM
; This subroutine is called to establish the initial state
; of the core pool referenced by the subroutines which allo-
; cate and deallocate core blocks. The pool consists of all
; memory extending from the end of the task image to the last
; physical location owned by the task.
;
;	R0	->	Address of free core pool listhead
;
;	CALL	$INIDM
;
;	R0	->	First address in task
;	R1	->	Address following task image
;	R2	 =	Size of current core pool
;
;	Any previous task memory extension is de-allocated. the
;	remaining free core area is allocated to the memory pool.
;-

$INIDM::MOV	LIMIT+2,R2	;R2 -> End of task D-SPACE
;;;	MOV	#$DSEND,R2	;R2 -> End of task D-SPACE
	ADD	#3,R2		;Round it up to next 4-BYTE boundary
	BIC	#3,R2		;
	MOV	R2,@R0		;Set address of pool
	EXTK$S			;Reset us to installed size.

.IF	NE,0
	BCC	10$		;Branch if directive went OK.
	MOV	@#$DSW,		;Save directive error code here
	.ERR	CODE=#ER$EXT,LEVEL=UABORT,PRINT=DIRECTIVE
;	<-U-Extend task directive failure>

10$:
.ENDC;	NE,0

.IF	NDF,I$$IAS	;If IAS

	DIR$	#PDPB		;Get partition parameters
	MOV	@#$DSW,R0	;R0 -> Starting virtual address of partition

.IFF;	NDF,I$$IAS	;If IAS

	CLR	R0		;IAS always begins at zero

.IFTF;	NDF,I$$IAS	;If IAS

	DIR$	#TDPB		;Get task parameters
	MOV	R2,-(SP)	;@SP -> Address of core pool start
	CLR	(R2)+		;Clear first word of dynamic store
	MOV	TBUF+G.TSTS,@R2	;Setup physical size of task

.IFF;	NDF,I$$IAS	;If IAS

	DEC	@R2		; and round down to 4 word boundary
	BIC	#3,@R2		; ...

.ENDC;	NDF,I$$IAS	;If IAS

	SUB	R0,@SP		;Compute apparent task size
	MOV	R0,R1		;Copy base address of task
	ADD	@R2,R1		;R1 = Next address after task
	SUB	(SP)+,@R2	;Set size of free pool
	MOV	@R2,R2		;R2 = Size of pool
	RETURN			;Return to the caller


	.END
