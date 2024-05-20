;  DEC/CMS REPLACEMENT HISTORY, Element VMRES.RSX
;  *3    18-AUG-1986 11:28:08 WORRALL "Complete HEX listing support for version 5.5 fieldtest"
;  *2    14-APR-1986 23:58:56 SYSTEM "Update 5.4 of MACRO-11"
;  *1    28-MAR-1986 02:46:47 SYSTEM "Load MACRO-11 sources from V5.3"
;  DEC/CMS REPLACEMENT HISTORY, Element VMRES.RSX
	.NLIST							;Edit Level 00
	.ENABL	LC,GBL
	.LIST
	.TITLE	VMRES - Custom virtual memory routines for MACRO-11
	.SBTTL	VMRES - Custom virtual memory routines for MACRO-11
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
;  Abstract:	VMRES - Custom virtual memory routines for MACRO-11
;
;     Externals	 Description
;     ---------	 -----------
;
;      Edit	Who	Date		Description of modification
;      ----	---	----		---------------------------
;--


	W$$KST	=	0	;Statistics please.

;+
; $ALBLK - ALLOCATE BLOCK
;
; THIS ROUTINE IS CALLED TO ALLOCATE A BLOCK OF STORAGE FROM THE FREE
; STORAGE POOL. IF THE BLOCK CANNOT BE ALLOCATED, THEN A FATAL ERROR
; MESSAGE IS ISSUED. ELSE THE BLOCK IS ALLOCATED AND ZEROED AND THE
; ADDRESS IS RETURNED TO THE CALLER.
;
; INPUTS:
;
;	R1=SIZE OF BLOCK TO ALLOCATE IN BYTES.
;
; OUTPUTS:
;
;	R0=ADDRESS OF ALLOCATED BLOCK.
;-

	PURE	PUREI,I

$ALBLK::MOV	R1,-(SP)	;SAVE SIZE OF BLOCK        
	CALL	$GTCOR		;REQUEST A BLOCK OF STORAGE
	BCC	10$		;IF CC GOT IT
	MOV	#$FRHD,R2	;GET ADDRESS OF FREE POOL HEADER
	MOV	(PC)+,R1	;GET ERROR/SEVERITY
	.BYTE	E$R4,S$V2	;
	CALL	$ERMSG		;ISSUE FATAL ERROR (NO RETURN)
10$:	MOV	(SP)+,R1	;RETRIEVE SIZE OF BLOCK
	ADD	#3,R1		;ROUND TO NEXT 2 WORD BOUNDRY
	BIC	#3,R1		;CLEAR EXECSS BITS
	ASR	R1		;CONVERT TO WORDS
	MOV	R0,R2		;COPY ADDRESS OF BLOCK
20$:	CLR	(R2)+		;CLEAR BLOCK
	SOB	R1,20$		;REPEAT
	RETURN			;


;+
; $ALPAG
; ALLOCATE VIRTUAL MEMORY ON A PAGE BOUNDRY
; $HGPAG = NEXT AVAILABLE VIRTUAL PAGE ADDRESS
; CALL $ALPAG
; R1 = VIRTUAL PAGE ADDRESS
; R0 = REAL PAGE ADDRESS
;-

$ALPAG::SAVRG			;PRESERVE R3-R5
	MOV	$HGPAG,R1	;GET FREE PAGE ADDR
	BEQ	VMERR		;BRANCH IF NO ADDRESS SPACE LEFT
	ADD	#256.,$HGPAG	;UPDATE FREE PAGE ADDRESS
	BR	$ALVR2		;LINK PAGE TO RESIDENT LIST, RETURN

;+
; $ALVRT - ALLOCATE VIRTUAL MEMORY
;
; INPUTS:
;
;	R1=NUMBER OF BYTES TO ALLOCATE
;
; OUTPUTS:
;
;	R0=REAL ADDRESS OF ALLOCATION
;	R1=VIRTUAL ADDRESS OF ALLOCATION
;
;	THE SPECIFIED PAGE IS WRITE-MARKED
;
; THIS ROUTINE IS CALLED TO ALLOCATE VIRTUAL MEMORY AND MAKE
; THE ALLOCATED SPACE RESIDENT IN CORE. THE AMOUNT OF SPACE
; TO BE ALLOCATED IS ROUNDED TO THE NEAREST WORD
;
;-

$ALVRT::SAVRG			; SAVE NON-VOLATILE REGISTERS
	CALL	$RQVCB		; REQUEST VIRTUAL CORE BLOCK
$ALVR1:	BCS	VMERR		; IF C/S NO VIRTUAL SPACE LEFT
$ALVR2:	MOV	R1,R5		; SAVE VIRTUAL ADDRESS
	SWAB	R5		; PUT RELATIVE BLOCK IN LOW BYTE
	TSTB	R1		; IS REQUEST ON BLOCK BOUNDRY?
	BNE	20$		; IF NE NO, BLOCK ALREADY EXISTS
	MOV	#P$GSIZ,R1	; CREATE A PAGE BUFFER
	CALL	$ALBLK		; ALLOCATE STORAGE SPACE
	MOV	$PAGLS,R2	; GET ADDRESS OF PAGE LIST
	BEQ	10$		; IF EQ NONE
	CLR	R1		; SET FOR MOVB WITHOUT EXTEND
	BISB	R5,R1		; GET RELATIVE BLOCK NUMBER
	ASL	R1		; CONVERT TO WORD OFFSET
	ADD	R1,R2		; COMPUTE ADDRESS IN LIST
	MOV	R0,(R2)		; STORE ADDRESS

10$:	MOV	$PAGHD,P$GNXT(R0) ; LINK OLD FIRST TO NEW FIRST
	MOV	R0,$PAGHD	; SET NEW FIRST
	MOVB	R5,P$GBLK(R0)	; SET RELATIVE BLOCK NUMBER OF PAGE
	MOV	R5,R1		; RESTORE SWAPPED VIRTUAL ADDRESS
	SWAB	R1		; STRAIGHTEN IT

20$:	CALL	$CVRL		; CONVERT TO REAL ADDRESS
30$:	JMP	$WMCPG		; WRITE-MARK PAGE AND EXIT

VMERR:	MOV	#<S$V2*400!E$R76>,R1 ; GET ERROR/SEVERITY
	CLR	R2		; SET DUMMY PARAMETER BLOCK ADDRESS
	CALL	$ERMSG		; FATAL-NO RETURN


;+
; $RQVCB - REQUEST VIRTUAL MEMORY ALLOCATION
;
; INPUTS:
;
;	R1=ALLOCATION REQUEST IN BYTES (</= 512.)
;	$HGVAD = NEXT FREE VIRTUAL ADDRESS
;	$HGPAG = NEXT FREE VIRTUAL PAGE BOUNDRY
;
; OUTPUTS:
;
;	C-CLEAR: REQUEST SUCCEEDED.
;
;	R1=VIRTUAL ADDRESS OF REQUESTED BLOCK
;	$HGVAD = UPDATED VALUE OF NEXT FREE ADDRESS
;	$HGPAG = UPDATE VIRTUAL PAGE BOUNDRY (ALWAYS GREATER THAN $HGVAD)
;
;	C-SET: REQUEST FAILED
;
;	FAILURE CONDITIONS:
;
;	(1) REQUEST EXCEEDED 512. BYTES
;	(2) VIRTUAL STORAGE EXHAUSTED
;
; THIS ROUTINE IS CALLED TO ALLOCATE SPACE FROM VIRTUAL
; MEMORY. THE ALLOCATION REQUEST IS ROUNDED UP TO THE
; NEAREST WORD. IF THE ROUNDED VALUE CROSSES A DISK BLOCK
; BOUNDRY THEN ALLOCATION BEGINS AT THE NEXT BLOCK.
;
;		*** NOTE ***
;
; VIRTUAL ADDRESSES ARE WORD VALUES.
;-

$RQVCB::MOV	R1,R2		; SAVE LENGTH
	MOV	$HGVAD,R1	; GET HIGHEST VIRTUAL ADDRESS
	BEQ	20$		; IF EQ NO SPACE LEFT

	CLC			; CLEAR CARRY
	ROR	R2		; DO UNSIGNED DIVIDE
	ADC	R2		; ROUND UP TO NEAREST WORD
	CMP	R2,#256.	; CHECK LENGTH OF REQUEST
	BHI	20$		; IF HIGH, TOO BIG
	DEC	R2		; BACK OFF SIZE BY ONE
	MOV	R1,R0		; COPY FREE ADDRESS
	ADD	R2,R0		; COMPUTE LAST ADDRESS IN ALLOCATION
	BCS	30$		; IF C/S ADDRESS SPACE OVERFLOW
	CLRB	R0		; CLEAR DISPLACEMENT INTO BLOCK
	CMP	R0,R1		; SEE IF CROSSING BLOCK BOUNDRY
	BLOS	10$		; IF LOS NO
	MOV	$HGPAG,R1	;SET HIS VIRTUAL ADDRESS TO NEXT PAGE IF YES
	BEQ	20$		;BRANCH IF NO ROOM LEFT

10$:	INC	R2		; CONVERT BACK TO LENGTH
	ADD	R1,R2		; COMPUTE NEXT FREE ADDRESS
	MOV	R2,R0		;COPY FREE ADDR
	CLRB	R0		;CLEAR OFFSET IN PAGE
	CMP	R0,$HGVAD	;SEE IF CROSSED PAGE BOUNDRY
	BLOS	15$		;BRANCH IF NO
	SUB	R0,R2		;CLEAR THE PAGE BITS
	ADD	$HGPAG,R2	;AND SET TO NEXT WHOLE PAGE BOUNDRY
	ADD	#256.,$HGPAG	;SET NEW FREE PAGE ADDR

15$:	MOV	R2,$HGVAD	; UPDATE FREE ADDRESS
	CLC			; SET SUCCESS
	RETURN			; AND EXIT

20$:	SEC			; SET FAILURE
30$:	RETURN			;


;+
; CVRL - MAKE VIRTUAL STORE RESIDENT
;
; INPUTS:
;
;	R1=VIRTUAL ADDRESS
;
; OUTPUTS:
;
;	R0=REAL ADDRESS
;	$CURPG POINTS TO CURRENT PAGE HEADER FOR FAST WRITE MARK/LOCK ACCESS
;	R1 IS UNCHANGED
;
; THIS ROUTINE IS CALLED TO CONVERT A VIRTUAL ADDRESS INTO
; AN ADDRESS IN REAL MEMORY. THE LIST OF RESIDENT PAGE
; BUFFERS IS CHECKED FIRST. IF THE PAGE IS NOT
; IN MEMORY A BUFFER IS ALLOCATED AND THE PAGE IS READ INTO
; CORE
;
;-

$CVRL::	SAVRG			; SAVE NON-VOLATILE REGISTERS
	MOV	R1,R5		; COPY VIRTUAL ADDRESS
	SWAB	R5		; POSITION BLOCK NUMBER TO LOW BYTE
	CALL	$FNDPG		; SEARCH FOR PAGE
	BCC	10$		; IF C/C PAGE IN CORE.
	MOV	#P$GSIZ,R1	; GET SIZE OF PAGE BUFFER
	CALL	$ALBLK		; ALLOCATE MEMORY
	MOV	$PAGLS,R4	; GET ADDRESS OF PAGE LIST
	BEQ	5$		; IF EQ NONE
	CLR	R2		; SET FOR MOVB WITH NO EXTEND
	BISB	R5,R2		; GET RELATIVE BLOCK NUMBER
	ASL	R2		; CONVERT TO WORD OFFSET
	ADD	R2,R4		; COMPUTE LIST ADDRESS
	MOV	R0,(R4)		; STORE ADDRESS OF PAGE
5$:	MOVB	R5,P$GBLK(R0)	; SET RELATIVE BLOCK NUMBER
	CALL	$RDPAG		; READ PAGE INTO CORE
	MOV	$PAGHD,P$GNXT(R0) ; LINK OLD FIRST TO NEW FIRST
	MOV	R0,$PAGHD	; SET NEW FIRST
	MOV	$TIME,P$GTIM(R0) ; TIME-STAMP PAGE
	MOV	R5,R1		; RESTORE VIRTUAL ADDRESS
	SWAB	R1		; STRAIGHTEN IT
	MOV	R0,$CURPG	;SAVE CURRENT PAGE NUMBER		;JR
10$:	CLRB	R5		; CLEAR BLOCK NUMBER
	SWAB	R5		; GET WORD DISPLACEMENT IN LOW BYTE
	ASL	R5		; MAKE BYTE OFFSET
	ADD	R5,R0		; ADD DISPLACEMENT IN BLOCK
	ADD	#P$GHD,R0	; OFFSET PAST HEADER

	.IF DF	W$$KST
	INC	$WRKAC+2	; INCREMENT TOTAL ACCESS COUNT
	BNE	20$		; IF NE NO OVERFLOW
	INC	$WRKAC		; INCREMENT HIGH PART OF COUNT
20$:				;
	.ENDC

	RETURN			;


;+
; $FNDPG - SEARCH PAGE BUFFERS FOR RESIDENT ADDRESS
;
; INPUTS:
;
;	R1=VIRTUAL ADDRESS
;
; OUTPUTS:
;
;	C-CLEAR: PAGE BUFFER RESIDENT
;
;		R0=ADDRESS OF BUFFER
;
;	C-SET:   PAGE NOT RESIDENT
;
; IN EITHER CASE R1 IS LEFT UNCHANGED AND $CURPG IS POINTED TO THE PAGE HEADER
;
; IF RESIDENT, PAGE BUFFER IS TIME-STAMPED AND RETURNED
; TO CALLER
;-

$FNDPG::INC	$TIME		; ADVANCE TIME
	BNE	4$		; IF NE NO WRAP-AROUND
	INC	$TIME		; SET TIME TO 1
	MOV	#<$PAGHD-P$GNXT>,R0 ; GET RESIDENT PAGE LIST
2$:				;
	MOV	P$GNXT(R0),R0	; GET ADDRESS OF NEXT RESIDENT PAGE
	BEQ	4$		; IF EQ DONE
	CLR	P$GTIM(R0)	; RESET TIME
	BR	2$		; GO AGAIN
4$:				;
	MOV	R1,R2		; COPY VIRTUAL ADDRESS
	CLRB	R2		; CLEAR DISPLACEMENT IN BLOCK
	SWAB	R2		; POSITION RELATIVE BLOCK TO LOW BYTE
	MOV	$PAGLS,R0	; GET ADDRESS OF RESIDENT PAGE LIST
	BEQ	8$		; IF EQ NONE
	ASL	R2		; CONVERT BLOCK TO WORD OFFSET
	ADD	R2,R0		; COMPUTE LIST INDEX
	SEC			; ASSUME PAGE NOT RESIDENT
	MOV	(R0),R0		; GET PAGE ADDRESS
	BEQ	20$		; IF EQ PAGE NOT IN MEMORY
	CLC			; SET SUCCESS
	BR	15$		; EXIT
8$:				;
	MOV	#<$PAGHD-P$GNXT>,R0 ; GET LISTHEAD ADDRESS MINUS OFFSET
10$:				;
	SEC			; ASSUME PAGE NOT RESIDENT
	MOV	P$GNXT(R0),R0	; GET NEXT PAGE BUFFER
	BEQ	20$		; IF EQ NO MORE
	CMPB	P$GBLK(R0),R2	; CHECK RELATIVE BLOCK
	BNE	10$		; IF NE NOT REQUESTED PAGE
15$:	MOV	$TIME,P$GTIM(R0) ; TIME-STAMP THIS PAGE
20$:	MOV	R0,$CURPG	;SET POINTER TO CURRENT PAGE HEADER
	RETURN


;+
; $LCKPG - LOCK PAGE IN MEMORY
; $UNLPG - UNLOCK PAGE FROM MEMORY 
; $WRMPG - MARK PAGE AS WRITTEN INTO
; 
; INPUTS:
;
;	R1=VIRTUAL ADDRESS
;
; OUTPUTS:
;
;	C-CLEAR: PAGE WAS MARKED AS REQUESTED
;
;	C-SET: PAGE NOT RESIDENT
;
; ALL REGISTER CONTENTS ARE PRESERVED
;-
 
$LCKPG::SAVVR			; SAVE VOLATILE REGISTERS
	CALL	$FNDPG		; FIND SPECIFIED PAGE
	BCS	10$		; IF C/S PAGE NOT IN MEMORY
	INCB	P$GLOK(R0)	; INCREMENT LOCK COUNT
10$:	RETURN			;

$UNLPG::SAVVR			; SAVE VOLATILE REGISTERS
	CALL	$FNDPG		; FIND SPECIFIED PAGE
	BCS	10$		; IF C/S PAGE NOT IN MEMORY
	DECB	P$GLOK(R0)	; DECREMENT LOCK COUNT
10$:	RETURN			;
 
$WRMPG::SAVVR			;
	CALL	$FNDPG		; FIND SPECIFIED PAGE
	BCS	10$		; IF C/S NOT RESIDENT
	BISB	#PG$WRT,P$GSTS(R0) ; WRITE-MARK PAGE
10$:	RETURN			;

;+
; $LOCPG, $ULCPG, $WMCPG
; LOCK, UNLOCK AND WRITE MARK CURRENTLY ACCESSED PAGES
; HANDLE THESE COMMON CASES SEPARATELY AND SPEEDILY
; ENTER WITH $CURPG POINTING AT CURRENTLY ACCESSED PAGE HEADER
;-

$LOCPG::MOV	R0,-(SP)	;SAVE WORK REGISTER
	MOV	$CURPG,R0	;PICK UP PAGE BUFFER POINTER
	INCB	P$GLOK(R0)	;BUMP PAGE USAGE COUNT
	BR	CPGEX		;TAKE COMMON EXIT

$ULCPG::MOV	R0,-(SP)	;SAVE WORK REGISTER
	MOV	$CURPG,R0	;PICK UP PAGE BUFFER POINTER
	DECB	P$GLOK(R0)	;DOWN THE PAGE USAGE COUNT
	BR	CPGEX		;TAKE COMMON EXIT

$WMCPG::MOV	R0,-(SP)	;SAVE WORK REGISTER
	MOV	$CURPG,R0	;PICK UP PAGE BUFFER POINTER
	BISB	#PG$WRT,P$GSTS(R0) ;SET DIRTY BIT
CPGEX:	MOV	(SP)+,R0	;RESTORE REGISTER
	RETURN


	.MCALL	READ$,WRITE$,WTSE$S,EXTK$S

;
; LOCAL DATA
;
	.PSECT	MIXED,D,RW	;PUT IN DATA PSECT FOR I/D TESTS	;JR

BLK:	.WORD	0		; HIGH ORDER PART OF VIRTUAL BLOCK NUMBER
BLKL:	.BLKW	1		; LOW ORDER PART OF VIRTUAL BLOCK NUMBER
IOSTS:	.BLKW	2		; I/O DONE STATUS BLOCK
WKEXT:	.WORD	W$KEXT		; WORK FILE EXTEND SIZE

	PURE	PUREI,I		;					;JR
;+
; RDPAG - READ A PAGE INTO MEMORY
;
; INPUTS:
;
;	R0=PAGE ADDRESS
;	PAGE HEADER (P$GBLK) MUST CONTAIN RELATIVE BLOCK
;	NUMBER OF PAGE.
;
; OUTPUTS:
;
;	REQUESTED PAGE IS TRANSFERRED INTO MEMORY
;-

	PURE	PUREI,I

$RDPAG::SAVVR			; SAVE VOLATILE REGISTERS
	CALL	STIOR		; SETUP FOR READ
	READ$	R0,R1,#512.,#BLK,#2,#IOSTS ; EXECUTE READ
	CALL	CHKST		; CHECK STATUS
	BCS	CHKST1		; IF C/S FATAL ERROR
 
	.IF DF	W$$KST
 
	INC	$WRKRD+2	; INCREMENT LOW PART OF READ COUNT
	BNE	10$		; IF NE HAVE CURRENT COUNT
	INC	$WRKRD		; INCREMENT HIGH PART OF COUNT
10$:				;
	.ENDC
 
	RETURN			; EXIT



;+
; $WRPAG - WRITE A PAGE INTO VIRTUAL STORE
;
; INPUTS:
;
;	R2=ADDDRESS OF PAGE TO BE SWAPPED OUT
;	PAGE HEADER (P$GBLK) MUST CONTAIN RELATIVE BLOCK NUMBER
;	OF PAGE
;
; OUTPUTS:
;
;	REQUESTED PAGE IS TRANSFERRED TO VIRTUAL STORE
;
;-

$WRPAG::SAVVR			; SAVE VOLATILE REGISTERS
	CALL	STIOW		; SET REGISTERS FOR WRITE
10$:	WRITE$	R0,R1,#512.,#BLK,#2,#IOSTS ; EXECUTE SWAP
	CALL	CHKST		; WAIT FOR COMPLETION
	MOV	WKEXT,F.ALOC(R0) ; RESET DEFAULT EXTENSION
	BCS	10$		; IF C/S REPEAT OPERATION
 
	.IF DF	W$$KST
 
	INC	$WRKWR+2	; INCREMENT LOW PART OF WRITE COUNT
	BNE	20$		; IF NE HAVE COUNT
	INC	$WRKWR		; INCREMENT HIGH PART OF COUNT
20$:				;
	.ENDC
 
	RETURN

;
; SETUP REGISTERS FOR A PAGE I/O REQUEST
;

STIOR:	MOV	R0,R1		; COPY PAGE ADDRESS
	BR	STIO		;
STIOW:	MOV	R2,R1		; COPY PAGE ADDRESS
STIO:	CLR	BLKL		; CLEAR LOW PART OF VBN
	MOVB	P$GBLK(R1),BLKL ; SET LOW PART OF VBN
	INC	BLKL		; CONVERT RELATIVE BLOCK TO VBN
	ADD	#P$GHD,R1	; STEP PAST HEADER
	MOV	$WRKPT,R0	; GET RECORD BLOCK
	RETURN			;

;
; CHECK STATUS OF I/O REQUEST
;

	.ENABL	LSB
 
CHKST:	BCS	5$		; IF C/S NEGATE EXTEND SIZE
	WTSE$S	#2		; WAIT FOR I/O COMPLETION
	ROLB	IOSTS		; GET SIGN OF STATUS BYTE
	BCC	10$		; IF C/C OK
5$:	NEG	WKEXT		; NEGATE WORK FILE EXTEND SIZE
	SEC			; SET FAILURE
	BMI	10$		; IF MI REPEAT OPERATION
CHKST1:	CLR	R2		; SET DUMMY PARAMETER BLOCK ADDRESS
	MOV	#<S$V2*400!E$R73>,R1 ; GET ERROR/SEVERITY
	CALL	$ERMSG		; FATAL-NO RETURN
10$:	RETURN			;
 
	.DSABL	LSB


;+
; GTCOR - ALLOCATE DYNAMIC STORAGE, DISPLACE VIRTUAL PAGE BUFFERS
;
; INPUTS:
;
;	R1=ALLOCATION REQUESTED(BYTES)
;	$TPADR=NEXT VIRTUAL ADDRESS ABOVE TASK IMAGE
;
; OUTPUTS:
;
;	C-CLEAR: ALLOCATION REQUEST SUCCEEDED
;
;	R0=ADDRESS OF STORAGE ALLOCATED
;
;	C-SET: ALLOCATION FAILED
;
; THIS ROUTINE IS CALLED TO OBTAIN DYNAMIC STORAGE. AN ATTEMPT IS MADE
; TO ALLOCATE THIS MEMORY FROM THE EXISTING POOL. IF THIS FAILS, A REQ-
; UEST TO EXTEND THE TASK IS ISSUED. IF THE REQUIRED MEMORY CANNOT BE OB-
; TAINED THEN UNLOCKED PAGE BUFFERS ARE DISPLACED UNTIL THE REQUESTED SPACE
; IS AVAILABLE. PAGE DISPLACEMENT IS ON THE BASIS OF 'LEAST RECENTLY USED'.
;-

$GTCOR::SAVRG			; SAVE NON-VOLATILE REGISTERS
10$:	MOV	R1,-(SP)	; SAVE BYTE COUNT
	MOV	#$FRHD,R0	; GET ADDRESS OF FREE CORE POOL
	CALL	$RQCB		; REQUEST CORE BLOCK
	BCC	60$		; IF C/C HAVE REQUESTED SPACE

	.IF DF	W$$KST

	MOV	#$TPADR,R3	; GET POINTER TO NEXT FREE ADDRESS
	MOV	(R3),R2		; GET NEXT FREE ADDRESS
	CMP	R2,#T$KMAX	; TASK AT MAX. ALLOWABLE SIZE?
	BHIS	17$		; IF HIS YES
	MOV	#T$KINC,R1	;R1 = # 32WD blocks to extend by
16$:	EXTK$S	R1		;Extend by default amount
	BCS	17$		; IF C/S EXTEND FAILED
.REPT	6
	ASL	R1		; R1 = R1*64. to get # bytes extended.
.ENDR
	ADD	R1,$FRSIZ	; ADD INCREMENT TO POOL
	ADD	R1,(R3)		; UPDATE TOP OF MEMORY
	BR	47$		; RELEASE BLOCK TO POOL

	MIN$PG	= <<<P$GSIZ+<64.-1>>&177700>/64.> ;32wd chuck minimum

17$:	CMP	R1,#MIN$PG	;Can we reduce the allocation?
	BLE	18$		;Branch if not, at the end of dynamic mem.
	MOV	#MIN$PG,R1	;Set new increment value
	BR	16$		;Try one again.

18$:	MOV	#-1,(R3)	; BLOCK FURTHER ATTEMPTS TO EXTEND TASK

	.ENDC

;
; INSUFFICIENT STORAGE IS AVAILABLE. FREE UP MEMORY
; BY DISPLACING PAGE BUFFERS
;

	MOV	#<$PAGHD-P$GNXT>,R4 ; GET LISTHEAD ADDRESS MINUS OFFSET
	CLR	R2		; CLEAR ADDRESS OF LRU PAGE
	MOV	#-1,R3		; SET MAXIMUM TIME
	CLR	-(SP)		; CLEAR LRU PREDECESSOR
20$:	MOV	R4,R5		; SAVE PREDECESSOR
	MOV	P$GNXT(R5),R4	; GET NEXT PAGE
	BEQ	30$		; IF EQ DONE
	CMP	P$GTIM(R4),R3	; TEST TIME OF LAST USE VS. LEAST
	BHI	20$		; IF HI, THIS BUFFER NOT LEAST
	TSTB	P$GLOK(R4)	; THIS PAGE LOCKED IN MEMORY?
	BNE	20$		; IF NE YES
	MOV	R4,R2		; MARK BUFFER AS LRU
	MOV	P$GTIM(R4),R3	; SAVE TIME
	MOV	R5,(SP)		; SAVE PREDECESSOR FOR RELINK
	BR	20$		; GO AGAIN
30$:	TST	R2		; FIND LRU?
	SEC			; ASSUME NO
	BEQ	50$		; IF EQ NO
	BITB	#PG$WRT,P$GSTS(R2) ; PAGE WRITTEN INTO ?
	BEQ	40$		; IF EQ NO
	CALL	$WRPAG		; WRITE OUT PAGE INTO VM
40$:				;
	MOV	$PAGLS,R3	; GET ADDRESS OF PAGE LIST
	BEQ	45$		; IF EQ NONE
	CLR	R0		; SET FOR MOVB WITH NO SIGN EXTEND
	BISB	P$GBLK(R2),R0	; GET BLOCK NUMBER
	ASL	R0		; CONVERT TO WORD OFFSET
	ADD	R0,R3		; COMPUTE LOCATION IN LIST
	CLR	(R3)		; CLEAR ENTRY
45$:				;
	MOV	(SP)+,R3	; RETRIEVE LRU PREDECESSOR
	MOV	P$GNXT(R2),P$GNXT(R3) ; RELINK REMAINING PAGES
	MOV	#P$GSIZ,R1	; SET SIZE TO RELEASE
47$:				;
	MOV	#$FRHD,R0	; GET ADDRESS OF FREE POOL LISTHEAD
	CALL	$RLCB		; RELEASE MEMORY
	MOV	(SP)+,R1	; RESTORE COUNT
	BR	10$		; GO AGAIN
50$:				;
	INC	(SP)+		; CLEAN STACK, LEAVE C INTACT
60$:				;
	INC	(SP)+		; CLEAN STACK
	RETURN			;


;
; MACRO LIBRARY CALLS
;
	.MCALL	FCSBT$,FDAT$A,FDBDF$,FDOP$A,FDRC$A
;
; EQUATED SYMBOLS
;
;
; DEFINE PAGE BUFFER OFFSETS
;
	.PSECT	$$VMDF,ABS
 
P$GNXT::.BLKW	1		; LINK TO NEXT PAGE
P$GBLK::.BLKB	1		; PAGE RELATIVE BLOCK NUMBER
P$GSTS::.BLKB	1		; PAGE STATUS BYTE
P$GTIM::.BLKW	1		; 'TIME' OF LAST REFERENCE
P$GLOK::.BLKB	1		; PAGE LOCK COUNT
 
	.EVEN
 
P$GHD::				; SIZE OF PAGE HEADER
 
	.BLKW	256.
 
P$GSIZ::			; SIZE OF PAGE 
 
;
; PAGE STATUS BITS
;
 
PG$WRT==000001			; PAGE WRITTEN INTO (1=YES)
 
	.PSECT	MIXED,D,RW	;					;JR
 
;
; GLOBAL DATA
;
$HGVAD::.BLKW	1		;NEXT AVAILABLE VIRTUAL ADDRESS
$HGPAG::.BLKW	1		;NEXT AVAILABLE VIRTUAL PAGE BOUNDRY 
$PAGLS::.BLKW	1		;POINTER TO RESIDENT PAGE LIST
$PAGHD::.BLKW	1		;RESIDENT PAGE LIST HEAD
$TIME::	.BLKW	1		;LRU TIMER
$CURPG::.BLKW	1		;POINTER TO LAST ACCESSED PAGE HEADER

;
; SYMBOL TABLE DATA
;
$HSYBS::.BLKW	1		;BASE ADDR FOR INITIAL PAGE OF HASH TABLE
$HPSBS::.BLKW	1		;BASE ADDR FOR PST/MACRO HASH TABLE
$HSYLM::.BLKW	1		;NUMBER OF PAGES IN USER HASH TABLE*400
$HPSLM::.BLKW	1		;NUMBER OF PAGES IN PST HASH TABLE*400
$HSOFL::.BLKW	1		;OVERFLOW PAGE FOR USER SYMBOL HASH TABLE
$HPOFL::.BLKW	1		;OVERFLOW PAGE FOR OPCODES/MACRO NAMES TABLE
 
	.IF DF	W$$KST
 
$TPADR::.BLKW	1		;TOP OF TASK R/W VIRTUAL MEMORY
 
; Workfile access counts (Adjacency required for init by INIVM)
 
$WRKAC::.BLKW	2		;TOTAL NUMBER OF WORK FILE ACCESSES
$WRKRD::.BLKW	2		;WORK FILE READ COUNT
$WRKWR::.BLKW	2		;WORK FILE WRITE COUNT
 
	.ENDC
 
; Work file FDB

WRKPT:	FDBDF$			; ALLOCATE SPACE FOR FDB
	FCSBT$			; DEFINE FCS BITS
	FDAT$A	R.FIX,,512.	; FIXED LENGTH, 512.BYTE RECORDS
	FDRC$A	FD.RWM!FD.RAN	; READ/WRITE MODE, RANDOM ACCESS
	FDOP$A	W$KLUN		; I/O VIA LUN DEFINED AT TASK BUILD TIME

; Pointer for workfile FDB

	PURE	DPURE,D
 
$WRKPT::.WORD	WRKPT		; ADDRESS OF FDB


	.END