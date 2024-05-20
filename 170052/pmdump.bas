1		EXTEND
2!		PROGRAM NAME	: PMDUMP.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10		EXTEND
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1977, 1991 by &
  !	        Digital Equipment Corporation, Maynard, Mass. &
  !	&
  !	&
  !	This software is furnished under a license and may be used and &
  !	copied  only  in accordance with the terms of such license and &
  !	with the  inclusion  of  the  above  copyright  notice.   This &
  !	software  or  any  other copies thereof may not be provided or &
  !	otherwise made available to any other person.  No title to and &
  !	ownership of the software is hereby transferred. &
  !	&
  !	The information in this software is subject to change  without &
  !	notice  and should not be construed as a commitment by Digital &
  !	Equipment Corporation. &
  !	&
  !	DIGITAL assumes no responsibility for the use  or  reliability &
  !	of its software on equipment that is not supplied by DIGITAL. &
  !	&
  !******************************************************************* &
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&
	! VER/ED	EDIT DATE	REASON &
	! &
	! 9.0-08/KCG	04-OCT-84	MADE SWITCHES AVAILABLE THROUGH &
	!				CCL ENTRY POINT.  GAVE ASCII DUMP &
	!				KNOWLEDGE OF DEC MULTINATIONAL &
	!				CHARACTER SET. &
	! 9.7-06/FEK	08-Mar-89	Add Support for new TKB options &
	!				and switches &

25	! VER/ED	EDIT DATE	REASON &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	! THIS PROGRAM PROVIDES A DEBUGGING FACILITY FOR PROGRAMS &
	! WHICH RUN UNDER THE RSXRTS RUN-TIME SYSTEM OR ANY OF ITS &
	! DERIVATIVES.  THE PROGRAM PROVIDES A FORMATTED DUMP OF &
	! THE CONTENTS OF LOW CORE AT THE TIME OF THE ABORT OF THE &
	! PROBLEM PROGRAM, AND IT PROVIDES AN OCTAL DUMP OF THE &
	! STATE OF THE ENTIRE MEMORY OCCUPIED BY THE JOB AT THE TIME &
	! OF THE ABORT.  THE OUTPUT OF THIS PROGRAM CAN BE DIRECTED &
	! TO ANY RSTS/E OUTPUT DEVICE. &
	! &
	! THE PROGRAM EXPECTS A COMMAND LINE OF THE FORM : &
	!	<OUTPUT FILE SPEC>=<INPUT FILE SPEC> &
	! WHERE <OUTPUT FILE SPEC> IS THE FILE TO PRINT TO, AND &
	! <INPUT FILE SPEC> IS THE PMD FILE CREATED BY THE RUN-TIME &
	! SYSTEM.  IF NO <INPUT FILE SPEC> APPEARS, THE PROGRAM &
	! ASSUMES A NAME OF THE FORM &
	!	PMDNNN.PMD &
	! WHERE NNN IS THE JOB NUMBER OF THE CURRENT PROGRAM.  IF &
	! NO <OUTPUT FILE SPEC> APPEARS, THE PROGRAM ASSUMES THAT &
	! THE OUTPUT IS TO GO TO THE SAME DEVICE AND FILENAME FROM &
	! WHICH THE INPUT IS TAKEN, BUT THE PPN OF THE OUTPUT IS THAT &
	! UNDER WHICH THE PROGRAM IS RUNNING AND THE EXTENSION OF THE &
	! OUTPUT FILE IS '.LST'. &
	! &
	! THE PROGRAM HAS CCL AND CHAIN ENTRY POINTS.  THE CCL COMMAND &
	! IS IN THE FORM : &
	!		PMDUMP <OUTPUT>=<INPUT> &
	! AND THE CONTENTS OF CORE COMMON UPON A CHAIN ENTRY MUST BE &
	! A COMMAND STRING OF THE FORM : &
	!		<OUTPUT>=<INPUT> &
	! THE SAME DEFAULTS ARE APPLIED TO CCL COMMANDS AND CHAIN &
	! COMMANDS AS TO INTERACTIVE COMMANDS. &
	! &
	! THE PROGRAM DOES NOT, UPON COMPLETION OF A DUMP, COME BACK &
	! TO THE USER TO REQUEST ANOTHER COMMAND LINE.  IT GOES TO &
	! READY.  IT WILL ALSO NOT CHAIN BACK TO SOME OTHER PROGRAM. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
	! CHANNEL #	USED FOR &
	! &
	!	0	GENERAL ERROR PRINTING AND PROMPT TEXT &
	!	1	.PMD FILE TO DUMP &
	!	2	.LST FILE TO CREATE &
	!	3	COMMAND DEVICE &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&
	! VARIABLE NAME		USED FOR &
	! &
	!	ACCT$		HOLDS THE PPN UNDER WHICH THIS JOB IS &
	!			RUNNING FOR DEFAULT FILE STRINGS &
	!	BEG.POS%	USED IN THE PRINT A LINE ROUTINE TO &
	!			KEEP TRACK OF THE PART OF THE LINE &
	!			NOT YET PRINTED &
	!	BIT.CNT%	RETURNED FROM THE FNBIT.DCD$() FUNCTION &
	!			WITH THE COUNT OF BITS DECODED &
	!	C$		HOLDS THE COMMAND STRING &
	!	CHR10$		CHR$(10%) &
	!	CHR12$		CHR$(12%) &
	!	CHR255$		CHR$(255%) &
	!	CHR9$		CHR$(9%) - A TAB &
	!	CHR9.BL$	CHR$(9%)+CHR$(32%) - <TAB><SPACE> &
	!	CR.LF$		CHR$(13%)+CHR$(10%) - A CR/LF COMBINATION &
	!	CUR.BLK%	BLOCK NUMBER OF THE BLOCK FROM THE INPUT &
	!			FILE WHICH IS CURRENTLY IN THE BUFFER &
	!	CUR.POS%	USED IN THE PRINT A LINE ROUTINE TO KEEP &
	!			TRACK OF WHERE THE LAST SCAN ENDED &
	!	DEF.INP$	FILENAME STRING CONTAINING THE DEFAULT &
	!			INPUT FILE SPECS &
	!	DEV.FLG$	USED TO HOLD THE FLAG BYTE FOR THE LOGICAL &
	!			DEVICE ASSIGNMENT TABLE &
	!	DEV.NAM$	USED TO HOLD THE NAME OF THE PHYSICAL &
	!			DEVICE TO WHICH THE LOGICAL ASSIGNMENT &
	!			REFERS IN THE LOGICAL DEVICE ASSIGNMENT &
	!			TABLE &
	!	DEV.NM1$	HOLDS THE FIRST WORD OF RADIX 50 OF THE &
	!			LOGICAL NAME &
	!	DEV.NM2$	HOLDS THE SECOND WORD OF RADIX 50 OF THE &
	!			LOGICAL NAME &
	!	DEV.UNT$	HOLDS THE UNIT NUMBER OF THE DEVICE &
	!	DMP.HDR$	A STRING CONSTANT WHICH HOLDS THE &
	!			HEADER LINES TO USE WHEN DUMPING THE &
	!			CONTENTS OF THE INPUT FILE IN OCTAL &
	!	E%		GENERAL ERROR VARIABLE &
	!	E0%		ENTRY TYPE FLAG - 0 => RUN ENTRY &
	!					  2 => CCL ENTRY &
	!					  3 => CHAIN ENTRY &
	!	END.POS%	USED IN THE PRINT A LINE ROUTINE TO &
	!			KEEP TRACK OF THE PART OF THE STRING TO &
	!			BE PROCESSED &
	!	FF.POS%		USED IN THE PRINT A LINE ROUTINE TO KEEP &
	!			TRACK OF THE POSITION OF THE NEXT <FF> &
	!			CHARACTER &
	!	FIR.WRD%	USED TO HOLD CONTENTS OF WORDS IN THE &
	!			SAVED FIRQB &
	!	I$		VERSION/EDIT STRING &
	!	ID.VAL%		Task system ID byte &
	!	IN.PUT$		NAME OF THE INPUT FILE &
	!	IN.PUT%		CHANNEL NUMBER ON WHICH THE INPUT FILE &
	!			IS OPEN &
	!	IO.STS%		VALUE OF THE SAVED IOSTS VALUE FROM THE &
	!			DUMPED FIRQB &
	!	ITM.BEG%	BEGIN POSITION IN BLOCK 1 OF THE DUMP &
	!			FILE OF A PARTICULAR ITEM (VALUE OF THIS &
	!			WORD IS READ OUT OF DATA STATEMENTS FOR &
	!			EACH ITEM TO PRINT IN THE FIRST BLOCK) &
	!	ITM.DCD%	DECODE TYPE FOR AN ITEM TO PRINT &
	!			(SEE LINE 3991 FOR A DESCRIPTION OF &
	!			THE DECODE TYPES) &
	!	ITM.HDR$	THE HEADER STRING TO PRINT WITH THIS &
	!			ITEM &
	!	ITM.LEN%	LENGTH IN THE BUFFER OF THIS ITEM &
	!	ITM.RAW$	THE FIELDED, 'RAW', UNDECODED VALUE &
	!			OF THIS ITEM &
	!	ITM.RAW%	THE 'RAW' VALUE OF THE ITEM CONVERTED &
	!			TO BINARY &
	!	ITM.TRL$	THE TRAILER STRING TO PRINT WITH THIS &
	!			ITEM &
	!	ITM.VAL$	THE DECODED VALUE OF THIS ITEM &
	!	JOB.KEY.DCD$	THE DECODE STRING FOR THE JOB KEYWORD &
	!			IN THE FIRST BLOCK &
	!	JOB.NO%		JOB NUMBER OF THE JOB UNDER WHICH PMDUMP &
	!			IS RUNNING &
	!	LEN.COM%	LENGTH OF THE CORE COMMON STRING STORED &
	!			IN THE DUMP &
	!	LF.POS%		USED IN THE PRINT A LINE ROUTINE TO KEEP &
	!			TRACK OF THE LAST PLACE A <LF> WAS FOUND &
	!	LIB.FLG.DCD$	Library FLAG WORD DECODE STRING &
	!	LNE.CNT%	USED IN THE PRINT A LINE ROUTINE TO KEEP &
	!			TRACK OF THE LINE TO PRINT NEXT FOR &
	!			AUTOMATIC PAGE OVERFLOW &
	!	LOG.CNT%	COUNT OF LUN'S ASSIGNED TO THE JOB &
	!	LP.CNT%		GENERAL LOOP COUNTER &
	!	LP1.CNT%	GENERAL LOOP COUNTER &
	!	LP2.CNT%	GENERAL LOOP COUNTER &
	!	LP3.CNT%	GENERAL LOOP COUNTER &
	!	LUN.BUF$	BUFFERSIZE FOR AN ASSIGNED LUN &
	!	LUN.DEV$	DEVICE NAME FOR AN ASSIGNED LUN &
	!	LUN.FLG$	LUN FLAG &
	!	LUN.FLG.DCD$	DECODE STRING FOR THE VALUES IN THE LUN &
	!			FLAG &
	!	LUN.UNT$	UNIT NUMBER OF THE DEVICE ASSOCIATED WITH &
	!			A LUN &
	!	NO.STCK%	INHIBIT FLAG FOR PRINT OF USER STACK &
	!			SET IF AN ERROR OCCURED IN THE LOOKUP OF THE &
	!			SAVED REGISTER SET &
	!	NUL.STG$	"" - THE NULL STRING &
	!	OUT.FND%	RESULT OF THE SCAN FOR A = IN THE COMMAND &
	!			LINE &
	!	OUT.PUT$	NAME OF THE OUTPUT FILE &
	!	OUT.PUT%	CHANNEL NUMBER OF THE OUTPUT FILE &
	!	PG.TTL$		PAGE TITLE - USED IN THE PAGE BREAK &
	!			ROUTINE IN THE PRINT A LINE ROUTINE - &
	!			HOLDS THE PRIMARY TITLE FOR THE PAGE &
	!	PGE.CNT%	COUNT OF THE NUMBER OF PAGES PRINTED &
	!	PPN.FLG%	FLAGS WHETHER OR NOT PPN'S ARE ASSOCIATED &
	!			WITH USER-ASSIGNED LOGICALS &
	!	PRT.STG$	STRING TO PRINT IN THE PRINT A LINE &
	!			ROUTINE &
	!	R.NLUN$		STRING HOLDING THE COUNT OF ASSIGNED &
	!			LUNS &
	!	R.NLUN%		CONVERTED R.NLUN$ VALUE &
	!	REG.VAL%	HOLDER FOR A REGISTER VALUE &
	!	RTS.KEY.DCD$	DECODE STRING FOR THE RTS CONTROLLED &
	!			VALUES IN THE JOB KEYWORD &
	!	SB.TTL$		SUB-TITLE TO BE PRINTED FOR EACH &
	!			PAGE BREAK &
	!	SP.BASE%	THE INITIAL VALUE OF THE STACK &
	!			POINTER IN THE DUMPED PROGRAM &
	!	SP.LOOP%	LOOP COUNTER IN THE DUMP THE STACK RTN &
	!	SP.SAV%		VALUE OF THE STACK POINTER AT THE TIME &
	!			OF THE ABORT &
	!	TSK.FLG.DCD$	TASK FLAG WORD DECODE STRING &
	!	USR.PPN$	USED TO EXTRACT PPN'S FOR USER-ASSIGNED &
	!			LOGICALS &
	!	XRB.WRD$	A WORD FROM THE SAVED XRB &
	!	Z$		GENERAL LOCAL VARIABLE &
	!	Z%		GENERAL LOCAL VARIABLE &
	!	Z0%		GENERAL LOCAL VARIABLE &
	!	Z1%		GENERAL LOCAL VARIABLE &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E &
	!		D E S C R I P T I O N S &
	&
	&
	! SUBROUTINE 	LINES		PURPOSE &
	! &
	! PRINT A STRING &
	!		10000-10050	PRINT A LINE TO THE OUTPUT FILE, WITH &
	!				CHECKING FOR <LF> AND <FF> CHRS IN THE &
	!				TEXT - KEEPS A LINE COUNT AND A PAGE &
	!				COUNT &
	! PRINT A PAGE BREAK &
	!		10100-10150	CAUSES A PAGE BREAK IN THE OUTPUT FILE &
	! RAD50 DECODE	11000-11010	DECODES A VALUE FROM 4 BYTE STRING TO &
	!				6 BYTE STRING &
	! STRING TO OCTAL &
	!		11050-11060	DECODES A TWO BYTE STRING TO OCTAL &
	! STRING TO OCTAL AND DECIMAL &
	!		11100-11110	DECODES A TWO BYTE STRING TO OCTAL AND &
	!				DECIMAL IN THE FORM 177777 (-1.) &
	! BYTE TO OCTAL	11200-11210	DECODES A ONE BYTE STRING TO OCTAL, &
	!				RETURNING ONLY THE LOW 9 BITS &
	! BYTE TO OCTAL AND DECIMAL &
	!		11300-11310	DECODES A ONE BYTE STRING TO OCTAL AND &
	!				DECIMAL &
	! PPN DECODE	11400-11410	TURNS A TWO BYTE STRING INTO A PPN &
	!				STRING &
	! PROTECTION DECODE &
	!		11500-11510	TURNS A TWO BYTE STRING INTO A &
	!				PROTECTION STRING OF THE FORM <NNN> &
	! WORD * 32 IN OCTAL AND DECIMAL &
	!		11600-11610	TURNS A TWO BYTE STRING TO A NUMERIC &
	!				VALUE, MULTIPLIES IT BY 32, AND RETURNS &
	!				THE OCTAL AND DECIMAL VALUES.  USED &
	!				FOR CONVERSION OF LENGTHS WHICH ARE &
	!				DUMPED IN MULTIPLES OF 32 WORDS BECAUSE &
	!				OF THE CHARACTERISTICS OF RMSRTS. &
	! RTN 9		11700-11790	RESERVED FOR FUTURE IMPLEMENTATION &
	! RESIDENT LIBRARY PRINTER &
	!		11800-11890	PRINT ALL USED LIBRARY BLOCKS &
	! PRINT OUT THE LUN TABLE &
	!		12000-12020	FORMATS THE LUN TABLE FOR PRINTING &
	! PRINT OUT THE JOB KEYWORD &
	!		12100-12120	FORMATS THE JOB KEYWORD &
	! PRINT OUT IOSTS AND THE FIRQB &
	!		12200-12210	FORMATS THE JOB'S FIRQB &
	! PRINT OUT THE SAVED XRB &
	!		12300-12310	FORMATS THE JOB'S SAVED XRB &
	! PRINT CORE COMMON &
	!		12400-12410	FORMATS THE JOB'S CORE COMMON &
	! PRINT THE LOGICAL DEVICE ASSIGNMENTS &
	!		12500-12520	FORMATS THE TABLE OF LOGICAL DEVICE &
	!				ASSIGNMENTS (NOT THE LUNS) &
	! PRINT OUT SAVED REGISTERS &
	!		12600-12620	FORMATS THE REGISTERS SAVED IN THE DUMP &
	! PRINT OUT THE STACK &
	!		12700-12730	FORMATS THE STACK SAVED IN THE DUMP &
	! PRINT OUT THE TASK FLAGS &
	!		12800-12810	DECODES/FORMATS THE TASK FLAGS &
	! PRINT A BLANK LINE &
	!		12900-12910	CAUSES A BLANK LINE TO BE PRINTED &
	! PRINT A PAGE BREAK &
	!		13000-13010	CAUSES A PAGE BREAK TO BE PRINTED &
	&
	! FUNCTION	LINES		USED FOR &
	! &
	! FNOCT.AL$(OP.RND%) &
	!		15000		CONVERTS A BINARY NUMBER TO AN OCTAL &
	!				STRING &
	! FNOCT.DEC$(OP.RND%) &
	!		15100		CONVERTS A BINARY NUMBER TO AN OCTAL &
	!				AND DECIMAL STRING &
	! FNCVT%(OP.RND$) &
	!		15200		CONVERTS A 2 BYTE STRING TO BINARY &
	! FNGET.WRD%(OP.RND%) &
	!		15400-15430	RETURNS THE VALUE FROM THE INPUT FILE &
	!				OF THE WORD WHOSE ADDRESS IS OP.RND% &
	! FNBIT.DCD$(BIT.VAL%,BIT.DCD$,DCD.HDR$,DCD.TRL$) &
	!		15500-15530	RETURNS A STRING WHICH CONTAINS THE &
	!				DECODED VALUES OF THE BITS WHICH ARE &
	!				ON IN THE BIT.VAL% VARIABLE, BASED &
	!				ON THE DECODE STRING IN BIT.DCD$; &
	!				EACH DECODED VALUE IS PRECEDED IN THE &
	!				STRING BY THE STRING IN DCD.HDR$, AND &
	!				EACH IS FOLLOWED BY THE STRING IN &
	!				DCD.TRL$. &
	! FNFIL.PCK$(SPC.FIL$,DEF.FIL$,OVR.FIL$) &
	!		15600-15700	RETURNS THE FILENAME STRING WHICH &
	!				RESULTS BY OVERRIDING EACH ELEMENT &
	!				IN SPC.FIL$ WITH ANY ELEMENT WHICH &
	!				EXISTS IN OVR.FIL$, AND THEN &
	!				REPLACING ANY ELEMENT IN SPC.FIL$ &
	!				WHICH DOES NOT EXIST WITH THE SAME &
	!				ELEMENT OF DEF.FIL$, IF THAT ELEMENT &
	!				EXISTS. &
	&

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&

910	DIM M%(30%),M0%(30%)		! USED IN THE PARSE OF INPUT FILES. &
	&
	&

1000	! &
	&
	&
	!	M A I N    C O D E &
	&

1010	I$="V10.1-A" &
	\ PRIV.OFF$=SYS(CHR$(6%)+CHR$(-21%)) &
		! SET UP V/L. &

1020	PRINT IF CCPOS(0%) UNLESS E0% &
	\ Z$=SYS(CHR$(6%)+CHR$(9%)) &
	\ PRINT "PMDUMP	"+I$+"	"+CVT$$(RIGHT(Z$,3%),4%) UNLESS E0% &
	\ JOB.NO%=ASCII(Z$)/2% &
		! GET TO LEFT MARGIN UNLESS A CHAIN OR CCL ENTRY; &
		! GET THE SYSTEM NAME; &
		! PRINT THE HEADER UNLESS A CHAIN OR CCL ENTRY; &
		! TAKE THE JOB NUMBER OUT OF THE SYSTEM NAME MESSAGE. &

1030	ON ERROR GOTO 19000 &
	\ IF E0%=0% THEN &
		PRINT IF CCPOS(0%) &
	\	PRINT "#"; &
		! IF NOT A CHAIN OR CCL ENTRY, THEN &
		!	GET TO LEFT MARGIN; &
		!	PRINT A PROMPT. &

1200	NUL.STG$="" &
	\ CR.LF$=CHR$(13%)+CHR$(10%) &
	\ CHR10$=CHR$(10%) &
	\ CHR9$=CHR$(9%) &
	\ CHR12$=CHR$(12%) &
	\ CHR9.BL$=CHR9$+CHR$(32%) &
	\ CHR255$=CHR$(255%) &
		! SET UP SOME USEFUL STRING VARIABLES. &

1210	DMP.HDR$=CR.LF$+CR.LF$+SPACE$(7%) &
	\ DMP.HDR$=DMP.HDR$+SPACE$(5%)+RIGHT(FNOCT.AL$(LP.CNT%),5%) FOR &
		LP.CNT%=0% TO 14% STEP 2% &
	\ DMP.HDR$=DMP.HDR$+CR.LF$ &
		! SET UP THE HEADER FOR THE MEMORY DUMP ROUTINE. &

1220	JOB.KEY.DCD$= &
		STRING$(8%,255%)+ &
		"JFSPRI"+CHR255$+ &
		"JFFPP "+CHR255$+ &
		"JFPRIV"+CHR255$+ &
		"JFSYS "+CHR255$+ &
		"JFNOPR"+CHR255$+ &
		"JFBIG "+CHR255$+ &
		"JFLOCK"+CHR255$ &
	&
	\ RTS.KEY.DCD$= &
		STRING$(6%,255%)+ &
		"Post-Mortem Dump Enabled"+CHR255$+ &
		"Traps Enabled"+CHR255$+ &
		STRING$(7%,255%)+ &
		"No Temporary Privileges"+CHR255$ &
	&
	\ LUN.FLG.DCD$= &
		"Record Oriented Device" +CHR255$+ &
		"Carriage Control Device"+CHR255$+ &
		"Terminal Device"        +CHR255$+ &
		"Directory Device"       +CHR255$+ &
		"Single Directory Device"+CHR255$+ &
		"Sequential Device"      +CHR255$ &
	&
	\ TSK.FLG.DCD$= &
		"Task label has revision level"+CHR255$+ &
		STRING$(2%,255%)+ &
		"Image linked as supervisor-mode library"+CHR255$+ &
		CHR255$+ &
		"Memory-resident overlays in Task"+CHR255$+ &
		"Non-Checkpointable Task"+CHR255$+ &
		"Task built in compatibility mode"+CHR255$+ &
		STRING$(4%,255%)+ &
		"Post-Mortem Dump Requested"+CHR255$+ &
		CHR255$+ &
		"No Task Header allocated"+CHR255$+ &
		"Position-Independent code"+CHR255$ &
	&
	\ LIB.FLG.DCD$= &
		CHR255$+ &
		"Type = Common"+CHR255$+ &
		"Position-Independent code"+CHR255$+ &
		"Supervisor mode Library"+CHR255$+ &
		CHR255$+ &
		"Memory-resident overlays in Library"+CHR255$+ &
		CHR255$+ &
		"Default Cluster member or HISEG"+CHR255$+ &
		STRING$(5%,255%)+ &
		"Library is a cluster member"+CHR255$+ &
		"APR was reserved"+CHR255$+ &
		"Access = R/W"+CHR255$ &
	&
		! JOB KEYWORD DECODE STRING : &
		!	BITS 0-7	=> USER CONTROLLED &
		!	BIT 8		=> JFSPRI &
		!	BIT 9		=> JFFPP &
		!	BIT 10		=> JFPRIV &
		!	BIT 11		=> JFSYS &
		!	BIT 12		=> JFNOPR &
		!	BIT 13		=> JFBIG &
		!	BIT 14		=> JFLOCK &
		!	BIT 15		=> UNUSED BY MONITOR &
	&
		! RTS CONTROLLED BITS IN THE JOB KEYWORD : &
		!	BITS 0-6	=> USER CONTROLLED &
		!	BIT 7		=> POST-MORTEM DUMP ENABLED &
		!	BIT 8		=> TRAPS ENABLED &
		!	BITS 9-14	=> SYSTEM KEYWORD FLAGS &
		!	BIT 15		=> NO TEMPORARY PRIVILEGES IN EFFECT &
		!			   WHEN JOB WAS DUMPED &
		!	(NOTE : IF THESE THREE ARE NOT ON, NO DUMP WILL BE &
		!	PERFORMED, ANYWAY.) &
	&
		! LUN FLAG DECODE STRING : &
		!	BIT 0		=> RECORD ORIENTED DEVICE &
		!	BIT 1		=> CARRIAGE CONTROL DEVICE &
		!	BIT 2		=> TERMINAL TYPE DEVICE &
		!	BIT 3		=> DIRECTORY TYPE DEVICE &
		!	BIT 4		=> SINGLE DIRECTORY TYPE DEVICE &
		!	BIT 5		=> SEQUENTIAL DEVICE &
		!	BITS 6-15	=> UNUSED &
	&
		! TASK FLAG DECODE STRING : &
		!	BIT 0		=> Task label block uses new format &
		!	BIT 1		=> Task has a header &
		!	BIT 2		=> Task built with external header &
		!	BIT 3		=> Task is supervisor-mode library &
		!	BIT 4		=> Prived task doesn't map I/O page &
		!	BIT 5		=> TASK HAS MEMORY RESIDENT OVERLAYS &
		!	BIT 6		=> TASK IS NOT CHECKPOINTABLE &
		!	BIT 7		=> TASK IS BUILT IN COMPATIBILITY MODE &
		!	BIT 8		=> Task is privileged &
		!	BIT 9		=> UNUSED &
		!	BIT 10		=> No SEND can go to task &
		!	BIT 11		=> Task can be slaved &
		!	BIT 12		=> POST-MORTEM DUMP REQUESTED &
		!	BIT 13		=> Task is ACP &
		!	BIT 14		=> TASK HAS NO HEADER &
		!	BIT 15		=> Task is POSITION-INDEPENDENT CODE &
	&
		! Library FLAG DECODE STRING : &
		!	BIT 1		=> Common if set / Library of clear &
		!	BIT 2		=> POSITION-INDEPENDENT CODE &
		!	BIT 3		=> Supervisor mode library &
		!	BIT 4		=> Unused &
		!	BIT 5		=> Lib has memory resident overlays &
		!	BITS 6-12	=> Unused &
		!	BIT 13		=> Library is part of a cluster &
		!	BIT 14		=> APR was reserved &
		!	BIT 15		=> R/W is set / R/O if clear &
	&

1240	IN.PUT%=1% &
	\ OUT.PUT%=2% &
		! SET UP SOME USEFUL NUMERIC CONSTANTS. &

2000	! &
	&
	&
	!	G E T    T H E    I N P U T    A N D    O U T P U T &
	!		F I L E S    S E T    U P &
	&

2010	Z%=FNCVT%(MID(SYS(CHR$(6%)+CHR$(14%)+STRING$(6%,0%)+CHR$(1%)),7%,2%)) &
	\ ACCT$="["+NUM1$(SWAP%(Z%) AND 255%)+","+NUM1$(Z% AND 255%)+"]" &
	\ DEF.INP$="SY:"+ACCT$+"PMD"+RIGHT(NUM1$(JOB.NO%+1000%),2%)+".PMD" &
	&
	\ GOTO 2015 IF E0% &
	\ OPEN "_KB:COMMND.CMD" FOR INPUT AS FILE 3% &
	\ INPUT LINE #3%,C$ &
	\ PRINT #3% IF CCPOS(3%) UNLESS (STATUS AND 255%)<>2% &
	\ CLOSE 3% &
		! SET UP THE DEFAULT INPUT AND THE DEFAULT OUTPUT NAMES; &
		! IF THIS IS A 'RUN' ENTRY, THEN &
		!	OPEN THE KB: AS A COMMAND FILE; &
		!	GET A COMMAND LINE; &
		!	CLOSE THE COMMAND FILE. &

2015	C$=CVT$$(C$,2%+4%+32%) &
	\ C$="_KB:="+C$ UNLESS INSTR(1%,C$,"=") &
		! MAKE THE DEFAULT OUTPUT BE _KB: INSTEAD OF _SY:*.LST &
	\ DISK.DUMP%=INSTR(1%,C$,"/T")			! /T[SK] &
	\ IF DISK.DUMP% THEN &
		I%=DISK.DUMP% &
	\	J%=INSTR(I%+2%,C$+"/","/") &
	\	C$=LEFT(C$,I%-1%)+RIGHT(C$,J%) &
	! STRIP OFF THE "/TSK" SWITCH AND SET DISK.DUMP% IF SPECIFIED &

2016	WIDE%=INSTR(1%,C$,"/W")				! /W[IDE] &
	\ IF WIDE% THEN &
		I%=WIDE% &
	\	J%=INSTR(I%+2%,C$+"/","/") &
	\	C0GL$=STRING$(32%,46%) &
			+' !"#$%&'+"'()*+,-./0123456789:;<=>?" &
			+"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_" &
			+"`abcdefghijklmnopqrstuvwxyz{|}~." &
	\	C1GR$="" &
	\	C1GR$=C1GR$+CHR$(CH.8BIT%) FOR CH.8BIT%=161% TO 254% &
	\	C1GR$=STRING$(32%,46%)+" "+C1GR$+STRING$(1%,46%) &
	\	X.LATE$=C0GL$+C1GR$ &
	\	C$=LEFT(C$,I%-1%)+RIGHT(C$,J%) &
		! LOOK FOR "/WIDE" TO SIGNIFY AN ASCII DUMP AS WELL &
		! SUPPORT FULL ASCII PLUS EIGHT BIT CHARACTER SET &

2017	START.OFFSET%=INSTR(1%,C$,"/S")			! /S[TART]:NNNNNN &
	\ IF START.OFFSET% THEN &
		I%=START.OFFSET% &
	\	M%=INSTR(I%,C$,":")+1% &
	\	J%=INSTR(M%,C$+"/","/") &
	\	START.OFFSET%=FNDECIMAL.VALUE%(MID(C$,M%,J%-M%)) AND -256% &
	\	C$=LEFT(C$,I%-1%)+RIGHT(C$,J%) &
		! LOOK FOR "/START:nnnnnn" TO SPECIFY STARTING OFFSET &

2018	END.OFFSET%=INSTR(1%,C$,"/E")			! /E[ND]:NNNNNN &
	\ IF END.OFFSET% THEN &
		I%=END.OFFSET% &
	\	M%=INSTR(I%,C$,":")+1% &
	\	J%=INSTR(M%,C$+"/","/") &
	\	END.OFFSET%=(FNDECIMAL.VALUE%(MID(C$,M%,J%-M%))+255%) AND -256% &
	\	C$=LEFT(C$,I%-1%)+RIGHT(C$,J%) &
		! LOOK FOR "/END:nnnnnn" TO SPECIFY ENDING OFFSET &

2020	C$=CVT$$(C$,-2%) &
	\ OUT.FND%=INSTR(1%,C$,"=") &
	\ OUT.PUT$=LEFT(C$,OUT.FND%-1%) &
	&
	\ IN.PUT$=RIGHT(C$,OUT.FND%+1%) &
	\ IN.PUT$=FNFIL.PCK$(IN.PUT$,DEF.INP$,NUL.STG$) &
	\ OUT.PUT$=FNFIL.PCK$(OUT.PUT$,FNFIL.PCK$(IN.PUT$,NUL.STG$, &
		"_SY:"+ACCT$+".LST"),NUL.STG$) &
		! CLEAR OUT ANY UNNECESSARY STUFF FROM THE INPUT STRING; &
		! FIND ANY =; &
		! THE OUTPUT IS TO THE LEFT OF THE =, INPUT TO THE RIGHT; &
		! CREATE THE INPUT FILE NAME, USING THE DEFAULT STRING AS &
		!  THE DEFAULT AND NO OVERRIDE; &
		! CREATE THE OUTPUT FILENAME, USING THE INPUT FILE SPEC, &
		!  WITH ANY EXTENSION REPLACED BY '.LST', AS THE DEFAULT, &
		!  NO OVERRIDE. &

2030	DISK.DUMP%=DISK.DUMP%+INSTR(1%,IN.PUT$,".TSK")+INSTR(1%,IN.PUT$,".SIL") &
		! TO PRINT OUT .TSK AND .SIL IMAGES AS DISK DUMPS &

2500	OPEN IN.PUT$ FOR INPUT AS FILE IN.PUT% &
	\ OPEN OUT.PUT$ AS FILE OUT.PUT% &
		! OPEN THE NECESSARY FILES. &

2510	ONES.OFFSET%=1% &
	\ IF DISK.DUMP% THEN &
		GET #IN.PUT% &
	\	FIELD #IN.PUT%, 2% AS MODULE$, 508% AS FILLER$, 2% AS SIL$ &
	\	ONES.OFFSET%=2% IF RAD$(FNCVT%(SIL$))="SIL" &
					AND FNCVT%(MODULE$)=1% &
		! CHECK FOR THE .TSK REALLY BEING A "SIL", AND ALLOW &
		!  FOR SIL HEADER. &

2900	PG.TTL$="Post-Mortem" UNLESS DISK.DUMP% &
	\ PG.TTL$="Task-Image" IF DISK.DUMP% &
	\ PG.TTL$=PG.TTL$+" Dump of "+IN.PUT$+" on "+DATE$(0%)+" at "+TIME$(0%) &
	\ Z%=(70%-LEN(PG.TTL$))/2% &
	\ Z%=1% UNLESS Z% &
	\ PG.TTL$=CHR12$+STRING$(Z%,32%)+PG.TTL$+STRING$(Z%,32%)+"Page " &
	&

3000	! &
	&
	&
	!	F O R M A T    F I R S T    B L O C K &
	&

3005	ID.VAL%=FNGET.WRD%(21%) AND 255% &
	\ TSK.FLAG%=FNGET.WRD%(24%) &
	\ HDR.BLK%=FNGET.WRD%(FNID%(250%))	! D space header &
	\ D.TSK%=0%				! not D unless... &
	\ D.TSK%=-1% IF HDR.BLK%		! Flag if /ID &
	\ HDR.BLK%=FNGET.WRD%(FNID%(238%)) UNLESS HDR.BLK% &
	\ R.NLUN%=FNGET.WRD%(FNID%(242%)) &
	\ GOTO 4000 IF START.OFFSET% OR END.OFFSET% &
		! Get the system I/D value - to properly offset &
		! Get the task flag word in a handy place &
		! Find the real header (I or D) in case we need it &
		! Find out how may LUNs there are in the task &
		! SKIP FORMATTED TASK HEADER IF ASKING FOR SELECTED RANGE &

3020	SB.TTL$=STRING$(26%,32%)+"Formatted Dump of " &
	\ SB.TTL$=SB.TTL$+"Low Memory" UNLESS DISK.DUMP% &
	\ SB.TTL$=SB.TTL$+"Task Header" IF DISK.DUMP% &
	\ GOSUB 10100 &
	\ GOTO 19000 IF E% &
	\ RESTORE &
	\ Z$=NUL.STG$ &
	\ READ Z$ UNTIL Z$="*STARTFORMAT" UNLESS DISK.DUMP% &
	\ READ Z$ UNTIL Z$="*STARTDISKFORMAT" IF DISK.DUMP% &
		! PRINT OUT THE HEADING ON THE FIRST PAGE; &
		! EXIT IF ERROR; &
		! FIND THE BEGINNING OF THE FORMAT LIST. &

3030	READ ITM.BEG%,ITM.LEN%,ITM.HDR$,ITM.DCD%,ITM.TRL$ &
	\ GOTO 3100 IF ITM.HDR$="*ENDFORMAT" &
	\ ITM.BEG%=FNID%(ITM.BEG%) IF DISK.DUMP% &
	\ GOTO 3030 if TSK.FLAG% and 16384% if ITM.BEG%>=1024% if DISK.DUMP% &
		! Skip this one if it's the header and there isn't one. &
	\ ITM.BEG%=ITM.BEG%+((HDR.BLK%-2%)*512%) IF ITM.BEG%>=1024% &
		IF DISK.DUMP%	! Adjust to where the header really is. &
	\ Z%=FNGET.WRD%(ITM.BEG%) IF ITM.LEN% &
	\ FIELD #IN.PUT%,NXT.BYT% AS ITM.RAW$,ITM.LEN% AS ITM.RAW$ &
	\ ON ITM.DCD% GOSUB &
		11000,11050,11100,11200,11300,11400,11500,11600,11700,11800, &
		12000,12100,12200,12300,12400,12500,12600,12700,12800,12900, &
		13000,11900 &
	\ PRT.STG$=ITM.HDR$+ITM.VAL$+ITM.TRL$+CR.LF$ &
	\ GOSUB 10000 &
	\ ITM.HDR$,ITM.VAL$,ITM.TRL$=NUL.STG$ &
	\ GOTO 19000 IF E% &
	\ GOTO 3030 &
		! GET THE FIRST BLOCK IF WE DON'T ALREADY HAVE IT; &
		! READ THE FORMAT INFORMATION FOR THE NEXT ITEM; &
		! EXIT IF END OF FORMAT LIST; &
		! CORRECT FOR THE EXTRA LIB BLOCKS FOR M+ (IF M+) &
		! Correctly adjust if no header block &
		! REMEMBER WHAT THE SYSTEM ID BYTE IS &
		! FIELD THE NEW ITEM; &
		! DISPATCH TO THE PROPER DECODE ROUTINE; &
		! SET UP AND PRINT THE STRING; &
		! PROCESS ANY ERROR; &
		! GO LOOK FOR NEXT ITEM. &
	&
	&

3900	DATA	"*STARTFORMAT" &

3910	! BEGIN POS	LENGTH	HEADER	DECODE			TRAILER &

3920	DATA	0,	4,	"Task Name      : ", &
					1,			"", &
		4,	4,	"Partition Name : ", &
					1,			"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,			"", &
		8,	2,	"Task Size (without Extension) : ", &
					8,		" words", &
		28,	2,	"Load Size of Task             : ", &
					8,		" words", &
		30,	2,	"Current Task Size             : ", &
					8,		" words", &
		0,	0,	"PRINT A BLANK LINE", &
					20,			"", &
		10,	2,	"ODT SST Vector Address     : ", &
					2,			"", &
		12,	1,	"ODT SST Vector Length      :    ", &
					5,			"", &
		14,	2,	"Task SST Vector Address    : ", &
					2,			"", &
		16,	1,	"Task SST Vector Length     :    ", &
					5,			"", &
		18,	2,	"FPP AST Service Address    : ", &
					2,			"", &
		20,	2,	"CTRL/C AST Service Address : ", &
					2,			"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,			"", &
		22,	2,	"TASK FLAGS", &
					19,			"", &
		24,	2,	"User Parameter on Entry : ", &
					2,			"", &
		26,	2,	"CCL Entry Flags         : ", &
					2,			"", &
		38,	2,	"Directive Status Word   : ", &
					2,			"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,			"", &
		40,	2,	"FCS Impure Area Pointer       : ", &
					2,			"", &
		42,	2,	"OTS Impure Area Pointer       : ", &
					2,			"", &
		44,	2,	"Auto-Load Impure Area Pointer : ", &
					2,			"", &
		46,	2,	"Extended Impure Area Pointer  : ", &
					2,			"", &
		0,	0,	"GET TO TOP OF FORM", &
					21,			"", &
		128,	64,	"LOGICAL UNIT NUMBER TABLE", &
					11,			"", &
		0,	0,	"GET TO TOP OF FORM", &
					21,			"", &
		256,	2,	"JOB KEYWORD", &
					12,			"", &
		258,	32,	"FIRQB", &
					13,			"", &
		434,	16,	"SAVED XRB", &
					14,			"", &
		304,	128,	"CORE COMMON", &
					15,			"", &
		0,	0,	"GET TO TOP OF FORM", &
					21,			"", &
		476,	2,	"User Assignable PPN : ", &
					6,			"", &
		478,	2,	"User Default Protection : ", &
					7,			"", &
		480,	16,	"USER LOGICAL NAME TABLE", &
					16,			"", &
		0,	0,	"GET TO TOP OF FORM", &
					21,			"", &
		472,	2,	"Initial PC : ", &
					2,			"", &
		474,	2,	"Initial PS : ", &
					2,			"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,			"", &
		432,	2,	"POINTER TO SAVED REGISTERS", &
					17,			"", &
		470,	2,	"INITIAL SP", &
					18,			"" &

3940	DATA	0,	0,	"*ENDFORMAT", &
					0,			"" &
	&

3950	DATA	"*STARTDISKFORMAT" &

3960	! BEGIN POS	LENGTH	HEADER	DECODE			TRAILER &
	! NOTE: IN ORDER TO DUMP M+ TASKS, THE SYSTEM ID BYTE &
	!	MUST BE EXAMINED BEFORE ANY DATA WHICH COMES AFTER &
	!	THE LIBRARY BLOCKS. &

3970	DATA	0,	4,	"Task Name      : ", &
					1,		"", &
		4,	4,	"Partition Name : ", &
					1,		"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		8,	2,	"Base Address of Task          : ", &
					2,		"", &
		10,	2,	"Highest Window 0 Virtual Addr : ", &
					2,		"", &
		12,	2,	"Highest Virtual Address       : ", &
					2,		"", &
		14,	2,	"Load size                     : ", &
					8,		" words", &
		16,	2,	"Max size                      : ", &
					8,		" words", &
		18,	2,	"Task Offset into Partition    : ", &
					3,		"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		20,	1,	"Number of Task Windows        : ", &
					5,		"", &
		21,	1,	"System ID                     : ", &
					5,		"", &
		22,	2,	"Size of Overlay Segment Desc  : ", &
					3,		" bytes", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		24,	2,	"Task Flag Word                : ", &
					19,		"", &
		26,	2,	"Task Creation Date - Year     : ", &
					3,		"", &
		28,	2,	"Task Creation Date - Month    : ", &
					3,		"", &
		30,	2,	"Task Creation Date - Day      : ", &
					3,		"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		32,	4,	"DO THE LIB BLOCKS", &
					10,		"", &
		230,	2,	"Task Priority                 : ", &
					3,		"", &
		232,	2,	"Task Transfer Address         : ", &
					2,		"", &
		234,	2,	"Task Extension Size           : ", &
					8,		" words", &
		236,	2,	"Block Number of Segment List  : ", &
					3,		"", &
		238,	2,	"Block Number of Header        : ", &
					3,		"", &
		240,	2,	"Number of Blocks in Label     : ", &
					3,		"", &
		242,	2,	"Number of Logical Units       : ", &
					3,		"", &
		250,	2,	"Block Number of Data Header   : ", &
					3,		"", &
		254,	2,	"Highest Data Virtual Address  : ", &
					2,		"", &
		256,	2,	"Data Load size                : ", &
					8,		" words", &
		258,	2,	"Data Max size                 : ", &
					8,		" words", &
		260,	2,	"D space APR mask word         : ", &
					2,		"", &
		506,	2,	"Second task flags word        : ", &
					2,		"", &
		508,	2,	"Label block revision level    : ", &
					2,		"", &
		0,	0,	"GET TO TOP OF FORM", &
					21,		"", &
		512,	2,	"LOGICAL UNIT NUMBER TABLE", &
					22,		"", &
		1025,	0,	"GET TO TOP OF FORM", &
					21,		"", &
		1024,	2,	"Current Stack Pointer (R6)    : ", &
					2,		"", &
		1026,	2,	"Header length                 : ", &
					3,		"", &
		1028,	2,	"Event Flag mask               : ", &
					2,		"", &
		1030,	2,	"Event Flag address            : ", &
					2,		"", &
		1032,	2,	"Current UIC                   : ", &
					6,		"", &
		1034,	2,	"Default UIC                   : ", &
					6,		"", &
		1036,	2,	"Initial PS                    : ", &
					2,		"", &
		1038,	2,	"Initial PC (R7)               : ", &
					2,		"", &
		1040,	2,	"Initial Stack Pointer (R6)    : ", &
					2,		"", &
		1042,	2,	"ODT SST Vector Address        : ", &
					2,		"", &
		1044,	2,	"ODT SST Vector Length         : ", &
					3,		"", &
		1046,	2,	"Task SST Vector Address       : ", &
					2,		"", &
		1048,	2,	"Task SST Vector Length        : ", &
					3,		"", &
		1052,	2,	"FPP AST Service Address       : ", &
					2,		"", &
		1054,	2,	"CTRL/C AST Service Address    : ", &
					2,		"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		1060,	2,	"Pointer to number of Windows  : ", &
					2,		"", &
		1062,	2,	"Directive Status Word         : ", &
					2,		"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		1064,	2,	"FCS Impure Area Pointer       : ", &
					2,		"", &
		1066,	2,	"OTS Impure Area Pointer       : ", &
					2,		"", &
		1068,	2,	"Auto-load Impure Area Pointer : ", &
					2,		"", &
		1070,	2,	"Extended Impure Area Pointer  : ", &
					2,		"", &
		0,	0,	"PRINT A BLANK LINE", &
					20,		"", &
		1082,	2,	"Header guard word Pointer     : ", &
					2,		"", &
		1084,	2,	"Number of LUNS                : ", &
					3,		"" &

3990	DATA	0,	0,	"*ENDFORMAT", &
					0,			"" &

3991		! THE DATA IN THE ABOVE GIVES : &
		!	BEGIN POS	- THE POSITION IN THE BUFFER AT WHICH &
		!			  THE ITEM BEGINS &
		!	LENGTH		- THE LENGTH OF THE (RAW) ITEM &
		!	HEADER		- THE TEXT TO BE PRINTED OUT IN FRONT &
		!			  OF THE ITEM &
		!	DECODE		- THE DECODE ROUTINE TO USE ON THIS &
		!			  ITEM : &
		!				1  => RAD50 DECODE (2 WORDS) &
		!				2  => OCTAL DECODE &
		!				3  => OCTAL DECODE WITH DECIMAL &
		!				      VALUE IN PARENTHESIS &
		!				4  => OCTAL DECODE OF A BYTE &
		!				5  => OCTAL DECODE OF A BYTE &
		!				      WITH DECIMAL VALUE IN &
		!				      PARENTHESES &
		!				6  => PPN DECODE &
		!				7  => PROTECTION DECODE &
		!				8  => MULTIPLY BY 64 AND PRINT &
		!				      OCTAL AND DECIMAL &
		!				9  => RESERVED &
		!				10 => LIB TABLE &
		!				11 => LUN TABLE (image) &
		!				12 => JOB KEYWORD &
		!				13 => IOSTS/FIRQB &
		!				14 => SAVED XRB &
		!				15 => CORE COMMON &
		!				16 => USER LOGICAL NAME TABLE &
		!				17 => SAVED REGISTER LIST &
		!				18 => CURRENT STACK &
		!				19 => TASK FLAGS &
		!				20 => PRINT A BLANK LINE &
		!				21 => GET TO TOP OF FORM &
		!				22 => LUN Table (Task file) &
		! &
		!	TRAILER		- STRING TO BE PRINTED OUT AFTER THE &
		!			  ITEM &
		! &
		! EACH ITEM GETS PRINTED ON ITS OWN LINE. &

3100	Z%=FNGET.WRD%(FNID%(236%)) &
	\ GOTO 4000 UNLESS Z%		! Do this if there's a segment list &
	\ ITM.HDR$=CR.LF$+"  Segment Load List :"+CR.LF$+CR.LF$ &
	\ Z%=FNGET.WRD%(Z%*512%)	! Get the start of the block &
	\ PRT.STG$="Length of Root segment" &
	\ FOR LP.CNT%=0% to 256% &
	\	PRT.STG$="Length of segment #"+NUM1$(LP.CNT%)+ &
			string$(3%-log10(LP.CNT%+.01),32%) IF LP.CNT% &
	\	FIELD #IN.PUT%, LP.CNT%*2% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\	GOSUB 11100 &
	\	GOTO 3110 IF ITM.RAW%=0% &
	\	PRT.STG$=ITM.HDR$+PRT.STG$+" : "+ITM.VAL$+" bytes"+CR.LF$ &
	\	GOSUB 10000 &
	\	GOTO 3110 IF E% &
	\	ITM.HDR$=NUL.STG$ &
	\ NEXT LP.CNT% &

3110	! &

4000	! &
	&
	&
	!	D U M P    T H E    F I L E ,    I N    W O R D S &
	&

4010	SB.TTL$=STRING$(29%,32%) &
	\ SB.TTL$=SB.TTL$+"Memory" UNLESS DISK.DUMP% &
	\ SB.TTL$=SB.TTL$+"Disk Image" IF DISK.DUMP% &
	\ SB.TTL$=SB.TTL$+" Dump, in Octal" &
	\ GOSUB 10100 &
	\ LP.CNT%,LP.OFF%,I.SIZE%,S.TEP%=0% &
	\ GOTO 4015 UNLESS DISK.DUMP% &
	\ TSK.BASE%=FNGET.WRD%(8%) &
	\ TSK.LABEL%=FNGET.WRD%(FNID%(240%)) &
	\ LP.CNT%=TSK.BASE%			! Get base address &
	\ LP.CNT%=512% UNLESS (TSK.FLAG% AND 16384%)	! 1000 if /HD &
	\ LP.OFF%=TSK.LABEL%*512% &
	\ LP.OFF%=LP.OFF%+512% UNLESS (TSK.FLAG% AND 16384%) ! +1000 if /HD &
	\ LP.OFF%=LP.OFF%-LP.CNT% &
	\ IF D.TSK% THEN OVR.STRT%=FNGET.WRD%(FNID%(256%))*64% ELSE &
		S.TEP%=1% &
	\	OVR.STRT%=FNGET.WRD%(14%)*64% &

4015	TEMP$=SYS(CHR$(0%)) ! CHR$(OUT.PUT%) &
	\ LP.CNT%=START.OFFSET% IF START.OFFSET% &
	\ PRT.STG$=NUL.STG$ &
	\ WHILE E%=0% &
	\	Z%=FNGET.WRD%(LP.CNT%+LP.OFF%) &
	\	GOTO 4020 IF E% &
	\	PRT.STG$=PRT.STG$+DMP.HDR$ &
	\	GOSUB 10000 &
	\	LP1.CNT% = LP.CNT% AND 511% &
	\	FOR LP3.CNT%=LP1.CNT% STEP 16% &
				WHILE (LP3.CNT%<LP1.CNT% + 256%) AND (E%=0%) &
	\		ITM.VAL$=FNOCT.AL$(LP.CNT%+LP3.CNT%-LP1.CNT%- &
							I.SIZE%)+" / " &
	\		TEMP.LINE$="" IF WIDE% &
	\		FOR LP2.CNT%=0% TO 14% STEP 2% &
	\			FIELD #IN.PUT%, LP2.CNT%+LP3.CNT% AS ITM.RAW$, &
						2% AS ITM.RAW$ &
	\			ITM.VAL$=ITM.VAL$+" "+FNOCT.AL$ &
					(FNCVT%(ITM.RAW$)) &
	\			TEMP.LINE$=TEMP.LINE$+ITM.RAW$ IF WIDE% &
	\		NEXT LP2.CNT% &
	\		ITM.VAL$=ITM.VAL$+"     "+XLATE( &
				TEMP.LINE$,X.LATE$) IF WIDE% &
	\		PRT.STG$=ITM.VAL$+CR.LF$ &
	\		GOSUB 10000 &
	\	NEXT LP3.CNT% &
	\	PRT.STG$=NUL.STG$ &
	\	Z%=LP.CNT%+LP3.CNT%-LP1.CNT%-I.SIZE% &
	\	ON S.TEP%+1% GOSUB 6000,6100,6200 IF DISK.DUMP% &
	\	LP.CNT%=LP.CNT%+256% &
	\	GOTO 4020 IF FNUNSIGNED(END.OFFSET%)<=FNUNSIGNED(LP.CNT%+ &
			LP.OFF%) IF END.OFFSET% &
	\	GOSUB 10100 IF LNE.CNT%>50% &
	\ NEXT &
		! SET UP OUR CURRENT POSITION POINTER AND OUR BLOCK COUNTER; &
		! WHILE NO ERROR IS DETECTED (THE LOOP WILL END ON EOF, AT &
		!  LEAST); &
		!	SET UP THE HEADER; &
		!	LOOP FOR 16 LINES; &
		!		SET UP THE CURRENT ADDRESS; &
		!		APPEND ON 8 WORDS; &
		!		PRINT THE LINE; &
		!	CONTINUE LOOP FOR A BLOCK; &
		!	INCREMENT THE CURRENT POSITION POINTER; &
		! NEXT BLOCK. &

4020	IF E% AND E%<>11% THEN &
		GOTO 19000 &
	ELSE	GOTO 5000 &
		! IF THE ERROR WAS NOT EOF, THEN &
		!	PRINT IT AS A FATAL ERROR; &
		! ELSE	GO CLEAN UP. &
	&

5000	! &
	&
	&
	!	S H U T D O W N &
	&

5980	CLOSE IN.PUT%,OUT.PUT% &
	\ GOTO 32767 IF E0% OR ((E%=11%) AND (ERL=2010%)) &
		! ENSURE A CLOSE; &
		! EXIT IF CCL ENTRY. &
	\ E%,PGE.CNT%=0% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT CR.LF$+"#"; &
	\ GOTO 2010 &
		! IF NOT A CHAIN OR CCL ENTRY, THEN &
		!	reset old errors, and page count. &
		!	GET TO LEFT MARGIN; &
		!	PRINT A PROMPT. &
		!	and go ask for more &

6000	! Come here for step 0 - Check for D Root &
	! &
	IF Z% >= (HDR.BLK%-TSK.LABEL%)*512% THEN I.SIZE%=Z% &
	\	I.SIZE%=Z%-512% IF TSK.FLAG% AND 16384% &
	\	PRT.STG$=CR.LF$+CR.LF$+"Start of Root 'D' portion of "+ &
			"the task" &
	\	PRT.STG$=PRT.STG$+" (including 1 block header)" &
			UNLESS TASK.FLAG% AND 16384% &
	\	PRT.STG$=PRT.STG$+CR.LF$ &
	\	S.TEP%=1%	! done with D root step &

6090	RETURN &

6100	! Come here for step 1 - Check for Overlays &
	IF Z% >= (OVR.STRT% OR 511%) THEN I.SIZE%=0% &
	\	I.SIZE%=Z% IF D.TSK% &
	\	PRT.STG$=CR.LF$+CR.LF$+"Start of Overlay portions of "+ &
			"the task"+CR.LF$ &
	\	S.TEP%=2% &

6200	RETURN &

10000	! &
	&
	&
	!	P R I N T    A    S T R I N G    T O    T H E    O U T P U T &
	!				D E V I C E &
	&

10010	CUR.POS%,BEG.POS%=1% &
	\ END.POS%=LEN(PRT.STG$) &
		! SET UP THE 'CURRENT POSITION', 'BEGIN SCAN POSITION', AND &
		!  'END POSITION' POINTERS. &

10020	FF.POS%=INSTR(CUR.POS%,PRT.STG$,CHR12$) AND E%=0% &
	\ IF FF.POS% THEN &
		END.POS%=FF.POS%-1% &
	\	GOSUB 10030 &
	\	GOSUB 10100 &
	\	END.POS%=LEN(PRT.STG$) &
	\	BEG.POS%=FF.POS%+1% &
	\	GOTO 10020 UNLESS E% &
		! SCAN FOR A FORM FEED UNLESS ERROR; &
		! IF FOUND, THEN &
		!	SET UP THE 'END POSITION' TO POINT TO THE CHARACTER &
		!	 BEFORE THE FORM FEED; &
		!	PROCESS THE PRECEDING PART OF THE STRING NORMALLY; &
		!	DO A TOP OF PAGE; &
		!	RESTORE THE 'END POSITION' AND SET THE NEW 'BEGIN &
		!	 POSITION' TO THE CHARACTER AFTER THE FORM FEED; &
		!	GO CHECK FOR ANOTHER FORM FEED UNLESS SOME ERROR WAS &
		!	 FOUND. &

10030	LF.POS%=INSTR(CUR.POS%,PRT.STG$,CHR10$) AND E%=0% &
	\ IF LF.POS% THEN &
		CUR.POS%=LF.POS%+1% &
	\	LNE.CNT%=LNE.CNT%+1% &
	\	GOTO 10030 UNLESS LNE.CNT%>64% &
	\	ON ERROR GOTO 10150 &
	\	PRINT #OUT.PUT%,MID(PRT.STG$,BEG.POS%,CUR.POS%-BEG.POS%-1%); &
	\	BEG.POS%=CUR.POS% &
	\	GOSUB 10100 &
	\	GOTO 10030 UNLESS E% &
		! NEXT, SCAN FOR LINE FEED UNLESS ERROR; &
		! IF FOUND, THEN &
		!	SET THE 'CURRENT POSITION' TO THE CHARACTER PAST THE &
		!	 LINE FEED; &
		!	INCREMENT THE LINE COUNT; &
		!	GO LOOK AGAIN IF THE LINE COUNT IS LESS THAN A PAGE &
		!	 LENGTH (64 LINES PER PAGE); &
		!	SET UP THE ERROR TRAP; &
		!	PRINT OUT THE PART BETWEEN THE BEGINNING POINTER AND &
		!	 THE LINE FEED (INCLUDING THE LINE FEED); &
		!	UPDATE THE BEGIN POSITION SO THE ALREADY PRINTED PART &
		!	 IS NOT RE-PRINTED; &
		!	DO A PAGE BREAK; &
		!	GO BACK TO LOOK FOR MORE LINE FEED CHARACTERS UNLESS &
		!	 SOME ERROR OCCURRED. &

10040	ON ERROR GOTO 10150 &
	\ PRINT #OUT.PUT%,MID(PRT.STG$,BEG.POS%,END.POS%-BEG.POS%+1%); UNLESS &
		E% &
		! PRINT OUT WHATEVER REMAINS. &

10050	ON ERROR GOTO 19000 &
	\ RETURN &
		! RESET ERROR TRAP; &
		! EXIT. &

10100	! &
	&
	&
	!	P R I N T    A    P A G E    B R E A K &
	&

10110	ON ERROR GOTO 10150 &
	\ PGE.CNT%=PGE.CNT%+1% &
	\ PRINT #OUT.PUT%,PG.TTL$+NUM1$(PGE.CNT%)+CR.LF$+SB.TTL$+CR.LF$+CR.LF$ &
	\ LNE.CNT%=5% &
		! SET UP THE ERROR TRAP; &
		! INCREMENT THE PAGE COUNT SO THE FIRST TIME THROUGH IT IS &
		!  PRINTED AS "1"; &
		! PRINT OUT THE TITLE LINE, WITH PAGE NUMBER; &
		! RESET THE LINE COUNTER FOR THIS PAGE. &

10120	ON ERROR GOTO 19000 &
	\ RETURN &
		! RESET THE ERROR TRAP; &
		! EXIT. &

10150	E%=ERR &
	\ RESUME 10120 &
		! SET UP THE ERROR FLAG; &
		! RESUME AT SUBROUTINE EXIT. &

11000	! &
	&
	&
	!	R A D 5 0    D E C O D E &
	!		( R T N    # 1 ) &
	&

11010	ITM.VAL$=RAD$(FNCVT%(ITM.RAW$))+RAD$(FNCVT%(RIGHT(ITM.RAW$, &
		3%))) &
	\ RETURN &
		! RAD50 DECODE. &

11050	! &
	&
	&
	!	S T R I N G    T O    O C T A L &
	!		( R T N    # 2 ) &
	&
	&

11060	ITM.VAL$=FNOCT.AL$(FNCVT%(ITM.RAW$)) &
	\ RETURN &
		! CHANGE A TWO BYTE STRING INTO ITS OCTAL VALUE. &

11100	! &
	&
	&
	!	S T R I N G    T O    O C T A L    A N D    D E C I M A L &
	!		( R T N    # 3 ) &
	&

11110	ITM.RAW%=FNCVT%(ITM.RAW$) &
	\ ITM.VAL$=FNOCT.DEC$(ITM.RAW%) &
	\ RETURN &
		! CHANGE A TWO BYTE STRING INTO ITS OCTAL VALUE AND APPEND ON &
		!  THE DECIMAL VALUE IN PARENS FOLLOWED BY '.'. &

11200	! &
	&
	&
	!	B Y T E    T O    O C T A L &
	!		( R T N    # 4 ) &
	&

11210	ITM.RAW%=ASCII(ITM.RAW$) &
	\ ITM.VAL$=RIGHT(FNOCT.AL$(ITM.RAW%),4%) &
	\ RETURN &
		! CHANGE A ONE BYTE STRING INTO ITS OCTAL VALUE (USING ONLY THE &
		!  LAST THREE DIGITS). &

11300	! &
	&
	&
	!	B Y T E    T O    O C T A L    A N D    D E C I M A L &
	!		( R T N    # 5 ) &
	&

11310	ITM.RAW%=ASCII(ITM.RAW$) &
	\ ITM.VAL$=RIGHT(FNOCT.DEC$(ITM.RAW%),4%) &
	\ RETURN &
		! CHANGE A ONE BYTE STRING INTO ITS OCTAL VALUE AND APPEND ON &
		!  THE DECIMAL VALUE IN PARENS FOLLOWED BY '.'. &

11400	! &
	&
	&
	!	P P N    D E C O D E &
	!		( R T N    # 6 ) &
	&

11410	ITM.RAW%=FNCVT%(ITM.RAW$) &
	\ ITM.VAL$="["+NUM1$(SWAP%(ITM.RAW%) AND 255%)+","+NUM1$(ITM.RAW% AND &
		255%)+"]" IF ITM.RAW% &
	\ ITM.VAL$="<None>" UNLESS ITM.RAW% &
	\ RETURN &
		! CHANGE A TWO BYTE STRING INTO ITS PPN EQUIVALENT STRING. &

11500	! &
	&
	&
	!	P R O T E C T I O N    D E C O D E &
	!		( R T N    # 7 ) &
	&

11510	ITM.RAW%=FNCVT%(ITM.RAW$) &
	\ ITM.VAL$="<"+NUM1$(SWAP%(ITM.RAW%) AND 255%)+">" &
		IF ITM.RAW% AND 255% &
	\ ITM.VAL$="<None>" UNLESS ITM.RAW% AND -256% &
	\ RETURN &
		! CHANGE A TWO BYTE STRING INTO ITS PROT EQUIVALENT STRING. &

11600	! &
	&
	&
	!	W O R D * 3 2    I N    O C T A L    A N D    D E C I M A L &
	!		( R T N    # 8 ) &
	&

11610	ITM.RAW%=FNCVT%(ITM.RAW$)*32% &
	\ ITM.VAL$=FNOCT.DEC$(ITM.RAW%) &
	\ RETURN &
		! RETURN THE OCTAL AND DECIMAL VALUES OF THE VALUE * 32. WORDS. &

11700	! &
	&
	&
	!	R T N    # 9 &
	&
	! RESERVED FOR FUTURE IMPLEMENTATION &
	&
	&

11710	RETURN &
		! IN CASE THIS ROUTINE IS EVER CALLED. &

11800	! &
	&
	&
	!	R T N    # 1 0 &
	&
	! Print the information from the lib blocks &
	&

11810	FOR LP.CNT%=0% TO 14%		! DO ALL 15 (7-M 15-M+)LIBS &
	\ Z9%=ITM.BEG%+(28%*LP.CNT%)		! STARTING OFFSET &
	\ FIELD #IN.PUT%, Z9% AS ITM.RAW$, 4% AS ITM.RAW$ &
	\ GOSUB 11000			! GO CONVERT THE NAME OF THE LIB &
	\ GOTO 11820 IF LP.CNT%>1% AND FNCVT%(ITM.RAW$)=0%	! EXIT &
	\ PRT.STG$="Library/Common Name		: "+ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+4% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11050				! CONVERT TO OCTAL &
	\ PRT.STG$=PRT.STG$+"Base address of Library		: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+6% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11050				! CONVERT TO OCTAL &
	\ PRT.STG$=PRT.STG$+"Highest Address First Window"+ &
		"	: "+ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+8% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11050				! CONVERT TO OCTAL &
	\ PRT.STG$=PRT.STG$+"Highest Address in Library"+ &
		"	: "+ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+10% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11600		! *64 USING OCTAL AND DECIMAL &
	\ PRT.STG$=PRT.STG$+"Library Load Size		: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+12% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11600		! *64 USING OCTAL AND DECIMAL &
	\ PRT.STG$=PRT.STG$+"Library Max Size		: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+14% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11100		! OCTAL WITH (DECIMAL) &
	\ PRT.STG$=PRT.STG$+"Library Offset into Region	: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+16% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11100		! OCTAL WITH (DECIMAL) &
	\ PRT.STG$=PRT.STG$+"Number of Library Window Blks	: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+18% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11100		! OCTAL WITH (DECIMAL) &
	\ PRT.STG$=PRT.STG$+"Size of Library Segment Desc	: "+ &
		ITM.VAL$+CR.LF$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+20% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ FLG.DCD$=LIB.FLG.DCD$	! use the library flag labels &
	\ GOSUB 12810		! DO A FLAG WORD &
	\ PRT.STG$=PRT.STG$+"Library Flags : "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+22% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11100		! OCTAL WITH (DECIMAL) &
	\ PRT.STG$=PRT.STG$+"Library Creation Date - Year	: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+24% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11100		! OCTAL WITH (DECIMAL) &
	\ PRT.STG$=PRT.STG$+"Library Creation Date - Month	: "+ &
		ITM.VAL$+CR.LF$ &
	\ FIELD #IN.PUT%, Z9%+26% AS ITM.RAW$, 2% AS ITM.RAW$ &
	\ GOSUB 11100		! OCTAL WITH (DECIMAL) &
	\ PRT.STG$=PRT.STG$+"Library Creation Date - Day	: "+ &
		ITM.VAL$+CR.LF$+CR.LF$ &
	\ GOSUB 10000		! AND PRINT IT OUT &
	\ GOTO 11820 IF E% &
	\ NEXT LP.CNT% &

11820	ITM.HDR$,ITM.VAL$,ITM.TRL$=NUL.STG$ &
	\ RETURN &
	! CLEAR OUT THE VALUES TO PRINT SINCE WE PRINTED IT ALREADY &
	! AND EXIT &

11900	! &
	&
	&
	!	P R I N T    T H E    T A S K    L U N    T A B L E &
	!		( R T N    # 2 2 ) &
	&

11910	ITM.HDR$=CR.LF$+"  LUN (Logical Unit Number) Table :"+CR.LF$+CR.LF$ &
	\ ITM.VAL$=NUL.STG$ &
	\ ITM.VAL$="	<Table Empty>" UNLESS R.NLUN%>0% &
	\ FOR LP.CNT%=1% TO R.NLUN% &
	\	PRT.STG$="LUN #"+NUM1$(LP.CNT%)+" :"+CR.LF$ &
	\	FIELD #IN.PUT%, 4%*(LP.CNT%-1%) AS LUN.DEV$, 2% AS LUN.DEV$, &
			2% AS LUN.UNT$ &
	\	PRT.STG$=PRT.STG$+"	Device Name  : " &
	\	Z$=LUN.DEV$+NUM1$(FNCVT%(LUN.UNT$))+":" &
	\	Z$="<none>" IF FNCVT%(LUN.DEV$)=0% &
	\	PRT.STG$=ITM.HDR$+PRT.STG$+Z$+CR.LF$+CR.LF$ &
	\	GOSUB 10000 &
	\	GOTO 11920 IF E% &
	\	ITM.HDR$=NUL.STG$ &
	\ NEXT LP.CNT% &

11920	ITM.TRL$=CR.LF$ &
	\ RETURN &
		! RESET THE ITEM VALUE AND SET UP AN ITEM TRAILER FOR THE &
		!  RETURN; &
		! EXIT. &

12000	! &
	&
	&
	!	P R I N T    T H E    I M A G E    L U N    T A B L E &
	!		( R T N    # 1 1 ) &
	&

12010	FIELD #IN.PUT%,32% AS R.NLUN$, 2% AS R.NLUN$ &
	\ R.NLUN%=FNCVT%(R.NLUN$) &
	\ ITM.HDR$=CR.LF$+"  LUN (Logical Unit Number) Table :"+CR.LF$+CR.LF$ &
	\ ITM.VAL$=NUL.STG$ &
	\ ITM.VAL$="	<Table Empty>" UNLESS R.NLUN%>-1% &
	\ FOR LP.CNT%=0% TO R.NLUN% &
	\	PRT.STG$="LUN #"+NUM1$(LP.CNT%+1%)+" :"+CR.LF$ &
	\	FIELD #IN.PUT%,ITM.BEG%+8%*LP.CNT% AS LUN.DEV$, &
			2% AS LUN.DEV$, 2% AS LUN.UNT$, 2% AS LUN.FLG$, &
			2% AS LUN.BUF$ &
	\	PRT.STG$=PRT.STG$+"	Device Name  : "+LUN.DEV$ &
	\	PRT.STG$=PRT.STG$+NUM1$(FNCVT%(LUN.UNT$)) &
			UNLESS FNCVT%(LUN.UNT$)=-1% &
	\	PRT.STG$=PRT.STG$+":"+CR.LF$ &
	\	ITM.RAW%=FNCVT%(LUN.FLG$) &
	\	PRT.STG$=PRT.STG$+"	Device Flags : "+FNOCT.AL$(ITM.RAW%) &
	\	Z$=FNBIT.DCD$(ITM.RAW%,LUN.FLG.DCD$,CHR9$+CHR9$+CHR9$+ &
			CHR9.BL$,CR.LF$) &
	\	Z$="	<No Flags Set>"+CR.LF$ UNLESS BIT.CNT% &
	\	Z$="	Flags are :"+CR.LF$+Z$ IF BIT.CNT% &
	\	PRT.STG$=PRT.STG$+Z$ &
	\	PRT.STG$=PRT.STG$+"	Buffer Size  : "+ &
			FNOCT.DEC$(FNCVT%(LUN.BUF$))+CR.LF$ &
	\	PRT.STG$=ITM.HDR$+PRT.STG$+CR.LF$ &
	\	GOSUB 10000 &
	\	ITM.VAL$=NUL.STG$ &
	\	GOTO 12020 IF E% &
	\	ITM.HDR$=NUL.STG$ &
	\ NEXT LP.CNT% &
		! FIELD THE NUMBER OF LUNS ASSIGNED; &
		! SET UP THE HEADING; &
		! IF NO LUNS ARE ASSIGNED, THEN DONE; &
		! OTHERWISE, FOR EACH ASSIGNED LUN, &
		!	SET UP THE INTRODUCTION TO THE LUN; &
		!	FIELD THE INFORMATION FOR THE LUN; &
		!	PRINT OUT DEVICE NAME AND UNIT, THE FLAGS VALUE, &
		!	 AND THE BUFFER SIZE, IN BOTH OCTAL AND DECIMAL; &
		!	PRINT A LINE; &
		!	EXIT IF ERROR; &
		! LOOP FOR ALL. &

12020	ITM.TRL$=CR.LF$ &
	\ RETURN &
		! RESET THE ITEM VALUE AND SET UP AN ITEM TRAILER FOR THE &
		!  RETURN; &
		! EXIT. &

12100	! &
	&
	&
	!	P R I N T    O U T    T H E    J O B    K E Y W O R D &
	!		( R T N    # 1 2 ) &
	&
	&

12110		ITM.RAW%=FNCVT%(ITM.RAW$) &
	\ ITM.HDR$="Job Keyword :"+CR.LF$ &
	\ ITM.VAL$="	Value : "+FNOCT.AL$(ITM.RAW%)+CR.LF$+CR.LF$+ &
		"	User Controlled : "+ &
		 RIGHT(FNOCT.AL$(ITM.RAW% AND 63%),5%)+CR.LF$+CR.LF$ &
	\ Z$=FNBIT.DCD$(ITM.RAW% AND -32576%,RTS.KEY.DCD$,CHR9$+CHR9.BL$,CR.LF$) &
	\ Z$=CHR9$+CHR9.BL$+"<No Flags Set>" UNLESS BIT.CNT% &
	\ ITM.VAL$=ITM.VAL$+"	Run-Time System Controlled :"+CR.LF$+Z$+CR.LF$ &
	\ Z$=FNBIT.DCD$(ITM.RAW% AND 32512%,JOB.KEY.DCD$,CHR9$+CHR9.BL$,CR.LF$) &
	\ Z$=CHR9$+CHR9.BL$+"<No Flags Set>" UNLESS BIT.CNT% &
	\ Z$="	System Controlled :"+CR.LF$+Z$ &
	\ ITM.VAL$=ITM.VAL$+Z$+CR.LF$ &
	\ ITM.TRL$=CR.LF$ &
		! CONVERT THE ITEM VALUE TO BINARY; &
		! SET UP HEADER; &
		! PRINT OUT THE WHOLE VALUE AND THE USER CONTROLLED VALUE (BITS &
		!  0-7); &
		! CALL THE DECODE ROUTINE TO GET THE DECODED VALUES; &
		! ISSUE ONE MESSAGE IF THE DECODE DIDN'T FIND ANY VALUES, &
		!  ANOTHER IF IT DID; &
		! SET UP THE BULK OF THE MESSAGE AND THE TRAILER. &

12120	RETURN &
		! AND EXIT. &

12200	! &
	&
	&
	!	P R I N T    O U T    I O S T S    A N D    T H E    F I R Q B &
	!		( R T N    # 1 3 ) &
	&

12210	ITM.HDR$=CR.LF$+"Job's FIRQB :"+CR.LF$ &
	\ IO.STS%=(FNCVT%(ITM.RAW$) AND 255%) &
	\ ITM.VAL$="	IOSTS:	"+RIGHT(FNOCT.DEC$(IO.STS%),4%) &
	\ ITM.VAL$=ITM.VAL$+"	"+ &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(IO.STS%)),3%),4%) IF &
			IO.STS%<128% AND IO.STS% &
	\ ITM.VAL$=ITM.VAL$+ CR.LF$+CR.LF$ &
	\ ITM.VAL$=ITM.VAL$+"	FIRQB:"+CR.LF$ &
	\ FOR LP.CNT%=0% TO 30% STEP 2% &
	\	FIELD #IN.PUT%,ITM.BEG%+LP.CNT% AS FIR.WRD$, 2% AS FIR.WRD$ &
	\	ITM.VAL$=ITM.VAL$+"	"+" "+FNOCT.AL$(FNCVT%(FIR.WRD$))+ &
		 CR.LF$ &
	\ NEXT LP.CNT% &
	\ ITM.TRL$=CR.LF$ &
	\ RETURN &
		! SET UP THE HEADER; &
		! FIELD THE IOSTS WORD AND CONVERT IT TO OCTAL, DECIMAL, AND &
		!  AN ERROR MESSAGE; &
		! APPEND THE FIRQB WORDS INTO THE VALUE STRING; &
		! SET UP THE TRAILER; &
		! AND RETURN (NOTE : THE DISPATCH ROUTINE WILL PRINT THE STRING &
		!	<HDR>+<VALUE>+<TRAILER>. &

12300	! &
	&
	&
	!	P R I N T    O U T    T H E    S A V E D    X R B &
	!		( R T N    # 1 4 ) &
	&

12310	ITM.HDR$=CR.LF$+"Job's XRB :"+CR.LF$ &
	\ ITM.VAL$=NUL.STG$ &
	\ FOR LP.CNT%=0% TO 12% STEP 2% &
	\	FIELD #IN.PUT%, ITM.BEG%+LP.CNT% AS XRB.WRD$, 2% AS XRB.WRD$ &
	\	ITM.VAL$=ITM.VAL$+"	"+FNOCT.AL$(FNCVT%(XRB.WRD$))+CR.LF$ &
	\ NEXT LP.CNT% &
	\ ITM.TRL$=CR.LF$ &
	\ RETURN &
		! PUT IN THE HEADER; &
		! APPEND IN EACH WORD OF THE XRB; &
		! PUT IN THE TRAILER; &
		! EXIT. &

12400	! &
	&
	&
	!	P R I N T    C O R E    C O M M O N &
	!		( R T N    # 1 5 ) &
	&

12410	ITM.HDR$="Job's Core Common :"+CR.LF$ &
	\ LEN.COM%=ASCII(ITM.RAW$) &
	\ ITM.VAL$="	<Empty>" UNLESS LEN.COM% &
	\ ITM.VAL$="	Length : "+NUM1$(LEN.COM%)+CR.LF$ IF LEN.COM% &
	\ PRT.STG$=NUL.STG$ &
	\ FOR LP.CNT%=1% TO LEN.COM% &
	\	FIELD #IN.PUT%,ITM.BEG%+LP.CNT% AS ITM.RAW$,1% AS ITM.RAW$ &
	\	Z%=ASCII(ITM.RAW$) &
	\	Z$="  "+ITM.RAW$+" " &
	\	Z$="<CR>" IF Z%=13% &
	\	Z$="<LF>" IF Z%=10% &
	\	Z$="<FF>"+CR.LF$ IF Z%=12% &
	\	Z$=NUM1$(Z%)+" " IF Z%<>10% AND Z%<>12% AND Z%<>13% IF &
			Z%<32% &
	\	ITM.VAL$=ITM.VAL$+Z$ &
	\	PRT.STG$=PRT.STG$+" "+RIGHT(FNOCT.AL$(Z%),4%) &
	\ NEXT LP.CNT% &
	\ ITM.VAL$=ITM.VAL$+CR.LF$+PRT.STG$+CR.LF$ &
	\ ITM.TRL$=CR.LF$ &
	\ RETURN &
		! SET UP HEADING; &
		! GET THE LENGTH OUT OF THE BUFFER; &
		! IF NONE, SAY SO; &
		! OTHERWISE, PRINT THE LENGTH; &
		! FOR EACH CHARACTER IN THE STRING (MAXIMUM LENGTH OF &
		!  127); &
		!	GET THE CHR; &
		!	TRANSFORM IT AS APPROPRIATE; &
		!	APPEND IT TO THE OUTPUT STRING; &
		! LOOP FOR ALL CHRS; &
		! SET UP THE TRAILER; &
		! EXIT. &

12500	! &
	&
	&
	!	P R I N T    T H E    L O G I C A L    D E V I C E &
	!		A S S I G N M E N T    T A B L E &
	!		( R T N    # 1 6 ) &
	&

12510	ITM.HDR$="User Logical Name Table :"+CR.LF$ &
	\ ITM.VAL$=NUL.STG$ &
	\ LOG.CNT%=0% &
	\ FIELD #IN.PUT%, ITM.BEG%+24% AS USR.PPN$, 2% AS USR.PPN$ &
	\ USR.PPN%=0% &
	\ PPN.FLG%=(CVT$%(USR.PPN$)=-1%) &
	&
	\ FOR LP.CNT%=0% TO 24%+(8%*PPN.FLG%) STEP 8% &
	\	FIELD #IN.PUT%, ITM.BEG%+LP.CNT% AS DEV.NM1$, &
			2% AS DEV.NM1$, 2% AS DEV.NM2$, 2% AS DEV.NAM$, &
			1% AS DEV.UNT$, 1% AS DEV.FLG$ &
	\	IF FNCVT%(DEV.NM1$) THEN &
			LOG.CNT%=LOG.CNT%+1% &
	\		ITM.VAL$=ITM.VAL$+"	Assignment #"+ &
			 NUM1$(LOG.CNT%)+": "+ &
			 RAD$(FNCVT%(DEV.NM1$))+RAD$(FNCVT%(DEV.NM2$))+ &
			 ": = "+DEV.NAM$ &
	\		ITM.VAL$=ITM.VAL$+NUM1$(ASCII(DEV.UNT$)) IF &
				ASCII(DEV.FLG$) &
	\		IF PPN.FLG%=0% THEN &
				ITM.VAL$=ITM.VAL$+":"+CR.LF$ &
			ELSE &
			FIELD #IN.PUT%,ITM.BEG%+26%+(LP.CNT%/4%) AS USR.PPN$, &
				2% AS USR.PPN$ &
	\		USR.PPN%=CVT$%(USR.PPN$) &
	\		ITM.VAL$=ITM.VAL$+":" &
	\		ITM.VAL$=ITM.VAL$+" ["+NUM1$(USR.PPN% AND 255%)+ &
				","+NUM1$(SWAP%(USR.PPN%) AND 255%)+"]" &
				IF USR.PPN% &
	\		ITM.VAL$=ITM.VAL$+CR.LF$ &
		! SET UP THE HEADER; &
		! INIT THE RETURN STRING; &
		! INIT THE COUNT OF LOGICAL NAMES (LOG.CNT%); &
		! LOOP FOR ALL ENTRIES IN THE TABLE; &
		!	FIELD THE APPROPRIATE POSITIONS; &
		!	IF THE FIRST WORD IS NON-ZERO, THEN &
		!	 (THIS ENTRY IS USED) &
		!		INCREMENT THE COUNT; &
		!		APPEND THE APPROPRIATE INFORMATION ONTO &
		!		 THE RETURN STRING. &
		!	NOTE THAT IF THE FIRST WORD OF ENTRY 4 IS -1, THE &
		!	REMAINDER OF THE ENTRY HOLDS PPN'S TO BE ASSOCIATED &
		!	WITH THE OTHER 3 ENTRIES. &

12520	NEXT LP.CNT% &
	\ ITM.VAL$="	<Table Empty>"+CR.LF$ UNLESS LOG.CNT% &
	\ ITM.TRL$=CR.LF$ &
	\ RETURN &
		! LOOP FOR ALL ENTRIES (SOME MAY BE UNUSED); &
		! SET UP THE RETURN VALUE IF THE TABLE IS EMPTY; &
		! SET UP THE TRAILER; &
		! RETURN. &

12600	! &
	&
	&
	!	P R I N T    O U T    S A V E D    R E G I S T E R S &
	!		( R T N    # 1 7 ) &
	&

12610	ITM.RAW%=FNCVT%(ITM.RAW$) &
	\ ITM.VAL$=NUL.STG$ &
	\ ITM.HDR$="Registers at time of Dump :"+CR.LF$ &
	\ FOR LP.CNT%=0% STEP 2% WHILE LP.CNT%<17% AND E%=0% &
	\	REG.VAL%=FNGET.WRD%(ITM.RAW%) &
	\	GOTO 12620 IF E% &
	\	ITM.RAW%=ITM.RAW%+2% &
	\	ITM.VAL$=ITM.VAL$+"	R"+NUM1$(LP.CNT%/2%) UNLESS &
			LP.CNT%>10% &
	\	ITM.VAL$=ITM.VAL$+"	"+MID("SPPCPS",LP.CNT%-11%,2%) IF &
			LP.CNT%>10% &
	\	ITM.VAL$=ITM.VAL$+"	"+FNOCT.AL$(REG.VAL%)+ &
			CR.LF$ &
	\	SP.SAV%=REG.VAL% IF LP.CNT%=12% &
		! GET THE NUMERIC VALUE OF THE POINTER TO THE REGS; &
		! INIT THE RETURN STRING; &
		! SET UP THE HEADER; &
		! FOR EACH REGISTER (R0, R1, R2, R3, R4, R5, SP, PC, PS); &
		!	CALCULATE THE BLOCK NUMBER IN WHICH THE STORED &
		!	 REGISTER LIES; &
		!	GET THE BLOCK IF NECESSARY; &
		!	CHOOSE A NAME FOR THE REGISTER AND APPEND ON &
		!	 ITS VALUE; &
		!	SAVE THE STACK POINTER, WHEN YOU HIT IT;
12620	NEXT LP.CNT% &
	\ NO.STCK%=(E%<>0%) &
	\ ITM.VAL$=ITM.VAL$+"*** Error in Register Lookup - "+ &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E%)),3%),4%)+ &
		" on "+IN.PUT$ IF E% &
	\ E%=0% &
	\ ITM.TRL$=CR.LF$ &
	\ RETURN &
		! LOOP FOR ALL REGS; &
		! SET THE 'INHIBIT PRINT OF STACK' FLAG IF AN ERROR WAS FOUND; &
		! SET UP AN ERROR MESSAGE FOR PRINTOUT IF AN ERROR WAS FOUND; &
		! RESET THE ERROR VALUE; &
		! SET UP THE TRAILER; &
		! EXIT. &

12700	! &
	&
	&
	!	P R I N T    O U T    T H E    S T A C K &
	!		( R T N    # 1 8 ) &
	&

12710	SP.BASE%=FNCVT%(ITM.RAW$)+4% &
	\ SP.LOOP%=SP.BASE%-SP.SAV% &
	\ ITM.HDR$="Job's Stack :"+CR.LF$+CHR9$+"Initial SP = "+ &
		FNOCT.AL$(SP.BASE%)+CHR9$+"Final SP = "+FNOCT.AL$(SP.SAV%)+ &
		CR.LF$ &
	\ ITM.HDR$=ITM.HDR$+"	<Stack Dump Inhibited because of Error in"+ &
		" Register Lookup>"+CR.LF$ IF NO.STCK% &
	\ GOTO 12730 IF NO.STCK% &
	\ IF SP.LOOP%>2048% THEN &
		SP.LOOP%=2048% &
	\	ITM.HDR$=ITM.HDR$+"  (Value for Initial SP too large - "+ &
		 "Truncating Stack Dump to 1024 words.)"+CR.LF$ &

12720		ITM.HDR$=ITM.HDR$+CR.LF$+"  SP"+CR.LF$+"  !"+CR.LF$+"  V"+CR.LF$ &
	\ FOR LP.CNT%=0% STEP 16% WHILE LP.CNT%<SP.LOOP% AND E%=0% &
	\	ITM.VAL$=FNOCT.AL$(SP.SAV%+LP.CNT%)+" / " &
	\	ITM.VAL$=ITM.VAL$+" "+FNOCT.AL$(FNGET.WRD%(SP.SAV%+ &
			LP.CNT%+LP1.CNT%)) UNLESS &
				LP.CNT%+LP1.CNT%>SP.LOOP% OR E% FOR &
					LP1.CNT%=0% STEP 2% UNTIL &
					 LP1.CNT%>14% UNLESS E% &
	\	PRT.STG$=ITM.HDR$+ITM.VAL$+CR.LF$ &
	\	GOSUB 10000 UNLESS E% &
	\	ITM.HDR$,ITM.VAL$=NUL.STG$ &
	\ NEXT LP.CNT% &

12730	RETURN &
		! AND EXIT. &

12800	! &
	&
	&
	!	P R I N T    O U T    T H E    T A S K    F L A G S &
	!		( R T N    # 1 9 ) &
	&
	&
	FLG.DCD$=TSK.FLG.DCD$	! Use the task flag labels &

12810	ITM.HDR$="Task Flags : " &
	\ ITM.RAW%=FNCVT%(ITM.RAW$) &
	\ ITM.VAL$=FNOCT.DEC$(ITM.RAW%) &
	\ Z$=FNBIT.DCD$(ITM.RAW%,FLG.DCD$,CHR9$+CHR9$+CHR9$+CHR9.BL$,CR.LF$) &
	\ Z$=CHR9$+"Flags Set :"+CR.LF$+Z$ &
	\ Z$=CHR9$+"<No Flags Set>" UNLESS BIT.CNT% &
	\ ITM.VAL$=ITM.VAL$+Z$ &
	\ ITM.TRL$=CR.LF$ &
	\ RETURN &

12900	! &
	&
	&
	!	P R I N T    A    B L A N K    L I N E &
	!		( R T N    # 2 0 ) &
	&

12910	ITM.HDR$=NUL.STG$ &
	\ RETURN &
	&
	&
	&

13000	! &
	&
	&
	!	G E T    T O    T O P    O F    F O R M &
	!		( R T N    # 2 1 ) &

13010	ITM.HDR$=NUL.STG$ &
	\ GOTO 10100 &
		! MAKE SURE THE HEADER IS NULLED OUT; &
		! GO DO A PAGE BREAK. &
	&

15000	DEF* FNOCT.AL$(OP.RND%) &
	\ Z$=NUL.STG$ &
	\ Z%=(OP.RND%<0%) &
	\ OP.RND%=OP.RND% AND 32767% &
	\ FOR Z0%=1% TO 5% &
	\	Z$=NUM1$(OP.RND% AND 7%)+Z$ &
	\	OP.RND%=OP.RND%/8% &
	\ NEXT Z0% &
	\ Z$="0"+Z$ UNLESS Z% &
	\ Z$="1"+Z$ IF Z% &
	\ FNOCT.AL$=Z$ &
	\ FNEND &
		! FUNCTION TO CONVERT A BINARY NUMBER INTO AN OCTAL STRING. &
		!	OP.RND%		NUMBER TO CONVERT &
		! &
		! INIT THE RETURN STRING; &
		! SAVE THE VALUE OF THE SIGN BIT FOR LATER; &
		! STRIP THE SIGN BIT; &
		! GET 5 CHARACTERS OF OCTAL; &
		! PUT A '0' ON THE BEGINNING IF THE SIGN BIT WAS OFF ON ENTRY, &
		!  ELSE PUT A '1' ON THE BEGINNING; &
		! SET THE RETURN VALUE; &
		! EXIT. &
	&

15010	DEF* FNID%(X%) &
	\ FNID%=X% &
	\ FNID%=X%+224% IF ((ID.VAL%=4%) AND ((X%>=230%) AND (X%<=260%))) &
				! M+ HAS EXTRA STUFF &
	\ FNEND &

15080	DEF* FNDECIMAL.VALUE%(X$) &
	!------------------------------------------------------------! &
	!	RETURN INTEGER VALUE OF OCTAL DIGIT STRING. &
	!------------------------------------------------------------! &
	\ Y%=0% &
	\ IF INSTR(1%,X$,".") OR INSTR(1%,X$,"8") OR INSTR(1%,X$,"9") THEN &
		X%=VAL(X$) &
	\	GOTO 15082 &

15081	Y%=(32767%+1%) IF (ASCII(X$)=49%) IF (LEN(X$)=6%) &
	\ X$=RIGHT(X$,2%) IF LEN(X$)=6% &
	\ X%=0% &
	\ FOR Z%=1% TO LEN(X$) &
	\    X%=(X%*8%)+(ASCII(X$)-48%) &
	\    X$=RIGHT(X$,2%) &
	\ NEXT Z% &

15082	FNDECIMAL.VALUE%=X% OR Y% &
	\ FNEND &

15100	DEF* FNOCT.DEC$(OP.RND%)=FNOCT.AL$(OP.RND%)+" ("+NUM1$(OP.RND%)+".)" &
		! FUNCTION TO RETURN BOTH THE OCTAL AND DECIMAL VALUE IN &
		!  STRING FORM. &
	&

15200	DEF* FNCVT%(OP.RND$)=SWAP%(CVT$%(OP.RND$)) &
		! FUNCTION TO GET A WORD VALUE FROM A 2-BYTE STRING. &
		! NOTE : CVT$%() RETURNS THE BYTES SWAPPED FROM THE WAY &
		! THEY WERE DUMPED TO DISK. &
	&

15400	DEF* FNGET.WRD%(OP.RND%) &
		! FUNCTION TO GET THE WORD WHOSE MEMORY ADDRESS WAS OP.RND% &
		!  FROM THE FILE. &

15410	ON ERROR GOTO 15430 &
	\ Z%=OP.RND% AND 32767% &
	\ NXT.BLK%=Z%/512% &
	\ NXT.BYT%=Z%-NXT.BLK%*512% &
	\ NXT.BLK%=NXT.BLK%+64% IF OP.RND%<0% &
	\ NXT.BLK%=NXT.BLK%+ONES.OFFSET% &
	\ GET #IN.PUT%, RECORD NXT.BLK% UNLESS CUR.BLK%=NXT.BLK% &
	\ CUR.BLK%=NXT.BLK% &
	\ FIELD #IN.PUT%, NXT.BYT% AS Z$, 2% AS Z$ &
	\ FNGET.WRD%=FNCVT%(Z$) &
		! SET UP AN ERROR TRAP; &
		! STRIP OFF THE SIGN BIT, BUT SAVE ITS VALUE FOR LATER &
		! CALCULATE THE DISK ADDRESS OF THE BLOCK CONTAINING THE &
		!  REQUESTED WORD; &
		! CALCULATE THE BYTE OFFSET IN THAT BLOCK FOR THE WORD; &
		! IF A NEW BLOCK IS NEEDED, GET IT; &
		! UPDATE THE CURRENT BLOCK POINTER; &
		! FIELD AND CVT THE APPROPRIATE WORD; &
		! SET UP THE RETURN VALUE. &
	! IF THE .TSK IS REALLY A "SIL" THEN OFFSET ALL REFS BY 1 DISK BLOCK. &

15420	ON ERROR GOTO 19000 &
	\ FNEND &
		! RESET ERROR TRAP; &
		! EXIT. &

15430	E%=ERR &
	\ FNGET.WRD%=0% &
	\ RESUME 15420 &
		! SET ERROR VALUE AND RESUME AT EXIT. &
	&

15500	DEF* FNBIT.DCD$(BIT.VAL%,BIT.DCD$,DCD.HDR$,DCD.TRL$) &
	\ Z$=NUL.STG$ &
		! FUNCTION TO DECODE A BIT STRING. &
		!	BIT.VAL%	- WORD CONTAINING BITS TO DECODE &
		!	BIT.DCD$	- STRING HOLDING THE DECODE VALUES &
		!	BIT.HDR$	- STRING TO PUT BEFORE EACH DECODED &
		!			  VALUE &
		!	BIT.TRL$	- STRING TO PUT AFTER EACH DECODED &
		!			  VALUE &
		! RETURNS : &
		!	FUNCTION VALUE	= STRING DECODED &
		!	BIT.CNT%	= COUNT OF NUMBER OF BITS DECODED &

15510	Z0%,BIT.CNT%=0% &
	\ Z1%=-1% &
	\ FOR Z%=0% WHILE Z%<16% AND Z1%<>0% &
	\	Z1%=INSTR(Z0%+1%,BIT.DCD$,CHR$(255%)) &
	\	IF BIT.VAL% AND 2%^Z% AND Z1%<>0% THEN &
			Z$=Z$+DCD.HDR$+MID(BIT.DCD$,Z0%+1%,Z1%-Z0%-1%) &
	\		Z$=Z$+"Unknown" IF Z1%=Z0%+1% &
	\		Z$=Z$+DCD.TRL$ &
	\		BIT.CNT%=BIT.CNT%+1% &
		! SET THE INITIAL VALUE OF THE POSITION POINTER AND THE COUNT &
		!  OF BITS DECODED; &
		! FOR EACH POSSIBLE BIT VALUE; &
		!	FIND THE END OF THE BIT STRING; &
		!	APPEND ON THE DECODE VALUE IF THE BIT IS SET IN THE &
		!	 PARAMETER WORD; &
		!	INCREMENT THE BIT COUNTER; &

15520		Z0%=Z1% &
	\ NEXT Z% &
		! LOOP. &
	&

15530	FNBIT.DCD$=Z$ &
	\ FNEND &
		! APPEND ONTO Z$ THE DECODE FOR EACH BIT SET IN THE PARAMETER &
		!  WORD; &
		! SET THE FUNCTION VALUE; &
		! EXIT. &
	&
	&

15600	DEF* FNFIL.PCK$(SPC.FIL$,DEF.FIL$,OVR.FIL$) &
	\ FNFIL.PCK$=NUL.STG$ &
		! FUNCTION TO APPLY DEFAULTS AND OVERRIDES TO A FILENAME STRING. &
		!	SPC.FIL$	FILENAME STRING TO WHICH TO APPLY THE &
		!			DEFAULTS/OVERRIDES &
		!	DEF.FIL$	STRING GIVING THE DEFAULTS TO APPLY &
		!			ELEMENTS OF THIS STRING ARE PUT INTO &
		!			THE RETURNED STRING ONLY IF THOSE &
		!			ELEMENTS DO NOT ALREADY EXIST IN THE &
		!			SPC.FIL$ STRING &
		!	OVR.FIL$	OVERRIDES TO APPLY TO THE SPC.FIL$ &
		!			STRING - ELEMENTS OF THIS STRING ARE &
		!			MOVED INTO THE RETURN STRING REGARDLESS &
		!			OF WHETHER OR NOT THOSE ELEMENTS ALREADY &
		!			EXISTED IN THE SPC.FIL$ STRING - THEY &
		!			OVERRIDE VALUES IN THE SPC.FIL$ STRING. &
		! &
		! USES THE M%() AND M0%() ARRAYS. &
		! &
		! CAN RETURN ANY FILENAME STRING SCAN ERROR. &

15610	ON ERROR GOTO 15690 &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+OVR.FIL$) TO M0% &
	\ M0%(Z%)=M0%(Z%)+SWAP%(M0%(Z%+1%)) FOR Z%=5% TO 29% STEP 2% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+SPC.FIL$) TO M% &
	\ M%(Z%)=M%(Z%)+SWAP%(M%(Z%+1%)) FOR Z%=5% TO 29% STEP 2% &
	\ M%(29%)=M%(29%) AND NOT M0%(29%) &
	\ M%(27%)=M%(27%) AND NOT M0%(27%) &
	\ GOSUB 15700 &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+DEF.FIL$) TO M0% &
	\ M0%(Z%)=M0%(Z%)+SWAP%(M0%(Z%+1%)) FOR Z%=5% TO 29% STEP 2% &
	\ GOSUB 15700 &
	\ Z$=NUL.STG$ &
	\ IF M%(29%) AND 4096% THEN &
		Z$=CVT%$(SWAP%(M%(23%))) &
	\	Z$=Z$+NUM1$(M%(25%) AND 255%) IF M%(25%) AND -256% &
	\	Z$=Z$+":" &
		! SET UP AN ERROR TRAP FOR THE FILENAME STRING SCAN; &
		! SCAN THE OVERRIDE STRING AND PACK IT; &
		! SCAN THE SOURCE STRING AND PACK IT; &
		! MAKE SURE THAT ANY FLAGS SET IN THE OVERRIDE STRING ARE &
		!  NOT SET IN THE SOURCE STRING SO THE SUBROUTINE WILL REPLACE &
		!  ALL VALUES PRESENT IN THE OVERRIDE STRING; &
		! CALL THE REPLACEMENT SUBROUTINE; &
		! NOW SCAN AND PACK THE DEFAULT STRING; &
		! CALL THE REPLACEMENT SUBROUTINE ON IT; &
		! FINALLY, WE REPACK INTO Z$ - INIT Z$; &
		! IF A DEVICE NAME EXISTS IN THE NEW SOURCE, THEN &
		!	PUT IN THE DEVICE NAME. &

15620	Z$=Z$+"["+NUM1$(SWAP%(M%(5%)) AND 255%)+","+NUM1$(M%(5%) AND 255%)+ &
		"]" IF M%(29%) AND 128% &
	\ Z$=Z$+RAD$(M%(7%))+RAD$(M%(9%)) IF M%(29%) AND 1% &
	\ Z$=Z$+"."+RAD$(M%(11%)) IF M%(29%) AND 8% &
	\ Z$=Z$+"<"+NUM1$(SWAP%(M%(21%)) AND 255%)+">" IF M%(29%) AND 3072% &
	&
	\ Z$=Z$+"/CL:"+NUM1$(M%(15%)) IF M%(27%) AND 1% &
	\ Z$=Z$+"/MO:"+NUM1$(M%(17%) AND 32767%) IF M%(27%) AND 2% &
	\ Z$=Z$+"/SI:"+NUM1$(M%(13%)) IF M%(27%) AND 4% &
	&
	\ FNFIL.PCK$=Z$ &
		! PUT IN THE PPN IF ONE EXISTS; &
		! PUT IN THE FILENAME IF ONE EXISTS; &
		! PUT IN THE EXTENSION IF ONE EXISTS; &
		! PUT IN THE PROTECTION IF ONE EXISTS; &
	&
		! PUT IN THE CLUSTERSIZE IF ONE EXISTS; &
		! PUT IN THE MODE IF ONE EXISTS; &
		! PUT IN THE FILESIZE IF ONE EXISTS; &
	&
		! RETURN THE FUNCTION VALUE. &

15630	ON ERROR GOTO 19000 &
	\ FNEND &
		! RESET ERROR TRAP; &
		! EXIT. &

15690	E%=ERR &
	\ RESUME 15630 &
		! TRAP ANY ERROR OUT OF A FILENAME STRING SCAN; &
		! RESUME AT EXIT. &
		! NOTE : ANY ERROR IN EITHER THE DEFAULT STRING OR THE OVERRIDE &
		! STRING WILL TRAP HERE. &

15700	M%(Z%)=M0%(Z%) FOR Z%=23% TO 25% STEP 2% UNLESS M%(29%) AND 4096% &
	\ M%(5%)=M0%(5%) UNLESS M%(29%) AND 128% &
	\ M%(Z%)=M0%(Z%) FOR Z%=7% TO 9% STEP 2% UNLESS M%(29%) AND 1% &
	\ M%(11%)=M0%(11%) UNLESS M%(29%) AND 8% &
	\ M%(21%)=M0%(21%) UNLESS M%(29%) AND 3072% &
	&
	\ M%(15%)=M0%(15%) UNLESS M%(27%) AND 1% &
	\ M%(17%)=M0%(17%) UNLESS M%(27%) AND 2% &
	\ M%(13%)=M0%(13%) UNLESS M%(27%) AND 4% &
	&
	\ M%(29%)=M%(29%) OR (M0%(29%) AND NOT M%(29%)) &
	\ M%(27%)=M%(27%) OR (M0%(27%) AND NOT M%(27%)) &
	&
	\ RETURN &
		! REPLACE DEVICE NAME UNLESS ONE ALREADY EXISTS; &
		! REPLACE THE PPN UNLESS ONE ALREADY EXISTS; &
		! REPLACE THE FILENAME UNLESS ONE ALREADY EXISTS; &
		! REPLACE THE EXTENSION UNLESS ONE ALREADY EXISTS; &
		! REPLACE THE PROTECTION UNLESS ONE ALREADY EXISTS; &
	&
		! REPLACE THE CLUSTERSIZE UNLESS ONE ALREADY EXISTS; &
		! REPLACE THE MODE UNLESS ONE ALREADY EXISTS; &
		! REPLACE THE FILESIZE UNLESS ONE ALREADY EXISTS; &
	&
		! MAKE SURE THAT ALL FW2 BITS SET IN THE REPLACEMENT STRING &
		!  ARE ALSO SET IN THE SOURCE; &
		! MAKE SURE THAT ALL FW1 BITS SET IN THE REPLACEMENT STRING &
		!  ARE ALSO SET IN THE SOURCE; &
	&
		! EXIT. &
	&
	&
	&

18400	DEF* FNUNSIGNED(X%)=32768.+(X% EQV 32767%) &
	!------------------------------------------------------------! &
	!	RETURN THE UNSIGNED MAGNITUDE OF AN INTEGER VALUE &
	!------------------------------------------------------------! &

19000	! &
	&
	&
	!	E R R O R    H A N D L E R &
	&

19010	E%=ERR UNLESS E% &
	\ RESUME 19020 &
		! SET UP THE ERROR VALUE. &

19020	IF E%=11% AND ERL=2010 THEN &
		GOTO 5000 &
		! IF THE USER TYPED A CTRL/Z, THEN &
		!	SHUTDOWN. &

19990	PRINT CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E%)),3%),4%); &
		" at line";ERL &
	\ GOTO 5000 &
		! PRINT THE FATAL ERROR AND SHUT DOWN. &

30000	! &
	&
	&
	!	C C L    E N T R Y &
	&

30010	C$=CVT$$(SYS(CHR$(7%)),-2%) &
	\ IF LEFT(C$,6%)="PMDUMP" THEN &
		C$=RIGHT(C$,7%) &
	\	E0%=2% AND C$<>"" &
	\	GOTO 1000 &
		! GET CORE COMMON; &
		! IF THE ENTRY IS PROPER, THEN &
		!	SET THE 'CCL ENTRY' VALUE IF ANY COMMAND LINE &
		!	 WAS PASSED (OTHERWISE, SET 'RUN ENTRY'); &
		!	GO START UP. &

30020	PRINT "Illegal CCL Entry" &
	\ GOTO 32767 &
		! IMPROPER ENTRY. &

31000	! &
	&
	&
	!	C H A I N    E N T R Y &
	&

31010	C$=CVT$$(SYS(CHR$(7%)),-2%) &
	\ E0%=3% &
	\ GOTO 1000 &
		! GET THE COMMAND STRING; &
		! SET THE 'CHAIN ENTRY' VALUE; &
		! GO START UP. &

32767	END
