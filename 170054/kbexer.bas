2!		PROGRAM		: KBEXER.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND			!USE EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1976, 1991 by &
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

21	! VER/ED	EDIT DATE	REASON &
	!
99!	&

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

101	! THE KEYBOARD EXERCISER TEST KBEXER IS USED TO TEST LOCAL &
	! AND REMOTE TERMINALS CONNECTED TO RSTS/E SYSTEMS.  KBEXER &
	! WILL TEST ONLY ONE TERMINAL AT A TIME. &
	! &
	! KBEXER IMPLEMENTS FOUR TERMINAL TESTS: &
	! &
	!		 THE SPACE TEST &
	! &
	! VERIFIES THAT THE TERMINAL CARRIAGE WILL RETURN RELIABLY &
	! FROM ANY POSITION. &
	! &
	!		THE ASCII PATTERN TEST &
	! &
	! VERIFIES THAT THE TERMINAL WILL PRINT THE STANDARD ASCII &
	! CHARACTER SET IN ALL PRINT POSITIONS. &
	! &
	!		THE WORST CASE TEST &
	! &
	! FORCES THE HEAD ON ASR-33 TERMINALS TO ROTATE A HALF &
 	! REVOLUTION AT EVERY PRINT POSITION. &
	! &
	!		THE REPEAT TEST &
	! &
	! REPEATS EVERYTHING TYPED,  ONE LINE AT A TIME. &
	! &
	! &
	! THE TESTS ARE STARTED BY TYPING THE TEST NAME.  THEY ARE &
	! TERMINATED BY TYPING CONTROL/C.  THE HELP FILE CAN BE OBTAINED &
	! BY TYPING HELP. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !	&
   !		1		USED FOR TERMINAL BEING TESTED &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !	&
   !		DEV.NAM$	NAME OF THE KEYBOARD TO TEST. &
   !		DHI%		DEVICE HANDLER INDEX OF KEYBOARD (=2). &
   !		FSS.ARRAY%(30)	ARRAY TO HOLD SYS CALL STRING SCAN INFO. &
   !		I%		TEMPORARY VARIABLE. &
   !		I$		VERSION-EDIT LEVEL. &
   !		ITER.NO%	NUMBER OF TEST ITERATIONS TO PERFORM. &
   !		J%		TEMPORARY VARIABLE. &
   !		JOB.NO%		OUR JOB NUMBER. &
   !		KB$		OUR KEYBOARD SPECIFICATION. &
   !		KB.NO%		OUR KEYBOARD NUMBER. &
   !		P$		TEST STRING TO PRINT. &
   !		S$		TEMPORARY STRING FLAG FOR INPUT AND SYS(). &
   !		TEMP$		TEMPORARY STRING. &
   !		TEST%		NUMBER OF THE TEST TO PERFORM. &
   !		W%		WIDTH OF CURRENT KB IN CHARACTERS. &
   !	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
900	! &
	&
	&
	&
	!	D I M E N S I O N   S T A T E M E N T S &
	&

910	DIM FSS.ARRAY%(30%) &
	! FILENAME STRING SCAN DATA ARRAY. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ S$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ S$=SYS(CHR$(6%)+CHR$(-7%)) &
		! SET UP STANDARD ERROR TRAP. &
		! PERMANENTLY DROP TEMPORARY PRIVILEGES. &
		! SET CTRL/C TRAP. &

1010	I$="V10.1-A" &
	! SET UP VERSION AND EDIT NUMBER &

1020	PRINT "KBEXER";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! PRINT HEADER. &

2000	I%=CVT$%(SYS(CHR$(6%)+CHR$(9%))) &
	\ KB.NO%=(I% AND 255%)/2% &
	\ JOB.NO%=(SWAP%(I%) AND 255%)/2% &
	\ KB$="_KB"+NUM1$(KB.NO%)+":" &
		! GET OUR KB # AND JOB #. &
		! BUILD DEVICE STRING FOR OUR KB:. &

2110	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Keyboard to test <";KB$;">"; &
	\ INPUT LINE DEV.NAM$ &
	\ GOTO 32767 IF ASCII(DEV.NAM$)=27% &
	\ DEV.NAM$=CVT$$(DEV.NAM$,254%) &
	\ DEV.NAM$=KB$ UNLESS LEN(DEV.NAM$) &
	\ DEV.NAM$=FNCHEK.DEV$(DEV.NAM$,2%) &
	\ GOTO 2110 UNLESS LEN(DEV.NAM$) &
		! GET DEVICE TO CHECK AND MAKE SURE IT'S LEGAL. &

2120	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Test (SPA[CE], ASC[II], WOR[ST], REP[EAT], HEL[P])"; &
	\ INPUT LINE S$ &
	\ GOTO 2110 IF ASCII(S$)=27% &
	\ S$=LEFT(CVT$$(S$,254%),3%) &
	\ GOTO 2120 UNLESS LEN(S$) &
	\ TEST%=0% &
	\ TEST%=1% IF S$="SPA" &
	\ TEST%=2% IF S$="ASC" &
	\ TEST%=3% IF S$="WOR" &
	\ TEST%=4% IF S$="REP" &
	\ TEST%=5% IF S$="HEL" &
	\ GOTO 2120 UNLESS TEST% &
	\ IF TEST%=5% THEN &
		GOSUB 8000 &
	\	GOTO 2120 &
		! GET AND DECODE TEST TO PERFORM. &
		! GIVE HIM HELP IF HE WANTS IT. &

2130	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of test iterations <8>"; &
	\ INPUT LINE S$ &
	\ GOTO 2120 IF ASCII(S$)=27% &
	\ S$=CVT$$(S$,254%) &
	\ S$="8" UNLESS LEN(S$) &
	\ ITER.NO=VAL(S$) &
	\ IF ITER.NO=-1. THEN &
		ITER.NO=1000000000000000000000000000000. &
	  ELSE	IF ITER.NO<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 2130 &
		! GET NUMBER OF ITERATIONS TO DO. &

2140	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE S$ &
	\ GOTO 2130 IF ASCII(S$)=27% &
	\ PRINT &
	\ S$=CVT$$(S$,254%) &
	\ IF ASCII(S$)=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	S$=SYS(CHR$(6%)+CHR$(7%)) &
		! DETACH IF HE WANTS TO. &

2200	OPEN DEV.NAM$ AS FILE 1% &
	\ W%=ASCII(MID(SYS(CHR$(6%)+CHR$(-8%)+CHR$(1%)),20%,1%))-1% &
	\ ON TEST% GOSUB 3000, 3200, 3400, 3600 &
	\ GOTO 9000 &
		! OPEN THE KEYBOARD, GET ITS WIDTH, &
		! GO DO IT AND EXIT. &

3000	PRINT #1%, &
	\ PRINT #1%, "  ***  SPACING TEST  ***" &
	\ PRINT #1%, &
	! SPACING TEST "SPA" &

3010	FOR J=1. TO ITER.NO &
	\ PRINT #1%, SPACE$(I%); "\" FOR I%=0% TO W%-1% &
	\ PRINT #1%, SPACE$(I%); "/" FOR I%=W%-1% TO 0% STEP -1% &
	\ NEXT J &
	\ RETURN &
	! SPACING TEST. &

3200	PRINT #1%, &
	\ PRINT #1%, "  ***  ROTATING ASCII CHARACTERS TEST  ***" &
	\ PRINT #1%, &
	! IDENTIFY TEST &

3210	P$=" !"+'"'+"#$%&'()*+,-./0123456789:;<=>?@"+ &
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_" &

3220	P$=P$+P$ UNTIL LEN(P$)>=2%*W% &
	! CREATE STRING OF AT LEAST TWICE TERMINAL WIDTH. &

3230	PRINT #1%, MID(P$,J%,W%); FOR J%=W% TO 1% STEP -1% &
		FOR I=1. TO ITER.NO &
	\ RETURN &
	! PRINT OUT ROTATING ASCII TEST. &

3400	PRINT #1%, &
	\ PRINT #1%, "  ***  ROTATING WORST CASE (ASR33) PATTERN TEST  ***" &
	\ PRINT #1%, &
	! IDENTIFY THE TEST. &

3410	P$="'_W/" &
	\ GOTO 3220 &
	! DEFINE STRING AND JUMP INTO ASCII TEST. &

3600	PRINT #1%, &
	\ PRINT #1%, "  ***  REPEAT TEST  ***" &
	! IDENTIFY THE TEST. &

3610	INPUT LINE S$ &
	\ PRINT #1%, "? "; S$ &
	\ GOTO 3610 &
	! REPEAT LINE BY LINE LOOP FOREVER. &

8000	PRINT &
	\ S$=CHR$(13%)+CHR$(10%) &
	\ PRINT	"The four keyboard tests available are:"	;S$; &
								 S$; &
		"  SPA[CE]	Space and carriage return test"	;S$; &
		"  ASC[II]	Rotating ASCII character test"	;S$; &
		"  WOR[ST]	Worst case (ASR33) pattern test";S$; &
		"  REP[EAT]	Repeats line typed on terminal"	;S$; &
								 S$; &
		" To halt any test during execution, type CTRL/C.";S$ &
	\ RETURN &
		! PRINT HELP MESSAGE. &

9000	CLOSE 1% &
	\ TEMP$=TIME$(0%) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "KBEXER Finished at ";TEMP$ &
	\ GOTO 32767 &
	! NORMAL EXIT &

16100	! &
	&
	!	C H E C K    D E V I C E    S P E C I F I E D &
	&
	&

16110	DEF* FNCHEK.DEV$(DEV.NAM$,DHI%) &

16120	CHANGE SYS(CHR$(6%)+CHR$(-23%)+DEV.NAM$) TO FSS.ARRAY% &
	&
	\ IF	(STATUS AND 255%)<>DHI% OR &
		RECOUNT<>0% OR &
		FSS.ARRAY%(27%)<>0% OR &
		(FSS.ARRAY%(28%) AND 143%)<>0% OR &
		(FSS.ARRAY%(30%) AND 16%)=0% OR &
		(FSS.ARRAY%(30%) AND 128%)<>0% THEN &
	&
			TEMP$="?Illegal device specified: "+DEV.NAM$ &
	\		GOTO 16170 &
	&
		!ONLY A DEVICE SPECIFIED? &

16130	FSS.ARRAY%(25%)=0% UNLESS FSS.ARRAY%(26%) &
	\ TEMP$="_"+ &
		CHR$(FSS.ARRAY%(23%))+CHR$(FSS.ARRAY%(24%))+ &
		NUM1$(FSS.ARRAY%(25%))+":" &
	&
	\ IF	FSS.ARRAY%(30%) AND 64% THEN &
			PRINT  IF CCPOS(0%) &
	\		PRINT "%Warning: "; DEV.NAM$; &
				" is a logical device:  "; &
				TEMP$; " will be used" &
	\		DEV.NAM$=TEMP$ &
	\		GOTO 16120 &
		!RE-CREATE THE DEVICE STRING.  IF IT WAS A LOGICAL, &
		! REPORT THE FACT AND RE-SCAN THE STRING. &

16140	ON ERROR GOTO 16150 &
	\ OPEN TEMP$ FOR INPUT AS FILE 1% &
	\ CLOSE -1% &
	\ GOTO 16190 &
		!TRY TO OPEN THE DEVICE.  IF SUCCESSFUL, DO A &
		! RESET AND GO AWAY. &

16150	TEMP$="?Open failure on "+TEMP$+" "+ &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\ RESUME 16170 &
		!HANDLE RANDOM OPEN ERRORS. &

16170	PRINT IF CCPOS(0%) &
	\ PRINT TEMP$ &
	\ TEMP$="" &
		!PRINT A MESSAGE AND RETURN UNSUCCESSFULLY. &

16190	ON ERROR GOTO 19000 &
	\ FNCHEK.DEV$=TEMP$ &
	\ FNEND &

19000	! &
	&
	&
	!	S T A N D A R D   E R R O R   T R A P &
	&

19010	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)) &
	\ IF ERR=52% AND ERL=2130% THEN &
		PRINT "?Illegal number of iterations selected." &
	\	RESUME 2130 &
	! GET THE ERROR MESSAGE TEXT &

19015	IF ERR=28% OR ERR=11% AND ERL<2200% THEN RESUME 32767 &
	\ CLOSE 1% &
	! USER CTRL/C OR CTRL/Z. &
	! CLOSE THE TEST KEYBOARD. &
	! THIS CLOSE IS IMPORTANT IF WE ARE DETACHED. &
	! IT FREES THE KEYBOARD BEFORE WE HIBERNATE SO THAT THE USER &
	! CAN DO SOMETHING. &

19020	PRINT &
	\ PRINT CVT$$(RIGHT(S$,3%),4%);" at line";ERL;"in KBEXER ";I$ &
	! PRINT ERROR MESSAGE TEXT &

19030	RESUME 32767 &
	! GO EXIT &

32767	END
