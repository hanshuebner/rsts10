2!		PROGRAM		: DXEXER.BAS
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

101!	THE DXEXER FLOPPY DISK EXERCISER IS DESIGNED TO TEST THE NORMAL &
   !	OPERATION OF THE RX11 DISK CONTROLLER AND ANY ONE OF 8 RX01 &
   !	FLOPPY DISK DRIVES.  A FILE OF ROTATING CHARACTERS IS WRITTEN
102!	ON THE DRIVE UNDER TEST AND THEN READ BACK AND COMPARED. &
   !	THE PROGRAM DISPLAYES THE NUMBER OF WORDS WRITTEN, &
   !	THE NUMBER OF WORDS READ, AND THE ERROR RATE.
103!	THE DEFAULTS ARE DRIVE 0, AND ITERATIONS 20. &
   !
299!
300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
302!		1		DXN:  NON-FILE STRUCTURED
399!
400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !
799!
800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
899!
900	! &
	&
	&
	&
	!	D I M E N S I O N   S T A T E M E N T S &
	&

910	DIM A%(512%),B%(512%), FSS.ARRAY%(30%) &
	! BUFFERS FOR ARRAYS, FILENAME STRING SCAN ARRAY. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ TEMP$=SYS(CHR$(6%)+CHR$(-21%)) &
	! SET UP STANDARD ERROR TRAP &
	! PERMANENTLY DROP TEMPORARY PRIVILEGES. &

1010	I$="V10.1-A" &
	! SET UP VERSION AND EDIT NUMBER &

1020	PRINT "DXEXER";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! SET UP HEADER AND PRINT IT &

1025	PRINT &
	\ PRINT "Warning: This exerciser will destroy data on the"; &
		" tested diskette." &
	! LET THEM KNOW THAT THIS MIGHT BE DANGEROUS. &

1030	JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)))/2% &
		! FIND OUR JOB NUMBER. &

1100	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Floppy drive to test"; &
	\ INPUT LINE P1$ &
	\ GOTO 32767 IF ASCII(P1$)=27% &
	\ P1$=CVT$$(P1$,254%) &
	\ P1$=FNCHEK.DEV$(P1$,18%) &
	\ GOTO 1100 UNLESS LEN(P1$) &
		! GET DRIVE TO TEST AND CHECK IT. &

1110	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of iterations <20>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1100 IF ASCII(TEMP$)=27% &
	\ TEMP$=CVT$$(TEMP$,254%) &
	\ TEMP$="20" UNLESS LEN(TEMP$) &
	\ Z8=VAL(TEMP$) &
	\ IF Z8=-1. THEN &
		Z8=1000000000000000000000000000000. &
	  ELSE	IF Z8<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 1110 &
		! GET NUMBER OF ITERATIONS. &

1120	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1110 IF ASCII(TEMP$)=27% &
	\ IF ASCII(CVT$$(TEMP$,254%))=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	TEMP$=SYS(CHR$(6%)+CHR$(7%)) &
		! DETACH IF HE WANTS TO. &
	&

1130	N2% = 150% &
	! SET THE NUMBER OF 512 BYTE BLOCKS TO 150 &
	! OR 600 128 BYTE SECTORS &

1200	FOR Z9=1. TO Z8 &
		! START MAIN ITERATION LOOP &

1210		OPEN P1$ FOR OUTPUT AS FILE 1%, MODE 0% &
	\	FIELD #1%, 512% AS T$ &
		! OPEN FLOPPY DESIRED NON-FILE STRUCTURED, BLOCK MODE &

1220		A%(0%)=512% &
	\	A%(1%)=0% &
		! SET UP THIS FIRST BLOCK &

1230		FOR I8%=1% TO N2% &
			! START LOOP FOR THIS FLOPPY WRITE SECTION &

1240			A%(I%)=(A%(I%-1%)+1%) AND 255%  FOR I%=2% TO 512% &
	\		CHANGE A% TO A$ &
	\		LSET T$=A$ &
			! CREATE STRING FOR WRITE - MOVE DATA TO I/O BUFFER &

1250			PUT #1% &
	\		W9=W9+1 &
			! WRITE AND INCREMENT COUNT &

1260			A%(1%)=(A%(512%)+1%) AND 255% &
			! ROTATING PATTERN FOR NEXT BLOCK &

1270		NEXT I8% &
		! END OF WRITE LOOP &

1280		CLOSE 1% &
		! CLOSE FILE FOR OUTPUT &

1290		OPEN P1$ FOR INPUT AS FILE 1%, MODE 0% &
	\	KILL P1$ &
	\	FIELD #1%,512% AS B$ &
		! OPEN FILE FOR COMPARISON &

1300		E1%=0% &
	\	A%(0%)=512% &
	\	A%(1%)=0% &
		! SET UP COMPARE ARRAY &

1310		FOR I8%=1% TO N2% &
			! START OF COMPARE LOOP &

1320			A%(I%)=(A%(I%-1%)+1%) AND 255% FOR I%=2% TO 512% &
	\		CHANGE A% TO A$ &
			! CREATE COMPARE STRING &

1330			GET #1% &
	\		R9=R9+1 &
			! GET RECORD AND INCREMENT COUNT &

1340			IF B$=A$ THEN 1380 ELSE CHANGE B$ TO B% &
			! COMPARE STRINGS &

1350			FOR I%=1% TO 512% &
			! START WORD BY WORD COMPARE &

1360			IF VAL(NUM$(A%(I%)))<>VAL(NUM$(B%(I%))) THEN E1%=E1% +1% &
			! COMPARE WORD BY WORD &

1370			NEXT I% &
			! END OF BLOCK CHECK &

1380			A%(1%)=(A%(512%)+1%) AND 255% &
			! ROTATE PATTERN &
	&

1390		NEXT I8% &
		! END OF READ COMPARE LOOP &

1400		IF E1%=0% THEN 1420 &
		! SKIP MESSAGE IF NO ERRORS &

1410		PRINT &
	\	PRINT "?DXEXER -";E1%; &
		"Errors detected on ";P1$;" during iteration";Z9 &

1420	NEXT Z9 &
	! END OF MAIN LOOP &

1430	GOSUB 6000 &
	! PRINT STATS &

1500	PRINT "DXEXER Finished at ";TIME$(0%) &

1510	GOTO 32767 &

6000	! &
	&
	&
	!	D A T A   P R I N T   R O U T I N E &
	&

6010	PRINT &
	\ PRINT "Floppy disk DXEXER I/O data" &
	\ PRINT &

6020	PRINT "Device","Reads","Words","Writes","Words" &
	\ PRINT P1$, &
	! ALL VALUES ARE ON TAB STOP &

6040	IF R9<1000. THEN PRINT NUM$(R9), ELSE &
		PRINT NUM$(R9/1000.)+"K", &
	! PRT # READS/ 1000 ADD "K" &

6050	IF (R9*256)<1000. THEN PRINT NUM$(R9*256), ELSE &
		PRINT NUM$((R9*256)/1000.)+"K", &
	! PRT # WORDS/ 1K ADD "K" &

6060	IF W9<1000. THEN PRINT NUM$(W9), ELSE &
		PRINT NUM$(W9/1000.)+"K", &
	! PRT # WRITES /1000 ADD "K" &

6070	IF (W9*256)<1000. THEN PRINT NUM$(W9*256) ELSE &
		PRINT NUM$((W9*256)/1000.)+"K" &
	! PRT # WDS /1000 ADD "K" &

6080	PRINT &
	\ RETURN &
	! END OF STATS ROUTINE &
	&

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
	\ GET #1% &
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

19010	S$=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
		! GET ERROR MESSAGE STRING &
	\ GOTO 32767 IF ERR=11% AND ERL<1200% &
		! QUIETLY GO AWAY IF USER TYPED CTRL/Z TO A PROMPT. &
	\ PRINT IF CCPOS(0%) &
	\ IF ERR=52% AND ERL=1110% THEN &
		PRINT "?Illegal number of iterations selected." &
	\	RESUME 1110 &

19020	PRINT IF CCPOS(0%) &
	\ PRINT S$;" at line";ERL;"in DXEXER ";I$ &
	\ PRINT &
	\ RESUME 19040 &
	! PRINT ERROR MESSAGE AND CLEAR ERROR STATUS. &

19040	GOSUB 6000 UNLESS ERL>=6000% &
	! PRINT STATS UNLESS ERROR OCCURRED THERE OR HERE. &

19060	ON ERROR GOTO 19070 &
	\ KILL P1$ &
	\ CLOSE #1% &
	\ GOTO 32767 &
	! ATTEMPT TO CLOSE FILE AND KILL &

19070	RESUME 32767 &
	! RECOVER FROM ERROR &

32767	END
