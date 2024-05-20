2!		PROGRAM		: DSKEXR.BAS
5!		VERSION		: V10.1
6!		EDIT		: K
7!		EDIT DATE	: 14-MAY-92
10	EXTEND			!USE EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1976, 1992 by &
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
	&
	! DSKEXR BEGINS BY ASKING SEVERAL QUESTIONS TO DETERMINE THE DRIVE &
	! TYPE, UNIT NUMBER, AND NUMBER OF TEST ITERATIONS TO BE PERFORMED. &
	! AFTER THIS DIALOGUE, THE EXERCISER OPENS THE DISK FILE WITH A &
	! LENGTH OF ZERO.  A PATTERN BUFFER IS THEN LOADED WITH ONE OF &
	! FOUR PATTERNS (ALL ZEROES, ALL ONES, 125252, AND 52525) AND &
	! THE FILE IS WRITTEN AND EXTENDED ONE BLOCK AT A TIME.  EACH BLOCK &
	! IS THEN READ AND COMPARED.  THIS PROCEDURE IS REPEATED FOR EACH &
	! PATTERN.  UPON COMPLETION OF ALL ITERATIONS FOR A DRIVE, A STATUS &
	! REPORT WILL BE PRINTED. &
	! &
	! MULTIPLE COPIES OF THIS EXERCISER MAY BE RUN TO TEST MORE THAN &
	! ONE UNIT, OR TWO INCREASE THE LOAD ON A SINGLE UNIT. &
	! &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
302!		1		DISK EXERCISER FILE
399!
400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !
402 !		A$	TEMPORARY BUFFER &
	&
	!	A%	RESULTS IN ADDING TWO BITS(5,6) FROM A SYS CALL &
	&
	!	A%(512)	BUFFER ARRAY &
	&
	!	A1%	RESULTS OF A PEEK &
	&
	!	B%	RESULTS IN ADDING TWO BITS(5,6) FROM A SYS CALL &
	&
	!	B1%	RESULTS OF A PEEK &
	&
	!	B2%	COUNTER &
	&
	!	D$	UNIT TO TEST &
	&
	!	D$(10)	LIST OF ALL POSSIBLE DISKS TO TEST &
	&
	!	D%	DISK UNIT &
	&
	!	D1%	NUMBER OF BLOCKS &
	&
	!	E(100)	ERROR COUNTER &
	&
	!	E1	ERROR TALLY &
	&
	!	ERL	ERROR LINE &
	&
	!	ERR	TYPE OF ERROR &
	&
	!	F$	UNIQUE FILE NAME &
	&
	!	H$	DISK TYPE &
	&
	!	I$	HEADER &
	&
	!	I%	COUNTER &
	&
	!	J%	COUNTER &
	&
	!	J1%	COUNTER &
	&
	!	L$	PLURAL MAKER &
	&
	!	P1%	?? &
	&
	!	P2%	?? &
	&
	!	P3$	JOB NUMBER &
	&
	!	P5%	ITERATION COUNTER &
	&
	!	Q$(20)	RESULTS OF V1$ (DEVICE TYPE ON SYSTEM) &
	&
	!	Q%(20)	RESULTS OF V$  (NUMBER OF EACH DEVICE) &
	&
	!	R%	RECORD NUMBER &
	&
	!	S$	ERROR MESSAGE &
	&
	!	T$	CONSTANT FOR FIELD &
	&
	!	T%(512)	STORAGE BUFFER &
	&
	!	V$	SYS CALL TO TABLE MONITOR #1 &
	&
	!	V1$	SYS CALL TO TABLE MONITOR #2 &
	&
	!	W%	COUNTER &
	&
	!	X%,Z%	COUNTER &
	&

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

910	DIM A%(4096%),T%(4096%),FSS.ARRAY%(30%) &
		!BUFFER ARRAYS, STRING SCAN ARRAY. &

920	DATA 0,0,255,255,170,170,85,85
930	! PATTERN DATA FOR GENERATING BUFFERS &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ TEMP$=SYS(CHR$(6%)+CHR$(-21%)) &
	! SET UP STANDARD ERROR TRAP AND DROP ALL PRIV'S &

1010	I$="V10.1-K" &
	! SET UP VERSION AND EDIT NUMBER &

1020	PRINT IF CCPOS(0%) &
	\ PRINT "DSKEXR"; CHR$(9%); I$; CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%), TIME$(0%) &
	! PRINT HEADER &

1030	JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)))/2% &
	\ F$="DSKE"+NUM1$(JOB.NO%)+".TMP/MODE:32" &
	! DEFINE GARBAGE STRING TO FILL RECEIVE BUFFER. &
	! GET THE CURRENT JOB NUMBER. &
	! CREATE UNIQUE DISK FILE NAME FOR TENTATIVE ENTRY &

1100	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Disk drive to test"; &
	\ INPUT LINE H$ &
	\ GOTO 32767 IF ASCII(H$)=27% &
	\ H$=CVT$$(H$,-2%) &
	\ H$=FNCHEK.DEV$(H$,0%) &
	\ GOTO 1100 UNLESS LEN(H$) &
		!GET UNIT TO TEST.  BACK UP (GO AWAY) ON <ESC>, ELSE &
		! SEE IF WE GOT A PROPER DEVICE. &
		!ARGS TO FNCHEK.DEV$(): &
		!	H$ STRING TO CHECK/OPEN &
		!	0% DISK DHI &

1160	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Number of test iterations <8>"; &
	\ INPUT LINE P5$ &
	\ IF ASCII(P5$)=27% THEN &
		GOTO 1100 &
		!GET NUMBER OF ITERATIONS &
		! AND GO BACK ON <ESC>. &

1162	P5$=CVT$$(P5$,-2%) &
	\ P5$ = "8" UNLESS LEN(P5$) &
	\ P5 = VAL(P5$) &
	\ IF P5=-1. THEN &
		P5=1000000000000000000000000000000. &
	  ELSE	IF P5<0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 1160 &
	! Check for correct iteration input. &
	&

1165	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Blocks to XFER <1>"; &
	\ INPUT LINE P5$ &
	\ GOTO 1160 IF ASCII(P5$)=27% ! AND GO BACK ON <ESC>. &
	\ P5$ = CVT$$(P5$,-2%) &
	\ P5$ = "1" UNLESS LEN(P5$) &
	\ B.S% = VAL(P5$) &
	\ GOTO 1165 IF B.S%>8% &
	\ PRINT &
	\ R.S% = B.S%*512% &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE D$ &
	\ GOTO 1165 IF ASCII(D$)=27% &
	\ D$=CVT$$(D$,-2%) &
	\ IF (ASCII(D$) AND -33%)=ASCII("Y") THEN &
		PRINT &
	\	PRINT "Type ATTACH"; JOB.NO%; "when SYSTAT shows job"; &
			JOB.NO%; "in HB state." &
	\	PRINT &
	\	V$=SYS(CHR$(6%)+CHR$(7%)) &
			!PRINT A MESSAGE AND DETACH, IF DESIRED. &

1170	D1% = 1000% &
	\ w.c=time(0) &
	\ cp.u=time(1) &
	\ d.a%=peek(512%) &
	! SET NUMBER OF BLOCKS TO 1000 &
	! MAKE EACH WRITE DO A SEEK BY CAUSING THE SATT TO CHANGE &
	! Get current wall, cpu time, and date &
	&

1300	FOR I=1. TO P5 &
		! MAIN ITERATION LOOP &

1310		RESTORE &
		! RESET PATTERN DATA POINTER &

1330		OPEN H$+F$ FOR OUTPUT AS FILE 1%, recordsize r.s% &
		! OPEN FILE MARKED FOR DELETION. &

1340		FIELD #1%, r.s% AS T$ &

1400		FOR Z%=1% TO 4% &
			! DO ALL FOUR PATTERNS &

1410			READ P1%,P2% &
			\ A%(0%)=r.s% &
			\ A%(J%)=P1% 	FOR J%=1% TO r.s%-1% STEP 2% &
			\ A%(J%)=P2%	FOR J%=2% TO r.s% STEP 2% &
			\ CHANGE A% TO A$ &
			! FILL BUFFER WITH SELECTED PATTERN &

1450			LSET T$=A$ &
			! MOVE DATA INTO I/O BUFFER &

1460			ON ERROR GOTO 1465 &
	\		PUT #1%, RECORD R%	FOR R%=1% TO D1% step b.s% &
	\		GOTO 1470 &
			! WRITE OUT THE WHOLE FILE WITH THIS PATTERN &

1465			GOTO 19000 UNLESS ERR=4% AND R%>1% &
	\		D1%=R%-b.s% &
	\		RESUME 1470 &
			! HANDLE OUT OF SPACE CONDITIONS. ADJUST SIZE FROM 1000 &
			! TO WHATEVER WAS FREE (>1). &

1470			ON ERROR GOTO 19000 &
	\		A%(J%)=A%(J%)+A%(J%+1%)*256% FOR J%=1% TO &
					r.s%-1% STEP 2% &
			! CONVERT THE ARRAY FOR COMPARISON &

1500			FOR R%=1% TO D1% step b.s% &
				! START COMPARISON LOOP FOR THIS PATTERN &

1505				LSET T$=STRING$(r.s%,204%) &
				! FILL I/O BUFFER WITH GARBAGE BEFORE THE READ &

1510				GET #1%, RECORD R% &

1520				IF T$=A$ THEN 1590 ELSE CHANGE T$ TO T% &
				! COMPARE DATA READ WITH DATA WRITTEN &

1540				E=E+1. &
				  IF T%(W%)+T%(W%+1%)*256%<>A%(W%) &
					FOR W%=1% TO r.s%-1% STEP 2% &
				! CHECK EACH WORD IN BLOCK AND INCREMENT &
				! COUNT ON EACH ERROR &

1590			NEXT R% &
			\ GOSUB 6000 IF E<>0. &
			! NEXT RECORD &
			! IF THERE WAS AN ERROR ON THIS PASS, REPORT IT. &

1600		NEXT Z% &
		! NEXT PATTERN &

1610		CLOSE -1% &
		! CLOSE AND KILL FILE &

1620	NEXT I &
	\ w.c1=time(0) &
	\ cp.u1=time(1) &
	\ w.c2=((86400.*(peek(512%)-d.a%))-w.c)+w.c1 &
	\ w.c%=w.c2/60		! minutes &
	\ w.c1%=w.c2-(60.*w.c%)	! seconds &
	\ w.c2%=w.c%/60%	! hours &
	\ w.c%=w.c%-(60.*w.c2%)	! left over minutes &
	\ cp.u=cp.u1-cp.u &
	! END OF MAIN ITERATION LOOP &
	! Calculate wall clock time and CPU time &

1630	GOSUB 6030 &
	! PRINT OUT RUN STATISTICS. &

3000	GOTO 32767 &
	! DONE SO EXIT HERE &

6000	! &
	&
	&
	!	S T A T I S T I C S   P R I N T E R &
	&

6010	GOTO 6020 IF E1=0. &
	\ PRINT &
	\ PRINT "DSKEXR Error Statistics for ";H$ &
	\ PRINT &
	\ PRINT "Drive","Pass","File size","Error count" &
	\ PRINT &

6020	PRINT H$,I,D1%,E &
	\ E1=E1+E &
	\ E=0. &
	\ RETURN &
		! PRINT OUT THE NUMBER OF ERRORS FOR THIS PASS, &
		! ADD ONTO THE TOTAL ERROR COUNT, RESET THE &
		! PASS ERROR COUNT, AND RETURN. &

6030	PRINT &
	\ PRINT "FILE SIZE USED WAS";D1%;"BLOCKS" &
	\ PRINT "?"; NUM1$(E1); IF E1>0. &
	\ PRINT "No"; IF E1=0 &
	\ PRINT " error"; &
	\ PRINT "s"; UNLESS E1=1. &
	\ PRINT " detected on "; H$ &
	\ PRINT &
	\ print "Time used          CPU time used" &
	\ print " ";num1$(w.c2%);":";num1$(w.c%);":";num1$(w.c1%); &
					tab(20%);cp.u/10;" Sec." &
	\ print &
	\ GOTO 6090 IF E1=0. &
	\ PRINT "Please check the system error log for a list" &
	\ PRINT "of possible bad blocks." &
	! TOTAL AND PRINT TOTAL NUMBER OF ERRORS &

6090	RETURN &

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
	\ OPEN TEMP$+F$ FOR OUTPUT AS FILE 1% &
	\ PUT #1% &
	\ CLOSE -1% &
	\ GOTO 16190 &
		!TRY TO OPEN THE FILE ON DEVICE.  IF SUCCESSFUL, MAKE SURE A &
		! DISK IS MOUNTED. THEN RESET AND GO AWAY. &

16150	TEMP$="?Open failure on "+TEMP$+F$+" "+ &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\ RESUME 16170 &
		!HANDLE RANDOM OPEN ERRORS. &

16170	CLOSE -1% &
	\ PRINT IF CCPOS(0%) &
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
		! GET ERROR MESSAGE &
	\ RESUME 32767 IF ERR=11% AND ERL<1200% &
		! EXIT QUIETLY IF USER TYPED CTRL/Z TO A PROMPT. &
	\ IF ERR=52% THEN &
		PRINT "?Illegal number of iterations selected." IF ERL=1162% &
	\	PRINT "?Illegal number of blocks to transfer." IF ERL=1165% &
	\	RESUME 1160 IF ERL=1162% &
	\	RESUME 1165 IF ERL=1165% &
			! CATCH BAD NUMBERS. &

19015	PRINT IF CCPOS(0%) &
	\ PRINT CVT$$(RIGHT(S$,3%),4%);" at line";ERL;"in DSKEXR ";I$ &
	! PRINT ERROR MESSAGE &

19030	CLOSE -X% FOR X%=1% TO 12% &
	\ RESUME 32700 &
	! RESET ALL FILES AND CLEAR ERROR FLAG. &

32700	PRINT "?DSKEXR aborting at", TIME$(0%) &

32767	END
