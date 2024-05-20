2!		PROGRAM		: CPUTST.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND
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

101	! THE PROGRAM REQUESTS THE NUMBER OF WALL CLOCK MINUTES DESIRED FOR &
	! RUN TIME (# OF MINUTES DESIRED ).  THE PROGRAM CHECKS FOR THE &
	! EXPERATION OF THIS REQUESTED TIME AT STRATEGIC POINTS DURING A &
	! NORMAL RUN SEQUENCE ( 7 CHECKPOINTS) . &
	! &
	! THE TWO TWO DIMENSIONAL ARRAYS ARE THEN CLEARED AND/OR SET TO &
	! SPECIFIC VALUES.  MATRIX "A" IS THEN FILLED WITH RANDOM NUMBERS FROM &
	! THE BASIC-PLUS RANDOM NUMBER GENERATOR CALL.  MATRIX "A" IS THEN &
	! TRANSPOSED AND THE RESULTS PLACED IN MATRIX "B".  THE TRANSPOSITION &
	! IS CHECKED BY THE EQUATION/STATEMENT A(I,J) = B(J,I) WITH "I" AND &
	! "J" BEING VARIED.  ANY ERRORS WILL BE REPORTED.  IF NO ERROR, THE &
	! PROGRAM GOES BACK TO THE MATRIX CLEAR INSTRUCTIONS.  WHEN THE TIME &
	! EXPIRES, THE PROGRAM WILL PRINT THE CPU SECONDS EXPENDED FOR THIS &
	! JOB. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
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

910	DIM A(30%,30%),B(30%,30%) &
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

1010	I$ = "V10.1-A" &
	! SET UP VERSION AND EDIT NUMBER &

1020	PRINT "CPUTST";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! SET UP HEADER AND PRINT IT &

1030	RANDOMIZE &
	\ JOB.NO%=ASCII(SYS(CHR$(6%)+CHR$(9%)))/2% &
		! RANDOMIZE RANDOM NUMBER GENERATOR. &
		! GET OUR JOB NUMBER. &

1040	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "How many minutes to run <15>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 32767 IF ASCII(TEMP$)=27% &
	\ TEMP$=CVT$$(TEMP$,254%) &
	\ TEMP$="15" UNLESS LEN(TEMP$) &
	\ K=VAL(TEMP$) &
	\ IF K=-1. THEN &
		K=1000000000000000000000000000000. &
	  ELSE	IF K<=0. THEN &
			PRINT IF CCPOS(0%) &
	\		PRINT "?Illegal number of iterations selected" &
	\		GOTO 1040 &
		! GET TOTAL TESTING TIME. &

1050	PRINT IF CCPOS(0%) &
	\ PRINT &
	\ PRINT "Detach <No>"; &
	\ INPUT LINE TEMP$ &
	\ GOTO 1040 IF ASCII(TEMP$)=27% &
	\ IF ASCII(CVT$$(TEMP$,254%))=89% THEN &
		PRINT "Type ATTACH";JOB.NO%;"when SYSTAT shows job"; &
			JOB.NO%;"in HB state." &
	\	PRINT &
	\	TEMP$=SYS(CHR$(6%)+CHR$(7%)) &
		! DETACH IF HE WANTS TO. &

1070	T=TIME(1%) &
	\ L%=TIME(0%) /60% &
	! GET CPU USAGE IN .1 SEC AND MINUTES SINCE MIDNIGHT &

1100	FOR I%=1% TO 30% &

1110	FOR J%=1% TO 30% &

1120		IF J%=I% THEN A(I%,J%)=1% ELSE A(I%,J%)=0% &
		! DIAG=1 OFF DIAG=0 &

1130	NEXT J% \ NEXT I% &
	! CREATE IDENTITY MATRIX A &

1140	GOSUB 15000 &
	! CHECK TIME USED &

1160	B(I%,J%)=0% FOR J%=1% TO 30% FOR I%=1% TO 30% &
	&
	! ZERO MATRIX B &

1170	GOSUB 15000 &
	! CHECK TIME USED &

1200	D=0 &

1220	FOR I%=1% TO 30% &

1230		FOR J%=1% TO 30% &

1240		D=RND &
	\	A(I%,J%)=D &

1260		GOSUB 15000 &
		! CHECK TIME USED &

1270	NEXT J% \ NEXT I% &
	! INITIALIZE MATRIX A WITH RANDOM VALUES &
	&

1290	GOSUB 15000 &
	! CHECK TIME USED &

1310	B(I%,J%)=A(J%,I%) FOR I%=1% TO 30% FOR J%=1% TO 30% &
	! B=TRN(A) &

1320	GOSUB 15000 &
	! CHECK TIME USED &

1330	FOR I%=1% TO 30% &

1340		FOR J%=1% TO 30% &

1350		IF A(J%,I%) <> B(I%,J%) THEN GO TO 1430 &

1360		GOSUB 15000 &
		! CHECK TIME USED &

1370	NEXT J% \ NEXT I% &

1390	GOSUB 15000 &
	\ GOTO 1100 &
	! CHECK TIME THEN RESTART TEST &

1400	PRINT &
	\ PRINT NUM1$((TIME(1%)-T)/10%);" seconds of CPU time used." &
	\ PRINT &
	\ PRINT "CPUTST Finished at ";TIME$(0%) &
	\ GOTO 32767 &
	! ALL DONE TAKE NORMAL EXIT &

1430	PRINT "?CPUTST - Error in matrix transposition routine" &
	\ GOTO 32700 &
	! PRINT MESSAGE AND EXIT. &
	&

15000	! &
	&
	&
	!	T I M E   C H E C K &
	&
	&

15010	T5=TIME(0%) \ IF T5>=T6 THEN 15030 &
	! T5 = PRESENT # SEC SINCE MIDNIGHT &
	! IF MIDNIGHT HAS NOT PASSED THEN 15030 &

15020	T7=T7+86400 &
	! MIDNIGHT BOUNDARY CROSSED ADD ONE DAY OF SECONDS &

15030	T6=T5 \ T5=(T5+T7)/60% &
	! SET OLD VALUE  T5 NOW ELAPSED WALL CLOCK MINUTES &

15040	IF T5>=K+L% THEN 1400 ELSE RETURN &
	! IF ELAPSED WALL CLOCK TIME >= DESIRED THEN EXIT &
	&

19000	! &
	&
	&
	!	S T A N D A R D   E R R O R   T R A P &
	&

19010	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)) &
	\ RESUME 32767 IF ERR=11% AND ERL<=1050% &
	\ PRINT IF CCPOS(0%) &
	\ IF ERR=52% AND ERL=1040% THEN &
		PRINT "?Illegal number of minutes specified." &
	\	RESUME 1040 &

19020	PRINT CVT$$(RIGHT(S$,3%),4%);" at line";ERL;"in CPUTST ";I$ &
		! GET ERROR MESSAGE &
		! EXIT QUIETLY IF USER TYPED CTRL/Z TO A PROMPT. &
		! OTHERWISE, PRINT THE ERROR MESSAGE. &

19030	RESUME 32700 &
	! EXIT WITH ABORT MESSAGE &

32700	PRINT \ PRINT "CPUTST aborting at ";TIME$(0%) &

32767	END
