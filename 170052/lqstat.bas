2!		PROGRAM		: LQSTAT
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
  !		      Copyright (C) 1979, 1991 by &
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

20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	! &
	! &

21	! VER/ED	EDIT DATE	REASON &
	! &
	! V9.4-04	07-May-87	JFM  	Added error messages if data &
	!					runs out in .DIR or .VAL files &

100	! &
	! &
	! &
	!	P R O G R A M   D E S C R I P T I O N &
	! &
	! &
	! LQSTAT extracts and prints the data stored by QSTATS. &

300	! &
	! &
	! &
	!	I / O    C H A N N E L S &
	! &
	! &
	! &

301!	     CHANNEL #		USED FOR &
	! &
	!	1		filnam.DIR &
	!	2		filnam.VAL &
	!	6		filnam.LST - user output file &
	!	8		filnam.VIR &

400	! &
	! &
	!	V A R I A B L E    D E F I N I T I O N S &
	! &

401!	&
   !	BIG.OUTER%	loop controller &
   !	D%		utility variable &
   !	DUMMY$		utility variable &
   !	END.TIME$	time QSTATS run was ended &
   !	F.NAME$		file prefix &
   !	HI.END%		high range of samples to check &
   !	HI.OUTER%	loop controller &
   !	I%		utility variable &
   !	J%		utility variable &
   !	LOW.END%	sample to start with &
   !	LOW.OUTER%	loop controller &
   !	M%		utility variable for tabs &
   !	MAX.OBS%	max number of observations allowed &
   !	MAX.VARS%	max number of variables allowed &
   !	MBASE%		utility variable for data column tabs &
   !	NOW.OUTER%	loop controller &
   !	NUM.DISKS%	number of disks &
   !	NUM.REC%	number of records &
   !	NUM.SAMPLE%	number of samples taken by QSTATS &
   !	SAMPLE%		sample number &
   !	START.TIME$	time QSTATS run was started &
   !	TEMP		temporary floating point variable &
   !	TEST.NAME$	name of the test run by QSTATS &
   !	TICKER &
   !	USE.SIZE% &
   !	VERSION$	version of LQSTAT &
   !	W.NAME$		name of file being opened &
   !	&
   ! NO FUNCTIONS OR SUBROUTINES ARE USED. &

900	! &
	! &
	! &
	!	D I M E N S I O N    S T A T E M E N T S &
	! &
	! &
	! &

910	DIM #8%, TABLE(363%,60%) &
		! STORES INFO EXTRACTED FROM QSTATS CREATED FILES. &
		! &
		! NOTE THAT TABLE MUST BE DIMENSIONED AS &
		! TABLE(MAX.VARS%,MAX.OBS%) &

920	DIM  LABELS$(363%),WORK.TABLE(10%,3%) &
		! LABELS() - REPORT COLUMN HEADERS &

930		MAX.VARS% = 363% &
	\	MAX.OBS%  =  60% &
	!  NOTE THAT TABLE IS DIM AS (MAX.VARS%,MAX.OBS%) &
	!	AND LABELS$ AS (MAX.VARS%) &
	&
	&

999	! &
	! &
	! &
	!	S T A R T    O F    P R O G R A M &
	! &
	! &
	! &

1000	ON ERROR GOTO 19000 &
		! SET UP STANDARD ERROR TRAP. &
	\ DUMMY$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(-1%)) &
		! DROP TEMPORARY PRIVILEGES. &

1010	IDENT.STG$ = "V10.1-A" &

1020	PRINT IF CCPOS(0%) &
	\ PRINT "LQSTAT";CHR$(9%);IDENT.STG$;CHR$(9%); &
	 CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! PRINT THE SYSTEM HEADER &

1100	! &
	! &
	! &
	!	M A I N    L I N E    C O D E &
	! &
	! &
	! &

1150	INPUT "Generic stats file prefix <QSTAT>";F.NAME$ &
	\ F.NAME$ = CVT$$(F.NAME$,-2%) &
	\ F.NAME$ = "QSTAT" UNLESS LEN(F.NAME$) &
	\ W.NAME$ = F.NAME$ + ".DIR" &
	\ OPEN W.NAME$ FOR INPUT AS FILE #1% &
	\ W.NAME$ = F.NAME$ + ".VAL" &
	\ OPEN W.NAME$ FOR INPUT AS FILE #2% &
	\ W.NAME$ = F.NAME$ + ".VIR" &
	\ OPEN W.NAME$ FOR OUTPUT AS FILE #8% &
	\ W.NAME$ = F.NAME$ + ".LST" &
	\ OPEN W.NAME$ FOR OUTPUT AS FILE #6% &
	\ PRINT "Output file is ";W.NAME$ &
	\ I% = 0% &
		! OPEN SOME FILES. &
		! CLEAR VARIABLE COUNTER. &

1210	INPUT #1%, TEST.STR$ &
	\ GOTO 1250 IF TEST.STR$ = "$$$HEADER$$$" &
	\ I% = I% + 1% &
	\ GOTO 1215	IF I% > MAX.VARS% &
	\ LABELS$(I%) = TEST.STR$ &
	\ TABLE(I%,0%) = 1% &
	\ TABLE(I%,0%) = 2% IF ASCII(TEST.STR$)=37% &
	\ TABLE(I%,0%) = 3% IF ASCII(TEST.STR$)=91% &
	\ GOTO 1210 &
		! EXTRACT LABELS AND SET-UP FLAGS. &
		! 37 -> "%" &
		! 91 -> "[" &
	&

1215	NUM.REC% = I% &
	\ GOTO 1265 &

1250	INPUT #1%, NUM.SAMPLE% &
	\ INPUT #1%, NUM.REC% &
	\ INPUT #1%, TICKER &
	\ INPUT #1%, NUM.DISKS% &
	\ INPUT #1%, AT.FILE% &
	\ INPUT #1%, START.TIME$ &
	\ INPUT #1%, END.TIME$ &
	\ INPUT #1%, TEST.NAME$ &
	\ GO TO 1260 IF NUM.SAMPLE% > MAX.OBS% &
	\ GO TO 1265 IF NUM.REC%    > MAX.VARS% &
	\ GO TO 1300 &
		! &

1260	PRINT "Number of observations ";NUM1$(NUM.SAMPLE%);" exceeds maximum" &
	\ GOTO 1270 &

1265	PRINT "Number of variables ";NUM1$(NUM.REC%);" exceeds maximum" &

1270 	PRINT "Change lines 910-930 and rerun" &
	\ GOTO 32767 &

1300	PRINT "Number of samples =";NUM.SAMPLE% &
	\ INPUT "Low Range Value <2%>";DUMMY$ &
	\ DUMMY$ = CVT$$(DUMMY$,-2%) &
	\ DUMMY$ = "2" UNLESS LEN(DUMMY$) &
	\ LOW.END% = VAL(DUMMY$) &
	\ INPUT "High Range Value <Number of samples -1%>";DUMMY$ &
	\ DUMMY$ = CVT$$(DUMMY$,-2%) &
	\ DUMMY$ = NUM1$(NUM.SAMPLE%) UNLESS LEN(DUMMY$) &
	\ HI.END% = VAL(DUMMY$) &
	\ HI.END% = NUM.SAMPLE% IF HI.END%>NUM.SAMPLE% &
	\ LOW.END% = 2% IF LOW.END%<0% OR LOW.END%>HI.END% &
	\ USE.SIZE% = HI.END% - LOW.END% +1% &
		! LET THE USER KNOW HOW MANY SAMPLES WERE TAKEN. &
		! SET UP RANGES. &

1350	PRINT #6%, CHR$(10%); FOR I% = 1% TO 10% &
	\ PRINT #6%, " Experiment Name:	";TEST.NAME$ &
	\ PRINT #6%, " Number of Samples:	";NUM1$(NUM.SAMPLE%) &
	\ PRINT #6%, " Number of Variables:	";NUM1$(NUM.REC%) &
	\ PRINT #6%, " Tick Rate:		";NUM1$(TICKER) &
	\ PRINT #6%, " Number of Disk Units:	";NUM1$(NUM.DISKS%) &
	\ PRINT #6%, " Experiment Started at ";START.TIME$ &
	\ PRINT #6%, " Experiment Ended at   ";END.TIME$ &
		! PRINT THE INITIAL HEADERS. &

1400	PRINT &
	\ PRINT "Begin table loading" &
	\ INPUT #2%, TABLE(I%,J%) &
		FOR I%=1% TO NUM.REC% &
			FOR J%=1% TO HI.END% &
		! GET ENTRIES FROM .VAL FILE &
	&

1450	FOR I% = 1% TO NUM.REC% &
	\	D%=TABLE(I%,0%) &
	\	GOTO 1500 IF D% = 3% &
	\	FOR J% = NUM.SAMPLE% TO 2% STEP -1% &
	\		TABLE(I%,J%) = TABLE(I%,J%) - TABLE(I%,J%-1%) &
	\		IF D%=2% THEN IF TABLE(1%,J%)=0% &
				THEN TABLE(I%,J%)=0% &
				ELSE TABLE(I%,J%)=100.*(TABLE(I%,J%)) / &
					TABLE(1%,J%) &

1460		NEXT J% &
		! &
		! TABLE(I,0)=2 -> "%" &
		! TABLE(I,0)=3 -> "[" &
		! &

1500	NEXT I% &
	\ PRINT "Table loading complete " &

1550	FOR BIG.OUTER% = 1% TO NUM.REC% STEP 5% &
	\	PRINT #6%, CHR$(12%);SPACE$(10%); &
	\	LOW.OUTER% = BIG.OUTER% &
	\	HI.OUTER% = LOW.OUTER% + 4% &
	\	HI.OUTER% = NUM.REC% IF HI.OUTER% > NUM.REC% &
		! 5 PER PAGE. &

1600		M% = 0% &
	\	FOR I% = LOW.OUTER% TO HI.OUTER% &
	\		M% = M% + 1% &
	\		PRINT #6%, TAB(M%*20%);LABELS$(I%); &
	\	NEXT I% &
	\	PRINT #6% &
	\	PRINT #6% &
		! PRINT COLUMN HEADERS. &

1610		WORK.TABLE(I%,J%) = 0. FOR J% = 0% TO 3% FOR I% = 1% TO 10% &

1620		FOR SAMPLE% = LOW.END% TO HI.END% &
	\		MBASE% = 0% &
	\		PRINT #6%, SPACE$(4%);SAMPLE%;TAB(11%); &
		! PRINT SAMPLE NUMBER. &

1630			FOR NOW.OUTER% = LOW.OUTER% TO HI.OUTER% &
	\			MBASE% = MBASE% + 1% &
	\			TEMP=TABLE(NOW.OUTER%,SAMPLE%) &
	\			PRINT #6%, &
				 TAB(MBASE%*20%);TEMP; &
	\			WORK.TABLE(MBASE%,1%) = WORK.TABLE(MBASE%,1%) &
				 + TEMP &
	\			WORK.TABLE(MBASE%,2%) = WORK.TABLE(MBASE%,2%) &
				 + TEMP &
				 * TEMP &
	\		NEXT NOW.OUTER% &
	\		PRINT #6%, " " &
		! PRINT THE VALUE &

1650		NEXT SAMPLE% &

1660		PRINT #6%, CHR$(10%);CHR$(10%);"   MEAN ";TAB(11%); &
	\	FOR I% = 1% TO MBASE% &
	\		WORK.TABLE(I%,1%) = WORK.TABLE(I%,1%) / USE.SIZE% &
	\		PRINT #6%, TAB(I%*20%);WORK.TABLE(I%,1%); &
	\	NEXT I% &
	\	PRINT #6%, " " &
		! PRINT THE MEAN &
	&
	\	PRINT #6%, CHR$(10%);CHR$(10%);"   SDEV ";TAB(11%); &
	\	FOR I% = 1% TO MBASE% &
	\		TEMP = WORK.TABLE(I%,2%) - &
			 (USE.SIZE% * (WORK.TABLE(I%,1%)^2)) &
	\		WORK.TABLE(I%,2%) = 0. &
	\		WORK.TABLE(I%,2%) = SQR(TEMP)/(USE.SIZE% - 1%) &
				IF TEMP > 0. &
	\		PRINT #6%, TAB(I%*20%);WORK.TABLE(I%,2%); &
	\	NEXT I% &
	\	PRINT #6%, " " &
		! PRINT THE STANDARD DEVIATION &

1750	NEXT BIG.OUTER% &
		! FALL THROUGH TO PROGRAM CLEAN UP. &

9000	! &
	! &
	! &
	!	P R O G R A M    C L E A N    U P &
	! &
	! &
	! &

9010	CLOSE #1% &
	\ CLOSE #2% &
	\ CLOSE #6% &
	\ CLOSE #8% &
	\ GOTO 32767 &
	&

19000	! &
	! &
	!	E R R O R    H A N D L I N G &
	! &
	! &

19010	RESUME 9000 IF ERL=19010% OR &
		(ERR=11% AND (ERL=1150% OR ERL=1300%)) &
	\ E$=" at line "+NUM1$(ERL) &
	\ E$=" - "+W.NAME$ IF ERL=1150% &
	\ GOTO 19020 IF ERR=11% AND (ERL=1400%) &
	\ GOTO 19030 IF ERR=11% AND (ERL=1210% OR 1250%) &
	\ PRINT "?LQSTAT - "; &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%);E$ &
	\ RESUME 9000 &
		! STANDARD ERROR TRAP &

19020	PRINT "?LQSTAT - "; &
		"?Unexpected end of file - in ";F.NAME$;".VAL" &
	\ RESUME 9000 &

19030	PRINT "?LQSTAT - "; &
		"?Unexpected end of file - in ";F.NAME$;".DIR" &
	\ RESUME 9000 &

32767	END
