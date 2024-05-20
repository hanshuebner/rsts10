2!		PROGRAM		: ERRCPY
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
  !		      Copyright (C) 1974, 1991 by &
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
	! V9.0-13 VAM	23-Mar-85	Multiple privileges &
	! 9.3		10-NOV-86	ADD UNDERSCORE TO NL:'S &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110!	&
   !	 ERRCPY receives error messages and logs them to the the error &
   !	 file  ERRLOG.FIL.   The  error  file  consists of a one block &
   !	 header followed by N blocks of error records.  Error  records &
   !	 are  variable  length non-spanned.  The header block contains &
   !	 64-4 word control records (RECRD.LOG$-RECRD.ALW$).  The first &
   !	 62  control  records  hold  error totals for the 62 definable &
   !	 error types.  The last two hold general  control  information &
   !	 (TOT.LOG$-TOT.LIM$). &
   !	&
   !	 ERRCPY receives messages, updates specific and general header &
   !	 records and writes out the  error  received  itself.   ERRCPY &
   !	 sleeps  when  no  messages  are  pending.  While in the sleep &
   !	 state, the error file is closed, but a  copy  of  the  header &
   !	 block is maintained in the channel 3 buffer.  Upon awakening, &
   !	 ERRCPY opens the error file and receives  and  processes  all &
   !	 pending messages. &
   !	&
   !	 Error  file  generation,  operator  dialogue  and  error file &
   !	 validation is performed by a separate program called  ERRINT. &
   !	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #	USE &
   !
400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !
410!	B%		BLOCK NUMBER WHERE ERROR RECORD IS TO BE WRITTEN &
   !	BYPASS%		SWITCH TO BYPASS RECEIVE SYSTEM CALL IF THE &
   !			MESSAGE HAS ALREADY BEEN RECEIVED. &
   !				0% =BYPASS &
   !				-1% = DO THE RECEIVE &
   !	BYT.POS$	BYTE POSITION BLOCK TOT.USED$ WHERE NEXT ERROR &
   !		  	RECORD IS TO BE WRITTEN. &
   !	ER.COD%		ERROR TYPE &
   !	ER.LOG$		NUMBER OF ERRORS LOGGED AND REPEATED FOR ERROR &
   !			TYPE ER.COD%. &
   !	ER.RCV$		NUMBER OF ERRORS RECEIVED FOR ERROR TYPE ER.COD% &
   !			INCLUDING REPEATS. &
   !	I$		MESSAGE RECEIVE CALL COMMAND STRING &
   !	L%		TOTAL LENGTH OF ERROR RECORD &
   !	O%		OFFSET FROM START OF BLOCK TO 1ST CHAR POSITION &
   !	RECRD.ALW$	NUMBER OF ALLOWABLE ERROR RECORDS FOR ERROR &
   !			TYPE ER.COD%. &
   !	RECRD.LOG$	NUMBER OF RECORDS LOGGED FOR THIS ERROR TYPE ER.COD%. &
   !	REPEAT%		REPEAT COUNT &
   !	S%()		ARRAY WHICH HOLDS INFORMATION, INCLUDING A &
   !			PARAMETER STRING, RETURNED ON A RECEIVE. &
   !	S$		PARAMETER STRING OF RECEIVED ERROR MESSAGE &
   !	TOT.LOG$	TOTAL ERRORS LOGGED (INCLUDING REPEATS). &
   !	TOT.REC$	TOTAL ERRORS RECEIVED (INCLUDING REPEATS). &
   !	TOT.USED$	BLOCK NUMBER FOR START OF NEXT ERROR RECORD. &
   !	TOT.LIM$	LIMIT ON NUMBER OF ERROR BLOCKS IN FILE &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
805!		10000		PROCESS ERROR MESSAGE &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

910	DIM S%(40%) &
	! ARRAY TO HOLD DATA RETURNED ON RECEIVE &
	!	BYTE		CONTENTS &
	!	1-2		Meaningless &
	!	3		Data message subfunction &
	!	4		Job #*2 of local sender &
	!	5-6		PPN of sender &
	!	7-12		Not used by ERRCPY &
	!	13-14		Length of message transferred &
	!	15-20		Currently Unused &
	!	21		Code # for the error type &
	!	22		Job #*2 &
	!	23		# of the last field in the data &
	!	24		Error repeat count &
	!	25-26		Error sequence # &
	!	27-28		Date of error &
	!	29-30		Time of error &
	!	31-32		Seconds to next minute &
	!	33-40		Currently unused &
	! &
	! Bytes 21-40 are stored along with the Message. &
	! &
	! NOTE: if a message is received from someone other than the &
	! Monitor, bytes 21-40 are replaced by &
	!	21		55 = 'unrecognized MSG' &
	!	22		Job #*2 of sender &
	!	23-24		PPN of sender &
	!	25-26		0's &
	!	27-28		Current date &
	!	29-30		Current time &
	!	31-32		0's &
	!	33-40		not touched &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ PKG.LOC$="ERROR$:" &
	\ IF ENTRY.TYP%=2% &
		THEN 1030 &
		! SET UP STANDARD ERROR TRAP &
		! Set up the package location. &

1010	I$="V10.1-A" &
		! SET UP VERSION/EDIT #. &

1020	PRINT IF CCPOS(0%) &
	\ PRINT "ERRCPY";CHR$(9%);I$;CHR$(9%); &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	\ PRINT "?Illegal Entry - Please 'RUN ";PKG.LOC$;"ERRINT'" &
	\ GOTO 32760 &
		! PRINT THE HEADER &
		! TELL USER ERRCPY HAS NO RUN ENTRY &
		! NOW ABORT &

1030	S$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &
	\ IF NOT FNPRIV%("SWCTL") THEN &
		PRINT "?SWCTL privilege required" &
		\ GOTO 32760 &
	! They've got to have SWCTL to be here. &

1050	CHR6$=CHR$(6%) &
	\ S$=SYS(CHR6$+CHR$(-21%)+CHR$(0%)) &
	\ S$=SYS(CHR6$+CHR$(-20%)+CHR$(0%)) IF SWAP$ = "NOSWAP" &
	\ CHR6.22$=CHR6$+CHR$(22%) &
	\ REM.REC$=CHR6.22$+CHR$(0%)+CHR$(0%) &
		! Get our privileges back. &
		! LOCK JOB IN MEMORY, IF CALLED FOR. &
		! SET UP SOME FREQUENTLY USED STRINGS. &

1055	BYPASS%=-1% &
	\ OPEN "_NL:" AS FILE 3%, RECORDSIZE 512% &
	\ OPEN "_NL:" AS FILE 4%, RECORDSIZE 512% &
	\ FIELD #4%,512% AS M0$ &
	\ S$=SYS(REM.REC$) &
	\ S$=SYS(CHR6.22$+CHR$(1%)+CHR$(0%)+ &
		"ERRLOG"+STRING$(11%,0%)+CHR$(1%+2%)+ &
		CHR$(40%)+CHR$(SWAP%(40%))+CHR$(40%)) &
	\ I$=STRING$(6%,0%)+CHR$(4%)+STRING$(29%,0%) &
		! SET SWITCH ON (-1) TO DO A MESSAGE RECEIVE &
		! SET DETACH SWITCH. &
		! GET UP A 512 WORD BUFFER TO HOLD HEADER RECORD &
		! SET UP A 512 WORD BUFFER TO HOLD RECEIVED MESSAGE. &
		! REMOVE THEN DECLARE OURSELVES AS A RECEIVER. &

1060	OPEN PKG.LOC$+"ERRLOG.FIL" FOR INPUT AS FILE 2% &
	\ IF (STATUS AND 1024%)<>0% THEN &
		CLOSE 2% &
	\	SLEEP 2% &
	\	GOTO 1060 &
		! LOOP UNTIL WE GET THE ERRLOG FILE OPEN WITH WRITE PRIVS. &

1070	GET #2%+SWAP%(3%), RECORD 1% &
	\ FIELD #3%, 496% AS W$, 2% AS TOT.LOG$, 2% AS TOT.REC$, &
	2% AS TOT.USED$, 2% AS BYT.POS$, 2% AS TOT.LIM$ &
		! GET THE HEADER RECORD FROM ERRLOG.FIL INTO BUFFER 3 &
		! AND FIELD IT TO GET CURRENT 'TOTALS'. &

1080	IF BYPASS%<>0% THEN &
		S$=SYS(CHR6.22$+CHR$(2%)+CHR$(0%+2%)+I$) &
		! IF THE RECEIVE/BYPASS SWITCH IS ON (-1) THEN &
		!	DO A RECEIVE (NO SLEEP) MESSAGE &

1090	CHANGE S$ TO S% &
	\ GOSUB 10010 &
	\ GOTO 3000 IF ER.COD%=56% &
	\ BYPASS%=-1% &
	\ GOTO 1080 &
		! CHANGE DATA RETURNED ON THE RECEIVE TO AN INTEGER ARRAY. &
		! GO PROCESS THE MESSAGE. &
		! PREPARE TO CLOSE UP (3000) IF SHUTUP SENT THE MESSAGE. &
		! SET SWITCH ON (-1) TO RECEIVE ANOTHER MESSAGE AND &
		! GO TRY TO RECEIVE ONE. &

1100	! &
	&
	&
	!	N O    E R R O R   F I L E &
	&
	&

1110	CLOSE 2%,3%,4% &
	\ SWAP$ = SYS(CHR$(7%)+SWAP$) &
	\ CHAIN PKG.LOC$+"ERRINT" LINE 31000 &
		! PUT SWAP/NOSWAP FLAG IN CORE COMMON &
		! CLOSE THE FILES &
		! CHAIN TO INITIALIZE MODULE TO RE-CREATE FILE &

1200	! &
	&
	&
	!	N O    M E S S A G E S    P E N D I N G &
	&
	&

1210	PUT #2%+SWAP%(3%), RECORD 1% &
	\ CLOSE 2% &
	\ S$=SYS(CHR6.22$+CHR$(2%)+CHR$(1%+2%)+I$) &
	\ BYPASS%=0% &
	\ GOTO 1060 &
		! PUT THE HEADER RECORD BACK AND CLOSE THE ERROR LOG FILE. &
		! DO A RECEIVE WITH SLEEP.  IF A MESSAGE IS PENDING &
		! THEN WE FALL THRU AND SET THE SWITCH TO BYPASS. &
		! OTHERWISE WE SLEEP UNTIL A MESSAGE IS RECEIVED. &
		! IN THIS CASE WE WAKE UP AT LINE 19000, THE ERROR TRAP. &

3000	! &
	&
	&
	!	S H U T U P    S E Q U E N C I N G &
	&
	&

3010	PUT #2%+SWAP%(3%), RECORD 1% &
	\ CLOSE 2%, 3%, 4% &
	\ S$=SYS(REM.REC$)+ &
	SYS(CHR6$+CHR$(8%)+CHR$((PEEK(518%) AND 255%)/2%)+ &
	STRING$(24%,0%)+CHR$(255%)) &
	\ STOP &
		! PUT AWAY HEADER RECORD. &
		! CLOSE ALL FILES. &
		! REMOVE OURSELVES FROM THE RECEIVER TABLES. &
		! KILL THE JOB. &
	&

10000	! &
	&
	&
	!	S U B R O U T I N E S &
	&
	&
	&
	!	P R O C E S S    E R R O R    M E S S A G E &
	&
	&

10010	REPEAT%=S%(24%) &
	\ GOTO 10020 IF (S%(4%)=0%) OR (S%(21%)=56%) &
	\ S%(L%)=0% FOR L%=25% TO 32% &
	\ S%(21%)=55% &
	\ S%(22%)=S%(4%) &
	\ S%(23%)=S%(5%) &
	\ S%(24%)=S%(6%) &
	\ L%=PEEK(512%) &
	\ S%(27%)=(L% AND 255%) &
	\ S%(28%)=(SWAP%(L%) AND 255%) &
	\ L%=PEEK(514%) &
	\ S%(29%)=(L% AND 255%) &
	\ S%(30%)=(SWAP%(L%) AND 255%) &
	\ S%(0%)=40% &
	\ CHANGE S% TO S$ &
	\ REPEAT%=0% &
	! ONLY THE MONITOR (JOB 0) SHOULD BE SENDING US MESSAGES. &
	! IF SENDING JOB ISN'T ZERO: &
	! SET ERROR CODE TO 55, STORE SENDING JOB*2, SENDING PPN. &
	! SET ERROR DATE/TIME TO CURRENT DATE/TIME. &
	! CONVERT BACK TO STRING - DON'T WIPE OUT REST OF PARAMETER OR &
	! MESSAGE - COULD BE USED TO FIND OUT WHAT WAS SENT &
	! RESET REPEAT COUNT. &

10020	ER.COD%=S%(21%) &
	\ L%=2%+20%+((S%(13%)+SWAP%(S%(14%))) AND 1023%) &
	\ FIELD #3%, ER.COD%*8% AS W$, 2% AS RECRD.LOG$, 2% AS ER.LOG$, &
	2% AS ER.RCV$, 2% AS RECRD.ALW$ &
	\ LSET ER.RCV$=CVT%$(CVT$%(ER.RCV$)+REPEAT%+1%) &
	\ LSET TOT.REC$=CVT%$(CVT$%(TOT.REC$)+REPEAT%+1%) &
	\ GOTO 10100 &
		IF (CVT$%(RECRD.LOG$)>=CVT$%(RECRD.ALW$)) OR &
		CVT$%(TOT.USED$)>=CVT$%(TOT.LIM$) &
		! ER.COD% = THE CODE FOR THE ERROR TYPE &
		! REPEAT% = ERROR REPEAT COUNT &
		! L% = LENGTH OF THE MESSAGE TRANSFERRED+ &
		!      2 BYTES FOR THE LENGTH FIELD+ &
		!      20 BYTES FOR THE PARAMETER STRING &
		! FIELD THE HEADER RECORD FOR ERROR TYPE ER.COD%. &
		! BUMP THE # OF ERRORS RECEIVED FOR THIS TYPE. &
		! BUMP THE TOTAL # OF ERRORS RECEIVED. &
		! &
		! QUIT HERE IF # OF RECORDS LOGGED IS = MAX FOR ERROR TYPE &
		! ER.COD% OR IF FILE LENGTH EXCEEDS MAX ALLOWABLE. &

10050	B%=CVT$%(TOT.USED$) &
	\ O%=CVT$%(BYT.POS$) &
	\ IF (512%-O%)<L% THEN &
		B%=B%+1% &
	\	O%=0% &
		! B%=BLOCK # &
		! O%=OFFSET IN BLOCK B% WHERE NEXT RECORD STARTS &
		! IF REMAINING SPACE IN BLOCK IS LESS THAN ERROR RECORD LENGTH &
		! THEN &
		!	BUMP BLOCK # AND RESET OFFSET TO BEGINNING OF BLOCK &

10060	GET #2%, RECORD B% UNLESS O%=0% &
	\ D%=512%-L%-O% &
	\ FIELD #2%, O% AS W$, L% AS M$, D% AS D$ &
	\ LSET D$=STRING$(D%,0%) &
	\ LSET M$=CVT%$(L%)+MID(S$,21%,20%)+LEFT(M0$,(L%-22%)) &
	\ PUT #2%, RECORD B% &
		! GET THE PROPER BLOCK. &
		! FIELD IT AND THE 'MESSAGE BUFFER'. &
		! PUT THE TOTAL RECORD LENGTH, THE PARAMETER &
		! STRING AND THE ACTUAL ERROR RECORD INTO THE BLOCK. &
		! WRITE OUT THE BLOCK &

10070	LSET TOT.USED$=CVT%$(B%) &
	\ LSET BYT.POS$=CVT%$(O%+L%) &
	\ LSET RECRD.LOG$=CVT%$(CVT$%(RECRD.LOG$)+1%) &
	\ LSET ER.LOG$=CVT%$(CVT$%(ER.LOG$)+REPEAT%+1%) &
	\ LSET TOT.LOG$=CVT%$(CVT$%(TOT.LOG$)+REPEAT%+1%) &
		! UPDATE THE HEADER RECORD: &
		! STORE THE LAST BLOCK # USED. &
		! STORE THE OFFSET INTO BLOCK. &
		! BUMP # OF ERROR RECORDS LOGGED FOR TYPE ER.COD% &
		! BUMP # OF ERRORS (INCLUDING REPEATS) LOGGED FOR TYPE ER.COD% &
		! BUMP TOTAL ERRORS LOGGED (INCLUDING REPEATS) &

10100	RETURN &
	&

19000	! &
	&
	!	E R R O R    H A N D L E R &
	&
	&

19010	GOTO 19020 IF ERR<>5% &
	\ RESUME 1100 IF ERL=1060% &
	\ RESUME 1200 IF ERL=1080% &
	\ RESUME 1060 IF ERL=1210% &
		! QUIT IF ERROR WAS NOT "CANT FIND FILE OR ACCOUNT" &
		! 1100 IF ERROR FILE NON EXISTENT &
		! 1200 IF NO MESSAGE PENDING &
		! 1060 IF WE JUST AWOKE VIA MESSAGE RECEPTION &

19020	IF ERR=10% AND ERL=1060% THEN &
		SLEEP 2% &
	\	RESUME 1060 &
		! DIDN'T GET WRITE ACCESS ON ATTEMPT TO OPEN ERRLOG FILE. &
		! SLEEP THEN RETRY. &

19030	S$=SYS(REM.REC$) &
	\ ON ERROR GOTO 0 &
	\ GOTO 32767 &
		! UNEXPECTED ERROR - HIBERNATE AS A MESSAGE RECEIVER &

20000	! &
	&
	!	P r i v i l e g e   C h e c k   F u n c t i o n &
	&
	! &
	DEF*	FNPRIV%(X$) &
	\ X$=SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+X$) &
	\ FNPRIV%=(ASCII(MID(X$,3%,1%))=0%) &
	\ FNEND &
	! This function takes a privilege name as it's argument, and returns &
	!  -1 if the user has the specified privilege, or 0 if not. &

31000	! &
	&
	&
	!	C H A I N    E N T R Y &
	&
	&

31010	ON ERROR GOTO 19000 &
	\ SWAP$ = SYS(CHR$(7%)) &
	\ ENTRY.TYP%=2% &
	\ GOTO 1000 &
		! WE'RE BACK FROM OUR CHAIN TO ERRINT &
		! RESET ERROR TRAP. &

32760	S$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ S$=SYS(CHR$(9%)) &
		! Get rid of privileges. &
		! REMOVE OURSELVES FROM MEMORY. &

32767	END
