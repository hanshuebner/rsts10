2!		PROGRAM		: OPSER
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
8!
10		EXTEND		! EXTEND MODE
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
  !*******************************************************************
20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! V9.0-06	21-MAY-84	(KMF) Add priv check &
	! V9.0-09	08-Nov-84	(PRL) Fix for multiple privs &
	! V10.0-H	01-Feb-90	(REG) Add feature patches &
	! &
	&
	&
	&
	&

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	! OPSER is the initialization/restart program for running &
	! the OPSER package. OPSER must be run before any of the &
	! spooling programs. &
	&
	! OPSER requires the user to have SWCFG privilege before &
	! continuing.  If not, OPSER displays an error message and &
	! exits. &
	&
	! OPSER will test for the presence of its two work files &
	! OPSER0.WRK and OPSER1.WRK which contain working tables &
	! and the valid-operator list and on-line job table. &
	&
	! a. Files not present: &
	&
	! If the files are not present, OPSER displays a warning and then &
	! proceeds to initialize all its tables.  Initial entry in the &
	! valid-operator list is user's PPN on any KB.  Initial entry in &
	! the on-line job table is ERRCPY. &
	&
	! b. Files present: &
	&
	! If the files are present, OPSER performs the following: &
	&
	! 1. if no jobs in the on-line job table are currently active in &
	!    the system other than ERRCPY, OPSER will leave ERRCPY in &
	!    the table, and test the entries in the valid-operator list. If &
	!    any entries are illegal, OPSER will initialize the list to all &
	!    [1,2] users and the user running OPSER. Additionally, a command &
	!    will be created internally to display the valid-operator list &
	!    when processing commences. &
	&
	! 2. If there are jobs in the on-line job table currently active &
	!    in the system, OPSER will remove those not active, create &
	!    a 'last' command to send to each active job in the table (which &
	!    requests retransmission of its last message), and create an &
	!    internal command to display the on-line job table entries. &
	!    upon scanning the valid-operator list, OPSER will remove any &
	!    illegal entries and, if any are removed, create an internal &
	!    command to display the valid-operator list. &
	&
	! OPSER will terminate by chaining to OPSRUN. &
	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
310!		3		OPSER0.WRK - OPSER WORK FILE. &

315!		4		OPSER1.WRK - OPSER WORK FILE. &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&

401	!	VARIABLE NAME		USE &

410	!	A%()			ACTIVITY TABLE. &
	!	A%			ENTRY COUNT IN A%() &
	!	A0%			OPSER'S PPN &
	!	A1%			ACTIVITY CODE TO SET &
	!	B$()			BUILD TABLE FOR STRING STORAGE &
	!	B%()			BUILD TABLE FREE LIST &
	!	B%			BUILD TABLE FREE LIST ROOT &
	!	D$()			LEGAL COMMAND LIST &
	!	D%()			LEGAL COMMAND CODE LIST
415	!	I%			INDEX INTO BUILD TABLE &
	!	I1%			INDEX INTO MESSAGE CONTROL TBL &
	!	I2%			INDEX INTO ACTIVITY TABLE &
	!	I0%			PRIORITY INCREMENT &
	!	I$			VERSION/EDIT NUMBERS &
	!	J%()			TABLE OF ONLINE JOBS &
	!	J0%			OPSER'S JOB # &
	!	K0%			OPSER'S KB:
420	!	M%()			MESSAGE CONTROL TALE &
	!	M0%()			MSG CTL TBL DIRECTORY OF &
	!					ASSIGNED MSG #'S &
	!	M1%()			MSG CTL TBL FREE LIST &
	!	M1%			MSG CTL TBL FREE LIST ROOT &
	!	M8%			MAX. KB# &
	!	M9%			MAX # OF JOBS ALLOWED &
	!	N%			MSG # ASSIGNMENT &
	!	O%()			VALID OPERATOR LIST
425	!	P$			CHAIN PROGRAM NAME &
	!	P1%			PRIORITY TO BE SET IN ACT. TBL. &
	!	S%			STARTUP FLAG &
	!	S$			SYS CALL STRINGS; MSG BUILD &
	!	S1%			(JOBTBL)-START OF JOB TABLE &
	!	S1$			SYS CALL- FILENAME STRING SCAN &
	!	S2$			SYS CALL-DECLARE RECEIVER &
	!	S4$			REMOVE RECEIVER
430	!	W%()			SORT WORK AREA &
	!	Z$			> &
	!	Z%			> &
	!	Z0%			>	WORK VARIABLES &
	!	Z1%			> &
	!	Z2%			> &
	!	Z3%			> &
	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
810!	S/R(LINE 10010)		SET ACTIVITY TBL ENTRY &
   !	S/R(LINE 10400)		SET ENTRIES IN BUILD TBL, MCT, ACT. TBL. &
	&

820!	FNA% (LINE  13510)		LEGAL PPN TEST &
   !	FNI% ( "   12050)	NEXT BUILD TBL OINDEX &
   !	FNI1%( "   12110)	NEXT INDEX- MCT &
   !	FNI2%( "   12080)	NEXT INDEX - ACTIV TBL &
   !	FNJ3%( "   13450)	ONLINE JOBS ACTIVE TEST &
   !	FNN3%( "   12900)	JOB # AN ELIGIBLE RECVR TEST &
   !	FNO1%( "   13380)	VALID OPERATOR TEST &
   !	FNO2%( "   13600)	STORE ENTRY IN VALID OPER LIST &
   !	FNO3%( "   13550)	LEGAL KB: & VALID PPN TEST &
   !	FNP$( "    12010)	FILENAME STRING SCAN &
   !	FNS2%( "   12350)	SORT ACTIVITY TBL. DESCENDING PRIORITY &
	&

830	! SYS CALLS :	6,9,0	SYSTEM HEADER INFO &
	!		6,14	ACCOUNTING DATA &
	!		6,-3	MONITOR TABLES - PART I &
	!		6,-10	FILENAME STRING SCAN &
	!		6,22,1	DECLARE RECEIVER &
	!		6,22,0	REMOVE RECEIVER &
	&

840	! PEEKS :	JOB ACTIVE TEST &
	!		RETRIEVE JOB NAME &
	!		RETRIEVE JOB'S PPN &
	!		TEST IF JOB AN ELIGIBLE RECEIVER &
	!		RETRIEVE JOB'S LOGICAL ID IF AN ELIGIBLE RECVR. &
	&

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

910	DIM #3%,	A%(31%,3%), &
			M1%(63%), &
			M0%(31%,1%), &
			B%(32767%) &
		! ACTIVITY TABLE: ACTIVITIES TO BE EXECUTED. &
		! MESSAGE-CONTROL-TABLE FREE LIST. &
		! BUILD-TABLE FREE LIST. &
		! MESSAGE-CONTROL-TABLE DIRECTORY OF ASSIGNED MSG #'S. &

920	DIM #4%,	J%(23%,7%), &
			O%(15%,3%), &
			M%(63%,11%), &
			D$(27%)=16%, &
			D%(31%), &
			B$(32767%)=32% &
		! J%() - JOB TABLE OF ON-LINE JOBS. &
		! O%() - VALID-OPERATOR TABLE. &
		! M%() - MESSAGE-CONTROL-TABLE. &
		! D$() - LEGAL COMMANDS TABLE. &
		! D%() - CORRESPONDING LEGAL COMMAND CODES. &
		! B$() - BUILD TABLE STRING STORAGE. &

980	DIM	W%(5%) &
	\ DIM	Z%(30%) &
		! TEMPORARY WORK AREA FOR SORTING. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
		! SET UP STD ERROR TRAP. &

1010	I$="V10.1-A" &
	\ CHR6$ = CHR$(6%) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "OPSER ";I$;"   ";FNERROR$(0%) &
	\ PRINT &
		! VERSION - EDIT #'S. &
		! READ SYSTEM HEADER INFO. &
		! RETURN KB: TO LEFT MARGIN. &
		! PRINT PROGRAM AND AND SYSTEM INFO. &

1020	IF NOT (FNPRV%("WACNT") AND FNPRV%("WREAD") AND &
	   FNPRV%("WWRITE") AND FNPRV%("RDMEM"))  THEN &
		PRINT "?OPSER must be privileged" &
		\ GOTO 32767 &
		!MAKE SURE PROGRAM HAS REQUIRED PRIVILEGES TO RUN &

1030	S$=SYS(CHR6$+CHR$(-21%)+CHR$(255%)) &
	\ SWCFG%=FNPRV%("SWCFG") &
	\ S$=SYS(CHR6$+CHR$(-21%)+CHR$(0%)) &
	\ IF NOT SWCFG% THEN &
		PRINT "?SWCFG privilege required" &
		\ GOTO 32767 &
		!MAKE SURE USER HAS SWCFG PRIV TO RUN &

1040	PKG.LOC$="OPSER$:" &
	\ S$=SYS(CHR6$+CHR$(26%)) &
	\ J0%=ASCII(MID(S$,3%,1%))/2% &
	\ K0%=ASCII(MID(S$,4%,1%)) &
	\ A0%=SWAP%(CVT$%(MID(S$,21%,2%))) &
		! DEFINE OPSER PACKAGE LOCATION &
		! SAVE USER'S JOB NO., KB NO., & PPN &

1070	I0%=0% &
		! INITIALIZE PRIORITY INCREMENT. &

1100	S$=SYS(CHR6$+CHR$(-3%)) &
	\ S1%=SWAP%(CVT$%(MID(S$,11%,2%))) &
	\ M8%=SWAP%(CVT$%(MID(S$,3%,1%))) &
	\ M9%=SWAP%(CVT$%(MID(S$,4%,1%))) &
		! MONITOR TABLES PART 1. &
		! (JOBTBL) START OF JOB TABLE. &
		! GET MAX ALLOWABLE KB #. &
		! GET MAX ALLOWABLE JOB #. &

1120	S1$=CHR6$+CHR$(-10%) &
	\ T2% = 60% &
	\ S2$=CHR6$+CHR$(22%)+CHR$(1%)+CHR$(0%)+"OPSER " &
		+STRING$(11%,0%)+CHR$(1%)+CHR$(0%)+CHR$(1%) &
		+CHR$(30%) &
	\ S4$=CHR6$+CHR$(22%)+CHR$(0%)+CHR$(0%) &
		! OFTEN USED SYS() CALL STRINGS: &
		! S1$=FILENAME STRING SCAN; &
		! S2$=DECL RECVR; &
		! S4$=REMOVE RECVR; &
	\ CRLF$=CHR$(10%)+CHR$(13%) &
	\ WAIT.SECS% = 0% &

1125	S$=SYS(S4$) &
	\ S$=SYS(S2$) &
		! REMOVE RECEIVER (RIB 0) &
		! DECLARE "OPSER" AS AN ELIGIBLE RECEIVER. &

1130	OPEN PKG.LOC$+"OPSER0.WRK" FOR INPUT AS FILE #3% &
	\ GOTO 1160 IF (STATUS AND 1024%) <> 0% &
	\ OPEN PKG.LOC$+"OPSER1.WRK" FOR INPUT AS FILE #4% &
	\ GOTO 1160 IF (STATUS AND 1024%) <> 0% &
	\ S%=1% &
	\ S%=2% IF FNJ3%(Z0%) <> 0% &
	\ GOTO 1180 &
		! IF EITHER FILE NOT PRESENT, ERROR TRAP &
		!  WILL OPEN THEM. &

1140	OPEN PKG.LOC$+"OPSER0.WRK" AS FILE 3% &
	\ OPEN PKG.LOC$+"OPSER1.WRK" AS FILE 4% &
	\ PRINT "%OPSER files not found - initializing..." &
	\ S%=0% &
	\ GOTO 1180 &
		! ERROR TRAP COMES HERE TO OPEN THE FILES &
		!  AND SET STARTUP FLAG. &

1160	PRINT "?No write access to OPSER files" &
	\ GOTO 32000 &

1180	B$(Z%)="" FOR Z%=0% TO 255% &
	\ B%(Z%)=Z%+1% FOR Z%=1% TO 254% &
	\ B%(0%)=0% &
	\ B%(255%)=-1% &
	\ B%=1% &
		! CLEARS STRING STORAGE FOR BUILD TABLE; &
		! INITS. THE FREE LIST; SETS ROOT TO 1ST ENTRY. &

1190	M1%(Z%)=Z%+1% FOR Z% = 1% TO 62% &
	\ M1%(0%)=0% &
	\ M1%(63%)=-1% &
	\ M1%=1% &
	\ M0%(Z%,0%),M0%(Z%,1%)=0% FOR Z%=0% TO 31% &
	\ A%(0%,Z%)=0% FOR Z%=0% TO 3% &
	\ A%=1% &
		! INITS. THE FREE LIST FOR MSG. CTL. DIRECTORY TBL.; &
		! SETS ITS ROOT TO 1ST ENTRY. &
		! CLEARS 1ST ENTRY IN ACTIVITY TABLE; SETS CTR FOR TBL. &

1200	IF S%=0% THEN GOTO 1240 &
		ELSE &
		IF S%=1% THEN &
			IF FNO3%(Z0%)=0% THEN GOTO 1220 &
			ELSE &
			S$="LIS OP" &
	\		GOSUB 10400 &
	\		GOTO 1240 &
		! IF INITIAL START GO CLEAR OPER. TABL. &
		! IF NO JOBS IN THE ON-LINE JOB TABLE ARE ACTIVE &
		!	AND IF ANY OPERATOR ENTRY IS INVALID, &
		!	ENTER 'LIST VALID-OPERATORS' ACTIVITY, &
		!	CLEAR OPERATOR TABLE. &

1210	IF S%=2% THEN &
		IF FNO3%(Z0%) <> 0% THEN &
			S$="LIS OP" &
	\		GOSUB 10400 &
		! IF SOME JOBS FOUND ONLINE AND IF ANY OPERATOR ENTRY &
		!	INVALID, REMOVE ENTRY FROM TABLE AND ENTER &
		!	ACTIVITY TO DISPLAY VALID-OPERATOR LIST. &

1220	IF FNO1%(K0%,A0%) <> 0% THEN GOTO 1260 &
		ELSE &
		IF FNO2%(K0%,A0%) <> 0% THEN GOTO 1260 &
		! INSURE THAT THIS KB: AND PPN ARE IN THE TABLE &
		! IF NOT, PUT THEM IN IF ROOM AVAILABLE. &
		! IF NO ROOM AVAILABLE, CLEAR TABLE AND DEFAULT THIS KB: &
		!	 AND PPN. &

1240	FOR Z%=0% TO 15% &
	\ O%(Z%,Z3%)=0% FOR Z3%=0% TO 3% &
	\ NEXT Z% &
		! CLEAR ALL OF EACH ENTRY IN TABLE. &

1250	O%(0%,0%)=255% OR 256% &
	\ O%(0%,1%)=A0% &
	\ IF S% <> 0% AND FNO1%(K0%,A0%)=0% THEN &
		Z%=FNO2%(K0%,A0%) &
		! INIT. VALID-OPERATOR LIST TO USER'S PPN ON ANY KB: &

1260	IF S%=2%  THEN GOTO 1300 &

1270	J%(Z%,Z3%)=0% FOR Z3%=0% TO 7% FOR Z%=0% TO 23% &
		! CLEAR ALL FIELDS OF ALL ENTRIES. &

1280	FOR Z%= 1% TO M9% &
	\ IF PEEK(S1%+Z%*2%) <> 0% THEN &
		UU0$=SYS(CHR$(6%)+CHR$(26%)+CHR$(Z%)) &
	\	IF "ERRCPY"=RAD$(SWAP%(CVT$%(MID(UU0$,17%,2%))))+ &
		    RAD$(SWAP%(CVT$%(MID(UU0$,19%,2%)))) &
		      THEN J%(0%,1%)=Z% &
	\		J%(0%,2%)=SWAP%(CVT$%(MID(UU0$,21%,2%))) &
	\		Z$=FNP$("ERRLOG") &
	\		J%(0%,5%)=SWAP%(CVT$%(Z$)) &
	\		J%(0%,6%)=SWAP%(CVT$%(RIGHT(Z$,3%))) &
	\		J%(0%,0%)= -1% &
	\		J%(0%,7%)=9% &
	\		GOTO 1330 &
	! IF "ERRCPY" IS AN ACTIVE JOB, PUT ITS JOB #, PPN, &
	!	LOGICAL ID (RECVR. NAME), & SHUTDOWN LEVEL = 9 &
	!	IN OPSER'S JOB TABLE. &

1290	NEXT Z% &
	\ GOTO 1330 &

1300	S$="LIST JO" &
	\ GOSUB 10400 &
		! ENTER COMMAND ACTIVITY TO LIST ONLINE JOBS. &

1310	FOR Z1%=0% TO 23% &
	\ IF J%(Z1%,0%)=0% THEN GOTO 1320 &
		ELSE &
		S$="INT "+RAD$(J%(Z1%,5%))+RAD$(J%(Z1%,6%))+":LAST" &
	\	GOSUB 10400 UNLESS RAD$(J%(Z1%,5%))+RAD$(J%(Z1%,6%))="ERRLOG" &
		! ENTER SEND-MESSAGE ACTIVITY FOR ALL &
		!	REMAINING ONLINE JOBS REQUESTING &
		!	RETRANSMISSION OF LAST MESSAGE. &

1320	NEXT Z1% &

1330	IF S% <> 2% THEN &
		N%=1% &
		ELSE &
		N%=M%(0%,0%)+10% &
		! MESSAGE #'S START AT 1 IF INIT., &
		!	+10 FROM STORED VALUE IF RESTART. &

1420	I2%=FNI2%(I2%) &
	\ A1%=1% &
	\ P1%=8% &
	\ GOSUB 10010 &
		! SET RECEIVE-MESSAGE ACTIVITY, &
		!	HIGH PRIORITY. &

1430	I2%=FNI2%(I2%) &
	\ A1%=2% &
	\ P1%=8% &
	\ GOSUB 10010 &
		! SET READ-KEYBOARD ACTIVITY, &
		!	HIGH PRIORITY. &

1440	I2%=FNI2%(I2%) &
	\ A1%=9% &
	\ P1%=0% &
	\ GOSUB 10010 &
		! SET SLEEP ACTIVITY, LOW PRIORITY. &
		! SET A% TO # OF ACTIVITY ENTRIES. &

1450	FOR Z%=A% TO 31% &
	\ A%(Z%,Z3%)=0% FOR Z3%=0% TO 3% &
	\ NEXT Z% &
		! CLEAR REST OF TABLE. &

1460	FOR Z%=I1%+1% TO 63% &
	\ M%(Z%,Z3%)=0% FOR Z3%=0% TO 11% &
	\ NEXT Z% &
		! CLEAR ALL OF EACH ENTRY IN MESSAGE-CTL-TBL. &

1470	DATA	*STARTCMD, &
	OPERATOR,	1, &
	LOGFILE,	2, &
	MESSAGE,	3, &
	RETYPE,		4, &
	CHANGECONSOLE,	5, &
	DETACH,		6, &
	EXIT,		7, &
	ANSWER,		8, &
	INTERRUPT,	9, &
	DELETE,		10, &
	ONLINE,		11, &
	OFFLINE,	12, &
	LIST,		13, &
	SLE,		14, &
	SIM,		15, &
	NOFORMS_WAIT,	16, &
	FORMS_WAIT,	17, &
	NOFORM_FEED,	18, &
	FORM_FEED,	19 &

1480	DATA	*ENDCMD, &

1490	RESTORE &
	\ READ Z$ UNTIL Z$="*STARTCMD" &
	\ Z1%=0% &
		! INIT THE LEGAL CMD LIST. &

1500	WHILE Z$ <> "*ENDCMD" &
	\ READ Z$,Z% &
	\ Z1%=Z1%+1% &
	\ D$(Z1%)=Z$ &
	\ D%(Z1%)=Z% &
	\ NEXT &
	\ D%(0%)=Z1%-1% &
	\ D$(0%)="" &
	\ D$(Z3%)="" FOR Z3%=Z1% TO 27% &
	\ D%(Z3%)=0% FOR Z3%=Z1% TO 31% &
		! SET LEGAL COMMANDS AND COMMAND CODES. &

1600	Z%=FNS2%(I2%) &
		! SORT THE ACTIVITY TABLE. &

1650	M%(0%,0%)=N% &
	\ M%(0%,1%)=A% &
	\ M%(0%,2%)=B% &
	\ M%(0%,3%)=K0% &
	\ M%(0%,4%)=K0% &
	\ M%(0%,5%)=0% &
	\ M%(0%,6%)=M1% &
	\ M%(0%,7%)=I0% &
	\ M%(0%,8%)=A0% &
	\ M%(0%,9%)=J0% &
	\ M%(0%,10%)=255% &
	\ M%(0%,11%)=0% &
		! THIS 0'TH ENTRY USED TO PASS DATA TO OPSRUN: &
		! 0=MSG # TO ASSIGN;	1=# ENTRIES IN ACT. TBL.; &
		! 2=FREE LIST FOR BLD.TBL.	3=OPSER'S KB:; &
		! 4=OSC			5=FREE LIST FOR MSG. CTL. TBL.; &
		! 6=FREE LIST FOR MSG. TBL.	7= ACTIVITY INCR.; &
		! 8=OPSER'S PPN;	9=OPSER'S JOB #; &
		! 10=LENGTH OF BLD. TBL. FREE LIST. &

1800	NAME PKG.LOC$+"OPSER1.WRK" AS "OPSER1.WRK<40>" &
		! CHANGE PROTECTION SO FOR QUEMAN ACCESS. &

1810	CLOSE #3%, #4% &
		! CLOSE THE FILES BEFORE CHAINING. &

1900	P$=PKG.LOC$+"OPSRUN" &

2000	CHAIN P$ LINE (31000% OR NOT 32767%) &
		! DO THE CHAIN. &

10000	! &
	&
	&
	!	S U B R O U T I N E S &
	&
	&

10010	A%(I2%,0%)=P1% &
	\ A%(I2%,1%)=I0% &
	\ A%(I2%,2%)=A1% &
		! SUBROUTINE:	SET PRIORITY, PRIOR. INC. & ACTIV. CODE &
		! ********** &

10020	A%(I2%,3%)=0% &
	\ IF A1% >2% THEN IF A1% < 9% THEN A%(I2%,3%)=I1% &
		! SET INDEX INTO MSG. TBL. IF NEEDED. &

10030	RETURN &
	&
	&

10400	I%=FNI%(I%) &
	\ B$(I%)=CHR$(128%+64%)+S$ &
		! SUBROUTINE:	GET ENTRY IN BUILD-TABLE. &
		! **********	PREFIX STRING TO BE STORED &
		!		WITH COMMAND BITS SET. &

10410	I1%=FNI1%(I1%) &
	\ M%(I1%,0%)=0% &
	\ M%(I1%,1%)=J0% &
	\ M%(I1%,2%)=A0% &
	\ M%(I1%,3%)=0% &
	\ M%(I1%,4%)=0% &
	\ M%(I1%,5%)=0% &
	\ M%(I1%,6%)=0% &
	\ M%(I1%,7%)=0% &
	\ M%(I1%,8%)=I% &
	\ M%(I1%,9%)= -1% &
	\ M%(I1%,10%)=0% &
	\ M%(I1%,11%)=0% &
		! SET THE ENTRY IN THE MESSAGE-CONTROL-TABLE. &

10420	A1%=3% &
	\ P1%=9% &
	\ I2%=FNI2%(I2%) &
	\ GOSUB 10010 &
		! SET ACTIVITY-TABLE ENTRY. &

10430	RETURN &
	&
	&

12000	! &
	&
	&
	!	F U N C T I O N S &
	&
	&

12010	DEF* FNP$(Z$) &
	\ ON ERROR GOTO 12030	!SET LOCAL ERROR TRAP &
	\ FNP$=MID(SYS(S1$+Z$),7%,4%) &
	&
		! FUNCTION:	FILENAME STRING TO RAD 50. &
		! -------- &

12020	ON ERROR GOTO 19000	!RESET STANDARD ERROR TRAP &
	\ GOTO 12040		!GO EXIT &

12030	FNP$ = ""		!RETURN A NULL STRING ON ERROR &
	\ RESUME 12020		!LET'S GET OUT OF HERE &

12040	FNEND &
	&

12050	DEF* FNI%(I%) &
	\ FNI%=0% &
		! FUNCTION:	GET NEXT ENTRY IN BUILD TABLE. &
		! -------- &

12060	IF B%= -1% THEN GOTO 12070 ELSE Z%=B% &
	\ B%=B%(Z%) &
	\ B%(Z%)=0% &
	\ FNI%=Z% &
		! B% IS NEXT-AVAILABLE POINTER IN FREE LIST. &

12070	FNEND &
	&
	&

12080	DEF* FNI2%(I2%) &
	\ FNI2%=0% &
		! FUNCTION: GET NEXT ENTRY IN CTL-TABLE. &
		! -------- &

12090	IF A% >= 31% THEN GOTO 12100 &
		ELSE FNI2%=A% &
	\ A%=A%+1% &
		! A% IS COUNT OF ENTRIES IN ACTIVITY-TABLE. &

12100	FNEND &
	&
	&

12110	DEF* FNI1%(I1%) &
	\ FNI1%=0% &
		! FUNCTION:	GET NEXT ENTRY IN MSG. CTL. TBL. &
		! -------- &

12120	IF M1%= -1% THEN GOTO 12130 ELSE Z%=M1% &
	\ M1%=M1%(Z%) &
	\ M1%(Z%)=0% &
	\ FNI1%=Z% &
		! M1% IS NEXT-AVAILABLE POINTER IN FREE LIST FOR &
		!	MESSAGE-CONTROL-TABLE. &
		! 0 TO ENTRY IF USED. &

12130	FNEND &
	&
	&

12350	DEF* FNS2%(I2%) &
	\ FNS2%=0% &
		! FUNCTION:	SORT THE ACTIVITY TABLE; DESCENDING &
		! --------	PRIORITY. &

12352	FOR Z%=0% TO A%-2% &
	\ A%(Z%,Z3%)=A%(Z%+1%,Z3%) FOR Z3%=0% TO 3% &
	\ NEXT Z% &
	\ A%=Z%+1% &
		! SHUFFLE ENTRIES TO START OF TBL. &

12356	A%(Z%+1%,Z3%)=0% FOR Z3%=0% TO 3% &
		! CLEAR UNUSED ENTRY. &

12358	Z2%=A%-1% &
		! SET # OF ENTRIES TO BE SORTED. &

12360	Z%=0% &
	\ FOR Z1%=0% TO Z2% &
	\ IF A%(Z1%,0%) < A%(Z1%+1%,0%) THEN &
		W%(Z3%)		= A%(Z1%,Z3%)	FOR Z3%=0% TO 3% &
	\	A%(Z1%,Z3%)	= A%(Z1%+1%,Z3%)FOR Z3%=0% TO 3% &
	\	A%(Z1%+1%,Z3%)	= W%(Z3%)	FOR Z3%=0% TO 3% &
	\	Z%= -1% &
		! SWAP ENTRIES IF NOT IN ORDER; SET INVERSION FLAG. &

12370	NEXT Z1% &

12380	Z2%=Z2%-1% &
	\ GOTO 12360 UNLESS Z%=0% &
		! BACK AGAIN IF ANY INVERSIONS. &

12390	FNEND &
	&
	&

12900	DEF* FNN3%(Z%) &
	\ FNN3%=0% &
		! FUNCTION:	IF JOB IS AN ELIGIBLE RECEIVER, &
		! --------		RETRIEVE ITS LOGICAL ID. &

12910	Z0%=SWAP%(CVT$%(MID(SYS(CHR$(6%)+CHR$(26%)+CHR$(Z%)+ &
		CHR$(1%)),29%,2%))) &
	\ IF Z0% <> 0% THEN &
		Z$=FNP$(CVT%$(SWAP%(PEEK(Z0%+2%)))+ &
			CVT%$(SWAP%(PEEK(Z0%+4%)))+ &
			CVT%$(SWAP%(PEEK(Z0%+6%)))) &
	\	GOTO 12930 IF Z$="" &
	\	Z2%=SWAP%(CVT$%(LEFT(Z$,2%))) &
	\	Z3%=SWAP%(CVT$%(RIGHT(Z$,3%))) &
	\	FNN3%=  -1% &
		! EXIT IF NOT AN FSS'ABLE RECEIVER NAME &
		! IF JOB # HAS BEEN DECLARED AN ELIGIBLE RECEIVER, &
		!	STORE ITS LOGICAL ID IN Z2% AND Z3%. &

12930	FNEND &
	&
	&

13380	DEF* FNO1%(Z1%,Z2%) &
	\ FNO1%=0% &
		! FUNCTION:	SEARCH VALID-OPERATOR TABLE FOR MATCH &
		! --------	ON KB: AND PPN. &

13390	FOR Z%=0% TO 15% &
	\ IF O%(Z%,0%)=0% THEN GOTO 13430 &
		ELSE &
		IF (O%(Z%,0%) AND 255%) <> 255% THEN &
		IF (O%(Z%,0%) AND 255%) <> Z1% THEN GOTO 13430 &

13400	IF O%(Z%,1%)= -1% THEN GOTO 13420 &
		ELSE &
		IF O%(Z%,1%)=Z2% THEN GOTO 13420 &
			ELSE &
			IF (O%(Z%,1%) AND 255%) <> 255% THEN &
			IF (O%(Z%,1%) AND 255%) <> (Z2% AND 255%) THEN &
				GOTO 13430 &

13410	IF (O%(Z%,1%) AND -256%) <> -256% THEN &
		IF (O%(Z%,1%) AND -256%) <> (Z2% AND -256%) THEN &
			GOTO 13430 &

13420	FNO1%= -1% &
	\ GOTO 13440 &

13430	NEXT Z% &

13440	FNEND &
	&
	&

13450	DEF* FNJ3%(Z0%) &
	\ FNJ3%=0% &
		! FUNCTION:	SEARCH ONLINE-JOB TABLE TO &
		! --------	SEE IF ANY OF THEM ARE ACTIVE. &

13460	Z$=FNP$("ERRCPY") &
	\ FOR Z%=0% TO 23% &
	\	IF J%(Z%,0%)=0% THEN GOTO 13490 &
		ELSE &
		IF J%(Z%,5%)=SWAP%(CVT$%(Z$)) THEN &
			IF J%(Z%,6%)=SWAP%(CVT$%(RIGHT(Z$,3%))) &
				THEN GOTO 13490 &
		! DON'T TEST IF  ERRCPY. &

13470	IF PEEK(S1%+J%(Z%,1%)*2%) <> 0% THEN &
		IF FNA%(J%(Z%,2%)) <> 0% THEN &
				IF FNN3%(J%(Z%,1%)) <> 0% THEN &
					IF J%(Z%,5%)=Z2% THEN &
					IF J%(Z%,6%)=Z3% THEN &
						FNJ3%= -1% &
	\					GOTO 13490 &
		! SET = -1 IF JOB ACTIVE, LEGAL PPN, JOB NAME &
		!	MATCHES, AND LOGICAL ID MATCHES. &

13480	J%(Z%,0%)=0% &
		! IF NOT SET ENTRY TO 0-NOT USED. &

13490	NEXT Z% &

13500	FNEND &
	&
	&

13510	DEF* FNA%(Z1%) &
	\ FNA%=0% &
		! FUNCTION:	TEST IF LEGAL PPN. &
		! -------- &
	&
	&

13520	IF (Z1% AND 255%) <= 255% THEN &
		IF (SWAP%(Z1%) AND 255%) <= 255% &
			AND (SWAP%(Z1%) AND 255%) <> 0% THEN &
			FNA%= -1% &
		! SET = -1 IF EACH IN RANGE: 0-255. &

13530	FNEND &
	&
	&

13550	DEF* FNO3%(Z0%) &
	\ FNO3%=0% &
		! FUNCTION:	TEST FOR LEGAL KB: AND VALID PPN. &
		! -------- &

13560	FOR Z%=0% TO 15% &
	\ IF O%(Z%,0%)=0% THEN GOTO 13580 &
		ELSE &
		IF FNA%(O%(Z%,1%)) <> 0% THEN &
			IF (O%(Z%,0%) AND 255%) <= M8% THEN &
				GOTO 13580 &
			ELSE &
			IF (O%(Z%,0%) AND 255%)=255% THEN GOTO 13580 &
		! KB: MUST NOT EXCEED MAX. ALLOWABLE, &
		!	AND PPN MUST BE IN VALID RANGE. &

13570	O%(Z%,Z3%)=0% FOR Z3%=0% TO 3% &
	\ FNO3%= -1% &
		! CLEAR ENTRY IF ILLEGAL; SET = -1. &

13580	NEXT Z% &

13590	FNEND &
	&
	&

13600	DEF* FNO2%(Z1%,Z2%) &
	\ FNO2%=0% &
		! FUNCTION:	STORE KB: AND PPN IN VALID-OPER. TBL. &
		! -------- &

13610	FOR Z%=0% TO 15% &
	\ IF O%(Z%,0%)=0% THEN &
		O%(Z%,0%)=Z1% OR 256% &
	\	O%(Z%,1%)=Z2% &
	\	FNO2%= -1% &
	\	GOTO 13640 &
		! IF AVAILABLE, SET BIT 256 WITH KB: #. &
		! SET FUNCTION = -1 WHEN STORED. &

13630	NEXT Z% &

13640	FNEND &

13700	DEF* FNPRV%(PRIV$) &
	&
	\ CHANGE SYS(CHR6$+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+ &
		PRIV$) TO Z% &
	\ FNPRV% = (Z%(3%)=0%) &
	\ FNEND &
	! Check to see if job currently has privileged named &
	! If privileged then return -1% &
	! Else return 0% &

18000	! &
	! &
	!	F N E R R O R $ (ERROR%) &
	! &
	! &
	DEF FNERROR$ (ERROR%) = &
		CVT$$(RIGHT(SYS(CHR6$+CHR$(9%)+CHR$(ERROR%)),3%),4%) &
		! return error text for error no. ERROR% &

19000	! &
	&
	&
	!	E R R O R   H A N D L E R &
	&
	&

19010	RESUME 20000 IF ERL=1125% &
		! GO PROCESS RECEIVER DECLARATION ERRORS &
	\ RESUME 1140 IF ERL=1130% AND ERR=5% &
	\ RESUME 13480 IF ERL=13470% AND ERR=10% &

19020	IF ERL = 2000% THEN &
		PRINT "?Unable to chain to "; P$ &
		\ PRINT FNERROR$(ERR) &
		\ RESUME 32000 &
		! if error trying to chain to OPSRUN, &
		!	display error message &
		!	and exit &

19999	PRINT "??Program failure in OPSER" &
	\ PRINT FNERROR$(ERR); " at line"; ERL &
	\ RESUME 32000 &
	&
	&

20000	! &
	!	RECOVERY FROM RECEIVER DECLARED ERRORS &
	! &
	! &
	IF ERR=32% THEN &
		WAIT.SECS%=WAIT.SECS%+3% &
		\ IF WAIT.SECS%<=30% THEN &
			SLEEP 3% &
			\ GOTO 1125 &
			! in no small buffers, &
			!	incr wait counter &
			!	if less than 30 secs waiting, &
			!		sleep 3 more secs &
			!		and retry the declare &
	&

20010	PRINT IF CCPOS(0%) &
	\ PRINT "?Unable to declare receiver OPSER" &
			! ensure left margin &
			! display 1st line of error text &

20020	IF ERR=16% THEN &
		PRINT "?Receiver already exists" &
		\ GOTO 32000 &
		! if error 16 "?Name or account now exists" &
		!	show rcvr already exists &
		!	and exit &

20030	PRINT FNERROR$(ERR) &
	\ GOTO 32000 &
		!unexpected error &
		!display it and exit &

32000	CLOSE 1%,2%,3%,4% &
	\ S$=SYS(S4$) &
		!close any open channels &
		!remove possible rcvr &

32767	END
