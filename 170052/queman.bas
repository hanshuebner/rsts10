2!		PROGRAM		: QUEMAN.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
8!
10		EXTEND		! EXTEND MODE
11	! &
	! &
	! &
	!		  C O P Y R I G H T &
  !	&
  !	&
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
  !*******************************************************************
20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	! &
	! &

21	! VER/ED	EDIT DATE	REASON &
	! V9.0-06	14-MAY-84	(KMF) Declare QUEMAN reciever as local &
	!				      object type 6; add priv check &
	! V9.0-09	08-NOV-84	(PRL) Change pkg location to OPSER$: &
	! V9.3-02	07-FEB-86	(DLS) Don't init queue.sys if next &
	!				      free entry is 0 (full queue). &
	! &
	&

100	! &
	! &
	! &
	!	G E N E R A L    D E S C R I P T I O N &
	! &
	! &

300	! &
	! &
	! &
	!	I / O    C H A N N E L S &
	! &
	! &

301!	CHANNEL #		USED FOR &
   !
800	! &
	! &
	! &
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	! &
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
900	! &
	! &
	! &
	!	D I M E N S I O N    S T A T E M E N T S &
	! &
	&

910	DIM J%(64%,5%),M%(30%) &
	! DIM JOB TABLE, SYS CALL ARRAY. &

920	DIM #3%,J$(64%,21%)=64% &
	! "TEMPNN.TMP" FILE. &

930	DIM #2%,Z0%(255%,15%),Z$(255%)=512% &
	! "QUEUE.SYS" FILE. &

940	DIM #4%, C0$(27%)=16%, C0%(31%), &
		O$(16%,2%)=4%, O%(16%,7%), O9%(17%), &
		H$(63%)=32%, H0$(63%)=512% &
	! "QUEUE.WRK" FILE:LEGAL COMMANDS/CODES; ONLINE-SPOOLER TABLES. &

942	DIM #4%, F9%(511%), H%(63%,15%) &
	! REDIM. '$QUEUE.WRK' TO OVERLAY H%() ONTO H$(). &

950	DIM Z%(255%) &
	! WORK ARRAY FOR FREE LIST CONFLICTS. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ I$="V10.1-A" &
	\ CRLF$=CHR$(13%)+CHR$(10%) &
	\ CHR9$=CHR$(9%) &
	\ J0$=SYS(CHR$(6%)+CHR9$+CHR$(0%)) &
	\ PRINT "QUEMAN ";I$;"   ";CVT$$(RIGHT(J0$,3%),4%) &
	\ J%=ASCII(J0$)/2% &
	\ J0$=RIGHT(NUM1$(100%+J%),2%) &
	\ Y9$=CHR$(6%)+CHR$(22%)+CHR$(1%)+CHR$(0%)+"QUEMAN" &
		+STRING$(10%,0%)+CHR$(6%)+CHR$(1%)+CHR$(0%)+ &
		CHR$(1%)+CHR$(20%) &
	\ Y7$=CHR$(6%)+CHR$(22%)+CHR$(-1%)+CHR$(0%) &
		+"OPSER "+STRING$(10%,0%) &
			! Y9$=DECLARE "QUEMAN" AS A RECEIVER. &
			!     (DECLARE AS LOCAL OBJECT TYPE 6) &
 			! Y7$=SEND A MESSAGE TO 'OPSER'. &
	\ Y6$=CHR$(6%)+CHR$(-10%) &
			! Y6$=FILENAME STRING SCAN. &
	\ Y4$=CHR$(6%)+CHR$(22%)+CHR$(0%)+CHR$(0%) &
			! Y4$=REMOVE ONESELF AS A RECEIVER &
	\ PRIV.OFF$=CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR$(6%)+CHR$(-21%)+CHR$(0%) &
			! DEFINE PRIV OFF/ON STRINGS &
	\ PKG.LOC$="OPSER$:" &
			! DEFINE PACKAGE LOCATION &

1005	IF NOT (FNPRV%("WREAD") AND FNPRV%("WWRITE") AND &
	   FNPRV%("WACNT") AND FNPRV%("SEND")) THEN &
		PRINT &
		\ PRINT "?QUEMAN must be privileged" &
		\ GOTO 32767 &
		! ENSURE PROGRAM HAS ENOUGH PRIVILEGES &

1010	S$=SYS(PRIV.OFF$) &
	\ IF NOT FNPRV%("SWCFG") THEN &
		PRINT &
	\	PRINT "?SWCFG privilege required" &
	\	GOTO 32767 &
		! DROP TEMP PRIVS &
		! IF USER DOESN'T HAVE SWCFG, THEN PRINT ERROR AND EXIT &

1015	TUNE.PRIV%=FNPRV%("TUNE") &
	\ S$=SYS(PRIV.ON$) &
	\ WAIT.SECS%=0% &
		! REMEMBER IF USER HAS TUNE PRV &
		! ENABLE TEMP PRIVS AGAIN &
		! INIT RETRY COUNTER FOR DECLARE &

1017	S$=SYS(Y9$) &
		! DECLARE "QUEMAN" AS A RECEIVER. &

1020	OPEN "_KB:QUEMAN.CMD" FOR INPUT AS FILE 1% &
	\ GOSUB 10300 &
	\ F$=PKG.LOC$+"TEMP"+J0$+".TMP" &
	\ OPEN F$ AS FILE 3% &
		! OPEN WORK FILE. &

1030	ON ERROR GOTO 1040 &
	\ OPEN PKG.LOC$+"QUEUE.SYS" FOR INPUT AS FILE 2% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-26%)+CHR$(2%)+CHR$(0%)) TO M% &
	\ IF (M%(2%) AND 16%) <> 0% THEN &
		PRINT #1%,"QUEUE.SYS WAS CONTIGUOUS -- MADE NONCONTIGUOUS" &
	\	Z$ = SYS(CHR$(6%)+CHR$(-26%)+CHR$(2%)+CHR$(32%))
1035	CLOSE 2% &
	\ GOTO 1050 &

1040	IF ERR<>5% THEN GOTO 19000 &
		ELSE PRINT #1,"NO QUEUE FILE FOUND -- WILL INITIALIZE" &
	\ I9%=-1% &
	\ RESUME 1050 &
	! CHECK FOR PRESENCE OF QUEUE FILE - IF NOT FOUND, INIT IT. &

1050	ON ERROR GOTO 19000 &
	\ PRINT"STARTED AT: ";TIME$(0%);" ON ";DATE$(0%) &
	\ OPEN PKG.LOC$+"QUEUE.WRK" AS FILE 4% &
	\ OPEN PKG.LOC$+"QUEUE.SYS" AS FILE 2% &
	\ CHANGE SYS(CHR$(12%)) TO M% &
	\ IF M%(19%)+SWAP%(M%(20%)) AND 1024% THEN &
		PRINT "QUEUE FILE OPENED BY ANOTHER PROGRAM" &
	\	INPUT "TRY AGAIN (Y/N) <N> ";Z$ &
	\	IF CVT$$(LEFT(Z$,1%),32%) <> "Y" THEN &
			GOTO 32767 &
		ELSE	GOTO 1050 &
		! PRINT MSG, CHECK-OUT QUEUE. &

1060	H0%=M%(13%)+SWAP%(M%(14%)) &
	\ C%=(M%(21%) AND 255%) &
	\ F%=(H0%/C%)*C%-17% &
	\ IF (H0%/C%)*C% <> H0% THEN I9%= 1% &
	! SET UP CLUSTERSIZE, SUBSCRIPT BASE; IF FILE LENGTH NOT A &
	!	MULTIPLE OF THE CLUSTERSIZE, FILE NEEDS INITIALIZATION. &

1070	IF I9%>=0% THEN &
		IF Z0%(0%,4%)=0% THEN Z0%(0%,4%)=-1% &
		ELSE &
		PRINT "QUEUE FILE NOT CLOSED PROPERLY -- "+ &
			"NOW CHECKING DATA CONSISTENCY" &
				! CHECK 'DIRTY' FLAG. &

1080	Z0%(0%,5%)=-1% &
	\ Z0%(0%,3%)=0% &
	\ Z$=Z$(0%) &
		! SET QUEMAN FLAGS, ENABLE EVERYTHING. &

1110	Q%=SWAP%(CVT$%(MID(SYS(CHR$(6%)+CHR$(-12%)),13%,2%))) &
	! SAVE MONITOR LOCATION FOR LATER: &
	 	! 	Q%	JOBCNT &

1120	M$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ P%=0% &
	\ R%=-1% &
	! ENABLE ^C TRAP;INIT. PRIORITY AND RUNBURST VALUES. &
	&

1150	M$=CHR$(128%+64%)+"ONL 5" &
	\ GOSUB 10160 &
		! SEND CMD TO OPSER TO PUT US IN ITS ONLINE TABLE. &
	\ W%=-1% &
	\ W0%=0% &
	\ GOTO 1370 IF I9%=1% &
	\ GOTO 1400 IF I9% &
		! SET US TO ATTACHED, INIT IF REQUIRED. &

1200	Z%(Z%)=0% FOR Z%=0% TO 255% &
	! CLEARS WORK ARRAY. &

1210	Z1%=Z0%(0%,0%) &
	\ Z1%=-1% IF Z0%(0%,7%)<>701% &
	\ GOTO 1370 UNLESS ((Z1% = 0%) OR ((Z1% > 6%) AND (Z1% < 256%))) &
	! POINTER TO FREE LIST MUST BE WITHIN RANGE. &
	! INVALIDATE Z1% IF THIS IS NOT THE CORRECT VERSION OF QUEUE.SYS &
	! ***** THIS LINE MUST BE CHANGED IF A SPOOLER TYPE IS ADDED. **** &
	&

1220	WHILE Z1% <> 0% &
	\	Z%(Z1%)= -1% &
	\	Z1%=Z0%(Z1%,0%) AND 255% &
	\ NEXT &
	! SET WORK ARRAY ENTRY IF CORRESPONDING ENTRY IN Z0%() IS IN &
	!	FREE LIST. &

1250	FOR Z3%=1% TO Z0%(0%,1%) &
	\ Z6%=Z0%(Z3%,0%) AND 255% &
	\ WHILE Z6% <> Z3% &
	\	IF Z%(Z6%) <> 0% THEN GOTO 1370 &
		ELSE &
		Z6%=Z0%(Z6%,0%) AND 255% &

1255	NEXT &
	\ NEXT Z3% &
	! CHECK AFTER QUEUE AND SPOOLER QUEUES FOR DUPLICATE &
	!	ENTRIES IN THE FREE LIST; IF ANY INITIALIZE "QUEUE.SYS". &

1300	FOR Z3%=2% TO Z0%(0%,1%) &
	\ Z6%=Z0%(Z3%,0%) AND 255% &
	\ WHILE Z6% <> Z3% &
	\	Z2%=SWAP%(Z0%(Z6%,1%)) AND 255% &
	\	IF Z2%=255% OR (Z2% AND 8%) <> 0% THEN &
			Z9%=Z0%(Z6%,0%) AND 255% &
	\		 GOSUB 10000 &
	\		Z6%=Z9% &
	\		GOTO 1330 &
	! IF JOB HEADER INCOMPLETE (STATUS = 255), OR 'KILL' BIT &
	!	SET IN STATUS ( = 8), RETURN ENTRY TO FREE LIST, &
	!	TELL OPSER. &

1302	IF Z2%=1% THEN &
		Z0%(Z6%,1%)=(Z0%(Z6%,1%) AND 255%) OR 512% &
	\	GOSUB 10050 &
	! IF JOB ALREADY SENT(=1%), RESET SENT BIT,SET HOLD BIT &
	!	AND TELL OPSER. &

1310	IF Z0%(Z6%,2%) <> 0% OR Z0%(Z6%,3%) <> 0% THEN &
		IF FNA%(Z6%)=0% THEN GOTO 1370 &
			UNLESS &
			Z0%(Z6%,2%) < PEEK(512%) &
				OR &
			(Z0%(Z6%,2%)=PEEK(512%) AND &
				Z0%(Z6%,3%) > PEEK(514%)) &
	! IF AFTER DATE OR TIME SPECIFIED, CHECK AFTER QUEUE FOR AN &
	!	ENTRY FOR IT; IF NOT THERE CLEAR THE QUE FILE. &

1320	Z6%=Z0%(Z6%,0%) AND 255% &
	! STEP POINTER TO GET NEXT ENTRY. &

1330	NEXT &
	\ NEXT Z3% &

1350	Z3%=1% &
	\ Z6%=Z0%(Z3%,0%) AND 255% &
	\ WHILE Z6% <> Z3% &
	\	IF FNA1%(Z6%)=0% THEN &
			I%=Z6% &
	\		Z9%=Z0%(Z6%,0%) AND 255% &
	\		Z7%=0% &
	\		GOSUB 2330 &
	\		Z6%=Z9% &
	\		GOTO 1362 &
	! FOR EACH ENTRY IN THE AFTER QUEUE THERE MUST BE AN ENTRY &
	!	IN A QUEUE LIST; IF NOT WILL REMOVE ENTRY. &

1360	Z6%=Z0%(Z6%,0%) AND 255% &
	! GET NEXT ENTRY. &

1362	NEXT &
	\ GOTO 1400 &
	! STEP THRU THE AFTER QUEUE. &

1370	I9%= -1% &
	\ PRINT "QUEUE FILE DATA INCONSISTENT - WILL INITIALIZE" &
	! SET TO DO INITIALIZATION OF "QUEUE.SYS". &

1400	IF I9% <> 0% OR I8% <> 0% THEN GOTO 1440 &
		ELSE &
		PRINT "QUEUE FILE DATA CHECKED FOR CONSISTENCY" &
	\	PRINT "# SPOOLERS ONLINE = 0; WILL CLEAR TABLE" &
				IF O%(0%,0%)=0% &
	\	GOTO 1440 IF O%(0%,0%)=0% &
	\	IF O%(0%,0%) < 1% OR O%(0%,0%) > 16% THEN &
			PRINT "ON-LINE SPOOLER TABLE CORRUPT"+ &
					" - WILL CLEAR TABLE" &
	\		GOTO 1440 &
	! COUNT OF ONLINE SPOOLERS MUST BE WITHIN RANGE. &

1410	FOR Z%=1% TO O%(0%,0%) &
		\ SPOOLERS.NAME$=FNU$(O$(Z%,1%)) &
		\ M$="% SPOOLER '"+SPOOLERS.NAME$+"' JOB #" &
			+NUM1$(O%(Z%,0%))+" TAKEN OFFLINE" &
		\ GOSUB 10160 &
			! SEND THE MESSAGE TO "OPSER". &

1420		M$=CHR$(2%)+CHR$(5%) &
		\ Z$=SYS(CHR$(6%)+CHR$(22%)+CHR$(-1%)+CHR$(0%)+SPOOLERS.NAME$ &
			+STRING$(16%-LEN(SPOOLERS.NAME$),0%)+M$) &
				! SEND "ONLINE" TOKEN TO ALL SPOOLERS. &

1430	NEXT Z% &

1440	O%(0%,0%)=0% &
		! CLEAR ONLINE SPOOLER TABLE. &

1450	RESTORE &
	\ READ Z$ UNTIL Z$="*STARTCMD" &
	\ Z1%=0% &

1460	WHILE Z$ <> "*ENDCMD" &
	\ READ Z$,Z% &
	\ Z1%=Z1%+1% &
	\ C0$(Z1%)=Z$ &
	\ C0%(Z1%)=Z% &
	\ NEXT &
	\ C0%(0%)=Z1%-1% &
	\ C0$(0%)="" &
	\ C0$(Z2%)="" FOR Z2%=Z1% TO 27% &
	\ C0%(Z2%)=0% FOR Z2%=Z1% TO 31% &
	\ GOTO 1480 &
	! SET LEGAL COMMANDS AND CMD. CODES. &

1470	DATA	*STARTCMD, &
	LAST,		1, &
	END,		2, &
	OFFLINE,	3, &
	STATUS,		4, &
	NEXT,		5, &
	DISABLE,	6, &
	ENABLE,		7, &
	*ENDCMD, &

1480	FOR Z%=0% TO 63% &
	\	H%(Z%,Z1%)=0% FOR Z1%=0% TO 15% &
	\	H%(Z%,2%)=Z% &
	\	H%(Z%,5%)= -256% &
	\	H%(Z%,15%)= -256% &
	\ NEXT Z% &
	! INIT POINTERS TO STRING STORAGE. &

1500	IF I9% <> 0% THEN &
		GOTO 2770 &
	ELSE	GOTO 2490 &
	! DO THE INITIALIZATION OF THE QUEUE FILE. &

1550	GOTO 2490 &
	! RETURN HERE IF NO KB: INPUT. &
	&

2300	! PUT ENTRY AT I% INTO LIST #Z4% IN 'PRIORITY' ORDER &
	!  PART I - FIND POSITION IN LIST Z4%
2310	Z7%=Z4% &
	\ I0%=Z0%(I%,4%) &
	! GET LIST ADDRESS; HOLD PRIORITY. &

2320	IF Z0%(Z7%,4%)<I0% THEN Z7%=SWAP%(Z0%(Z7%,0%)) AND 255% &
		ELSE IF(Z0%(Z7%,0%) AND 255%)<>Z4% &
			THEN Z7%=Z0%(Z7%,0%) AND 255% &
	\ GOTO 2320 &
	! SCAN QUEUE UNTIL: &
	!	 1) PRIORITY POSITION IS FOUND; OR &
	!	 2) YOU RUN OUT OF QUEUE. &

2330	! PART II - PUT ENTRY INTO POSITION &
	! REMOVE ENTRY #I% FROM CURRENT LIST; &
	! INSERT IT INTO LIST AFTER POSITION Z7%. &
	! RETURNS WITH I% UNCHANGED, Z7% POINTING TO ENTRY IN ORIGINAL LIST &
	! BEFORE THE ONE WHICH WAS MOVED.
2340	IF Z7%=I% THEN I0%=Z7% &
	\ GOTO 2370 &
	! IN CASE WE'RE TRYING TO PUT A JOB INTO IT'S CURRENT POSITION. &

2350	I0%=Z0%(I%,0%) &
	\ I1%=I0% AND 255% &
	\ I0%=SWAP%(I0%) AND 255% &
	\ Z0%(I0%,0%)=(Z0%(I0%,0%) AND NOT(255%)) OR I1% &
	\ Z0%(I1%,0%)=(Z0%(I1%,0%) AND 255%) OR SWAP%(I0%) UNLESS I1% = 0% &
	! ENTRY # I% IS NOW OUT OF ORIGINAL POSITION IN LIST. &

2360	I1%=Z0%(Z7%,0%) AND 255% &
	\ Z0%(I%,0%)=SWAP%(Z7%) OR I1% &
	\ Z0%(Z7%,0%)=(Z0%(Z7%,0%) AND NOT(255%)) OR I% &
	\ Z0%(I1%,0%)=(Z0%(I1%,0%) AND 255%) OR SWAP%(I%) UNLESS I1% = 0% &
	! I% IS NOW IN NEW POSITION IN NEW LIST. &

2370	Z$=Z$(0%) &
	\ Z7%=I0% &
	\ RETURN &
	! RETURN LEFT PNTR(ORIGINAL I%). &

2480	! &
	! &
	! &
	!	K B :   I N  P U T &
	! &

2490	PRINT IF CCPOS(0%) <> 0% UNLESS W0% <> 0% &
	\ PRINT #1%,"#"; UNLESS W0% <> 0% &
	\ W0%=-1% &
	\ GET #1% &
	\ FIELD #1%,1% AS Z$ &
	\ FIELD #1%,RECOUNT AS Z0$ &
	\ Z0$=CVT$$(Z0$,38%) &
	\ W0%=0% &
	\ GOTO 2490 UNLESS LEN(Z0$) <> 0% &
	\ GOTO 2540 &
	! GET SOME INPUT AND DISPATCH. &

2520	Z0$="^Z" &

2530	PRINT #1%,"INVALID RESPONSE -- ";Z0$ &
	\ GOTO 2490 &
	! BAD RESPONSE. &

2540	Z1%=INSTR(1%,Z0$,"/") &
	\ Z1%=LEN(Z0$)+1% IF Z1%=0% &
	\ Z1$=LEFT(Z0$,Z1%-1%) &
	\ Z2$=RIGHT(Z0$,Z1%+1%) &
	\ GOTO 2610 IF LEN(Z2$)=0% &
	! GET CMD IN Z1$, SWITCH(ES) IN Z2$ &

2550	Z1%=INSTR(1%,Z2$,"/") &
	\ Z1%=LEN(Z2$)+1% IF Z1%=0% &
	\ Z3$=RIGHT(Z2$,Z1%+1%) &
	\ Z2$=LEFT(Z2$,Z1%-1%) &
	! 1ST SWITCH IN Z2$, 2ND,IF ANY, IN Z3$. &

2560	Z1%=INSTR(1%,Z2$,":") &
	\ IF LEFT(Z2$,3%)="RUN" THEN &
		GOTO 2620 UNLESS TUNE.PRIV% &
	\	R%=VAL(RIGHT(Z2$,Z1%+1%)) &
	\	GOTO 2590 &
	! GET SPECIFIED RUN BURST IN R%. &
	! ERROR UNLESS USER HAS TUNE PRIVILEGE &

2570	GOTO 2530 IF LEFT(Z2$,3%) <> "PRI" &
	\ GOTO 2620 UNLESS TUNE.PRIV% &
	\ Z0%=0% &
	\ IF MID(Z2$,Z1%+1%,1%)="+" THEN &
		Z0%=1% &
		ELSE &
		IF MID(Z2$,Z1%+1%,1%)="-" THEN &
			Z0%= -1% &
	! SET FLAG IF SIGN INCLUDED WITH PRIORITY VALUE. &
	! ERROR UNLESS USER HAS TUNE PRIVILEGE &

2580	Z1%=Z1%+1% IF Z0% <> 0% &
	\ P%=VAL(RIGHT(Z2$,Z1%+1%)) &
	\ P%=P%*Z0% IF Z0% <> 0% &
	! GET THE PRIORITY VALUE. &

2590	IF LEN(Z3$) <> 0% THEN &
		Z2$=Z3$ &
	\	Z3$="" &
	\	GOTO 2560 &
	! PROCESS OTHER SWITCH IF PRESENT. &

2600	GOTO 2490 IF LEN(Z1$)=0% &
	! NO CMD SPECIFIED, SWITCH(ES) ONLY, SO RETURN. &

2610	GOTO 2770 IF LEFT(Z1$,3%)="INI" &
	\ GOTO 2650 IF LEFT(Z1$,3%)="DET" &
	\ GOTO 2530 &
	! PROCESS THE CMD. &

2620	PRINT #1%, "?Switch requires TUNE privilege" &
	\ GOTO 2490 &
	! DISPLAY TUNE PRIV NEEDED ERROR &
	! GO BACK TO COMMAND PROMPT &

2650	! &
	! &
	! &
	!	D E T A C H   C M D . &
	! &

2660	CLOSE Z% FOR Z%=1% TO 6% &
	\ W%=0% &
	\ M$=SYS(CHR$(8%)+F$) &
	! CLOSE ALL FILES, SET DETACHED FLAG, FILE NAME TO CORE COMMON. &

2670	P$=CHR$(-1%)+CHR$(P%) &
	\ R$=CHR$(0%)+CHR$(0%) &
	\ R$=CHR$(-1%)+CHR$(R%) IF R% <> -1% &
	\ M$=SYS(CHR$(6%)+CHR$(-13%)+CHR$(-1%)+P$+R$+CHR$(0%)) IF TUNE.PRIV% &
	! SET PRIORITY AND RUNBURST AS SPECIFIED. &

2680	CHAIN PKG.LOC$+"QUMRUN" LINE (31000% OR NOT 32767%) &

2770	! &
	! &
	! &
	!	I N I T   C M D . &
	! &

2780	IF O%(0%,0%)<>0% THEN PRINT #1%,"SPOOLERS ON-LINE -- CAN'T INIT" &
	\ GOTO 2490 &
	! CAN'T INIT IF SPOOLERS ARE PRESENT. &

2790	Z0%(0%,Z0%)=0% FOR Z0%=0% TO 15% &
	\ RESTORE &
	\ READ Z$ UNTIL Z$="*STARTQU" &
	\ Z0%=1% &
	\ Z0%(Z0%,0%)=Z0%+SWAP%(Z0%) &
	\ Z0%(Z0%,1%)=-1% &
	\ Z0%(Z0%,3%)=0% &
	\ Z0%(Z0%,4%)=32767% &
	! INIT BASE PNTR, SET UP FOR DEVICES, SET UP 'AFTER' QUEUE. &

2800	READ Z$ &
	\ IF Z$<>"*ENDQU" THEN Z0%=Z0%+1% &
	\ Z0%(Z0%,0%)=Z0%+SWAP%(Z0%) &
	\ Z0%(Z0%,1%)=CVT$%(Z$) &
	\ Z0%(Z0%,4%)=255% &
	\ GOTO 2800 &
	! INIT EACH DEVICE. &

2810	Z0%(0%,1%)=Z0% &
	\ Z0%(0%,0%)=(Z0%(0%,0%) AND NOT(255%)) OR (Z0%+1%) &
	\ Z0%(0%,8%)=1% &
	\ Z0%(0%,4%)= -1% &
	\ Z0%(0%,5%)= -1% &
	\ Z0%(0%,7%)= 701% &
	\ Z0%(Z1%,0%)=Z1%+1% FOR Z1%=Z0%+1% TO 254% &
	\ Z0%(255%,0%)=0% &
	\ Z1%=Z0%(0%,0%) &
	! SET UP # QUEUE LISTS, FREE LIST PTR., SEQ #, AND FREE LIST. &
	! Z0%(0%,7%) IS THE QUEUE.SYS VERSION NUMBER. &
	!	IT MUST BE THE SAME AS IS CHECK AT LINE 1210. &
	&

2820	OPEN PKG.LOC$+"QUEUE.SYS" AS FILE 2% &
	\ CHANGE SYS(CHR$(12%)) TO M% &
	\ C%=(M%(21%) AND 255%) &
	\ Z1%=M%(13%)+SWAP%(M%(14%)) &
	\ F%=(Z1%/C%)*C%-17% &
	\ IF ((Z1%/C%)*C% <> Z1%) OR Z1%<17% THEN &
		Z$=Z$(FNR%(F%+Z0%(0%,1%)+2%)) &
	\	GOTO 2820 &
	! GET CLUSTERSIZE,FILE SIZE, & HIGHEST SUBSCRIPT FOR THIS &
	!	CLUSTER; IF THE FILE SIZE NOT A MULTIPLE OF THE CLUSTER &
	!	SIZE, ACCESS THE FIRST SUBSCRIPT IN THE ARRAY TO EXPAND. &

2830	NAME PKG.LOC$+"QUEUE.SYS" AS "QUEUE.SYS<40>" &
	\ PRINT #1%,"INITIALIZED" &
	\ GOTO 2490 &
	! CHANGE PROT. CODE OF QUEUE FILE, ANNOUNCE IT'S DONE. &

2850	DATA *STARTQU, &
	"LP    ", &
	"BA    ", &
	"PP    ", &
	"RJ    ", &
	"NR    ", &
	*ENDQU,
2851	! THE ORDER OF THESE DATA STATEMENTS MUST BE PRESERVED, &
	!	ENTRIES MUST BE ADDED AT END ONLY. ************ &
	&

10000	! &
	! &
	! &
	!	S U B R O U T I N E S &
	! &

10010	GOSUB 10110 &
	\ IF Z0%(Z6%,2%) <> 0% OR Z0%(Z6%,3%) <> 0% THEN &
		IF FNA%(Z6%) <> 0% THEN &
			I%=Z7% &
	\		Z7%=0% &
	\		GOSUB 2330 &
	\		Z%(I%)= -1% &
	! RETURN THE AFTER QUEUE ENTRY, IF PRESENT, TO FREE LIST. &

10020	I%=Z6% &
	\ Z7%=0% &
	\ GOSUB 2330 &
	\ Z%(I%)=0% &
	! RETURNS THE JOB ENTRY TO FREE LIST. &

10030	M$=M$+"QUEUED JOB INCOMPLETE" IF Z2%=255% &
	\ M$=M$+"QUEUED JOB IN 'KILL' STATUS" &
		IF (Z2% AND 8%) <> 0% UNLESS Z2%=255% &
	\ M$=M$+" - REMOVED"+CRLF$+CHR9$+CHR9$+"FROM QUEUE" &
	\ GOSUB 10160 &
	! BUILD MSG. AND SEND IT TO OPSER. &

10040	RETURN &
	&

10050	! SUBROUTINE &

10060	GOSUB 10100 &
	\ M$=M$+"JOB PREVIOUSLY SENT TO SPOOLER;" &
		+CRLF$+CHR9$+CHR9$+" WILL BE PUT INTO HOLD STATUS." &
	\ GOSUB 10160 &
	! BUILD  ENTIRE MESSAGE. &

10070	RETURN &
	&

10100	! S/R TO BUILD MSG. PREFIX &

10110	M$="% "+CVT%$(Z0%(Z3%,1%))+FNU1$(Z0%(Z6%,1%) AND 255%)+ &
		": "+RAD$(SWAP%(Z0%(Z6%,5%))) &
		+RAD$(SWAP%(Z0%(Z6%,6%)))+ &
		"["+NUM1$(Z0%(Z6%,7%) AND 255%)+ &
		","+NUM1$(SWAP%(Z0%(Z6%,7%)) AND 255%)+"];" &
	! BUILD MESSAGE PREFIX. &

10120	RETURN &
	&

10160	! S/R TO SEND MSG TO OPSER. &

10166	R1%=0% &
	! RESET 1ST RETRY COUNTER. &

10170	IF LEN(M$) > 19% THEN Z$=SYS(Y7$+CHR$(255%)+LEFT(M$,19%)) &
		\ M$=RIGHT(M$,20%) &
		\ R1%=0% &
		\ GOTO 10170 &
			! MULTIPLE SENDS REQUIRED. &

10176	R1%=0% &
			! RESET 2ND RETRY COUNTER. &

10180	Z$=SYS(Y7$+CHR$(LEN(M$)+1%)+M$) &
	\ R1%=0% &
			! LAST/ONLY MESSAGE SENT. &

10190	M$="" &
	\ RETURN &
	&

10200	! S/R TO PUT AN ENTRY INTO AFTER QUEUE FOR SLOT Z6% &

10210	I%=FNF% &
	\ IF I%=0% THEN GOTO 10250 &
		ELSE &
		Z0%(I%,1%)=Z3% &
	\	Z0%(I%,3%)=Z0%(Z6%,2%) &
	\	Z0%(I%,4%)=Z0%(Z6%,3%) &
	\	Z0%(I%,8%)=Z6% &
	! GET A FREE LIST ENTRY AND STORE DATA. &

10220	Z4%=1% &
	\ Z7%=Z4% &
	\ I0%=Z0%(I%,3%) &
	\ I1%=Z0%(I%,4%) &
	! SET INDEX POINTERS, SET DATE AND TIME FOR COMPARE. &

10230	IF Z0%(Z7%,3%) < I0% AND Z0%(Z7%,4%) < I1% THEN &
		Z7%=SWAP%(Z0%(Z7%,0%)) AND 255% &
		ELSE &
		IF (Z0%(Z7%,0%) AND 255%) <> Z4% THEN &
			Z7%=Z0%(Z7%,0%) AND 255% &
	\ GOTO 10230 &
	! SEARCH LIST UNTIL Z7% POINTS TO INSERT POSITION. &

10240	GOSUB 2330 &

10250	RETURN &
	&

10300	ON ERROR GOTO 10330 &
	\ OPEN PKG.LOC$+"QUEUE.WRK" FOR INPUT AS FILE #4% &
	\ CLOSE 4% &

10310	ON ERROR GOTO 10340 &
	\ OPEN PKG.LOC$+"OPSER1.WRK" FOR INPUT AS FILE #6% &

10320	ON ERROR GOTO 19000 &
	\ RETURN &

10330	IF ERR <> 5% THEN GOTO 19000 &
		ELSE &
		PRINT "'QUEUE.WRK' NOT FOUND - WILL INITIALIZE" &
	\	I8%= -1% &
	\	RESUME 10310 &

10340	IF ERR <> 5% THEN GOTO 19000 &
		ELSE RESUME 10350 &

10350	PRINT "'$OPSER1.WRK' NOT FOUND - - - CAN'T RUN" &
	\	STOP &
	\	GOTO 10350 &
	&

15000	DEF* FNP$(Z$)=MID(SYS(Y6$+Z$),7%,4%) &

15010	DEF* FNF% &
	\ I0%,FNF%=Z0%(0%,0%) AND 255% &
	\  IF I0% THEN Z0%(I0%,I1%)=0% FOR I1%=1% TO 15%
15020	FNEND &
	! RETURN THE FIRST FREE ENTRY NUMBER, IF THERE IS ONE, &
	! AFTER FIRST CLEARING IT. &

15050	DEF* FNR%(I%) &
	\ I%=I%-Z0%(0%,1%) &
	\ IF I% > F% THEN &
		F%=F%+C% WHILE I% > F% &
	\	F%=255% IF F% > 255% &
	\	Z$(F%)="" &
	!	FUNCTION:	SEE IF THE FILE NEEDS TO BE EXPANDED. &

15060	I%=1% IF I% < 0% &
	\ FNR%=I% &
	\ FNEND &
	&

15250	DEF* FNU1$(Z%) &
	\ FNU1$=CVT$$(NUM$(Z%),-2%) UNLESS Z%=255% &
	\ FNU1$="*" IF Z%=255% &
	\ FNEND &
	! FUNCTION TO RETURN UNIT NUMBER. &

15260	DEF* FNU$(Z$)=RAD$(SWAP%(CVT$%(Z$)))+ &
		RAD$(SWAP%(CVT$%(RIGHT(Z$,3%)))) &

15270	DEF* FNE$(I%,J%)=CVT$$(SYS(CHR$(6%)+CHR$(I%)+CHR$(J%)),128%) &

15300	! FUNCTION TO SEARCH "AFTER" QUEUE FOR ENTRY FOR Z6% &

15310	DEF* FNA%(Z6%) &
	\ FNA%=0% &

15320	Z4%=1% &
	\ Z7%=Z0%(Z4%,0%) AND 255% &
	! SET TO "AFTER" QUEUE, GET FIRST RIGHT-POINTER. &

15330	WHILE Z7% <> Z4% &
	\ IF Z6%=Z0%(Z7%,8%) THEN &
		IF Z0%(Z7%,1%)=Z3% THEN &
			IF Z0%(Z7%,3%)=Z0%(Z6%,2%) THEN &
			IF Z0%(Z7%,4%)=Z0%(Z6%,3%) THEN &
				FNA%= -1% &
	\		GOTO 15350 &
	! COMPARE ADDRESS OF CORRESPONDING ENTRY, QUEUE LIST OF &
	!	THAT ENTRY, AND AFTER DATE AND TIME. &

15340	Z7%=Z0%(Z7%,0%) AND 255% &
	\ NEXT &

15350	FNEND &
	&
	&

15400	! FUNCTION TO FIND OUT IF IN QUEUE &

15410	DEF* FNA1%(Z6%) &
	\ FNA1%=0% &

15420	Z4%=Z0%(Z6%,1%) &
	\ Z7%=Z0%(Z4%,0%) AND 255% &
	! QUEUE LIST OF THE ENTRY, FIRST RIGHT POINTER. &

15430	WHILE Z7% <> Z4% &
	\	IF Z0%(Z6%,8%) <> Z7% THEN &
		Z7%=Z0%(Z7%,0%) AND 255% &
		ELSE &
		FNA1%= -1% &
	\		GOTO 15450 &
	! SEARCH FOR THE QUEUE ENTRY TO WHICH THE AFTER ENTRY POINTS. &

15440	NEXT &

15450	FNEND &

15550	DEF* FNPRV%(PRIV$) &
	&
	\ CHANGE SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+ &
		PRIV$) TO M% &
	\ FNPRV% = (M%(3%)=0%) &
	\ FNEND &
	! Check to see if job currently has privilege named &
	! If privileged then return -1% &
	! Else return 0% &
	&
	&
	&

19000	! &
	!ERROR HANDLERS &
	! &

19010	RESUME 22000% IF ERL=1017% &
		! GO PROCESS RECEIVER DECLARATION ERRORS &
	\ RESUME 1550% IF ERR=13% AND ERL=2490% &
		! TAKE CARE OF WAITING FOR OPERATOR. &
	\ RESUME 2520% IF ERR=11% AND ERL=2490% &
		! ^Z TYPED WHEN ATTACHED. &
	\ GOTO 23000% IF ERL=10170% OR ERL=10180% &
		! GO PROCESS SEND ERRORS &
	\ RESUME 1430% IF ERL=1420% AND ERR=5% &
		! TRIED TO SEND THE "ONLINE TOKEN" TO A SPOOLER &
		! THAT WASN'T THERE. &

19090	IF ERR=28% THEN PRINT "QUEUE FILE ENDANGERED" &
	\ RESUME 32700 &
		! ^C IS AN IMPOSSIBLE PROBLEM. &

19100	IF ERR=4% AND ERL=1420% THEN CNT%=CNT%+1% &
		\ SLEEP 2% &
		\ IF CNT%<60% THEN RESUME 1420% &
		  ELSE CNT%=0% &
			\ RESUME 1430% &

19999	PRINT "??Program failure in QUEMAN" &
	\ PRINT CVT$$(RIGHT(FNE$(9%,ERR),3%),4%);" at line";ERL &
	\ RESUME 32767 &

22000	! &
	!	RECOVERY FROM RECEIVER DECLARED ERRORS &
	! &
	! &
	IF ERR=32% THEN &
		WAIT.SECS%=WAIT.SECS%+3% &
		\ IF WAIT.SECS%<=30% THEN &
			SLEEP 3% &
			\ GOTO 1017 &
			! in no small buffers, &
			!	incr wait counter &
			!	if less than 30 secs waiting, &
			!		sleep 3 more secs &
			!		and retry the declare &
	&

22010	PRINT IF CCPOS(0%) &
	\ PRINT "?Unable to declare receiver QUEMAN" &
			! ensure left margin &
			! display 1st line of error text &

22020	IF ERR=16% THEN &
		PRINT "?Receiver already exists" &
		\ GOTO 32767 &
		! if error 16 "?Name or account now exists" &
		!	show rcvr already exists &
		!	and exit &

22030	PRINT CVT$$(RIGHT(FNE$(9%,ERR),3%),4%) &
	\ GOTO 32767 &
		!unexpected error &
		!display it and exit &

23000	! &
	!	RECOVERY FROM SYS(SEND) CALL ERRORS &
	! &
	! &
	IF ERR=32% OR ERR=4% THEN R1%=R1%+1% &
			! 32%=?No buffer space available &
			! 4% =?No room for user on device &
			! &
		\ IF R1%>=20% THEN RESUME 23140% &
			      ELSE SLEEP 3% &
				\ RESUME &
			! GIVE THE SYSTEM A CHANCE TO RECOVER &
			! &

23020	PRINT "?QUEMAN CAN NOT RUN WITHOUT 'OPSER' ACTIVE" IF ERR=5% &
	\ PRINT "?PROTECTION VIOLATION"  IF ERR=10% &
	\ RESUME 32700% IF ERR=5% OR ERR=10% &

23100	GOTO 19999 &
			! JUST IN CASE SOMETHING STRANGE HAPPENS, GO &
			! STOP WITH THE ERROR MESSAGE &

23140	PRINT "MESSAGE FOR 'OPSER' BUT "; &
	\ PRINT "THERE ARE NO SMALL BUFFERS AVAILABLE" IF ERR=32% &
	\ PRINT "'OPSER' IS HIBERNATING, OR ITS MESSAGE TABLE IS FULL" &
		IF ERR=4% &
	\ PRINT &
	\ PRINT M$ &
	\ PRINT &
	\ RETURN &

32700	Z0%(0%,3%)=3% &
	\ Z0%(0%,I%)=0% FOR I%=4% TO 5% &
	\ Z$=Z$(0%) &
	\ Z$=FNE$(18%,0%) &
	\ Z$="QUEMAN SHUTTING DOWN" &
	\ PRINT Z$ &
	\ CLOSE I% FOR I%=1% TO 12% &
	\ Z$=SYS(CHR$(6%)+CHR$(-13%)+CHR$(-1%)+CHR$(-1%)+ &
		CHR$(-2%)+CVT%$(0%)+CVT%$(0%)+CVT%$(0%)) IF TUNE.PRIV%
32710	IF (SWAP%(PEEK(Q%)) AND 255%)<3% THEN M%(I%)=0% FOR I%=0% TO 30% &
	\ M%(0%)=30% &
	\ M%(1%)=6% &
	\ M%(2%)=8% &
	\ M%(3%)=VAL(J0$) &
	\ M%(28%)=255% &
	\ CHANGE M% TO M$ &
	\ M$=SYS(M$) &
	\ STOP &
	! IF CLOSED OUT BY SHUTUP, KILL YOURSELF (QUIETLY). &

32767	END
