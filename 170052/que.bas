2!		PROGRAM		: QUE
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
8!
10		EXTEND		! EXTEND
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
  !******************************************************************* &
	&

20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	! &
	! &

21	! VER/ED	EDIT DATE	REASON &
	! V9.0-06	14-MAY-84	(KMF) Send to local object type 6 &
	! V9.0-07	25-JUL-84	(RJS) Fix QU/L BA:(1,*) to list only &
	!				      jobs in accounts (1,*). &
	! V9.0-07	25-AUG-84	(PRL) Fix for multiple privileges &
	!				      Change pkg location to OPSER$: &
	! V9.0-09	06-MAR-85	(JAC) Fix for multiple V9.0 privs: &
	!				      /F: Must have SWCFG priv &
	!				      /K: Must have accounting priv &
	!				      /M: Must have accounting priv &
	!				      /Q: Use UU.3PP for checks &
	! V9.3-02	7-FEB-86	(DLS) Fix for /after entry overflow &
	! V9.3		08-JAN-87	(REG) Fix /AF:87.01.01 bug &
	! &

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
310!		1		'KB:QUE.CMD': KB: INPUT. &
   !		2		QUEUE FILE &
   !		6		OPSER1.WRK,VIRTUAL ARRAY &
   !		11		WORK FILE, FIELD VIRTUAL ARRAY &

400	! &
	! &
	! &
	!	V A R I A B L E    D E F I N I T I O N S &
	! &

401	!	VARIABLE NAME	USE &
	!
410	!	A%		STRING COMPARE &
	!	A7%()		STRING COMPARE &
	!	A8%()		STRING COMPARE &
	!	C$		INPUT LINE &
	!	C0$		INPUT LINE WORK SUBSTRING &
	!	C1$		CMD SUBSTRING &
	!	C3$		WORK STRING FOR '.EXT' DEFAULT
420	!	D$		SWITCH TABLE ENTRY FIELD &
	!	D%		SWITCH VARIABLE &
	!	D1$		DATE ARRAY &
	!	D8$		DATE WORK STRING &
	!	D8%		DATE WORK VARIABLE &
	!	D9%		DATE WORK VARIABLE &
	!	E$		ERROR STRING &
	!	E%		ERROR VARIABLE &
	!	E0%		ENTRY MODE FLAG
430	!	F$		FILE COUNT STORED IN VIRTUAL ARRAY &
	!	F0$		JOB/FILE NAME STRING BEING PROCESSED &
	!	F9%		FOUND FLAG, FNN9% &
	!	H0%		CURRENT QUEUE FILE SIZE &
	!	I%		INPUT FILE COUNT AND INDEX &
	!				INTO WORK ARRAYS &
	!	I0%		WORK INDEX INTO WORK ARRAYS &
	!	I1%		WORK INDEX INTO WORK ARRAYS &
	!	I2%		WORK INDEX INTO WORK ARRAYS &
	!	J1$		MODE VALUE STORED IN VIRTUAL ARRAY ENTRY &
	!	J2$		TYPE VALUE STORED IN VIRTUAL ARRAY ENTRY &

440	!	L%		MIN LENGTH OF MATCH FOR FNR9% &
	!	LPPN%		LITERAL PPN IN COMMAND STRING &
	!	M$		FILENAME STRING FOR BUILDING FILE PACKET &
	!				DATA STRING TO BE SENT TO QUEMAN &
	!	M%		WORK ARRAY FOR SYS CALLS &
	!	M0$		SEND MSG. STRING &
	!	M9%		MODE-DETECTED FLAG, SWITCH PROCESSING &
	!	O%		OUTPUT SPECIFICATION COUNT &
	!	OPER%		USER IS AN OPERATOR IF SET. &
	!	P%		CHARACTER POSITION FOR FNR9% &
	!	PPN%		USER'S PPN &
	!	P8%		USER'S PROJ NO. &
	!	P9%		USER'S PROG NO. &
	!	QUE.FIL$	QUEUE.SYS FILESPEC &
	!	Q0$		SYS CALL STRING - SEND TO QUEMAN
450	!	R%		SWITCH TABLE ENTRY FIELD &
	!	R8$		CHAIN ENTRY RETURN: RETURNED DATA &
	!	R9$		CHAIN ENTRY RETURN: PROGRAM NAME &
	!	R9%		CHAIN ENTRY RETURN: LINE # &
	!	S%		STATUS WORD FROM FSS &
	!	S0$		SWITCH STRING BEING PROCESSED &
	!	S1$		SWITCH TABLE ENTRY FIELD &
	!	S1%		STATUS VARIABLE - FSS &
	!	S8%,S9%		WORK VARIABLES, FND9% &
	!	T%		TOKEN OF RECEIVED MSG.; LENGTH OF MSG. &
	!	T1%		TEMP VARIABLE &
	!	T2%		TEMP VARIBLE &
	!	T0%		SWITCH TABLE ENTRY FIELD &
	!	WRK.FIL$	COMMON WORK FILESPEC
460	!	Y1$,Y1%		WORK ARRAYS - JOB/FILE STORAGE &
	!	Y2$		PARAMETER STORAGE FOR WORK ARRAYS &
	!	Y5%		JOB'S UNIT # &
	!	Y6%		JOB'S PRIORITY &
	!	Y8%		PRIORITY SWITCH VALUE
490	!	Z$-Z9%		WORK STRINGS AND VARIABLES &
	!
800	! &
	! &
	! &
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	! &
	! &

801!	FUNCTION/SUBROUTINE		USE &
   !
810!	S/R(LINE 10010)		BREAK UP CMD STRING &
   !	S/R(LINE 10110)		SWITCH PROCESSING &
   !	S/R(LINE 10320)		GET ENTRY FROM SWITCH TABLE &
   !	S/R(LINE 10500)		STORE A MSG &
   !	S/R(LINE 10530)		SEND A MSG TO QUEMAN &
   !	S/R(LINE 10590)		GET CMD STRING FROM KB: &
   !	S/R(LINE 10670)		SET UP LIST FOR PROCESSING JOB/FILES &
   !	S/R(LINE 10730)		BUILD JOB PACKET &
   !	S/R(LINE 10750)		BUILD JOB PACKET FOR 'KILL' CMD &
   !	S/R(LINE 10800)		GET NEXT REQUESTED JOB NAME 'L' CMD &
	&

820!	FNC1%(LINE 15020)	COMPARE TWO FILENAME STRINGS &
   !	FNC2%(LINE 15070)	COMPARE TWO INTEGERS &
   !	FND% (LINE 15400)	DETERMINE DEVICE TYPE &
   !	FND9%(LINE 15700)	GET DATE AND TIME &
   !	FNF% (LINE 15090)	BREAK UP FILENAME SPECIFICATION &
   !	FNL% (LINE 15060)	CALCULATE REQUIRED SWITCH STRING LENGTH &
   !	FNN9%(LINE 15600)	RETURN A NUMBER &
   !	FNP$ (LINE 15380)	PAD A STRING WITH NULL CHARACTERS &
   !	FNP0$(LINE 15260)	RAD 50 PACK &
   !	FNP9%(LINE 15450)	PRINT DEV: FILENAME .EXT &
   !	FNR% (LINE 15250)	CALCULATE CORRESPONDING STRING STORAGE &
   !					FOR A JOB IN QUEUE FILE &
   !	FNR9%(LINE 15500)	KEYWORD MATCH &
   !	FNS% (LINE 15310)	FIND FIRST OCCURRENCE OF NON-QUOTED &
   !					SUBSTRING &
   !	FNU$ (LINE 15270)	UNPACK RAD 50 NAME &
   !	FNV% (LINE 15800)	DETERMINE IF USER IS A VALID OPERATOR &
   !	FNZ$ (LINE 15050)	UNPACK RAD 50 FILE SPEC STRING &
   !	FNZ3$(LINE 15200)	UNPACK RAD 50 FILENAME &

830	! SYS CALLS :		6,-22	SPECIAL RUN PRIVILEGES &
   !				6,-21	DROP OTHER PRIVILEGES &
   !				6,-10	FILENAME STRING SCAN &
   !				6,-8	GET OPEN CHANNEL STATISTICS &
   !				6,-3	MONITOR TABLES, PART I &
   !				6,9,0	SYSTEM HEADER INFO &
   !				6,14	READ ACCOUNTING DATA &
   !				6,18,-1	SEND A MESSAGE &
   !	&
   !				7	READ CORE COMMON &
   !	&
   !				8	STORE COR COMMON &

840	! PEEKS :		( NONE ) &
	!
900	! &
	! &
	! &
	!	D I M E N S I O N    S T A T E M E N T S &
	! &
	! &

910	DIM #2%,Z0%(255%,15%),Z$(255%)=512% &
		! QUEUE.SYS FILE. &

920	DIM Y2$(2%) &
		! WORK ARRAY. &

930	DIM A7%(6%),A8%(6%), &
		Y1$(15%,9%),Y1%(15%,4%) &
		! WORK ARRAYS FOR CHANGING STRING AFTER FSS. &
		! ARRAYS FOR JOB & FILE STORAGE. &

940	DIM D%(12%), M%(30%) &
		! DAYS-IN-MONTH ARRAY; WORK ARRAY FOR SYS CALLS. &

950	DIM #6%, J9%(23%,7%),V%(15%,3%) &
		! 'OPSER1.WRK' FOR FNV%() &

999	! &
	! &
	! &
	! &
	!	M A I N    C O D I N G    A R E A &
	! &

1000	C$=SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
	\ C$=CVT$$(RIGHT(C$,3%),4%) &
	\ OPEN "_KB:QUE.CMD" FOR INPUT AS FILE 1% &
	\ Z$=SYS(CHR$(0%)) &
	\ DEF.OUT$="LP0" UNLESS E0% &
	\ E0%=4% &
	\ PRINT "SUBMIT"; IF DEF.OUT$<>"LP0" &
	\ PRINT "QUE"; IF DEF.OUT$="LP0" &
	\ PRINT "	V10.1-A ";C$ &
		! GET SYSTEM HDR. INFO, PRINT WITH PGM NAME, ETC. &

1010	Z$=SYS(CHR$(6%)+CHR$(-22%))+SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &
		! SET SPECIAL RUN PRIVILEGES, DROP OTHER PRIVILEGES. &

1020	PRIV.OFF$=CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ QUE.FIL$ = "OPSER$:QUEUE.SYS" &
	\ WRK.FIL$ = "OPSER$:OPSER1.WRK" &
	\ KB%=ASCII(MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),2%,1%))/2% &
	\ Z$=MID(SYS(CHR$(6%)+CHR$(14%)+STRING$(6%,0%)+CHR$(1%)),7%,2%) &
		UNLESS (E0% AND 16%) <> 0% &
	\ P8%=ASCII(RIGHT(Z$,2%)) &
	\ P9%=ASCII(Z$) &
	\ Q0$=CHR$(6%)+CHR$(22%)+CHR$(-1%)+CHR$(128%+6%)+STRING$(16%,0%) &
		! Send to local object type 6 &
	\ Z$=CHR$(0%) &
	\ Z$=Z$+CHR$(0%) UNTIL LEN(Z$)>=4% &
	\ Y2$(0%),Y2$(1%)=LEFT(Z$,4%) &
	\ Y2$(2%)=LEFT(Z$,2%) &

1030	P8%,P9%=255% IF (E0% AND 16%) <> 0% &
	\ PPN%=SWAP%(P8%)+P9% &
		! SET PROJ,PROG NOS. TO WILD IF THIS IS A LOGGED OUT CALL &
		! SAVE PPN AS A WORD &

1035	EXQTA% = FNPRIV%("EXQTA") &
	\ WACNT% = FNPRIV%("WACNT") &
	\ GACNT% = FNPRIV%("GACNT") &
	\ WREAD% = FNPRIV%("WREAD") &
	\ GREAD% = FNPRIV%("GREAD") &
	\ SWCFG% = FNPRIV%("SWCFG") &
		!SET/CLEAR USEFUL PRIVILEGE FLAGS &

1040	ON ERROR GOTO 19000 &
	\ Z$=SYS(PRIV.ON$) &
	\ OPEN QUE.FIL$ FOR INPUT AS FILE 11%, MODE 8192% &
	\ Z$=SYS(PRIV.OFF$) &
	\ FIELD #11%,2% AS F$,1% AS F$,1% AS J1$,2% AS J1$,1% AS J2$ &
	\ FIELD #11%,512% AS Z2$ &
		! OPEN, FIELD A WORK BUFFER - &
		!	FIELD JOB FILE COUNT,COUNT COUNT. &

1045	Z$=SYS(PRIV.ON$) &
	\ OPEN WRK.FIL$ FOR INPUT AS FILE #6% &
	\ Z$=SYS(PRIV.OFF$) &
	\ OPER% = FNV%(SWAP%(PPN%)) &
		! "OPSER1.WRK" FOR USE BY FNV%() &
		! SET/CLEAR OPERATOR FLAG. &

1050	E%,S%=0% &
	\ D1$="-JAN-FEB-MAR-APR-MAY-JUN-JUL-AUG-SEP-OCT-NOV-DEC" &
	\ RESTORE &
	\ READ D%(Z%) FOR Z%=1% TO 12% &
	\ Y1$(0%,1%),Y1$(1%,1%)=FNP0$("??????") &
	\ Y1%(0%,3%)=0% &
	\ GOSUB 10600 &
	\ GOTO 1100 IF E% <> 0% &
		! RESET ERROR & STATUS FLAGS; GET CMD LINE; END IF ERR &

1054	DATA 31,28,31,30,31,30,31,31,30,31,30,31 &
	&

1060	GOTO 32760 IF C1$="E" &
	\ Z$=SYS(PRIV.ON$) &
	\ OPEN QUE.FIL$ FOR INPUT AS FILE 2%, MODE 8192% &
	\ Z$=SYS(PRIV.OFF$) &
	\ GOSUB 10900 &
	\ E%=17% IF (Z0%(0%,3%) AND 5%) AND (C1$<>"L" AND C1$<>"S") &
	\ E%=15% UNLESS Z0%(0%,4%)=Z0%(0%,5%) &
	\ E%=15% UNLESS Z0%(0%,7%)=701% &
	\ GOSUB 10670 UNLESS E% <> 0% &
	\ GOTO 1100 IF E% <> 0% &
		! SET ERROR IF QUEING DISABLED OR FILE NOT SET BY QUEMAN &
		! OR WRONG VERSION OF QUEUE.SYS &

1070	IF (E0% AND 16%)<>0% AND (C1$<>"E") AND (C1$<>"L") AND (C1$<>"S") &
	THEN E%=1% &
	\ E$=C1$ &
	\ GOTO 1100 &
		! THE ONLY COMMANDS WHICH ARE LEGAL WHEN LOGGED OUT ARE &
		!	END AND LIST. &

1080	IF C1$="Q" THEN GOTO 2000 &
		ELSE IF C1$="K" THEN GOTO 3000 &
		ELSE IF C1$="F" THEN GOTO 4000 &
		! EXECUTE THE CMDS. &

1090	IF C1$="L" OR C1$="S" THEN GOTO 5000 &
		ELSE IF C1$="H" THEN GOTO 6000 &
		ELSE IF C1$="M" THEN GOTO 7000 &
		ELSE IF C1$=""  THEN GOTO 1100 &
		ELSE E%=1% &
	\	E$=C1$ &
		! EXECUTE CMDS. OR SET ERROR. &

1100	E$=Z$(0%) UNLESS ((ERR=32% OR ERR=4%) AND ERL=10550%) IF E%=17% &
	\ CLOSE 2% &
	\ GOTO 31100 IF (E0% AND 2%) <> 0% &
			IF E% = 0% &
	\ IF E% <> 0% THEN RESTORE &
	\	READ Z$ UNTIL Z$="*ERRST" &
	\	READ Z$ FOR I1%=1% TO E% &
	\	GOTO 31100	IF (E0% AND 2%) <> 0% &
	\	PRINT Z$; &
	\	IF LEN(E$) <> 0% THEN PRINT " - ";E$ &
			ELSE PRINT &
		! CLOSE QUEUE.SYS. &
		! EXIT IF CHAIN ENTRY. &
		! IF ERROR, PRINT THE ERROR INFO. &

1110	IF (E0% AND 1%) <> 0% THEN GOTO 32760 &
		ELSE GOTO 1050 &
		! IF CCL OR LOGGED-OUT ENTRY, EXIT. &
		!	ELSE GET MORE INPUT. &
	&

2000	! &
	! &
	! &
	!	" Q " - C M D    P R O C E S S I N G &
	! &

2010	E%=9% IF S% <> 0% &
	\ E%=1% IF O%>1% &
	\ E%=1% UNLESS I% <> 0% &
	\ E$=C1$ &
	\ E%=10% IF ((Z0%(0%,0%) AND 255%) = 0%) &
		 OR (AFTER% AND ((Z0%((Z0%(0%,0%) AND 255%),0%) AND 255%)=0%)) &
	\ E$="" IF E%=10% &
	\ IF E% <> 0% THEN GOTO 1100 &
		! ERROR IF WILDCARD PPN, MORE THAN 1 JOBNAME, &
		!	NO FILES SPECIFIED, OR NO ROOM IN &
		!	QUEUE.SYS TO STORE REQUEST. &

2015	IF Y1%(0%,1%) <> PPN% THEN &
	    IF NOT WACNT% THEN &
		IF (P8% <> (SWAP%(Y1%(0%,1%)) AND 255%)) OR NOT GACNT% &
		    THEN	E%=11% &
	\		 	E$=FNPPN$(Y1%(0%,1%)) &
	\			GOTO 1100 &
		!ERROR IF NOT AN OPERATOR AND JOB PPN DIFFERENT THAN USER'S &

2017	Z$=SYS(CHR$(6%)+CHR$(14%)+STRING$(4%,0%)+CVT%$(SWAP%(Y1%(0%,1%)))+ &
	CHR$(1%)) &
		! TRAP AN ERROR IF JOB [P,PN] IS NOT ON THE SYSTEM. &

2030	IF (Y1%(0%,4%) AND (64%+128%+16384%)) <> 0% THEN &
		E%=18% &
	\	E$=C1$ &
	\	GOTO 1100 &
		! '/HOLD', & '/UNHOLD' ARE ILLEGAL SWITCHES FOR 'Q' CMD &

2035	FOR I1%=Z2%+I%-1% TO Z2% STEP -1% &

2040	M$="_"+FNZ$(I1%) &
	\ DUMMY$ = SYS(PRIV.ON$) &
	\ OPEN M$ FOR INPUT AS FILE 12% UNLESS (Y1%(I1%,3%) AND 870%) &
		OR (Y1%(I1%,3%) AND 8%)=0% OR FND%(M$)=0% &
		OR (Y1%(0%,4%)/64% AND 8%) &
	\ CLOSE 12% &
	\ DUMMY$ = SYS(PRIV.OFF$) &
	\ IF	(FND%(M$)=0%) OR (Y1%(I1%,3%) <0%) &
	  THEN	E%=8% &
		\ E$=" "+RIGHT(M$,2%) &
		\ GOTO 1100 &
		! TEST FOR EXISTENCE OF EACH FILE IF DISK, NO WILDCARDS, &
		!	AND IT HAS A '.EXT'. &

2045	PRO%=0% &
	\ GOTO 2050 IF (Y1%(I1%,3%) AND 870%) &
		OR (Y1%(I1%,3%) AND 8%)=0% OR FND%(M$)=0% &
		OR (Y1%(0%,4%)/64% AND 8%) &
	\ CHANGE SYS(CHR$(12%)) TO M% &
	\ PRO%=M%(22%) &
		! GET DISK FILE'S PROTECTION CODE &

2046	DUMMY% = FNINSTALL.3PP%(SWAP%(Y1%(0%,1%))) IF Y1%(0%,1%) <> PPN% &
	\ ACFLAG% = SWAP%(CVT$%(MID(SYS(CHR$(6%)+CHR$(32%)+CHR$(0%)+CHR$(0%) &
		+CHR$(M%(5%))+CHR$(M%(6%))+STRING$(15%,0%)+CHR$(PRO%)) &
		,3%,2%))) &
	\ DUMMY% = FNDEINSTALL.3PP% &
	&
	\ IF	((ACFLAG% AND 2%)<>0%) &
	  OR	(((ACFLAG% AND 4%)<>0%) AND ((Y1%(I1%,4%) AND 4%)<>0%)) &
	  THEN	E%=8% &
	\	E$= "Protection Violation - " + RIGHT(M$,2%)+ &
						"<"+NUM1$(PRO%)+">" &
	\	GOTO 1100 &
	!	TEST FOR ILLEGAL PRIVILEGE. &
	!	IF WE DID THE OPEN ON THE FILE. &
	!	FIRST, INSTALL THIRD PARTY PRIVS &
	!	THEN LOOK UP THE ACCESS CONTROL FLAGS, AS DOCUMENTED BELOW: &
	!		BIT	MEANING &
	!		 0	CREATE/RENAME RIGHTS ARE GRANTED &
	!		 1	READ ACCESS IS NOT ALLOWED &
	!		 2	WRITE ACCESS IS NOT ALLOWED &
	!		3-4	RESERVED &
	!		 5	EXECUTE ACCESS IS NOT ALLOWED &
	!	DEINSTALL THIRD PARTY PRIVS, NO LONGER NEEDED &
	!	IF	WE DO NOT HAVE READ ACCESS TO FILE, &
	!	OR	WE DON'T HAVE WRITE ACCESS AND /DE WAS SPECIFIED &
	!	THEN	SIGNAL A PROTECTION VIOLATION &

2050	IF ((Y1%(I1%,4%) AND 4%)<>0%) AND &
		((((Y1%(I1%,3%) AND 870%)<>0%)) OR (PRO% AND 128%)) THEN &
		E%=1% &
	\	E$="/DE not valid for wild card files - "+RIGHT(M$,2%) &
	\	E$="/DE not valid for protected files - "+RIGHT(M$,2%)+ &
				"<"+NUM1$(PRO%)+">" IF PRO% &
	\	GOTO 1100 &
	!	ERROR IF '/DELETE' AND WILDCARD SPEC. &

2055	Y1$(I1%,1%)=FNP0$("??????") IF Y1$(I1%,1%)=Y2$(1%) &
	\ M$=Y1$(I1%,0%)+Y1$(I1%,1%)+Y1$(I1%,2%)+CVT%$(SWAP%(Y1%(I1%,1%))) &
		+CHR$(Y1%(I1%,4%))+CHR$(0%)+Y1$(I1%,3%) &
	\ GOSUB 10500 &
		! BUILD FILE PACKET AND STORE IT. &

2060	NEXT I1% &
	\ I1%=0% &

2090	GOSUB 10730 &
	\ GOSUB 10500 &
	\ M$=CHR$(6%) &
	\ GOSUB 10530 &
	\ GOTO 1100 &
		! BUILD JOB PACKET, STORE IT, SEND IT WITH 'QUE' TOKEN. &
	&

3000	! &
	! &
	! &
	!	" K " - C M D    P R O C E S S I N G &
	! &

3010	IF O%>1% OR (Y1%(0%,3%) AND 2047%) OR I%=0% THEN E%=1% &
	\ E$=C1$ &
	\ GOTO 1100 &
		! ERROR IF OUTPUT JOB NAME ENTERED OR IF NO INPUT &
		!	JOB NAMES SPECIFIED. &

3020	FOR I1%=Z2%+I%-1% TO Z2% STEP -1% &
	\ IF	NOT OPER% AND Y1%(I1%,1%)<>PPN% AND NOT WACNT% &
	  AND	(NOT GACNT% OR ((SWAP%(Y1%(I1%,1%)) AND 255%) <> P8%)) &
	  THEN	E%=11% &
	\ 	E$=FNPPN$(Y1%(I1%,1%)) &
	\ 	GOTO 1100 &

3030	IF (Y1%(I1%,4%) AND NOT(16384%)) <> 0% THEN &
		E%=18% &
	\	E$=C1$ &
	\	GOTO 1100 &
		! '/SEQ' ONLY LEGAL SWITCH FOR 'K' CMD. &

3040	IF FND%(FNU$(Y1$(I1%,0%))+":")=0% THEN &
		E%=8% &
	\ E$=FNZ$(I1%) &
	\ GOTO 1100 &
		! MUST BE DISK DEVICE. &

3045	Y1$(1%,1%)=FNP0$("??????") IF Y1$(1%,1%)=Y2$(1%) &
		! SET FILE NAME TO WILDCARDS IS NONE HAD BEEN SPECIFIED. &

3050	GOSUB 10750 &
	\ GOSUB 10500 &
	\ NEXT I1% &
	\ M$=CHR$(7%) &
	\ GOSUB 10530 &
	\ GOTO 1100 &
		! BUILD JOB PACKET, SEND WITH KILL TOKEN. &
	&

4000	! &
	! &
	! &
	!	" F " - C M D    P R O C E S S I N G &
	! &

4010	IF (NOT SWCFG% AND NOT OPER%) OR I%>1% OR O%>1% OR &
		(FNU$(Y1$(I%,1%))<>"??????" AND FNU$(Y1$(I%,1%))<>"") &
	THEN E%=1% &
	\ E$="" &
	\ GOTO 1100 &
		! MUST HAVE NO JOBS/FILES SPECIFIED. &
		! REQUIRED PRIVS: SWCFG *OR* OPER STATUS &

4020	GOSUB 10720 &
	\ IF E% THEN &
		GOTO 1100 &
	ELSE PRINT "Really flush the whole "+Z1$+Z0$+"queue"; &
	\	INPUT Z$ &
	\	IF LEFT(CVT$$(Z$,32%),1%)<>"Y" THEN &
			PRINT "FLUSH ABORTED" &
	\		GOTO 1100 &
	! GIVE OPR A CHANCE TO RECONSIDER. &

4030	I%=1% &
	\ Y1$(I%,0%)=FNP0$("SY") &
	\ Y1$(I%,1%)=FNP0$("??????") &
	\ Y1%(I%,1%)=-1% &
	\ Y1$(I%,8%)=CVT%$(-1%) &
	\ Y1%(I%,4%)=0% &
	\ GOTO 3010 &
	&

5000	! &
	! &
	! &
	!	" L " - C M D    P R O C E S S I N G &
	! &

5010	GOSUB 10720 &
	\ IF E% <> 0% THEN GOTO 1100 &
		! FIND LIST IF SPECIFIED. &

5030	IF I0%=-1% OR (I%=1% AND Y1$(I%,1%)=Y2$(1%)) THEN I%=1% &
	\ Y1$(Z2%,1%)=FNP0$("??????") &
	\ Y1$(Z2%,2%)=LEFT(Y1$(Z2%,1%),2%) &
	\ Y1$(Z2%,8%)=CVT%$(-1%) &
	\ Y1%(Z2%,1%)=SWAP%(255%)+255% UNLESS LPPN% &
		! SET WILDCARD DEFAULTS FOR FILE, EXT, AND PPN. &

5050	HDR.PRNT%=0% &
	\ Z4$=Z1$ &
	\ Z7%=Z4% &

5060	Y9%=Z7% &
	\ GOSUB 10800 &
	\ IF Z7%=0% THEN &
		GOSUB 10630 &
	\	GOTO 1100 &

5063	IF (SWAP%(Z0%(Z7%,0%)) AND 255%) <> Y9% THEN GOTO 5110 &
	ELSE IF (SWAP%(Z0%(Z7%,1%)) AND 255%)=255% &
		THEN GOTO 5060 &
	! SAVE PREVIOUS CONTENTS OF Z7% TO COMPARE TO REVERSE POINTER &
	!	OF NEXT ENTRY TO BE PROCESSED; IF DIFFERENT, THE QUEUE &
	!	LIST IS BEING CHANGED BY 'QUEMAN'; BETTER RESTART; &
	!	DO ALL FILES WHOSE STATUS IS NOT 255%. &

5065	IF FNR%(Z7%)+17% > H0% THEN &
		CLOSE #2% &
	\	SLEEP 1% &
	\	OPEN QUE.FIL$ FOR INPUT AS FILE #2%, MODE 8192% &
	\	GOSUB 10900 &
	\	GOTO 5065 &
	! IF FILE OPENED PRIOR TO QUEMAN'S EXTENDING IT, CLOSE & REOPEN. &
	&

5070	HDR.PRNT%=1% UNLESS HDR.PRNT%=-1% &
	\ GOSUB 10630 &
	\ PRINT " *"; IF Z6%=255% &
	\ PRINT Z6%; IF Z6%<>255% &
	\ PRINT "	";Z0$;"["+NUM1$(Z9%)+ &
		","+NUM1$(Z8%);"]"; &
	\ LSET Z2$=Z$(FNR%(Z7%)) &
	\ Z1$="/SE:"+NUM1$(Z0%(Z7%,15%)) &
	\ Z1$=Z1$+"/FO:"+ &
		RAD$(SWAP%(Z0%(Z7%,9%)))+RAD$(SWAP%(Z0%(Z7%,10%))) &
			IF Z4$="LP" &
	\ Z1$=Z1$+"	" IF Z4$<>"LP" AND C1$="S" &
	\ IF C1$="S" THEN &
		PRINT Z1$; &
	ELSE &
		Z1%=CVT$%(J1$) &
	\	Z1$=Z1$+"/LEN:"+NUM1$(Z1% AND 127%) IF (Z1% AND 127%) <> 0% &
	\	Z1$=Z1$+"/CON" IF (Z1% AND 128%) <> 0% &
	\	Z1$=Z1$+"/TRU" IF (Z1% AND 256%) <> 0% &
	\	Z1$=Z1$+"/LPF" IF (Z1% AND 512%) <> 0% &
	\	Z1$=Z1$+"/UPP" IF (Z1% AND 1024%) <> 0% &
	\	Z1$=Z1$+"/SKI" IF (Z1% AND 2048%) <> 0% &
	\	Z1$=Z1$+"/TOP" IF (Z1% AND 4096%) <> 0% &
	\	PRINT Z1$ &

5080	Z1$="" &
	\ Z1$="S" IF (Y5% AND 1%) <> 0% &
	\ Z1$=Z1$+"H" IF (Y5% AND 2%) <>0% &
	\ Z1$=Z1$+"A" IF (Y5% AND 4%) <> 0% &
	\ Z1$=Z1$+"K" IF (Y5% AND 8%) <> 0% &
	\ Z1$=Z1$+" " IF LEN(Z1$)=1% &
	\ Z1$="0 " IF LEN(Z1$)=0% &
	\ IF C1$="L" THEN &
		PRINT "			";Z1$+"/"+NUM1$(Y6%); &
	ELSE &
		PRINT TAB(44%);Z1$+"/"+NUM1$(Y6%); &
	&

5090	Z1$="" &
	\ Z1$=Z1$+"/AF:"+DATE$(Z0%(Z7%,2%))+":"+TIME$(Z0%(Z7%,3%)) &
		IF Z0%(Z7%,2%) <> 0% OR Z0%(Z7%,3%) <> 0% &
	\ PRINT Z1$ IF C1$="S" &
	\ Z1%=ASCII(J2$) &
	\ Z3$="" &
	\ Z3$="EMB" IF (Z1% AND 127%)=0% &
	\ Z3$="FTN" IF (Z1% AND 127%)=1% &
	\ Z3$="IMP" IF (Z1% AND 127%)=2% &
	\ Z1$=Z1$+"/TY:"+Z3$ IF Z3$ <> "" &
	\ Z1$=Z1$+"/JC:"+NUM1$(Z0%(Z7%,8%)) IF (Z0%(Z7%,8%) AND 255%)>1% &
	\ PRINT Z1$ IF C1$="L" &
	\ Z3%=0% &
	\ Z$="" &
	\ GOSUB 5100 UNLESS C1$="S" &
	\ GOTO 5110 IF (SWAP%(Z0%(Z7%,0%)) AND 255%) <> Y9% &
	\ GOTO 5060 &
		! LIST JOB DATA; SUBROUTINE TO LIST FILE DATA; &
		!	TEST BREAK IN QUEUE LIST LINKAGE, RESTART IF &
		!	'QUEMAN' HAS CHANGED THE LIST. &
	&

5100	FOR I2%=1% TO ASCII(F$) &
	\	FIELD #11%,I2%*20% AS Z$,20% AS Z$ &
	\	PRINT TAB(36%); FNZ3$(Z$); &
	\	Z%=ASCII(RIGHT(Z$,13%)) &
	\	Z1$="" &
	\	Z1$=Z1$+"/C:"+NUM1$(ASCII(RIGHT(Z$,15%))) &
			IF ASCII(RIGHT(Z$,15%)) > 1% &
	\	Z1$=Z1$+"/R" IF (Z% AND 2%) <> 0% &
	\	Z1$=Z1$+"/D" IF (Z% AND 4%) <> 0% &
	\	Z1$=Z1$+"/B" IF (Z% AND 8%) <> 0% &
	\	Z1$=Z1$+"/E" IF (Z% AND 16%) <> 0% &
	\	Z1$=Z1$+"/N" IF (Z% AND 32%) <> 0% &
	\	PRINT Z1$ &
	\ NEXT I2% &
	\ PRINT &
	\ RETURN &
		! PRINT OUT THE FILE NAMES. &

5110	PRINT "***** QUEUE CHANGING - WILL RESTART LISTING *****" &
	\ PRINT &
	\ Z1$=Z4$ &
	\ HDR.PRNT%=0% &
	\ GOTO 5050 &
	! REVERSE POINTER OF CURRENT ENTRY DOES NOT POINT TO ENTRY JUST &
	!	PROCESSED; RESTART THE LISTING FROM BEGINNING OF QUEUE. &
	&

6000	! &
	! &
	! &
	!	" H " - C M D    P R O C E S S I N G &
	! &

6010	IF (E0% AND 2%) <> 0% THEN E%=1% ELSE PRINT "COMMANDS ARE:" &
	\ PRINT "	Q	QUE A JOB" &
	\ PRINT "	K	KILL A JOB" &
	\ PRINT "	F	FLUSH A QUEUE" IF OPER% &
	\ PRINT "	M	MODIFY A JOB" &
	\ PRINT "	L	LIST THE QUEUE" &
	\ PRINT "	S	SHORT QUEUE LIST" &
	\ PRINT "	H	PRINT THIS TEXT" &
	\ PRINT "	E	EXIT TO MONITOR" &
		! PRINT THE HELP LIST. &

6020	GOTO 1100 &
	&

7000	! &
	! &
	! &
	!	" M " - C M D    P R O C E S S I N G &
	! &

7010	IF O% > 1% OR I% <> 0% THEN &
		E%=1% &
	\	E$=C1$ &
	\	GOTO 1100 &
		! ONE JOBNAME ONLY, NO INPUT FILES. &

7015	IF	NOT OPER% AND Y1%(0%,1%)<>PPN% AND NOT WACNT% &
	AND	(NOT GACNT% OR ((SWAP%(Y1%(0%,1%)) AND 255%) <> P8%)) &
	THEN	E%=11% &
	\ 	E$=FNPPN$(Y1%(0%,1%)) &
	\	GOTO 1100 &
		! MUST BE AN OPERATOR TO MOD OTHERS JOBS &
		! APPROPRIATE ACNT PRIVS WILL ALSO ALLOW OTHER JOB MODS &

7020	I1%=0% &
	\ IF (Y1%(I1%,4%) AND 63%) <> 0% THEN &
		E%=18% &
	\	E$=C1$ &
	\	GOTO 1100 &
		! FILE SPEC SWITCHES NOT ALLOWED ON JOB. &

7025	Y1$(0%,1%)=FNP0$("??????") IF Y1$(0%,1%)=Y2$(1%) &
		! SET JOB NAME TO WILDCARDS IF NONE HAD BEEN SPECIFIED. &

7030	GOSUB 10730 &
	\ GOSUB 10500 &
	\ M$=CHR$(8%) &
	\ GOSUB 10530 &
	\ GOTO 1100 &
		! BUILD JOB PACKET, STORE MSG, SEND WITH &
		!	'M' TOKEN = '8'. &
	&

10000	! &
	! &
	! &
	!	U S E R    S U B R O U T I N E S &
	! &

10010	! &
	! &
	! &
	!	SUBROUTINE:	BREAK UP COMMAND STRING. &
	!		ENTER WITH: &
	!			C$	COMMAND STRING WITHOUT GARBAGE. &
	!		RETURNS: &
	!			(SEE DOCUMENTATION.) &

10020	I1%=FNS%(C$,"<",1%) &
	\ I1%=FNS%(C$,"=",1%) UNLESS I1% <> 0% &
	\ RETURN IF E% <> 0% &
	\ C$=LEFT(C$,I1%-1%)+","+CHR$(0%)+RIGHT(C$,I1%+1%) &
		! REPLACE "=" WITH ","+CHR$(0%). &

10030	RETURN UNLESS LEN(C$) <> 0% &
	\ I1%,I2%=1% &
	\ IF ASCII(C$)=0% THEN C$=RIGHT(C$,2%) &
	\	RETURN IF LEN(C$)=0% AND C1$="M" &
	\	I0%=Z2%-1% &
	\	F%=1% &
	\	Y2$(0%)=FNP0$("_SY") &
		! IF FIRST CHR IS CHR$(0%), WE ARE STARTING THE &
		! 	INPUT FILES - SET UP ACCORDINGLY. &

10040	I1%=FNS%(C$,",",I1%) &
	\ IF I1%=0% THEN I1%=LEN(C$)+1% ELSE &
		I2%=FNS%(C$,"(",I2%) &
	\ IF I2% <> 0% AND I2%<I1% THEN I1%,I2%=I1%+1% &
	\ GOTO 10040 &
		! FIND FIRST COMMA NOT WITHIN PARENS. &

10050	F0$=LEFT(C$,I1%-1%) &
	\ C$=RIGHT(C$,I1%+1%) &
	\ I0%=I0%+1% &
	\ IF I0%>Z1%+Z2% THEN E%=5% &
	\	E$=F0$+C$ &
	\	RETURN &
		! GET NEXT FILE AND CHECK FOR TOO MANY. &

10060	I1%,Y1%(I0%,4%)=0% &
	\ M9%=0% &
	\ GOSUB 10320 &
	\ WHILE I1%<>0% &
	\	Y1$(I0%,I1%-7%)=FNP$("",FNL%(D%),0%) &
			UNLESS (D%/8192% AND F%)=0% OR R% <> 0% &
				OR (F%=2% AND I1% < 10%) &
				OR F%=1% &
	\	Y1$(I0%,I1%+2%)=CHR$(1%) IF F%=1% AND I1%=1% &
	\	Y1%(I0%,2%)=1% IF F%=2% AND I1%=1% &
	\	Y1$(I0%,I1%-7%)=FNP0$("NORMAL") IF F%=2% AND R%=2% &
	\	Y1$(I0%,I1%-7%)=CHR$(0%) IF F%=2% AND R%=4% &
	\	Y1$(I0%,I1%+1%)=CVT%$(-1%) IF F%=1% AND I1%=7% &
	\	Y1$(I0%,I1%-7%)=CVT%$(-1%) IF F%=2% AND I1%=15% &
	\	Y1$(I0%,I1%-7%)=CHR$(1%) IF F%=2% AND I1%=13% &
	\	GOSUB 10320 &
	\ NEXT &
		! INITIALIZE SWITCH VALUES. &

10066	IF F%=1% THEN &
		Y1%(I0%,4%)=Y1%(0%,4%) AND 63% &
	\	Y1$(I0%,3%)=CHR$(Y1%(0%,2%)) &
		! FOR EACH FILE STORE FILE SWITCH DATA ENTERED WITH &
		!	JOB SWITCHES. &

10070	I1%=FNS%(F0$,"/",1%) &
	\ WHILE I1%<>0% &
	\	GOSUB 10110 &
	\	RETURN IF E% <> 0% &
	\	I1%=FNS%(F0$,"/",1%) &
	\ NEXT &
		! PROCESS SWITCHES. &

10080	Y1$(I0%,Z%)=Y2$(Z%) FOR Z%=0% TO 2% &
	\ Y1%(I0%,1%)=PPN% &
	\ Y1%(I0%,3%)=0% &
	\ IF LEN(F0$)=0% THEN &
		IF F%=2% THEN GOTO 10030 &
			ELSE E%=6% &
	\		E$="" &
	\		RETURN &
		! INIT FILE NAME ENTRIES, CHECK FOR NULL. &

10090	E$=F0$ &
	\ Z%=FNF%(F0$,I0%) &
	\ RETURN IF E% <> 0% &
	\ Y2$(0%)=Y1$(I0%,0%) UNLESS Y1$(I0%,0%)=Y2$(1%) &
	\ I%=I%+1% IF F%=1% &
	\ O%=O%+1% IF F%=2% &
	\ S%=S% OR (Z% AND 768%) &
		! INCREMENT JOB COUNT OR FILE COUNT DEP. UPON F%. &
		! S% IS SET IF PPN HAD WILDCARDS. &

10100	E%=5% IF O%>Z2% OR I%>Z1% &
	\ IF E% <> 0% THEN RETURN &
		ELSE GOTO 10030 &
		! DO THE FLNME SCAN, DO CLEAN-UP. &
	&

10110	! &
	! &
	! &
	!	SUBROUTINE:	SWITCH PROCESSING &
	! &

10120	I2%=FNS%(F0$,"/",I1%+1%) &
	\ RETURN IF E% <> 0% &
	\ I2%=LEN(F0$)+1% UNLESS I2% <> 0% &
	\ S0$=MID(F0$,I1%+1%,I2%-I1%-1%) &
	\ F0$=LEFT(F0$,I1%-1%)+RIGHT(F0$,I2%) &
		! FIND END OF SWITCH; HOLD SWITCH; &
		!	TAKE IT OUT OF FILENAME SPEC. &

10130	I1%=0% &
	\ I2%=FNS%(S0$,":",1%) &
	\ RETURN IF E% <> 0% &
	\ IF I2%=0% THEN E$="" &
		ELSE E$=RIGHT(S0$,I2%+1%) &
	\ 	S0$=LEFT(S0$,I2%-1%) &
		! COMPARE SWITCH NAME TO TABLE ENTRIES. &

10140	GOSUB 10320 &
	\ T%=2% &
	\ IF I1% <> 0% THEN &
		IF LEFT(S1$,T%)=LEFT(S0$,T%) THEN GOTO 10150 &
			ELSE GOTO 10140 &
		ELSE E%=4% &
	\ GOTO 10230 &
		! COMPARE SWITCH IN TO TABLE VALUES. &

10150	T%=T%+1% &
	\ IF T%>LEN(S0$) THEN GOTO 10160 &
		ELSE IF T%<=LEN(S1$) THEN &
		IF LEFT(S1$,T%)=LEFT(S0$,T%) THEN GOTO 10150 ELSE E%=4% &
	\ GOTO 10230 &
		! TEST ALL OF SWITCH NAME. &

10160	IF (D%/8192% AND F%)=0% THEN &
		IF I1% < 7% OR I1%=15% THEN GOTO 10140 &
		ELSE &
		E%=2% &
	\	GOTO 10230 &
		! CORRECT SIDE TEST OF SWITCH. &

10170	IF ((D% AND 1%) <> 0% AND I2%<>0%) &
	OR ((D% AND 2%) <> 0% AND I2%=0%) THEN GOTO 10220 &
		! ERROR IF ':N' NOT ALLOWED & PRESENT OR IF REQUIRED &
		!	& NOT PRESENT. &

10180	ON ERROR GOTO 10240 &
	\ GOTO 10220 IF LEN(E$)=0% UNLESS (D% AND 1%) <> 0% &
	\ D$=E$ IF (D% AND 4%) <> 0% OR ((D% AND 2%) <> 0% AND I2%<>0%) &
	\ GOTO 10240 IF ((VAL(D$)>255% OR VAL(D$)<0%) AND (D% AND 64%)) OR &
		(VAL(D$)<0% AND (D% AND 128%)) IF D% AND 192% &
	\ D$=CHR$(VAL(D$)) IF (D% AND 64%) <> 0% &
	\ D$=CVT%$(VAL(D$)) IF (D% AND 128%) <> 0% &
		! DO THE CONVERSION ON THE 'N' OF ':N'. &

10190	IF (D% AND 1024%) <> 0% THEN &
		Z9%=INSTR(1%,D$,"-") OR INSTR(1%,D$,".") &
	\	P%,Z8%,EXTR.DAY%,EXTR.HR%=0% &
	\	Z8%=FNR9%("+",1%) UNLESS Z9% &
	\	EXTR.DAY%=FNN9% IF Z9%=0% AND Z8% &
	\	Z8%,Z9%,P%,EXTR.DAY%=0% UNLESS FNR9%("DAYS",1%)<>0% OR &
			INSTR(P%,D$,":")=P%+1% OR P%=LEN(D$) OR Z9% &
	\	D$=CVT$$(DATE$(0%),32%)+":"+D$ IF Z9%=0% AND Z8%=0% &
	\	D$=CVT$$(DATE$(0%),32%)+RIGHT(D$,P%+1%) IF Z8% &
	\	Z9%=INSTR(1%,D$,":") &
	\	D$=D$+":23:59" IF Z9%=0% &
	\	P%=0% &
	\	E%=0% &
	\	GOTO 10220 IF FND9%(0%,1%)=0% &
	\	GOTO 10220 IF E% &
	\	D8$=CVT%$(1440%-D8%) &
	\	D$=CVT%$(D9%)+D8$ &
	\	AFTER%=-1% &
		! PROCESS 'AFTER' SWITCH. &

10200	IF I1%=11% AND T0%=0% THEN &
		IF (Y1%(I0%,4%) AND 2%^(I1%-1%)) <> 0% THEN &
			GOTO 10220 &
		ELSE &
		M9%= -1% &
		! CAN'T HAVE '/MODE' IF ANY OF THE OTHERS, &
		!	(/LE,/CN...) ALREADY IN; SET MODE DETECTED FLAG. &

10206	IF I1%=1% THEN &
		Y1%(I0%,2%)=VAL(D$) IF F%=2% &
	\	Y1$(I0%,I1%+2%)=D$ IF F%=1% &
	\	GOTO 10246 &
		! SAVE IF FILE COPIES ON JOB SWITCH. &

10208	IF I1%=7% AND F%=1% THEN &
		Y1$(I0%,I1%+1%)=D$ &
	\	I1%=15% &
	\	GOTO 10246 &
		! SET SEQ VALUE IF ON FILE SIDE. &

10210	IF R% <> 0% THEN &
		ON R% GOTO 10290,10310,10270,10280 &
		ELSE &
		Y1$(I0%,I1%-7%)=D$ UNLESS (D% AND 1%) <> 0% &
	\	GOTO 10246 &
		! PROCESS 'R-VALUE' SWITCHES. &

10220	E%=3% &

10230	E$=S0$+"" &
	\  GOTO 10250 &

10240	E%=11% &
	\ RESUME 15265 IF ERL = 15260 &
	\ RESUME 10230 &
		! TRAP BAD SWITCH VALUES. &

10246	Y1%(I0%,4%)=Y1%(I0%,4%) OR 2%^(I1%-1%) &
		! SET STATUS/SWITCH BIT FOR THIS SWITCH. &

10250	ONERROR GOTO 19000 &
	\ RETURN &
		! RESET ERROR TRAP, RETURN. &

10270	Y8%=ASCII(D$) OR 256% &
	\ GOTO 10246 IF  ((Y8% AND 255%)<129%) OR OPER% OR EXQTA% &
	\ GOTO 10240 &
		! PROCESS PRIORITY SWITCH &

10280	IF (Y1%(I0%,4%) AND 2%^(I1%-1%)) <> 0% THEN GOTO 10220 &
		ELSE &
		R%=INSTR(1%,"\EMB\FTN\IMP",D$) &
	\ GOTO 10240 UNLESS R% <> 0% &
	\ R%=(R%-2%)/4% &
	\ D$=CHR$(R%) &
	\ R%=0% &
	\ GOTO 10210 &
		! PROCESS TYPE SWITCH &
		! GET THE PROPER TYPE, CONVERT IT TO A STRING. &

10290	GOTO 10220 IF M9% <> 0% &
	\ D9%=CVT$%(D$) IF T0%=1% &
	\ D9%=2%^(T0%+5%) IF T0% <> 1% &
	\ Y1$(I0%,I1%-7%)=CVT%$(CVT$%(Y1$(I0%,I1%-7%)) OR D9%) &
	\ IF T0%=4% THEN &
		IF (Y1%(I0%,4%) AND 2%^(I1%)) <> 0% THEN GOTO 10220 &
			ELSE &
			Y1%(I0%,4%)=Y1%(I0%,4%) OR 2%^(I1%) &
	\		Y1$(I0%,I1%-6%)=CHR$(0%) &
		! ERROR IF 'MODE' SWITCH PROCESSED ALSO. &
		! SET MODE FROM D$. &
		! IF 'LPFORM' SWITCH SET TYPE IF NO 'TYPE' SWITCH PROC. &

10300	GOTO 10246 &

10310	D$=FNP0$(D$) &
	\ R%=0% &
	\ GOTO 10230 IF E%=11% &
	\ GOTO 10210 &
		! PROCESS FORMS NAME. &
	&

10320	! &
	! &
	!	SUBROUTINE:	GET NEXT SWITCH FROM DATA TABLE &
	! &
	&

10330	IF I1%=0% THEN RESTORE &
	\ READ S1$ UNTIL S1$="*STARTSW" &
		! RESTORE IF NECESSARY READ ENTRY. &

10340	READ I1%,S1$,T0%,D%,D$,R% &
	\ RETURN &
		! READ THE ENTRY INTO PARAMETERS. &

10350	DATA *STARTSW, &
	1,	COPIES,		0,	16386,	"",	0, &
	2,	RESTART,	0,	16385,	"",	0, &
	3,	DELETE,		0,	16385,	"",	0, &
	4,	BINARY,		0,	16385,	"",	0, &
	5,	END,		0,	16385,	"",	0, &
	6,	NHEADER,	0,	16385,	"",	0
10360	DATA &
	7,	HOLD,		0,	16385,	"",	0, &
	8,	UNHOLD,		0,	16385,	"",	0, &
	9,	PRIORITY,	0,	16450,	"",	3, &
	10,	AFTER,		0,	17410,	"",	0, &
	11,	MODE,		0,	16516,	"",	0, &
	12,	TYPE,		0,	16386,	"",	4, &
	13,	JCOPIES,	0,	16450,	"",	0, &
	14,	FORMS,		0,	16386,	"",	2, &
	15,	SEQUENCE,	0,	16514,	"",	0
10370	DATA &
	11,	LENGTH,		1,	16514,	"",	1, &
	11,	CNVERT,		2,	16513,	"",	1, &
	11,	TRUNCATE,	3,	16513,	"",	1, &
	11,	LPFORM,		4,	16513,	"",	1, &
	11,	UPPERCASE,	5,	16513,	"",	1, &
	11,	SKIP,		6,	16513,	"",	1, &
	11,	TOP,		7,	16513,	"",	1
10380	DATA &
	1,	COPIES,		1,	8258,	"",	0, &
	2,	RESTART,	1,	8193,	"",	0, &
	3,	DELETE,		1,	8193,	"",	0, &
	4,	BINARY,		1,	8193,	"",	0, &
	5,	END,		1,	8193,	"",	0, &
	6,	NHEADER,	1,	8193,	"",	0, &
	7,	SEQUENCE,	1,	8322,	"",	0
10400	DATA &
	0,	*ENDSW,		0,	0,	"",	0 &
	&

10410	DATA *ERRST, &
	"?Invalid command", &
	"?SW on wrong side of command", &
	"?Invalid switch format", &
	"?Undefined switch", &
	"?Too many files", &
	"?Null file spec" &

10420	DATA &
	"?MOUNT error for device", &
	"?Illegal input file", &
	"?Wildcards not allowed in PPN", &
	"?QUEUE full", &
	"?Bad switch value" &

10430	DATA &
	"?'MORE' requested on a CHAIN", &
	"?Not a QUEUEABLE device", &
	"?Unmatched quotes", &
	"?QUEUE not initialized", &
	"?QUEMAN not running - can't QUE OR KILL" &

10440	DATA &
	"?QUEUEING disabled", &
	"?Illegal switches for CMD" &

10450	DATA &
	*ENDERR &
	&

10500	! &
	! &
	! &
	!	SUBROUTINE:	STORE A MESSAGE &
	! &

10510	I2%=LEN(M$) &
	\ I2%=I2%-1% UNTIL ASCII(RIGHT(M$,I2%)) <> 0% OR I2%=0% &

10520	M0$=M0$+CHR$(5%)+CHR$(I2%)+LEFT(M$,I2%) &
	\ RETURN &

10530	! &
	! &
	! &
	!	SUBROUTINE:	SEND STORED MESSAGES &
	! &

10540	SND.RET%=0% &
	\ M0$=M0$+M$ &
	\ WHILE LEN(M0$)>0% &
	\	M$=CHR$(-LEN(M0$)*(LEN(M0$)<256%)-255%* &
			(LEN(M0$)>255%))+LEFT(M0$,19%) &
	\	M0$=RIGHT(M0$,20%) &
		! LENGTH BYTE IS 255 UNTIL LAST MSG SENT. &

10550	Z$=SYS(Q0$+M$) &
	\ E%=0% &
	\ E$="" &

10560	NEXT &

10570	RETURN &
		! SEND THE MESSAGES. &
	&
	&
	&

10590	! &
	! &
	! &
	!	SUBROUTINE:	GET COMMAND STRING FROM KB: &
	! &

10600	C0$="" &
	\ AFTER%=0% &
	\ PRINT IF (CCPOS(0%) <> 0%) AND (E0%<>2%) &
	\ PRINT "#"; IF (E0% AND 12%) <> 0% &
			OR (E0%=1% AND LEN(C$)=0%) &
	\ INPUT LINE #1%,C$ IF (E0% AND 12%) <> 0% &
			OR (E0%=1% AND LEN(C$)=0%) &
	\ GOTO 10600 IF ((E0% AND 12%) <> 0% &
			OR (E0%=1% AND LEN(C$)=0%)) AND &
			(ASCII(C$)=33% OR ASCII(C$)=59%) &
	\ PRINT C$; IF (E0% AND 8%) <> 0% &
	\ C0$=C0$+C$ &
	\ Z3%=INSTR(1%,CVT$$(C0$,32%),"/MORE") &
	\ WHILE Z3%<>0% &
	\	IF E0% AND 2% THEN E%=12% ELSE C0$=LEFT(C0$,Z3%-1%) &
	\	PRINT "MORE>"; &
	\	INPUT LINE #1%,C$ &
	\	C$="/MORE" IF ASCII(C$)=33% OR ASCII(C$)=59% &
	\	PRINT C$; IF (E0% AND 8%) <> 0% &
	\	C0$=C0$+C$ &
	\	Z3%=INSTR(1%,CVT$$(C0$,32%),"/MORE") &

10605	NEXT &
		! GET ALL OF THE COMMAND. &

10610	RETURN IF E% <> 0% &
	\ C$=CVT$$(C0$,-2%) &
	\ GOTO 1100 IF LEN(C$)=0% &
	\ C1$=LEFT(C$,1%) &
	\ C$=RIGHT(C$,2%) &
	\ Z1%=11% &
	\ Z2%=1% &
	\ O%,I%,Y8%=0% &
	\ F%=2% &
	\ I0%=-1% &
	\ Y2$(0%)=FNP0$(DEF.OUT$) &
	\ Y1$(0%,Z%)=Y2$(Z%) FOR Z%=0% TO 2% &
	\ C$=C$+"=" IF C1$="M" &
	\ IF LEN(C$) > 0% THEN GOSUB 10010 &
	\	RETURN IF E% <> 0% &
		! PROCESS FLNME PART. &

10620	Y1$(0%,1%)=Y1$(Z2%,1%) UNLESS (Y1%(0%,3%) AND 1%) <> 0% &
	\ RETURN &
		! GET LIST SET UP. &
	&

10630	! &
	! &
	! &
	!	SUBROUTINE:	PRINT LISTING HEADER &
	! &

10640	PRINT IF CCPOS(0%) <> 0% &
	\ RETURN IF HDR.PRNT%=-1% &
	\ IF ((Y1%(1%,1%)=-1%) AND (FNU$(Y1$(1%,1%))="??????") AND &
			(CVT$%(Y1$(1%,8%))=-1%)) OR HDR.PRNT% THEN &
	GOTO 10645 &
	ELSE &
	PRINT "No jobs matching ";Z4$; &
	\ PRINT NUM1$(Z5% AND 255%);IF Z5%<>255% &
	\ PRINT "*"; IF Z5%=255% &
	\ PRINT ":";FNU$(Y1$(1%,1%));FNPPN$(Y1%(1%,1%));"/SEQ:"; &
	\ PRINT NUM1$(CVT$%(Y1$(1%,8%))) IF CVT$%(Y1$(1%,8%))<>-1% &
	\ PRINT "*" IF CVT$%(Y1$(1%,8%))=-1% &
	\ GOTO 10650 &

10645	PRINT Z4$; &
	\ PRINT CHR$(Z5%+48%); IF Z5%<>255% &
	\ IF HDR.PRNT%=0% THEN &
		PRINT " queue is empty" &
	  ELSE	PRINT " SHORT"; IF C1$="S" &
	\	PRINT " QUEUE LISTING	"+DATE$(0%)+"	"+TIME$(0%) &
	\	PRINT "UNIT	JOB		"; &
	\	PRINT TAB(44%);"S / P" IF C1$="S" &
	\	PRINT "S / P      FILES" IF C1$="L" &
		! PRINT LISTING HEADING. &

10650	HDR.PRNT%=-1% &
	\ PRINT IF CCPOS(0%) <> 0% &
	\ RETURN &

10670	! &
	! &
	! &
	!	SUBROUTINE:	GET PROPER LIST SET UP &
	! &

10680	Z1$=FNU$(Y1$(0%,0%)) &
	\ Z0$=RIGHT(Z1$,3%) &
	\ Z1$=LEFT(Z1$,2%) &
	\ Z5%=255% &
		! GET DEVICE AND UNIT # &

10690	Z5%=VAL(Z0$) UNLESS Z0$="" &
		! THIS MUST BE ON A SEPARATE LINE FOR THE ERROR &
		!	 TRAP TO WORK. &

10700	RETURN IF CVT$%(Z1$)=Z0%(Z4%,1%) FOR Z4%=1% TO Z0%(0%,1%) &
		UNLESS ((Z5% AND -8%) AND (Z5%<>255%)) OR Z0$="255" &
		! FIND THE QUEUE LIST. &

10710	E%=13% &
	\ E$=Z1$+Z0$ &
	\ RETURN &
		! SET ERROR IF QUEUE LIST NOT FOUND. &
	&

10720	IF O% <> 0% OR I0%=-1% THEN RETURN &
		ELSE Y1$(0%,0%)=Y1$(Z2%,0%) &
			UNLESS Y1$(Z2%,0%)=FNP0$("SY") &
	\ GOSUB 10670 &
	\ RETURN &
	&

10730	! &
	! &
	! &
	!	SUBROUTINE:	BUILD A JOB PACKET &
	! &

10740	Y8%=128% UNLESS Y8% <> 0% &
	\ M$=Z1$+CHR$(Z5%)+Y1$(I1%,1%)+CHR$(Y8%)+ &
		RIGHT(Y1$(I1%,3%),3%)+CHR$(I%)+ &
		CHR$(Y1%(I1%,4%)/64% AND 255%)+Y1$(I1%,4%) &
		+Y1$(I1%,5%)+CVT%$(SWAP%(Y1%(I1%,1%)))+ &
		LEFT(Y1$(I1%,3%),2%)+Y1$(I1%,6%)+Y1$(I1%,7%)+Y1$(I1%,8%) &
	\ RETURN &
		! SET PRIORITY = 128 DEFAULT. &

10750	! &
	! &
	! &
	!	SUBROUTINE:	BUILD JOB PACKET FOR 'K' CMD &
	! &

10760	M$=Z1$+CHR$(Z5%)+Y1$(I1%,1%)+STRING$(4%,0%)+CHR$(0%)+ &
		STRING$(3%,0%)+CVT%$(SWAP%(Y1%(I1%,1%)))+STRING$(7%,0%)+ &
		Y1$(I1%,8%)+" " &
	\ RETURN &
	&
	! TACK ON A SPACE AT THE END SO WE DON'T DROP PART OF THE &
	! SEQUENCE NUMBER IF THE LOW ORDER BYTE IS NULL. &
	&

10800	! &
	! &
	! &
	!	SUBROUTINE:	GET NEXT REQUESTED JOBNAME &
	! &

10810	IF (Z0%(Z7%,0%) AND 255%)=Z4% THEN Z7%=0% &
	\ RETURN &
		! IF EOL, RETURN 0. &

10820	Z7%=Z0%(Z7%,0%) AND 255% &
	\ Z0$=RAD$(SWAP%(Z0%(Z7%,5%)))+RAD$(SWAP%(Z0%(Z7%,6%))) &
	\ Z9%=Z0%(Z7%,7%) &
	\ Z8%=SWAP%(Z9%) AND 255% &
	\ Z9%=Z9% AND 255% &
	\ Y5%=Z0%(Z7%,1%) &
	\ Z6%=Y5% AND 255% &
	\ Y5%=SWAP%(Y5%) AND 255% &
		! GET JOBNAME, PPN, UNIT #, AND STATUS. &

10830	Y6%=Z0%(Z7%,4%) &
	\ Y6%=Y6% AND 255% &
		! GET PRIORITY. &

10840	RETURN &
		IF (FNC2%(Y1%(I1%,1%) AND 255%,Z8%) &
		AND FNC2%(SWAP%(Y1%(I1%,1%)) AND 255%,Z9%) &
		AND FNC2%(Z5%,Z6%) AND FNC1%(FNU$(Y1$(I1%,1%)),Z0$))<>0% &
		AND (CVT$%(Y1$(I1%,8%))= -1% OR &
			(CVT$%(Y1$(I1%,8%))<>-1% AND &
				CVT$%(Y1$(I1%,8%))=Z0%(Z7%,15%))) &
		FOR I1%=Z2% TO Z2%+I%-1% &
		! MATCH PPN, JOBNAME, UNIT #. &

10850	Y9%=Z7% &
	\ GOTO 10810 &
		! GET NEXT IN LIST. &

10900	CHANGE SYS(CHR$(12%)) TO M% &
	\ H0%=M%(4%)*65536.+32768.+(M%(13%)+SWAP%(M%(14%)) EQV 32767%) &
	! GET FILE SIZE FROM SYS CALL. &

10910	RETURN &
	&

15000	! &
	! &
	! &
	!	U S E R - D E F I N E D    F U N C T I O N S &
	! &

15010	! &
	! &
	! &
	!	FUNCTION:	FILENAME COMPARE OF TWO STRINGS &
	! &

15020	DEF* FNC1%(S1$,S2$) &
	\ FNC1%=0% &

15030	CHANGE S1$ TO A7% &
	\ CHANGE S2$ TO A8% &
	\ GOTO 15040 IF A7%(I2%)<>A8%(I2%) AND A7%(I2%)<>63% &
		FOR I2%=1% TO A7%(0%) &
	\ FNC1%=-1% &
		! COMPARE THE TWO STRINGS, "?" ARE OK. &

15040	FNEND &
	&
	&

15050	DEF* FNZ$(I%)=CVT$$(FNU$(Y1$(I%,0%))+":"+FNU$(Y1$(I%,1%))+"." &
	+FNU$(Y1$(I%,2%))+"["+NUM1$(SWAP%(Y1%(I%,1%)) AND 255%)+"," &
	+NUM1$(Y1%(I%,1%) AND 255%)+"]",2%) &
		! FUNCTION:	UNPACK FILESPEC. STRING. &
	&
	&

15060	DEF* FNL%(T%)=(T% AND 64%)/64%+(T% AND 128%)/64%+(T% AND 256%)/32%+ &
		17%*(T% AND 512%)/512%+2%*(T% AND 1024%)/512% &
		! FUNCTION:	CALC. REQ'D. STRING LENGTHS. &
	&
	&

15070	DEF* FNC2%(A%,B%)=(A%=B% OR A%=255%) &
		! FUNCTION:	COMPARE A & B, OK IF A IS 255. &
	&
	&
	&

15080	! &
	! &
	! &
	!	FUNCTION:	BREAK UP A FILE SPECIFICATION &
	!				(WITHOUT SWITCHES) TO VARIABLES. &
	! &

15090	DEF* FNF%(F0$,I0%) &

15100	F0$=SYS(CHR$(6%)+CHR$(-10%)+F0$) &
	\ FNF%,Z%,Y1%(I0%,3%)=SWAP%(CVT$%(MID(F0$,29%,2%))) &

15110	LPPN% = Z% AND 128% &
\	Y1%(I0%,1%)=SWAP%(CVT$%(MID(F0$,5%,2%))) IF LPPN% &

15120	IF (Z% AND 8192%) <> 0% THEN &
		IF Z% AND (-32767%-1%) THEN Y1$(I0%,0%)=MID(F0$,23%,4%) &
			ELSE Z$="_"+MID(F0$,23%,2%) &
	\		Z$=Z$+NUM1$(ASCII(MID(F0$,25%,1%))) &
				IF ASCII(MID(F0$,26%,1%)) &
	\			Y1$(I0%,0%)=FNP0$(Z$) &

15130	Y1$(I0%,1%)=MID(F0$,7%,4%) IF (Z% AND 1%) <> 0% &
	\ Y1$(I0%,2%)=MID(F0$,11%,2%) IF (Z% AND 8%) <> 0% &
	\ IF (Z% AND 8%) = 0% &
	  THEN &
		C3$ = LEFT(FNU$(Y1$(0%,0%)),1%) &
	\	Y1$(I0%,2%) = CHR$(12%)+CHR$(78%)  IF C3$ = "L" &
	\	Y1$(I0%,2%) = CHR$(236%)+CHR$(21%) IF C3$ = "B" &
	\	Y1$(I0%,2%) = CHR$(178%)+CHR$(90%) IF C3$ = "N" &
	\	Y1$(I0%,2%) = CHR$(21%)+CHR$(114%) IF C3$ = "R" &
	\	Y1%(I0%,3%)=Y1%(I0%,3%) OR 8%	   IF C3$="L" OR C3$="B" &
			OR C3$="N" OR C3$="R" OR C3$="A" UNLESS C1$="K" &
		! STORE FILENAME AND EXT; &
		! DEFAULT EXT IS '.LST' IF LP:, &
		!		'.CTL' IF BA: &
		!		'.NTR" IF NR:. &
		!		'.RJE" IF RJ:. &

15140	FNEND &
	&
	&

15200	DEF* FNZ3$(Z$)=LEFT(FNU$(LEFT(Z$,4%)),3%)+":"+ &
		"["+NUM1$(ASCII(MID(Z$,12%,1%)))+ &
			","+NUM1$(ASCII(MID(Z$,11%,1%)))+"]" &
		+FNU$(MID(Z$,5%,4%))+"."+LEFT(FNU$(MID(Z$,9%,2%)),3%) &
		! FUNCTION:	UNPACK RAD50 FILE NAME. &
	&
	&

15250	DEF* FNR%(P%)=P%-Z0%(0%,1%) &
		! FUNCTION:	CALCULATE WHICH RECORD IN Z$(P%) &
		!			TO GET FOR THIS ENTRY. &
	&
	&

15260	DEF* FNP0$(Z$) &
	\ FNP0$=MID(SYS(CHR$(6%)+CHR$(-10%)+Z$),7%,4%)
15265	FNEND &
		! FUNCTION:	PACK NAME TO RAD50. &
	&
	&
	&

15270	DEF* FNU$(Z$)=RAD$(SWAP%(CVT$%(Z$)))+ &
		RAD$(SWAP%(CVT$%(RIGHT(Z$,3%)))) &
		! FUNCTION:	UNPACK 6-CHAR. NAME FROM RAD50. &
	&

15300	! &
	! &
	! &
	!	FUNCTION:	FIND FIRST OCCURRENCE OF A NON-QUOTED &
	!				SUBSTRING &

15310	DEF* FNS%(Z$,Z0$,Z%) &

15320	W0%=INSTR(Z%,Z$,Z0$) &
	\ IF W0%=0% THEN GOTO 15340 &

15330	W1%=INSTR(Z%,Z$,'"') &
	\ IF W1% <> 0% AND W1%<W0% THEN &
		W1%=INSTR(W1%+1%,Z$,'"') &
	\ 	IF W1%=0% THEN GOTO 15350 &
		ELSE Z%=W1%+1% &
	\ 	IF W1%<W0% THEN GOTO 15330 ELSE GOTO 15320 &

15340	FNS%=W0% &
	\ GOTO 15360 &

15350	E%=14%
15360	FNEND &
	&
	&

15370	! &
	! &
	! &
	!	FUNCTION:	TO PAD TO LENGTH WITH NULLS &
	! &

15380	DEF* FNP$(Z$,Z%,Z0%)=LEFT(Z$+STRING$(Z%-LEN(Z$),Z0%),Z%) &
	&
	&

15390	! &
	! &
	! &
	!	FUNCTION:	TO DETERMINE TYPE OF DEVICE &
	! &

15400	DEF* FND%(M$) &
	\ FND%=0% &
	\ Z8$=SYS(CHR$(6%)+CHR$(-10%)+M$) &
	\ S1%=SWAP%(CVT$%(MID(Z8$,29%,2%))) &
	\ IF (S1% AND 4096%) <> 0% AND S1% > 0% THEN &
		IF (STATUS AND 255%)=0% THEN &
			FND%= -1% &

15410	FNEND &
	&
	&
	&

15450	DEF* FNP9%(I%) &
	\ PRINT FNU$(Y1$(I%,Z%)); FOR Z%=0% TO 2% &
	\ FNEND &
		! FUNCTION:	PRINT DEV: FILENAME & EXT. &
	&
	&
	&

15500	DEF* FNR9%(S$,L%) &
	\ FNR9%=0% &
	! FUNCTION:	KEYWORD MATCHING ROUTINE. MATCHES A STRING IN &
	!		THE COMMAND STRING D$ STARTING AT POSITION P%+1% &
	!		TO THE DUMMY STRING S$. A MATCH IS MADE WHEN THE &
	!		MINIMUM NUMBER OF CHARACTERS ARE MATCHED. THIS &
	!		MINIMUM NUMBER IS HELD IN THE DUMMY VARIABLE &
	!		L%. &
	! PARAMETERS:	S$	STRING TO MATCH TO. &
	!		L%	MINIMUM LENGTH OF MATCH. &
	! GLOBAL &
	! VARIABLES &
	! AFFECTED:	P%	POSITION POINTER POINTS TO THE LAST CHAR- &
	!			ACTER MATCHED SUCCESSFULLY. &
	! LOCAL &
	! VARIABLES &
	! USED:		Z%	TEMPORARY CHARACTER POINTER. &
	!		Z$	CHARACTER MATCHING VARIABLE FOR PROCESS- &
	!			ING BEYOND THE MINIMUM LENGTH. &
	! RETURNS:	THE NUMBER OF CHARACTERS SUCCESSFULLY MATCHED. &
	! ERRORS:	NONE EXPECTED. &

15510	Z%=ASCII(RIGHT(D$,P%+1%)) &
	\ IF Z%=32% OR Z%=9% THEN P%=P%+1% &
				\ GOTO 15510 &
		! SKIP PAST BLANKS AND TABS. &

15520	IF MID (S$,1%,L%)=MID(D$,P%+1%,L%) THEN Z%=L% &
	ELSE GOTO 15550 &
		! SEARCH FOR MINIMUM MATCH. &

15530	Z$=MID(D$,P%+Z%+1%,1%) &
	\ GOTO 15540 IF Z$<>MID(S$,Z%+1%,1%) &
	\ Z%=Z%+1% UNLESS Z$="" &
	\ GOTO 15530 UNLESS Z$="" &
		! SEARCH FOR MORE MATCHING CHARACTERS. &

15540	P%=P%+Z% &
	\ FNR9%=Z% &
		! RETURN WITH P% POINTING TO THE LAST SUCCESSFULLY &
		! MATCHED CHARACTER. FUNCTION WILL RETURN AS THE NUMBER &
		! OF CHARACTERS MATCHED. &

15550	FNEND &
	&
	&
	&

15600	DEF* FNN9% &
	\ FNN9%,F9%=0% &
	! FUNCTION:	RETURN A NUMBER ROUTINE. &
	! PARAMETERS:	NONE &
	! GLOBAL &
	! VARIABLES &
	! AFFECTED:	P%	CHARACTERS POSITION POINTER. &
	!		F9%	FOUND FLAG. &
	! LOCAL &
	! VARIABLES &
	! USED:		Z%	TEMPORARY POSITION POINTER. &
	!		Z0%	ASCII CHARACTER REPRESENTATION. &
	! RETURNS:	VALUE OF NUMERIC DIGITS FOLLOWING POSITION P% &
	!		IN THE STRING D$. IF NONE ARE FOUND THE FOUND FLAG &
	!		IS RETURNED AS ZERO. &
	! ERRORS:	NONE EXPECTED. &

15610	ON ERROR GOTO 15650 &
	\ Z%=P%-1% &
	\ Z0%=-1% &
		! SET LOCAL ERROR HANDLER. &
		! SET LOCAL CHARACTER POINTER. &
		! INITIALIZE ASCII DIGIT FLAG. &

15620	WHILE (Z0%>=48% AND Z0%<=57%) OR Z0%=-1% &
		\ Z%=Z%+1% &
		\ Z0%=ASCII(RIGHT(D$,Z%+1%)) &
	\ NEXT &
		! CHECK FOR DIGITS, AND EXIT HERE WITH Z%=P% OR Z% &
		! POINTING TO THE LAST DIGIT FOUND. &

15630	IF Z%<>P% THEN FNN9%=VAL(MID(D$,P%+1%,Z%-P%)) &
				\ P%=Z% &
				\ F9%=-1% &
		! WE HAVE FOUND A NUMBER. SET THE FUNCTION VALUE TO &
		! THE VALUE OF THE NUMBER FOUND AND SET THE FOUND FLAG. &

15640	ON ERROR GOTO 19000 &
	\ FNN9%=0% IF E%<>0% &
	\ FNEND &
		! RESET THE ERROR HANDLER. &
		! IF ERR IN VAL FUNCTION, RETURN ZERO RESULT &
		! RETURN. &
	&

15650	E% = 512% + 52%	IF ERR/10% = 5% &
	\  RESUME 15640 &
		! PROCESS ERROR IN VAL FUNCTION &
	&

15700	DEF* FND9%(P1%,T%) &
	\ F9%,FND9%,Z8%=0% &
	! FUNCTION:	GET A DATE AND, OPTIONALLY, A TIME. &
	! PARAMETERS:	P1%	WORK FILE RECORD TO PLACE THE TIME OR DATE &
	!		IN. &
	!		T%	LOOK FOR TIME FLAG. IF <>0%, LOOK FOR TIME &
	! GLOBAL &
	! VARIABLES &
	! AFFECTED:	Z0%()	WORK-FILE. &
	!		P%	POSITION POINTER. &
	!		F9%	FOUND FLAG. &
	!		S%,S%()	STACK POINTER,STACK. &
	!		D%(0%)	DAYS IN MONTH SPECIFIED. &
	!		D%()	DAYS IN MONTH TABLE. &
	!		D1$	MONTHS DECODE STRING. &
	! LOCAL &
	! VARIABLES &
	! USED:		W2%	DATE WORD ACCUMULATOR. &
	!		W1%,Z3%	UTILITY VARIABLES. &
	! RETURNS:	FOUND FLAG SET IF DATE IS FOUND. &
	!		DATE RETURNED AS FUNCTION VALUE. &
	!		WORK-FILE UPDATED WITH DATE AND TIME WORDS. &
	! ERRORS:	ILLEGAL OPERAND &

15710	S9%=P% UNLESS Z8% &
	\ W1%=1% &
	\ LEP%,W2%=0% &
		! PUSH THE OLD CHARACTER POINTER. &
		! INITIALIZE THE MONTHS STRING DECODE POINTER. &
		! INITIALIZE THE DAY COUNTER. &

15715	IF INSTR(1%,D$,".") THEN &
		Z3%=FNN9% &
	\	GOTO 15770 UNLESS FNR9%(".",1%) &
	\	W1%=FNN9% &
	\	GOTO 15770 IF (W1% > 12%) OR (W1% < 1%) &
	\	W2%=W2%+D%(T1%) FOR T1%=1% TO W1%-1% UNLESS W1%=1% &
	\	W1%=(W1%-1%)*4%+1% &
	\	GOTO 15770 UNLESS FNR9%(".",1%) &
	\	D%(0%)=FNN9% &
	\	GOTO 15740 &

15720	D%(0%)=FNN9% &
	\ WHILE W1%<49% AND FNR9%(MID(D1$,W1%,4%)+CHR$(255%),4%)=0% &
		\ W1%=W1%+4% &
		\ W2%=W2%+D%((W1%-1%)/4%) &
	\ NEXT &
		! GET THE DAY AND SEARCH FOR THE MONTH ADDING IN THE &
		! DAYS OF EACH MONTH AS WE GO ALONG. &

15730	GOTO 15770 IF W1%=49% &
	\ S9%=P% &
	\ Z3%=FNR9%("-",1%) &
	\ IF Z3%<>0% THEN Z3%=FNN9% &
		ELSE &
		CHANGE SYS(CHR$(6%)+CHR$(-3%)) TO M% &
	\	Z3%=(M%(27%)+SWAP%(M%(28%)))/1000%+70% &
		! IF WE DIDN'T FIND A MONTH NO USE GOING ON. CHECK FOR &
		! YEAR AND GET IT ONE WAY OR ANOTHER. WE MUST SAVE THE &
		! CHARACTER POINTER BEFORE DOING THE YEAR CHECK. &

15740	F9%=0% &
	\ GOTO 15770 IF Z3%<70% OR Z3%>99% &
	\ LEP%,EXTR.DAY%=EXTR.DAY%+1% IF ((Z3%+EXTR.DAY%/365%) AND 3%)=0% &
		AND EXTR.DAY%<>0% AND &
		(W2%+D%(0%)+EXTR.DAY%-(W2%+D%(0%)+EXTR.DAY%)/1000%)*1000% >59% &
		AND ((Z3% AND 3%)<>0% AND (W1%<6%)) &
	\ IF (Z3% AND 3%)=0%  THEN D%(2%)=D%(2%)+1% &
				\ LEP%,W2%=W2%+1% IF W1%>5% &
		! CHECK IF WE FOUND A VALID YEAR AND THEN HANDLE &
		! LEAP YEAR PROPERLY. &

15750	GOTO 15770 IF D%(0%)>D%(W1%/4%+1%) OR D%(0%)<=0% &
	\ F9%=-1% &
	\ FND9%,W2%=(Z3%-70%)*1000%+W2%+D%(0%) &
	\ Z3%=0% &
	\ W1%=0% &
		! IF THE DAY FOR THE MONTH SPECIFIED IS VALID THEN SET &
		! THE FOUND FLAG AND SET THE VALUE OF THE FUNCTION. &

15760	IF P%=S9% OR T%<>0% THEN Z1$=",/:" &
				\ Z3%=1% &
			\ S8%=P% &
			\ WHILE FNR9%(MID(Z1$,Z3%,1%),1%)=0% AND Z3%<=4% &
					\ Z3%=Z3%+1% &
			\ NEXT &
			\ P%=S8% &
			\ IF ((Z3%>4%) AND (P%=S9%)) OR &
					((T%=0%) AND (Z3%=3%)) THEN &
						F9%,FND9%=0% &
		! THIS IS THE SPECIAL TERMINATOR CHECK. &
		! IF WE FOUND THE MINIMUM MONTH, BUT SOMETHING NON &
		! STANDARD FOLLOWED IT, WE FAIL. WE ALSO MAKE THIS CHECK &
		! IF TIME IS POSSIBLE. &

15765	IF T%<>0% AND Z3%=3% THEN &
		GOTO 15770 UNLESS FNR9%(":",1%) &
	\	Z8%=0% &
	\	Z8%=FNR9%("+",1%) &
	\	EXTR.HR%=FNN9% IF Z8% &
	\	W1%=FNR9%("HOURS",1%) IF Z8% &
	\	GOTO 15770 IF LEN(D$)>P% IF Z8% &
	\	EXTR.DAY%=EXTR.DAY%+EXTR.HR%/24% &
	\	EXTR.HR%=EXTR.HR%-24% UNTIL EXTR.HR%<24% &
	\	D$=CVT$$(TIME$(0%),32%) IF Z8% &
	\	P%=0% IF Z8% &
	\	W1%=FNN9% &
	\	IF W1%<0% OR W1%>23% THEN &
			F9%,FND9%=0% &
		ELSE &
			IF FNR9%(":",1%)=0% THEN &
				F9%,FND9%=0% &
			ELSE &
				Z3%=FNN9% &
	\			F9%,FND9%=0% IF W1%<>12% AND FNR9%("M",1%) &
	\			W1%=W1%+12% IF FNR9%("PM",2%) &
	\			W1%=0% IF W1%=24% AND Z3%=0% &
	\			W1%=12% IF W1%=24% &
	\		W1%=0% IF W1%=12% AND Z3%<>0% AND FNR9%("AM",2%) &
	\			F9%,FND9%=0% IF W1%>23% OR Z3%<0% OR Z3%>59% &
	\			IF F9% THEN &
				EXTR.DAY%=EXTR.DAY%+1% IF W1%+EXTR.HR%>23% &
	\				W1%=W1%+EXTR.HR% &
	\				W1%=W1%-24% UNTIL W1%<24% &
	\					W1%=W1%*60%+Z3% &
	&
		! PUT THE TIME INTO W1% IF IT IS VALID.
15770	IF F9%=0% THEN P%=S9% &
			\ E%=512%+53% &
			\ GOTO 15780 &
		! IF ITS AN ERROR, RETURN THE OLD CHAR POINTER, SET THE &
		! ERROR NUMBER AND RETURN. &

15775	D8%=365% &
	\ D8%=D8%+1% IF LEP% &
	\ Z8%=0% &
	\ Z8%=Z8%+1% WHILE EXTR.DAY%-Z8%*D8%>D8% &
	\ W2%=W2%+1000%*Z8% &
	\ EXTR.DAY%=EXTR.DAY%-Z8%*D8% &
	\ D9%=W2%+EXTR.DAY% &
	\ Z8%=D9% &
	\ Z8%=Z8%-1000% UNTIL Z8%<1000% &
	\ D9%=D9%+1000%*(Z8%/D8%)-D8%*(Z8%/D8%) IF Z8%>D8% AND EXTR.DAY% &
	\ D9%=D9%-634% IF D9%=(D9%/1000%)*1000% &
	\ D8%=W1% &
		! ASSIGN THE DATE AND TIME TO THEIR PROPER PLACES. &
		! FUNCTION RETURNS AS DATE. &

15780	D%(2%)=28% &
	\ FNEND &
		! RESTORE THE STACK AND RETURN. &
	&

15800	DEF* FNV%(T2%) &
	\ FNV%=0% &

15810	FOR T1% =0% TO 15% &
	\ IF V%(T1%,0%) =0% THEN GOTO 15850 &
		ELSE &
		IF (V%(T1%,0%) AND 255%) <>255% THEN &
		IF (V%(T1%,0%) AND 255%) <> KB% THEN GOTO 15850 &

15820	IF V%(T1%,1%) = SWAP%(T2%) THEN GOTO 15840 &
		ELSE &
		IF (V%(T1%,1%) AND 255%) <> 255% THEN &
			IF (V%(T1%,1%) AND 255%) <> SWAP%(T2%) AND 255% &
				THEN GOTO 15850 &

15830	IF (V%(T1%,1%) AND -256%) <> -256% THEN &
		IF (V%(T1%,1%) AND -256%) <> (SWAP%(T2%) AND -256%) THEN &
			GOTO 15850 &

15840	FNV% = -1% &
	\ GOTO 15860 &

15850	NEXT T1% &

15860	FNEND &
		! DETERMINE IF THE USER IS A VALID OPERATOR. &
	&
	&
	&

15900	DEF* FNPPN$(PPN%) &
	\ ZZ$ = "[" &
	\ ZZ% = SWAP%(PPN%) AND 255% &
	\ GOSUB 15910 &
	\ ZZ$ = ZZ$ + "," &
	\ ZZ% = PPN% AND 255% &
	\ GOSUB 15910 &
	\ FNPPN$ = ZZ$ + "]" &
	\ FNEND &
		!FUNCTION TO BUILD PPN STRING '[xxx,yyy]' &

15910	ZZ$ = ZZ$ + "*" IF ZZ% = 255% &
	\ ZZ$ = ZZ$ + NUM1$(ZZ%) IF ZZ% <> 255% &
	\ RETURN &
		!SUBROUTINE TO APPEND * OR PPN TO ZZ$ &

16000	! &
	! &
	!		C H E C K    P R I V I L E G E S &
	! &
	! &
	DEF* FNINSTALL.3PP%(Z0%) &
	\ ON ERROR GOTO 16090 &
	\ FNINSTALL.3PP% = 0% &
	\ DUMMY$ = SYS(PRIV.ON$) &
		! INSTALL THIRD PARTY PRIV CHECKING ON ACCOUNT Z0% &
		! NOTE:	(Z0% AND 255%) RETURNS THE PROJECT NUMBER &
		!	(SWAP%(Z0%) AND 255%) RETURNS PROGRAMMER NUMBER &
		! GAIN TEMP PRIVS THE PROGRAM USES (DON'T CHECK USER PRIVS) &

16010	MASK$ = MID(SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(2%) &
		+CVT%$(Z0%)),9%,8%) &
		! GET THE OWNER PRIV MASK FROM DISK &
		! IF ERROR, THEN USE MASK OF NULLS (NO PRIVS) &

16020	DUMMY$ = SYS(CHR$(6%)+CHR$(31%)+STRING$(2%,0%)+CVT%$(Z0%)+MASK$) &
		! NOW, INSTALL THIRD PARTY PRIVS W/ACCT AND THE MASK &

16030	GOTO 16099 &
		! WE'RE DONE; RETURN TO CALLER &

16090	IF	ERL = 16010% &
	THEN	MASK$ = STRING$(8%,0%) &
		\ RESUME 16020 &
		! IF WE HAD ERROR LOOKING UP THE PRIV MASK, USE NULL MASK &

16092	IF	ERR = 32% AND ERL = 16020% &
	THEN	SLEEP 2% &
		\ RESUME 16020 &
		! IF WE'RE HAVING PROBLEMS GETTING A BUFFER, &
		! THEN SLEEP FOR A WHILE AND RETRY THE OPERATION &

16094	ON ERROR GOTO 0 &
		! IN CASE SOMETHING STRANGE HAPPENED ... &

16099	ON ERROR GOTO 19000 &
	\ FNEND &
		! END OF FNINSTALL.3PP% &

16100	DEF* FNDEINSTALL.3PP% &
	\ FNDEINSTALL.3PP% = 0% &
	\ DUMMY$ = SYS(CHR$(6%)+CHR$(31%)) &
	\ DUMMY$ = SYS(PRIV.OFF$) &
	\ FNEND &
		! DEINSTALL THIRD PARTY PRIV CHECKING &
		! DROP THE TEMP PRIVS &

16200	DEF* FNPRIV%(PRIV$) &
	\ CHANGE SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+PRIV$) TO M% &
	\ FNPRIV% = (M%(3%) = 0%) &
	\ FNEND &
	! Check to see if job currently has named privilege &
	! If so then return -1%, else return 0% &

19000	! &
	! &
	! &
	!	E R R O R    H A N D L E R &
	! &

19010	IF ERR=11% AND ERL=10600% THEN C0$="E" &
	\ RESUME 10610 &
		! ^Z TYPED TO COMMAND STRING. &

19020	IF ERL=10550% THEN &
		E%=16% &
	\	RESUME 19090 IF ERR=5% &
	\	SLEEP 15% &
	\	SND.RET%=SND.RET%+1% &
	\	RESUME 10550 IF SND.RET%<45% &
	\	E%=17% &
	\	E$=RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),4%) &
	\	RESUME 19090 &
		! CATCH 'QUEMAN NOT THERE'. &
		! IN CASE THE ADDRESSEE IS SLOW TO RECEIVE. &
		! IF WE  TIME OUT SET ERROR MESSAGE AND GO BACK. &

19030	IF ERL=2040% THEN &
		Z$=SYS(PRIV.OFF$) &
	\	E$=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),4%),4%) &
		+"- "+RIGHT(M$,2%) &
	\	E%=8% &
	\	RESUME 1100 &
		! CATCH FILENAME ERRORS IF YOU CAN. &

19040	IF ERR=2% AND ERL=15100% THEN E%=1% &
	\	E$=F0$ &
	\	RESUME 15140 &
		! CATCH FILENAME ERRORS IN FNF%. &

19050	IF ERR=5% AND (ERL>1030% AND ERL<1070%) THEN &
	Z$=SYS(PRIV.OFF$) &
	\ E%=15% &
	\ E$="" &
	\	RESUME 1100 &
		! IF THE QUEUE FILE OR OPSER WORK FILE ISN'T THERE. &

19070	IF ERR=52% AND ERL=10690% THEN RESUME 10710 &
		! INVALID DEVICE NUMBER IN LIST SPEC. &

19075	IF ERR=5% AND ERL=2017% THEN E%=11% &
	\	E$="["+NUM1$(SWAP%(Y1%(0%,1%)) AND 255%)+","+ &
		NUM1$(Y1%(0%,1%) AND 255%)+ &
		"] Account not on SYSTEM" &
	\	RESUME 1100 &
		! IF THE JOB [P,PN] IS NOT ON THE SYSTEM. &

19080	ON ERROR GOTO 0 &

19090	RETURN &
	&

30000	! &
	! &
	! &
	!	C C L    E N T R Y &
	! &

30010	PRINT &
	\ C$,D$=SYS(CHR$(7%)) &
		! GET STRING FROM CORE COMMON. &

30020	OPEN "_KB:QUE.CMD" FOR INPUT AS FILE 1% &
	\ E0%=E0% OR 1% &
	\ P%=0% &
	\ DEF.OUT$="" &
	\ DEF.OUT$="LP0" IF FNR9%("QUEUE",3%) &
	\ DEF.OUT$="LP0" IF FNR9%("PRINT",3%) UNLESS LEN(DEF.OUT$) &
	\ DEF.OUT$="BA" IF FNR9%("SUBMIT",3%) UNLESS LEN(DEF.OUT$) &
	\ GOTO 31030 UNLESS LEN(DEF.OUT$) &
	\ C$=RIGHT(C$,P%+1%) &
	\ IF ASCII(C$)=47% THEN &
			C$=RIGHT(C$,2%) &
		ELSE	C$="Q"+C$ UNLESS  LEN(C$)=0% &
		! MUST BE FOR US, DEFAULT "Q" CMD. &

30030	GOTO 1010 IF LEN(C$)<>0% &
	\ GOTO 1000 IF (E0% AND 16%)=0% &
	\ E%=1% &
	\ E$="" &
	\ GOTO 1100 &
		! GO PROCESS UNLESS THE COMMAND LENGTH IS 0 AND &
		! WE'RE LOGGED OUT. IF THAT'S THE CASE, THEN WE &
		! HAVE AN ILLEGAL COMMAND. &
	&

31000	! &
	! &
	! &
	!	C H A I N    E N T R Y &
	! &
	! &
   !	CORE COMMON: &
	!	NAME	RETURN PROGRAM NAME &
	!	<CR>	DELIMITER (MUST BE PRESENT) &
	!	CVT$%	RETURN PROGRAM LINE NUMBER &
	!	C$	COMMAND LINE FOR QUE &
	!	<REMAINDER> &
	!		IGNORED; PUT BACK IN COMMON CORE FOR &
	!			RETURN CHAIN. &

31010	E0%=2% &
	\ DEF.OUT$="LP0" &
	\ C$=SYS(CHR$(7%)) &
	\ I0%=INSTR(1%,C$,CHR$(13%)) &
		! STRING IN CORE COMMON, <CR> TERMINATOR. &

31020	IF I0% <> 0% THEN R9$=LEFT(C$,I0%-1%) &
	\ R9%=CVT$%(RIGHT(C$,I0%+1%)) &
	\ C$=RIGHT(C$,I0%+3%) &
	\ I0%=INSTR(1%,C$,CHR$(13%)) &
	\ I0%=LEN(C$)+1% UNLESS I0% <> 0% &
	\ R8$=RIGHT(C$,I0%+1%) &
	\ C$=LEFT(C$,I0%-1%) &
	\ GOTO 1020 &
		! SET RETURN PGM NAME & LINE #, AND REMAINDER TEXT &
		!	AND COMMAND STRING. &

31030	E%=1% &
	\ E$=C$ &
	\ GOTO 1100 &
		! SET ENTRY AND GO PROCESS. &

31100	! &
	! &
	! &
	!	EXIT: FROM CHAIN ENTRY. &
	! &

31110	CLOSE 1%,2% &
	\ Z1$=SYS(PRIV.ON$) &
	\ R8$ = Z$ + CHR$(13%) + R8$	IF E% <> 0% &
	\ C$=SYS(CHR$(8%)+CHR$(E%)+R8$) &
	\ CHAIN R9$ LINE R9% &
		! RETURN FROM CHAIN ENTRY, &
	&

32000	! &
	! &
	! &
	!	L O G G E D - O U T    E N T R Y &
	! &

32010	E0%=16% &
	\ C$=SYS(CHR$(7%)) &
	\ DEF.OUT$="LP0" &
	\ IF LEN(C$) <> 0% THEN &
		C$,D$="QUEUE"+C$ &
	\	GOTO 30020 &

32760 	CLOSE 1%,2% &
	\ IF (E0% AND 16%) <> 0% THEN PRINT &
	\	PRINT &

32767	END
