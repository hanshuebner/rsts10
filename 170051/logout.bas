2!		PROGRAM		: LOGOUT.BAS
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
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! &
	! 9.0-13	19-MAR-85	Added qualifier validation &
	! &
	! V9.5-05	21-Aug-87	(JJT) Add CR/LF to end of KB0: message &
	! V9.5-08	7-Oct-87	(DRW) Fix device > 9 with disk quota &
	!				errors on logout. &
	! V9.6-07	22-Feb-88	(REG) Add LAT/SERVER/PORT info to &
	!				OPSER message. &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	&
	!	THIS ROUTINE: &
	!		CLOSES ALL OPEN CHANNELS; &
	!		DEASSIGNS ALL DEVICES; &
	!		UPDATES STATISTICS ON DISK; &
	!		CLEARS THE JOB FROM MONITOR MESSAGE TABLE; &
	!		DISASSOCIATES THE PROJECT,PROGRAMMER NUMBER FROM &
	!			THE JOB NUMBER. &
	! &
	!	IT IS INVOKED BY THE USER TYPING "BYE" AT A LOGGED-IN &
	!	TERMINAL. The command "BYE/F" results in a fast (one-line) &
	!	logout.  All others result in the longer logout message. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !	&
   !	(none) &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !	&
   !	A$	FILENAME AND EXT FOR INDIVIDUAL FILE DELETION &
   !	I%	ACTIVE JOB COUNTER; ELAPSED TIME STORAGE &
   !	I0%	ELAPSED TIME CALCULATION &
   !	J%	USER'S JOB # &
   !	L$	READS CORE COMMON; ACCEPTS KB INPUT &
   !	L9%	INDICATOR FOR NORMAL(0%) OR FAST(-1%) LOGOUT &
   !	M()	ACCOUNTING INFORMATION STORED HERE &
   !	N()	DIRECTORY LOOKUP INFORMATION STORED HERE &
   !	Q	CALCULATES DISK QUOTA &
   !	T.0	 HOLDS TIME IN SECONDS &
   !	T.2	JOB CONNECT TIME IN MINUTES &
   !	T	CALCULATES BLOCKS OWNED &
   !	T.1	JOB CPU TIME &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !	&
   !	LINE 10010		DISPLAYS TIME: HOURS, MINUTES, SECONDS. &
	&
   !	LINE 10200		DISPLAYS ELAPSED TIME: HOURS, MINUTES. &
	&
   !	SYS(6,4)		LOG-IN &
	&
   !	SYS(6,5)		LOG-OUT &
	&
   !	SYS(6,9,0)		SYSTEM HEADER LINE &
	&
   !	SYS(6,14)		USER'S ACCOUNTING DATA &
	&
   !	SYS(6,15)		DIRECTORY LOOK-UP &
	&
   !	SYS(7)			READS CORE COMMON &
	&
   !	SYS(9)			EXIT AND CLEAR PROGRAM FROM MEMORY &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

910	DIM M%(30) &
		! ACCOUNTING INFO READ INTO THIS STRING. &

920	DIM N%(30) &
		! WORK AREA TO PROCESS DIRECTORY LOOKUP. &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ PRINT IF CCPOS(0%) &
	! SET UP STANDARD ERROR TRAP; RETURN KB TO LEFT MARGIN. &

1010	L$=CVT$$(SYS(CHR$(7%)),-2%) &
	\ IF LEFT(L$,6%)="LOGOUT" THEN &
		L$=RIGHT(L$,7%) &
	\	GOTO 1040 &

1020	IF LEFT(L$,3%)="BYE" THEN &
		L$=RIGHT(L$,4%) &
	\	GOTO 1040 &

1040	L$=RIGHT(L$,2%) IF ASCII(L$)=47% &
	\ L9%= (ASCII(L$) = 70%) &
	\ IF LEN(L$)>1% OR (LEN(L$) AND ASCII(L$)<>89% AND ASCII(L$)<>70%) &
	    THEN &
		PRINT "?Invalid qualifier" &
	\	GOTO 32767 &
	! &
	! Remove the "/" if one was present &
	! Set the flag for fast or full logout &
	! If qualifier is more than 1 character or is not a "Y" or "F" &
	!    then tell the use Invalid qualifier and &
	!    exit &

1050	ON ERROR GOTO 19000 &
	! RE-SET STANDARD ERROR TRAP. &

1070	GOSUB 22500 &
	\ CHANGE SYS(CHR.6$+CHR$(-10%)+ &
	"????"+RIGHT(NUM1$(100%+JOB%),2%)+".TMP") TO M% &
	\ M%(1%)=6% &
	\ M%(2%)=33% &
	\ M%(3%)=4% &
	\ M%(I%)=0% FOR I%=4% TO 6% &
	\ M%(I%)=0% FOR I%=13% TO 30% &
	\ CHANGE M% TO A$ &
	! GET THE USER'S JOB # &
	! SET UP TO DELETE THE TEMP FILE: "TEMPNN.TMP" (WHERE NN IS THE &
	! USER'S JOB NUMBER) FROM THE USER'S FILE DATA AS WELL AS ALL &
	! OTHER "????NN.TMP" FILES. &

1075	CHANGE SYS(A$) TO M% &
	\ KILL RAD$(M%(7%)+SWAP%(M%(8%)))+RAD$(M%(9%)+SWAP%(M%(10%)))+ &
	"."+RAD$(M%(11%)+SWAP%(M%(12%))) &
	\ GOTO 1075 &
	! PLOW THROUGH USER'S "????NN.TMP" FILES, KILLING EACH. &

1080	M%(I%) = 0% FOR I% = 1% TO 30% &
	\ M%(0%)=30% &
	\ M%(1%)=6% &
	\ M%(2%)=14% &
	\ M%(9%)=4% &
	\ CHANGE M% TO A$ &
	\ CHANGE SYS(A$) TO M% &
	! READS THE USER'S ACCOUNTING DATA INTO ARRAY M. &

1090	S.Q=(65536.*M%(14%))+(256.*M%(10%)+M%(9%)) &
	\ S.T=(65536.*M%(16%))+(256.*M%(20%)+M%(19%)) &
	! GET DISK QUOTA, Q. AND DISK USED, T. VALUES &
	!  THESE VALUES ARE FOR SY: &

1410	CHANGE SYS(CHR.6$+CHR$(4%)) TO N% &
	\ USERS%=N%(3%)-1		! COUNT THE JOBS LEFT &
	\ T.0=TIME(0%) &
	\ T.1=TIME(1%) &
	\ T.2=TIME(2%) &
	! DOES A LOGIN FOR THIS USER TO COUNT ALREADY LOGGED-IN JOBS &
	! T.0: SECONDS SINCE MIDNIGHT; T.1: JOB CPU TIME; &
	! T.2: JOB CONNECT TIME IN MINUTES. &

1420	!     	CHECK FOR DISK STORAGE AND DETACHED JOB QUOTAS &
	! &
	! &
	I$=SYS(CHR.6$+CHR$(5%)+CHR$(1%))		! KEEP DEVICES ETC... &
	\ QUO%=SWAP%(CVT$%(MID(I$,3%,2%)))		! QUO% IS RETURNED &
							! IN THE FQERNO &
							!    0%=SUCCESS, &
							!   -1%=FATAL ERROR, &
							!   -2%=WARNING ERROR &
	\ GOTO 1500 IF QUO%=0%				! CONT'UE IF NO ERROR &
	\ FQSIZE%=SWAP%(CVT$%(MID(I$,13%,2%))) 		! VALUES FOR FQSIZE%: &
							!   0%=DISK ERROR &
							!   1%=DETACHED ERROR &
	\ PLURAL$="" &
	\ A.SOME$=" a " &
	! &
	! &
	!	DETACHED ERROR &
	! &
	\ IF FQSIZE%=1% THEN DJ%=ASCII(MID(I$,17%,1%)) 	! DET'D JOBS IN ACC'T &
		\ DT%=ASCII(MID(I$,18%,1%))		! DET'D TOTALS ALLO'D &
		\ DJ%=DJ%-DT% &
		\ A.SOME$=" some " IF DJ%>1% &
		\ PLURAL$="s" IF DJ%>1% &
		\ PRINT FNC$;CRLF$;"?Detached-job quota of";DT%;"exceeded by" &
			;DJ%;"job";PLURAL$;".";FNC$;"?Please ATTACH to"; &
			A.SOME$;"job";PLURAL$;" in this account." &
		\ GOTO 19030 			! FATAL SO STAY LOGGED IN &

1425 	! &
	!	DISK ERRORS &
	! &
	DV$=MID(I$,23%,2%)				! GET THE DEVICE NAME &
	\ DV$=DV$+NUM1$(ASCII(MID(I$,25%,1%)))		!      ADD THE NUMBER &
		IF ASCII(MID(I$,26%,1%)) &
	\ DV$=DV$+":" 					!      ADD THE ":" &
	\ T=(65536.*ASCII(MID(I$,16%,1%)))+		! TOTAL DISK USAGE &
		(256.*ASCII(MID(I$,22%,1%))+ &
		 ASCII(MID(I$,21%,1%))) &
	\ Q=(65536.*ASCII(MID(I$,15%,1%)))+ 		! TOTAL DISK QUOTA &
		(256.*ASCII(MID(I$,20%,1%))+ &
		 ASCII(MID(I$,19%,1%))) &
	\ T=S.T IF LEFT(DV$,2%)="SY"			! USE VALUES ALREADY &
	\ Q=S.Q IF LEFT(DV$,2%)="SY"			!	FOUND FOR SY: &
	\ PLURAL$="s" IF T-Q>1. &
	! &
	! &
	!	NON-FATAL DISK ERROR &
	! &
	\ IF QUO%=-2% THEN PRINT FNC$;CRLF$; &
		"%Disk-storage quota of";Q;"on ";DV$;" exceeded by";T-Q; &
		"allocated block";PLURAL$;".";CRLF$ &
		\ GOTO 1500				! LET 'EM LOGOUT &

1430	! &
	!	FATAL DISK ERROR &
	! &
	A.SOME$=" some " IF T-Q>1. &
	\ PRINT FNC$;CRLF$;"?Disk-storage quota of";Q; &
		"on ";DV$;" exceeded by";T-Q;"allocated block";PLURAL$;"."; &
		FNC$;"?Please DELETE";A.SOME$;"file";PLURAL$;" on ";DV$;"." &
	\ GOTO 19030 			! FATAL SO STAY LOGGED IN &

1500	A$=NUM1$(M%(8%))+","+NUM1$(M%(7%)) &
	\ T0%=FNS%("LOGOUT terminating ["+A$+"]") IF SEND.OPSER% &
	\ DV$=SYS(PRIV.ON$) &
	\ IF L9% THEN &
		GOTO 9020 &
	ELSE &
		PRINT "Saved all disk files on SY:";NUM$(S.T);"block"; &
	\	PRINT "s"; UNLESS S.T=1. &
	\	PRINT " in use"; &
	\	PRINT ", ";NUM1$(S.Q-S.T);" free"; IF S.Q-S.T>=0. &
			        AND (S.Q<>0. AND S.Q<>16777215.) &
	\	PRINT &
	\	PRINT "Job ";NUM1$(JOB%);" User ";A$;" logged off KB"; &
			NUM1$(KB%);": at ";DATE$(0%);" "+TIME$(0%) &
	\	IF USERS%<>0% THEN &
			PRINT NUM1$(USERS%);" other user"; &
	\		PRINT "s"; UNLESS USERS%=1% &
	\		PRINT " still logged in under this account" &
	! SET UP PPN STRING WITH BRACKETS. &
	! TELL OPSER WHO IS LOGGING OFF. &
	! TURN ON PRIVILEGES. &
	! IF FAST LOGOUT THEN &
	!	GOTO 9020 &
	! ELSE &
	!	PRINT THE LONG LOGOUT MESSAGE. &
	! 	IF THERE ARE OTHER USERS IN THIS ACCOUNT THEN &
	! 		TELL THEM HOW MANY. &

1580	PRINT "System ";CVT$$(RIGHT(SYS(CHR.6$+CHR$(9%)),3%),4%) &
	\ PRINT "Run time was "; &
	\ GOSUB 10010 &
	\ PRINT "Elapsed time was "; &
	\ GOSUB 10200 &
	\ PRINT "Good "; &
	\ IF T.0<43200 THEN PRINT "morning" &
		ELSE	IF T.0<64800 THEN PRINT "afternoon" &
		ELSE	PRINT "evening" &
	! PRINT THE SYSTEM HEADER LINE. &
	! T = CPU TIME IN .1 SECS. &
	! T.2 = CONNECT TIME IN MINUTES. &

9020	GOTO 9030 IF (AKSES% AND 2%) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT FOR I%=1% TO 5% &
	\ FOR I%=1% TO 20% &
	\	GOTO 9030 IF (PEEK(KB.DDB%+10%)=PEEK(KB.DDB%+12%)) &
	\	SLEEP 2% &
	\ NEXT I% &
	! SKIP ALL OF THIS IF THIS IS A BATCH JOB &
	! INSURE WE ARE AT THE LEFT MARGIN &
	! PRINT FIVE BLANK LINES &
	! LOOP TWENTY TIMES &
	! EXIT THE LOOP IF EVERYTHING HAS BEEN PRINTED &
	! SLEEP A LITTLE WHILE &
	! DO AGAIN UNTIL DONE &

9030	CLOSE 1% &
	\ L$=SYS(CHR$(6%)+CHR$(-9%)+CHR$(KB%)+CHR$(2%)) &
	! CLOSE FILE CHANNEL #1. &
	!##WAS OPTIONAL PATCH, NOW STANDARD &
	!-- HANG UP THE DATASET ON LOGOUT &

9040	M%(0%)=30% &
	\ M%(1%)=6% &
	\ M%(2%)=8% &
	\ M%(3%)=JOB% &
	\ M%(27%)=0% &
	\ M%(28%)=255% &
	\ CHANGE M% TO A$ &
	\ A$=SYS(A$) &
	! KILL SELF &

9050	GOTO 32767 &
	&

10000	! &
	&
	&
	!	S U B R O U T I N E &
	&
	&

10010	I%=T.1/36000. &
	! CALCULATES # OF HOURS. &

10020	IF I%<>0% THEN PRINT NUM1$(I%);" hour"; &
	\ IF I%=1% THEN PRINT ", "; ELSE &
			PRINT "s, "; &

10050	T.1=T.1-36000.*I% &
	\ I0%=T.1/600. &
	! CALCULATES # OF MINUTES. &

10070	IF I%<>0% OR I0%<>0% THEN PRINT NUM1$(I0%);" minute"; &
	\ IF I0%=1% THEN PRINT ", "; ELSE &
			PRINT "s, "; &

10100	T.1=INT(T.1-600.*I0%) &
	\ PRINT NUM1$(T.1/10);" second"; &
	\ IF T.1/10==1 THEN PRINT ELSE &
			PRINT "s" &
	! CALCULATES # OF SECONDS. &

10130	RETURN &

10200	I%=T.2/60. &
	\ IF I%<>0% THEN &
		PRINT NUM1$(I%);" hour"; &
	\	IF I%=1% THEN &
			PRINT ", "; &
		ELSE	PRINT "s, "; &

10210	I%=T.2-60.*I% &
	\ PRINT NUM1$(I%);" minute"; &
	\ IF I%=1% THEN &
		PRINT &
	  ELSE	PRINT "s" &

10220	RETURN &
	&

19000	! &
	&
	&
	!	E R R O R    H A N D L I N G &
	&
	&

19005	RESUME 19010 &

19010	IF ERR=27% THEN CLOSE 1% &
	\ M$=SYS(CHR$(9%)) &
	\ GOTO 32767 &
	! DETACHED KB, SO EXIT AND CLEAR PROGRAM FROM MEMORY. &

19020	IF ERL=1075% THEN GOTO 1080 &

19028	GOTO 19035 IF ERR = 11% &
\	PRINT CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%); &
		" at line"; ERL &

19030	PRINT CHR$(7%); CHR$(7%); CHR$(7%); &

19035	CLOSE 1% &

19040	PRINT &
	\ PRINT &

19050	L$=SYS(CHR$(9%)) &
	! EXIT AND CLEAR PROGRAM FROM MEMORY. &

19060	GOTO 32767 &
	&
	&

22500	! &
	!	COMMON ROUTINE TO ALL ENTRY POINTS. &
	!	SET UP CONSTANTS AND INITIAL VALUES. &
	!	DROP TEMPORARY PRIVILEGES. &
	! &
	CHR.6$=CHR$(6%) &
	\ CHANGE SYS(CHR.6$+CHR$(-3%)) TO M% &
	\ JOBTBL%=M%(11%)+SWAP%(M%(12%)) &
	\ JOB%=(M%(1%)/2%) &
	\ AKSES%=ASCII(MID(SYS(CHR$(6%)+CHR$(26%)+CHR$(JOB%)+CHR$(2%)),15%,1%)) &
	\ I%=PEEK(520%) &
	\ PPNPTR%=PEEK(I%+8%)+24% &
	\ PPN%,A%=PEEK(PPNPTR%) &
	\ KB.DDB%=PEEK(PEEK(I%)) &
	\ KB%=SWAP%(PEEK(KB.DDB%+2%)) AND 255% &
	\ TTINTF%=PEEK(KB.DDB%+30%) &
	\ LAT.SERVER.PORT$=FNLAT.SERVER.PORT$(KB%) &
	\ ON ERROR GOTO 19000 &
	\ SEND.OPSER%=((TTINTF% AND 16384%)<>0%) &
		OR (LEN(LAT.SERVER.PORT$)<>0%) &
	\ MAXCNT%=M%(4%) &
	\ PRIV.OFF$=CHR.6$+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR.6$+CHR$(-21%)+CHR$(0%) &
	\ CRLF$=CHR$(13%)+CHR$(10%) &

22510	RETURN &
	!	RETURNS: &
	!	JOB%		OUR JOB NUMBER. &
	!	PPNPTR%		PEEK(PPNPTR%) WILL ALWAYS RETURN CURRENT PPN. &
	!	PPN%		CURRENT PPN. &
	!	KB.DDB%		OUR CONSOLE KEYBOARD'S DDB LOCATION. &
	!	TTINTF%		OUR CONSOLE KEYBOARD'S INTERFACE TYPE. &
	!	SEND.OPSER%	WHETHER WE SHOULD LOG EVERYTHING TO OPSER. &
	!	JOBTBL%		LOCATION OF THE SYSTEM JOB TABLE. &
	!	CHR.6$		CHR$(6%) &
	!	PRIV.OFF$	SYS(PRIV.OFF$) TEMPORARILY DROPS PRIV'S. &
	!	PRIV.ON$	SYS(PRIV.ON$) REGAINS DROPPED PRIV'S. &

22550	! &
	!	L A T   S E R V E R   P O R T   N A M E / I D &
	! &
	! NOTE: A call to this sys call must be followed immediately by &
	!	an ON ERROR GOTO statement because this call sets the error &
	!	trap for its own purposes. &
	! &
	DEF* FNLAT.SERVER.PORT$(KEYBOARD.NO%) &
	\ FNLAT.SERVER.PORT$="" &
		! CLEAR IT RIGHT OUT &
	\ OPEN "_NL:" AS FILE 11%, RECORDSIZE 54% &
		! GET A BUFFER &
	\ FIELD #11%, 1% AS LAT.KB$, 1% AS PORT.BIT$, 1% AS PORT.LEN$, &
		50% AS LAT.BUFF$ &
		! LAT KB no., Port bit mask, Port ID len, Rest of the buffer &
	\ ON ERROR GOTO 22570 &
		! SET LOCAL ERROR TRAP &
	\ V$ = SYS(CHR$(6%)+CHR$(22%)+CHR$(12%)+ &
		CHR$(6%)+STRING$(2%,0%)+CHR$(KEYBOARD.NO%)+ &
		STRING$(3%,0%)+CHR$(11%)) &
		! GET THE LAT SERVER PORT ID FOR THIS KB &
	\ PORT.LEN%=ASCII(PORT.LEN$) &
		! TURN THE PORT LENGTH INTO AN INTEGER &
	\ FNLAT.SERVER.PORT$="LAT Server/Port: " + &
		MID(LAT.BUFF$,PORT.LEN%+2%, &
			ASCII(RIGHT(LAT.BUFF$,PORT.LEN%+1%))) + &
		"/"+LEFT(LAT.BUFF$,PORT.LEN%) &
		! GET THE LAT HEADER AND THE LAT SERVER PORT AND ID &

22560	CLOSE 11% &
		! CLOSE THE NULL DEVICE &
	\ FNEND &
		! GO BACK TO WHAT WE WERE DOING &

22570	RESUME 22560 &
		! JUST EXIT ON ANY ERROR &

22600	! &
	!	SEND A MESSAGE TO OPSER, OR TO KB0: IF OPSER NOT FOUND. &
	! &
	DEF* FNS%(I$) &
	\ DUMB% = -1%		! SET DUMB% = 0% TO SEND TO OPSER &
	\ I$=I$+" at" UNLESS (TTINTF% AND 16384%) &
	\ I$=I$+" on dial-up line" IF (TTINTF% AND 16384%) &
	\ I$=I$+" KB"+NUM1$(KB%)+":" &
	\ I$=I$+CHR$(13%)+CHR$(10%)+STRING$(2%,9%)+LAT.SERVER.PORT$ &
		IF LEN(LAT.SERVER.PORT$) &
	\ T1$ = CHR.6$+CHR$(22%) &
	\ IF DUMB% THEN &
		T1$=T1$+CHR$(-11%)+CHR$(139%)+STRING$(24%,0%)+CHR$(2%) &
			+STRING$(11%,0%)+CHR$(4%)+CHR$(6%)+"LOGOUT" &
			+CHR$(3%)+CHR$(0%)+CHR$(1%)+CHR$(LEN(I$)+1%)+CHR$(9%) &
	  ELSE	T1$=T1$+CHR$(-1%)+CHR$(0%)+"OPSER "+STRING$(10%,0%) &
		! ADD KEYBOARD NUMBER TO MESSAGE. &
		! SET UP STRING TO BE USED IN THE SEND MESSAGE SYS CALL. &

22610	ON ERROR GOTO 22630 &
	\ T0$=SYS(PRIV.ON$) &
	\ IF DUMB% THEN &
		T0$=SYS(T1$+I$) &
	  ELSE	IF LEN(I$)<=19% THEN &
			T0$=SYS(T1$+CHR$(LEN(I$)+1%)+I$) &
		ELSE	T0$=SYS(T1$+CHR$(255%)+LEFT(I$,19%)) &
		\	I$=RIGHT(I$,20%) &
		\	GOTO 22610 &
		! IF THE MESSAGE REMAINING TO BE SENT IS 19 BYTES OR UNDER, &
		! SEND IT TO OPSER WITH ITS LENGTH. &
		! IF IT IS LONGER THAN 19 BYTES, SEND 19 BYTES, FLAG THAT &
		! THERE IS MORE COMMING, AND BACK AND SEND MORE. &

22620	T0$=SYS(PRIV.OFF$) &
	\ GOTO 22690 &
		! DROP THE TEMPORARY PRIVILEGES AND EXIT. &

22630	IF ERR<>32% AND ERL=22610% THEN &
		RESUME 22640 &
	ELSE	PRINT FNC$; CVT$$(RIGHT(SYS(CHR.6$+CHR$(9%)+CHR$(ERR)),3%),4%) &
	\	RESUME 22690 &
		! IF 'NO SMALL BUFFERS', WE PRINT THE ERROR MESSAGE AND ABORT. &
		! IF ANY OTHER ERROR ON SEND, WE WILL BROADCAST TO KB0:. &

22640	ON ERROR GOTO 19000 &
	\ DUMB% = 0% &
	\ PPN%=PEEK(PPNPTR%) &
	\ I$=CRLF$+"MESSAGE"+STRING$(3%,9%)+": "+DATE$(0%)+" " &
		+TIME$(0%)+"  JOB:"+NUM1$(JOB%)+"  KB"+NUM1$(KB%) &
		+":  LOGOUT ["+NUM1$(SWAP%(PPN%) AND 255%)+"," &
		+NUM1$(PPN% AND 255%)+"]"+CRLF$+CHR$(9%)+I$+CRLF$ &
		! INITIALIZE RETRY COUNTER &
		! BUILD STRING TO BROADCAST TO KB0:. &

22650	T0$=SYS(PRIV.ON$) &
	\ T0$=SYS(CHR.6$+CHR$(-5%)+CHR$(0%)+I$) &
	\ DUMB%=DUMB%+1% &
	\ GOTO 22690 IF 30%<= DUMB% &
	\ IF RECOUNT THEN &
		I$=RIGHT(I$,LEN(I$)-RECOUNT+1%) &
	\	GOTO 22690 IF ASCII(MID(SYS(CHR.6$+CHR$(16%)),28%,1%))=255% &
	\	SLEEP 1% &
	\	GOTO 22650 &
		! BROADCAST THE MESSAGE TO KB0:. &
		! IF RECOUNT<>0%, NOT ALL CHARACTERS WERE BROADCAST, &
		!	SO GO BACK AND BROADCAST THE REMAINING ONES. &

22690	ON ERROR GOTO 19000 &
	\ T0$=SYS(PRIV.OFF$) &
	\ FNEND &
		! END OF FNS%(I$). &

25000	DEF* FNC$ &
	\ FNC$="" &
	\ FNC$=CRLF$ IF CCPOS(0%) &
	\ FNEND &
	! FNC$ RESTORES CARRIAGE IF NEEDED &

32767	END
