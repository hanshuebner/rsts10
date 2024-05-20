2!		PROGRAM		: LOGIN.BAS
5!		VERSION		: V10.1
6!		EDIT		: I
7!		EDIT DATE	: 31-JAN-92
10	EXTEND
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1974, 1992 by &
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
	! VER-ED	EDIT DATE	Who	REASON &
	! 9.2-04	06-JAN-86	DLS	Use the first 13 bytes of &
	!					error 0 for the header and &
	!					replace peeks at jdb2 with &
	!					data returned from uu.sys &
	! &
	! 9.2-06	14-FEB-86	DLS	Add check of core common at &
	!					line 200 for UTLMGR entry &
	!					with LOGIN/TERM	command &
	! &
	! 9.3-04	27-MAY-86	PRL	Fix header line display &
	!					to work on Micro/RSTS &
	! &
	! 9.5-04	03-AUG-87	MRK	Prompt warning messages &
	!					at the start of a login &
	!					sequence and before the &
	!					standard NOTICE.TXT &
	!					message &
	!  9.6		12-Aug-87	JJT	Move above file to &
	!					[0,1]LOGIN.TXT and remove &
	!					message before NOTICE.TXT &
	!  9.6		24-Aug-87	JJT	Allow non-priv users to attach &
	!					to jobs in other accounts &
	!  9.6		06-Oct-87	JJT	Allow attach to job with &
	!					number higher than JOBMAX &
	!					Fix 2-char unit number display &
	!  9.6		19-Feb-88	JJT	Always set DTR on &
	!  9.6		22-Feb-88	REG	Add LAT/SERVER/PORT info to &
	!					OPSER message. &
	!  9.6		15-Mar-88	JJT	Make local LAT line work, too &
	!  9.6-10	27-APR-88	JJT	Send LAT info to LOGIN.COM &
	!  9.7-03	07-Dec-88	FEK	Call EMS on ^E startup &
	!  9.7-06	19-Jan-89	DLS	Remove new mail message check &
	!					and associated code &
	!  10.0-I	22-Jan-90	FEK	Add non-interactive date &
	!  10.0-I	24-Jan-90	JJT	Add name logicals, and pass a &
	!					bit mask to LOGIN.COM &
	!  10.0-K	17-May-90	JJT	Switch bits sent to LOGIN.COM &
	!  10.1-C	19-Jul-91	JJT	Enable privs before gosub 13000 &
	!  10.1-C	19-Jul-91	JJT	Don't log out if invalid acct &
	!  10.1-H	05-Dec-91	JJT	Don't display det jobs if /TER &

100	!	ENTRY POINT IF LOGGED IN &
	! &
	DIM M%(30%),W%(30%), M2%(30%), JOB.CLASS$(7%), S1%(30%), S2%(30%) &

150	ON ERROR GOTO 32400 &
	\ KB.SPAWNED%=0% 		! NOT SPAWNED FROM A TERMINAL &
					! WITH "LOGIN/TERM" &

200	LOGIN$= SYS(CHR$(7%)) &
	\ LOGGED.IN%=-1% &
	\ GOSUB 22500 ! COMMON ROUTINE FOR ALL ENTRIES TO SET THINGS UP &
	\ KB.SPAWNED%=-1% IF LEFT(LOGIN$,8%) = "*UTLMGR*" ! LOGIN/TER COMMAND &
	\ SPAWNED$=RIGHT(LOGIN$,9%) IF KB.SPAWNED%  ! DATE TIME AND KB# &
	\ IF SEND.OPSER% &
	  	THEN IF A% THEN &
			T0$=LOGIN$ &
	\		T0%=INSTR(1%,T0$,";") &
	\		T0$=LEFT(T0$,T0%) IF T0% &
	\		T0%=FNS%("LOGIN invoked logged-in ("+T0$+")") &

205	I$=SYS(CHR$(0%))+SYS(CHR$(2%)) &
	\ OPEN "_KB:LOGIN.CMD" FOR INPUT AS FILE 1% &
	\ LOGIN$=CVT$$(SYS(CHR$(7%)),444%) &
	\ LOGIN$=CVT$$(RIGHT(LOGIN$,FNC%(LOGIN$,"HELLO",5%)),8%) &
	\ LOGIN$=CVT$$(RIGHT(LOGIN$,FNC%(LOGIN$,"LOGIN",3%)),8%) &
	\ GOSUB 16000 IF A% &
	! DELETE TEMP FILES &

207	LOGIN$="" IF LOGIN$=CVT$$(SYS(CHR$(7%)),444%) &
	! PREVENT UNWANTED LOGOUT WHEN TYPING "RUN $LOGIN" &

210	IF A% THEN A$=SYS(PRIV.ON$)	! PRIVS ON SO WE GET LAST LOGIN DATE &
	\	GOSUB 13000 		! VERIFY THE PPN &
	\	A$=SYS(PRIV.OFF$)	! PRIVS OFF AGAIN &
	\	DO.COMFILE%=-1%		! FLAG TO SET LOGIN.COM PARAMETERS &
	\	IF I% THEN GOTO 5000	! NO PPN TYPED OR PPN IS CURRENT PPN &
		ELSE GOSUB 9000 	! QUOTA CHECK FOR DISK STORAGE AND &
					! DETACHED JOBS &
		\ IF QUO%=-1% &
		THEN	GOTO 32650	! FATAL, OVER-QUOTA, exit &
		ELSE	GOTO 2000	! NON-FATAL,CONTINUE, GO LOG BACK IN &
	&
	&

500	!	START HERE  IF "HELLO" OR "LOGIN" AND NOT LOGGED IN &
	! &
	T%=0% &
	\ KB.SPAWNED%=0% 		! NOT SPAWNED FROM A TERMINAL &
	\ IF LEN(LOGIN$) THEN 		! WITH "LOGIN/TERM" &
		GOSUB 13000		! VERIFY THE PPN &
	\	IF I% THEN 900 		! PPN PROBLEMS &
		ELSE	GOSUB 14000	! CHECK FOR SYSTEM PASSWORD &
	\		GOTO 900 IF I%	! SKIP IF BAD SYSTEM PASSWORD &
	\		DO.COMFILE%=-1% &
	\		GOTO 2000	! PROMPT FOR PASSWORD &

510	GOSUB 10000 ! TYPE SYSTEM HEADER &
	\ GOTO 1000 &

800	I%=-3% &
	\ A%=0% &
	\ GOTO 900 UNLESS M%(4%)	! Reject if no detached jobs &
	\ PRINT FNC$;"?Job quota exceeded - you must attach to some job" &
	\ DET.QUO%=-1%			! Indicate detached quota error &
	\ GOTO 2100			! Now display and ask &

900	T%=T%+1% &
	\ A%=0% &
	\ T0%=FNS%("LOGIN attempt to "+LOGIN$+" failed - count "+NUM1$(T%)) &
			IF SEND.OPSER% &
	\ IF T%>=5% THEN &
		PRINT FNC$;"??Access denied" &
	\	GOTO 32650 &

910	PRINT FNC$; &
	\ IF I%=-3% &
		THEN &
		  PRINT "?Job quota exceeded" &
		ELSE &
		  IF I%=-2% &
			THEN &
			  PRINT "?Access not permitted" &
			ELSE &
			  I$=SYS(PRIV.ON$) &
	\		  J%=(PEEK(516%) AND 255%) &
	\		  DELAY%=(T%-1%)*5% &
	\		  DELAY%=50% IF DELAY%>50% &
	\		  HOLD%=-1% &
	\		  WHILE HOLD% AND (DELAY%>0%) &
	\			SLEEP 1% &
	\			DELTA%=J%-(PEEK(516%) AND 255%) &
	\			HOLD%=((DELTA%>=0% AND DELTA%<DELAY%) &
				  OR (DELTA%<0%  AND (DELTA%+60%)<DELAY%)) &
	\		  NEXT &
	\		  I$=SYS(PRIV.OFF$) &
	\		  PRINT "?Invalid entry - try again" &

950	ATTACHING%,DET.QUO%,L.PROG%,L.PROJ%=0% 	! CLEAN HOUSE &

1000	GOSUB 14000		! Check for system password required &
	\ T%=0% IF DO.COMFILE%=0% AND SYS.PW.OK%  ! Only set it this way if &
						  ! this is the first time &
						  ! after a good sys pswrd &
	\ GOTO 900 IF I%	! Issue error message if no good &
	\ DO.COMFILE%=-1% &
	\ GOSUB 11000 		! DO "User:" PROMPT &
	\ A$=SYS(PRIV.ON$)	! PRIVS ON SO WE GET LAST LOGIN DATE &
	\ GOSUB 13000 		! VERIFY PPN &
	\ A$=SYS(PRIV.OFF$)	! PRIVS OFF AGAIN &
	\ IF I% THEN GOTO 900 	! PROBLEM, SO GO LOG THE TRY AND SEE IF &
				! 	WE SHOULD TRY AGAIN &

2000	PASSWORD$=""		! Try for no-password-prompt case first &
	\ GOSUB 15000		! Check it out &
	\ IF I%=-1% THEN 	! If password required... &
		GOSUB 12000 	! GET PASSWORD AND AUX PASSWORD (DOES NOT &
				! 	CHECK THEIR VALIDITY) &
	\	GOSUB 15000 	! CHECK THE VALIDITY OF THE PASSWORD &

2010	GOTO 900 IF I% 			! ERROR, GO TRY AGAIN &
	\ GOTO 800 IF M%(2%)<>255%	! QUOTA PROBLEM &
	&

2100	GOSUB 17100 		! INDICATE IF THERE ARE ANY DETACHED JOBS &
	\ GOTO 5010 &
	&
	&

3000	!	Handle incoming ^E - Answerback sent, message waiting &
	!		so lets go load it into a file &
	! &
	TIMEOUT%=-2%			! hangup quick if we fail &
	\ GOTO 32600 UNLESS EMS%	! SKIP IT IF NOT AN EMS TERMINAL &
	\ on error goto 3400 &
	\ i$=sys(chr$(3%)+chr$(1%))	! enter noecho mode &
	\ i%=1440%-peek(514%)		! minutes from midnight &
	\ a$="EMS$:"+FNZ$(i%,4%) &
	\ i%=60%-(peek(516%) and 255%)	! seconds from minute &
	\ a$=a$+fnz$(i%,2%)+"." &
	\ i%=0%				! start with file mmmmss.001 &

3010	i%=i%+1%			! try the next one &
	\ goto 3500 if i%>=1000%	! can't go that high &
	\ i$=a$+fnz$(i%,3%) &
	\ open I$ for output as file 2%, mode 128% &
	\ T0%=FNS%("EMS Received into ("+I$+")") IF send.opser% &
	\ on error goto 3400 &
	\ a$="***************************************************"+CRLF$ &
	\ print #2%, a$;"Message received at: ";date$(0%);"  ";time$(0%); &
			CRLF$;a$ &
	\ a0$="ended" &
	\ wait 120%		! Give them 2 minutes per line &

3100	input line #1%, i$	! get the data &
	\ print #2%, i$;	! and put it out into the file &
	\ goto 3100		! keep it up until there isn't anymore &

3190	a0$="timed out" &

3200	print #2%, CRLF$;a$;"Message ";a0$;" at: ";date$(0%);"  ";time$(0%); &
			CRLF$;a$; &
	\ goto 3500 &

3400	resume 3010 If err=16 and erl=3010	! try the next name &
	\ if erl=3100 then &
		resume 3200 if err=11 		! End of this message &
	\	resume 3200 if err=27 		! hung up - no more message &
	\	resume 3190 if err=15 		! where did they go? &

3410	if err=6 then				! no EMS$: &
		T0%=FNS%("ERROR - EMS$: not specified") &
	\	resume 3500 &

3420	if err=28 then				! ^C typed &
		resume 3500 if erl<3100		! but nothing read in yet &
	\	a0$="^C terminated"		! correct message &
	\	resume 3200			! go tell them &

3490	resume 3500				! who knows what happened &

3500	i$=sys(chr$(2%)+chr$(1%))		! enable echo again &
	\ close 1,2				! all done &
	\ goto 32650				! so now quit &

5000	!	START HERE IF "HELLO" OR "LOGIN" WITHOUT PPN (OR PPN &
	!	IS THE CURRENT PPN) AND LOGGED IN &
	! &
	GOSUB 10000 		! TYPE SYSTEM HEADER &
	\ GOSUB 17000		! DO THE LOGIN SYSCALL AND PRINT HOW MANY &
				!	JOBS ARE DETACHED IN THIS ACCOUNT &
	\ GOSUB 15010 UNLESS KB.SPAWNED% &
	\ NEW.ACCOUNT%=A%	! SET UP FOR THE GOSUB &

5010	IF M%(4%) AND (NOT (KB.SPAWNED%)) &
		THEN GOSUB 27000 ! GET JOBMAX AND JOBTOTAL &
	\	GOSUB 18000 	! FIND OUT JOB TO ATTACH TO AND VERIFY IT &
	\	GOTO 7000   	! CONTINUE IN THE REGULAR ATTACH SEQUENCE &

5100	PW.DATE.AGO%=0%		! DEFAULT PASSWORD AGE &
	\ GOTO 5150 IF M2%(9%)+SWAP%(M2%(10%))=0%	! EXIT IF NO DATES &
			AND D.L.NI%=0% &
        \ PRINT "Last interactive login on ";DATE$(M2%(9%)+SWAP%(M2%(10%))); &
                ", ";TIME$(M2%(11%)+SWAP%(M2%(12%)) AND 2047%); &
		IF M2%(9%)+SWAP%(M2%(10%)) &
        \ PRINT " at KB";NUM1$(M2%(8%));":" IF M2%(8%)<>255% &
		IF M2%(9%)+SWAP%(M2%(10%)) &
        \ PRINT IF M2%(8%)=255% IF M2%(9%)+SWAP%(M2%(10%)) &
        \ PRINT "Last non-interactive login on "; &
                DATE$(D.L.NI%);", ";TIME$(T.L.NI% AND 2047%) IF D.L.NI% &
	\ IF (ASCII(MID(SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(4%)+ &
		CHR$(NEW.ACCOUNT% AND 255%)+ &
		CHR$(SWAP%(NEW.ACCOUNT%) AND 255%)),12%,1%)) AND 8%)<>8% THEN &
			L.PAS.CHA%=M2%(13%)+SWAP%(M2%(14%)) &
	\		PW.DATE.AGO%=365%	! ASSUME A YEAR OR MORE &
	\		GOTO 5150 IF CUR.DAT%-L.PAS.CHA%>=1000%	! 1 YEAR MAX &
	\		I%=(L.PAS.CHA%/1000%)+70%	! YEAR SINCE 1900 &
	\		PW.DATE.AGO%=CUR.DAT%-L.PAS.CHA% &
	\		PW.DATE.AGO%=PW.DATE.AGO%-1000%+365%-(((I%/4%)*4%)=I%) &
				IF PW.DATE.AGO%>366% &

5150	IF M%(3%)>1% THEN &
		PRINT FNC$;NUM1$(M%(3%)-1%);" other user"; &
		FNS$(M%(3%)-1%);" ";FNA$(M%(3%)-1%); &
		" logged in under this account" &

5200	I$=SYS(CHR$(0%))+SYS(CHR$(2%)) &
	\ CLOSE I% FOR I%=1% TO 12% &
	\ PRINT &
	\ GOTO 32650 UNLESS DO.COMFILE% &
	\ P1$ = '"'+NUM1$(INSTR(1%,SLASH$,","))+'"' &
	\ P2$=' "" ' &
	\ P2$=' "'+NUM1$(SWAP%(OLD.ACCOUNT%) AND 255%)+"," &
		+NUM1$(OLD.ACCOUNT% AND 255%)+'" ' IF LOGGED.IN% AND &
		NEW.ACCOUNT%<>OLD.ACCOUNT% &
	\ P3$='"'+NUM1$(PW.DATE.AGO%)+'"' &
	\ P5$ = '"' + RIGHT(LAT.SERVER.PORT$,18%) + '"' &
	\ P6% = P6% AND NOT(1%) IF INSTR(1%,SLASH$,",") = 0% &
	\ P6% = P6% OR 16% IF LEN(P5$) > 2% &
	\ P6% = P6% OR 32% IF L.CLASS% = 2% &
	\ P6$=' "'+NUM1$(P6%)+'"' &
	\ I$=SYS(PRIV.ON$) &
	\ I$=SYS(NOTERM.CTRLC$) &
	\ I$=SYS(CHR$(14%)+'$@[0,1]LOGIN.COM '+P1$+P2$+P3$+P4$+P5$+P6$) &
	\ GOTO 32650 &
	! Cancel CTRL/O and enable echo on the terminal &
	! Close all open channels &
	! Exit the program unless we are suppose to execute LOGIN.COM &
	! Build the "LOGIN.COM" argument string &
	! Make sure the "/" is passed if it was part of the LOGIN$. &
	!	P1 = flag (0/1) to print $NOTICE.TXT &
	!	P2 = previous account (e.g. "1,2") or null &
	!	P3 = no. days since last password change &
	!	P4 = flag (0/1) to re-enable terminal's CTRL/C &
	!	P5 = LAT Server and Port ID information &
	!	P6 = A bit mask with the following definitions: &
	!		 1 = User wants $NOTICE.TXT printed and it exists &
	!		 2 = User's LOGIN.COM exists &
	!		 4 = User's [,0]LOGIN.COM exists &
	!		 8 = MAIL$:NEWMAIL.TSK exists &
	!		16 = This is a LAT terminal &
	!		32 = This is a BATCH job &
	! Regain temporary privileges &
	! Disable CTRL/C, if it was enabled, before we pass to [0,1]LOGIN.COM &
	! Pass to [0,1]LOGIN.COM &
	&

6000	!	START HERE IF "ATTACH" &
	! &
	GOSUB 27000 			! GET JOBMAX AND JOB TOTALS &
	\ M2%(9%),M2%(10%)=0%		! SUPPRESS LAST LOGIN DATE DISPLAY &
	\ IF LEN(LOGIN$) THEN GOSUB 18800 ! VERIFY JOB SPECFIED IN LOGIN$ &
	  ELSE  GOSUB 10000 		! TYPE SYSTEM HEADER &
	\	GOSUB 17000 IF A% 	! GO DO THE LOGIN SYSCALL &
	\	GOSUB 18000 		! FIND OUT JOB TO ATTACH TO AND &
					! 	VERIFY THE JOB NUMBER &

7000	IF I% &
	THEN	GOTO 950 IF DET.QUO% &
	\	GOTO 32650 UNLESS A%	! EXIT IF LOGGED-OUT ATTACH &
	\	CHANGE SYS(CHR.6$+CHR$(4%)) TO M%	! GET # JOBS INFO &
	\	GOTO 5100		! EXIT VIA TAIL END OF LOGIN CODE &

7020	I$=SYS(PRIV.ON$) &
	\ NEW.ACCOUNT%=PEEK(PEEK(J0%+8%)+24%) &
	\ I$=SYS(PRIV.OFF$) &
	\ ATTACHING%=-1% &
	\ ATT%=J% &
	\ PASSWORD$="" UNLESS DET.QUO%	! Assume no password needed &
	\ GOTO 7100 IF (A%=NEW.ACCOUNT%) OR (LEN(PASSWORD$) <> 0%) &
					! Go right to the attach if the &
					! detached job is in the same account &
					! OR we have a password anyway &

7030	QUO%=0%				! Assume quota OK &
	\ GOSUB 9000 IF A%		! Check quota if logged in &
	\ GOTO 32650 IF QUO%=-1% 	! DON'T LET 'EM ATTACH IF OVER QUOTA &
	\ A%=0% &
	\ Z%=3% &
	\ Z%=10% IF LOGGED.IN% &
	\ I$=SYS(PRIV.ON$) &
	\ I$=SYS(CHR.6$+CHR$(31%)+CVT%$(0%)+ &
		CVT%$(SWAP%(OLD.ACCOUNT%))+PRIV.MASK$) IF LOGGED.IN% &
	\ I$=SYS(CHR.6$+CHR$(4%)+CHR$(0%)+CHR$(Z%)+CVT%$(SWAP%(NEW.ACCOUNT%)) &
		+STRING$(16%,0%)+"SY"+CVT%$(255%)) &
	\ GOTO 7100 &

7035	GOSUB 12000 UNLESS DO.COMFILE%	!WE RETURN HERE IF THERE WAS AN &
					!ERROR FROM THE LOGIN SYS CALL &
					!ABOVE.  GO GET A PASSWORD UNLESS &
					!WE ARE GOING TO EXECUTE THE COMFILE &

7100	I$=SYS(CHR.6$+CHR$(31%)) UNLESS A%=NEW.ACCOUNT% AND DET.QUO% &
	\ I$=SYS(PRIV.OFF$) &
	\ GOSUB 19000 			! GO DO THE ATTACH TO A JOB SYSCALL &
	\ GOTO 7000			! Didn't attach so we came back here &

9000	!    CHECK FOR DISK STORAGE AND DETACHED JOB QUOTAS &
	! &
	! &
	PRIV.MASK$=STRING$(8%,255%) &
	\ N%=1% &

9005	I$=SYS(PRIV.OFF$) &
	\ I$=SYS(CHR$(6%)+CHR$(31%)+CHR$(0%)+CHR$(0%) &
		+CVT%$(SWAP%(OLD.ACCOUNT%))+PRIV.MASK$)	! GRAB A SMALL BUFFER &
	\ CHANGE SYS(CHR$(6%)+CHR$(28%)) TO W%		! GET USER'S PRIVS &
	\ PRIV.MASK$=""					! CREATE A MASK STRING &
	\ PRIV.MASK$=PRIV.MASK$+CHR$(W%(Z%)) FOR Z%=3% TO 10% &
	\ I$=SYS(PRIV.ON$) &
	\ I$=SYS(CHR.6$+CHR$(5%)+CHR$(1%))		! LOGOUT, KEEP SPECS &
	\ QUO%=SWAP%(CVT$%(MID(I$,3%,2%)))		! QUO% RETURNED IN &
							! THE FQERNO &
							!    0%=SUCCESS, &
							!   -1%=FATAL ERROR, &
							!   -2%=WARNING ERROR &
	\ RETURN IF QUO%=0% &
	\ FQSIZE%=SWAP%(CVT$%(MID(I$,13%,2%))) 		! VALUES FOR FQSIZE%: &
							!   0%=DISK ERROR &
							!   1%=DETACHED ERROR &
	\ PLURAL$="" &
	\ A.SOME$=" a " &
	! &
	! &
	! 	DETACHED ERROR &
	! &
	\ IF FQSIZE%=1% THEN DJ%=ASCII(MID(I$,17%,1%)) 	! DET'D JOBS IN ACC'T &
		\ DT%=ASCII(MID(I$,18%,1%)) 		! DET'D TOTALS ALLO'D &
		\ DJ%=DJ%-DT% &
		\ A.SOME$=" some " IF DJ%>1% &
		\ PLURAL$="s" IF DJ%>1% &
		\ PRINT FNC$;CRLF$;"?Detached-job quota of";DT%;"exceeded by" &
			;DJ%;"job";PLURAL$;".";FNC$;"?Please ATTACH to"; &
			A.SOME$;"job";PLURAL$;" in this account." &
		\ RETURN &

9100	! &
	! 	DISK ERRORS &
	! &
	T0$=MID(I$,23%,2%)				! GET THE DEVICE NAME &
	\ T0$=T0$+NUM1$(ASCII(MID(I$,25%,1%)))		!      ADD THE NUMBER &
		IF ASCII(MID(I$,26%,1%)) &
	\ T0$=T0$+":" 					!      ADD THE ":" &
	\ II$=SYS(PRIV.OFF$) &
	\ T= 256.*ASCII(MID(I$,22%,1%)) 		! TOTAL DISK QUOTA &
			+ASCII(MID(I$,21%,1%)) &
	\ Q= 256.*ASCII(MID(I$,20%,1%))			! BLOCKS ALLOCATED &
			+ASCII(MID(I$,19%,1%)) &
	\ PLURAL$="s" IF T-Q>1. &
	! &
	! &
	! 	NON-FATAL DISK ERROR &
	! &
	\ IF QUO%=-2% THEN PRINT FNC$;CRLF$; &
		"%Disk-storage quota of";Q;"on ";T0$;" exceeded by";T-Q; &
		"allocated block";PLURAL$;".";CRLF$ &
	! &
	! &
	! 	FATAL DISK ERROR &
	! &
	  ELSE A.SOME$=" some " IF T-Q>1. &
		\ PRINT FNC$;CRLF$;"?Disk-storage quota of";Q; &
		  "on ";T0$;" exceeded by";T-Q;"allocated block";PLURAL$;"."; &
		  FNC$;"?Please DELETE";A.SOME$;"file";PLURAL$;" on ";T0$;"." &

9150	RETURN &

9900	! THIRD PARTY PRIV CHECK ERRORS HANDLED HERE. &
	! &
	N%=N%+1% &
	\ PRINT "?No buffer space available" IF N%>10% &
	\ GOTO 32650 IF N%>10%  	! QUIT AFTER TEN TRIES &
	\ SLEEP 1% 			! WAIT FOR ONE SECOND &
	\ GOTO 9005 			! AND TRY PRIV CHECKS AGAIN &
	! NO BUFFER SPACE AVAILABLE FOR THIRD PARTY PRIV CHECK SYS CALL &
	&
	&

10000	! &
	! 	TYPE SYSTEM HEADER &
	! &
	RETURN IF HDR.PRINTED% &
	\ HDR.PRINTED%=-1% &
	\ I$=SYS(CHR.6$+CHR$(9%)) &
	\ CHANGE I$ TO M% &
	\ J$=CVT$$(RIGHT(I$,3%),4%) &
	\ IF A% THEN &
		HDR$ =	  J$ &
			+ " Job " + NUM1$(M%(1%)/2%) &
			+ " [" + NUM1$(SWAP%(A%) AND 255%) + "," &
			+ NUM1$(A% AND 255%) + "] " &
			+ " <" + JOB.CLASS$(L.CLASS%) + ">" &
			+ " KB" + NUM1$(M%(2%)/2%) &
	ELSE	HDR$ =	  LEFT(J$,13%) &

10010	PRINT	FNC$; &
		CVT$$(HDR$ + " " + DATE$(0%) + " " + TIME$(0%),8%+16%+128%) &
	\ RETURN &

10900	ON ERROR GOTO 0 &
	\ GOTO 32767 &

11000	!	PROMPT FOR "User:" &
	! &
	I$=SYS(TERM.CTRLC$) IF CTRLC.ENAB%	! ENABLE TERMINAL CTRL/C &
	\ I$=SYS(CHR$(2%)) &
	\ PRINT FNC$;"??Access denied" IF USER% >= 5% &
	\ GOTO 32650 IF USER% >= 5% &
	\ PRINT FNC$;"User: "; &
	\ WAIT 30% UNLESS A% &
	\ GET #1% &
	\ FIELD #1%, RECOUNT AS LOGIN$ &
	\ LOGIN$=CVT$$(LOGIN$,444%) &
	\ I$=SYS(CHR$(3%)) &
	\ WAIT 0% &
	\ USER%=USER%+1% &
	\ GOTO 11000 UNLESS LEN(LOGIN$) &
	\ I$=SYS(NOTERM.CTRLC$) 	! DISABLE TERMINAL CTRL/C &
	\ USER%=0% &
	\ RETURN &

11900	WAIT 0% &
	\ LOGIN$="'" &
	\ RESUME 32650 IF ERR=11% &
	\ RESUME 19999 &
	&
	&

12000	! &
	! &
	!	PROMPT FOR PASSWORD and system password &
	! &
	! &
	T0$="Password: " &
	\ GOSUB 12200 &
	\ PASSWORD$=T0$ 		! GET/SAVE USER INPUT &
	\ PRINT &
	\ RETURN &
		! &
		! DISABLE ECHO-PRINT; PROMPT AND GET PASSWORD; &
		! RE-ENABLE ECHO-PRINT &
	&

12050	T0$="System password: " &
	\ GOSUB 12200 &
	\ SYSPW$=T0$ &
	\ PRINT &
	\ RETURN &
	&

12100	I$=STRING$(16%,8%) &

12110	PRINT STRING$(16%,88%); I$;	! XXXXXXXX &
	\ PRINT STRING$(16%,79%); I$;	! OOOOOOOO &
	\ PRINT STRING$(16%,37%); I$;	! %%%%%%%% &
	\ RETURN &
	&

12200	! COMMON SUBROUTINE TO ASK FOR PASSWORD AND SYSTEM PASSWORD. &
	! &
	I$=SYS(TERM.CTRLC$) IF CTRLC.ENAB%	! ENABLE TERMINAL CTRL/C &
	\ PRINT FNC$;"??Access denied" IF USER% >= 5% &
	\ GOTO 32650 IF USER% >= 5% &
	\ I$=SYS(CHR$(3%)) &
	\ PRINT FNC$+T0$; &
	\ GOSUB 12100 IF (I0% AND 1024%) &
	\ WAIT 30% UNLESS A% &
	\ GET #1% &
	\ FIELD #1%, RECOUNT AS TEMP$ &
	\ PRINT CHR$(10%); IF (I0% AND 1024%) &
	\ TEMP$=CVT$$(TEMP$,4%) &
	\ I$=SYS(CHR$(2%)) &
	\ USER%=USER%+1% &
	\ GOTO 12200 UNLESS LEN(TEMP$) &
	\ I$=SYS(NOTERM.CTRLC$)			! DISABLE TERMINAL CTRL/C &
	\ USER%=0% &
	\ T0$=TEMP$ &
	\ WAIT 0% &
	\ RETURN &

12900	I$=SYS(CHR$(2%)) &
	\ WAIT 0% &
	\ PASSWORD$="?" &
	\ RESUME 32650 IF ERR=11% &
	\ RESUME 19999 &
	&

13000	! &
	! 	VERIFY PPN &
	! &
	&

13001	LOGIN$="" IF KB.SPAWNED% &
	\ SLASH$="," &
	\ COMMA%=-1% &
	\ I%=INSTR(1%,LOGIN$,"/") &
	\ IF I% THEN &
		LOGIN$=LEFT(LOGIN$,I%-1%)+","+RIGHT(LOGIN$,I%+1%) &
	\	SLASH$="/" &
	\	COMMA%=0% &
	! DON'T CARE ABOUT ANY OF THIS IF JOB WAS SPAWNED FROM A TERMINAL. &
	! DEFAULT PPN SEPARATOR SET TO "," &
	! CHECK THE STRING FOR A SLASH &
	! IF THERE IS A "/" THEN &
	!	INSERT A "," IN PLACE OF THE "/" &
	!	SET PPN SEPARATOR TO "/" &

13010	I%=-1% &
	\ SLASH$="/" UNLESS LEN(LOGIN$) &
	\ GOTO 13020 UNLESS LEN(LOGIN$) &
	\ LOGIN$=CVT$$(LOGIN$,64%) &
	\ IF INSTR(1%,LOGIN$,",") <> 0% THEN &
		LOGIN$="["+LOGIN$ UNLESS INSTR(1%,LOGIN$,"(") &
	\	LOGIN$=LOGIN$+"]" UNLESS INSTR(1%,LOGIN$,")") &
	\	CHANGE SYS(CHR.6$+CHR$(-10%)+LOGIN$) TO M% &
	\	I%=0% IF M%(5%)<=254% AND M%(6%)<=254% AND M%(6%)<>0% &
				AND (M%(28%) AND 154%)=0% &
	\	GOTO 13015 &
	! SET DEFAULT TO ERROR CONDITION &
	! SKIP THE REST IF THE LENGTH OF LOGIN$ IS ZERO &
	! IF THERE IS A COMMA IN LOGIN$, THEN IT MUST BE A PPN &
	! PUT THE BRACKETS AROUND THE PPN IF NOT ALREADY IN THE STRING &
	! DO THE SYS CALL TO SEE IF THE PPN IS REAL &
	! I%=0% IF THERE IS NO ERROR IN THE PPN &
	&

13012	LOGIN$=LOGIN$+":" IF INSTR(1%,LOGIN$,":")=0% &
	\ V$=SYS(CHR.6$+CHR$(-10%)+LOGIN$) &
	\ CHANGE V$ TO M% &
	\ IF (M%(30%) AND 128%)=128% THEN &
		M%(6%), I% = 0% &
	ELSE &
	  M%(30%) = M%(30%) AND NOT (8%)	!Clear protection code bit &
	\ IF M%(5%)<=254% AND M%(6%)<=254% AND M%(6%)<>0% &
	     THEN IF M%(27%)+SWAP%(M%(28%))=(12288%) &
	     THEN IF M%(29%)+SWAP%(M%(30%))=(28800%) &
	     THEN IF ASCII(MID(SYS(CHR.6$+CHR$(-25%)+CHR$(-4%) &
			+STRING$(19%,0%)+MID(V$,23%,4%)),22%,1%))=1% &
	     THEN I%=0% &
	! ADD A COLON IF USER DIDN'T &
	! SCAN THE LOGICAL NAME (MUST BE SYSTEM LOGICAL FOR SY:[PPN]) &
	! CHECK PPN SAME AS ABOVE &
	! ALLOW NON-TRANSLATABLE LOGICALS TO PASS THROUGH &
	! Clear protection code bit incase it is set &
	! IF ACCOUNT IS LEGAL (SAME CRITERIA AS IN 13010), &
	!    THEN IF COLON, LOGICAL DEVICE NAME SET IN FLAG 1, &
	!    THEN IF PPN, COLON, DEV NAME, DEV NAME=LOGICAL SET IN FLAG 2, &
	!      (AND NO OTHER BITS SET IN EITHER FLAG WORD), &
	!    THEN IF DEVICE IS A SYSTEM DISK, &
	!    THEN RESET I% TO 0% TO SAY NO ERROR IN LOGIN DESTINATION &

13015	L.PROJ%=M%(6%) &
	\ L.PROG%=M%(5%) &
	\ IF LOGGED.IN% THEN &
		IF A% AND M%(5%)+SWAP%(M%(6%))=OLD.ACCOUNT% THEN &
			I%=-1% &
	\		SLASH$="/" UNLESS COMMA% &
	! STORE THE PPN IN THE CORRECT NAMES. &
	! I%=-1% IF THE PPN SPECIFIED IS THE SAME AS THE CURRENT PPN. &
	&

13020	GOSUB 26500				!Go lookup LOGIN.COM's files &
		IF (I% = 0%) OR (LOGGED.IN% <> 0%)! If the PPN is valid &
	\ SEND.OPSER%=-1% IF L.CLASS% OR (LEN(LAT.SERVER.PORT$)<>0%) &
	\ M2%(J%)=0% FOR J%=0% TO 30%	! CLEAR IT OUT FIRST &
        \ CHANGE SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(6%)+ &
                CHR$(L.PROG%)+CHR$(L.PROJ%)) TO M2% &
                ! Read last non-interactive login time info &
        \ D.L.NI%=M2%(17%)+SWAP%(M2%(18%)) ! Date of last non-interactive &
        \ T.L.NI%=M2%(19%)+SWAP%(M2%(20%)) ! Time of last non-interactive &
	\ CHANGE SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(4%)+ &
		CHR$(L.PROG%)+CHR$(L.PROJ%)) TO M2%  ! Read login time info &
	\ IF KB.SPAWNED% &
	    THEN &
		M2%(8%)=ASCII(RIGHT(SPAWNED$,5%))	! KB &
	\	M2%(9%)=ASCII(MID(SPAWNED$,2%,1%))	! LOW BYTE DATE &
	\	M2%(10%)=ASCII(SPAWNED$)		! HIGH BYTE DATE &
	\	M2%(11%)=ASCII(MID(SPAWNED$,4%,1%))	! LOW BYTE TIME &
	\	M2%(12%)=ASCII(MID(SPAWNED$,3%,1%))	! HIGH BYTE TIME &

13030	RETURN &

13900	RESUME 19999 IF KB.SPAWNED% OR (NOT (LOGGED.IN%)) OR A%=0% &
	\ PRINT "?Invalid entry - try again" IF A% &
	\ I$=SYS(TERM.CTRLC$) IF CTRLC.ENAB%	! ENABLE TERMINAL CTRL/C &
	\ RESUME 32650 &
	&
	&

14000	GOTO 14100 IF A% OR SYS.PW.OK% &
	\ I$=SYS(CHR.6$+CHR$(4%)+CHR$(0%)+CHR$(6%)) &
	\ GOTO 14100 &
	! Exit if already OK or logged in &

14010	GOSUB 10000			! Print header if not done yet &
	\ GOSUB 12050			! Get system password &
	\ I$=SYS(CHR.6$+CHR$(4%)+CHR$(0%)+CHR$(6%)+CVT%$(0%)+SYSPW$) &

14100	I%=0% &
	\ SYS.PW.OK%=-1% &
	\ RETURN &

14900	I%=-1% &
	\ RESUME 14010 IF ERL=14000%	! Ask if PW required &
	\ RESUME 19999			! Other error, assume failure &

15000	! &
	!		CHECK OUT THE PASSWORD &
	! &
	I%=-1% &
	\ Z%=2% &
	\ I$=SYS(CHR.6$+CHR$(-9%)+CHR$(KB%)+CHR$(-1%)) 	   ! SET DTR ON &
	\ NEW.ACCOUNT%=SWAP%(L.PROJ%)+L.PROG% UNLESS ATTACHING% &
	\ GOSUB 15010		! SET PRIORITY ETC. &
	\ Z%=10% IF A%		! IGNORE PASSWORD IF PRIVILEGED ENOUGH &
	\ I$=SYS(PRIV.ON$) &
	\ I$=SYS(CHR$(6%)+CHR$(31%)+CHR$(0%)+CHR$(0%) &
		+CVT%$(SWAP%(OLD.ACCOUNT%))+PRIV.MASK$)	! ENABLE PRIV CHECK &
	\ CHANGE SYS(CHR.6$+CHR$(4%)+CHR$(0%)+CHR$(Z%) &
		+CVT%$(SWAP%(NEW.ACCOUNT%))+PASSWORD$) TO M% &
	\ I$=SYS(CHR$(6%)+CHR$(31%))		! TURN OFF PRIV CHECKING &
	\ I$=SYS(PRIV.OFF$) &
	\ A%=NEW.ACCOUNT% &
	\ LOGIN$ = LEFT(LOGIN$,LEN(LOGIN$)-1%)	!Strip off the colon &
	       IF RIGHT(LOGIN$,LEN(LOGIN$)) = ":" ! if it's there &
	\ T0%=FNS%("LOGIN entering "+LOGIN$) IF SEND.OPSER% &
	\ I%=0% &
	\ RETURN &
	! Set error condition &
	! Set Z% to check the password &
	! Set Z% to ignore the password if they are logged in &
	! Turn temp privs back on &
	! Enable third party priv check &
	! Do the login &
	! Disable the terminal's CTRL/C if no quota problems and no det jobs &
	! Turn off third party priv check &
	! Drop temp privs &
	! I%=0% IF THERE IS NO ERROR IN THE PASSWORD. &
	&

15010	RETURN IF L.CLASS%=2%	! SKIP THIS STUFF IF THIS IS A BATCH JOB &
	\ I$=SYS(PRIV.ON$) &
	\ I$=SYS(CHR.6$+CHR$(-13%)+CHR$(-1%)+ &
		CHR$(-1%)+CHR$(-8%)+ &
		CHR$(-1%)+CHR$( 6%)+ &
		CHR$(-1%)+CHR$(64%)) &
	\ I$=SYS(PRIV.OFF$) &
	\ I$=SYS(CHR$(6%)+CHR$(11%)) ! DEASSIGN ALL LOGICAL NAMES AND DEVICES &
	\ I$=SYS(CHR$(6%)+CHR$(22%)+STRING$(33%,0%)+CHR$(255%)) ! REMOVE RIBS &
	\ RETURN &

15900	I%=-2% IF ERR=8% AND ERL=15000% &
	\ I$=SYS(PRIV.OFF$) &
	\ I$=SYS(CHR$(6%)+CHR$(31%))		! TURN OFF PRIV CHECKING &
	\ RESUME 19999 &
	&
	&

16000	! &
	!	DELETE ALL ????NN.TMP FILES &
	! &
	W%=A% &
	\ I$=SYS(PRIV.ON$) &
	\ CHANGE SYS(CHR.6$+CHR$(-10%)+"????"+ &
		RIGHT(NUM1$(100%+(255% AND PEEK(518%))/2%),2%)+ &
		".TMP") TO W% &
	\ I$=SYS(PRIV.OFF$) &
	\ W%(0%)=30% &
	\ W%(1%)=6% &
	\ W%(2%)=33% &
	\ W%(3%)=4% &
	\ W%(TEMP%)=0% FOR TEMP%=4% TO 6% &
	\ W%(TEMP%)=0% FOR TEMP%=13% TO 30% &
	\ CHANGE W% TO W$ &

16010	CHANGE SYS(W$) TO W% &
	\ I$=SYS(PRIV.ON$) &
	\ KILL RAD$(W%(7%)+SWAP%(W%(8%)))+RAD$(W%(9%)+SWAP%(W%(10%)))+ &
		"."+RAD$(W%(11%)+SWAP%(W%(12%))) &
	\ I$=SYS(PRIV.OFF$) &
	\ GOTO 16010 &

16900	RESUME 19999 &
	&
	&

17000	! &
	!	DO THE LOGIN SYSCALL &
	!	CHECK FOR DETACHED JOBS IN THE ACCOUNT &
	! &
	CHANGE SYS(CHR.6$+CHR$(4%)) TO M% &

17100	RETURN UNLESS M%(4%) &
	\ RETURN IF KB.SPAWNED% &
	\ I%=4% &
	\ PRINT FNC$ &
	\ PRINT "Jobs detached under this account:" &
	\ PRINT "   Job  What  Size  State   Run-time   RTS" &
	\ I$=SYS(PRIV.ON$) &

17110	RETURN IF I%>30% &
	\ J%=M%(I%) &
	\ RETURN UNLESS J% &
	\ I%=I%+1% &
	\ CHANGE SYS(CHR.6$+CHR$(26%)+CHR$(J%)+CHR$(0%)) TO S1% &
	\ CHANGE SYS(CHR.6$+CHR$(26%)+CHR$(J%)+CHR$(1%)) TO S2% &
	\ PRINT FNC$; SPACE$(3%-(J%<10%)); NUM1$(J%); "  "; &
		RAD$(S1%(17%)+SWAP%(S1%(18%)));RAD$(S1%(19%)+SWAP%(S1%(20%))); &
		" "; NUM1$(S2%(13%)); "K"; TAB(20%); &
	\ S%=S2%(9%)+SWAP%(S2%(10%)) &
	\ W%=S2%(11%)+SWAP%(S2%(12%)) &
	\ S$="RN" &
	\ GOTO 17120 IF S% AND W% &
	\ W%=W% AND -16383% IF W% AND -16383% &
	\ S$="BF" &
	\ GOTO 17120 IF W% AND 16384% &
	\ S$="SL" &
	\ IF W% AND 8192% &
	THEN	S$="SR" IF S2%(7%) &
	\	GOTO 17120 &

17115	S$="FP" &
	\ GOTO 17120 IF W% AND 4096% &
	\ S$="TT" &
	\ GOTO 17120 IF W% AND 2048% &
	\ S$="HB" &
	\ GOTO 17120 UNLESS W% &
	\ S$=CHR$(S2%(21%))+CHR$(S2%(22%)) &
	\ GOTO 17120 UNLESS W% AND 2% &
	\ S$="KB" &
	\ S$="^C" IF S2%(22%) AND 128% &

17120	PRINT S$; " "; &
	\ S$="   " &
	\ S$="Lck" IF S2%(14%) AND 240% &
	\ IF S2%(14%) AND 8% &
	THEN	S$=CHR$(65%+S1%(6%)/64%)+RIGHT(NUM1$(100%+(S1%(6%) AND 63%)),2%) &
	\	S$="Swi" IF S2%(14%) AND 4% &
	\	S$="Swo" IF S2%(14%) AND 2% &

17130	PRINT S$; &
	\ T0=(S1%(7%)+SWAP%(S1%(8%))) &
	\ T0=T0+65536. IF T0<0. &
	\ T0=T0+65536.*S1%(16%) &
	\ T=INT(T0/10.) &
	\ T0=T0-10.*T &
	\ GOSUB 22000 &
	\ PRINT SPACE$(8%-LEN(S$)); S$; "."; CHR$(48%+T0); "  "; &
		RAD$(S1%(27%)+SWAP%(S1%(28%))); RAD$(S1%(29%)+SWAP%(S1%(30%))) &

17900	RESUME 17110 &
	&
	&

18000	!	GET JOB NUMBER TO ATTACH TO AND VERIFY IT &
	!	START HERE WHEN THE JOB NUMBER IS NOT SPECIFIED IN THE &
	!	ATTACH COMMAND AS ONE LINE &
	! &
	J%=0% &
	\ I%=-1% &
	\ WAIT 30% UNLESS A% &
	\ I$=SYS(CHR$(0%))+SYS(CHR$(2%)) &
	\ I$=SYS(TERM.CTRLC$) IF CTRLC.ENAB% 	! ENABLE TERMINAL CTRL/C &
	\ PRINT FNC$;"Job number to attach to"; &
	\ INPUT J% &
	\ I$=SYS(NOTERM.CTRLC$) &
	\ WAIT 0% &

18010	I%=-3% &
	\ I%=-2% UNLESS J% &
	\ I%=0% IF J%>0% AND J%<=J9% ! CHECK TO BE SURE THAT IT IS A VALID # &

18020	IF I%=0% THEN I$=SYS(PRIV.ON$) &
	\	J0%=PEEK(M%+(J%*2%)) &
	\	IF J0%=0% THEN &
			I%=-4% &
		ELSE	IF J%*2%=(PEEK(PEEK(PEEK(J0%))+2%) AND 255%) &
				AND &
			(PEEK(PEEK(PEEK(J0%))+6%) AND 8192%)<>0% &
			THEN I%=-5% &

18030	I$=SYS(PRIV.OFF$) &
	\ IF I%=0% THEN RETURN &
	  ELSE ON -I% GOTO 18100,18200,18300,18400,18500 &
	&

18100	PRINT FNC$;"%Illegal job number"; &
	\ GOTO 18600 &

18200	RETURN &

18300	PRINT FNC$;"%Job number out of range"; &
	\ GOTO 18600 &

18400	PRINT FNC$;"%No job by that number"; &
	\ GOTO 18600 &

18500	PRINT FNC$;"%Job not detached"; &

18600	IF A% THEN PRINT " - try again" &
	\	GOTO 18000 &

18610	PRINT " - access denied" &
	\ RETURN &

18800	! 	ENTRY POINT INTO THIS SUBROUTINE IF THE JOB NUMBER TO &
	! 	ATTACH TO WAS SPECIFIED IN THE LOGIN STRING ON ONE LINE &
	! &
	I%=-1% &
	\ J%=VAL(LOGIN$) &
	\ GOTO 18010 &

18900	WAIT 0% &
	\ I%=-2% IF ERR=11% &
	\ J%=0% IF ERR=11% &
	\ RESUME 18030 &
	&
	&

19000	! &
	!	DO THE ATTACH TO A JOB SYSCALL &
	!	SEND TO OPSER &
	! &
	I%=-1% &
	\ GOSUB 16000 IF A% 			! GO DELETE .TMP FILES &

19010	IF SEND.OPSER% THEN &
		I$=SYS(PRIV.ON$) &
	\	T0$=SYS(CHR$(6%)+CHR$(26%)+CHR$(ATT%)+CHR$(0%)) &
	\	I$=SYS(PRIV.OFF$) &
	\	T0%=SWAP%(CVT$%(MID(T0$,21%,2%))) &
	\	I0$=NUM$(ATT%)+"- ["+NUM1$(SWAP%(T0%) AND 255%)+ &
			","+NUM1$(T0% AND 255%)+"] "+ &
			RAD$(SWAP%(CVT$%(MID(T0$,17%,2%))))+ &
			RAD$(SWAP%(CVT$%(MID(T0$,19%,2%)))) &
	\	T0%=FNS%("ATTACHing to job"+I0$) &
	! 	IF WE ARE LOGGING THINGS, LOG THE ATTACH ATTEMPT. &

19020	PRINT FNC$;"Attaching to job";ATT% &
	\ CLOSE B% FOR B%=1% TO 12% &
	\ Z%=2% &
	\ Z%=4% UNLESS LEN(PASSWORD$) &
	\ Z%=2% IF (DET.QUO% <> 0%) AND (LEN(PASSWORD$) <> 0%) &
	\ I$=SYS(PRIV.ON$) IF LOGGED.IN% &
	\ I$=SYS(TERM.CTRLC$) IF CTRLC.ENAB% &
	\ I$=SYS(CHR.6$+CHR$(31%)+CVT%$(0%)+CVT%$(SWAP%(OLD.ACCOUNT%))+ &
		PRIV.MASK$) IF (LOGGED.IN%) AND (DET.QUO%=0%) &
				AND (LEN(PASSWORD$) = 0%) &
	\ I$=CHR.6$+CHR.6$+CHR$(ATT%)+CHR$(Z%) &
		+CVT%$(SWAP%(NEW.ACCOUNT%))+PASSWORD$ &
	\ I$=I$+STRING$(30%-LEN(I$),0%) &
	\ I$=SYS(I$) &
	\ GOTO 32767 &
	&

19900	RESUME 19910 &

19910	OPEN "_KB:LOGIN.CMD" FOR INPUT AS FILE 1% &
	\ T0%=FNS%("Failure to attach to job"+I0$) IF SEND.OPSER% &
	\ PRINT FNC$;"?Failure to attach to job";ATT% &
	\ GOTO 19999 UNLESS LOGGED.IN% &
	\ I$=SYS(PRIV.ON$) &
	\ CHANGE SYS(CHR.6$+CHR$(4%)+CHR$(0%)+CHR$(8%)+ &
		CVT%$(SWAP%(OLD.ACCOUNT%))) TO M% &
	\ I$=SYS(PRIV.OFF$) &
	\ A% = OLD.ACCOUNT% &
	\ GOTO 19999 &
	! If we failed to attach let the user (and OPSER) know. &
	! Go back unless we were logged-in. &
	! Turn on temp privs, &
	! Log us back into our old account, &
	! Turn off privs. &
	! Put A% back to our old account number &
	!  so that we won't look like we failed (and get killed). &
	! Go back. &
	&
	&
	&

19999	!	GENERAL RETURN &
	! &
	RETURN &
	&

20000	RETURN IF WARN.PRINTED%   ! RETURN IF WARNING MESSAGE ALREADY PRINTED &
	\	RETURN IF A%	      ! RETURN IF ALREADY LOGGED IN &
	\	WARN.PRINTED% = -1%   ! INDICATE WARNING MESSAGE PRINTED &
	\	ON ERROR GOTO 20500   ! SET ERROR HANDLER &

20010	I$=SYS(PRIV.ON$) &
	\	OPEN "[0,1]LOGIN.TXT" FOR INPUT AS FILE 3% &
	! OPEN THE TEXT FILE THAT CONTAINS THE WARNING MESSAGES &

20020	UNTIL 0% &
	\	INPUT LINE #3%, A$ &
	\	PRINT A$; &
	\NEXT &
	!	PRINT OUT WARNING MESSAGE BEFORE THE PROMPT &

20045	CLOSE 3%   ! CLOSE THE MESSAGE FILE. &
	\	I$=SYS(PRIV.OFF$) &
	\	ON ERROR GOTO 32400   ! RESET THE ERROR HANDLER &
	\	RETURN &

20500	RESUME 20045 IF (ERR = 11%) AND (ERL = 20020) &
	\	RESUME 20045 IF (ERR = 5%) AND (ERL = 20010) &
	\	I$=SYS(PRIV.OFF$) &
	\	GOTO 32400 &
	&

21700	DEF* FNJOB.CLASS%(JOB.NO%)= &
	    ASCII(MID(SYS(CHR.6$+CHR$(26%)+CHR$(JOB.NO%)+CHR$(2%)),15%,1%)) &

22000	! &
	&
	&
	!	T I M E -    H H : M M : S S . S &
	&
	&

22010	GOTO 22060 IF T<0. OR T>3600000. &
	\ S$="" &
	\ T%=0% &
	\ T1%=T/3600. &
	\ IF T1%<>0% THEN &
		T=T-3600.*T1% &
	\	GOSUB 22040 &
	\	S$=S$+":" &
	! MAKE SURE TIME IS WITHIN ALLOWABLE RANGE. &
	! CLEAR STRING TO BE RETURNED AND INITIALIZE &
	! T1%= # OF HOURS - SUBTRACT FROM TOTAL IF NON-ZERO &

22020	T1%=T/60% &
	\ IF (T1% OR T%)<>0% THEN &
		T=T-60%*T1% &
	\	GOSUB 22040 &
	\	S$=S$+":" &
	! NOW T1%= # OF MINUTES - SUBTRACT FROM TOTAL IF NON-ZERO AND &
	! TACK ON TO STRING &

22030	T1%=T &
	! PREPARE TO GET SECONDS.TENTHS OF SEC. &

22040	T$=NUM$(T1%+T%*100%) &
	\ S$=S$+MID(T$,2%+T%,LEN(T$)-2%-T%) &
	\ T%=1% &
	! TACK THIS PART ONTO THE STRING &

22050	RETURN &
	&

22060	S$="???:??:?" &
	\ GOTO 22050 &
	! COULDN'T CONVERT THE NUMBER. &
	&

22500	! &
	!	COMMON ROUTINE TO ALL ENTRY POINTS. &
	!	SET UP CONSTANTS AND INITIAL VALUES. &
	!	DROP TEMPORARY PRIVILEGES. &
	! &
	JOB%=(PEEK(518%) AND 255%)/2% &
	\ I%=PEEK(520%) &
	\ PPNPTR%=PEEK(I%+8%)+24% &
	\ OLD.ACCOUNT%,PPN%,A%=PEEK(PPNPTR%) &
	\ KB.DDB%=PEEK(PEEK(I%)) &
	\ KB%=SWAP%(PEEK(KB.DDB%+2%)) AND 255% &
	\ TTINTF%=PEEK(KB.DDB%+30%) &
	\ SEND.OPSER%=(TTINTF% AND 16384%)<>0% &
	\ CUR.DAT%=PEEK(512%) &
	\ I0%=PEEK(PEEK(PEEK(PEEK(520%)))+34%) &
	\ USER%=0% &
	\ CHR.6$=CHR$(6%) &
	\ CHANGE SYS(CHR.6$+CHR$(-3%)) TO M% &
	\ JOBTBL%=M%(11%)+SWAP%(M%(12%)) &
	\ PRIV.OFF$=CHR.6$+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR.6$+CHR$(-21%)+CHR$(0%) &
	\ I$=SYS(PRIV.OFF$) &
	\ ATTACHING%=0% &
	\ CRLF$=CHR$(13%)+CHR$(10%) &
	\ CHANGE SYS(CHR.6$+CHR$(16%)+CHR$(1%)+CHR$(255%)) TO W% &
	\ P4$=' "'+NUM1$(W%(23%) AND 1%)+'" ' &
	\ CTRLC.ENAB%=W%(23%) AND 1% &
	\ EMS%=w%(26%) and 16% &
	\ NOTERM.CTRLC$=CHR$(6%)+CHR$(16%)+CHR$(1%)+CHR$(255%)+ &
		STRING$(19%,0%)+CHR$(1%)+STRING$(6%,0%) &
	\ TERM.CTRLC$=CHR$(6%)+CHR$(16%)+CHR$(1%)+CHR$(255%)+ &
		STRING$(18%,0%)+CHR$(1%)+STRING$(7%,0%) &
	\ I$=SYS(NOTERM.CTRLC$) &
	! GET THE CURRENT JOB NUMBER. &
	! GET OLD.ACCOUNT%, PPN% AND A%. &
	! GET KEYBOARD DEVICE DESCRIPTER BLOCK. &
	! GET THE CURRENT DATE (WITH PEEK AT 512%). &
	! SET FIP CALL VARIABLE. &
	! GET MONITOR TABLES PART 1. &
	! &

22505	CTRL.CTRAP$=CHR$(6%)+CHR$(-7%) &
	\ I$=SYS(CTRL.CTRAP$) &
	\ L.PROG%=(A% AND 255%) &
	\ L.PROJ%=(SWAP%(A%) AND 255%) &
	\ L.CLASS%=FNJOB.CLASS%(JOB%) &
	\ JOB.CLASS$(0%)="Local" &
	\ JOB.CLASS$(1%)="Dialup" &
	\ JOB.CLASS$(2%)="Batch" &
	\ JOB.CLASS$(4%)="Net" &
	\ JOB.CLASS$(6%)="Server" &
	\ LAT.SERVER.PORT$=FNLAT.SERVER.PORT$(KB%) &
	\ SEND.OPSER% = -1% IF (LEN(LAT.SERVER.PORT$)<>0%) &
	\ ON ERROR GOTO 32400 &
	\ GOSUB 20000 UNLESS EMS% &
	! ENABLE CTRL/C TRAPPING TO HANGUP MODEM LINES. &
	! CREATE THE PROGRAMER NUMBER. &
	! CREATE THE PROJECT NUMBER. &
	! CREATE THE JOB TYPE FOR THIS JOB. &
	! CREATE JOB CLASS TABLE. &
	! LOOK UP THE LAT/SERVER/PORT INFO IF APPROPRIATE. &
	! PRINT THE WARNING MESSAGE. &
	&

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
	&
	&

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
			+STRING$(11%,0%)+CHR$(4%)+CHR$(5%)+"LOGIN"+CHR$(0%) &
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
	\	RESUME 32650 &
		! IF 'NO SMALL BUFFERS', WE PRINT THE ERROR MESSAGE AND ABORT. &
		! IF ANY OTHER ERROR ON SEND, WE WILL BROADCAST TO KB0:. &

22640	ON ERROR GOTO 32400 &
	\ DUMB% = 0% &
	\ PPN%=PEEK(PPNPTR%) &
	\ I$=CRLF$+"MESSAGE"+STRING$(3%,9%)+": "+DATE$(0%)+" " &
		+TIME$(0%)+"  JOB:"+NUM1$(JOB%)+"  KB"+NUM1$(KB%)+":  LOGIN [" &
		+NUM1$(SWAP%(PPN%) AND 255%)+","+NUM1$(PPN% AND 255%)+"]" &
		+CRLF$+CHR$(9%)+I$+CRLF$ &
		! INITIALIZE RETRY COUNTER &
		! BUILD STRING TO BROADCAST TO KB0:. &

22650	T0$=SYS(PRIV.ON$) &
	\ T0$=SYS(CHR.6$+CHR$(-5%)+CHR$(0%)+I$) &
	\ DUMB%=DUMB%+1% &
	\ GOTO 22690 IF 30%<=DUMB% &
	\ IF RECOUNT THEN &
		I$=RIGHT(I$,LEN(I$)-RECOUNT+1%) &
	\	GOTO 22690 IF ASCII(MID(SYS(CHR.6$+CHR$(16%)),28%,1%))=255% &
	\	SLEEP 1% &
	\	GOTO 22650 &
		! BROADCAST THE MESSAGE TO KB0: UNLESS IT IS GAGGED. &
		! IF RECOUNT<>0%, NOT ALL CHARACTERS WERE BROADCAST, &
		!	SO GO BACK AND BROADCAST THE REMAINING ONES. &

22690	ON ERROR GOTO 32400 &
	\ T0$=SYS(PRIV.OFF$) &
	\ FNEND &
		! END OF FNS%(I$). &
	&


23000	DEF* FNS$(Q%) &
	\ FNS$="" &
	\ FNS$="s" IF Q%<>1% &
	\ FNEND &
	! FNS$ IS FOR PLURALS &

23100	DEF* FNZ$(q%,q0%) &
	\ FNZ$=right(string$(q0%,48%)+num1$(q%),len(num1$(q%))+1%) &
	\ fnend &

24000	DEF* FNA$(Q%) &
	\ FNA$="is" &
	\ FNA$="are" IF Q%<>1% &
	\ FNEND &
	! FNA$ IS FOR PLURAL VERBS &

25000	DEF* FNC$ &
	\ FNC$="" &
	\ FNC$=CRLF$ IF CCPOS(0%) &
	\ FNEND &
	! FNC$ RESTORES CARRIAGE IF NEEDED &

26000	DEF* FNC%(Q$,Q0$,Q%) &
	\ FOR Q0%=LEN(Q0$) TO Q% STEP -1% &
	\	GOTO 26010 IF LEFT(Q$,Q0%)=LEFT(Q0$,Q0%) &
	\ NEXT Q0% &
	\ Q0%=-1% &

26010	FNC%=Q0%+1% &
	\ FNEND &
		! FNC% DETERMINES UNIQUENESS OF A COMMAND
26500	ON ERROR GOTO 26509			!Set local error trap &
	\ P6% = 0%				!Init the P6 bit mask &
	\ V$ = SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(2%)+ 	!Lookup privs &
		CHR$(L.PROG%)+CHR$(L.PROJ%)+STRING$(16%,0%)+"SY"+ &
		CHR$(0%)+CHR$(255%)) &
	\ PRIV.MASK$ = MID(V$,9%,8%)		!Get the priv mask &
	\ N% = 1%				!Init the NO BUFFS counter &

26503	V$ = SYS(CHR$(6%)+CHR$(31%)+CVT%$(0%)+	!Install 3rd party privs &
		CHR$(L.PROG%)+CHR$(L.PROJ%)+PRIV.MASK$) &
	\ FILE.TO.FIND$ = "$NOTICE.TXT"		!Lookup $NOTICE.TXT &
	\ BIT.VALUE% = 1% &
	\ GOSUB 26510 &
	\ FILE.TO.FIND$ = "["+NUM1$(L.PROJ%)+","+NUM1$(L.PROG%)+"]LOGIN.COM" &
	\ BIT.VALUE% = 2%			!Lookup user's LOGIN.COM &
	\ GOSUB 26510 &
	\ FILE.TO.FIND$ = "MAIL$:NEWMAIL.TSK"	!Lookup NEWMAIL program &
	\ BIT.VALUE% = 8% &
	\ GOSUB 26510 &
	\ IF L.PROG% THEN			!If the account is not [,0] &
		FILE.TO.FIND$ = "["+NUM1$(L.PROJ%)+",0]LOGIN.COM" &
	\	BIT.VALUE% = 4%			! Lookup [,0]LOGIN.COM &
	\	GOSUB 26510 &

26505	ON ERROR GOTO 32400			!Reset main error trap &
\	V$ = SYS(CHR$(6%)+CHR$(31%))		!Disable 3rd party privs &
\	RETURN &

26509	RESUME 26505 IF ERR <> 32%	!Exit if the error isn't NO BUFFS &
	\ N%=N%+1%			!Inc the counter &
	\ PRINT "?No buffer space available" IF N%>10% &
	\ RESUME 32650 IF N%>10%  	! Quit after ten tries &
	\ SLEEP 1% 			! Wait for one second &
	\ RESUME 26503			! and try priv checks again &
	! No buffer space available for third party priv check sys call &

26510	ON ERROR GOTO 26590			!Set local error trap &
\	V$ = SYS(CHR$(6%)+CHR$(-10%)+FILE.TO.FIND$) !FSS the file &
\	GOTO 26599 IF (ASCII(RIGHT(V$,30%)) AND 128%) = 128% !Exit if invalid &
\	V$ = SYS(CHR$(6%)+CHR$(17%)+CHR$(255%)+CHR$(255%)+ !Look it up &
		MID(V$,5%,8%)+STRING$(10%,0%)+MID(V$,23%,4%)) &
\	P6% = P6% OR BIT.VALUE%			!It's there, set the bit &
\	GOTO 26599				! and exit &

26590	RESUME 26599				!Error occured, so exit &

26599	ON ERROR GOTO 26509			!Reset main error trap &
\	RETURN					!End of routine &
	&
	&

27000	! GET JOBMAX (J9%) AND JOBTBL (M%) &
	! &
	CHANGE SYS(CHR.6$+CHR$(-12%)) TO W% &
	\ V$ = SYS(PRIV.ON$) &
	\ J9% = PEEK(W%(13%) + SWAP%(W%(14%)) + 4%) AND 255% &
	\ V$ = SYS(PRIV.OFF$) &
	\ CHANGE SYS(CHR.6$+CHR$(-3%)) TO W% &
	\ M%=W%(11%)+SWAP%(W%(12%)) &
	\ RETURN &
	&

29000	! SPAWNED ENTRY POINT. &
	! &
	ON ERROR GOTO 32400 &
	\ CHR.6$=CHR$(6%) &
	\ Q$=SYS(CHR$(7%)) &
	\ I%=ASCII(SYS(CHR.6$+CHR$(9%)))/2% &
	\ LOGIN$=SYS(CHR.6$+CHR$(6%)+CHR$(I%)+MID(Q$,9%,1%)) &
	\ LOGIN$=MID(Q$,7%,2%) &
	\ A%=SWAP%(CVT$%(LOGIN$)) &
	\ GOSUB 15010 &
	\ LOGIN$=SYS(CHR.6$+CHR$(4%)+CVT%$(0%) &
			+MID(SYS(CHR.6$+CHR$(14%)+CVT%$(0%) &
			+CVT%$(0%)+LOGIN$+CHR$(1%)),7%,6%)) &
	\ I$=SYS(CHR.6$+CHR$(-21%)) &
	\ Q$=CHR$(9%)+MID(Q$,3%,4%) &
	\ GOTO 32670 &
		! &
		! GET OUR JOB NUMBER, ATTACH TO KEYBOARD, &
		! GOSUB TO SET PRIORITY, RUN BURST, SWAP MAX, &
		! LOG IN (LOOKING UP PASSWORD), &
		! DROP TEMPORARY PRIVILEGES FOR GOOD. &
		! SET UP Q$ SO THAT EXIT WILL BE TO SPECIFIED RTS, &
		! AND GO EXIT. &
		! &
		! FROM CORE COMMON: &
		!  1-2	RESERVED (SHOULD BE 0). &
		!  3-6	RTS NAME TO EXIT TO (RADIX-50). &
		!  7-8	PPN TO LOG IN UNDER. &
		!    9	KEYBOARD TO ATTACH TO. &
		!   10	RESERVED (SHOULD BE 0). &
	&
	&

30000	!	ENTRY POINT IF "ATTACH" &
	! &
	ON ERROR GOTO 32400 &
	\ GOSUB 22500 &
	\ I$=SYS(CHR$(0%))+SYS(CHR$(2%)) &
	\ OPEN "_KB:LOGIN.CMD" FOR INPUT AS FILE 1% &
	\ LOGIN$=CVT$$(SYS(CHR$(7%)),444%) &
	\ LOGIN$=CVT$$(RIGHT(LOGIN$,FNC%(LOGIN$,"ATTACH",3%)),8%) &
	\ LOGGED.IN%=-1% &
	\ GOTO 6000 &
	&
	&
	&

32000	!	ENTRY POINT IF LINE TYPED AND NOT LOGGED IN &
	! &
	ON ERROR GOTO 32400 &
	\ I$=SYS(CHR$(0%))+SYS(CHR$(2%)) &
	\ GOSUB 22500 &
	\ LOGGED.IN%=0% &
	\ OPEN "_KB:LOGIN.CMD" FOR INPUT AS FILE 1% &

32010	WAIT 1% &
	\ GET #1% &
	\ FIELD #1%, RECOUNT AS LOGIN$ &
	\ LOGIN$=CVT$$(LOGIN$,444%) &
	\ WAIT 0% &
	\ LOGIN$="HELLO" UNLESS LEN(LOGIN$) &
	\ GOSUB 20000 UNLESS (EMS% AND (ASCII(LOGIN$)=5%)) &
	\ A%=0% &
	\ RESTORE &
	\	IF SEND.OPSER% THEN &
			T0$=LOGIN$ &
	\		T0$="EMS Message" IF LOGIN$="" if EMS% &
	\		T0%=INSTR(1%,T0$,";") &
	\		T0$=LEFT(T0$,T0%) IF T0% &
	\		T0%=FNS%("LOGIN invoked logged-out ("+T0$+")") &
	! GET USER INPUT &
	! PUT IT IN LOGIN$ &
	! STRIP OFF THE JUNK AND MAKE IT UPPER CASE &
	! CARRIAGE RETURN IS AS GOOD AS "HELLO" &
		! LOG THE INVOCATION, IF APPROPRIATE. &
	&
	&

32020	READ I$,I%,J% &
	\ TIMEOUT%=17% IF L.CLASS%=1% &
	\ I%=FNC%(LOGIN$,I$,I%) &
	\ GOTO 32020 UNLESS I% &
	\ T0$=LOGIN$ &
	\ LOGIN$=RIGHT(LOGIN$,I%) &
	\ ON J% GOTO 500,6000,3000,32300 &
	&

32200	DATA	HELLO,	5,	1
32210	DATA	LOGIN,	3,	1
32220	DATA	I,	1,	1
32230	DATA	ATTACH,	3,	2
32240	DATA	"",	1,	3
32250	DATA	HELP,	4,	4
32299	DATA	"",	0,	0 &
	&
	&

32300	I$=SYS(PRIV.ON$) &
	\ OPEN "_HELP$:HELP.TXT" FOR INPUT AS FILE 2%, MODE 8192% &
	\ I$=SYS(PRIV.OFF$) &
	\ PRINT FNC$; &

32310	INPUT LINE #2%,I$ &
	\ PRINT I$; &
	\ GOTO 32310 &

32320	CLOSE 2% &
	\ GOTO 32650 &
		! &
		!	HELPER &
	&
	&

32400	! &
	!	E R R O R    H A N D L E R &
	! &
	I$=SYS(CTRL.CTRAP$) &
	\ I$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &
	\ RESUME 32700 IF ERR=27% &
	\ IF ERR=28% THEN &
		IF LINE <> 18000% THEN &
			RESUME 32650 &
		ELSE &
			RESUME 18010 &
		! ?PROGRAMMABLE CTRL/C TRAP &
		!-- RE-ENABLE CTRL/C TRAP AND EXIT &

32410	E%=(ERL/1000%)-9% &
	\ IF E%>0% AND E%<=10% THEN ON E% GOTO &
		10900,11900,12900,13900,14900,15900,16900,17900, &
		18900,19900 &

32420	RESUME 7035 IF ERL=7030% &
	\ RESUME 9900 IF ERL= 9005% &
	\ RESUME 32670 IF ERL=32010% AND ERR=15% &
	\ RESUME 32320 IF ERL=32310% AND ERR=11% &
	\ RESUME 32600 IF ERL=32300% OR ERL=32310% &
	\ RESUME 32670 IF ERL=32650% &
	\ RESUME 32767 IF ERL>=32670% &
	\ RESUME 32650 IF A% &
	\ RESUME 32600 &
	&

32600	LOGIN$="" &
	\ TIMEOUT%=17% &
	\ GOTO 500 &
		! GIVE THE USER A 20 SECOND CHANCE TO TRY AGAIN &
	&

32650	! &
	!	E X I T  S E Q U E N C E &
	! &
	! &
	IF A%=0% THEN &
		I$=SYS(CHR$(2%)) &
	\	I$=SYS(PRIV.ON$) &
	\	I$=SYS(CHR.6$+CHR$(-9%)+CHR$(KB%)+CHR$(TIMEOUT%+3%)) &
	\	I$=SYS(CHR.6$+CHR$(8%)+STRING$(25%,0%)+CHR$(255%)) &
			!-- HANG UP DATASET ON NO LOGIN &
			! KILL THE JOB IF USER HAS FAILED TO LOGIN &
			! THIS WILL PREVENT THE RTS FROM PROMPTING "BYE" &

32670	I$=SYS(CHR$(0%))+SYS(CHR$(2%)) &

32700	CLOSE I% FOR I%=1% TO 12% &
	\ I$=SYS(CHR$(9%)) &

32767	END
