2!		PROGRAM		: GRIPE.BAS
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
	! V9.0-06	22-MAY-84	(KMF) Add priv check &
	! V9.0-07	26-AUG-84	(PRL) Read account name &
	! V9.0-10	15-JAN-85	(PRL) Remove <ESC> support &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	! GRIPE allows users to communicate comments to the system &
	! manager. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
	!	CHANNEL #		USED FOR &
	!	=========		======== &
	!	    1			GRIPE.TXT file &
	!	    2			*LIST output file &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&
	! VARIABLE NAME		USED FOR &
	! &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&
	! FUNCTION/SUBROUTINE		USE &
	! &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&
	DIM #1,N%(1),A$(10000) &
		! Virtual array for GRIPE.TXT file &
	\ DIM M%(30%) &
		! Work array for SYS calls &
	&

1000	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&
	ON ERROR GOTO 19000 &
	\ PRINT IF CCPOS(0%)<>0% &
		! init error handling &
		! return KB to left margin &

1010	I$="V10.1-A" &
		! set up version no. &

1020	PRINT "GRIPE "; I$; "   "; FNERROR$(0%) &
	\ PRINT &
		! print the header &

1030	CHANGE SYS(CHR$(6%)+CHR$(26%)) TO M% &
	\ PROJ% = M%(22%) &
	\ PROG% = M%(21%) &
	\ KB% = M%(4%) &
	\ CR.LF$ = CHR$(13%)+CHR$(10%) &
	\ GRIPE.FIL$ = "AUXLIB$:GRIPE.TXT" &
	\ PRIV.ON$ = CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ PRIV.OFF$ = CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ Z$ = SYS(PRIV.OFF$) &
		! save KB no., proj, prog nos. &
		! build <CR><LF> string &
		! define GRIPE text file &
		! define strings to drop/regain temp privs &
		! drop temp privs for now &

1040	CHANGE SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+ &
		"WACNT") TO M% &
	\ WACNT% = (M%(3%)=0%) &
		! set/clear WACNT priv flag &

1050	Z$ = SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(5%)+ &
		CHR$(PROG%)+CHR$(PROJ%)) &
	\ NAM$ = '"'+CVT$$(MID(Z$,8%,13%),4%)+'"' &
		! get user's account name. &
		! trap error if no name block for account &

1060	OPEN "_KB:GRIPE.CMD" FOR INPUT AS FILE 3% &
		! open user's KB for input &

1070	PRINT "Enter message below (<CTRL/Z> to end):" &
	\ PRINT "(*LIST to list gripes, *RESET to reset file)" &
		IF WACNT% &
	\ PRINT &
	\ MSG$ = "" &
		! display initial prompt &
		! display commands if user has WACNT privilege &
		! skip a line &
		! init null message text &

1100	PRINT IF CCPOS(0%) &
	\ PRINT "Message> "; &
	\ INPUT LINE #3%, TXT$ &
	\ TXT$ = CVT$$(TXT$,4%+128%) &
	\ CMD$ = CVT$$(TXT$,-2%) &
		! ensure left margin &
		! display prompt &
		! get message line or command &
		! strip delimiters and trailing blanks &
		! save mashed current line &

1200	IF CMD$ = "*LIST" THEN &
		IF NOT WACNT% THEN &
			GOTO 1800 &
		ELSE	GOSUB 10000 &
	\		GOTO 32000 IF ERROR% &
	\		GOTO 1100 &
		! if *LIST command, &
		!	if user lacks WACNT privilege, &
		!		skip to display error &
		!	else	go list gripe file &
		!		exit if error &
		!		and re-prompt &

1300	IF CMD$ = "*RESET" THEN &
		IF NOT WACNT% THEN &
			GOTO 1800 &
		ELSE	GOSUB 11000 &
	\		GOTO 32000 IF ERROR% &
	\		GOTO 1100 &
		! if *RESET command, &
		!	if user lacks WACNT privilege, &
		!		skip to display error &
		!	else	go reset gripe file &
		!		exit if error &
		!		and re-prompt &

1400	MSG$ = MSG$ + TXT$ + CR.LF$ &
	\ GOTO 1100 &
		! append text to message buffer &
		! and input next line &

1800	PRINT IF CCPOS(0%) &
	\ PRINT "?WACNT privilege required" &
	\ GOTO 1100 &
		! ensure left margin &
		! display priv violation msg &
		! re-prompt &

2000	CLOSE 1% &
	\ PRINT IF CCPOS(0%) &
	\ GOTO 32000 UNLESS LEN(MSG$) &
	\ MSG$ = FNHEADER$ + MSG$ + CR.LF$ &
	\ WAIT.SECS% = 0% &
		! init wait timer for file access &
		! resume here from CTRL/Z trap &
		! close KB channel &
		! ensure left margin &
		! exit if no message &
		! prefix message with header &
		! add blank line at end of message &

2100	GOSUB 13000 &
	\ GOTO 32000 IF ERROR% &
	\ IF STATUS AND 1024% THEN &
		GOTO 2400 &
		! go open GRIPE file &
		! exit GRIPE if any errors &
		! if no write access to file, &
		!	skip to retry &

2200	N% = N%(0%) + 1% &
	\ L% = LEN(MSG$) + 1% &
	\ IF N% + (L%/16%) > 10000% THEN &
		CLOSE 1% &
	\	PRINT "?GRIPE file is full" &
	\	PRINT "?Please notify system manager" &
	\	GOTO 32000 &
		! compute no recs needed for message &
		! if exceeds max for file, &
		!	close the file &
		!	display error message &
		!	and exit GRIPE &

2300	N%(0%) = N% + (L%/16%) &
	\ A$(I%+N%) = RIGHT(MSG$,I%*16%+1%) &
		FOR I% = 0% TO L%/16% &
	\ CLOSE 1% &
	\ PRINT "Your message has been recorded"; CR.LF$ &
	\ GOTO 32000 &
		! update in-use recs counter &
		! add message text to file &
		!	16 chars per rec &
		! tell the user &
		! and exit GRIPE &

2400	CLOSE 1% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "%GRIPE file in use - please wait..." &
		UNLESS WAIT.SECS% &
	\ WAIT.SECS% = WAIT.SECS% + 5% &
	\ IF WAIT.SECS% > 60% THEN &
		PRINT IF CCPOS(0%) &
	\	PRINT "?Unable to write to GRIPE file" &
	\	PRINT "?Please try again later" &
	\	GOTO 32000 &
		! close the GRIPE file &
		! display wait message &
		!	if 1st time through &
		! add 5 secs to counter &
		! if waited more than a minute, &
		!	display error message &
		!	and exit GRIPE &

2500	SLEEP 5% &
	\ GOTO 2100 &
		! sleep for 5 secs &
		! retry opening file for writing &

10000	! &
	! &
	!	P R O C E S S   * L I S T   C O M M A N D &
	! &
	! &
	GOSUB 13000 &
	\ RETURN IF ERROR% &
	\ NO.RECS% = N%(0%) &
		! go open GRIPE file &
		! exit if any errors &
		! get no. recs to display &

10010	ON ERROR GOTO 10100 &
	\ PRINT IF CCPOS(0%) &
	\ IF NO.RECS% < 0% THEN &
		PRINT "%No GRIPES found" &
	\ 	RETURN &
		! trap local errors &
		! ensure left margin &
		! if no records in file, &
		!	print warning message &
		!	exit &

10020	PRINT "Output to <_KB:GRIPE.LOG>"; &
	\ INPUT LINE OUT$ &
	\ OUT$ = CVT$$(OUT$,-2%) &
	\ OUT$ = "_KB:GRIPE.LOG" &
		UNLESS LEN(OUT$) &
		! prompt for output file &
		! trim response string &
		! use default if null response &

10030	Z$ = SYS(PRIV.OFF$) &
	\ OPEN OUT$ FOR OUTPUT AS FILE 2% &
		! ensure temp privs are off &
		! try to open output file &

10040	IF NO.RECS% > 9000% THEN &
		PRINT "%GRIPE file almost full - please *RESET it." &
		! if more than 90% full, &
		!	display warning message &

10050	PRINT #2%, A$(K%); &
		FOR K%=0% TO NO.RECS% &
		! print text record in file &
		!	for each record &

10060	CLOSE 1%,2% &
	\ ON ERROR GOTO 19000 &
	\ RETURN &
		! close channels &
		! restore standard error trap &
		! exit &

10100	IF ERL = 10020% THEN &
		IF ERR = 11% THEN &
			RESUME 10060 &
		! if error prompting for output file, &
		!	if CTRL/Z detected, &
		!		resume to exit &

10110	IF ERL = 10030% THEN &
		PRINT "?Unable to open output file "; OUT$ &
	\	PRINT FNERROR$(ERR) &
	\	RESUME 10020 &
		! if error opening output file, &
		!	print error message &
		!	resume to reprompt &

10999	GOTO 19000 &
		! unexpected error - skip to standard error trap &

11000	! &
	! &
	!	P R O C E S S   * R E S E T   C O M M A N D &
	! &
	! &
	ON ERROR GOTO 11100 &
	\ Z$ = SYS(PRIV.ON$) &
	\ KILL GRIPE.FIL$ &
	\ Z$ = SYS(PRIV.OFF$) &
		! trap local errors &
		! gain temp privs &
		! delete the gripe file &

11010	GOSUB 12000 &
	\ CLOSE 1% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "GRIPE file reset" &
	\ ERROR% = 0% &
		! go create GRIPE file &
		! close it &
		! ensure left margin &
		! say it's reset &
		! show reset succeeded &

11020	Z$ = SYS(PRIV.OFF$) &
	\ ON ERROR GOTO 19000 &
	\ RETURN &
		! ensure temp privs dropped &
		! reset error handler &
		! exit &

11100	IF ERR = 5% THEN &
		RESUME 11010 &
		! if GRIPE file doesn't exist, &
		!	resume to create it &

11999	PRINT IF CCPOS(0%) &
	\ PRINT "?Unable to reset GRIPE file" &
	\ PRINT FNERROR$(ERR) &
	\ ERROR% = -1% &
	\ RESUME 11020 &
		! ensure left margin &
		! print error message &
		! show reset failed &
		! resume to exit &

12000	! &
	! &
	!	C R E A T E   G R I P E   F I L E &
	! &
	! &
	ON ERROR GOTO 12100 &
	\ Z$ = SYS(PRIV.ON$) &
	\ OPEN GRIPE.FIL$+"<188>" FOR OUTPUT AS FILE 1% &
	\ ERROR% = 0% &
	\ N%(0%)=-1% &
		! trap local errors &
		! gain temp privs &
		! create GRIPE file &
		! show create succeeded &
		! init in-use recs counter &

12010	Z$ = SYS(PRIV.OFF$) &
	\ ON ERROR GOTO 19000 &
	\ RETURN &
		! drop temp privs &
		! restore standard error trap &
		! exit &

12100	PRINT IF CCPOS(0%) &
	\ PRINT "?Unable to create GRIPE file" &
	\ PRINT FNERROR$(ERR) &
	\ ERROR% = -1% &
	\ RESUME 12010 &
		! ensure left margin &
		! display error msg &
		! show create failed &
		! resume to exit &

13000	! &
	! &
	!	O P E N   G R I P E   T E X T   F I L E &
	! &
	! &
	ON ERROR GOTO 13100 &
	\ Z$ = SYS(PRIV.ON$) &
	\ OPEN GRIPE.FIL$ FOR INPUT AS FILE 1% &
	\ ERROR% = 0% &
	\ GOTO 13020 &
		! trap local errors &
		! gain temp privs &
		! try to open GRIPE file &
		! show success &
		! skip to exit &

13010	GOSUB 12000 &
		! go create GRIPE file &

13020	Z$ = SYS(PRIV.OFF$) &
	\ ON ERROR GOTO 19000 &
	\ RETURN &
		! drop temp privs &
		! restore standard error trap &
		! exit &

13100	IF ERR = 5% THEN &
		RESUME 13010 &
		! if file doesn't exist, &
		!	resume to create it &

13999	PRINT IF CCPOS(0%) &
	\ PRINT "?Unable to open GRIPE file" &
	\ PRINT FNERROR$(ERR) &
	\ ERROR% = -1% &
	\ RESUME 13020 &
		! unexpected error &
		! display error msg &
		! show open failed &
		! resume to exit &

15000	! &
	! &
	!	F N E R R O R $ ( ERROR.CODE% ) &
	! &
	! &
	DEF FNERROR$ (ERROR.CODE%) = &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERROR.CODE%)),3%),4%) &
		! return error text for error ERROR.CODE% &

16000	! &
	! &
	!	F N H E A D E R $ &
	! &
	! &
	DEF FNHEADER$() = &
		CR.LF$ + "*** From user " + &
		"[" + NUM1$(PROJ%) + "," + NUM1$(PROG%) + "] " + &
		NAM$ + " on KB" + NUM1$(KB%) + ":  " + DATE$(0%) + &
		" " + TIME$(0%) + " ***" + CR.LF$ + CR.LF$ &
		! build message header text &

19000	! &
	! &
	!	E R R O R   H A N D L I N G &
	! &
	! &
	IF ERL = 1050% THEN &
		NAM$ = "(no name)" &
	\	RESUME 1060 &
		! if error reading account name block, &
		!	use 'noname' &
		!	resume to contine &

19010	IF ERR = 11% THEN &
		IF ERL = 1100% THEN &
			RESUME 2000 &
		! if CTRL/Z, &
		!	if prompt for gripe text, &
		!		resume to exit prompt loop &

19999	PRINT IF CCPOS(0%) &
	\ PRINT "??Program failure in GRIPE" &
	\ PRINT FNERROR$(ERR); " at line"; ERL &
	\ RESUME 32000 &
		! unexpected error &
		! display fatal error msg &
		! resume to exit &

32000	! &
	! &
	!	E X I T   G R I P E &
	! &
	! &
	CLOSE 1%,2%,3% &
		! close any open channels &

32767	END
