2!		PROGRAM		: TALK.BAS
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
	! &
	! V9.0-10	15-JAN-85	(PRL) Remove support for <ESC> &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110!	TALK enables users to broadcast messages line by line to another &
   !	terminal. &
   !	&
   !	Both the sending and receiving terminal must be on-line, but &
   !	no user need be logged in on the receiving terminal. &
   !	&
   !	TALK is called as follows: &
   !	&
   !		RUN $TALK &
   !	&
   !	If the receiving terminal is off-line, each line of the message &
   !	is thrown away. &
   !	&
   !	Typing the number of a non-existent terminal does not return &
   !	an error message.  The program is terminated after the cr. &
   !	&
   !	Messages sent to an off-line terminal are broadcast &
   !	normally, but never received. &
   !	&
   !	&
   !	There are no special formats. &
   !	&
	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
310!	   1			_KB:INPUT (INPUT LINE USED TO OBTAIN &
   !					  MESSAGE) &
	&

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME		USED FOR &
   !
410!	   I%			USED TO LOCATE RECEIVING TERMINAL FROM &
   !				USER SPECIFICATION. &
   !	   I$			PROGRAM VERSION. &
   !	   TO.KB%			USED TO IDENTIFY RECEIVING TERMINAL. &
   !	   KB%			OUR KB NUMBER. &
   !				AND LATER TO IDENTIFY SENDING KB. &
   !	   MSG$			USED TO HOLD MESSAGE TO BE SENT. &
   !	   T$			TEMP STRING - TARGET OF SYS CALLS. &
	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
850!	SYS(6,-5)		BROADCAST MESSAGE &
   !	SYS(6,9)		ERROR MESSAGE RETURN &
	&
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	DIM M%(30%) &
	\ ON ERROR GOTO 19000 &
		! dim sys call array &
		! init error handler &
	\ PRIV.ON$ = CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ PRIV.OFF$ = CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ T$=SYS(PRIV.OFF$) &
		! define sys calls for gain/drop temp privs &
		! drop temp privileges &

1010	I$="V10.1-A" &
		! define program version &

1020	PRINT "TALK "; I$; "   "; FNERROR$(0%) &
	\ PRINT &
	\ KB% = ASCII(RIGHT(SYS(CHR$(6%)+CHR$(9%)),2%)) / 2% &
		! print program header &
		! save our kb: number &

1100	PRINT IF CCPOS(0%) &
	\ INPUT "To which keyboard"; K$ &
	\ K$ = CVT$$(K$,-2%) &
	\ GOTO 1100 UNLESS LEN(K$) &
	\ TO.KB% = VAL(K$) &
	\ K$ = "_KB" + NUM1$(TO.KB%) + ":" &

1110	K$ = K$ + ":" UNLESS INSTR(1%,K$,":") &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%) + K$) TO M% &
	\ Z% = M%(29%) + SWAP%(M%(30%)) &
	\ IF Z% >= 0% THEN &
		IF (STATUS AND 255%) = 2% THEN &
			TO.KB% = M%(25%) &
	\		TO.KB% = KB% UNLESS M%(26%) &
	\		GOTO 1160 &

1120	Print "?Invalid keyboard" &
\	GOTO 1100 &
		! display invalid KB message &
		! and re-prompt &

1160	OPEN "_KB:TALK.TXT" FOR INPUT AS FILE 1% &
	\ PRINT "Enter message below (CTRL/Z to end):" &
	\ PRINT &
		! open channel 1 for message input &
		! display prompt &

1190	PREFIX$=" ** KB"+NUM1$(KB%)+" ** " &
		! build message line prefix &

1200	PRINT IF CCPOS(0%) &
	\ PRINT "Message: "; &
	\ INPUT LINE #1%, MSG$ &
		! ensure left margin &
		! display prompt &
		! get next line of message &

1210	T$=SYS(PRIV.ON$) &
	\ T$=SYS(CHR$(6%)+CHR$(-5%)+CHR$(TO.KB%)+PREFIX$+MSG$) &
	\ T$=SYS(PRIV.OFF$) &
	&
	\ PRINT "%_KB"; NUM1$(TO.KB%);": is busy" &
		IF LEN(PREFIX$+MSG$) = RECOUNT &
	\ GOTO 1200 &
	! regain privileges &
	! send message &
	! drop privileges &
	! test to see if terminal is gagged &
	! go get another line &

15000	! &
	! &
	!	F N E R R O R $ ( ERROR.CODE% ) &
	! &
	! &
	DEF FNERROR$ (ERROR.CODE%) = &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERROR.CODE%)),3%),4%) &
		! return error text for error ERROR.CODE% &

19000	! &
	! &
	!	E R R O R   H A N D L I N G &
	! &
	! &
	IF ERR = 11% THEN &
		RESUME 32767 &

19100	IF ERR = 52% THEN &
		IF ERL = 1100% THEN &
			RESUME 1110 &
	! continue if VAL fails on KB string &

19200	IF ERL = 1110% THEN &
		RESUME 1120 &
	! error FSS'ing the KB string &

19999	PRINT "??Program failure in TALK" &
\	PRINT FNERROR$(ERR); " at line"; ERL &
\	RESUME 32767 &
		! unexpected error &
		! display fatal error and exit &

32767	END
