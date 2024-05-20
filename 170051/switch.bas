2!		PROGRAM		: SWITCH.BAS
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
  !		      Copyright (C) 1977, 1991 by &
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
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

101	!	CCL COMMAND TO CHANGE JOB'S DEFAULT RUN TIME SYSTEM &
	! &
	!		NEW SWITCH &
	! &
	!		COMPILE$SWITCH &
	! &
	!		NAME "$SWITCH.BAC" AS "$SWITCH.BAC<232>" &
	! &
	!		RUN$UTILTY &
	!		#CCL SW-ITCH=$SWITCH.BAC;PRIV 30000 &
	!		#EXIT &
	! &
	!		TO USE TYPE SW OR SWITCH FOLLOWED BY THE RTS NAME. &
	! &
	!		EX:	SW RT11 &
	! &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301	! CHANNEL #		USED FOR &
	&

400	! &
	! &
	!	V A R I A B L E    D E F I N I T I O N S &
	! &

401	! &
	!	RTS.FSS$	REQUESTED RTS NAME IN RAD-50. &
	!	RTS.PTR%	POINTER TO NEXT RTS IN LIST. &
	!	KBM.PTR%	POINTER TO SYSTEM DEFAULT KBM &
	!	RTSLST%		ADDR OF POINTER TO FIRST RUNTIME SYSTEM &
	!	DEFKBM%		ADDR OF POINTER TO DEFAULT KBM &
	!	TEMP$		TEMPORARY VARIABLE. &
	!	USER.RTS$	REQUESTED RTS NAME IN ASCII. &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
899!	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 32700 &
	\ PRINT IF CCPOS(0%) &

2000	INPUT "Keyboard Monitor to switch to"; USER.RTS$ &
	\ USER.RTS$=CVT$$(USER.RTS$,-2%) &
	! ENABLE ERROR TRAPPING. &
	! RESTORE CARRIAGE IF NEEDED. &
	! OBTAIN THE DESIRED RUN-TIME SYSTEM NAME. &
	! SQUISH OUT ANY GARBAGE CHARACTER(S). &

3000	RTSLST%=SWAP%(CVT$%(MID(SYS(CHR$(6%)+CHR$(-12%)),15%,2%))) &
	\ DEFKBM%=RTSLST%-2% &
	\ RTS.PTR%=PEEK(RTSLST%) &
	\ KBM.PTR%=PEEK(DEFKBM%) &
	\ USER.RTS$=CVT$$(RAD$(PEEK(KBM.PTR%+2%)) &
	            +RAD$(PEEK(KBM.PTR%+4%)),-2%) IF LEN(USER.RTS$)=0% &
	\ RTS.FSS$=MID(SYS(CHR$(6%)+CHR$(-10%)+USER.RTS$),7%,4%) &
	\ USER.RTS$=RAD$(SWAP%(CVT$%(LEFT(RTS.FSS$,2%)))) &
		   +RAD$(SWAP%(CVT$%(RIGHT(RTS.FSS$,3%)))) &
	! GET THE MONITOR ADDRESS OF RUN-TIME SYSTEM LIST ('RTSLST'). &
	! USE THE ARGUMENT IF THERE IS ONE. &
	! ELSE USE THE NAME OF THE SYSTEM DEFAULT KBM &
	! CONVERT SYSTEM DEFAULT RUN-TIME SYSTEM NAME TO STRING. &
	! CHANGE RUN-TIME SYSTEM NAME TO 4 BYTES OF RAD50. &
	! RESET RTS NAME SPECIFIED TO EXACTLY 6 ASCII CHARACTERS. &

3010	UNTIL USER.RTS$=RAD$(PEEK(RTS.PTR%+2%))+RAD$(PEEK(RTS.PTR%+4%)) &
	\	IF RTS.PTR%=0% THEN &
			PRINT "?No Keyboard Monitor "+USER.RTS$ &
	\		GOTO 32767 &
	!		PRINT ERROR MESSAGE IF RUN-TIME SYSTEM NOT FOUND &

3020		RTS.PTR%=PEEK(RTS.PTR%) &
	\ NEXT &
	! CHECK NEXT RUN-TIME SYSTEM IN LIST &

3030	IF (PEEK(RTS.PTR%+30%) AND 256%) = 0% THEN &
		PRINT "?No Keyboard Monitor in "+USER.RTS$ &
	\	GOTO 32767 &
	! PRINT ERROR MESSAGE IF RTS DOESN'T HAVE KEYBOARD MONITOR. &

9010	TEMP$=SYS(CHR$(9%)+RTS.FSS$) &
	\ STOP &
	! NOW SWITCH TO DESIRED RUN-TIME SYSTEM &
	!	ESTABLISHING IT AS THE JOB'S PRIVATE DEFAULT. &
	! NOTE: THE "STOP" IS NEVER REACHED... &

29999	! &
	&
	&
	!	C C L   E N T R Y   P O I N T &
	&

30000	ON ERROR GOTO 32700 &
	\ PRINT IF CCPOS(0%) &
	\ USER.RTS$=CVT$$(RIGHT(SYS(CHR$(7%)),7%),-2%) &
	\ GOTO 3000 &
	! ENABLE ERROR TRAPPING. &
	! RESTORE CARRIAGE IF NEEDED. &
	! GET THE CCL COMMAND ARGUMENT. &
	! GO PROCESS IT. &

32700	IF ERR=11% AND ERL=2000% THEN &
		RESUME 32767 &
	! JUST EXIT IF USER TYPES CTRL/Z. &

32710	IF ERL=3000% THEN &
		PRINT "?Illegal Keyboard Monitor name" &
		\ RESUME 32767 &
	! TRAP ILLEGAL RUN-TIME SYSTEM NAMES. &

32720	ON ERROR GOTO 0 &
	! DIE ON OTHER ERROR(S). &

32767	END
