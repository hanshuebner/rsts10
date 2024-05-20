2!		PROGRAM		: SYSDIE.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND		!USE BASIC-PLUS EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1985, 1991 by &
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
	&
	&
	&
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

30	! VER/ED	EDIT DATE	REASON &
	! V2.0-01	24-JUL-85	(PRL) Creation for Micro/RSTS V2.0 &
	! V2.0-02	10-SEP-85	(FRL) Only issue UU.DIE &
	! ================================================================== &
	! V2.1-01	09-Apr-86	(PRL) Make sure all chars output to &
	!				      KB before shutting down &

980	!	D I M E N S I O N   S T A T E M E N T S &
	&
	&
	Dim Firqb%(30) &
		! Define sys call array &

1000	!	M A I N   P R O G R A M &
	&
	&
	On error Goto 19000 &
\	Display% = 0% &
		! Set up standard error trap &
		! Turn off display flag &

1010	I$="V10.1-A" &
		! Set up version/edit numbers &

1020	If Display% then &
		Print "SYSDIE "; I$; "   "; FnError$(0%) &
\		Print &
		! Print the system header if flag set &

1030	Chr.6$ = Chr$(6%) &
		! Define useful constant &

1040	Z$ = SYS(Chr.6$+Chr$(-21%)) &
\	If NOT FnPrv%("ALL") then &
		Print "?SYSDIE requires all privileges" &
\		Goto 32767 &
		! Permanently drop any temp privs &
		! Check if user has all privileges &
		! If not, display error and exit &

1050	Change Sys(Chr$(12%)) to Firqb% &
\	If Firqb%(3%) + Swap%(Firqb%(4%)) <> 15% * 2% then &
		Print "?SYSDIE must be compiled" &
\		Goto 32700 &
		! Get last opened channel info &
		! If not channel 15 (i.e., SYSDIE compiled program), &
		!	Display error message and exit &

1100	Gosub 12500 &
		! Go remove the installed RTS/Resident Libraries &

1200	Kb.Ddb% = Peek(Peek(Peek(520%))) &
\	Sleep 1% until Peek(Kb.Ddb%+10%) = Peek(Kb.Ddb%+12%) &
\	Sleep 2% &
		! Get pointer to our KB's DDB &
		! Sleep until no more chars in output buffer chain: &
		! Wait a few more secs for any xmitted chars to print &

1300	RESTART%=1% &
	\ Z$ = SYS(CHR$(6%) &
		+CHR$(-16%) &
		+CHR$(RESTART%)) &
\	Stop &
		! Shut down the system &
		! Add a useless STOP just for fun &


12500	! &
	! &
	! &
	!	U N L O A D    A N D    R E M O V E    R U N - T I M E &
	! &
	!   S Y S T E M S    A N D    R E S I D E N T    L I B R A R I E S &
	&

12510	CHANGE SYS(CHR$(6%)+CHR$(-12%)) TO FIRQB% &
	\ RTS.LIST% = FIRQB%(15%)+SWAP%(FIRQB%(16%)) &
		! GET THE POINTER TO THE RTS LIST &

12520	  W%=PEEK(PEEK(RTS.LIST%)) &
	\ W%=PEEK(W%) IF W%=PEEK(RTS.LIST%-2%) &
	\ GOTO 12525  IF W%=0% &
	\ GOTO 12530 &
		! GET SECOND ENTRY IN RTS LINKED LIST &
		! IF LIST ENDS THEN GOTO RES LIB REMOVAL &
		! IF THIS IS SYSTEM DEFAULT KBM THEN USE THIRD ENTRY INSTEAD &

12525	RES.LIB%=-1% &
	\ W%=PEEK(RTS.LIST%+2%) &
	\ GOTO 12900 IF W%=0% &
		! SET RES LIB FLAG &
		! GET FIRST ENTRY IN RES.LIB LINKED LIST &
		! IF LIST ENDS THEN EXIT &
	&

12530	  D$ = RAD$(PEEK(W%+2%)) &
	      +RAD$(PEEK(W%+4%)) &
	\ CHANGE SYS(CHR$(6%) &
		    +CHR$(-10%) &
		    +D$)	 TO FIRQB% &
	\ FIRQB%(0%) = 30% &
	\ FIRQB%(1%) =  6% &
	\ FIRQB%(2%) = -18% &
	\ FIRQB%(3%) = 4% &
	\ FIRQB%(3%) =  20% IF RES.LIB% &
	\ FIRQB%(W1%) = 0% 	FOR W1% = 13% TO 18% &
	\ CHANGE FIRQB% TO M$ &
	\ M$ = SYS(M$) &
		! GET RTS/RES LIB SYSTEM NAME.  FILE NAME STRING SCAN NAME &
		! INTO SYS CALL ARRAY.  COMPLETE REST OF ARRAY FOR SYS CALL. &
		! CHANGE TO STRING AND MAKE REMOVE CALL TO SYSTEM &

12550	  GOTO 12520 UNLESS RES.LIB% &
	\ GOTO 12525 &
		! GO BACK FOR MORE IN LIST &
	&

12900	RETURN &

15000	!		F  U  N  C  T  I  O  N  S &
	&
	&
	&
	!	F N P R V % &
	&
	! Check for privilege &
	&
	&
	Def FnPrv%(Priv$) &
\	Change Sys(Chr.6$+Chr$(32%)+Chr$(1%)+String$(3%,0%)+Priv$) &
		to Firqb% &
\	FnPrv% = (Firqb%(3%) = 0%) &
\	Fnend &
	! Check to see if job currently has privilege named &
	! If privileged then return -1% &
	! Else return 0% &

16200	!	F N E R R O R $ &
	&
	! Return error message text &
	&
	&
	Def FnError$(Error%) = &
		Cvt$$(Right(Sys(Chr$(6%)+Chr$(9%)+Chr$(Error%)),3%),4%) &

19000	!	E R R O R   H A N D L E R &
	&
	&
	Error$=FnError$(ERR) &
		! Save error message text &

19020	If ERL = 1300% then &
		Print "?Unable to shut down system" &
\		Print Error$ &
\		Resume 32700 &
		! If error shutting down system, &
		!	Display error message &
		!	And exit &

19999	Print "??Program failure in SYSDIE" &
\	Print Error$; " at line"; Erl &
\	Resume 32700 &
		! Unexpected error &
		! Display the cause and line no. &
		! Resume to end program &

32700	!	E X I T   P R O G R A M &

32767	End
