2!		PROGRAM		: HELP.BAS
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
	! V9.0-13	20-Mar-85	(DS)  Add DCL entry point (31500) &
	! V9.0-14	13-Apr-85	(PRL) Fix to permit @ as topic &
	!				      Change master file to DCL.HLP &
	!				      Fix "Sorry, no help" message &
	!				      Use "HELP.HLP" as master file &
	! V9.6-09	21-Mar-88	(JJT) Make CCL entry same as DCL &
	! V9.7-03	28-Dec-88	(JHC) add CHAIN entry (GOTO CCL) &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110	!	HELP is a system utility that provides help messages &
	!	for users. &
	! &
	!	HELP is a data driven utility.  It initially opens &
	!	a master HELP file and all other HELP files are &
	!	found by indirects from the master HELP file. &
	! &
	!	All HELP file references are defaulted as: &
	!	   package's dev:[ppn]	if no explicit device name or ppn &
	!	   .HLP			if no explicit extension &
	!	The initial setting of the "package location" is where &
	!	HELP itself is located (almost always $).  An indirect &
	!	file specification that has an explicit device name &
	!	or ppn (possibly derived from a logical name) causes &
	!	the "package location" to be changed to that device/ppn &
	!	combination for all further levels of indirect file &
	!	references.  When HELP backs out of that level of indirect &
	!	file(s), the "package location" is restored to what it &
	!	was.  The master HELP file is HELP which must be located &
	!	where HELP is and named HELP.HLP. &
	! &
	!	HELP files can be edited freely by the system manager. &
	! &
	!	WARNING: Do NOT use TAB's to format your help information. &
	!	The help information is indented by 2 spaces for every &
	!	nested level which throws off TAB stops. &
	! &
	!	Control characters recognized by HELP are: &
	!		Line type		Line's first character &
	!		   Comment		   ! &
	!		   Keyword		   * &
	!		   Switch		   / &
	!		   Indirect file	   @ &
	!		   Text			   other &
	! &
	!	Comments may be used anywhere; they are totally ignored. &
	! &
	!	There are three (3) parts to a HELP file. &
	!		1) Text describing the command (required). &
	!		2) A series of "Switch blocks" (see below) &
	!		   describing switches that always apply to &
	!		   the command (optional). &
	!		3) A series of "Keyword blocks" (see below) &
	!		   describing options to the command (optional, &
	!		   but almost always present). &
	!	As the switches that always apply to the command (the &
	!	"global switches") can be expressed in one of three (3) &
	!	ways there are four (4) formats for a HELP file. &
	! &
	!	Format of a HELP file without global switches: &
	!		1) Text describing the command. &
	!		2) An optional series of "Keyword blocks" &
	!		   describing options to the command. &
	! &
	!	Format of a HELP file with leading global switches: &
	!		1) Text describing the command. &
	!		2) One or more "Switch blocks" for the global &
	!		   switches. &
	!		3) An optional series of "Keyword blocks" &
	!		   describing options to the command. &
	! &
	!	Format of a HELP file with trailing global switches: &
	!		1) Text describing the command. &
	!		2) An optional series of "Keyword blocks" &
	!		   describing options to the command. &
	!		3) One or more "Switch blocks" for the global &
	!		   switches.  Note that the keyword block &
	!		   immediately preceeding these switch blocks &
	!		   must use an indirect file specification to &
	!		   distinguish these switch blocks as global &
	!		   switches. &
	! &
	!	Format of a HELP file with keyworded global switches: &
	!		1) Text describing the command. &
	!		2) An optional series of "Keyword blocks" &
	!		   describing options to the command. &
	!		3) A single keyword line (almost always "*Switches") &
	!		   announcing the global switches immediately &
	!		   followed by one or more "Switch blocks" for &
	!		   the global switches. &
	! &
	!	The format of a "Keyword block" is: &
	!		1) Start of "Keyword block" flag.  This is one or &
	!		   more line(s) starting with a "*" immediately &
	!		   followed by the keyword(s).  For example: &
	!			*DELETE &
	!		       --or-- &
	!			*RESUME CTRL/C &
	!			*RESUME ANY &
	!		2) Either &
	!		     a)	An indirect file specification (see below). &
	!		   --or-- &
	!		     b)	Text describing the option optionally &
	!			followed by one or more "Switch blocks" &
	!			describing switches to the option. &
	! &
	!	The format of a "Switch block" is: &
	!		1) Start of "Switch block" flag.  This is one or &
	!		   more line(s) starting with a "/" immediately &
	!		   followed by the switch(es).  For example: &
	!			/DELETE &
	!		       --or-- &
	!			/LOG &
	!			/NOLOG &
	!		2) Either &
	!		     a)	An indirect file specification. &
	!		   --or-- &
	!		     b)	Text describing the switch. &
	! &
	!	A keyword block line (*XXX) or switch line (/XXX) is &
	!	available to all but logged out users.  To make a line &
	!	also available to logged out users, a "`", CHR$(96%), &
	!	should be added immediately after the "*" or "/" &
	!	(e.g., *`XXX).  To restrict a line to only logged in &
	!	privileged users, a "~", CHR$(126%), should be added &
	!	immediately after the "*" or "/" (e.g., *~XXX). &
	! &
	!	To allow for common spelling errors, "hidden" keyword and &
	!	switch lines can be used.  A hidden line has a "|", &
	!	CHR$(124%), immediately after the "*" or "/" (or the &
	!	"*~", etc.).  Hidden lines should immediately follow &
	!	the real line they correspond to.  For example, &
	!		*`HELP &
	!		*`|HEPL &
	!	to allow HEPL as a common mistake when typing HELP. &
	!	Hidden lines can also be used for synonyms.  For example, &
	!		*FUNCTION 0 &
	!		*|0 &
	!	to allow simply 0 as well as FUNCTION 0.  Hidden lines can &
	!	also be used for keywords or switches that can be matched &
	!	upon, but that shouldn't appear in a summary for one reason &
	!	or another.  For example, if you have a HELP file about &
	!	numeric error messages, it could be formatted as: &
	!		Numeric error message listing. &
	!		*Type the numeric error code you want &
	! &
	!		*|0 &
	!		Text for error message 0... &
	!		*|1 &
	!		Text for error message 1... &
	!	The blank line just after the "*Type ..." line is &
	!	important; makes the "*|0" not a synonym for anything. &
	!	Hidden lines are never listed in the information available &
	!	summaries.  If a hidden line is matched and the previous &
	!	non-hidden line is of the correct type (i.e., a keyword &
	!	or switch) then that non-hidden line is assumed to be &
	!	the correctly spelled or main line and a message about &
	!	it is printed. &
	! &
	!	To speed things up a bit, HELP will stop scanning a file &
	!	if it gets an exact match and there is no more command &
	!	line to match at the next level.  For example, if your &
	!	HELP file has both "D" and "DATE" as keywords, then if &
	!	the "D" comes before the "DATE" then a D will only match &
	!	the "D"; DA, DAT, or DATE will match only "DATE".  On the &
	!	other hand, if the "DATE" comes before the "D" then a D &
	!	will match both "DATE" and "D".  Another case involves &
	!	multi-word keywords.  For example, if your HELP file has &
	!	both "ESC" and "ESC SEQ", then if the "ESC" comes before &
	!	the "ESC SEQ" then a ESC will only match the "ESC" and &
	!	a ESC SEQ will attempt to find out about SEQ in the next &
	!	level of the "ESC" as well as match the "ESC SEQ". &
	!	Obviously, it would be better to place the "ESC" after &
	!	the "ESC SEQ".  This results in a ESC matching both the &
	!	"ESC SEQ" and then the "ESC" and a ESC SEQ matching the &
	!	"ESC SEQ" and terminating. &
	! &
	!	An indirect file specification is of the form: &
	!		@file-specification &
	!	As described above, the file specification has a default &
	!	extension of .HLP and will use the current "package location" &
	!	in the absense of an explicit device name or ppn.  If an &
	!	explicit device name or ppn is specified, the "package &
	!	location" to changed to that device/ppn combination for &
	!	further indirect file references.  This method allows all &
	!	of the HELP files for a given set of utilities to be located &
	!	in the same account (usually the same account as the utilities &
	!	themselves).  The master HELP file has one (or more) new &
	!	"Keyword block(s)" added to it corresponding to the utilities' &
	!	command name(s).  Each "Keyword block" uses an indirect file &
	!	specification that explicitly specifys the account of the &
	!	utilities.  The HELP file(s) in the utilities' account can &
	!	reference their neighbors by simply using "@_file". (Note &
	!	that "@_file" is used by convention instead of just "@file" &
	!	to explicitly indicate that no device/ppn is being used.) &
	! &
	!	The leading text in the master HELP file describes HELP &
	!	itself.  The Keywords in the master HELP file are the &
	!	commands that HELP can talk about.  The global switches &
	!	are the switches that apply to the HELP command itself &
	!	(e.g., direct HELP's output to a file). &
	! &
	!	Help can be obtained on a particular topic by typing: &
	!		HELP  topic  subtopic  subsubtopic  ... &
	!	A topic can have the following format: &
	!	 1) an alphanumeric string (e.g., command name, option, etc.) &
	!	 2) same preceded by a "/" (=> interpreted as a switch) &
	!	 3) the match-all symbol "*" &
	!	Examples: &
	!	    HELP DIRECTORY /S &
	!	    HELP SET LC &
	!	Abbreviations result in all matches being displayed. &
	! &
	!	HELP may be run by users logged into the system (RUN or &
	!	CCL entry) or by users not logged into the system (CHAIN &
	!	entry from LOGIN). &
	! &
	!	The RUN entry will prompt "Topic?" and output will always &
	!	be to the user's terminal.  Further prompting will &
	!	utilize the last command line entered and ask "Subtopic?". &
	!	A CTRL/Z response to the "Subtopic?" question will revert &
	!	to the "Topic?" question; a CTRL/Z to the "Topic?" &
	!	question will exit from HELP. &
	! &
	!	The CCL entry can be used in one or three (3) ways: &
	!		1) HELP command-line &
	!			Output is to the user's terminal.  If &
	!			there is no command-line, HELP lists its &
	!			command line format and all available &
	!			topics then enters prompting mode just &
	!			like the RUN entry.  Else, HELP lists &
	!			the specified information and exits. &
	!		2) HELP/O[UTPUT]:file-name command-line &
	!			Output is to the specified file with &
	!			a default file extension of ".LST". &
	!			HELP exits upon command completion. &
	!		3) HELP/P[ROMPT] command-line &
	!			Output is to the user's terminal.  If &
	!			there is no command-line then HELP &
	!			immediately enters prompting mode just &
	!			like the RUN entry.  Else, HELP enters &
	!			prompting mode after doing the command. &
	!		4) HELP/C[HAIN]:"file-name;line;core" command-line &
	!			Output is to the user's terminal.  Help &
	! 			chains to the specified file after command &
	!			completion. &
	! &
	!	The logged out entry is like the CCL entry, but HELP's &
	!	output cannot be directed to a file. &
	! &
	!	HELP is a privileged program, but immediately drops its &
	!	privileges upon entry. &
	! &
	!	HELP.BAS should be compiled into the system library &
	!	account ($) with the privileged protection code <232>. &
	!	HELP.HLP should also be placed in the system library &
	!	and should be protected <40>  Further HELP files should &
	!	also be protected at <40> &
	! &
	!	The HELP CCL command is: &
	!		CCL HE-LP=$HELP.*;PRIV 30000 &
	!	(The only conflicting CCL is HELLO which must be installed &
	!	 before HELP with the abbreviation set as HELL-O.) &
	! &
	!	HELP's logged out entry point is line 32000; &
	! &
	!	HELP's chain entry point is line 31000, but it does very &
	!	little besides GOTO the CCL entry point.  There &
	!	is also a /CHAIN switch that may be used to chain to other &
	!	programs. &

301!	CHANNEL #		USED FOR &
   !	  1			OUTPUT IF FILESPEC GIVEN &
   !	  2			MASTER HELP.HLP FILE &
   !	3-12			INDIRECTS FROM MASTER HELP.HLP FILE &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&

401!	VARIABLE NAME	USED FOR &
   !
410!	COMMAND$		CURRENT ACTIVE COMMAND LINE &
   !	COMMAND$()		SAVED COMMAND LINE FROM LEVEL X &
   !	CHANNEL%		CURRENT ACTIVE CHANNEL NUMBER &
   !	CHANNEL%()		SAVED STARTING CHANNEL NUMBER FROM LEVEL X &
   !	COM.PROMPT$		BUILT-UP COMMAND LINE FOR PROMPTING &
   !	CHAI%		FLAG TO INDICATE A /CHAIN SWITCH WAS SPECIFIED &
   !	CHAI.FILE$	NAME OF FILE TO CHAIN TO &
   !	CORE$		CONTENTS OF CORE-COMMON WHEN HELP CHAINS &
   !	DELIM$		DELIMITER (USED IN PARSER) &
   !	DELIM%		POSITION OF DELIMITER (USED IN PARSER) &
   !	E$		ERROR MESSAGE &
   !	E%		ERROR VALUE (FROM ERR) &
   !	E0%		LOGICAL EOF CODE &
   !	E0%()		SAVED LOGICAL EOF CODE FORM LEVEL X &
   !	EOS%		POINTER TO END OF /CHAIN SWITCH &
   !	H%		KEYWORD/SWITCH LINE IS A HIDDEN LINE &
   !	I$		IDENT STRING &
   !	I%		INDENT LEVEL &
   !	K$		KEYWORD/SWITCH TO MATCH &
   !	K$()		ACCUMULATED KEYWORDS/SWITCHES DURING SCAN &
   !	K%		NUMBER OF ACCUMULATED KEYWORDS/SWITCHES &
   !	K%()		ACCUMULATED PREVIOUS LINE'S CODES FOR FOUND &
   !			KEYWORDS/SWITCHES &
   !	L$		CURRENT LINE &
   !	L%		CURRENT LINE'S CODE &
   !	L0%		MAIN MATCH CODE &
   !	L1%		SECONDARY MATCH CODE &
   !	L1%()		SAVED SECONDARY MATCH CODE FROM LEVEL X &
   !	L9%		LAST LINE'S CODE &
   !	LN%		LINE NUMBER TO CHAIN TO &
   !	M%()		KEYWORD/SWITCH MATCH INDICATOR FROM LEVEL X &
   !	NEXT.SEMI%	POSITION OF SECOND SEMICOLON IN /CHAIN SWITCH &
   !	O$		OUTPUT FILE SPECIFICATION &
   !	O%		OUTPUT CHANNEL NUMBER &
   !	OUT%		FLAG TO INDICATE /OUTPUT SWITCH WAS SPECIFIED &
   !	P$		CURRENT PACKAGE LOCATION &
   !	P$()		SAVED PACKAGE LOCATION FROM LEVEL X &
   !	P%		"SOMETHING WAS PRINTED" FLAG &
   !	P0$		DEFAULT PACKAGE LOCATION &
   !	PARSE$		LINE TO PARSE &
   !	PARSE.CHAR$	CHARACTER THAT PARSE.POS% POINTS TO &
   !	PARSE.POS%	POSITION OF POINTER FOR PARSE$ &
   !	PRIV.OFF$ 	SYS CALL STRING TO TEMPORARILY DROP PRIVILEGES &
   !	PRIV.ON$ 	SYS CALL STRING TO REGAIN TEMP. DROPPED PRIVILEGES &
   !	PROMPT%		FLAG TO INDICATE THAT /PROMPT WAS SPECIFIED &
   !	QUO%		FLAG TO INDICATE CURRENT TEXT IS IN " " &
   !	SEMI%		POSITION OF FIRST SEMICOLON IN /CHAIN SWITCH &
   !	SW$		CURRENT SWITCH BEING PARSED &
   !	T$		GENERAL SCRATCH USAGE &
   !	T%		GENERAL SCRATCH USAGE &
   !	T%()		GENERAL SCRATCH USAGE &
   !	T0%		LAST ITEM'S CODE FOR COLUMN PRINTING, ETC. &
   !	TRAP$		SYS CALL STRING TO TRAP CONTROL/C &
   !	V$		OLD COMMAND LINE FOR PROMPTING &
   !	V%		"ADDITIONAL INFORMATION AVAILABLE" FLAG &
   !	VALID.SWITCHES$	LIST OF VALID SWITCHES &
   !	W%		LINE WIDTH &
   !	W0%		INPUT WAIT TIMEOUT VALUE &
   !	Z%		DUMMY ARGUMENT FOR FUNCTIONS &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE	USE &
   !	10100			PACKAGE INITIALIZATION &
   !	10200			READ A LINE FROM A HELP FILE &
   !	10300			HANDLE INDIRECT FILES &
   !	10400			FIND NEXT KEYWORD/SWITCH LINE &
   !	10500			PRINT COLUMN INFORMATION &
   !	11000			RECURSIVE KEYWORD/SWITCH SCANNER &
   !		11000			SET UP &
   !		12000			KEYWORD/SWITCH SCANNING &
   !		13000			INFORMATION PRINTING &
   !		14000			CLEAN UP &
   !	FNE$			FETCH A RSTS/E ERROR MESSAGE &
   !	FNCHK$			CHECK TO SEE IF SWITCH IS VALID &
   !	FNDSPLY%		PRINT HELP TEXT TO OUTPUT FILE &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

910	DIM T%(30) &
	! T%()		GENERAL MONITOR SYS() CALL USAGE &

920	DIM C0MMAND$(10%), P$(10%), CHANNEL%(10%), E0%(10%), &
		L1%(10%), M%(10%) &
	! COMMAND$(X)		SAVED COMMAND FROM LEVEL X &
	! P$(X)		SAVED PACKAGE LOCATION FROM LEVEL X &
	! CHANNEL%(X)		SAVED CHANNEL NUMBER FROM LEVEL X &
	! E0%(X)	SAVED LOGICAL EOF CODE FROM LEVEL X &
	! L1%(X)	SAVED SECONDARY MATCH CODE FROM LEVEL X &
	! M%(X)		SAVED MATCH INDICATOR FROM LEVEL X &

930	DIM K$(300%), K%(300%) &
	! K$()		ACCUMULATED KEYWORDS/SWITCHES DURING MATCH SCAN &
	! K%()		ACCUMULATED PREVIOUS CODES FOR FOUND KEYWORDS/SWITCHES &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ T$=SYS(CHR$(0%)) &
	\ PRINT IF CCPOS(0%) &
	! SET UP STANDARD ERROR TRAP. &
	! CANCEL CTRL/O. &
	! ENSURE CARRIAGE AT LEFT MARGIN. &

1010	I$='V10.1-A' &
	! SET UP V/E. &

1020	PRINT "HELP "; I$; "   "; FNE$(0%) &
	\ GOSUB 10100 &
	! PRINT THE SYSTEM HEADER. &
	! INITIALIZE PACKAGE (DOES PRIV.OFF$). &

1030	GOSUB 2000 &
	\ CLOSE #O% IF O% &
	\ GOTO 31200 &
	! CALL FOR INITIAL PROMPTING. &
	! CLOSE OUTPUT FILE IF THERE'S ONE. &
	! EXIT. &
	&

2000	!	ENTRY TO DO INITIAL PROMPTING &

2010	T$=SYS(CHR$(0%)) &
	\ RETURN IF E1%=5% &
	\ PRINT &
	\ LINE.COUNT%=0% &
	\ PRINT "Topic"; &
	\ INPUT LINE COMMAND$ &
	\ PRINT IF CCPOS(0%) &
	\ COMMAND$=CVT$$(COMMAND$,4%+8%+16%+32%+128%) &
	\ V$="" &
	\ RETURN UNLESS LEN(COMMAND$) &
	! CANCEL CTRL/O. &
	! ASK FOR A TOPIC FROM USER (WITH TIMEOUT IF LOGGED OUT). &
	! RESTORE CARRIAGE IF NEEDED. &
	! TRIM USER'S ANSWER. &
	! SET OLD COMMAND LINE TO NULL. &
	! RETURN AND QUIT IF COMMAND$="" &
	&

3000	!	ENTRY TO DO PROMPTING AFTER FIRST COMMAND DECODE &
	GOSUB 4000 &
	\ V$=COM.PROMPT$ IF V% IF COM.PROMPT$<>"" &

3010	GOTO 2010 IF V$="" OR P%=0% &
	\ T$=SYS(CHR$(0%)) &
	\ PRINT &
	\ LINE.COUNT%=0% &
	\ PRINT V$; " Subtopic"; &
	\ INPUT LINE COMMAND$ &
	\ PRINT IF CCPOS(0%) &
	\ T%=-1% &
	\ V$=CVT$$(V$,4%+8%+16%+32%+128%) &
	\ COMMAND$=CVT$$(COMMAND$,4%+8%+16%+32%+128%) &
	\ IF LEN(COMMAND$) THEN &
		COMMAND$=V$+" "+COMMAND$ &
	\	GOTO 3000 &
	! CALL FOR A COMMAND DECODE. &
	! UPDATE OLD COMMAND IF ADDITIONAL INFO TO BE FOUND AND IF WAS UNIQUE. &
	! JUST RE-ASK "TOPIC" IF NO COMMAND LINE OR NO INFO AT ALL. &
	! CANCEL CTRL/O. &
	! PRINT OLD COMMAND AND PROMPT FOR A SUBTOPIC. &
	! GET USER'S RESPONSE (WITH TIMEOUT IF LOGGED OUT). &
	! RESTORE CARRIAGE IF NEEDED. &
	! BUILD THE NEW COMMAND LINE. &
	! GO PROCESS NEW COMMAND. &
	&

3020	TEMP%=T% &
	\ T%=INSTR(T%+1%,V$," ") &
	\ GOTO 3020 IF T% &
	\ COMMAND$=LEFT(V$,TEMP%-1%) IF TEMP% <> -1% &
	\ V$=COMMAND$ &
	\ GOTO 3010 &
	&
	&

4000	!	ENTRY TO DECODE THE COMMAND &

4010	IF LEN(O$) AND O%=0% THEN &
		O%=1% &
	\	OPEN O$ FOR OUTPUT AS FILE #O% &
	! IF THERE'S AN OUTPUT FILE AND IT'S NOT ALREADY OPEN THEN &
	!	REMEMBER THAT WE HAVE AN OPEN OUTPUT FILE. &
	!	CREATE THE OUTPUT FILE. &

4020	T%=-1% &
	! INITIALIZE FOR MAKING "FOO/BAR" INTO "FOO /BAR". &

4030	T%=INSTR(T%+2%,COMMAND$,"/") &
	\ IF T% THEN &
		COMMAND$=LEFT(COMMAND$,T%-1%)+" "+RIGHT(COMMAND$,T%) &
	\	GOTO 4030 &
	! SCAN FOR THE NEXT "/". &
	! IF ONE IS FOUND THEN &
	!	MAKE THE "/" INTO " /". &
	!	LOOP. &

4040	COMMAND$=CVT$$(COMMAND$,4%+8%+16%+32%+128%) &
	\ P$=P0$ &
	\ CHANNEL%=1% &
	\ E%=0% &
	\ L$="@"+MASTER$ &
	\ L%=ASCII(L$) &
	! RE-SQUISH THE COMMAND STRING TO REMOVE MULTIPLE SPACES. &
	! PRESET DEFAULT PACKAGE LOCATION. &
	! PRESET CHANNEL NUMBER FOR MASTER HELP FILE. &
	! PRESET LAST LINE AS AN INDIRECT TO OPEN THE MASTER FILE. &

4050	P%=0% &
	\ V%=0% &
	\ COM.PROMPT$="" &
	\ I%=0% &
	\ GOSUB 11000 &
	\ TMP%=FNDSPLY%("",-1%) &
	\ RETURN &
	! SET SOMETHING PRINTED TO OFF INITIALLY (P%). &
	! SET MORE INFORMATION AVAILABLE TO OFF INITIALLY (V%). &
	! SET BUILT-UP COMMAND LINE TO NULL INITIALLY (COM.PROMPT$). &
	! SET INDENT LEVEL TO ZERO. &
	! CALL THE KEYWORD/SWITCH FINDER. &
	! RESTORE CARRIAGE IF NEEDED. &
	! RETURN TO CALLER. &

10000	! &
	&
	&
	!	S U B R O U T I N E S &
	&
	&

10100	! &
	&
	!	PACKAGE INITIALIZATION &
	&
	! THIS SUBROUTINE MUST BE CALLED IMMEDIATELY UPON ENTRY! &
	! &
	! INPUTS: &
	!	SAVED FIRQB FROM THE ENTRY TO BE OBTAINED VIA SYS(CHR$(12%)) &
	! &
	! OUTPUTS: &
	!	PRIV.OFF$ = SYS CALL STRING TO TEMPORARILY DROP PRIVILEGES &
	!	PRIV.ON$ = SYS CALL STRING TO REGAIN TEMPORARY PRIVILEGES &
	!	TRAP$ = TRAP CTRL/C &
	!	KBT% = USER KEY BOARD TYPE (VIDEO OR HARD COPY) &
	!	P0$ = DEFAULT PACKAGE LOCATION DEVICE:[PPN] &
	!	MASTER$ = MASTER HELP FILE TO BE USED &
	!	O$  = <NULL> (I.E., TERMINAL OUTPUT) &
	!	O%  = CHANNEL #0 (I.E., TERMINAL OUTPUT) &
	!	W%  = WIDTH OF OUTPUT FILE (DEFAULTED TO TERMINAL WIDTH) &
	!	V$  = <NULL> (I.E., NO OLD COMMAND LINE) &
	!	T$  = UNDEFINED &

10110	PRIV.OFF$=CHR$(6%)+CHR$(-21%)+CHR$(-1%) &
	\ T$=SYS(PRIV.OFF$) &
	\ PRIV.ON$=CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ T$=SYS(CHR$(0%)) &
	\ TRAP$=CHR$(6%)+CHR$(-7%) &
	\ T$=SYS(TRAP$) &
	\ PRINT IF CCPOS(0%) &
	\ KBT%=ASCII(MID(SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(255%)),11%,1%)) &
	! SYS(PRIV.OFF$) WILL TEMPORARILY DROP PRIVILEGES. &
	! TEMPORARILY DROP TEMPORARY PRIVILEGES. &
	! SYS(PRIV.ON$) WILL REGAIN TEMPORARILY DROPPED PRIVILEGES. &
	! CANCEL CTRL/O. &
	! SYS(TRAP$) WILL ENABLE TRAPPING CTRL/C. &
	! ENABLE CTRL/C TRAPPING. &
	! ENSURE CARRIAGE IS AT LEFT MARGIN. &
	! GET USER'S KB TYPE WITH SYS CALL. &

10130	P0$="HELP$:" &
	\ MASTER$="HELP$:HELP.HLP" &
	\ O$="" &
	\ O%=0% &
	\ W%=ASCII(MID(SYS(CHR$(6%)+CHR$(-8%)),20%,1%))-1% &
	\ W%=80% IF W%>80% &
	\ V$="" &
	\ RETURN &
	! HELP$: IS THE DEFAULT HELP PACKAGE LOCATION. &
	! THE MASTER HELP FILE IS HELP.HLP . &
	! DEFAULT OUTPUT TO NULL. &
	! NULL OUTPUT USES CHANNEL #0 (TERMINAL). &
	! SET LINE WIDTH FROM TERMINAL DDB. &
	! LIMIT WIDTH TO 80. &
	! SET OLD COMMAND LINE TO NULL. &
	! RETURN TO CALLER. &
	&

10200	! &
	&
	!	READ FROM CURRENT INPUT FILE &
	&
	! INPUTS: &
	!	CHANNEL%  = CURRENT INPUT CHANNEL NUMBER &
	!	L%  = ASCII CODE OF CURRENT LINE'S FIRST CHARACTER &
	! &
	! OUTPUTS: &
	!	L9% = LAST LINE'S FIRST CHARACTER (I.E., OLD L%) &
	!	L$  = NEXT NON-COMMENT LINE FROM FILE TRIMMED OF DELIMITERS &
	!	L%  = ASCII CODE OF LINE'S FIRST CHARACTER &
	!	E%  = EOF INDICATION &

10210	L9%=L% &
	! REMEMBER LAST LINE'S CODE. &

10220	INPUT LINE #CHANNEL%, L$ &
	\ L%=ASCII(L$) &
	\ GOTO 10220 IF L%=33% &
	\ L$=CVT$$(L$,4%) &
	\ E%=0% &
	! GET THE NEXT LINE (**ERROR TRAP DETECTION POINT**). &
	! EXTRACT LINE'S LEADING CHARACTER. &
	! KEEP READING IF A COMMENT LINE (EXCLAMATION POINT). &
	! TRIM DELIMITERS FROM THE LINE. &
	! SET NOT EOF. &

10230	RETURN &
	! (**RESUMED TO WITH E%=11% IF EOF ON READ ABOVE**). &
	! RETURN TO CALLER. &
	&

10300	! &
	&
	!	OPEN INDIRECT FILES AS NEEDED &
	&
	! INPUTS: &
	!	I%  = INDENT LEVEL &
	!	O%  = OUTPUT CHANNEL NUMBER &
	!	P$  = PACKAGE LOCATION DEVICE:[PPN] &
	!	CHANNEL%  = CURRENT INPUT CHANNEL NUMBER &
	!	L$  = CURRENT INPUT FILE LINE &
	!	L%  = ASCII CODE OF CURRENT LINE'S FIRST CHARACTER &
	!	E%  = EOF INDICATION &
	! &
	! OUTPUTS: &
	!	P$  = [NEW] PACKAGE LOCATION DEVICE:[PPN] &
	!	CHANNEL%  = [NEW] INPUT CHANNEL NUMBER &
	!	L9% = [NEW] LAST LINE'S FIRST CHARACTER (I.E., OLD L%) &
	!	L$  = [NEW] NON-COMMENT LINE FROM FILE TRIMMED OF DELIMITERS &
	!	L%  = [NEW] ASCII CODE OF LINE'S FIRST CHARACTER &
	!	E%  = [NEW] EOF INDICATION &
	!	T$  = UNDEFINED &
	!	T%()= UNDEFINED &

10310	RETURN IF E% &
	\ RETURN UNLESS L%=64% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+RIGHT(L$,2%)) TO T% &
	\ IF (T%(28%) AND 16%) OR T%(5%) OR T%(6%) THEN &
		E$="" &
	\	E$=CHR$(0%) UNLESS ASCII(P$) &
	\	T$="" &
	\	T$="["+NUM1$(T%(6%))+","+NUM1$(T%(5%))+"]" &
			IF T%(5%) OR T%(6%) &
	\	P$=E$+T$ &
	\	IF T%(28%) AND 16% THEN &
			IF T%(26%) THEN &
				P$=E$+"_"+CHR$(T%(23%))+CHR$(T%(24%))+ &
					NUM1$(T%(25%))+":"+T$ &
			ELSE &
				P$=E$+"_"+CHR$(T%(23%))+CHR$(T%(24%))+":"+T$ &
	! RETURN IF EOF. &
	! RETURN UNLESS INDIRECT FILE LINE (@). &
	! .FSS THE INDIRECT COMMAND FILE LINE. &
	! IF EXPLICIT DEVICE NAME OR PPN THEN &
	!	SET USER SUPPLIED PACKAGE LOCATION FLAG TO NULL. &
	!	SET THE FLAG IF PREVIOUS PACKAGE WAS USER SUPPLIED. &
	!	SET PPN TO NULL. &
	!	BUILD REAL PPN IF ONE. &
	!	SET PACKAGE LOCATION TO USER SUPPLIED FLAG PLUS PPN. &
	!	IF A DEVICE NAME THEN &
	!		IF DEVICE NAME INCLUDES UNIT NUMBER THEN &
	!			PACKAGE IS FLAG + DEVICE + UNIT + PPN. &
	!		ELSE &
	!			PACKAGE IS FLAG + DEVICE + PPN. &

10320	T$=P$+CVT$$(RAD$(T%(7%)+SWAP%(T%(8%)))+RAD$(T%(9%)+SWAP%(T%(10%))),2%) &
	\ IF T%(28%) AND 2% THEN &
		T$=T$+"."+CVT$$(RAD$(T%(11%)+SWAP%(T%(12%))),2%) &
	  ELSE &
		T$=T$+".HLP" &
	! RE-FORM THE FILE NAME ADDING PACKAGE LOCATION TO IT. &
	! USE THE EXTENSION SPECIFIED IF ONE. &
	! USE .HLP IF NO EXTENSION. &

10330	CHANNEL%=CHANNEL%+1% &
	\ OPEN T$ FOR INPUT AS FILE #CHANNEL% &
	\ GOSUB 10200 &
	\ GOTO 10310 &
	! ADVANCE THE CHANNEL NUMBER. &
	! REGAIN TEMPORARILY DROPPED PRIVILEGES IF ALLOWED. &
	! OPEN THE INDIRECT FILE (**ERROR TRAP DETECTION POINT**). &
	! TEMPORARILY DROP PRIVILEGES AGAIN. &
	! READ FILE'S FIRST LINE. &
	! GO CHECK FOR ANOTHER INDIRECT FILE LINE. &

10340	E$=SYS(PRIV.OFF$) &
	\ IF E%=5% THEN &
		TMP%=FNDSPLY%("",-1%) &
	\	TMP%=FNDSPLY%(SPACE$(I%+I%)+ &
			"?File "+T$+" not found",-1%) &
	\	E1%=E% &
	\	E%=11% &
	\	RETURN &
	! (**RESUMED TO WITH E%=ERR IF ERROR ON OPEN ABOVE**). &
	! TEMPORARILY DROP PRIVILEGES AGAIN. &
	! IF FILE WASN'T FOUND THEN &
	!	PRINT A BLANK LINE. &
	!	TELL THE USER ABOUT IT. &
	!	FAKE END-OF-FILE ON THE FILE. &
	!	RETURN TO CALLER. &

10350	IF E%=10% THEN &
		TMP%=FNDSPLY%("",-1%) &
	\	TMP%=FNDSPLY%(SPACE$(I%+I%)+ &
			"?No read access to file",-1%) &
	\	E%=11% &
	\	RETURN &
	! IF CAN'T BE ACCESSED THEN &
	!	PRINT A BLANK LINE. &
	!	TELL THE USER ABOUT IT. &
	!	FAKE END-OF-FILE ON THE FILE. &
	!	RETURN TO CALLER. &

10360	IF E%=46% THEN &
		TMP%=FNDSPLY%("",-1%) &
	\	TMP%=FNDSPLY%(SPACE$(I%+I%)+ &
			"?Too many nested indirect files",-1%) &
	\	E%=11% &
	\	RETURN &
	! IF WE RAN OUT OF I/O CHANNELS THEN &
	!	PRINT A BLANK LINE. &
	!	TELL THE USER ABOUT IT. &
	!	FAKE END-OF-FILE ON THE FILE. &
	!	RETURN TO CALLER. &

10370	E$=FNE$(E%)+" - "+T$ &
	\ GOTO 19999 &
	! OPEN ERROR OCCURED; BUILD AN ERROR MESSAGE. &
	! TAKE THE ERROR EXIT. &
	&

10400	! &
	&
	!	FIND NEXT KEYWORD/SWITCH LINE &
	&
	! INPUTS: &
	!	CHANNEL%  = CURRENT INPUT CHANNEL NUMBER &
	!	L0% = MAIN MATCH CODE &
	!	L1% = SECONDARY MATCH CODE &
	!	L9% = LAST LINE'S FIRST CHARACTER (I.E., OLD L%) &
	!	L$  = CURRENT INPUT FILE LINE &
	!	L%  = ASCII CODE OF CURRENT LINE'S FIRST CHARACTER &
	!	K%  = INDEX INTO ACCUMULATED LINE ARRAYS &
	!	E%  = EOF INDICATION &
	!	E0% = LOGICAL EOF CODE &
	! &
	! OUTPUTS: &
	!	L1% = [NEW] SECONDARY MATCH CODE &
	!	L9% = [NEW] LAST LINE'S FIRST CHARACTER (I.E., OLD L%) &
	!	L$  = FOUND KEYWORD/SWITCH LINE &
	!	L%  = ASCII CODE OF LINE'S FIRST CHARACTER &
	!	K%  = [NEW] INDEX INTO ACCUMULATED LINE ARRAYS &
	!	H%  = TRUTH VALUE OF WHETHER FOUND LINE IS A HIDDEN LINE &
	!	E%  = [NEW] EOF INDICATION &
	!	T%  = UNDEFINED &

10410	RETURN IF E% OR L%=E0% &
	\ L1%=47% IF (L9%=42% OR L9%=64%) IF L%=47% IF E0%=-1% &
	\ L1%=L0% IF L%=L0% &
	\ T%=ASCII(MID(L$,2%,1%)) &
	\ IF T%=96% OR T%=126% THEN &
		L$=CHR$(L%)+RIGHT(L$,3%) &
	! DONE IF EOF OR LOGICAL EOF CODE MATCHES. &
	! SET SECONDARY MATCH CODE TO SWITCHES (/) IF &
	!	1) IMMEDIATELY PREVIOUS WAS A KEYWORD (*) OR INDIRECT (@) AND; &
	!	2) THE LINE IS A SWITCH LINE (/) AND; &
	!	3) WE'RE IN THE MAIN SCAN. &
	! RESET SECONDARY MATCH CODE TO MAIN MATCH CODE IF MAIN CODE MATCHES. &
	! EXTRACT LINE'S SECOND CHARACTER FOR "`" OR "~" CHECK. &
	! IF "'" (LOGGED OUT O.K.) OR "~" (PRIVILEGED ONLY) THEN &
	!	RE-BUILD THE LINE WITHOUT THE SPECIAL CHARACTER. &
	&

10420	IF L%=L0% OR L%=L1% THEN &
		H%=(ASCII(MID(L$,2%,1%))=124%) &
	\	L$=CHR$(L%)+RIGHT(L$,3%) IF H% &
	\	RETURN IF H% &
	\	K$(K%)=L$ &
	\	K%(K%)=L9% &
	\	K%=K%+1% &
	\	RETURN &
	! IF EITHER MATCH CODE MATCHES THEN &
	!	SET TRUTH VALUE AS TO WHETHER IT'S A HIDDEN LINE ("|"). &
	!	RE-BUILD THE LINE WITHOUT THE SPECIAL CHARACTER IF ONE. &
	!	RETURN FOUND HIDDEN LINE WITHOUT STORING IF HIDDEN LINE. &
	!	STORE THE FOUND KEYWORD/SWITCH LINE. &
	!	STORE THE PREVIOUS LINE'S CODE. &
	!	COUNT AS A LINE STORED. &
	!	RETURN WITH THE FOUND REAL LINE. &

10430	GOSUB 10200 &
	\ GOTO 10410 &
	! READ NEXT LINE. &
	! LOOP TO CHECK AGAIN. &
	&

10500	! &
	&
	!	PRINT COLUMN INFORMATION &
	&
	! INPUTS: &
	!	W%  = CARRIAGE WIDTH &
	!	I%  = INDENT LEVEL &
	!	O%  = OUTPUT CHANNEL NUMBER &
	!	T$  = ITEM TO PRINT (W/ LEADING * IF KEYWORD) &
	!	T0% = TYPE OF ITEM PREVIOUSLY PRINTED (INITIALLY SET TO 0%) &
	! &
	! OUTPUTS: &
	!	T$  = UNDEFINED &
	!	T0% = TYPE OF ITEM JUST PRINTED &

10510	TMP%=FNDSPLY%("",-1%) IF CCPOS(O%) IF T0%<>ASCII(T$) &
	\ T0%=CCPOS(O%)-I%-I% &
	\ IF T0%>0% THEN &
		T0%=16%-(T0% AND 15%) &
	\	T0%=2% IF T0%=16% &
	\	IF CCPOS(O%)+T0%+LEN(T$)>W% OR CCPOS(O%)+T0%+16%>W% THEN &
			TMP%=FNDSPLY%("",-1%) &
		ELSE &
			TMP%=FNDSPLY%(SPACE$(T0%),0%) &
	! RESTORE CARRIAGE IF NEEDED IF ITEM ISN'T THE SAME AS PREVIOUS ITEM. &
	! FIND LOGICAL POSITION ON THE LINE. &
	! IF NOT AT THE LOGICAL LEFT MARGIN THEN &
	!	FIND DISTANCE TO NEXT MOD(16) COLUMN. &
	!	SET DISTANCE TO 2 IF WE WOULD SKIP A WHOLE COLUMN. &
	!	IF WE WOULD OVERFLOW THE RIGHT MARGIN THEN &
	!		RESTORE CARRIAGE ELSE; &
	!		SPACE TO THE COLUMN. &

10520	T0%=ASCII(T$) &
	\ T$=RIGHT(T$,2%) IF T0%=42% &
	\ TMP%=FNDSPLY%(SPACE$(I%+I%),0%) UNLESS CCPOS(O%) &
	\ TMP%=FNDSPLY%(T$,0%) &
	\ RETURN &
	! SET TYPE OF ITEM WE'RE PRINTING. &
	! REMOVE LEADING "*" FROM KEYWORD ITEMS. &
	! PRINT INDENTING IF AT LEFT MARGIN. &
	! PRINT THE ITEM. &
	! RETURN TO CALLER. &

11000	! &
	&
	!	DECODE NEXT LEVEL OF COMMAND &
	&
	! INPUTS: &
	!	COMMAND$  = COMMAND STRING &
	!	I%  = INDENT LEVEL &
	!	P$  = PACKAGE LOCATION DEVICE:[PPN] &
	!	CHANNEL%  = CURRENT INPUT CHANNEL NUMBER &
	!	L9% = LAST LINE'S FIRST CHARACTER (I.E., OLD L%) &
	!	L$  = CURRENT INPUT FILE LINE &
	!	L%  = ASCII CODE OF CURRENT LINE'S FIRST CHARACTER &
	!	E%  = EOF INDICATION &
	!	E0% = LOGICAL EOF CODE &
	! &
	! OUTPUTS: &
	!	COMMAND$  = [NEW] COMMAND STRING &
	!	I%  = [NEW] INDENT LEVEL &
	!	P$  = [NEW] PACKAGE LOCATION DEVICE:[PPN] &
	!	CHANNEL%  = [NEW] INPUT CHANNEL NUMBER &
	!	L9% = [NEW] LAST LINE'S FIRST CHARACTER (I.E., OLD L%) &
	!	L$  = [NEW] NON-COMMENT LINE FROM FILE TRIMMED OF DELIMITERS &
	!	L%  = [NEW] ASCII CODE OF LINE'S FIRST CHARACTER &
	!	E%  = [NEW] EOF INDICATION &
	!	E0% = [NEW] LOGICAL EOF CODE &
	!	L0% = UNDEFINED &
	!	L1% = UNDEFINED &
	!	K%  = UNDEFINED &
	!	K$  = UNDEFINED &
	!	K$()= UNDEFINED &
	!	K%()= UNDEFINED &
	!	T$  = UNDEFINED &
	!	T%  = UNDEFINED &
	!	T0% = UNDEFINED &
	!	T%()= UNDEFINED &

11010	IF ASCII(COMMAND$)=64% THEN &
		T%=INSTR(1%,COMMAND$+" "," ") &
	\	IF T% > 2% THEN &
			FSS0$=CVT$$(MID(COMMAND$,2%,T%-1%),64%) &
	\		FSS1$=SYS(CHR$(6%)+CHR$(-10%)+FSS0$) &
	\		FL1%=SWAP%(CVT$%(MID(FSS1$,27%,2%))) &
	\		FL2%=SWAP%(CVT$%(MID(FSS1$,29%,2%))) &
	\		P$="" &
	\		COL%=INSTR(1%,FSS0$,":") &
	\		P$=LEFT(FSS0$,COL%) IF COL% &
	\		P$=P$+"["+NUM1$(ASCII(MID(FSS1$,6%,1%)))+"," &
				+NUM1$(ASCII(MID(FSS1$,5%,1%)))+"]" &
					IF FL2% AND 128% &
	\		FIL1%=SWAP%(CVT$%(MID(FSS1$,7%,2%))) &
	\		FIL2%=SWAP%(CVT$%(MID(FSS1$,9%,2%))) &
	\		L$="@_"+RAD$(FIL1%)+RAD$(FIL2%) &
				IF FL2% AND 1% &
	\		COMMAND$=RIGHT(COMMAND$,T%+1%) &
	\		COM.PROMPT$=COM.PROMPT$+" @"+P$ &
	! IF COMMAND LINE STARTS WITH "@" &
	!	FIND 1ST SPACE AFTER THE @ &
	!	IF @XXX (NOT @ TOPIC), &
	!		SET PACKAGE LOCATION TO "XXX" WITH USER SUPPLIED FLAG. &
	!		DELETE THE @XXX FROM THE COMMAND LINE. &
	!		ADD THE @XXX TO THE BUILT-UP COMMAND LINE. &
	!		SET NEW MASTER HELP FILE NAME IF PRESENT IN COMMAND LINE. &

11020	CHANNEL%(I%)=CHANNEL% &
	\ GOSUB 10300 &
	\ E0%=-1% IF CHANNEL%<>CHANNEL%(I%) &
	\ COMMAND$(I%)=COMMAND$ &
	\ P$(I%)=P$ &
	\ E0%(I%)=E0% &
	\ L0%,L1%=47% &
	\ L0%=42% IF E0%=-1% &
	\ M%(I%)=0% &
	\ K%=0% &
	\ GOTO 13000 IF COMMAND$="" &
	! SAVE STARTING CHANNEL NUMBER (**ERROR TRAP DETECTION POINT**). &
	! GO DO INDIRECT(S) IF ANY. &
	! SET NO LOGICAL EOF CODE IF INDIRECT(S) WERE DONE. &
	! SAVE THIS LEVEL'S COMMAND LINE. &
	! SAVE THIS LEVEL'S PACKAGE LOCATION. &
	! SAVE THIS LEVEL'S LOGICAL EOF CODE. &
	! GUESS AT MATCH CODES FOR KEYWORD BLOCK. &
	! SET MAIN MATCH CODE FOR MAIN LEVEL IF MAIN LEVEL. &
	! ZERO THIS LEVEL'S MATCH INDICATOR. &
	! ZERO SCANNED KEYWORD/SWITCH COUNTER. &
	! JUST PRINT INFORMATION IF NO COMMAND LINE LEFT. &
	&

12000	T%=INSTR(1%,COMMAND$+" "," ") &
	\ K$=LEFT(COMMAND$,T%-1%) &
	\ COMMAND$=RIGHT(COMMAND$,T%+1%) &
	\ T0%=-1% &
	\ GOTO 14000 IF E0%=0% &
	\ GOTO 12500 IF ASCII(K$)=47% &
	\ GOTO 12300 IF K$="*" &
	! FIND THE NEXT KEYWORD/SWITCH IN COMMAND LINE. &
	! EXTRACT KEYWORD/SWITCH. &
	! FORM NEXT COMMAND LINE REMAINDER. &
	! SAY EXACT MATCHES SHOULD RESULT IN AN EARLY EXIT. &
	! DONE WITHOUT SUB-SCANNING IF WE'RE IN A SWITCH BLOCK. &
	! GO TO SWITCH SCANNER IF A SWITCH SCAN. &
	! GO TO TOTALLY WILD SCANNER IF TOTALLY WILD. &

12200	GOSUB 10400 &
	\ GOTO 14000 IF E% OR E0%=L% &
	\ IF L%<>42% THEN &
		GOSUB 10200 &
	\	GOTO 12200 &
	! FIND NEXT KEYWORD/SWITCH. &
	! DONE IF EOF OR LOGICAL EOF. &
	! IF NOT A KEYWORD LINE THEN &
	!	READ THE NEXT LINE. &
	!	LOOP. &

12210	L$=RIGHT(L$,2%) &
	\ T$=CVT$$(L$,4%+8%+16%+32%+128%) &
	\ IF K$<>LEFT(T$,LEN(K$)) THEN &
		GOSUB 10200 &
	\	GOTO 12200 &
	! REMOVE THE "*" FROM KEYWORD LINE. &
	! REMOVE MULTIPLE SPACES/TABS AND UPPER CASE KEYWORD LINE. &
	! IF NOT A MATCH THEN &
	!	READ THE NEXT LINE. &
	!	LOOP. &

12220	IF ASCII(COMMAND$)=47% AND ASCII(MID(T$,LEN(K$)+1%,1%))=47% THEN &
		T%=INSTR(1%,COMMAND$+" "," ") &
	\	K$=K$+LEFT(COMMAND$,T%-1%) &
	\	COMMAND$=RIGHT(COMMAND$,T%+1%) &
	\	IF INSTR(1%,K$,"*") THEN 12600 ELSE &
			IF K$<>LEFT(T$,LEN(K$)) THEN &
				COMMAND$=COMMAND$(I%) &
	\			GOSUB 10200 &
	\			GOTO 12000 &
	! IF NEXT IN COMMAND IS A SWITCH AND KEYWORD IS "XXX/YYY" THEN &
	!	FIND THE NEXT SPACE IN COMMAND LINE. &
	!	RE-BUILD A KEYWORD OF FORM "XXX/YYY" TO MATCH UPON. &
	!	RE-FORM THE REMAINDER OF THE COMMAND LINE. &
	!	IF WILD CARD HAS BEEN BUILT INTO MATCH KEYWORD THEN QUIT ELSE &
	!		IF NOT A TRUE MATCH THEN &
	!			RESTORE ORIGINAL COMMAND LINE FOR THIS LEVEL. &
	!			READ THE NEXT LINE. &
	!			GO SCAN SOME MORE. &

12230	IF COMMAND$<>"" AND ASCII(COMMAND$)<>47% THEN &
		T%=INSTR(LEN(K$)+1%,T$," ") &
	\	IF T% THEN &
			T0%=1% IF K$<>LEFT(T$,T%-1%) &
	\		K$=LEFT(T$,T%-1%) &
	\		T%=INSTR(1%,COMMAND$+" "," ") &
	\		K$=K$+" "+LEFT(COMMAND$,T%-1%) &
	\		COMMAND$=RIGHT(COMMAND$,T%+1%) &
	\		IF INSTR(1%,K$,"*") THEN 12600 ELSE &
				IF K$=LEFT(T$,LEN(K$)) THEN 12220 ELSE &
					COMMAND$=COMMAND$(I%) &
	\				GOSUB 10200 &
	\				GOTO 12000 &
	! IF MORE NON-SWITCH COMMAND THEN &
	!	FIND NEXT PART OF FOUND KEYWORD LINE. &
	!	IF THERE'S A NEXT PART THEN &
	!		SAY NO EARLY EXIT EVEN IF 'EXACT' MATCH IF NOT EXACT. &
	!		NOW ENSURE FIRST PART WILL BE MATCHING. &
	!		FIND THE NEXT SPACE IN COMMAND LINE. &
	!		RE-BUILD A KEYWORD TO MATCH UPON. &
	!		RE-FORM THE REMAINDER OF THE COMMAND LINE. &
	!		IF WILD CARD MOVED INTO MATCH KEYWORD THEN QUIT ELSE &
	!			IF STILL A TRUE MATCH THEN LOOP FOR MORE ELSE &
	!				RESTORE ORIGINAL COMMAND LINE. &
	!				READ THE NEXT LINE. &
	!				GO SCAN SOME MORE. &

12240	GOTO 12600 &
	! END SCAN. &

12300	GOSUB 10400 &
	\ GOTO 14000 IF E% OR E0%=L% &
	\ L$=RIGHT(L$,2%) IF L%=42% &
	\ GOTO 12600 &
	! FIND NEXT KEYWORD/SWITCH. &
	! DONE IF EOF OR LOGICAL EOF. &
	! REMOVE THE "*" FROM A KEYWORD LINE. &
	! END SCAN. &

12500	GOSUB 10400 &
	\ GOTO 14000 IF E% OR E0%=L% &
	\ IF L%<>47% THEN &
		IF LEFT(L$,2%)="*/" THEN &
			L$=RIGHT(L$,2%) &
		ELSE &
			GOSUB 10200 &
	\		GOTO 12500 &
	! FIND NEXT KEYWORD/SWITCH. &
	! DONE IF EOF OR LOGICAL EOF. &
	! IF NOT A SWITCH LINE THEN &
	!	IF SPECIAL KEYWORD+SWITCH LINE (/*) THEN &
	!		REMOVE "*" FROM LINE AND KEEP IT. &
	!	ELSE &
	!		READ NEXT LINE. &
	!		LOOP. &

12510	IF K$<>"/*" THEN &
		IF K$<>LEFT(CVT$$(L$,4%+8%+16%+32%+128%),LEN(K$)) THEN &
			GOSUB 10200 &
	\		GOTO 12500 &
	! IF NOT A WILD CARD MATCH THEN &
	!	IF NOT A MATCH THEN &
	!		READ NEXT LINE. &
	!		LOOP. &

12600	M%(I%)=1% &
	\ M%(I%)=T0% IF K$=CVT$$(L$,4%+8%+16%+32%+128%) &
		UNLESS INSTR(1%,K$,"*") &
			IF COMMAND$="" &
	\ E0%=42% &
	\ E0%=0% IF L%=47% &
	\ K%,T0%=K%-1% &
	\ IF H% THEN &
		IF L9%<>L% THEN &
			H%,K%,T0%=0% &
	\		K$(T0%)=L$ &
	! SAY SOMETHING MATCHED AT THIS LEVEL. &
	! [MAYBE] SAY QUIT SOON IF EXACT MATCH IF NOT WILD IF NO COMMAND. &
	! GUESS AT A FOUND KEYWORD BLOCK FOR LOGICAL EOF CODE. &
	! CHANGE TO FOUND SWITCH BLOCK FOR LOGICAL EOF CODE IF SWITCH. &
	! BACK UP TO SAVED MATCHED KEYWORD/SWITCH LINE AND SAVE &
	!	1) ACCUMULATED ARRAY INDEX FOR PREVIOUS LINE BACK UPS. &
	!	2) POINTER TO MATCHED KEYWORD/SWITCH. &
	! IF WE FOUND A HIDDEN LINE THEN &
	!	IF IT'S NOT A SYNONYM FOR ANYTHING THEN &
	!		RESET HIDDEN FLAG, THE INDEX, AND POINTER. &
	!		MAKE FOUND LINE THE HIDDEN LINE. &

12610	K$(K%)=RIGHT(K$(K%),2%) IF ASCII(K$(K%))=42% &
	\ IF K%(K%)=L% AND K% THEN &
		IF ASCII(K$(K%-1%))=L% THEN &
			K%=K%-1% &
	\		GOTO 12610 &
	! REMOVE LEADING "*" FROM KEYWORD LINES. &
	! IF LINE PREVIOUS TO THIS ONE WAS SAME TYPE AND IT'S BEEN SAVED THEN &
	!	IF SAVED LINE'S CODE IS OF THE SAME TYPE THEN &
	!		BACK UP TO THAT SAVED LINE. &
	!		LOOP. &

12620	FOR T%=K% TO T0% &
	\ TMP%=FNDSPLY%("",-1%) &
	\ TMP%=FNDSPLY%(SPACE$(I%+I%)+K$(T%),0%) &
	\ NEXT T% &
	\ TMP%=FNDSPLY%("",-1%) &
	\ COM.PROMPT$=COM.PROMPT$+" "+K$(T0%) &
	! FOR ALL PREVIOUS KEYWORD(S)/SWITCH(ES) IN SAME BLOCK: &
	!	PRINT A BLANK LINE OR RESTORE CARRIAGE. &
	!	PRINT THE KEYWORD/SWITCH. &
	! TELL USE ABOUT TYPING MISTAKES. &
	! RESTORE CARRIAGE. &
	! BUILD BUILT-UP COMMAND LINE. &

12630	GOSUB 10200 &
	\ IF L%=L9% AND E%=0% THEN &
		T%=ASCII(MID(L$,2%,1%)) &
	\	L$=CHR$(L%)+RIGHT(L$,3%) IF T%=96% OR T%=126% &
	\	GOTO 12630 IF ASCII(MID(L$,2%,1%))=124% &
	\	L$=RIGHT(L$,2%) IF L%=42% &
	\	TMP%=FNDSPLY%(SPACE$(I%+I%)+L$,-1%) &
	\	GOTO 12630 &
	! READ THE NEXT LINE. &
	! IF IMMEDIATE NEXT LINE IS THE SAME TYPE AND NOT EOF THEN &
	!	GET VALUE OF SECOND CHARACTER OF LINE. &
	!	REMOVE SECOND CHARACTER IF IT'S "`" OR "~". &
	!	SKIP THIS LINE IF IT'S A HIDDEN LINE ("|"). &
	!	REMOVE LEADING "*" FROM KEYWORD LINES. &
	!	PRINT THE KEYWORD/SWITCH. &
	!	LOOP. &

12640	L1%(I%)=L1% &
	\ I%=I%+1% &
	\ GOSUB 11000 &
	\ I%=I%-1% &
	\ COMMAND$=COMMAND$(I%) &
	\ P$=P$(I%) &
	\ E0%=E0%(I%) &
	\ L0%=47% &
	\ L0%=42% IF E0%=-1% &
	\ L1%=L1%(I%) &
	\ K%=0% &
	\ GOTO 12000 IF M%(I%)>0% &
	\ GOTO 14000 &
	! SAVE SECONDARY MATCH CODE. &
	! GO TO THE NEXT DEEPER LEVEL. &
	! RE-CALL OURSELVES FOR A POSSIBLE SUB-SCAN. &
	! GO BACK TO OUR LEVEL. &
	! RESTORE OUR COMMAND LINE. &
	! RESTORE PACKAGE LOCATION. &
	! RESTORE LOGICAL EOF CODE. &
	! GUESS AT MATCH CODE FOR KEYWORD BLOCK. &
	! SET MATCH CODE FOR MAIN LEVEL IF MAIN LEVEL. &
	! RESTORE SECONDARY MATCH CODE. &
	! SET SCANNED KEYWORD/SWITCH COUNTER TO ZERO AGAIN. &
	! LOOP FOR THE NEXT MATCH AT THIS LEVEL IF WE SHOULD. &
	! ELSE SAY DONE. &
	&

13000	M%(I%)=-1% &
	\ COM.PROMPT$="" IF P% &
	\ P%=-1% &
	\ V%=0% &
	\ GOTO 14000 IF E% &
	\ T%=47% &
	\ T%=42% IF L%=47% AND E0%=42% &
	\ TMP%=FNDSPLY%("",-1%) &
	! SAY WE'RE TOTALLY DONE WITH THIS LEVEL. &
	! COMMAND NOT UNIQUE IF SOMETHING ALREADY PRINTED (P%). &
	! SAY WE'VE PRINTED SOMETHING (P%). &
	! DONE IF EOF. &
	! SET TERMINATING CONDITION TO "*" AND/OR "/". &
	! CHANGE TERMINATION TO ONLY "*" IF &
	!	1) FACING A "/" LINE RIGHT NOW AND; &
	!	2) WE'RE IN A KEYWORD BLOCK. &
	! PRINT A BLANK LINE. &

13010	GOTO 14000 IF E% &
	\ IF L%<>42% AND L%<>T% THEN &
		TMP%=FNDSPLY%(SPACE$(I%+I%)+L$,-1%) &
	\	GOSUB 10200 &
	\	GOTO 13010 &
	! DONE IF EOF. &
	! IF NOT TERMINATION CONDITION THEN &
	!	PRINT THE LINE WITH INDENTING. &
	!	READ THE NEXT LINE. &
	!	LOOP. &

13020	GOTO 14000 IF E0%=0% &
	\ GOTO 14000 IF L%=42% IF E0%=42% &
	\ T0%=0% &
	! DONE IF WE'RE IN A SWITCH BLOCK. &
	! DONE IF FACING ANOTHER KEYWORD LINE IF WE'RE IN A KEYWORD BLOCK. &
	! NO HEADER YET AND SET UP FOR COLUMN INFORMATION. &

13030	K%=0% &
	\ GOSUB 10400 &
	\ IF E% OR E0%=L% THEN &
		TMP%=FNDSPLY%("",-1%) IF CCPOS(O%) &
	\	GOTO 14000 &
	! RESET ACCMULATED LINE ARRAY POINTER EACH CALL. &
	! SCAN FOR NEXT KEYWORD/SWITCH LINE. &
	! IF EOF OR LOGICAL EOF THEN &
	!	RESTORE THE CARRIAGE IF NEEDED. &
	!	DONE. &

13040	GOTO 13060 IF H% &
	\ IF T0%=0% THEN &
		TMP%=FNDSPLY%("",-1%) &
	\	TMP%=FNDSPLY%(SPACE$(I%+I%)+ &
			"Additional help is available on:",-1%) &
	\	TMP%=FNDSPLY%("",-1%) &
	\	T0%,V%=-1% &
	! SKIP LINE IF IT'S A HIDDEN LINE. &
	! IF HEADER NOT YET PRINTED THEN &
	! 	ANNOUNCE MORE INFORMATION IS COMING. &
	! 	SAY HEADER AND ADDITIONAL INFORMATION PRINTED (V%). &

13050	T$=L$ &
	\ GOSUB 10500 &
	! SET THE ITEM TO PRINT. &
	! GO DO COLUMN PRINTING. &

13060	GOSUB 10200 &
	\ GOTO 13030 &
	! READ NEXT LINE. &
	! LOOP. &
	&

14000	IF CHANNEL%>CHANNEL%(I%) THEN &
		CLOSE #CHANNEL% &
	\	CHANNEL%=CHANNEL%-1% &
	\	IF CHANNEL%>1% THEN &
			GOSUB 10200 &
	\		GOTO 14000 &
	! IF THIS LEVEL OPENED AN INDIRECT FILE THEN &
	!	CLOSE THE INDIRECT FILE. &
	!	BACK UP TO THE PREVIOUS FILE. &
	!	READ IT'S NEXT LINE. &
	!	LOOP FOR INDIRECT FILE CHECK AGAIN. &

14010	RETURN IF M%(I%) &
	\ COM.PROMPT$="" IF P% &
	\ P%=-1% &
	\ V%=0% &
	\ IF ASCII(K$)<>63% THEN &
		Z$ = "Sorry, no help available on "+K$ &
	\	Z$ = Z$ + " at this level" IF I% &
	\	TMP%=FNDSPLY%("",-1%) &
	\	TMP%=FNDSPLY%(SPACE$(I%+I%)+Z$,-1%) &
	! RETURN IF WE REALLY DID SOMETHING. &
	! SAY WE'VE PRINTED SOMETHING (P%). &
	! NO MATCHES, SO SAY SO. &

14020	RETURN UNLESS K% &
	\ V%=-1% &
	\ TMP%=FNDSPLY%("",-1%) &
	\ TMP%=FNDSPLY%(SPACE$(I%+I%)+"Help is available on:",-1%) &
	\ TMP%=FNDSPLY%("",-1%) &
	\ T0%=0% &
	\ FOR T%=0% TO K%-1% &
	\	T$=K$(T%) &
	\	GOSUB 10500 &
	\ NEXT T% &
	\ TMP%=FNDSPLY%("",-1%) IF CCPOS(O%) &
	\ RETURN &
	! RETURN IF NOTHING AVAILABLE AT ALL AT THIS LEVEL. &
	! ANNOUNCE THAT SOMETHING'S AVAILABLE HERE. &
	! SET UP FOR COLUMN INFORMATION. &
	! FOR ALL SCANNED KEYWORD(S)/SWITCH(ES) &
	!	GET KEYWORD/SWITCH LINE. &
	!	GO PRINT AS COLUMN INFORMATION. &
	! RESTORE CARRIAGE IF NEEDED. &
	! RETURN TO CALLER. &

14030	TMP%=FNDSPLY%("",-1%) &
	\ TMP%=FNDSPLY%(SPACE$(I%+I%)+"?Too many nested subtopics",-1%) &
	\ RETURN &
	! (**RESUMED TO WITH E%=55% IF NESTED TOO DEEP ON ENTRY**). &
	! SAY KEYWORD/SWITCH NEST LEVEL TOO DEEP. &
	! RETURN TO CALLER. &

15000	! &
	&
	&
	!	F U N C T I O N S &
	&
	&

15100	DEF* FNE$(Z%)=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(Z%)),3%),4%) &
	! FUNCTION	FNE$(error-code) &
	!	RETURNS THE ERROR MESSAGE TEXT OF THE SUPPLIED ERROR CODE. &

15200	DEF * FNCHK%(TEST$) &
	\ FNCHK%=0% &
	\ DELIM%=INSTR(PARSE.POS%,PARSE$,":") &
	\ T%=INSTR(PARSE.POS%,PARSE$,"/") &
	\ DELIM%=T% IF (T%>0% AND T%<DELIM%) OR DELIM%=0% &
	\ DELIM%=LEN(PARSE$) IF DELIM%=0% &
	\ SW$=MID(PARSE$,PARSE.POS%,DELIM%-(PARSE.POS%)) &
	\ FNCHK%=-1% IF SW$<>MID(TEST$,1,LEN(SW$)) &
	\ FNEND &
	! IF TEST% IS SET THEN THIS IS A DUPLICATE SWITCH &
	! CHECK TO SEE IF SWITCH IS VALID (TEST$). &

15300	DEF* FNDSPLY%(TEXT$,FULL.LINE%) &
	\ FNDSPLY%=0% &
	\ IF LINE.COUNT%>=23% THEN &
		IF O%=0% THEN &
			IF KBT%=255% THEN &
				LINE.COUNT%=0% &
	\			PRINT IF CCPOS(0%) &
	\			TEMP$=SYS(CHR$(0%)) &
	\			PRINT "Press RETURN for more..."; &
	\			TEMP$=SYS(CHR$(3%)) &
	\			GET #O% &
	\			TEMP$=SYS(CHR$(2%)) &
	\			TEMP% = CCPOS(0%) &
	\			PRINT #O%, CHR$(13%); &
				  STRING$(TEMP%,32%);CHR$(13%); &
	! DEFINE THE PARAMETERS OF THIS FUNCTION. &
	! ALWAYS RETURN 0% TO THE CALLER. &
	! IF WE HAVE COUNTED 23 LINES &
	!	AND THE OUTPUT IS GOING TO OUR TERMINAL &
	!		AND OUR TERMINAL HAS VIDEO DISPLAY THEN. &
	!			RESET THE LINE COUNTER TO ZERO. &
	!			ENSURE WE ARE AT THE RIGHT MARGIN. &
	!			DISABLE CTRL/O. &
	!			TELL THEM HOW TO CONTINUE. &
	!			KILL THE ECHO OF CHARACTERS FOR NOW. &
	!			WAIT UNTIL THEY ARE DONE READING AND &
	!				WANT MORE TOPICS. &
	!			TURN THE ECHO OF CHARACTERS BACK ON. &
	!			CLEAN UP THE MESSAGE LINE WITHOUT &
	!				MESSING UP THE SCREEN. &
	&

15310	PRINT #O%, TEXT$; &
	\ IF FULL.LINE% THEN &
		PRINT #O% &
	\	LINE.COUNT%=LINE.COUNT%+1% &
	! PRINT THE TEXT SENT TO US WITHOUT A CARRIAGE RETURN AND LINE FEED. &
	! IF A FULL LINE IS SENT THEN &
	!	PRINT THE CARRIAGE RETURN AND LINE FEED, &
	!	BUMP THE LINE COUNTER, &

15320	FNEND &
	&
	&

19000	! &
	&
	&
	!	E R R O R    H A N D L I N G &
	&
	&

19010	E%=ERR &
	\ RESUME 19020 &
	! SAVE ERROR VALUE ON SYSTEM TRAPPED ERROR. &
	! RE-ENABLE ERROR TRAPPING WITH A 'RESUME'. &

19020	E$=SYS(PRIV.OFF$) &
	\ GOTO 32767 IF ERL>=19000% AND ERL<=19999% &
	! TEMPORARILY DROP PRIVILEGES ON ALL ERRORS. &
	! EXIT IF ERROR DURING ERROR PROCESSING. &

19030	IF (ERL=2010% OR ERL=3010% OR ERL=15300%) AND E%=11% THEN &
		TEMP$=SYS(CHR$(2%)) IF ERL=15300% &
	\	GOTO 31200 &
	! IF CONTROL/Z TYPED AT "TOPIC", "SUBTOPIC", OR "RETURN FOR MORE". &
	!	RESTORE SCREEN ECHO IF NEEDED. &
	!	EXIT. &

19050	GOTO 10230 IF ERL=10220% AND E%=11% &
	! BACK INTO READING SUBROUTINE IF END-OF-FILE. &

19060	GOTO 10340 IF ERL=10330% AND E%<>28% &
	! BACK INTO INDIRECT SUBROUTINE IF ANY TYPE OF OPEN ERROR. &

19070	GOTO 14030 IF ERL=11020% AND ERR=55% &
	! BACK INTO KEYWORD/SWITCH SCANNER FOR EXIT IF NESTED TOO DEEP. &

19080	GOTO 31200 IF ERR=28% &
	! CTRL/C TRAP - EXIT PROGRAM &

19090	IF ERR=52% AND ERL=30120% THEN &
		PRINT "?Illegal number in /CHAIN switch" &
	\ GOTO 32767 &
	! BAD NUMBER SPECIFIED FOR LINE NUMBER IN /CHAIN SWITCH &

19998	E$=FNE$(E%)+" at line "+NUM1$(ERL) &
	! FETCH THE ERROR MESSAGE TEXT WITH ERRING LINE NUMBER. &

19999	T$=SYS(CHR$(0%)) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "??Program failure in HELP - "; E$ &
	\ GOTO 31200 &
	! CANCEL CTRL/O. &
	! ENSURE CARRIAGE AT LEFT MARGIN. &
	! PRINT THE FATAL ERROR MESSAGE &
	! EXIT. &
	&
	&

30000	! &
	&
	&
	!	C C L    E N T R Y    F O R    H E L P &
	&
	&

30005	ON ERROR GOTO 19000 &
	\ VALID.SWITCHES$="POC" &
	\ PROMPT% = -1% &
	! SET UP STANDARD ERROR TRAP. &
	! VALID SWITCHES ARE /PROMPT, /OUTPUT, /CHAIN &
	! DEFAULT TO /PROMPT &

30010	GOSUB 10100 &
	\ PARSE$=CVT$$(SYS(CHR$(7%)),4%+8%+16%+32%+128%+256%) &
	\ QUO%,PARSE.POS%,OUT%,CHAI%=0% &
	\ GOSUB 30130 &
	! INITIALIZE PACKAGE (DOES PRIV.OFF$) &
	! GET LINE TO PARSE FROM CORE COMMON &
	! FIND SECTION OF LINE THAT CONTAINS WHAT WE WANT HELP ON &

30030	PARSE.POS%=INSTR(PARSE.POS%,PARSE$,"/") &
	\ IF PARSE.POS%=0% THEN GOTO 31010 &
	! FIND NEXT SWITCH.  IF NONE WAS FOUND THEN EXIT. &

30040	PARSE.POS%=PARSE.POS%+1% &
	\ ON INSTR(1%,VALID.SWITCHES$,MID(PARSE$,PARSE.POS%,1%))+1% &
		GOTO 30190,30050,30060,30090 &
	! SET POINTER TO FIRST CHARACTER OF SWITCH. &
	! GET SWITCH AND GOTO CORRECT SECTION TO PARSE IT. &

30050	GOTO 30190 IF FNCHK%("PROMPT") &
	\ PROMPT%=-1% &
	\ GOTO 30030 &
	! VALIDATE THE SWITCH AND SET PROMPT%, RETURN TO PARSER &

30060	GOTO 30190 IF FNCHK%("OUTPUT") &
	\ PARSE.POS%=DELIM% &
	! VALIDATE THE SWITCH AND SET OUT%, THEN SET POINTER TO THE &
	! ":" BEFORE THE FILENAME. &

30070	PARSE.POS%=DELIM% &

30080	PARSE.POS%=PARSE.POS%+1% &
	\ BLD$=MID(PARSE$,PARSE.POS%,1%) &
	\ GOTO 30085 IF BLD$="/" OR BLD$=CHR$(32%) OR BLD$=CHR$(10%) &
	\ O$=O$+BLD$ &
	\ GOTO 30080 &
	! FIND OUTPUT FILE ON /OUTPUT SWITCH. &

30085	O$=O$+"HELP" IF O$="" OR MID(O$,LEN(O$),1%)=":" &
	\ O$=O$+".LST" UNLESS INSTR(1%,O$,".") &
	\ OUT%=-1% &
	\ GOTO 30030 &
	! ADD DEFAULT FILENAME & EXT IF NONE SPECIFIED &

30090	GOTO 30190 IF FNCHK%("CHAIN") &
	\ PARSE.POS%=DELIM%+1% &
	\ DELIM$=MID(PARSE$,PARSE.POS%,1%) &
	\ PARSE.POS%=PARSE.POS%+1% &
	\ SEMI%=INSTR(PARSE.POS%,PARSE$,";") &
	\ IF SEMI%=0% THEN 30190 &
	! VALIDATE THE SWITCH, GET DELIMITER, SET CHAI%, AND &
	! FIND POSTITION OF THE FIRST ";" &

30110	CHAI.FILE$=MID(PARSE$,PARSE.POS%,SEMI%-PARSE.POS%) &
	\ NEXT.SEMI%=INSTR(SEMI%+1%,PARSE$,";") &
	\ IF NEXT.SEMI%=0% THEN 30190 &
	! GET NAME OF FILE TO CHAIN TO AND FILE NEXT ";" &

30120	T$=MID(PARSE$,SEMI%+1%,NEXT.SEMI%-SEMI%-1%) &
	\ T$="0" IF T$="" &
	\ LN%=VAL(T$) &
	\ EOS%=INSTR(PARSE.POS%,PARSE$,DELIM$) &
	\ GOTO 30190 IF EOS%=0% &
	\ CORE$=MID(PARSE$,NEXT.SEMI%+1%,EOS%-NEXT.SEMI%-1%) &
	\ PARSE.POS%=EOS%+1% &
	\ CHAI%=-1% &
	\ GOTO 30030 &
	! FIND CORE-COMMON AND SET POINTER TO END OF /CHAIN COMMAND &

30130	PARSE.POS%=PARSE.POS%+1% &
	\ PARSE.CHAR$=MID(PARSE$,PARSE.POS%,1%) &
	\ GOTO 30160 IF PARSE.CHAR$='"' &
	\ GOTO 30170 IF PARSE.CHAR$=" " AND NOT QUO% &
	\ GOTO 30130 &
	! SEARCH FOR START OF HELP TEXT &

30160	QUO%=NOT QUO% &
	\ GOTO 30130 &
	! IGNORE CHAR'S IN QUOTES &

30170	COMMAND$=RIGHT(PARSE$,PARSE.POS%+1%) &
	\ PARSE$=LEFT(PARSE$,PARSE.POS%-1%) &
	\ PARSE.POS%=0% &
	\ RETURN &
	! COMMAND$= HELP TEXT &

30190	PRINT "?Error in switch" &
	\ GOTO 31200 &
	! ERROR IN SWITCH SPEC OR A SWITCH OTHER THEN &
	! /PROMPT WAS SPECIFIED WHILE LOGGED OUT. &
	! EXIT... &
	&


31000	GOTO 30000 &
	&
	&
	!	C H A I N  E N T R Y    P O I N T    I N T O    H E L P &
	&
	! Just merge with CCL entry as core common is similiar &
	&

31010	T%=LEN(COMMAND$) &
	\ IF PROMPT%=-1% THEN &
		GOSUB 3000 &
	\	GOTO 31200 &

31030	GOSUB 4000 &
	\ GOTO 31200 &

31200	! &
	&
	&
	!	E X I T    O R    C H A I N &
	&
	&

31205	CLOSE #O% IF O% &
	! CLOSE OUTPUT FILE IF THERE IS ONE. &

31210	GOTO 32767 UNLESS CHAI% &
	\ T$=SYS(CHR$(8%)+CORE$) &
	\ T$=SYS(PRIV.ON$) &
	\ CHAIN CHAI.FILE$ LINE LN% &
	! WAS /CHAIN SPECIFIED?   NO => LEAVE PROGRAM &
	! YES => CHAIN TO SPECIFIED PROGRAM &
	&
	&

31500	!	D C L   E N T R Y   F O R   H E L P &
	! &

31510	ON ERROR GOTO 19000 &
	\ VALID.SWITCHES$="PO" &
	\ GOTO 30010 &
	! SET UP STANDARD ERROR TRAP &
	! VALID SWITCHES ARE /PROMPT AND /OUTPUT &
	! MERGE WITH CCL ENTRY &
	&

32767	END
