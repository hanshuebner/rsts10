1	ON ERROR GOTO 19000 &
	! &
	!		P B S K E Y . B A S &
	! &
	! &
	!This program is used to scan the CUI keyword file PBSKEY.MAC &
	!for symbols inserted into the PSECT named PBSKEY, and build a &
	!BP2 source file defining the corresponding common section. &
	!The following assumptions apply: &
	! &
	!	1.  Symbols are placed in the psect via the macros &
	!	    KEYDEF and TAGDEF; &
	! &
	!	2.  Only symbols ending with .C, .K, .Q, .T and .E &
	!	    are considered.  A warning is printed if any other &
	!	    symbol is detected following KEYDEF or TAGDEF; &

10	VERSION$ = "V10.1-A" &
\	CR.LF$ = CHR$(13%)+CHR$(10%) &
\	TB$ = CHR$(9%) &
\	FF$ = CHR$(12%) &
\	COMMA$ = "," &
		!define program version/edit level &
		!define control chars &
	&
\	Z% = VAL(MID(SYS(CHR$(6%)+CHR$(20%)+CVT%$(0%)+CVT%$(SWAP%(1%))),7%,2%)) &
\	Z% = 100% + Z% IF Z% < 70% &
\	COPYRIGHT.YEAR$ = NUM1$(1900%+Z%)
		! get 2-digit current year
		! add 100 to year if < 70 (year is >= 2000)
		! save as 4-digit copyright year string

20	DATA	"KEYDEF", &
		"TAGDEF", &
		"" &

30	DATA	".C", "define command keywords:", &
		".K", "define other keywords:", &
		".Q", "define qualifier keywords:", &
		".T", "define parameter tags:", &
		".E", "define equivalence classes:", &
		"" &

40	Z$ = SYS(CHR$(6%)+CHR$(26%)+CHR$(0%)) &
\	DEFAULT$ = "_SY:[" + NUM1$(ASCII(MID(Z$,22%,1%)) AND 255%) + &
			"," + NUM1$(ASCII(MID(Z$,21%,1%)) AND 255%) + "]" &
\	IN.PBSKEY.FILE$ = "PBSKEY.MAC" &
\	OUT.PBSKEY.FILE$ = "PBSKEY.B2S" &

50	Z$ = FNLOC$("Input location",DEFAULT$) &
\	GOTO 32767 UNLESS LEN(Z$) &
\	IN.FILE$ = Z$ + IN.PBSKEY.FILE$ &
\	DEFAULT$ = Z$ &
		!prompt for input file-spec &

60	Z$ = FNLOC$("Output location",DEFAULT$) &
\	GOTO 32767 UNLESS LEN(Z$) &
\	OUT.FILE$ = Z$ + OUT.PBSKEY.FILE$ &
		!prompt for output file-spec &

100	OPEN IN.FILE$ FOR INPUT AS FILE 1% &
		!open MACRO file containing keywords/tags &

200	OPEN OUT.FILE$ FOR OUTPUT AS FILE 2% &
		!open keyword/tag common append file &

300	PRINT "Building CUI keyword file "; OUT.FILE$; "..." &
\	GOSUB 10000 &
\	GOSUB 11000 &
\	GOSUB 12000 &
\	PRINT "Build complete" &
		!gosub to write heading &
		!gosub to build common fields &
		!gosub to write trailer &

400	CLOSE 1%, 2% &
\	GOTO 32767 &
		!close channels &
		!and exit &

10000	!	w r i t e   h e a d e r &
	! &
	! &
	PRINT #2%, &
		"	!******************************************************************** &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!	p r o g r a m   t i t l e				      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!	     program : PBSKEY					      &"; CR.LF$; &
		"	!	     version : V10.1					      &"; CR.LF$; &
		"	!	        edit : A					      &"; CR.LF$; &
		"	!	   edit date : 18-MAY-91				      &"; CR.LF$; &
		"	!	   author(s) : PRL/ACT					      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
\	PRINT #2%, &
		"	!******************************************************************** &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!		  C O P Y R I G H T				      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!		      Copyright (C) 1982, "; &
							COPYRIGHT.YEAR$; &
							" by		      &"; CR.LF$; &
		"	!	Digital Equipment Corporation, Maynard, Mass.		      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	! This software is furnished under a license and may be used and      &"; CR.LF$; &
		"	! copied  only  in accordance with the terms of such license and      &"; CR.LF$; &
		"	! with the  inclusion  of  the  above  copyright  notice.   This      &"; CR.LF$; &
		"	! software  or  any  other copies thereof may not be provided or      &"; CR.LF$; &
		"	! otherwise made available to any other person.  No title to and      &"; CR.LF$; &
		"	! ownership of the software is hereby transferred.		      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	! The information in this software is subject to change  without      &"; CR.LF$; &
		"	! notice  and should not be construed as a commitment by Digital      &"; CR.LF$; &
		"	! Equipment Corporation.					      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	! DIGITAL assumes no responsibility for the use  or  reliability      &"; CR.LF$; &
		"	! of its software on equipment that is not supplied by DIGITAL.	      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
\	PRINT #2%, &
		"	!******************************************************************** &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!	m o d i f i c a t i o n   h i s t o r y   l o g		      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
	! VER/ED	EDIT DATE	REASON &
		"	! "; VERSION$; "	"; DATE$(0%); "	(PRL) Creation			      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
\	PRINT #2%, &
		"	!******************************************************************** &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!			d e s c r i p t i o n			      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!	The COMMON (PBSKEY) is used to define all CUI constants	      &"; CR.LF$; &
		"	!	(command codes, keyword codes, and parameter or class tags)   &"; CR.LF$; &
		"	!	used by QUEUE.  The MACRO module PBSKEY.MAC is used to	      &"; CR.LF$; &
		"	!	assign values to the CUI constants.  All constants are	      &"; CR.LF$; &
		"	!	positionally dependent and must correspond to the order of    &"; CR.LF$; &
		"	!	constants in the MACRO module; any changes to one requires    &"; CR.LF$; &
		"	!	changes to the other.					      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
\	PRINT #2%, &
		"	!******************************************************************** &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
		"	!								      &"; CR.LF$; &
"	COMMON (PBSKEY)"; &
\	RETURN &
		!write the file header &
		!and exit &

11000	!	w r i t e   c o m m o n   f i e l d s &
	! &
	! &
	INPUT LINE #1%, LN$ &
\	LN$ = CVT$$(LN$,4%) &
\	RESTORE &
\	READ MACRO$ &
\	WHILE LEN(MACRO$) &
\		M% = INSTR(1%,LN$,MACRO$) &
\		GOTO 11100 IF M% &
\		READ MACRO$ &
\	NEXT &
\	GOTO 11000 &
		!get next line from file &
		!remove any control chars &
		!restore data list pointer &
		!read 1st macro &
		!do for each defined macro: &
		!	look for macro in line &
		!	exit loop if found &
		!	read next macro &
		!next (macro) &
		!try again &

11100	Z% = INSTR(1%,LN$,".MACRO") &
\	GOTO 11000 IF Z% > 0% AND Z% < M% &
\	Z% = INSTR(1%,LN$,".ENDM") &
\	GOTO 11000 IF Z% > 0% AND Z% < M% &
\	C% = INSTR(1%,LN$,";") &
\	C% = LEN(LN$)+1% &
		UNLESS C% &
\	GOTO 11000 IF C% < M% &
\	C1% = INSTR(C%+1%,LN$,";") &
\	C1% = LEN(LN$)+1% &
		UNLESS C1% &
		!look for .MACRO keyword &
		!skip line if part of macro def statement &
		!look for .ENDM keyword &
		!skip line if part of macro end statement &
		!look for comment (;) character &
		!put at end of line if none &
		!skip line if macro part of comment &
		!look for 2nd comment character (edit tag) &
		!put at end of line if none &

11200	M% = M% + LEN(MACRO$) &
\	Z$ = MACRO$ &
\	READ Z$ &
		UNTIL LEN(Z$) = 0% &
\	READ KEYWD$ &
\	FOR KW% = 1% WHILE LEN(KEYWD$) &
\		READ COMMNT$ &
\		K% = INSTR(M%,LN$,KEYWD$) &
\		GOTO 11300 IF K% &
\		READ KEYWD$ &
\	NEXT KW% &
\	PRINT "%Macro '"; MACRO$; "' has no keyword type match" &
\	GOTO 11000 &
		!position macro pointer following macro &
		!position data pointer at start of keyword types &
		!read 1st keyword type &
		!do for each keyword type: &
		!	look for keyword type after macro &
		!	exit loop of match &
		!	read next keyword type &
		!next &
		!print warning if no match &
		!skip line & try again &

11300	KW$ = CVT$$(MID(LN$,M%,K%+LEN(KEYWD$)-M%),-1%) + "%" &
\	IF LEN(KW$) > 7% THEN &
		PRINT "%Invalid keyword '"; KW$; "'" &
\		GOTO 11000 &
		!build BP2 variable from keyword &
		!if exceeds 7 chars, then &
		!	print warning msg &
		!	skip line & try again &

11400	GOSUB 13000 &
\	LAST.KW$ = KW$ &
\	LAST.KW% = KW% &
\	LAST.CMT$ = CVT$$(MID(LN$,C%+1%,C1%-C%-1%),128%) &
\	GOTO 11000 &
		!gosub to print any last keyword &
		!move current keyword into last &
		!continue with next line &

11900	IF LEN(LAST.KW$) THEN &
		COMMA$ = "" &
\		GOSUB 13000 &
		!if any last keyword, then &
		!	clear out comma &
		!	gosub to print it &

11999	RETURN &
		!exit &

12000	!	w r i t e   f i l e   t r a i l e r &
	! &
	! &
	PRINT #2%, FF$; &
\	RETURN &
		!end with a <ff> &
		!and exit &

13000	!	p r i n t   k e y w o r d   v a r i a b l e   l i n e &
	! &
	! &
	PRINT #2%, FNPOS$(25%); LAST.KW$; COMMA$; &
		   FNPOS$(49%); "!  "; LAST.CMT$; FNPOS$(78%); " &" &
		IF LAST.KW% &
\	PRINT #2%, FNPOS$(78%); " &"; CR.LF$; &
		   FNPOS$(49%); "!"; COMMNT$; FNPOS$(78%); " &" &
		IF LAST.KW% <> KW% &
\	RETURN &
		!exit if no keyword to print &
		!print <tab><tab>KEYWORD[,]<tab>...<tab>!comment &
		!exit &

19000	!	s t a n d a r d   e r r o r   t r a p &
	&
	&
	IF ERR=11% THEN &
		IF ERL = 21000 THEN &
			RESUME 21050 &
		ELSE	IF ERL = 11000 THEN &
				RESUME 11900 &
		!if EOF, then &
		!	if CTRL/Z on kb input, &
		!		exit program &
		!	else	if eof reading macro file, &
		!			resume to exit keyword routine &

19010	IF ERL = 21010 THEN &
		PRINT "?Invalid device/PPN" &
\		RESUME 21010 &
		!trap bad FSS errors on input &

19999	ON ERROR GOTO 0 &
		!unexpected error &

20000	!	FNPOS$	-   p o s i t i o n   l i n e   a t   NTH%   c h a r &
	&
	&
	DEF FNPOS$(NTH%) &
\	FNPOS$ = "" &
\	NTH% = NTH% - 1% &
\	IF NTH% > CCPOS(2%) THEN &
		Z% = ( NTH% / 8% ) - ( CCPOS(2) / 8% ) &
\		Z1% = CCPOS(2%) &
\		Z1% = 8% * (NTH% / 8%) IF Z% &
\		Z1% = NTH% - Z1% &
\		FNPOS$ = STRING$(Z%,9%) + SPACE$(Z1%) &

20010	FNEND &

21000	!	FNLOC$	-	p r o m p t   f o r   l o c a t i o n &
	&
	&
	DEF FNLOC$ (PROMPT$,DEFAULT$) &
\	DIM Z1%(30), Z2%(30) &

21010	Z$ = "" &
\	PRINT PROMPT$; &
\	PRINT " <"; DEFAULT$; ">"; &
		IF LEN(DEFAULT$) &
\	INPUT LINE Z$ &
\	Z$ = CVT$$(Z$,2%+4%+32%) &
\	CHANGE SYS(CHR$(6%)+CHR$(-10%)+Z$) TO Z1% &
\	CHANGE SYS(CHR$(6%)+CHR$(-10%)+DEFAULT$) TO Z2% &
\	Z1% = Z1%(29%) + SWAP%(Z1%(30%)) &
\	Z2% = Z2%(29%) + SWAP%(Z2%(30%)) &
\	Z1%(Z%) = Z2%(Z%) &
		FOR Z% = 5% TO 6% &
			UNLESS Z1% AND 128% &
				IF Z2% AND 128% &
\	Z1%(Z%) = Z2%(Z%) &
		FOR Z% = 23% TO 26% &
			UNLESS Z1% AND 8192% &
				IF Z2% AND 128% &

21020	Z$ = "" &
\	IF Z1%(23%) THEN &
		Z$ = "_" + CHR$(Z1%(23%))+CHR$(Z1%(24%)) &
\		Z$ = Z$ + NUM1$(Z1%(25%)) &
			IF Z1%(26%) &
\		Z$ = Z$ + ":" &

21030	IF Z1%(5%) OR Z2%(5%) THEN &
		Z$ = Z$ + "[" + NUM1$(Z1%(6%)) + "," + NUM1$(Z1%(5%)) + "]" &

21050	FNLOC$ = Z$ &
\	FNEND &

32767	END
