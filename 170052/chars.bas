2!		PROGRAM		: CHARS
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
  !******************************************************************* &
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&
	! VER/ED	EDIT DATE	REASON &
	! KFM 01	21-MAY-84	ADD PRIV CHECK &
	! &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&

910	DIM #1%, C%(6%,39%)		! USED IN THE CREATION OF THE FILE. &

920	DIM Q%(30%)			! USED TO FIND OUT WHERE WE CAME FROM. &

1000	! &
	&
	&
	!	M A I N    C O D E &
	&

1010	I$="V10.1-A" &
		! SET UP THE V/L. &

1020	PRINT IF CCPOS(0%) &
	\ PRINT "CHARS	"+I$+"	"+ &
		CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)),3%),4%) &
		! PRINT OUT THE HEADER. &

1030	! Check program privileges before continuing. &
	IF FNPRV%("WREAD") = 0% THEN &
		PRINT &
		\ PRINT "?WREAD privilege required" &
		\ GOTO 32767 &

1040	! Check program privileges before continuing. &
	IF FNPRV%("WWRITE") = 0% THEN &
		PRINT &
		\ PRINT "?WWRITE privilege required" &
		\ GOTO 32767 &

1050	! Check program privileges before continuing. &
	IF FNPRV%("WACNT") = 0% THEN &
		PRINT &
		\ PRINT "?WACNT privilege required" &
		\ GOTO 32767 &

1200	CHANGE SYS(CHR$(12%)) TO Q% &
	\ PKG.LOC$="["+NUM1$(Q%(6%))+","+NUM1$(Q%(5%))+"]" &
	\ PKG.LOC$=CHR$(Q%(23%))+CHR$(Q%(24%)) &
		+NUM1$(Q%(25%))+":"+PKG.LOC$ &
		IF Q%(26%) AND 1% &
	\ PKG.LOC$="_"+PKG.LOC$ &
	\ IF Q%(3%)+SWAP%(Q%(4%))<>15%*2% THEN &
		PRINT "?CHARS must be COMPILED" &
	\	GOTO 32767 &
		! SET UP THE NAME OF THE DEVICE AND THE ACCOUNT FROM WHICH WE &
		!  WERE RUN. &
		! ISSUE AN ERROR IF IT WAS NOT A COMPILED FILE. &
	&

2000	! &
	&
	&
	!	S E T    U P    T H E    F I L E &
	&

2010	OPEN PKG.LOC$+"CHARS.QUE" AS FILE 1% &
	\ FOR J%=0% TO 39% STEP 10% &
	\	FOR I%=0% TO 6% &
	\		FOR K%=0% WHILE K%<10% AND J%+K%<40% &
	\			READ C$ &
	\			C%(I%,J%+K%)=FNC%(C$) &
	\		NEXT K% &
	\	NEXT I% &
	\ NEXT J% &
	\ CLOSE 1% &
	\ GOTO 32767 &
		! OPEN THE DESTINATION FILE; &
		! FOR ALL 39. CHARACTERS : &
		!	FOR ALL 6 LINES OF EACH CHARACTER : &
		!		FOR EACH MULTIPLE OF 10 CHARACTERS : &
		!			READ THE VALUE FOR THIS CHARACTER; &
		!			CONVERT IT TO A BINARY VALUE AND STORE &
		!			 IT AWAY; &
		!		LOOP FOR NEXT CHARACTER; &
		!	LOOP FOR NEXT LINE; &
		! LOOP FOR ALL CHARACTERS; &
		! CLOSE THE DESTINATION FILE; &
		! EXIT. &
	&

10000 DATA .....,.AAA.,BBBB.,.CCC.,DDDD.,EEEEE,FFFFF,.GGG.,H...H,.III.
10001 DATA .....,A...A,B...B,C...C,D...D,E....,F....,G...G,H...H,..I..
10002 DATA .....,A...A,B...B,C....,D...D,E....,F....,G....,H...H,..I..
10003 DATA .....,AAAAA,BBBB.,C....,D...D,EEEE.,FFFF.,G....,HHHHH,..I..
10004 DATA .....,A...A,B...B,C....,D...D,E....,F....,G..GG,H...H,..I..
10005 DATA .....,A...A,B...B,C...C,D...D,E....,F....,G...G,H...H,..I..
10006 DATA .....,A...A,BBBB.,.CCC.,DDDD.,EEEEE,F....,.GGG.,H...H,.III.
10007 REM &
      REM
10010 DATA ....J,K...K,L....,M...M,N...N,.OOO.,PPPP.,.QQQ.,RRRR.,.SSS.
10011 DATA ....J,K...K,L....,MM.MM,NN..N,O...O,P...P,Q...Q,R...R,S...S
10012 DATA ....J,K..K.,L....,M.M.M,NN..N,O...O,P...P,Q...Q,R...R,S....
10013 DATA ....J,KKK..,L....,M...M,N.N.N,O...O,PPPP.,Q...Q,RRRR.,.SSS.
10014 DATA ....J,K..K.,L....,M...M,N..NN,O...O,P....,Q.Q.Q,R.R..,....S
10015 DATA J...J,K...K,L....,M...M,N..NN,O...O,P....,Q..QQ,R..R.,S...S
10016 DATA .JJJ.,K...K,LLLLL,M...M,N...N,.OOO.,P....,.QQQ.,R...R,.SSS.
10017 REM &
      REM
10020 DATA TTTTT,U...U,V...V,W...W,X...X,Y...Y,ZZZZZ,.$$$.,.....,.???.
10021 DATA ..T..,U...U,V...V,W...W,X...X,Y...Y,....Z,$.$.$,.....,?...?
10022 DATA ..T..,U...U,V...V,W...W,.X.X.,.Y.Y.,...Z.,$.$..,.....,....?
10023 DATA ..T..,U...U,.V.V.,W...W,..X..,..Y..,..Z..,.$$$.,.....,...?.
10024 DATA ..T..,U...U,.V.V.,W.W.W,.X.X.,..Y..,.Z...,..$.$,.....,..?..
10025 DATA ..T..,U...U,.V.V.,WW.WW,X...X,..Y..,Z....,$.$.$,.....,.....
10026 DATA ..T..,.UUU.,..V..,W...W,X...X,..Y..,ZZZZZ,.$$$.,..*..,..?..
10027 REM &
      REM
10030 DATA .000.,..1..,.222.,.333.,4...4,55555,.666.,77777,.888.,.999.
10031 DATA 0..00,.11..,2...2,3...3,4...4,5....,6...6,....7,8...8,9...9
10032 DATA 0..00,..1..,2...2,....3,4...4,4444.,6....,...7.,8...8,9...9
10033 DATA 0.0.0,..1..,...2.,..33.,44444,....5,6666.,..7..,.888.,.9999
10034 DATA 00..0,..1..,..2..,....3,....4,5...5,6...6,.7...,8...8,....9
10035 DATA 00..0,..1..,.2...,3...3,....4,5...5,6...6,.7...,8...8,9...9
10036 DATA .000.,.111.,22222,.333.,....4,.555.,.666.,.7...,.888.,.999.
10037 REM &
      REM
15000	DEF* FNC%(C$) &
		! FUNCTION TO RETURN A BIT-ENCODED REPRESENTATION OF EACH &
		!  OF THE LEGAL CHARACTERS. &
		!	C$	A 5 BYTE (OR LARGER) STRING WHICH IS THE &
		!		 DATA READ FOR THIS PART OF THIS CHARACTER. &
		! &
		! FOR EACH '.' CHARACTER IN THE STRING, THE FUNCTION SETS &
		!  A BIT TO ONE.  FOR EACH NON-'.' CHARACTER, THE FUNCTION &
		!  LEAVES A BIT 0.  IN SPLRUN, ANY BIT WHICH IS 0 IS REPLACED &
		!  BY A (STRING) OF THE APPROPRIATE CHARACTER (E.G., FOR EACH &
		!  BIT OF 0 IN THE CODING OF THE '?', SPLRUN SUBSTITUTES A &
		!  (STRING OF) '?' CHARACTERS). &

15010	Z0%=0% &
	\ Z0%=Z0% OR 2%^Z% IF ASCII(MID(C$,Z%+1%,1%))=46% FOR Z%=0% TO 4% &
	\ FNC%=Z0% &
	\ FNEND &
		! INITIALIZE THE ACCUMULATOR; &
		! SET A 1 BIT FOR EACH CHARACTER IN THE PASSED STRING WHICH &
		!  IS A '.'; &
		! SET THE FUNCTION VALUE; &
		! EXIT. &

15100	DEF* FNPRV%(PRIV$) &
	&
	\ CHANGE SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+ &
		PRIV$) TO Q% &
	\ FNPRV% = (Q%(3%)=0%) &
	\ FNEND &
	! Check to see if job currently has privilege named &
	! If privileged then return -1% &
	! Else return 0% &
	&

32767 END &

