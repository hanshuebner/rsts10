2!		PROGRAM		: BUFCHK.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
10	EXTEND
11!	&
  !			  C O P Y R I G H T &
  !	&
  !		      Copyright (C) 1978, 1991 by &
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

20	!------------------------------------------------------------! &
	!	M O D I F I C A T I O N   H I S T O R Y &
	!------------------------------------------------------------! &
	&

100	!------------------------------------------------------------! &
	!	P R O G R A M    D E S C R I P T I O N &
	!------------------------------------------------------------!
101!	SEARCH RSTS.STB AFTER THE LINK, AND CHECK OUT THE SMALL &
   !	BUFFER LIMITS, AND FIX CONFIG.MAC &
   !	&

900	!------------------------------------------------------------! &
	!	D I M E N S I O N   S T A T E M E N T S &
	!------------------------------------------------------------!
920	DIM s%(30), word%(256) &
		! S%(30)	SYS FUNCTION BUFFER &
		! WORD%(256)	GSD RECORD BUFFER &

1000	!------------------------------------------------------------! &
	!	H E R E   W E   G O &
	!------------------------------------------------------------!
1010	I$="V10.1-A" &
		! DEFINE EDIT DATA &

1020	ON ERROR GOTO 19000 &
	\ PRINT IF CCPOS(0%) &

1030	lstfile%=0% &
	\ gsdfile%=3% &
	\ null%=4% &
	&

2000	!------------------------------------------------------------! &
	!	S E T U P   G L O B A L   S Y M B O L   T A B L E &
	!------------------------------------------------------------!
2010	gsd.start%=1% &
	\ gsd.symbols%,gsd.blocks%=0% &
	\ OPEN "RSTS.STB" FOR INPUT AS FILE GSDFILE%, MODE 8192% &
	\ stbfile$="RSTS.STB" &

2015	GOSUB 17500 &
	\ FIELD #gsdfile%,510% AS f$,2% AS sil$ &
		! GET A BUFFER FOR INPUT AND SETUP &
		! FOR THE FIRST READ &
		! INITIALIZE TO NO .STB STUFF AND &

2020	CHANGE SYS(CHR$(6%)+CHR$(-25%)+CHR$(gsdfile%)) TO s% &
	\ f.rtyp%=s%(5%) &
	\ f.ratt%=s%(6%) &
	\ f.rsiz%=s%(7%)+SWAP%(s%(8%)) &
	\ f.hibk%=s%(11%)+SWAP%(s%(12%)) &
	\ f.efbk%=s%(15%)+SWAP%(s%(16%)) &
	\ f.ffby%=s%(17%)+SWAP%(s%(18%)) &
	\ fb%=0% &
	\ fixed%=1% &
	\ var%=2% &
	\ f.rsiz%=512% IF cref% &
	\ IF (f.rtyp%<>fb%) AND (f.rtyp%<>var%) THEN &
		IF (f.rtyp%<>fixed%) OR ((f.rtyp%=fixed%) AND (cref%=0%)) THEN &
			PRINT "?Illegal RSTS.STB FILE format" &
	\		CLOSE gsdfile% &
	\		GOTO 2000 &
		! READ FILE ATTRIBUTES AND SETUP &
		! FILE PARAMETERS. &
		! MAKE SURE IT'S FORMATTED BINARY OR RSX VARIABLE-LENGTH &
		! FB%		FORMATTED-BINARY RECORD TYPE CODE &
		! FIXED%	FIXED-LENGTH RECORD TYPE CODE &
		! VAR%		VARIABLE-LENGTH RECORD TYPE CODE &
		! F.RTYP%	RECORD TYPE &
		! F.RATT%	RECORD ATTRIBUTES (NOT USED) &
		! F.RSIZ%	RECORD SIZE &
		! F.HIBK%	HIGHEST VIRTUAL BLOCK NUMBER (NOT USED) &
		! F.EFBK%	END-OF-FILE BLOCK &
		! F.FFBY%	FIRST FREE BYTE IN F.EFBK% &

2030	OPEN "_NL:STB.BUF" AS FILE null%, RECORDSIZE 512% IF f.rtyp%<>fb% &
	\ GOT.IT%=0% &
	\ WHILE fnGSD.ENTRY% &
	\ GSD.NAME$=RAD$(G.NAME1%)+RAD$(G.NAME2%) &
	\ IF GSD.NAME$="$$EPMM" THEN &
		EPMM=FNI.TO.F(G.VALUE%) &
	\	GOT.IT%=-1% &

2050	GOTO 2060 IF GOT.IT% &
	\ NEXT &

2060	CLOSE GSDFILE% &
	\ OPEN "RSTS.SAV" FOR INPUT AS FILE GSDFILE%, MODE 8192% &
	\ GET #GSDFILE% &
	\ FIELD #GSDFILE%, 40% AS F$,2% AS F0$ &
	\ HIGH.ADDR=FNI.TO.F(SWAP%(CVT$%(F0$))) &
	\ CLOSE GSDFILE% &
	\ M.EPMM=FNI.TO.F(FNDEC%("120000")) &
	\ M.HIGH=FNI.TO.F(FNDEC%("140000")) &
	\ IF (EPMM<=M.EPMM) AND (HIGH.ADDR<=M.HIGH) THEN &
		PRINT "Cannot determine val.ues EPMM=";EPMM; &
			" High Address=";HIGH.ADDR;" Continuing" &
		   IF EPMM=0 OR HIGH.ADDR=0 OR EPMM>=HIGH.ADDR &
	\	PRINT "This monitor has a valid buffer count" &
		   IF EPMM<>0 AND HIGH.ADDR<>0 AND EPMM<HIGH.ADDR &
	\	GOTO 2200 &
		! EXIT NOW, IF IT'S ALL OK &

2100	OPEN "CONFIG.MAC" FOR INPUT AS FILE GSDFILE%, RECORDSIZE 1024% &
	\ FIELD #GSDFILE%, BUFSIZ(GSDFILE%) AS F$ &
	\ BLK%=1% &

2110	LSET F$=STRING$(1024%,0%) &
	\ GET #GSDFILE%, BLOCK BLK% &
	\ BLK%=BLK%+1% &
	\ I%=INSTR(1%,F$,"SMLBUF") &
	\ GOTO 2110 IF I%=0% OR I%>900% &
	\ I%=I%+10%	! POINT TO THE val.ue &
	\ MAX%=0 &
	\ MAX%=EPMM-M.EPMM IF EPMM>M.EPMM &
	\ Y%=HIGH.ADDR-M.HIGH IF HIGH.ADDR>M.HIGH &
	\ MAX%=Y% IF Y%>MAX%	! KEEP THE HIGHEST ONE &
	\ MAX%=(MAX%+31%)/32%	! CALCULATE NUMBER OF BUFFERS &
	\ GOTO 2120 IF MAX%=0	! NO CHANGE &
	\ MAX%=8% IF MAX%<8%	! Change buffer count by at least 8 &
	\ BUF=VAL(MID(F$,I%,4%))! NUMBER IN CURRENT SIL &
	\ BUF=BUF-MAX%		! REDUCE BY THIS NUMBER &
	\ BUF$=NUM1$(BUF)+".   "! NEW NUMBER OF BUFFERS &
	\ LSET F$=LEFT(F$,I%-1%)+LEFT(BUF$,4%) &
		+RIGHT(F$,I%+4%) &
	\ PUT #GSDFILE%, BLOCK BLK%-1%	! NOW PUT IT BACK &
		UNLESS BUF<=40 &
	\ PRINT "%Small Buffers Lowered by";MAX% &
	\ PRINT "%Leaving you with ";NUM1$(BUF);" Buffers." &
	\ PRINT "?This monitor does not have a valid buffer count" &
		IF BUF<=40 &

2120	CLOSE GSDFILE% &

2200	GOTO 32767 &

15000	!------------------------------------------------------------! &
	!	F U N C T I O N S &
	!------------------------------------------------------------! &

15010	DEF* FNDEC%(x$) &
	!------------------------------------------------------------! &
	!	RETURN INTEGER val.ue OF OCTAL DIGIT STRING. &
	!------------------------------------------------------------! &
	\ y%=0% &
	\ IF INSTR(1%,x$,".") OR INSTR(1%,x$,"8") OR INSTR(1%,x$,"9") THEN &
		x%=VAL(x$) &
	\	GOTO 15012 &

15011	y%=(32767%+1%) IF (ASCII(x$)=49%) IF (LEN(x$)=6%) &
	\ x$=RIGHT(x$,2%) IF LEN(x$)=6% &
	\ x%=0% &
	\ FOR z%=1% TO LEN(x$) &
	\    x%=(x%*8%)+(ASCII(x$)-48%) &
	\    x$=RIGHT(x$,2%) &
	\ NEXT z% &

15012	FNDEC%=x% OR y% &
	\ FNEND &

15100	DEF* FNWORD%(A$)=SWAP%(CVT$%(A$)) &

15200	DEF* FNI.TO.F(A%) &
	\	FNI.TO.F=A% &
	\	FNI.TO.F=65536.+A% IF A%<0% &
	\ FNEND &

15300	DEF* FNERR$(CODE%) = CVT$$(RIGHT( &
			SYS(CHR$(6%)+CHR$(9%)+CHR$(CODE%)),3%),4%) &

15400	DEF* fnO$(val.ue) &
	\	val.ue%=val.ue if val.ue<=32767 &
	\	val.ue%=val.ue-65536 if val.ue>32767 &
		! &
		! CONVERT val.ue% TO AN OCTAL STRING &
		! &
		! &

15420	IF val.ue% >= 0% THEN &
		q$="0" &
	ELSE	q$="1" &
	\	val.ue%=val.ue% AND 32767% &

15430	 q$=q$+CHR$(48% + ((val.ue%/8%^q%) AND 7%)) &
			FOR q%=4% TO 0% STEP -1% &
	\ fnO$=q$ &

15440	FNEND &

15460	DEF* fnB$(val.ue%,hi.byte%) &
		! &
		! CONVERT val.ue% TO AN OCTAL BYTE STRING &
		! &
		! &

15470	val.ue%=SWAP%(val.ue%) IF hi.byte% &
	\ fnB$=RIGHT(fnO$(val.ue% AND 255%),4%) &

15490	FNEND &

15500	!------------------------------------------------------------! &
	!	F U N C T I O N S   F O R   . G S D   P R O C E S S I N G &
	!------------------------------------------------------------!
15510	!------------------------------------------------------------! &
	!	F N G S D . E N T R Y % &
	!------------------------------------------------------------!
15520	DEF* fnGSD.ENTRY% &
		! &
		! COMPILE A .GSD ENTRY &
		! &
		! RETURN: -1 IF ENTRY COMPILED, 0 IF DONE OR ERROR &
		! &
		! ON A SUCCESSFUL RETURN, THE .GSD INFORMATION IS &
		! STORED IN G.NAME1%, G.NAME2%, G.FLAG%, AND G.VALUE% &
		! NOTE:  THIS ROUTINE ONLY RETURNS GLOBAL SYMBOLS &
		! (TYPE = 4).  ALL OTHERS ARE IGNORED &
		! &

15530	fnGSD.ENTRY%=0% &
	\ GOSUB 15570 &
	\ GOTO 15590 IF (q%=-2%) AND ((item% AND 255%) = 2%) &
	\ GOTO 15590 IF (q%=-1%) AND (f.rtyp%=fixed%) &
	\ GOTO 15580 IF q% and ((item% and 255%) <> 5%)	! not an isd &
	\ GOTO 15550 UNLESS Q% &
	\	ENTRY%=NWORDS%+1%	! GO ON TO NEXT RECORD &
	\	GOTO 15530		! AND TRY AGAIN &
		! GET THE FIRST WORD AND CHECK RETURN CODE: &
		!  0	COMPILE THE DATUM &
		! -2	EXIT IF RECORD TYPE =2, GET DATA IF TYPE = 1 &
		! -1,-3	ERRORS, PRINT MESSAGE AND EXIT &
	&

15550	g.name1%=item% &
	\ GOSUB 15570 \ GOTO 15580 IF q% &
	\ g.name2%=item% &
	\ GOSUB 15570 \ GOTO 15580 IF q% &
	\ g.flag%=item% &
	\ GOSUB 15570 \ GOTO 15580 IF q% &
	\ G.VALUE%=item% &
		! COMPILE THE ENTRIES &

15555!  GOSUB 15570 \ GOTO 15580 IF Q% &
!	\ G.CVALUE%=ITEM% &
!	\ GOSUB 15570 \ GOTO 15580 IF Q% &
!	\ G.CFLAG%=ITEM%
15560	fnGSD.ENTRY%=-1% &
	\ g.type%=SWAP%(g.flag%) AND 255% &
	\ GOTO 15590 IF all% &
	\ GOTO 15530 IF g.type% <> 4% &
	\ GOTO 15590 &
		! IGNORE THIS ONE IF IT'S NOT A REAL SYMBOL &

15570	q%=fnITEM% &
	\ IF q% = -2% THEN &
		IF ((item% AND 255%)=1%) OR (f.rtyp%=fixed%) THEN &
			GOTO 15570 &

15575	RETURN &

15580	PRINT CHR$(10%);CHR$(13%);"?Error getting a .GSD entry. RETURN code = "; &
		NUM1$(q%);", RETURN val.ue = ";NUM1$(item%) &

15590	FNEND &

15600	!------------------------------------------------------------! &
	!	F N I T E M % &
	!------------------------------------------------------------!
15610	DEF* fnITEM% &
		! &
		! GET THE NEXT WORD FROM GSDFILE%. &
		! THE DATUM IS RETURNED IN ITEM% &
		! fnITEM% RETURNS AN ERROR CODE: &
		!  0	NORMAL DATUM &
		! -1	END OF FILE ON INPUT DEVICE &
		! -2	RECORD HEADER READ, ITEM% HAS RECORD CODE &
		! -3	CHECKSUM ERROR ON FORMATTED BINARY READ &
		! &

15620	IF (entry%<1%) OR (entry%>nwords%) THEN &
		GOSUB 15700 IF f.rtyp% = fb% &
	\	GOSUB 15800 IF f.rtyp% <> fb% &
	\	item%=word%(1%) &
	\	entry%=2% IF f.rtyp% <> fixed% &
	\	fnITEM%=-2% &
	\	fnITEM%=-3% IF (f.rtyp%=fb%) AND ((csum% AND 255%)<>0%) &
	\	fnITEM%=-1% IF (nwords%<1%) OR fnEOF% &
	\	GOTO 15690 &
		! IF WE HAVE NOTHING LEFT IN THE &
		! RECORD, GET THE NEXT RECORD &
		! AND RETURN -2 (RECORD START) &
		! OR RETURN -3 ON CHECKSUM ERROR &
		! RETURN -1 ON END OF FILE &

15630	item%=word%(entry%) &
	\ entry%=entry%+1% &
	\ fnITEM%=0% &
		! JUST GET THE NEXT WORD &

15690	FNEND &

15700	!------------------------------------------------------------! &
	!	G E T   A   F B   R E C O R D &
	!------------------------------------------------------------!
15710	csum%=0% &
	\ nextbyte%=fnFBGETBYTE% &
	\ RETURN IF fnEOF% &
	\ GOTO 15710 UNLESS nextbyte% &
	\ GOTO 15790 IF nextbyte% <> 1% &
	\ GOTO 15790 IF fnFBGETBYTE% &
	\ nextbyte%=fnFBGETBYTE% &
	\ reclen%=nextbyte%+SWAP%(fnFBGETBYTE%)-4% &
	\ IF reclen% > 512% THEN &
		PRINT "?Long formatted binary RECORD" &
	\	GOTO 15790 &
		! READ THE HEADER AND CHECK FOR 0/1/0 FORMAT &
		! GET THE NUMBER OF DATA BYTES IN THE HEADER &

15720	GOTO 15790 IF (reclen% AND 1%) &
	\ nwords% = reclen%/2% &
	\ word%(word%)=fnFBGETBYTE%+SWAP%(fnFBGETBYTE%) FOR word%=1% TO nwords% &
	\ nextbyte%=fnFBGETBYTE% &
		! MAKE SURE WE HAVE AN EVEN NUMBER OF BYTES &
		! GET THE RECORD AND THROW AWAY CHECKSUM &

15730	RETURN &

15790	PRINT "?Illegal formatted binary RECORD" &
	\ error%=-1% &
	\ RETURN &
		! PRINT A TROUBLE MESSAGE &

15800	!------------------------------------------------------------! &
	!	G E T    A   V A R I A B L E   R E C O R D &
	!------------------------------------------------------------!
15810	GOSUB 17600 &
	\ RETURN IF fnEOF% &
		! GET A RECORD IF WE MUST &

15820	FIELD #gsdfile%, inbyte% AS junk$, 2% AS word$ &
	\ IF f.rtyp% = var% THEN &
		reclen%=SWAP%(CVT$%(word$)) &
	  ELSE	IF inrec% < f.efbk% THEN &
			reclen%=f.rsiz% &
		ELSE	reclen%=f.ffby% &

15825	IF reclen%=-1% THEN &
		inbyte%=512% &
	\	GOTO 15810 &
		! GET THE LENGTH OF THE NEXT RECORD, &
		! LENGTH = -1 MEANS "NEXT BLOCK" &

15830	GOTO 15890 IF reclen% > 512% &
	\ inbyte%=inbyte%+2% IF f.rtyp% = var% &
	\ IF reclen% <= (512%-inbyte%) THEN &
		FIELD #gsdfile%, inbyte% AS junk$, reclen% AS input.record$ &
	\	FIELD #null%, reclen% AS junk$ &
	\	LSET junk$=input.record$ &
	\	inbyte%=inbyte%+reclen% &
	\	entry% = 1% IF cref% &
	\	entry% = 10% IF cref% AND (inrec% <= 1%) &
	\	GOTO 15850 &
		! IF THE RECORD IS COMPLETELY CONTAINED IN &
		! THIS BLOCK, JUST EXTRACT IT &
		! BUMP BYTE OFFSET TO NEXT RECORD &

15840	FIELD #gsdfile%, inbyte% AS junk$, 512%-inbyte% AS partial$ &
	\ FIELD #null%, LEN(partial$) AS junk$ &
	\ LSET junk$=partial$ &
	\ inbyte%=reclen%-(512%-inbyte%) &
	\ GOSUB 17620 &
	\ FIELD #gsdfile%, inbyte% AS partial$ &
	\ FIELD #null%, LEN(junk$) AS junk$, inbyte% AS q$ &
	\ LSET q$=partial$ &
		! GET THE FIRST PART OF THE RECORD FROM THIS BLOCK &
		! AND GET THE SECOND PART FROM THE NEXT BLOCK &

15850	inbyte%=(inbyte%+1%) AND -2% &
	\ GOTO 15890 IF reclen% AND 1% &
	\ nwords%=reclen%/2% &
	\ q%=0% &
	\ FOR word%=1% TO nwords% &
	\   FIELD #null%, q% AS junk$, 2% AS junk$ &
	\   word%(word%)=SWAP%(CVT$%(junk$)) &
	\   q%=q%+2% &
	\ NEXT word% &
		! ROUND BYTE OFFSET TO NEXT WORD BOUNDARY &
		! CHECK FOR ODD RECORD AND CONVERT &
		! TO BINARY &

15860	RETURN &

15890	PRINT "?Illegal variable-length RECORD" &
	\ error%=-1% &
	\ RETURN &

16000	!------------------------------------------------------------! &
	!	O U T P U T   F U N C T I O N S &
	!------------------------------------------------------------! &
	! &
	! GLOBALS NEEDED BY THIS PACKAGE: &
	! OUT.BASE%		THE FIRST BLOCK OUTPUT BY fnSET% &
	!			MAY BE CHANGED AS DESIRED &
	! OUTPUT.NEEDED%	FLAG FOR OUTPUT ROUTINES. &
	!			INITIALLY SET TO 0 &
	! CURRENT.BLOCK%	WHAT IS IN MEMORY NOW &
	!			INITIALLY SET TO 0. &
	! HIGHEST.BLOCK%	THE HIGHEST BLOCK WRITTEN. &
	!			INITIALLY SET TO 0. &
	! &

16010	!------------------------------------------------------------! &
	!	F N S E T % ( P O S I T I O N % , V A L U E % ) &
	!------------------------------------------------------------!
16020	DEF* fnSET%(position%,val.ue%) &
		! &
		! OUTPUT ONE WORD TO THE FILE OPEN ON CHANNEL 2 &
		! THIS FUNCTION SIMULATES VIRTUAL MEMORY PROCESSING &
		! EXCEPT THAT A BASE BLOCK (THE POSITION OF ENTRY 0) &
		! MAY BE SPECIFIED. &
		! &
		! POSITION%	THE INDEX OF THE WORD TO CHANGE &
		! val.ue%	THE val.ue TO OUTPUT &
		! &

16030	fnSET%,q%=fnGET.BLOCK%(out.base%+(SWAP%(position%) AND 255%)) &
	\ IF q% = 0% THEN &
		FIELD #rtsfile%, (position% AND 255%)*2% AS q$, 2% AS q$ &
	\	LSET q$=CVT%$(SWAP%(val.ue%)) &
	\	output.needed%=-1% &
		! MAKE SURE WE HAVE THE RIGHT BLOCK &
		! THEN -- ASSUMING NO ERRORS -- GET THE &
		! BUFFER PART TO CHANGE AND DO IT.  THEN, &
		! OUTPUT THE val.ue AND SET THE "STUFF IT" FLAG &

16040	FNEND &
	&

16100	!------------------------------------------------------------! &
	!	F N G E T . B L O C K % ( W H I C H % ) &
	!------------------------------------------------------------!
16110	DEF* fnGET.BLOCK%(which%) &
		! &
		! MAKE SURE WE HAVE BLOCK WHICH% IN &
		! MEMORY.  FORCE OUT THE CURRENT IF NECESSARY &
		! &

16120	fnGET.BLOCK%=0% &
	\ IF current.block% <> which% THEN &
		fnGET.BLOCK%,q%=fnFORCE.OUT% &
	\	IF q% = 0% THEN &
			ON ERROR GOTO 16150 &
	\		GET #rtsfile%, BLOCK which% IF which% < highest.block% &
	\		current.block%=which% &
	\		highest.block%=which% IF which% > highest.block% &

16130	GOTO 16180 &
		! GOTCHA &

16150	GOTO 19000 IF ERL<>16120% &
	\ PRINT "?Error getting BLOCK "+NUM1$(which%)+" -- "+fnERR$(ERR) &
	\ fnGET.BLOCK%=ERR &
	\ RESUME 16180 &
		! WE HAVE A PROBLEM &

16180	ON ERROR GOTO 19000 &

16190	FNEND &

16200	!------------------------------------------------------------! &
	!	F N F O R C E . O U T % &
	!------------------------------------------------------------!
16210	DEF* fnFORCE.OUT% &
		! &
		! OUTPUT THE CURRENT BLOCK IF NEEDED &
		! &

16220	IF output.needed% THEN &
		ON ERROR GOTO 16250 &
	\	PUT #rtsfile%, BLOCK current.block% &
		! OUTPUT THE CURRENT BLOCK IF NECESSARY &

16230	output.needed%,fnFORCE.OUT%=0% &
	\ current.block%=-1% &
	\ GOTO 16280 &
		! NO ERRORS, NO OUTPUT NEEDED, AND &
		! THERE IS NOTHING IN CORE NOW &

16250	GOTO 19000 IF ERL<>16220% &
	\ PRINT "?Error putting BLOCK "+NUM1$(current.block%)+" -- "+fnERR$(ERR) &
	\ fnFORCE.OUT%=ERR &
	\ RESUME 16280 &
		! SORRY ABOUT THAT &

16280	ON ERROR GOTO 19000 &

16290	FNEND &

17000	!------------------------------------------------------------! &
	!	G S D   I N P U T   F U N C T I O N S &
	!------------------------------------------------------------!
17100	!------------------------------------------------------------! &
	!	F N F B G E T B Y T E % &
	!------------------------------------------------------------!
17110	DEF* fnFBGETBYTE% &
		! &
		! fnFBGETBYTE%	GET A BYTE FROM THE GSD FILE &
		! &

17120	GOSUB 17600 &
	\ FIELD #gsdfile%, inbyte% AS junk$, 1% AS junk$ &
	\ inbyte%=inbyte%+1%\ fnFBGETBYTE%=ASCII(junk$) &
	\ csum%=csum%+ASCII(junk$) &
		! GET A BLOCK IF NECESSARY, &
		! THEN EXTRACT THE DATUM AND UPDATE THE POINTER &
		! AND UPDATE THE FORMATED BINARY CHECKSUM &

17130	FNEND &

17200	!------------------------------------------------------------! &
	!	E N D - F I L E   T E S T E R &
	!------------------------------------------------------------!
17210	DEF* fnEOF% &
		! &
		! FNEOF%	CHECK FOR END OF FILE &
		! &

17220	IF f.rtyp%=fb% THEN &
		IF error% THEN &
			fnEOF%=-1% &
	  	ELSE	fnEOF% = 0% &

17320	IF f.rtyp%<>fb% THEN &
		IF error% THEN &
			fnEOF%=-1% &
		ELSE	IF inrec% < f.efbk% &
			OR (inrec%=f.efbk%) AND (entry%<f.ffby%) THEN &
				fnEOF%=0% &
			ELSE	fnEOF%=-1% &

17330	FNEND &
	&

17500	!------------------------------------------------------------! &
	!	I N I T I A L I Z E   G S D   F I L E &
	!------------------------------------------------------------!
17510	inrec%, entry%, error%=0% &
	\ inbyte%=512% &
	\ GOSUB 17600 &
		! PRESET VARIABLES AND GET THE FIRST RECORD &

17520	RETURN &

17600	!------------------------------------------------------------! &
	!	R E A D   A   B L O C K   I F   N E C E S S A R Y &
	!------------------------------------------------------------!
17610	RETURN IF inbyte% < 512% &
	\ inbyte%=0% &
		! RETURN IF WE'RE STILL WITHIN THE BLOCK, &
		! ELSE SETUP THE INPUT COUNTER &

17620	inrec%=inrec%+1% &
	\ ON ERROR GOTO 17650 &
	\ GET #gsdfile%, BLOCK inrec% &
	\ GOTO 17680 &
		! WE MUST INPUT (GOSUB 117620) &

17650	error%=ERR &
	\ IF error% = 11% THEN &
		RESUME 17680 &
	  ELSE	PRINT "?Error getting from "+stbfile$+" -- "+fnERR$(error%) &
	\	RESUME 17680 &

17680	ON ERROR GOTO 19000 &

17690	RETURN &

19000	!------------------------------------------------------------! &
	!	F A T A L   E R R O R   T R A P &
	!------------------------------------------------------------!
19010!
19020	e%=ERR &
	\ e0%=ERL &
	\ RESUME 19030 &

19030	IF e%<>11% THEN &
		PRINT "?Fatal error " &
			+NUM1$(e%)+" at line "+NUM1$(e0%)+" -- "+fnERR$(e%) &

19090	GOTO 32767 &

32767	END
