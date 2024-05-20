2!		PROGRAM		: COPY.BAS
5!		VERSION		: V10.1
6!		EDIT		: C
7!		EDIT DATE	: 19-JUL-91
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

21	! VER/ED	EDIT DATE	REASON &
	! &
	! V9.0-10	25-Jan-85	(PRL) Change help file loc to HELP$: &
	! V9.1		17-May-85	(JJT) Add high density support &
	! V10.1-A	23-Oct-90	(JJT) Verify tapes more efficiently &
	&

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110!	THE COPY PROGRAM ALLOWS THE USER TO COPY (AND VERIFY) THE &
   !	INFORMATION ON ONE LIKE DEVICE (DECTAPE, MAGTAPE OR DISK) TO &
   !	ANOTHER. THE PROGRAM ALSO ALLOWS THE USER TO SIMPLY VERIFY THAT &
   !	THE INFORMATION ON 2 SUCH DEVICES IS IDENTICAL. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
310!	   1			TRANSFER/VERIFY INPUT FILE &
   !	   2			TRANSFER/VERIFY OUTPUT FILE &
   !	   3			DECTAPE ROUTINE &
   !	  12			USER INPUT &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
810!	FNF$				CHECKS DEVICE SPECIFICATIONS &
   !	FNR%				USED BY DECTAPE ROUTINE &

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

910	DIM D%(2%), D1%(2%) &
	\ DIM P%(2%) &
	\ DIM M%(2%) &

920	DIM L%(30) &
	! USED FOR FILENAME STRING SCAN &
	&

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ PRIV.OFF$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ PRINT IF CCPOS(0%)<>0% &
		! SET ERROR TRAP AND &
		! DROP ALL PRIVILEGES. &
		! RETURN KB TO LEFT MARGIN. &

1010	I$="V10.1-C" &
	\ PRG.NAM$="COPY" &
	! SET UP VERSION/EDIT # &

1020	CHANGE SYS(CHR$(12%)) TO L% &
	\ PKG.LOC$="["+NUM1$(L%(6%))+","+NUM1$(L%(5%))+"]" &
	\ PKG.LOC$="_"+CHR$(L%(23%))+CHR$(L%(24%)) &
		+NUM1$(L%(25%))+":"+PKG.LOC$ &
			IF L%(26%) AND 1% &
	\ IF L%(3%)+SWAP%(L%(4%))<>15%*2% THEN &
		PRINT "?Please 'RUN "; PRG.NAM$; "'" &
	\	TEMP$=SYS(CHR$(9%)) &
	\	GOTO 32767 &
		! MAKE SURE THAT WE CAME FROM A COMPILED FILE SO THAT &
		! WE CAN FIND THE HELP FILE. &

1025	PRINT PRG.NAM$;" "; I$; CHR$(9%); &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)),3%),4%) &
	! PRINT HEADER &

1030	V1$="Beginning Verification Pass " &
	\ V2$="The following blocks are bad :	" &
	\ V3$="Verification complete " &
	\ V4$=" Bad Blocks" &
	! VERIFICATION MESSAGES &
	! &
	! &
	! &
	! CODE FOR DECODING THE COMMAND STRING AND CALLING THE &
	! SUBROUTINES TO SET THE SWITCHES DESIRED. &
	&

1060	ON ERROR GOTO 19000 &
	\ IO.DEV%=0% &
	\ F1%,F2%,D%,D1%,D2%,P%,P1%,P2%,B%,B1%,V%=-1% &
	\ OPEN "_KB:COPY.CMD" FOR INPUT AS FILE 12% &
	\ PRINT #12%, "#" ; &
	\ INPUT LINE #12%, A$ &
	\ CLOSE 12% &
	\ A$=CVT$$(A$,-2%) &
	\ IF A$="/HE" THEN &
		GOSUB 11810 &
	\	GOTO 19100 IF E%<>0% &
	\	GOTO 1060 &
	! GET COMMAND STRING &

1070	C5%=0% &
	\ I%=INSTR(1%,A$,"<") &
	\ I%=INSTR(1%,A$,"=") IF I%=0% &
	\ IF I%=0% THEN &
		E%=512%+50% &
	\	GOTO 19100 &
	! CLEAR LEFT HAND SWITCH COUNTER. &
	! COMMAND STRING OKAY? &

1080	O0$=LEFT(A$,I%-1%) &
	\ I0$=RIGHT(A$,I%+1%) &
	\ I%=INSTR(1%,I0$,"/") &
	\ IF I%=0% THEN &
		E%=512%+20% &
	\	GOTO 19100 &
	! SEPARATE INPUT AND OUTPUT SPECS. &
	! AT LEAST 1 INPUT SWITCH IS NECESSARY (FURTHER CHECK LATER). &

1090	I5$=RIGHT(I0$,I%) &
	\ I0$=LEFT(I0$,I%-1%) &
	\ I%=INSTR(1%,O0$+"/","/") &
	\ O5$=RIGHT(O0$,I%) &
	\ O0$=LEFT(O0$,I%-1%) &
	! I5$=INPUT SWITCHES, O5$=OUTPUT SWITCHES. &

1100	E1%=30% &
	\ I0$=FNF$(I0$) &
	\ GOTO 19100 IF E%<>0% &
	\ IDX%=L1% &
	\ E1%=40% &
	\ O0$=FNF$(O0$) &
	\ GOTO 19100 IF E%<>0% &
	\ IF IDX%<>L1% THEN &
		E%=512%+55% &
	\	GOTO 19100 &
	! SCAN INPUT AND OUTPUT - ISSUE ERROR IF NECESSARY. &
	! ASSURE THAT SAME TYPE OF DEVICES (SAME HANDLER INDEX) WERE &
	!  SPECIFIED. &

1103	IF IDX%<>14% THEN &
		IF LEFT(I0$,2%)<>LEFT(O0$,2%) THEN &
			E%=512%+55% &
	\		GOTO 19100 &
	! IF NOT TAPE THEN NAME MUST BE THE SAME. &

1105	IF I0$=O0$ THEN &
		E%=512%+60% &
	\	GOTO 19100 &
	! MUST HAVE DIFFERENT UNIT #'S. &

1110	D2%,P2%=0% &
	\ I%=INSTR(1%,O5$,"/") &
	\ GOTO 1130 IF I%=0% &
	\ C5%=C5%+1% &
	\ I1%=INSTR(I%+1%,O5$+"/","/") &
	\ S$=MID(O5$,I%+1%,I1%-I%-1%) &
	\ O5$=RIGHT(O5$,I1%) &
	\ IF C5%>2% &
	OR ((LEFT(S$,2%)<>"DE") AND (LEFT(S$,2%)<>"PA")) THEN &
		E%=512%+65% &
	\	GOTO 19100 &
	! ONLY DENSITY AND PARITY ARE ALLOWED WITH THE OUTPUT SPEC. &
	! FORCE ERROR IF APPEAR WITH DEVICE OTHER THAN MAGTAPE. &

1120	GOSUB 10100 &
	\ GOTO 19100 IF E%<>0% &
	\ GOTO 1110 &
	! CHECK SWITCH - LOOK FOR MORE &

1130	D2%,P2%=1% &
	\ I%=INSTR(1%,I5$,"/") &
	\ GOTO 1140 IF I%=0% &
	\ I1%=INSTR(I%+1%,I5$+"/","/") &
	\ S$=MID(I5$,I%+1%,I1%-I%-1%) &
	\ I5$=RIGHT(I5$,I1%) &
	\ GOSUB 10100 &
	\ GOTO 19100 IF E%<>0% &
	\ GOTO 1130 &
	! SCAN INPUT SWITCHES &

1140	IF F1%<1% AND F2%<1% THEN &
		E%=512%+20% &
	\	GOTO 19100 &
	! MUST HAVE /NC OR /FC &

1150	IF F2%=1% AND V%<1% THEN &
		E%=512%+65% &
	\	GOTO 19100 &
	! MUST HAVE /VE WITH /NC &

1180	CHANGE SYS(CHR$(6%)+CHR$(-10%)+I0$) TO L% &
	\ Z%=(STATUS AND 255%) &
	\ GOTO 13010 UNLESS Z% &
	\ GOTO 12510 IF Z%=4% &
	\ GOTO 12010 IF Z%=14% &
	\ E%=512%+15% &
	\ E0$=I0$ &
	\ GOTO 19100 &
		! DO A STRING SCAN ON THE INPUT DEVICE NAME; &
		! DISPATCH :	13010	DISK &
		!		12510	DT &
		!		12010	MAGTAPE &

5000	DATA	"FC",1,		"NC",2,		"VE",3,		"BL",4
5050	DATA	"DE",5,		"PA",6,		"*END1",-1 &

10000	! &
	&
	&
	!	S U B R O U T I N E S &
	&
	&

10050	! &
	&
	&
	!	S W I T C H    L O O K U P &
	&
	&

10100	RESTORE &
	\ S2$=LEFT(S$,2%) &
	! PREPARE TO CHECK SWITCHES &

10110	READ S1$,S% &
	\ IF S1$="*END1" THEN &
		E%=512%+65% &
	\	GOTO 10130 &
	! IF NOT IN LIST, SET UP TO ISSUE ERROR. &

10120	IF S1$<>S2$ THEN 10110 &
	ELSE	ON S% GOSUB 11000,11040,11080,11100,11160,11240 &
	! GOSUB TO APPROPRIATE SWITCH DECODE &

10130	RETURN &

10990	! &
	&
	&
	!	R O U T I N E S    W H I C H    S E T    T H E &
	!	S W I T C H E S    A S K E D    F O R    I N &
	!	T H E    C O M M A N D    S T R I N G &
	&
	&

11000	IF F2%=1% THEN &
		E%=512%+7% &
	\	GOTO 11020 &
	! CAN'T HAVE BOTH /FC AND /NC &

11010	F1%=1% &
	! SET COPY FLAG &

11020	RETURN &
	!	/ F C &
	&

11040	IF F1%=1% THEN &
		E%=512%+7% &
	\	GOTO 11060 &
	! CAN'T HAVE /NC IF /FC ALREADY SPECIFIED &

11050	F2%=1% &
	! SET NO COPY FLAG &

11060	RETURN &
	!	/ N C &
	&

11080	V%=1% &
	\ RETURN &
	!	/ V E &
	&

11100	S0%=INSTR(1%,S$,":") &
	\ IF S0%=0% THEN B%=0% &
	ELSE	ON ERROR GOTO 11130 &
	\	B%=VAL(RIGHT(S$,S0%+1%)) &
	\	B2=B% &
	\	B2=B2/512 &
	\	B1%=B%/512% &
	\	B3=B1% &
	\	IF (B2-B3)<>0 THEN &
			E%=512%+10% &
	! CHECK BLOCK SIZE &
	&

11124	ON ERROR GOTO 19000 &
	! RESET ERROR TRAP &

11125	RETURN &
	!	/ B L &
	&

11130	E%=ERR &
	\ E%=512%+10% UNLESS E%=11% &
	\ RESUME 11124 &
	! CATCH VAL() ERROR &

11140	RETURN &
	&

11160	IF L1%<>14% THEN E%=512%+65% &
	ELSE	S0%=INSTR(1%,S$,":") &
	\	IF S0%=0% THEN D%(D2%)=0% &
		ELSE	S$=RIGHT(S$,S0%+1%) &
	\		IF LEFT("MINIMUM",LEN(S$))=S$ THEN &
				D%(D2%)=1% &
			ELSE	IF LEFT("MAXIMUM",LEN(S$))=S$ THEN &
					D%(D2%) = 32767% &
				ELSE	ON ERROR GOTO 11190 &
	\				S0%=VAL(S$) &
	\				GOTO 11200 IF S0% < 0% OR S0% > 32767% &
	\				D%(D2%)=S0% &
	! SET THE DEFAULT DENSITY IF NONE WAS SPECIFIED. &
	! EXTRACT THE SPECIFICATION, CHECK FOR 'MINIMUM' AND 'MAXIMUM'. &
	! TEST # VALUE IF SPECIFIED. &

11170	ON ERROR GOTO 19000 &
	! RESET ERROR TRAP &

11180	RETURN &
	!	/ D E N S I T Y : &

11190	E%=ERR &
	\ RESUME 11200 &
	! CATCH VAL() ERROR &

11200	E%=512%+70% UNLESS E%=11% &
	\ GOTO 11170 &
	! SET UP TO ISSUE DENSITY ERROR. &

11240	IF L1%<>14% THEN E%=512%+65% &
	ELSE	S0%=INSTR(1%,S$,":") &
	\	IF S0%=0% THEN P%(P2%)=2% &
		ELSE	S$=RIGHT(S$,S0%+1%) &
	\		IF S$="EVEN" THEN P%(P2%)=2% &
			ELSE	IF S$="ODD" THEN P%(P2%)=1% &
				ELSE	E%=512%+75% &
	! SET DEFAULT PARITY IF NONE SPECIFIED. &
	! IF 'EVEN' OR 'ODD' SET FLAG &
	! ELSE	SET UP TO ISSUE ERROR. &

11260	RETURN &
	!	/ P A R I T Y : &
	&
	&

11800	! &
	&
	&
	!	/ H E    ( H E L P    T E X T ) &
	&
	&

11810	ON ERROR GOTO 11830 &
	\ OPEN "HELP$:COPY.HLP" FOR INPUT AS FILE 4% &
	! TRY TO ACCESS THE HELP FILE. &

11820	INPUT LINE #4%, L$ &
	\ PRINT L$; &
	\ GOTO 11820 &
	! INFORMATION &

11830	E%=ERR &
	\ RESUME 11840 &

11840	CLOSE 4% &
	\ E%=512%+E% IF E%=5% &
	\ E%=0% IF E%=11% &
	\ RETURN &
	! CLOSE FILE, CANCEL ERROR IF END OF FILE. &
	&

12000	! &
	&
	&
	!	M A G T A P E S &
	&
	!	T E - 1 6    ( 9 - T R A C K ) &
	!	T U - 1 6    ( 9 - T R A C K ) &
	!	T U - 1 0    ( 7 -    A N D    9 - T R A C K ) &
	&
	&

12010	ON ERROR GOTO 19000 &
	\ B%=2048% IF B%<2048% &
	\ OPEN "_"+O0$ AS FILE 2%, RECORDSIZE B% &
	\ OPEN "_"+I0$ AS FILE 1%, RECORDSIZE B% &
	! OPEN DEVICES &
	&

12020	FOR I%=1% TO 2% &
	\ K%=MAGTAPE(3%,0%,I%) &
	\ M%(I%)=MAGTAPE(7%,0%,I%) &
	\ D1%(I%)=MAGTAPE(12%,0%,I%) &
	\ NEXT I% &
	! REWIND TAPES AND GET STATUS AND DENSITY &
	&

12050	FOR I%=0% TO 1% &
	\	D%(I%) = D1%(2%-I%) IF D%(I%) = 0% &
	\	IF D%(I%) = 1% OR D%(I%) = 32767% THEN &
			D%(I%) = MAGTAPE(12%,D%(I%),2%-I%) &
	! IF USER WANTS THE MINIMUM OR MAXIMUM DENSITY, &
	!  FIND OUT WHAT THAT DENSITY IS. &
	! D E N S I T Y &

12060		IF P%(I%)=0% THEN P%=0% &
		ELSE	IF P%(I%)=1% THEN P%=1% &
		ELSE	IF ((M%(2%-I%) AND 2048%)=0%) AND (P%(I%)<1%) &
			THEN P%=0% &
		ELSE	P%=1% &
	!  P A R I T Y &

12070		A%=MAGTAPE(6%,P%,2%-I%) &
	\	IO.DEV% = 2% - I% &
	\	A% = MAGTAPE(12%,D%(I%)+32767%+1%,2%-I%) &
	\	IO.DEV% = 0% &
	\ NEXT I% &
	! SET CHARACTERISTICS &

12080	A%=MAGTAPE(3%,0%,I%) FOR I%=1% TO 2% &
	\ C1%=0% &
	\ IF (F2%=1%) AND (V%=1%) THEN 12140 &
	! REWIND &
	! INIT COUNTER &
	! VERIFY ONLY? &

12090	ON ERROR GOTO 12110 &
	\ IO.DEV%=1% &
	\ GET #1% &
	\ IO.DEV%=0% &
	\ C%=RECOUNT &
	\ C1%=0% &
	\ IF C%<14% THEN &
		FIELD #1%, C% AS TEMP$, 14%-C% AS TEMP$ &
	\	LSET TEMP$=STRING$(14%-C%,0%) &
	\	C%=14% &
	! GET RECORD &

12100	IO.DEV%=2% &
	\ PUT #2%+SWAP%(1%), COUNT C% &
	\ IO.DEV%=0% &
	\ GOTO 12090 &
	! COPY &

12110	IF ERR=11% THEN RESUME 12120 &
	ELSE	E%=ERR &
	\	RESUME 19100 &
	! RIGHT ERROR?(EOF) &

12120	ON ERROR GOTO 19000 &
	\ K%=MAGTAPE(2%,0%,2%) &
	\ C1%=C1%+1% &
	\ IF C1%<2% THEN 12090 &
	ELSE	K%=MAGTAPE(2%,0%,2%) &
	! AT END OF TAPE? &
	! NO, GET MORE &
	! YES, WRITE EOT &
	!
12130	A%=MAGTAPE(3%,0%,I%) FOR I%=1% TO 2% &
	\ IF (V%=1%) THEN 12140 &
	ELSE	CLOSE 1%,2% &
	\	GOTO 1060 &
	! REWIND &
	! CHECK FOR VERIFY &
	! CLOSE ALL CHANNELS &
	! GO BACK &

12140	C1%,C2%=0% &
	\ R%=-1% &
	\ B0%=0% &
	\ PRINT V1$ &
	! INIT FOR VERIFY &

12150	ON ERROR GOTO 12180 &
	\ R%=R%+1% &
	\ IO.DEV%=1% &
	\ GET #1% &
	\ IO.DEV%=0% &
	\ C%=RECOUNT &
	\ C1%=0% &
	! GET RECORD &
	!
12160	IO.DEV%=2% &
	\ GET #2% &
	\ IO.DEV%=0% &
	\ C0%=RECOUNT &
	\ C2%=0% &
	! GET OTHER RECORD &
	!
12170	FIELD #1%, C% AS I$ &
	\ FIELD #2%, C0% AS O$ &
	\ IF (I$ = O$) AND (C% = C0%) THEN 12150 &
	ELSE	B0%=B0%+1% &
	\	PRINT V2$ IF B0%=1% &
	\	PRINT R% &
	\	GOTO 12150 &
	! RE-FIELD THE BUFFERS WITH THE BLOCKSIZE OF RECOUNT &
	! VERIFY &
	! PRINT BAD BLOCKS &
	! GET MORE &
	!
12180	IF ERR=11% THEN RESUME 12185 &
	ELSE	E%=ERR &
	\	RESUME 19100
12185	C1%=C1%+1% &
	\ IF C1%>1% THEN 12200
12190	ON ERROR GOTO 12210 &
	\ IO.DEV%=2% &
	\ GET #2% &
	\ IO.DEV%=0% &
	\ B0%=B0%+1% &
	\ PRINT V2$ IF B0%=1% &
	\ PRINT R% &
	\ GOTO 12150 &
	! IF EOF IN WRONG PLACE &
	! ERROR &
	!
12200	A%=MAGTAPE(3%,0%,I%) FOR I%=1% TO 2% &
	\ CLOSE 1%,2% &
	\ PRINT V3$+NUM1$(B0%)+V4$ &
	\ GOTO 1060 &
	! END VERIFY &
	! PRINT TOTAL BAD BLOCKS &
	! GO BACK &

12210	IF ERR=11% THEN RESUME 12215 &
	ELSE	E%=ERR &
	\	RESUME 19100 &
	! TRAP UNEXPECTED ERRORS &

12215	C2%=C2%+1% &
	\ IF C1%=C2% THEN 12150 &
	ELSE	B0%=B0%+1% &
	\	PRINT V2$ IF B0%=1% &
	\	PRINT R% &
	\	GOTO 12150 &
	! NOT EOF &
	&

12500	! &
	&
	&
	!	T C - 1 1    D E C T A P E &
	&
	&

12510	ON ERROR GOTO 19000 &
	\ B%=512% IF B%<512% &
	\ IF (F2%=1%) AND (V%=1%) THEN 12560 &
	ELSE	OPEN "_"+O0$ AS FILE 2%, RECORDSIZE 512% &
	\	OPEN "_"+I0$ AS FILE 1%, RECORDSIZE 512% &
	! DEFAULT BLOCK SIZE &
	! OPEN DEVICES &
	!
12520	OPEN "_SY:DTTEMP.TMP" AS FILE 3%, RECORDSIZE B% &
	\ KILL "_SY:DTTEMP.TMP" &
	\ B1%=B%/512% &
	\ R%=-4% &
	\ R1%=-4% &
	\ FIELD #1%, 512% AS I$ &
	\ FIELD #2%, 512% AS O$ &
	! CREATE BUFFER &
	! FIELD DEVICES &

12530	FOR I%=1% TO B1% &
	\	IF R%=-1% THEN 12540 &
		ELSE	IO.DEV%=1% &
	\		GET #1%, RECORD FNR%(R%) &
	\		IO.DEV%=0% &
	\		FIELD #3%, (I%-1%)*512% AS T$, 512% AS T$ &
	\		LSET T$=I$ &

12535	NEXT I% &
	! GET CORRECT RECORD(S) &
	! PUT IN BUFFER &
	! GET MORE &

12540	FOR J%=1% TO B1% &
	\	IF R1%=-1% THEN 12550 &
		ELSE	FIELD #3%, (J%-1%)*512% AS T$, 512% AS T$ &
	\		LSET O$=T$ &
	\		IO.DEV%=2% &
	\		PUT #2%, RECORD FNR%(R1%) &
	\		IO.DEV%=0% &

12545	NEXT J% &
	\ GOTO 12530 &
	! GET RECORD(S) FROM BUFFER &
	! COPY &
	! GET MORE &
	!
12550	CLOSE 1%,2%,3% &
	\ IF V%=1% THEN 12560 &
	ELSE	GOTO 1060 &
	! CLOSE ALL CHANNELS &
	! CHECK FOR VERIFY &
	! GO BACK &

12560	ON ERROR GOTO 19000 &
	\ PRINT V1$ &
	\ OPEN "_"+O0$ AS FILE 2%, RECORDSIZE 512% &
	\ OPEN "_"+I0$ AS FILE 1%, RECORDSIZE 512% &
	\ FIELD #1%, 512% AS I$ &
	\ FIELD #2%, 512% AS O$ &
	! OPEN AND FIELD &
	! SET COUNTERS &

12570	R%=-4% &
	\ R1%=-4% &
	\ B0%=0% &
	! INIT COUNTERS &

12580	IF R%=-1% THEN 12590 &
	ELSE	IO.DEV%=1% &
	\	GET #1%, RECORD FNR%(R%) &
	\	IO.DEV%=2% &
	\	GET #2%, RECORD FNR%(R1%) &
	\	IO.DEV%=0% &
	\	IF I$=O$ THEN 12580 &
		ELSE	B0%=B0%+1% &
	\		PRINT V2$ IF B0%=1% &
	\		PRINT "",  R% &
	\		GOTO 12580 &
	! GET RECORDS &
	! VERIFY &
	! PRINT BAD ONES &

12590	CLOSE 1%,2% &
	\ PRINT V3$+NUM1$(B0%)+V4$ &
	\ GOTO 1060 &
	! CLOSE ALL CHANNELS &
	! PRINT OUT TOTAL BAD &
	! GO BACK &
	&

13000	! &
	&
	&
	!	    D I S K S &
	&
	&

13010	ON ERROR GOTO 19000 &
	\ B%=2048% IF B%<2048% &
	\ IF (F2%=1%) AND (V%=1%) THEN 13070 &
	ELSE	OPEN "_"+I0$ AS FILE 1%, RECORDSIZE B% &
	\	OPEN "_"+O0$ AS FILE 2%, RECORDSIZE B% &
	! VERIFY ONLY? &
	! OPEN DEVICES &

13020	FIELD #1%, B% AS I$ &
	\ FIELD #2%, B% AS O$ &
	! FIELD DEVICES &

13030	ON ERROR GOTO 13040 &
	\ WHILE -1%<>0% &
	\ IO.DEV%=1% &
	\ GET #1% &
	\ IO.DEV%=2% &
	\ PUT #SWAP%(1%)+2% &
	\ IO.DEV%=0% &
	\ NEXT &
	! GET RECORD(s) &
	! COPY &
	! GET MORE &

13040	IF ERR=11% THEN RESUME 13050 &
	ELSE	E%=ERR &
	\	RESUME 19100 &
	! ALLOW FOR UNEXPECTED ERRORS &

13050	ON ERROR GOTO 19000 &
	\ IF RECOUNT<>0% THEN &
		LSET O$=I$ &
	\	IO.DEV%=2% &
	\	PUT #2%, COUNT RECOUNT &
	\	IO.DEV%=0% &
	! CHECK FOR END OF DEVICE &
	! COPY LAST CHUNK &

13060	CLOSE 1%,2% &
	\ IF V%=1% THEN 13070 &
	ELSE	GOTO 1060 &
	! VERIFY? &
	! CLOSE ALL CHANNELS &
	! GO BACK &
	!
13070	PRINT V1$+"using block size of "+NUM1$(B%) &
	\ R%=-1% &
	\ B0%=0% &
	! INIT COUNTERS &
	!
13080	OPEN "_"+O0$ AS FILE 2%, RECORDSIZE B% &
	\ OPEN "_"+I0$ AS FILE 1%, RECORDSIZE B% &
	\ FIELD #1%, B% AS I$ &
	\ FIELD #2%, B% AS O$ &
	! OPEN AND FIELD DEVICES &

13090	ON ERROR GOTO 13110 &
	\ WHILE -1%<>0% &
	\	R%=R%+1% &
	\	IO.DEV%=1% &
	\	GET #1% &
	\	IO.DEV%=2% &
	\	GET #2% &
	\	IO.DEV%=0% &
	\	IF I$=O$ THEN 13100 &
		ELSE	B0%=B0%+1% &
	\		PRINT V2$ IF B0%=1% &
	\		PRINT "",  R%*(B%/512%) &
	! GET AND COMPARE RECORDS &
	! PRINT BAD BLOCKS &

13100	NEXT &

13110	IF ERR=11% THEN RESUME 13115 &
	ELSE	E%=ERR &
	\	RESUME 19100 &

13115	ON ERROR GOTO 19000 &
	\ IF RECOUNT=0% THEN 13120 &
	ELSE	IO.DEV%=2% &
	\	GET #2% &
	\	IO.DEV%=0% &
	\	IF I$=O$ THEN 13120 &
		ELSE	B0%=B0%+1% &
	\		PRINT B1$ IF B0%=1% &
	\		PRINT R%*(B%/512%) &
	! CHECK FOR END OF FILE &
	! GET LAST DATA &
	! VERIFY &

13120	PRINT V3$+NUM1$(B0%)+V4$ &
	\ CLOSE 1%,2% &
	\ GOTO 1060 &
	! PRINT OUT TOTAL BAD BLOCKS &
	! CLOSE ALL CHANNELS &
	! GO BACK &

15000	! &
	&
	&
	!	F U N C T I O N S &
	&
	&

15100	DEF* FNR%(W0%) &
	\ W0%=W0%+4% &
	\ W0%=-575% IF W0%=580% &
	\ W0%=2% IF W0%=1% &
	\ W0%=-577% IF W0%=578% &
	\ FNR%=W0% &
	\ FNEND &
	! STEP 4 BLOCKS ON DECTAPE &
	! LOOK FOR EOT &
	&

15300	DEF* FNF$(Q$)
15310	L$="" &
	\ ON ERROR GOTO 15400 &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+Q$) TO L% &
	\ Q%=L%(29%)+SWAP%(L%(30%)) &
	\ GOTO 15410 IF Q%<0% OR L%(26%)=0% OR (Q% AND 8192%)=0% &
	OR (Q% AND 1161%)<>0% &
	\ L1%=(STATUS AND 255%) &
	\ GOTO 15450 IF L1%<>0% AND L1%<>4% AND L1%<>14% &
	\ L$=CHR$(L%(23%))+CHR$(L%(24%))+NUM1$(L%(25%))+":" &
	! DO SCAN ON DEVICE SPECIFICATION. &
	! ISSUE AN 'ILLEGAL (INPUT OR OUTPUT) SPECIFICATION' ERROR IF: &
	!	UNRESOLVED LOGICAL NAME, NO UNIT #, NO DEVICE # OR &
	!	FILENAME,EXTENSION,PPN OR PROT. CODE WERE SPECIFIED. &
	! ISSUE A 'ILLEGAL DEVICE' IF HANDLER INDEX &
	! IS NOT 0 (DISK), 4 (DECTAPE) OR 14 (MAGTAPE). &
	! IF NO ERROR, BUILD THE DEVICE SPEC - DEV#: &

15330	ON ERROR GOTO 19000 &
	! RESET ERROR TRAP &

15340	FNF$=L$ &
	! SET FUNCTION &

15350	FNEND &

15400	RESUME 15410 &
	! TRAP ILLEGAL FILENAME - WILL BE ADJUSTED TO 'ILLEGAL (INUT &
	! OR OUTPUT) SPECIFICATION'. &

15410	E%=512%+E1% &

15420	E0$=Q$ &
	\ E0$="No specification" IF E0$="" &
	\ GOTO 15330 &

15450	E%=512%+15% &
	\ GOTO 15420 &
	! MUST BE DISK, DECTAPE OR MAGTAPE &
	&

19000	! &
	&
	&
	!	F A T A L    E R R O R    H A N D L I N G &
	&
	&

19040	E%=ERR &
	\ E% = 512%+80% IF E% = 52% AND ERL = 12070 &
	\ RESUME 32767 IF E%=11% &
	\ RESUME 19100 &
	! CATCH UNEXPECTED ERRORS &

19100	GOTO 32767 IF E%=11% &
	\ IF E%=512%+5% THEN &
		E$="No help file exists" &
	ELSE	IF E%=512%+7% THEN &
		E$="Cannot specify both /FC and /NC" &
	ELSE	IF E%=512%+10% THEN &
		E$="Illegal block size" &
	ELSE	IF E%=512%+15% THEN &
		E$="Illegal device" &
	ELSE	IF E%=512%+20% THEN &
		E$="/FC or /NC must be specified" &
	ELSE	IF E%=512%+30% THEN &
		E$="Illegal input specification" &
	ELSE	IF E%=512%+40% THEN &
		E$="Illegal output specification" &
	ELSE	IF E%=512%+50% THEN &
		E$="Syntax error in command string" &
	ELSE	IF E%=512%+55% THEN &
		E$="Must have same type devices" &
	ELSE	IF E%=512%+60% THEN &
		E$="Must have different unit numbers" &
	ELSE	IF E%=512%+65% THEN &
		E$="Error in specifying switches" &
	ELSE	IF E%=512%+70% THEN &
		E$="Error in specifying density" &
	ELSE	IF E%=512%+75% THEN &
		E$="Error in specifying parity" &
	ELSE	IF E%=512%+80% THEN &
		E$="Invalid density for this device" &
	! ERROR MESSAGES &

19200	E$="?"+E$ IF LEN(E$) &
	\ E$=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E% AND 127%)),3%),4%) &
		UNLESS LEN(E$) &
	\ E$=E$+" - "+E0$ IF LEN(E0$) &
	\ E$=E$+" on "+I0$ IF IO.DEV%=1% &
	\ E$=E$+" on "+O0$ IF IO.DEV%=2% &
	\ PRINT E$ &
	\ PRINT "Type '/HE' for help" &
	\ E%=0% &
	\ E0$,E$="" &
	\ GOTO 1060 &
	! ISSUE THE ERROR MESSAGE. &

32767	END
