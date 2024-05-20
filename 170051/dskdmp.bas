2!		PROGRAM		: DSKDMP.BAS
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
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21!	VER/EDIT	EDIT DATE	REASON &
	! 61A		24-MAY		DROP TEMPS &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110	! THIS PROGRAM PRINTS A FORMATTED DUMP OF A DISK FILE. &
	! THE DUMP CAN BE IN HEXADECIMAL, OCTAL (BYTE OR WORD, OR &
	! CVT WORD), DECIMAL, AND/OR RAD50. &

400	! &
	&
	&
	!	V A R I A B L E    M A P &
	&
	&

410	! A$		INPUT FILE &
	! A1$		OUTPUT FILE NAME &
	! B		STARTING BLOCK FOR CURRENT LOOP &
	! B1		NUMBER OF BLOCKS TO DUMP FOR CURRENT LOOP &
	! B2,B2%	NEXT BLOCK NUMBER TO GET &
	! C%		<>0 IF WANT OCTAL WORD CVT DUMP &
	! D$		CHARACTERS REMAINING TO DUMP FROM CURRENT BLOCK &
	! D%		<>0 IF WANT TO DUMP IN DECIMAL &
	! E1$-E9$	 XLATE STRINGS
412	! H%<>0%	 IF WANT HEX DUMP &
	! I%		LOOP COUNTER &
	! I$		CHARACTERS TO DUMP ON NEXT PRINT LINE &
	! I1$		WORK VAR &
	! M$		WORK VAR &
	! N%		NUMBER OF CHARS PER PRINT LINE &
	! N$		WORK VAR &
	! O%		<>0% IF WANT OCTAL BYTE DUMP &
	! T$		"CLUSTER" IF NFS, ELSE "BLOCK" &
	! W%		<>0% IF WANT OCTAL WORD DUMP &

1000	! &
	&
	&
	!	***   M A I N     C O D E   *** &
	&
	&

1001	ON ERROR GOTO 19000 &
	\ PRINT IF CCPOS(0%)<>0% &
	! SET UP STANDARD ERROR TRAP &
	! RETURN KB TO LEFT MARGIN &

1010	I$="V10.1-A" &
		! SET UP VERSION/EDIT FOR HEADER &

1020	PRINT "DSKDMP";CHR$(9%);I$;CHR$(9%); &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	\ PRIV.OFF$=SYS(CHR$(6%)+CHR$(-21%)) &
		! PRINT NAME AND SYSTEM ID &
		! AND DROP PRIVILEGES. &
	&

1030	! &
	!		---INITIALIZATION--- &
	! &
	&
	! NEXT 9 LINES SET UP STRINGS WHICH ARE USED WITH &
	! THE XLATE FUNCTION TO GET THE INDIVIDUAL LINES FOR OCTAL BYTE &
	! DECIMAL, AND HEXADECIMAL REPRESENTATION AS WELL AS THE ASCII &
	! REPRESENTATION WITH UNPRINTABLE AND CONTROL CHARACTERS &
	! TRANSLATED TO AN "*" &
	&

1040	E1$=E1$+STRING$(64%,I%) FOR I%=48% TO 51% &
		! XLATE FOR 1ST OCTAL DIGIT &

1050	E2$=E2$+STRING$(8%,I%) FOR I%=48% TO 55% &
	\ E2$=E2$+E2$+E2$+E2$ &
		! XLATE FOR 2ND OCTAL &

1060	E3$=E3$+'01234567' FOR I%=1% TO 32% &
		! XLATE FOR 3RD OCTAL DIGIT &

1070	E4$=E4$+STRING$(100%,I%) FOR I%=48% TO 50% &
		! 1ST DECIMAL DIGIT &

1080	E5$=E5$+STRING$(10%,I%) FOR I%=48% TO 57% &
	\ E5$=E5$+E5$+LEFT(E5$,56%) &
		! 2ND DECIMAL DIGIT &

1090	E6$=E6$+'0123456789' FOR I%=1% TO 26% &
		! 3RD DECIMAL DIGIT &

1100	E7$=E7$+CHR$(I%) FOR I%=32% TO 126% &
	\ E7$= STRING$(32%,42%) + E7$ + STRING$(129%,42%) &
		! XLATE STRING FOR ASCII CHAR &
		! NOTE: FILL CHAR FOR NON PRINTABLES IS "*" (42) &

1110	E8$=E8$ + STRING$(16%,I%) FOR I%=48% TO 57% &
	\ E8$=E8$ + STRING$(16%,I%) FOR I%=65% TO 70% &
		! XLATE STRING FOR 1ST HEX CHAR &

1120	E9$=E9$ + '0123456789ABCDEF' FOR I%=1% TO 16% &
		! XLATE STRING FOR 2ND HEX CHAR &

1500	! &
	!		---GET OPTIONS AND INFO FROM USER--- &
	! &

1510	PRINT 'Narrow (N) or Wide (W)  printout    <W>'; &
	\ INPUT LINE N$ &
	\ N$=LEFT(CVT$$(N$,38%),1%) &
	\ IF N$<>'W' AND N$<>'N' AND N$<>'' THEN &
		GOTO 1510 &
	ELSE	IF N$='N' THEN &
			N%=50% &
		ELSE	N%=100% &
		! PRINT 50 CHARS AT A TIME IF NARROW TERM &
		! ELSE PRINT 100 CHARS AT A TIME &

1520	PRINT 'Enter Output File <KB:>'; &
	\ INPUT LINE  A1$ &
	\ A1$=CVT$$(A1$,6%) &
	\ A1$='_KB:DSKDMP.LST' IF A1$='' &
		! GET OUTPUT FILE, DEFAULT=KB:DSKDMP.LST &
	&

1530	PRINT 'Enter Input File'; &
	\ INPUT LINE A$ &
	\ A$=CVT$$(A$,38%) &
	\ IF MID(A$,LEN(A$),1%)=':' THEN &
		T$='Cluster' &
	ELSE	T$='Block' &
		! GET INPUT, GET RID OF JUNK, SEE IF NFS OR FILE &

1540	PRINT 'Octal Byte, Octal Word, Octal Cvt word, Hex,' &
	\ PRINT 'Decimal, and/or Rad50 dump (OB,OW,OC,H,D,R)'; &
	\ INPUT LINE M$ &
	\ M$=CVT$$(M$,36%) &
	\ O%=INSTR(1%,M$,'OB') &
	\ W%=INSTR(1%,M$,'OW') &
	\ C%=INSTR(1%,M$,'OC') &
	\ H%=INSTR(1%,M$,'H') &
	\ D%=INSTR(1%,M$,'D') &
	\ R%=INSTR(1%,M$,'R') &
		! SET SWITCHES TO NON ZERO FOR EACH TYPE OF DUMP &
	&
		! NOTE THAT A TEXT SCAN IS MADE FOR EACH CODE. IF IT &
		! EXISTS, ITS INDEX WILL BE NON ZERO &

1555	IF W% OR C% THEN &
		U2$=U2$+'--  ' UNTIL LEN(U2$)>=N% &
			!CREATE UNDERLINE STRING &

1560	! &
	!		---OPEN FILES--- &
	!
1570	OPEN A$ FOR INPUT AS FILE #1 &
	\ OPEN A1$ FOR OUTPUT AS FILE 2
1580	! &
	!		---END OF SET UP--- &
	! &

2000	! &
	!		---MAIN LOOP--- &
	! &

2010	PRINT 'Starting ' + T$ + ' <1>'; &
	\ INPUT LINE I1$ &
	\ I1$=CVT$$(I1$,4%) &
	\ INPUT 'How many <1>'; B1 &
	\ B1=1. IF B1=0. &
	\ IF I1$='' THEN &
		B=1. &
	ELSE	B=VAL(I1$) &
	&
	&
	&

2020	FOR B2=B TO B+B1-1.	! DO ALL BLOCKS OR CLUSTERS &

2030		GET #1%, BLOCK B2 &
	\	FIELD #1%, RECOUNT AS D$ &

2040		! PROCESS RECORD
2041		PRINT #2, &
	\	PRINT #2, '-----------------------------'
2045		PRINT #2, 'File: ' +A$, T$; B2 &
			! PRINT RECORD/CLUSTER ID &
	&

2048	!************************************************************* &
	!  CHOP D$ INTO STRINGS <= LINE LENGTHS - PRINT EACH &
	!************************************************************* &

2050		IF LEN (D$)<=N% THEN &
			GOTO 2070 &
		ELSE	I$=LEFT(D$,N%) &
	\		D$=RIGHT(D$,N%+1%) &
	\		GOSUB 10000 &

2060		GOTO  2050
2070		I$=D$ &
	\	GOSUB 10000
2080	NEXT B2 &
		! DO NEXT BLOCK OR CLUSTER &
	&
	&

2081	I1$=SYS(CHR$(0%)) &
		! CANCEL ^O &

2083	B=B2+1. &
		! UP AFTER LOOP FOR NEXT CLUSTER/BLOCK &

2085	PRINT 'Next Starting '+T$+' <'; NUM1$(B); ' (Next)>'; &
	\ INPUT LINE I1$ &
	\ I1$=CVT$$(I1$,4%) &
	\ INPUT 'How Many <1>'; B1 &
	\ B1=1. IF B1=0. &
	\ B=VAL(I1$) IF I1$<>'' &
	\ IF B<1. THEN &
		PRINT 'Invalid Number or Range' &
	\	GOTO 2083 &
	&
		! GET INPUT FOR MORE BLOCKS OR CLUSTERS &
		! MAKE SURE ARE VALID &

2098	GOTO 2020 &
		! DO NEXT BUNCH &

2100	! &
	!		---END OF MAIN LOOP--- &
	! &

10000	! &
	&
	&
	!	PRINT "I$" IN ALL SELECTED REPRESENTATIONS &
	&
	&

10010	I1$=XLATE (I$,E7$)	!GET RID OF NON PRINTS &

10020	PRINT #2, '---------' &
	\ PRINT #2, &
	\ PRINT #2, 'ASCII', I1$ &

10030	IF H% THEN &
		PRINT #2, ' HEX',XLATE(I$,E8$) &
	\	PRINT #2, ' HEX', XLATE(I$,E9$) &
	\	PRINT #2, &
			! PRINT 2ND AND 3RD HEX LINES,SPACE A LINE &

10040	IF O% THEN &
		PRINT #2, 'O B', XLATE(I$,E1$) &
	\	PRINT #2, 'C Y', XLATE(I$,E2$) &
	\	PRINT #2, 'T T', XLATE(I$,E3$) &
	\	PRINT #2, &
			! PRINT 3 LINES FOR OCTAL REPRESENTATION &

10045	IF W% THEN &
		R1$,R2$,R3$='' &
	\	FOR J%=1% TO LEN(I$) STEP 2% &
	\		J1%=SWAP%(CVT$%(MID(I$,J%,2%))) &
	\		R4$='0' &
	\		R4$='1' IF J1%<0% &
	\		J1%=J1% AND 32767% &
	\		R6$=NUM1$(J1% AND 7%) &
	\		R5$=NUM1$((J1%/512%) AND 7%) &
	\		R8$=NUM1$((J1%/8%) AND 7%) &
	\		R7$=NUM1$((J1%/4096%) AND 7%) &
	\		R9$=NUM1$((J1%/64%) AND 7%) &
	\		R1$=R1$+R5$+R6$ &
	\		R2$=R2$+R7$+R8$ &
	\		R3$=R3$+R4$+R9$ &
	\	NEXT J% &
	\	PRINT #2, 'O W', R3$ &
	\	PRINT #2, 'C R', R2$ &
	\	PRINT #2, 'T D', R1$ &
	\	PRINT #2, '  ', U2$ &
	\	PRINT #2, &
		! DO OCTAL WORD DUMP &
	&

10048	IF C% THEN &
		R1$,R2$,R3$='' &
	\	FOR J%=1% TO LEN(I$) STEP 2% &
	\		J1%=CVT$%(MID(I$,J%,2%)) &
	\		R4$='0' &
	\		R4$='1' IF J1%<0% &
	\		J1%=J1% AND 32767% &
	\		R6$=NUM1$(J1% AND 7%) &
	\		R5$=NUM1$((J1%/512%) AND 7%) &
	\		R8$=NUM1$((J1%/8%) AND 7%) &
	\		R7$=NUM1$((J1%/4096%) AND 7%) &
	\		R9$=NUM1$((J1%/64%) AND 7%) &
	\		R1$=R1$+R5$+R6$ &
	\		R2$=R2$+R7$+R8$ &
	\		R3$=R3$+R4$+R9$ &
	\	NEXT J% &
	\	PRINT #2, 'O W C', R3$ &
	\	PRINT #2, 'C R V', R2$ &
	\	PRINT #2, 'T D T', R1$ &
	\	PRINT #2, '  ', U2$ &
	\	PRINT #2, &
		! DO OCTAL WORD CVT DUMP &

10050	IF D% THEN &
		PRINT #2, ' D', XLATE(I$,E4$) &
	\	PRINT #2, ' E', XLATE(I$,E5$) &
	\	PRINT #2, ' C', XLATE(I$,E6$) &
	\	PRINT #2, &
			! PRINT DECIMAL REPRESENTATION &

10060	IF R% THEN &
		R1$,R$='' &
	\	R$=R$+RAD$(SWAP%(CVT$%(MID(I$,J%,2%)))) &
			FOR J%=1% TO LEN(I$) STEP 2% &
	\	R1$=R1$+'-'+MID(R$,J%,1%) &
			FOR J%=1% TO LEN(R$) STEP 3% &
	\	PRINT #2, ' R', R1$ &
	\	R1$='' &
	\	R1$=R1$+' '+MID(R$,J%,1%) &
			FOR J%=2% TO LEN(R$) STEP 3% &
	\	PRINT #2, ' A', R1$ &
	\	R1$='' &
	\	R1$=R1$+' '+MID(R$,J%,1%) &
			FOR J%=3% TO LEN(R$) STEP 3% &
	\	PRINT #2, ' D',R1$ &
			! PRINT RAD50 FOR EVERY WORD &

10100	RETURN &
		! END OF PRINT ROUTINE &
	&

19000	! &
	&
	&
	!	E R R O R    H A N D L I N G &
	&
	&

19005	IF ERR=11 THEN IF ERL<>2030 THEN &
		RESUME 19100 &
		ELSE PRINT 'End of File Reached' &
	\	RESUME 2010 &
		! TRAP FOR CONTROL Z THEN EXIT NORMALLY &
		! IF HE WENT BEYOND EOF, START OVER &
	&

19100	CLOSE 1,2 &
	\ ON ERROR GOTO 0 &
		!DONT DO ANY ERROR RECOVERY &

32767	END
