2!		PROGRAM		: DIRECT.BAS
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

21	! VER/ED	EDIT DATE	REASON &
	! V9.0-07	26-AUG-84	(PRL) Update for multiple privileges &
	! V9.1-04	25-Jun-85	(VAM) Support for new-format boot &
	! V9.7-07	24-Apr-89	(JJT) Fix system disk #'s > 9 &
	! V10.0-F	07-Nov-89	(FEK) Add BACKUP and IGNORE flags &
	! &

50	DIM S$(32), S1%(32), S2%(32), V%(30), P%(25), &
	    D$(10), FCB%(10), FILE.ID%(128), FILE.PPN%(128), FCB.ROOT%(10%) &
	! S$()		LIST OF VALID SWITCHES. &
	! S1%()		FIRST FLAG WORD FOR EACH SWITCH. &
	! S2%()		SECOND FLAG WORD FOR EACH SWITCH. &
	! V%()		FOR SYS CALL INFO. &
	! P%()		INFO FOR LAST FILE LOOKED UP. &
	! D$()		DEVICE NAME(S) TO SCAN. &
	! DV$		DEVICE NAME USED FOR WILDCARD LOOKUP. &
	! DV%		DEVICE UNIT NUMBER FOR WILCARD LOOKUP. &
	! N0%		ACCOUNT INDEX FOR WILDCARD LOOKUP. &
	! FCB%()	START OF OPEN LIST FOR DISK(S) IN D$(). &
	! FILE.ID%()	LIST OF OPEN FILE ID'S (LARGE FILE SYSTEM ONLY). &
	! FILE.PPN%()	LIST OF OPEN FILE PPN'S (LARGE FILE SYSTEM ONLY). &

55	DIM #1, U%(3583,7) &
	! UFD AS VIRTUAL ARRAY &

65	I$="V10.1-A" &
		! SET UP VERSION/EDIT # &
	\ PRIV.OFF$=CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ TEMP$=SYS(PRIV.OFF$) &
		! DEFINE STRINGS TO TURN PRIVILEGES ON AND OFF. &
		! DROP TEMPORARY PRIVILEGES. &

70	PRINT "DIRECT	";I$;CHR$(9%); &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)),3%),4%) &
	! PRINT THE SYSTEM HEADER. &

80	CHANGE SYS(CHR$(12%)) TO V% &
	\ PKG.LOC$="["+NUM1$(V%(6%))+","+NUM1$(V%(5%))+"]" &
	\ PKG.LOC$="_"+CHR$(V%(23%))+CHR$(V%(24%)) &
	     +NUM1$(V%(25%))+":"+PKG.LOC$ &
		IF V%(26%) AND 1% &
	\ IF V%(3%)+SWAP%(V%(4%))<>15%*2% THEN &
		PRINT "?Please 'RUN DIRECT'" &
	\	GOTO 32760 &
		! BUILD NAME OF DEVICE AND ACCOUNT OF LAST OPENED FILE. &
		! WE MUST HAVE COME FROM A COMPILED FILE SO WE CAN BE &
		! SURE THAT THIS NAME IS REALLY OUR PACKAGE LOCATION. &

100	GOTO 300 IF ENTRY.TYP% &
	\ K%=3% &
	\ OPEN "_KB:DIRECT.CMD" FOR INPUT AS FILE K% &
	\ GOSUB 700 &
	! ENTRY FROM "RUN"; K%=3% AS FLAG &

200	PRINT #K%,FNC0$(K%);"#"; &
	\ INPUT LINE#K%, L$ &
	\ PRINT #K%,FNC$(K%); &
	\ L$=CVT$$(L$,254%) &
	! INPUT LINE AND SQUISH OUT EVERYTHING &

300	DIM B%(200) &
	\ BK.SIZE%=200% &
	! B%()		TO HOLD LINKS FOR BACKWARDS LISTINGS. &

310	TEMP$=SYS(PRIV.ON$) &
	\ P0%=PEEK(PEEK(PEEK(520%)+8%)+24%) &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ P1%=SWAP%(P0%) AND 255% &
	\ P$=FNP$(P0%) &
	! BUILD DEFAULT PPN AND REMEMBER PROJ# &

320	WREAD.PRIV% = FNPRIV%("WREAD") &
	\ GREAD.PRIV% = FNPRIV%("GREAD") &
	\ DEVICE.PRIV% = FNPRIV%("DEVICE") &
	! SET FLAGS FOR USER PRIVILEGES &

340	O$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ O$="_KB:DIRECT" &
	\ O%=0% &
	! ENABLE CTRL/C TRAPPING. &
	! DEFAULT OUTPUT DEVICE -- O%=0 IF DEFAULT &

350	CHANGE SYS(CHR$(6%)+CHR$(-3%)) TO V% &
	\ JOB%=V%(1%)/2% &
	\ C2% = V%(5%) + SWAP%(V%(6%)) &
	\ C3% = V%(19%) + SWAP%(V%(20%)) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-12%)) TO V% &
	\ C1% = V%(5%) + SWAP%(V%(6%)) &
	\ C4% = V%(9%) + SWAP%(V%(10%)) &
	\ FCBLST%= V%(29%) + SWAP%(V%(30%)) &
	! C1-DEVNAM &
	! C2-DEVCNT &
	! C3-UNTCNT &
	! C4-DEVOKB &

400	GOTO 500 IF FNP% &
	\ GOTO 9000 IF S2%=16% &
	\ GOSUB 2000 &
	! PARSE LINE AND DO IT &

500	IF LEN(L$) THEN 400 &
	ELSE	CLOSE 1%,2% &
	\	IF K% THEN 200 ELSE 32760 &
	! CONTINUE UNTIL STRING DIGESTED, THEN CLOSE UP SHOP &

700	READ S0% &
	\ READ S$(I%),S1%(I%),S2%(I%) FOR I%=1% TO S0% &
	\ ON ERROR GOTO 19000 &
	\ RETURN &
	! INITIALIZE SWITCHES AND FUNNY LIBRARY PPN CHARS &

799	DATA 32
800	DATA	/SI,9,0,	/SZ,9,0,	/AL,9,2048,	/PR,17,0
810	DATA	/CL,257,0,	/OP,1025,0,	/PO,2049,0,	/W,1,4
820	DATA	/RT,513,0,	/DA,65,0,	/LA,33,0,	/TI,193,0
830	DATA	/NA,3,0,	/EX,5,0,	/MD,1,32,	/HE,0,16
840	DATA	/SA,1,128,	/OA,1,256,	/BR,1,64,	/FU,7161,33
850	DATA	/HD,0,1,	/SU,0,2,	/BK,1,8,	/F,1,64
860	DATA	/DI:S,7161,161,	/S,7161,161,	/DI,89,1,	/N,1,512
870	DATA	/LI:S,7161,161,	/AT,1,128,	/LI,89,1,	/TY,5,0
900	! &
	! STANDARD SWITCHES ARE: &
	!     DESCRIPTION		S1%		S2% &
	! /NA	NAME+NO EXT		1,2		0 &
	! /EX	NAME+EXT		1,4		0 &
	! /TY	NAME+EXT		1,4		0 &
	! /SI	NAME, SIZE		1,8		0 &
	! /SZ	NAME, SIZE		1,8		0 &
	! /PR	NAME, PROT CODE		1,16		0 &
	! /LA	NAME, LAST ACCESS DATE	1,32		0 &
	! /DA	NAME, CREATION DATE	1,64		0 &
	! /TI	NAME, CRE. DATE+TIME	1,64,128	0 &
	! /CL	NAME, CLUSTER SIZE	1,256		0 &
	! /RT	NAME, RUN-TIME SYSTEM	1,512		0 &
	! /OP	NAME, OPEN COUNT	1,1024		0 &
	! /AL	NAME, ALLOCATED SIZE	1,8		2048 &
	! /PO	NAME, FILE POSITION	1,2048		0 &
	! /HD	COLUMN HEADER FOR ABOVE 0		1 &
	! /SU	SUMMARY FOR ACCOUNT(S)	0		2 &
	! /W	NAME, USE WIDTH OF PAGE	1		4 &
	! /BK	NAME, BACKWARDS LISTING	1		8 &
	! /HE	PRINT HELP MESSAGE	0		16 &
	! /MD	NAME, MARKED FOR DELETE	1		32 &
	! /BR	NAME, BRIEF SUMMARY	1		2,64 &
	! /F	NAME, FAST SUMMARY	1		2,64 &
	! /SA	NAME, SYMBOL ATTRIBUTES	1		128 &
	! /AT	SAME AS /SA		1		128 &
	! /OA	NAME, OCTAL ATTRIBUTES	1		256 &
	! /N	NAME, ALL BUT WILDCARD	1		256 &
	! &
	! USEFULL COMBINATIONS ARE: &
	! &
	! /DI	NA, SI, PR, DA		1,8,16,64	0 &
	! &
	! /LI	SAME AS /DI &
	! &
	! /DI:S	NA, SI, PR, LA, DA,	1,8,16,32,64, &
	!	TI, CL, RT,		128,256,512, &
	!	OP, LO, HD, SU, MD, SA	2048,4096	1,2,32,128 &
	! &
	! /S	SAME AS /DI:S &
	! &
	! /FU	FULL - SLOW WITHOUT ATTRIBUTES &
	! &
	! &
	! BITS IN S1% GOVERN THE LISTING FOR EACH FILE AS FOLLOWS: &
	! &
	!    1	NAME AND EXTENSION (SEE BELOW) &
	!    2	/NA SPECIFIED (SEE BELOW) &
	!    4	/EX SPECIFIED (SEE BELOW) &
	!    8	SIZE &
	!   16	PROTECTION CODE &
	!   32	DATE OF LAST ACCESS &
	!   64	DATE OF CREATION &
	!  128	TIME OF CREATION &
	!  256	CLUSTER SIZE &
	!  512	RUN-TIME SYSTEM NAME &
	! 1024	OPEN COUNT &
	! 2048	POSITION &
	! 4096	OPEN COUNT IF OUTPUT DEVICE WIDTH IS WIDE ENOUGH. &
	! &
	! BIT 2 IS SET ONLY BY THE /NA SWITCH AND MEANS "DO NOT PRINT &
	!	THE EXTENSION UNLESS /EX (WHICH SETS BIT 4) IS EXPLICITLY &
	!	SPECIFIED". &
	! BIT 4 MEANS "ALWAYS PRINT THE EXTENSION EVEN IF /NA SPECIFIED". &
	! AFTER THE BITS ARE SET FROM THE SWITCHES, BIT 4 IS SET &
	!	IF BIT 1 IS SET UNLESS BIT 2 IS ON AND BIT 4 IS OFF &
	!	WHICH INDICATES NAME BUT NOT EXTENSION TO BE PRINTED. &
	! AFTER THE SWITCH DECODING STAGE, BIT 4 MEANS PRINT EXTENSION &
	!	AND BIT 2 IS IGNORED. &
	! &
	! IF BIT 4096 IS SET, THEN 1024 WILL BE SET IF THE OUTPUT DEVICE &
	!	IS WIDE ENOUGH TO PRINT THE OPEN COUNT IN A SLOW LISTING. &
	! AFTER THE SWITCH DECODING PHASE, BIT 1024 MEANS PRINT OPEN COUNT &
	!	AND BIT 4096 IS IGNORED. &
	! &
	! &
	! BITS IN S2% FLAG THE FOLLOWING: &
	! &
	!    1	HEADER &
	!    2	SUMMARY &
	!    4	LIST ACCROSS THE WIDTH OF THE PAGE &
	!    8	BACKWARDS LISTING &
	!   16	HELP MESSAGE &
	!   32	LIST FILES MARKED FOR DELETION &
	!   64	SHORT (BRIEF OR FAST) SUMMARY &
	!  128	SYMBOLIC ATTRIBUTES &
	!  256	OCTAL ATTRIBUTES &
	!  512	LIST FILES IN ACCOUNT(S) THAT DON'T MATCH SPEC &
	! 2048	LIST ALLOCATED SIZE (ROUND UP TO NEXT CLUSTER) &

1000	! COMMAND LINE PARSER &
	! HANDLES FILE NAMES AND SWITCHES &
	! RETURNS 0 IF NO ERROR, <>0 OTHERWISE &

1010 	DEF* FNP% &
	\ FNP%, N9% = 0% &
	\ R$,S$,U$,DEVICE$,F$,N$="" &
	\ Q= FNSUBSTITUTE("(","[")+FNSUBSTITUTE(")","]") &
	! SUBSTITUTE BRACKETS FOR PARENS, IF ANY &

1050	I%=FNI%('=') &
	\ I1%=FNI%('<') &
	\ GOTO 1100 IF (I% OR I1%)=0% &
	\ GOTO 1065 IF I%=0% &
	! SKIP OUTPUT SPEC ROUTINE IF NO OUTPUT SPEC &
	! COULD POSSIBLY EXIST. &
	! CHECK FOR PROTECTION CODES IF '<' PRESENT. &

1055	O$=LEFT(L$,I%-1%) &
	\ L$=RIGHT(L$,I%+1%) &
	\ IF INSTR(1%,O$,".")=0% THEN &
		I%=INSTR(1%,O$,"<") &
	\	I%=INSTR(1%,O$,"/") UNLESS I% &
	\	IF I%=0% THEN O$=O$+".DIR" &
		ELSE O$=LEFT(O$,I%-1%)+".DIR"+RIGHT(O$,I%) &
	! OUTPUT SPEC IS PINPOINTED - BUILD OUTPUT SPEC, &
	! INSERTING DEFAULT EXTENSION IF NECESSARY. &

1060	GOTO 1075 &
	! GO CHECK SPEC. &

1065	I2%=INSTR(I1%+1%,L$,"<") &
	\ I3%=FNI%('>') &
	\ GOTO 1100 IF I3%<>0% AND I2%=0% &
	\ I%=I1% &
	\ I%=I2% IF I3%<>0% AND I3%<I2% &
	\ GOTO 1055 &
	! SKIP REST OF OUTPUT SPEC ROUTINE IF WE HAVE X<PC>. &
	! PINPOINT OUTPUT SPEC - WE HAVE ONE OF THE FOLLOWING: &
	!	X<INP. SPEC &
	!	X<INP. SPEC<PC> &
	!	X<PC><INP. SPEC &
	!	X<PC><INP. SPEC<PC> &

1075	CHANGE SYS(CHR$(6%)+CHR$(-10%)+O$) TO V% &
	\ GOTO 1510 IF V%(28%) AND 128% &
	\ GOTO 1550 IF (V%(28%) AND 1%)=0% AND (STATUS AND 255%)=14% &
	\ OPEN O$ FOR OUTPUT AS FILE 2% &
	\ GOTO 1550 IF (STATUS AND 255%)<16% AND (STATUS AND 8448%)=256% &
	\ P7%=72% &
	\ P7%=80% IF (STATUS AND 255%)=0% &
	\ P7%=ASCII(RIGHT(SYS(CHR$(6%)+CHR$(-8%)+CHR$(2%)),20%))-1% &
		IF STATUS AND 2048% &
	\ O%=2% &
	! PUSH OUTPUT THRU FILE NAME SCAN &
	! CATCH ERRORS, ELSE OPEN IT UP &
	! IF DEVICE KEEPS HORIZONTAL POSITION THEN GET ITS WIDTH, &
	!  ELSE ASSUME 72% &

1100	P7%=ASCII(RIGHT(SYS(CHR$(6%)+CHR$(-8%)+CHR$(0%)),20%))-1% &
		UNLESS O% &
	\ PRINT #O%, FNC0$(O%); &
	\ I%=FNI%(',') &
	\ GOTO 1175 UNLESS I% &
	\ I1%=FNI%('[') &
	\ GOTO 1150 UNLESS I1% AND I1%<I% &
	\ I%=INSTR(I%+1,L$,",") &
	\ GOTO 1175 UNLESS I% &
	! IF NO OUTPUT FILE SPECIFIED, GET WIDTH OF KB: &
	! RETURN CARRIAGE ON OUTPUT DEVICE (AND CANCELL CTRL/O). &
	! FIND COMMAS, IGNORE IF BETWEEN [...] &
	! IF FOUND, THEN SAVE REST OF LINE FOR LATER &

1150 	R$=RIGHT(L$,I%+1) &
	\ L$=LEFT(L$,I%-1) &
	! SAVE TEXT AFTER ',' FOR LATER; &
	! L$ NOW HAS ONE SUB-COMMAND &

1175 	I%=FNI%('/') &
	\ IF I% THEN &
		S$=RIGHT(L$,I%) &
	\ 	L$=LEFT(L$,I%-1) &
	! SEPARATE NAME IN L$ AND SWITCHES IN S$ &

1200	! HAVE DEV:NAME.EXT[PPN] IN L$ &
	! NOW PUSH IT THRU FILE SCAN TO GET IT CHECKED &
	! AND TO GET ANY LOGICAL OR PSEUDO LIB'S TRANSLATED. &

1210	CHANGE SYS(CHR$(6%)+CHR$(-10%)+L$) TO V% &
	\ V%(I%)=V%(I%)+SWAP%(V%(I%+1)) FOR I%=7% TO 11% STEP 2% &
	\ Q%=V%(29%)+SWAP%(V%(30%)) &
	\ H1%=-1% &
	\ IF Q% < 0% THEN &
		PRINT "?Invalid device specification" &
	\	GOTO 1500 &
	! PACK NAME.EXT INTO 3 RAD50 WORDS &
	! Q% HOLDS SCAN FLAGS &
	! H1% WILL HOLD THE HANDLER INDEX IF IT CAN BE DETERMINED. &
	! IF NO VALID DEVICE NAME FOUND, ERROR MSG AND TRY AGAIN &

1220	DEVICE$="_SY:" &
	\ GOTO 1230 UNLESS (Q% AND 4096%)<>0% &
	\ H1%=(STATUS AND 255%) IF Q%>=0% &
	\ DEVICE$="_"+CHR$(V%(23%))+CHR$(V%(24%)) &
	\ DEVICE$=DEVICE$+NUM1$(V%(25%)) IF V%(26%) &
	\ DEVICE$=DEVICE$+":" &
	! IF DEV: SEEN, THEN BUILD STRING FOR IT &

1230	U$=P$ &
	\ ULNK%,UAA%=0% &
	\ GOTO 1240 UNLESS (Q% AND 128%)<>0% &
	\ U0$,U1$="*" &
	\ U0$=FNN$(V%(6%),0%,0%) UNLESS V%(6%)=255% &
	\ U1$=FNN$(V%(5%),0%,0%) UNLESS V%(5%)=255% &
	\ U$="["+U0$+","+U1$+"]" &
	! BUILD PPN, WITH WILD CARDS; DEFAULT IS USER'S OWN &

1240	IF V%(7%)=0% OR V%(7%)=-1% THEN &
		N$="??????" &
	ELSE	N$=RAD$(V%(7%))+RAD$(V%(9%)) &
	! SET UP NAME--HANDLE NULL NAME=* &

1250	IF V%(11%)=-1% OR (V%(11%)=0% AND (Q% AND 8%)=0%) THEN E$="???" &
		ELSE IF (V%(11%)=0%) AND Q% AND 8% THEN E$="   " &
			ELSE E$=RAD$(V%(11%)) &
	! SET UP EXTENSION--	<DOT><NULL>=<NULL>; &
	!			<NODOT><NULL>=<DOT><STAR> &

1260	F$=CVT$$(N$+"."+E$,254%) &
	\ CHANGE N$+E$ TO N% &
	\ FOR I%=1% TO 9% &
	\	N%(I%)=0% IF N%(I%)=63% &
	\	N9%=4% IF N%(I%) &
	\ NEXT I% &
	! CHANGE '?' TO 0 IN EACH CHAR POSITION; &
	! N9%=4 IF NOT ALL WILD &

1400	S1%,S2%=0% &
	\ IF LEN(S$)=0% THEN &
		S1%=89% &
	\	S2%=1% &
	\ 	GOTO 1460 &
	! NULL CMD IS /NAME/EXT/SIZE/DATE/HDR &

1425	FOR I%=1% TO S0% &
	\	J%=INSTR(1%,S$,S$(I%)) &
	\	IF J% THEN &
			S1% = S1% OR S1%(I%) &
	\		S2% = S2% OR S2%(I%) &
	\		J1%=INSTR(J%+1,S$,"/") &
	\		J1%=32767% UNLESS J1% &

1455	S$=LEFT(S$,J%-1)+RIGHT(S$,J1%) IF J% &
	\ NEXT I% &
	\ GOTO 1530 IF LEN(S$) &
	! SET S1% AND S2% ACCORDING TO SWITCHES AND REMOVE THEM FROM STRING. &

1460	GOTO 1560 IF S2%=1% AND S1%=0% &
		! CAN'T USE /HD WITH NO OTHER SWITCHES (PATCH BY JAC ON 11/84) &
	\ GOTO 1530 IF S2% AND 16% AND (S2%<>16% OR S1%<>0% OR N9%=4%) &
		! CAN'T USE /HE WITH OTHER SWITCHES OR INPUT FILE SPEC. &
	\ S1%=S1% OR 4% IF (S1% AND 1%) > (S1% AND 2%) &
		! FLAG WHETHER TO PRINT EXTENSION &
	\ S1%=S1% OR 1024% IF (S1% AND 4096%) &
			AND (P7%>=80% OR (FCBLST%=0% AND P7%>=77%)) &
		! PRINT OPEN COUNT IF OUTPUT LINE LONG ENOUGH. &
	\ IF S2% AND 2% THEN S1% = 0% &
			   \ S2% = S2% AND -462% &
		! /SU SWITCH SUPERCEDES ALL OTHER LISTING OPTIONS &

1470	L$=R$ &
	! RESTORE WHAT'S LEFT OF INPUT STRING &

1475	FNEND &

1500	L$='' &
	\ GOTO 1540
1510	PRINT FNC0$(0%);"?Illegal file name: ";O$ &
	\ GOTO 1500
1520	PRINT FNC0$(0%);"?Illegal input file spec: ";L$ &
	\ GOTO 1500
1530	PRINT FNC0$(0%);"?Illegal switch ";S$ &
	\ FNP%=-1% &
	\ GOTO 1470
1540	FNP%=-1% &
	\ GOTO 1475
1550	PRINT FNC0$(0%);"?Output must be file structured: ";O$ &
	\ GOTO 1500
1560	PRINT FNC0$(0%);"?/HD illegal without other switches" &
	\ GOTO 1500 &

2000	! THIS ROUTINE DOES THE LOOKING IF DISK DIRECTORY, &
	! AND SWITCHES REQUESTS FOR DT AND MT AS APPROPRIATE &

2010	F8,F9,B8,B9,U9%,ENTRY.LEN%=0 &
	! F8 AND B8 ARE # OF FILES AND BLOCKS COUNTED IN UFD &
	! F9, B9 ARE GRAND TOTALS OF F8 & B8 &

2015	U$=P$ UNLESS LEN(U$) &
	\ U0$=U$ &
	! MAKE SURE WE HAVE UFD; REMEMBER IT &

2020	D0$=RIGHT(DEVICE$,2%) &
	\ Q$=LEFT(D0$,2%) &
	\ GOTO 3000 IF H1%=4% &
	\ GOTO 4000 IF H1%=14% &
	\ GOTO 2030 IF Q$="SY" OR H1%=0% &
	\ PRINT FNC0$(0%);"?Device not directory structured" &
	\ RETURN &
	! SORT OUT DEVICES AND DISPATCH &
	! H1%=4 IF DECTAPE, 14 IF MAGTAPE, 0 IF DISK &

2030	H1%, FCB.IDX% = 0% &
	\ IF ASCII(RIGHT(DEVICE$,4%))<>58% THEN &
		D$(0%) = DEVICE$ &
	\	D$(1%) = "" &
	\	FCB.ROOT%(0%)=0% &
	\	GOTO 2055 &
	! SPECIFIC DEVICE CALLED OUT. DON'T BOTHER WITH PUB STR &

2040	D%=0% &
	\ I1%= -2% &
	\ TEMP$=SYS(PRIV.ON$) &
	\ FOR I%= 0% TO C4%-2% STEP 2% &
	\	FOR J%= 0% TO PEEK(C2%+I%) &
	\		I1%=I1%+2% &
	\		GOTO 2050 IF PEEK(C3%+I1%) AND -12288% &
	\		Q%=PEEK(C1%+I%) &
	\		D$(D%)="_"+CHR$(Q%)+CHR$(SWAP%(Q%)) &
	\		D$(D%)=D$(D%)+CHR$(J%+48%)+":" IF J% < 10% &
	\		D$(D%)=D$(D%)+"1"+CHR$(J%+38%)+":" IF J% > 9% &
	\		D%=D%+1% &
	\		FCB.ROOT%(D%)=0% &
	! PUT NAME IN PUB STR LIST
2050		NEXT J% &
	\ NEXT I% &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ D$(D%)="" &

2055	D%=0% &
	\ GOTO 2060 IF DEVICE.PRIV% &
	\ TEMP$=SYS(PRIV.ON$) &
	\ WHILE LEN(D$(D%)) &
		\ GOTO 2057 IF D$(D%)="_SY0:" &
		\ I%,J%=0% &
		\ WHILE I%<C4% AND MID(D$(D%),2%,2%)<> &
					CVT%$(SWAP%(PEEK(C1%+I%))) &
			\ J%=J%+PEEK(C2%+I%)+1% &
			\ I%=I%+2% &
		\ NEXT &
		\ IF PEEK(C3%+(J%+ASCII(MID(D$(D%),4%,1%))-48%)*2%) AND 8192% &
			THEN I%=D% &
			\ WHILE LEN(D$(I%)) &
				\ D$(I%)=D$(I%+1%) &
				\ I%=I%+1% &
			\ NEXT &
	! IF USER IS NON-PRIVILEGED, &
	! GAIN TEMPORARY PRIVILEGES AND &
	! FOR EVERY DISK IN LIST; &
	!	(SKIPPING SY0:, WHICH CAN'T BE LOCKED, AND WHICH &
	!	IS OUTSIDE NORMAL TABLE RANGE) &
	!	SEE IF IT IS LOCKED &
	!	IF IT IS, REMOVE IT FROM THE LIST &

2057		D%=D%+1% &
	\ NEXT &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ IF LEN(D$(0%))=0% THEN &
		PRINT FNC0$(O%);"?Disk pack is restricted" &
		\ RETURN &
	! IF THERE ARE NO DISKS REMAINING IN LOOKUP LIST, REPORT THAT &
	! DISK IS LOCKED OUT AND EXIT &

2060	U$ = FNU$(U$) &
	\ F8,B8,D%=0% &
	\ G%=64% &
		! ALWAYS SKIP UFD ENTRIES. &
	\ G%=G% OR 128% UNLESS S2% AND 32% &
		! SKIP MARKED FOR DELETION UNLESS /MD OR /S. &
	\ G%=G% OR 4096% &
		IF FILE.PROJ% <> P1% &
			IF NOT WREAD.PRIV% &
		! READ OR RUN PROTECTED OUTSIDE GROUP &
		!	IF FILE NOT IN USER'S GROUP &
		!		IF USER LACKS WREAD PRIV &
	\ G%=G% OR 1024% &
		IF FILE.PPN% <> P0% &
			IF FILE.PROJ% = P1% &
				IF NOT (GREAD.PRIV% OR WREAD.PRIV%) &
		! READ OR RUN PROTECTED IN SAME GROUP &
		!	IF FILE NOT IN USER'S OWN ACCOUNT &
		!		IF FILE IN USER'S GROUP &
		!			IF USER LACKS GREAD OR WREAD PRIV &
	\ GOTO 2080 IF LEN(U$) &
	\ GOSUB 2600 &
	\ RETURN &
	! GET NEXT UFD IN LIST. IF END, THEN PRINT TOTALS AND RETURN &

2080	DEVICE$ = D$(D%) &
	\ D%=D%+1% &
	\ GOTO 2100 IF LEN(DEVICE$) &
	\ GOSUB 2500 &
	\ GOTO 2060 &
	! GET NEXT DEVICE IN LIST. IF END THEN PRINT TOTALS AND ON TO NEXT UFD &

2100	TEMP$=SYS(PRIV.ON$) &
	\ OPEN DEVICE$+U$ FOR INPUT AS FILE 1% &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ GOTO 2080 UNLESS SWAP%(CVT$%(MID(SYS(CHR$(12%)),13%,2%))) &
	\ U1%=U%(0%,0%) &
	\ F%=U%(31%,0%) &
	\ B%=0% &
	\ F8%=-1% &
	\ GOSUB 10000 IF FCBLST%<>0% AND FCB.ROOT%(D%)=0% AND (S1% AND 1032%) &
	\ GOTO 2270 IF S2% AND 8% &
	! OPEN UFD, SKIP IF NULL, GET CLUSTER, INIT POINTER &
	! IF LARGE FILE SYSTEM AND FIRST TIME THIS UNIT AND WE NEED OPEN INFO, &
	!	THEN GO FIND OUT WHAT FILES ARE OPEN ON THIS UNIT. &
	! SKIP IF BACKWARDS LIST. &

2120	U%=FNL%(U1%,F%) &
	\ GOTO 2080 UNLESS U% &
	\ USTAT%=U%(U%,4%) &
	\ FILE.ID%=U1% AND -16% &
	\ U1%=U%(U%,0%) &
	\ GOSUB 2250 UNLESS USTAT% AND G% &
	\ GOTO 2120 &
	! DO IT FOR ALL FILES IN UFD &

2249	! ROUTINE TO TAKE DATA OUT OF UFD, PUT IT IN P(), &
	! CALL FNW% TO CHECK FOR MATCH, AND 2300 TO LIST DATA IF A MATCH &

2250	P%(I%)=U%(U%,I%) FOR I%=1% TO 3% &
	\ P%(5%)=SWAP%(U%(U%,4%)) AND 255% &
	\ ULNK%=U%(U%,0%) &
	\ UAA%=U%(U%,6%) &
	\ R%=FNL%(UAA%,F%) &
	\ P%(4%)=U%(R%,2%) &
	\ P%(6%)=U%(R%,3%) &
	\ P%(7%)=U%(R%,4%) &
	\ P%(8%)=U%(R%,1%) &
	\ P%(9%),P8%=U%(R%,7%) &
	\ P%(10%)=U%(R%,5%) &
	\ P%(11%)=U%(R%,6%) &
	\ P%(I%)=0% FOR I%=12% TO 23% &
	\ P%(24%)=(U%(U%,5%) AND 255%)+(SWAP%(U%(U%,5%)) AND 255%) &
	\ P%(25%)=U%(FNL%(U%(U%,7%),F%),1%) &
	\ MSB.SI%=P%(11%) AND 255% AND P%(10%)=0% &
	\ GOSUB 10100 IF FCBLST%<>0% AND (S1% AND 1032%) &
	\ P%(4%)=(P%(4%)+P8%-1%) AND -P8% IF S2% AND 2048% &
	\ MSB.SI%=1% IF (S2% AND 2048%) AND (U%(R%,2%)<>0%) AND (P%(4%)=0%) &
	\ I%=U%(R%,0%) AND (NOT 15%) &
	\ IF I% THEN R%=FNL%(I%,F%) &
	\	P%(I%+11%)=U%(R%,I%) FOR I%=1% TO 7% &
	\	I%=U%(R%,0%) AND (NOT 15%) &
	\	IF I% THEN R%=FNL%(I%,F%) &
	\		P%(I%+18%)=U%(R%,I%) FOR I%=1% TO 5% &

2255	GOSUB 2300 IF FNW% &
	\ RETURN &
	! ORDER OF PARAMS IN P%() IS: &
	! 1,2,3	NAME.EXT &
	! 4	SIZE &
	! 5	PROT &
	! 6	CREATION DATE &
	! 7	CREATION TIME &
	! 8	ACCESS DATE &
	! 9	CLUSTER SIZE &
	! 10,11	RUN-TIME SYSTEM NAME, OR &
	!	(10)=0, (11)=FQSIZM &
	! 12-23	ATTRIBUTES &
	! 24	OPEN COUNT &
	! 25	FIRST BLOCK LOC &
	! USTAT% LOW BYTE IS UFD STATUS BYTE. &
	! MSB.SI% IS MOST SIGNIFICANT WORD OF FILE SIZE. &
	! ULNK% CONTAINS LINK TO NEXT ENTRY.  BIT VALUE 4 OF ULNK% &
	!	FLAGS THE FILE TO BE CACHED. &
	! UAA% CONTAINS LINK TO ACCOUNTING ENTRY.  BIT VALUE 4 OF UAA% &
	!	FLAGS THE FILE TO BE SEQUENTIALLY CACHED, IF CACHED. &

2270	U%=0% &
	\ FOR I%=1% TO BK.SIZE% &
	\	U%,B%(I%)=U%(U%,0%) &
	\	U%=FNL%(U%,F%) &
	\	GOTO 2275 UNLESS U% &
	\ NEXT I% &
	\ PRINT FNC0$(0%);"?Too many files for inverted directory listing" &
	\ RETURN &
	! BUILD REVERSE DIRECTORY LIST &

2275	FOR B%=I%-1% TO 1% STEP -1% &
	\	FILE.ID%=B%(B%) &
	\	U%=FNL%(FILE.ID%,F%) &
	\	USTAT%=U%(U%,4%) &
	\	GOSUB 2250 UNLESS USTAT% AND G% &
	\ NEXT B% &
	\ GOTO 2080 &
	! LIST DIRECTORY IN REVRSE ORDER &

2300	! ROUTINE ACCUMULATES TOTALS AND PRINTS PER SWITCHES S1% AND S2% &

2310	F8=F8+1 &
	\ B8=B8+FND(P%(4%))+65536.*FND(MSB.SI%) &
	\ F9=F9+1 &
	\ B9=B9+FND(P%(4%))+65536.*FND(MSB.SI%) &
	\ P2%=0% &
	! UPDATE TOTALS &

2320	RETURN UNLESS S1% &
	\ GOSUB 2700 &
	! SET UP TITLE IF NEEDED. &

2325	IF F8% THEN &
		F8% = 0% &
	\	PRINT #O%, D0$;U$ UNLESS P2% IF F8=1. &

2330	PRINT #O%, RAD$(P%(1%));RAD$(P%(2%)); IF S1% AND 1%
2350	PRINT #O%, ".";RAD$(P%(3%)); IF S1% AND 4%
2355	PRINT #O%, MID(" *",(USTAT% AND 128%)/64%,1%); IF S1% AND 5% &
	! IF BIT 7 OF THE UFD STATUS BYTE IS SET THEN THE &
	! FILE IS MARKED FOR DELETION. &

2360	IF S1% AND 8%	THEN &
		PRINT #O%, FNN$(P%(4%),MSB.SI%,7%); &
	\	IF H1% <> 0%	THEN &
			PRINT #O%, "   "; &
		ELSE	T$="" &
	\		T$=T$+"C" IF USTAT% AND 16% &
	\		T$=T$+"P" IF USTAT% AND 32% &
	\		T$=T$+"L" IF USTAT% AND 2% &
	\		PRINT #O%, T$;SPACE$(3%-LEN(T$)); &
		!PRINT THE FILE SIZE. &
		!IF BIT 1 OF THE UFD STATUS BYTE IS SET THEN &
		!THE FILE IS 'L'OCATED. &
		!IF BIT 4 OF THE UFD STATUS BYTE IS SET THEN &
		!THE FILE IS 'C'ONTIGUOUS. &
		!IF BIT 5 OF THE UFD STATUS BYTE IS SET THEN &
		!THE FILE IS 'P'ROTECTED (NON-DELETABLE). &

2370	PRINT #O%, "<";FNN$(P%(5%),0%,3%);">"; IF S1% AND 16% &
		! PROTECTION CODE. &

2380	IF S1% AND 32% THEN &
		P%(8%)=-1% UNLESS P%(8%) &
	\	PRINT #O%, " ";DATE$(P%(8%)); &
		! DATE OF LAST ACCESS. &

2390	IF S1% AND 64% THEN &
		P%(6%)=-1% UNLESS P%(6%) &
	\	PRINT #O%, " ";DATE$(P%(6%)); &
		! CREATION DATE. &

2400	PRINT #O%, " ";TIME$(P%(7%) AND 2047%); IF S1% AND 128% &
	\	F.FLG%=P%(7%)	! Store the extra file flags for later &
		! CREATION TIME. &

2410	PRINT #O%, FNN$(P%(9%),0%,4%); IF S1% AND 256% &
		! CLUSTER SIZE. &

2420	IF S1% AND 512% THEN &
		IF P%(10%) THEN &
			PRINT #O%, " ";RAD$(P%(10%));RAD$(P%(11%)); &
		ELSE	PRINT #O%, SPACE$(7%); &
		  ! PRINT RUN-TIME SYSTEM UNLESS THIS IS A LARGE FILE &
		  ! (AND THEREFORE DOESN'T HAVE ONE). &

2430	IF S1% AND 2048% THEN &
		IF P%(4%) OR MSB.SI% THEN PRINT #O%, FNN$(P%(25%),0%,6%); &
		ELSE PRINT #O%, " -----"; &
		  ! PRINT FILE POSITION, OR DASHES IF ZERO-LENGTH FILE. &

2440	GOTO 2450 UNLESS S1% AND 1024% &
	\	IF FCBLST%=0% THEN &
			PRINT #O%, FNN$(P%(24%),0%,3%); &
		ELSE	TEMP$=NUM1$(SWAP%(P%(24%)) AND 255%) &
	\		IF P%(24%)<>-1% THEN &
				PRINT #O%, FNN$(P%(24%) AND 255%,0%,3%); &
						"/";TEMP$; &
			ELSE	PRINT #O%, " ??/??"; &
		! IF SMALL FILE SYSTEM, PRINT THE PLAIN OPEN COUNT. &
		! IF LARGE FILE SYSTEM AND ALL OK, PRINT SPLIT OPEN COUNT: &
		!	NORMAL OPEN COUNT / READ-REGARDLESS OPEN COUNT. &
		! IF LARGE FILE SYSTEM AND PEEKS MESSED UP, PRINT "??/??". &

2445		PRINT #O%, MID("  W U WU",(USTAT% AND 12%)/2%+1%,2%); &
	\	PRINT #O%, SPACE$(2%-LEN(TEMP$)); IF FCBLST% &
		! IF WE PRINTED OPEN COUNT, APPEND: &
		! W IF 'W'RITE PRIVILEGES HAVE BEEN GRANTED (USTAT% AND 4%). &
		! U IF FILE OPENED FOR 'U'PDATE (USTAT% AND 8%). &

2450	GOSUB 2800 IF (S2% AND 4%) AND ENTRY.LEN%=0% &
		! FIGURE OUT HOW MANY ENTRIES WILL FIT ON ONE LINE &
		! IF WE ARE DOING A /W LISTING AND WE HAVEN'T ALREADY &
		! FIGURED IT OUT. &

2460	IF S2% AND 384% THEN &
		I1%=0% &
	\	FOR I%=23% TO 12% STEP -1% &
	\		I1%=I% IF I1%=0% AND P%(I%)<>0% &
	\	NEXT I% &
		! WE ARE TO PRINT OUT SOME KIND OF ATTRIBUTES &
		! (S2% AND 128%) OR (S2% AND 256%). &
		! SCAN THROUGH RETURNED ATTRIBUTES TO FIND OUT HOW MANY &
		! THERE ARE. &

2470	IF S2% AND 256% AND I1%<>0% THEN &
		PRINT #O%, IF CCPOS(O%) &
	\	PRINT #O%, " ";FNOCTAL$(P%(I%)); FOR I%=12% TO I1% &
	\	PRINT #O%, &
		! PRINT OCTAL ATTRIBUTES. &

2480	IF S2% AND 128% AND I1%<>0% THEN &
		PRINT #O%, IF CCPOS(O%) &
	\	I%=P%(12%) AND 15% &
	\	I%=5% IF I%>5% &
	\	PRINT #O%, " RF:";MID("UDFFIXVARVFCSTM???",I%*3%+1%,3%); &
	\	PRINT #O%, "=";NUM1$(FND(P%(20%))); IF P%(20%) &
		  ! RECORD FORMAT FROM ATTRIBUTE WORD 1, BITS 0-3. &
		  !  0 - UDF	UNDEFINED &
		  !  1 - FIX	FIXED &
		  !  2 - VAR	VARIABLE &
		  !  3 - VFC	VARIABLE WITH FIXED CONTROL FIELD &
		  !  4 - STM	STREAM ASCII &
		  !  5 - ???	NOT DEFINED &
		  !   ... &
		  ! 15 - ???	NOT DEFINED &
		  ! IF THERE IS A MAXIMUM SIZE IMPOSED, PRINT "=" MAXIMUM &
		  ! AFTER THE RECORD FORMAT. &
	&
	\	I%=P%(12%)/16% AND 15% &
	\	I%=3% IF I%>3% &
	\	PRINT #O%, TAB(12%);" FO:";MID("SEQRELIDX???",I%*3%+1%,3%); &
			TAB(22%);"USED:";FNN$(P%(17%),P%(16%),0%); &
				":";FNN$(P%(18%),0%,0%); &
			TAB(37%);" RECSI:";NUM1$(FND(P%(13%))); &
		  ! FILE ORGANIZATION FROM ATTRIBUTE WORD 1, BITS 4-7 &
		  !  0 - SEQ	SEQUENTIAL &
		  !  1 - REL	RELATIVE &
		  !  2 - IDX	INDEXED &
		  !  3 - ???	NOT DEFINED &
		  !   ... &
		  ! 15 - ???	NOT DEFINED &
		  ! SHOW HOW MANY BLOCKS IN USE AND HOW MANY BYTES BEING &
		  ! USED IN LAST BLOCK. &
		  ! GIVE THE RECORD SIZE, IF FIXED, OR MAXIMUM RECORD SIZE, &
		  ! IF VARIABLE. &
	&
	\	I%=SWAP%(P%(12%)) AND 7% &
	\	I%=3% IF I%>4% &
	\	PRINT #O%, TAB(52%);"CC:";MID("FORIMP???PRN",I%*3%-2%,3%); &
			IF I% &
		  ! SPECIAL CARRIAGE CONTROL, IF ANY, BITS 8-10 OF WORD 1. &
		  !  0 -	NO SPECIAL CC; DON'T PRINT ANYTHING. &
		  !  1 - FOR	FORTRAN &
		  !  2 - IMP	IMPLIED CR-LF AT END OF EACH RECORD &
		  !  3 - ???	NOT DEFINED &
		  !  4 - PRN	PRINT FORMAT (VAX) &
		  !  5 - ???	NOT DEFINED &
		  !  6 - ???	NOT DEFINED &
		  !  7 - ???	NOT DEFINED &
	&
	\	PRINT #O%, TAB(62%);"NOSPAN"; IF P%(12%) AND 2048% &
		  ! NOSPAN IF BIT 11 OF ATTRIBUTE WORD 1. &
	&
	\	PRINT #O%, &
	\	I%=P%(19%) AND 255% &
	\	PRINT #O%, "  BK:";NUM1$(I%); IF I% AND I%<>1% &
		  ! BUCKET SIZE IF NOT DEFAULT OF 1 RECORD. &
	&
	\	I%=SWAP%(P%(19%)) AND 255% &
	\	PRINT #O%, TAB(13%);"HS:";NUM1$(I%); IF I% AND I%<>2% &
		  ! HEADER SIZE IF NOT DEFAULT OF 2 BYTES. &
	&
	\	I%=P%(21%) &
	\	PRINT #O%, TAB(22%);"EX:";NUM1$(I%); IF I% &
		  ! EXTENSION QUANTITY IF NOT DEFAULT. &
	&
	\	PRINT #O%, IF CCPOS(O%) &
		  ! PRINT SYMBOLIC ATTRIBUTES. &

2490	IF (((ULNK% OR UAA%) AND 4%)<>0% OR (F.FLG% AND (2048%+4096%))<>0%) &
		AND (S2% AND 128%)<>0% THEN &
		PRINT #O%, IF CCPOS(O%) &
	\	PRINT #O%, "   CACHE:"; &
		MID("OFF ON",(ULNK% AND 4%)+1%,3%);":"; &
		MID("RAN SEQ",(UAA% AND 4%)+1%,3%); &
			IF ((ULNK% OR UAA%) AND 4%)<>0% &
	\	PRINT #O%, "    NOBACKUP"; IF F.FLG% AND 4096% &
	\	PRINT #O%, "    IGNORE"; IF F.FLG% AND 2048% &
	\	PRINT #O% &
		  ! PRINT FILE CACHING INFO IF FLAGGED TO: &
		  ! IF BIT VALUE 4 IN LINK TO NEXT FILE IS SET, THEN &
		  !	FILE IS MARKED FOR CACHING. &
		  ! IF BIT VALUE 4 IN LINK TO ACCOUNTING ENTRY IS SET, &
		  !	THEN FILE IS MARKED FOR SEQUENTIAL CACHING. &
		  ! IF BIT VALUE 4096% is set in the creation time, &
		  !	then the file is set /NOBACKUP &
		  ! IF BIT VALUE 2048% is set in the creation time, &
		  !	then the file is set /IGNORE &
		  ! IF NEITHER IS SET, DON'T PRINT ANYTHING. &

2495	PRINT #O%, UNLESS CCPOS(O%)=0% &
		OR (S2% AND 4%) AND CCPOS(O%)+ENTRY.LEN%<=P7% &
	\ PRINT #O%, SEPARATE$; IF CCPOS(O%) &
	\ RETURN &

2499	! THIS ROUTINE PRINTS THE SUMMARY FOR A GIVEN UFD &
	! IF A SPECIFIC FILE WAS LOOKED FOR, AND NOT FOUND, THEN &
	! A COMPLAINT IS ISSUED &

2500	RETURN IF F8=0 AND (N9% AND 1% XOR 1%) &
	\ PRINT #O%, FNC0$(O%)
2510	IF F8=0 THEN &
	IF N9% AND 4% THEN &
		PRINT #O%, FNC0$(O%);'?No such file as ';F$;' on ';D0$;U$ &
	ELSE	PRINT #O%, FNC0$(O%);'?Directory of ';D0$;U$;' is empty' &
		UNLESS NODIR% &
	\	NODIR%=0%
2520	IF F8<>0 THEN U9%=U9%+1 &
	\ IF S2% AND 64% THEN &
		PRINT #O%, FNC0$(O%);NUM1$(F8);' File';FNS$(F8);" "; &
		 NUM1$(B8); ' Block';FNS$(B8) &
	  ELSE	PRINT #O%, FNC0$(O%);'Total of ';NUM1$(B8);' block';FNS$(B8); &
		\ PRINT #O%, ' allocated'; IF S2% AND 2048% &
		\ PRINT #O%, ' in ';NUM1$(F8);' file';FNS$(F8);' in ';D0$;U$
2530	PRINT #O%, &
	\ RETURN &

2599	&
	! THIS ROUTINE PRINTS THE GRAND TOTAL FROM A MULTI-UFD &
	! SEARCH. IF A SPECIFIC FILE WAS CALLED FOR, AND NOT FOUND, &
	! THEN A COMPLAINT IS ISSUED. &

2600	RETURN IF N9% AND 1% &
	\ RETURN IF U9%=1% &
	! DON'T BOTHER WITH GRAND TOTAL IF ONLY ONE DIRECTORY
2605	PRINT #O%, FNC0$(O%) &
	\ GOTO 2610 IF F9=0 &
	\ IF S2% AND 64% THEN &
		PRINT #O%, FNC0$(O%);"Totals:";F9;"File";FNS$(F9); &
			" ";NUM1$(B9);" Block";FNS$(B9) &
	ELSE	PRINT #O%, FNC0$(O%);"Grand total of ";NUM1$(B9);" block"; &
			FNS$(B9); &
		\ PRINT #O%, ' allocated'; IF S2% AND 2048% &
		\ PRINT #O%, " in";F9;"file";FNS$(F9);" in ";D0$;U0$
2610	PRINT #O%, FNC0$(O%);'?No such file as ';F$;' on ';D0$;U0$ IF F9=0. &
	\ PRINT #O%, &
	\ RETURN &

2700	RETURN UNLESS F8=1 AND (S2% AND 1%) &
		! ROUTINE TO PRINT TITLES IF FIRST FILE IN ACCOUNT
2710	PRINT #O%, ' Name ';	   IF S1% AND 1% &
	\ PRINT #O%, '.Typ';	   IF S1% AND 4% &
	\ PRINT #O%, ' ';	   IF S1% AND 5% &
	\ PRINT #O%, '   Size   '; IF S1% AND 8% &
	\ PRINT #O%, 'Prot ';	   IF S1% AND 16% &
	\ PRINT #O%, '  Access  '; IF S1% AND 32% &
	\ PRINT #O%, '   Date   '; IF S1% AND 64% &
	\ PRINT #O%, '   Time  ';  IF S1% AND 128% &
	\ PRINT #O%, ' Clu';	   IF S1% AND 256% &
	\ PRINT #O%, 'ster';	   IF (S1% AND 3840%)=256% AND (S2% AND 4%)=0% &
	\ PRINT #O%, '  RTS  ';	   IF S1% AND 512% &
	\ PRINT #O%, '  Pos ';	   IF S1% AND 2048% &
	\ IF S1% AND 1024% THEN &
		PRINT #O%, ' Open';	 UNLESS FCBLST% &
	\	PRINT #O%, ' Op/rr  ';   IF FCBLST% &

2720	P2% = -1% &
	\ GOSUB 2800 IF (S2% AND 4%) AND ENTRY.LEN%=0% &
		! FIGURE OUT HOW MANY ENTRIES ON ONE LINE OF A /W &
		! LISTING IF WE HAVEN'T ALREADY. &
	\ IF (S2% AND 4%) AND CCPOS(O%)+ENTRY.LEN%<=P7% THEN &
		PRINT #O%, SEPARATE$; &
	\	GOTO 2700 &
		  ! IF A WIDE LISTING, REPEAT THE HEADER FOR AS MANY &
		  ! ENTRIES AS WILL FIT ON A LINE. &

2730	IF CCPOS(O%)+LEN(D0$)+LEN(U$)+4%>P7% THEN &
		PRINT #O%, &
	ELSE	PRINT #O%, "    "; &
		!WILL THE DEVICE AND ACCOUNT FIT ON THE HEADER LINE? &

2740	PRINT #O%, D0$;U$ &
	\ RETURN &

2800	I%=CCPOS(O%) &
	\ J%=(P7%+2%)/(I%+2%) &
	\ ENTRY.LEN%=I%+(P7%-J%*I%)/J% &
	\ SEPARATE$=SPACE$(ENTRY.LEN%-I%) &
	\ RETURN &
		! CALCULATE HOW MANY ENTRIES WILL FIT ON ONE LINE. &
		! FIGURE THE AMOUNT OF SEPARATION BETWEEN ENTRIES FOR &
		! A "/W" LISTING (MUST BE AT LEAST 2 SPACES). &
		! GET THE TOTAL ENTRY LENGTH INCLUDING THIS SEPARATION. &

3000	U$,U0$ = "" &
	\ S1% = S1% AND (NOT 32%+128%+256%+512%+1024%+2048%) &
	\ S2% = S2% AND (NOT 128%+256%+2048%) &
	\ F8,B8 = 0. &
	!DECTAPE DIRECTORY &

3010	FOR R% = 66% TO 67% &
	\ I1$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &
	\ OPEN DEVICE$ FOR INPUT AS FILE 1% &
	\ I1$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(0%)) &
	\ GET #1%, RECORD R% &
	\ FIELD #1%, 2% AS I1$, 504% AS I1$ &
	\ I1$ = I1$ + NL$ &
	\ CLOSE 1% &
	! GET EACH DIRECTORY BLOCK &

3020	P8% = FNF%(0%) &
	\ IF P8%	THEN &
		P%(1%) = P8% &
	\	P%(2%) = FNF%(2%) &
	\	P%(3%) = FNF%(4%) &
	\	P%(4%) = FNF%(12%) &
	\	P%(5%) = FNF%(16%) AND 255% &
	\	P%(6%) = FNF%(6%) AND 32767% &
	\	P%(10%)=-1% &
			! FLAGS 'NOT A LARGE FILE.' &
	\	GOSUB 2300 IF FNW% &

3030	I1$ = RIGHT(I1$,19%) &
	\ GOTO 3020 IF LEN(I1$) &

3040	NEXT R% &
	\ GOTO 2500 &

3120	PRINT #O%, "?DECtape error - ";FNERR.TXT$ IF O% &
	\RETURN &

4000	! &
	!	M a g t a p e   D i r e c t o r y   L i s t i n g &
	! &
	ON ERROR GOTO 4900 &
	\ I$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ E%,P9%,TAPE.FLAGS%=0% &
	\ S1% = S1% AND (NOT 32%+128%+256%+512%+1024%+2048%) &
	\ S2% = S2% AND (NOT 128%+256%+2048%) &
	\ I1$=SYS(PRIV.ON$) &
	\ OPEN DEVICE$ FOR INPUT AS FILE 1% &
	\ I1$=SYS(PRIV.OFF$) &
	\ GOSUB 4540 &
	&
	\ I%=MAGTAPE(3%,0%,1%) &
	\ GET #1% &
	\ I1%=RECOUNT &
	! Set ^C trapping. &
	! Turn on temp privileges and try opening the unit in question. &
	! If it opens OK, drop privileges again. &
	! Then, get the first record from the magtape. &
	! If it's legal, it will be one of three lengths; 14, 80 or 2062 &
	!  bytes. &
	! A 14 byte record indicates a DOS format tape. &
	!  an 80 byte record is for ANSI, and &
	!  a 2062 byte record is a new-format bootable tape. &
	! If we try to read 2062 bytes into a 512-byte buffer, we'll get a &
	!  ?Magtape record length error (ERR=40), which will send us to Line &
	!  4900.  There, we'll fudge I1% and come back to 4002, so it'll get &
	!  treated just like a legal DOS label. &
	! If that went OK, go find how if it's assigned already, and whether &
	!  the system thinks it's DOS or ANSI. &

4002	I%=MAGTAPE(3%,0%,1%) &
	\ CLOSE 1% &
	\ IF I1% >= 80% THEN GOSUB 4500 &
	  	ELSE &
	IF I1%=14%	THEN GOSUB 4520 &
		ELSE	E%=129% &
			\ GOTO 4940 &
	! Rewind and close the tape. &
	! If the label is 80 bytes or longer, assume it's ANSI. &
	! Go assign it as such (if necessary). &
	! Otherwise, do the same thing for DOS. &
	! If it's neither >=80 nor 14 bytes, it's bogus - give an error!! &

4003	I%=INSTR(1%,U0$,",") &
	\ I1%=INSTR(1%,U0$,"]") &
	\ U0%,M0% = 0% &
	\ GOTO 4010 IF INSTR(1%,U0$,"*,*") &
	\ IF INSTR(1%,U0$,"*,")	THEN &
		M0%=255% &
	\	U0%=VAL(MID(U0$,I%+1%,I1%-I%-1%)) &
	\	GOTO 4010 &

4005	IF INSTR(1%,U0$,",*")	THEN &
		M0%=-256% &
	\	U0%=SWAP%(VAL(MID(U0$,2%,I%-2%))) &
	\	GOTO 4010 &

4007	M0%=-1% &
	\ U0%=VAL(MID(U0$,I%+1%,I1%-I%-1%))+ &
		SWAP%(VAL(MID(U0$,2%,I%-2%))) &

4010	GOTO 4900 IF E% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+DEVICE$) TO V% &
	\ V%(0%)=30% &
	\ V%(1%)=6% &
	\ V%(2%)=15% &
	\ V%(I%)=0% FOR I%=3% TO 6% &
	\ CHANGE V% TO I1$ &
	\ I1$=RIGHT(SYS(I1$),5%) &
	! EXIT IF ERROR ENCOUNTERED IN DETERMINING DOS/ANSI STATUS, &
	!   REWIND AND CLOSE THE MAGTAPE, &
	!   SET UP THE DIRECTORY LOOKUP SYS CALL, AND TRY TO GET &
	!   THE FIRST ENTRY. &
	&

4020	WHILE LEN(I1$) &
	\	P8%=FNF%(16%) &
	\	P%(I%)=FNF%(I%*2%) FOR I%=1% TO 4% &
	\	P%(5%)=FNF%(10%) AND 255% &
	\	P%(6%)=FNF%(12%) &
		!EXTRACT THE RELEVANT DIRECTORY INFORMATION. &

4030		GOTO 4050 IF M0% AND (U0% XOR P8%) &
	\	WHILE P8%<>P9% &
	\		F8%=-1% &
	\		GOSUB 2500 IF P9% &
	\		P9%=P8% &
	\		U$=FNP$(P9%) &
	\		F8,B8=0. &
		!SKIP THE ENTRY IF IT IS NOT WANTED.  IF THIS IS A &
		! NEW PPN, THEN PRINT TOTALS FOR THE OLD ONE. &

4040		NEXT &
	\	GOSUB 2300 IF FNW% &
		!PRINT THE ENTRY IF IT MATCHES THE FILE SPECIFICATION. &

4050		V%(3%)=V%(3%)+1% &
	\	WHILE V%(3%)>255% &
	\		V%(3%)=0% &
	\		V%(4%)=V%(4%)+1% &
	\	NEXT &
	\	CHANGE V% TO I1$ &
	\	I1$=RIGHT(SYS(I1$),5%) &
		!SET UP TO GET, AND THEN GET, THE NEXT ENTRY.  NOTE &
		! THAT THE ERROR HANDLER SETS THE LENGTH OF I1$ TO &
		! ZERO WHEN EOF IS ENCOUNTERED, CAUSING US TO DROP &
		! OUT OF THIS LOOP. &

4060	NEXT &
	\ GOSUB 2500 IF P9% &
	\ N9%=N9% AND -2% &

4070	ON ERROR GOTO 19000 &
	\ IF TAPE.FLAGS% AND 4% THEN &
		GOSUB 4600 &
		\ GOTO 2600 &
	! Reset our error trap. &
	! If we didn't have the tape in the first place, just deassign it. &
	! Close up shop. &

4080	IF TAPE.FLAGS% AND 16% THEN &
		TAPE.LABEL$=".DOS" &
		\ TAPE.LABEL$=".ANS" IF TAPE.FLAGS% AND 1% &
		\ GOSUB 4610 &
	! We'll get here if we DID have the tape assigned in the first place. &
	! If we need to change formats... &
	!  See if we need to assign it as DOS or ANSI. &
	!  If so, reassign it with the new format. &

4090	GOTO 2600 &
	! Go back for more. &

4500	! &
	!	Assign Tape as ANSI &
	! &
	IF (TAPE.FLAGS% AND 4%) OR ((TAPE.FLAGS% AND 1%)=0%) THEN &
	 TAPE.FLAGS%=TAPE.FLAGS% OR 16% IF (TAPE.FLAGS% AND 1%)=0% &
	\ TAPE.LABEL$=".ANS" &
	\ GOSUB 4610 &
	! If the tape is not already assigned, or it's in DOS mode, then &
	!  say we changed the tape format if it's currently DOS. &
	! Go reassign it. &

4505	OPEN DEVICE$ FOR INPUT AS FILE 1% &
	\ FIELD #1%, 4% AS I1$ &
	\ GET #1% WHILE I1$<>"VOL1" &
	\ FIELD #1%, 4% AS I1$, 6% AS I1$ &
	\ U0$="[*,*]  ("+I1$+")" IF LEN(CVT$$(I1$,254%)) &
	\ S1%=S1% AND NOT 16% &
	! LOOK FOR "VOL1".  IF FOUND, GET THE TAPE LABEL. (FAILURE &
	!  WILL TRAP AT EOF AND GOTO 4515). &

4510	CLOSE 1% &
	\ RETURN &

4515	E%=129% &
	\ GOTO 4510 &
	! FLAG NON-FILE STRUCTURED ERROR AND RETURN. &

4520	! &
	!	Assign Tape as DOS &
	! &
	RETURN IF ((TAPE.FLAGS% AND 4%)=0%) AND ((TAPE.FLAGS% AND 1%)=0%) &
	\ TAPE.FLAGS%=TAPE.FLAGS% OR 16% IF (TAPE.FLAGS% AND 1%) &
	\ TAPE.LABEL$=".DOS" &
	\ GOSUB 4610 &
	\ RETURN &
	! If the tape is not already assigned, or it's in DOS mode, then &
	!  say we changed the tape format if it's currently DOS. &
	! Go reassign it. &

4540	! &
	!	Get Information About the Current Status of the Tape &
	! &
	! &
	!	TAPE.FLAGS% AND 1%	 = 0%	DOS &
	!				<> 0%	ANSI &
	! &
	!	TAPE.FLAGS% AND 4%	 = 0%	DEVICE IS ASSIGNED (AND SHOULD &
	!					REMAIN ASSIGNED). &
	!				<> 0%	DEVICE IS NOT ASSIGNED (AND &
	!					SHOULD BE DEASSIGNED ONCE &
	!					WE'RE DONE GETTING THE &
	!					DIRECTORY. &
	! &
	!	TAPE.FLAGS% AND 16%	= 0%	Tape format was not changed. &
	!				<>0%	Tape format was changed. &
	! &
	CHANGE SYS(CHR$(6%)+CHR$(-8%)+CHR$(1%)) TO V% &
	\ TAPE.FLAGS%=0% &
	\ TAPE.FLAGS%=1% IF V%(28%) &
	\ TAPE.FLAGS%=TAPE.FLAGS% OR 4% UNLESS V%(10%) AND 128% &
	\ RETURN &
	! Get some DDB information. &

4600	! &
	!	Deassign the Tape &
	! &
	! This routine expects the device name "ddu:" to be passed in &
	!  DEVICE$. &
	! &
	CHANGE SYS(CHR$(6%)+CHR$(-10%)+DEVICE$) TO V% &
	\ V%(1%)=6% &
	\ V%(2%)=11% &
	\ V%(I%)=0% FOR I%=3% TO 22% &
	\ CHANGE V% TO I1$ &
	\ I1$=SYS(I1$) &
	\ RETURN &
	! Convert device designator to standard format, set up the &
	!  deassign call, deassign it, and return. &

4610	! &
	!	Assign/Reassign a Tape as DOS or ANSI &
	! &
	! This routine expects the string ".DOS" or ".ANS" to be passed in &
	! TAPE.LABEL$, depending on whether or not the tape should be &
	! assigned as DOS or ANSI. &
	! &
	! It also expects the device name "ddu:" to be passed in DEVICE$. &
	! &
	CHANGE SYS(CHR$(6%)+CHR$(-10%)+DEVICE$+TAPE.LABEL$) TO V% &
	\ V%(1%)=6% &
	\ V%(2%)=10% &
	\ V%(I%)=0% FOR I%=3% TO 10% &
	\ V%(7%)=JOB% &
	\ CHANGE V% TO I1$ &
	\ I$=SYS(PRIV.ON$) &
	\ I1$=SYS(I1$) &
	\ I$=SYS(PRIV.OFF$) &
	\ RETURN &
	! Nothing significant is returned. &

4900	! &
	!	E r r o r   T r a p p i n g &
	! &
	L%=ERL &
	\ GOTO 4940 UNLESS ERR &
	\ IF ERR=28%	THEN &
		GOSUB 4600 UNLESS TAPE.FLAGS% AND 4% &
	\	RESUME 32760 &
		!IF CTRL/C, DEASSSIGN MT: IF NECESSARY AND EXIT. &

4905	IF ERR=5% AND ERL=4050%   THEN &
		I1$="" &
	\	RESUME 4060 &
		!NORMAL END OF DIRECTORY. &

4910	IF	(ERR=11% AND ERL=4000%)	OR &
		(ERR= 5% AND ERL=4010%)	THEN &
			E%=130% &
	\		I1$="" &
	\		I%=MAGTAPE(3%,0%,1%) IF BUFSIZ(1%) &
	\		RESUME 4020 &
		!IF THERE'S NO INITIAL DIRECTORY ENTRY, OR FIRST &
		! THING FOUND IS EOF, THEN PROCEED NORMALLY. &

4915	IF ERL=4505%   THEN &
		E%=129% &
	\	RESUME 4940 &
		!NON-FILE STRUCTURED TAPE. &

4918	IF ERR=40% AND ERL=4000% THEN &
		I1%=14% &
		\ RESUME 4002 &
	! If we got a ?Magtape record length error, pretend it's a DOS &
	!  format tape and go on. &

4920	RESUME 4940 &
	!ASSUME A RANDOM MAGTAPE ERROR. &

4940	IF E%=129%	THEN &
		I1$="?Magtape is non-file structured." &
	  ELSE	IF E%<>130%	THEN &
			I1$="?Magtape error - "+FNERR.TXT$+" ("+NUM1$(L%)+")" &
		ELSE	E%=0% &
	\		I1$="" &
	\		GOTO 4020 &

4950	GOTO 19090 IF ERL>=2300% AND ERL<=2800% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT I1$ &
	\ ON ERROR GOTO 19000 &
	\ IF O%	THEN &
		PRINT #O% IF CCPOS(O%) &
	\	PRINT #O%, I1$ &
		!PRINT A PRETTY ERROR MESSAGE. &

4960	E%=0% &
	\ N9%=3% &
	\ GOTO 4070 &
	&

5000	! THIS ROUTINE TELLS IF NAME.EXT IN P%(1,2,3) &
	! MEETS WILD CARD SPECS. RETURNS <>0 IF YES, =0 IF NO &

5010	DEF* FNW% &
	\ FNW%=1% &
	! ASSUME WE ARE GOING TO MAKE IT &

5020	GOTO 5030 UNLESS N9% AND 4% &
	\ CHANGE RAD$(P%(1%))+RAD$(P%(2%))+RAD$(P%(3%)) TO N1% &
	\ GOTO 5040 IF N%(I%)<>N1%(I%) IF N%(I%) FOR I%=1% TO 9% &
	\ FNW%=0% IF S2% AND 512% &
		!EXCLUDE IF IT MATCHES BUT /N WAS SPECIFIED. &

5030	FNEND &

5040	FNW%=0% UNLESS S2% AND 512% &
	\ GOTO 5030 &
		!EXCLUDE IF NO MATCH UNLESS /N WAS SPECIFIED. &

6000	! THIS ROUTINE HANDLES WILD CARD PPN'S &
	! BY LOOKING UP EACH PPN IN THE SYSTEM MFD, AND &
	! RETURNING MATCHING PPN'S ON SUCCESSIVE CALLS. &
	&
	! RETURNS A NULL STRING WHEN NO MATCH &
	&
	&

6002	DEF* FNU$(Q$) &

6003	GOTO 6100 UNLESS N9% AND 8% &
	\ GOTO 6300 IF N9% AND 1% &
	! GO INIT FIRST TIME THRU (N9% AND 8) &
	! RETURN NULL ON 2ND CALL IF SPECIFIC PPN &

6030	ON ERROR GOTO 6250 &
	\ V$=SYS(CHR$(6%)+CHR$(25%)+CVT%$(SWAP%(N0%))+ &
	   CVT%$(SWAP%(U0%))+STRING$(16%,0%)+DV$) &
	\ N0%=N0%+1% &
	! SET UP WILDCARD ERR TRAP FOR END OF SEARCH &
	! CALL MONITOR WILDCARD SEARCH &
	! INCREMENT ACCOUNT INDEX &
	&

6040	FILE.PPN%=SWAP%(CVT$%(MID(V$,5%,2%))) &
	\ FILE.PROJ% = SWAP%(FILE.PPN%) AND 255% &
	\ V$=FNP$(FILE.PPN%) &
	\ TEMP$=SYS(PRIV.ON$) &
	\ OPEN DV2$+V$ FOR INPUT AS FILE #11% &
	\ Z%=SWAP%(CVT$%(MID(SYS(CHR$(12%)),13%,2%))) &
	\ CLOSE #11% &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ IF Z%=0% THEN GOTO 6030 &
	! ASSIGN NEXT PPN &
	! SAVE PROJ NO. &
	! SEE IF UFD EXISTS &
	! IF UFD IS EMPTY, GET NEXT ACCOUNT (MSB IS NOT REQUIRED FOR SIZE) &

6050	ON ERROR GOTO 19000 &
	\ FNU$=V$ &
	\ GOTO 6310 &
	! RESET THE NORMAL ERR TRAP &
	! ASSIGN FUNCTION AND EXIT &

6100	&
	! INITIALIZE THE FUNCTION FIRST TIME THRU &
	! M0% IS MASK FOR PPN: &
	!	[*,*]	65535 &
	!	[*,M]	65280 &
	!	[N,*]	255 &
	!	[N,M]	0 &
	! U0% IS PPN IN INTERNAL INTEGER FORM &

6110	I%=INSTR(1%,Q$,',') &
	\ I1%=INSTR(1%,Q$,']') &
	\ U0%=0% &
	\ M0%=-1% &
	\ N9%=N9% OR 8% &
	\ GOTO 6200 IF INSTR(1%,Q$,'*,*') &
	\ IF INSTR(1%,Q$,'*,') THEN &
		M0%=-256% &
	\	U0%=VAL(MID(Q$,I%+1,I1%-I%-1)) &
	\	GOTO 6200 &

6160	IF INSTR(1%,Q$,',*') THEN &
		M0%=255% &
	\	U0%=SWAP%(VAL(MID(Q$,2%,I%-2%))) &
	\	GOTO 6200 &

6180	N9%=N9% OR 1% &
	\ FILE.PPN%=VAL(MID(Q$,I%+1%,I1%-I%-1%))+SWAP%(VAL(MID(Q$,2%,I%-2%))) &
	\ FILE.PROJ% = SWAP%(FILE.PPN%) AND 255% &
	\ FNU$=Q$ &
	\ GOTO 6310 &
	! NOTHING WILD--RETURN ARGUMENT AS RESULT &

6200	U0%=U0%+M0% &
	\ N0%=0% &
	\ DV$=SYS(CHR$(6%)+CHR$(-10%)+DEVICE$) &
	\ DV$=MID(DV$,23%,4%) &
	\ DV2$=DEVICE$ &
	\ GOTO 6030 &
	! SET UP FINAL MASK FOR WILD CARD CALL &
	! SET DEVICE STRING AND UNIT NUMBER &
	! GO MAKE THE FIRST CALL &

6250	IF ERR<>5% THEN GOTO 19000 &
	ELSE ON ERROR GOTO 19000 &
	     \ RESUME 6300 &
	! ERROR HANDLER FOR WILDCARD SYS CALL. &
	! ERR 5 IS FOR END OF DIRECTORY &
	! RESET ERR TRAP FOR 19000 STANDARD ERR TRAP &
	! OTHERWISE GOTO UNKNOWN ERROR &

6300	FNU$="" &

6310	FNEND &
	! FNU$(Q$) &

7000	DEF* FNN$(LSB%,MSB%,Q1%) &
	\ Q$=NUM1$(MSB%*65536.+32768.+(LSB% EQV 32767%)) &
	\ Q$=SPACE$(Q1%-LEN(Q$))+Q$ IF Q1% &
	\ FNN$=Q$ &
	\ FNEND &
	! GET NUMERIC STRING (OPTIONAL RIGHT JUSTIFY) &

7050	DEF* FNI%(Q$)= INSTR(1%,L$,Q$) &

7100	DEF* FNL%(Q%,Q1%)= &
		(((Q% AND 3584%)/512%)*Q1%+ &
		(SWAP%(Q% AND -4096%)/16%))*32%+((Q% AND 496%)/16%) &

7150	DEF* FNP$(Q%)= &
		'['+FNN$(SWAP%(Q%)AND 255%,0%,0%)+","+ &
		FNN$(Q% AND 255%,0%,0%) +']' &

7200	DEF* FNSUBSTITUTE(Q1$,Q2$) &
	\ Q%=1% &
	! SEARCH FOR STRING Q1$ IN L$, AND SUBSTITUTE Q2$ IF FOUND &

7220	Q%=INSTR(Q%,L$,Q1$) &
	\ GOTO 7300 UNLESS Q% &
	\ L$=LEFT(L$,Q%-1)+Q2$+RIGHT(L$,Q%+1) &
	\ GOTO 7220 &
	! FIND IT IN STRING AND REPLACE IT; REPEAT &

7300	FNEND &

7350	DEF* FND(Q%) &
	\ FND=Q% &
	\ FND=65536.+Q% IF Q%<0% &
	\ FNEND &
	! RETURN ABSOLUTE VALUE OF AN INTEGER &

7400	DEF* FNS$(Q) &
	\ FNS$="s" &
	\ FNS$="" IF Q=1 &
	\ FNEND &
	! RETURN 'S' FOR MANY, BLANK FOR 1 FILE/BLOCK &

7450	DEF* FNC0$(C%) &
	\ FNC0$=SYS(CHR$(0%)) UNLESS C% &
	\ FNC0$=FNC$(C%) &
	\ FNEND &
	! CANCEL CTRL/O AND RESTORE CARRIAGE IF NEEDED &

7460	DEF* FNC$(C%) &
	\ FNC$="" &
	\ FNC$=CHR$(13%)+CHR$(10%) IF CCPOS(C%) &
	\ FNEND &
	! RESTORE CARRIAGE IF NEEDED &

7470	DEF* FNOCTAL$(Q%) &
	\ Q$="0" &
	\ Q0%=4096% &
	\ IF Q%<0% THEN &
		Q$="1" &
	\	Q%=Q%+32767%+1% &

7475	Q1%=Q%/Q0% &
	\ Q%=Q%-Q0%*Q1% &
	\ Q$=Q$+CHR$(48%+Q1%) &
	\ Q0%=Q0%/8% &
	\ IF Q0% THEN 7475 &

7480	FNOCTAL$=Q$ &
	\ FNEND &
	! OCTAL CONVERTER &

7600	DEF* FNF%(I%) = SWAP%(CVT$%(MID(I1$,I%+1%,2%))) &

8510	DEF* FNERR.TXT$=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%) &
	&

9000	TEMP$=SYS(PRIV.ON$) &
	\ OPEN "HELP$:DIRECT.HLP" FOR INPUT AS FILE 1% &
	\ TEMP$=SYS(PRIV.OFF$) &

9010	INPUT LINE #1%, Q$ &
	\ PRINT #O%, Q$; &
	\ GOTO 9010 &
	! READ AND PRINT FILE ON OUTPUT DEVICE &

9020	CLOSE #1%,#O% &
	\ IF K% THEN 200 ELSE 32760 &
	! CLOSE UP SHOP AND EXIT OR SOLICIT INPUT &

10000	! &
	! &
	! &
	!	G E T    F C B    I N F O R M A T I O N &
	! &
	! &
	! &
	! SEE WHAT FILES ARE OPEN ON THE DISK UNIT JUST OPENED ON CH 1, &
	! BY TRAVERSING THE FCB LIST ASSOCIATED WITH THAT UNIT. &
	! &
	TEMP$=SYS(PRIV.ON$) &
	\ I%=PEEK(PEEK(PEEK(PEEK(PEEK(520%))+2%)+8%)-4%) AND 255% &
		! GET FIP UNIT NUMBER FOR DISK OPEN ON CH 1. &
		! (LARGE FILE SYSTEM ONLY) &
		! PEEK(520)	-> OUR JDB. &
		! PEEK(JDB)	-> OUR IOB. &
		! PEEK(IOB+2)	-> WCB OF OPEN CHANNEL 1. &
		! PEEK(WCB+8)	-> FCB @ F$CLUS (OFFSET 28 IN FCB). &
		! PEEK(FCB+24)	-> FIP UNIT NUMBER (LOW BYTE). &
	\ FCB.ROOT%(D%)=FCBLST%+I%*2% &
	\ I%=PEEK(FCB.ROOT%(D%)) &
		! GET POINTER TO FIRST FCB FOR UNIT. &
	\ J%=FCB.IDX% &
	\ RETRY%=0% &
		! REMEMBER WHERE FCB.IDX IS NOW SO WE CAN RESTART SCANNING &
		! THROUGH THE LIST IF IT CHANGES WHILE WE ARE LOOKING AND &
		! WE GET AN ERROR. &
		! INITIALIZE THE RETRY COUNT. &

10010	ON ERROR GOTO 10040 &
		! SET UP LOCAL ERROR TRAP. &

10020	IF I% THEN &
		FILE.ID%(FCB.IDX%)=PEEK(I%+2%) AND -16% &
	\	FILE.PPN%(FCB.IDX%)=PEEK(I%+4%) &
	\	FCB.IDX%=FCB.IDX%+1% UNLESS PEEK(I%+12%) AND 64% &
	\	I%=PEEK(I%) &
	\	GOTO 10020 &
		  ! GET THE FILE'S ID AND PPN TO UNIQUELY IDENTIFY IT. &
		  ! INCREMENT THE INDEX UNLESS THIS IS AN OPEN UFD &
		  ! WHICH WE WILL IGNORE. &

10030	ON ERROR GOTO 19000 &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ FCB%(D%+1%)=FCB.IDX% &
	\ RETURN &

10040	GOTO 19000 UNLESS ERR=33% OR ERR=55% &
	\ RETRY%=RETRY%+1% &
	\ PRINT "%More than";FCB.IDX%-1%;"files open on ";DEVICE$ &
		IF ERR=55% AND RETRY%>2% &
	\ FCB.IDX%=J% &
	\ I%=PEEK(FCB.ROOT%(D%)) &
	\ RESUME 10020 IF RETRY%<=2% AND ERR=55% OR RETRY%<=5% &
	\ PRINT "%Open file information changing rapidly" IF ERR=33% &
	\ PRINT "%Open count and size information may not be accurate" &
	\ RESUME 10030 &
		! IF SUBSCRIPT OUT OF BOUNDS, RETRY TWICE JUST TO &
		! MAKE SURE WE WEREN'T ONLY IN AN INFINITE LOOP. &
		! AFTER THAT, ASSUME THAT THEY REALLY DO HAVE n FILES OPEN. &
		! IF ODD ADDRESS TRAP, RETRY 5 TIMES AND THEN GIVE UP. &

10100	! &
	!	UPDATE THE OPEN STATISTICS AND FILE SIZE FROM IN-CORE &
	!	WCB INFORMATION FOR AN OPEN FILE IN A LARGE FILE SYSTEM. &
	! &
	RETRY%=0% &
	\ GOTO 10110 IF FILE.PPN%=FILE.PPN%(I%) &
		   AND (FILE.ID% AND -16%)=FILE.ID%(I%) &
		FOR I%=FCB%(D%) TO FCB%(D%+1%)-1% &
	\ RETURN &
		! INITIALIZE THE RETRY COUNT. &
		! SCAN THROUGH OUR LIST OF OPEN FILES FOR THE UNIT &
		! TO SEE IF WE HAVE ONE OF THEM.  RETURN IF WE DON'T. &

10110	ON ERROR GOTO 10130 &
	\ TEMP$=SYS(PRIV.ON$) &
	\ I%=PEEK(FCB.ROOT%(D%)) &
	\ I%=PEEK(I%) FOR J%=1% UNTIL J%>2%*FCB%(D%+1%) OR I%=0% &
		OR (PEEK(I%+2%) AND -16%)=FILE.ID% AND PEEK(I%+4%)=FILE.PPN% &
			AND (PEEK(I%+12%) AND 64%)=0% &
		  ! SCAN THROUGH FCB LIST FOR UNIT UNTIL THE FILE &
		  ! IS FOUND AND NOT A UFD TYPE OR UNTIL THE END OF THE LIST &
		  ! (IN WHICH CASE FILE HAS BEEN CLOSED RECENTLY). &
	\ GOTO 10130 IF J%>2%*FCB%(D%+1%) &
		  ! IF WE KEEP 'PEEK'ING FOR TWICE AS LONG AS WE EXPECT TO &
		  ! FIND OPEN FILES, THEN WE'RE PROBABLY LOST. &
	\ IF I% THEN &
		P%(4%)=PEEK(I%+26%) &
	\	P%(24%)=PEEK(I%+14%) &
	\	USTAT%=PEEK(I%+12%) &
		  ! IF FILE WAS FOUND, UPDATE THE LOW-ORDER PART &
		  ! OF FILESIZE, OPEN COUNT, AND USTAT INFO. &

10120	ON ERROR GOTO 19000 &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ RETURN &

10130	RETRY%=RETRY%+1% &
	\ RESUME 10110 IF RETRY%<=5% &
	\ P%(24%)=-1% &
	\ RESUME 10120 &
		! ERROR HANDLER FOR ABOVE SUBROUTINE. &
		! RETRY FIVE TIMES ON AN ODD ADDRESS TRAP ERROR &
		!	OR AN INFINITE PEEKING LOOP. &

15000	! &
	! &
	!		C H E C K    P R I V I L E G E S &
	! &
	! &
	DEF FNPRIV%(PRIV$) &
\	CHANGE SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+PRIV$) TO V% &
\	FNPRIV% = (V%(3%) = 0%) &
\	FNEND &
	! Check to see if job currently has privilege named &
	! If privileged then return -1%, else return 0% &
	&

19000	! &
	! &
	!	E R R O R   H A N D L I N G &
	! &
	! &
	TEMP$=SYS(PRIV.OFF$) &
	\ RESUME 32760 IF ERR=28% &
	! ^C SHOULD STOP IT REAL FAST. &

19010	IF ERR=11% AND ERL=200% THEN RESUME 32760 &
	! ^Z ON KEYBOARD MEANS QUIT!!! &

19020	IF ERR=2% AND ERL=1210% THEN RESUME 1520 &
	! BAD FILE NAME TO RIGHT OF = &

19030	IF (ERR=2% OR ERR=6%) AND ERL=1075% THEN RESUME 1510 &
	!BAD FILE NAME OR DEVICE &

19040	IF (ERR=5% OR ERR=10%) AND ERL=2100% THEN &
		PRINT #O% IF D%=1% &
		\ PRINT #O%, FNC0$(O%);"?No directory for ";U$; &
			" on ";RIGHT(DEVICE$,2%) IF D%=1% &
	\	NODIR% = 1% IF D%=1% &
	\	RESUME 2080 &

19050	IF ERR=5% AND ERL=9000%	THEN &
		PRINT "?No help available" &
	\	RESUME 9020 &

19060	IF ERR=11% AND ERL=9010% THEN RESUME 9020 &
	! END OF FILE WHILE READING HELP FILE &

19070	IF ERR=21% THEN &
		PRINT "?Disk pack is not on-line" &
	\	RESUME 9020 &

19080	IF ERR=6% THEN &
		PRINT "?Invalid device specification" &
	\	RESUME 9020 &

19090	IF ((ERL>=2300% AND ERL<=2800%) OR ERL=3120% OR ERL=4950%) &
		AND (ERR=14% OR ERR=39%) THEN &
		PRINT "?";LEFT(O$,INSTR(1%,O$,":"));" - ";FNERR.TXT$ &
	\	CLOSE O% &
	\	GOTO 32760 &
	! IF OUTPUT DEVICE HUNG, PRINT ERROR AND CLOSE IT &
	! EXIT BECAUSE WE DON'T KNOW WHERE WE CAME FROM &

19100	IF ERL/1000% = 3%	THEN &
		PRINT "?DECtape error - ";FNERR.TXT$ &
	\	RESUME 3120 &

19110	IF ERL/1000% = 4%	THEN &
		PRINT "?Magtape error - ";FNERR.TXT$ &
	\	RESUME 4070 &

19120	IF ERL = 1075% THEN &
		PRINT "?Unable to create output file - ";FNERR.TXT$ &
	\	RESUME 9020 &

19999	PRINT "??Program failure in DIRECT" &
	\ PRINT FNERR.TXT$;" at line";ERL &
	\ RESUME 32760 &
		! UNEXPECTED ERROR. &

30000	! &
	! &
	!	C C L   E N T R Y   P O I N T &
	! &
	! &
	PRIV.OFF$=CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ TEMP$=SYS(PRIV.OFF$) &
	\ ENTRY.TYP%=2% &
		! START FROM CHAIN ENTRY FROM 'DIR' &
		! K%=0% AS FLAG TO EXIT AFTER REQUEST &
		! DEFINE SYS CALL STRINGS TO TURN PRIVILEGES ON AND OFF. &
		! DROP TEMPORARY PRIVILEGES. &

30010	K%=0% &
	\ GOSUB 700 &
	\ L$=CVT$$(SYS(CHR$(7%)),4%+8%+16%+32%+128%+256%) &
	\ FOR I%=9% TO 1% STEP -1% &
	\	IF LEFT(L$,I%)=LEFT("DIRECTORY",I%) THEN &
			L$=RIGHT(L$,I%+1%) &
	\		GOTO 80 &

30020	NEXT I% &
	\ GOTO 300 &

32760	TEMP$=SYS(CHR$(9%)) &
		! EXIT AND CLEAR PROGRAM. &

32767	END
