2!		PROGRAM		: FILCOM.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
8!
10		EXTEND
11!	&
  !	&
  !	&
  !	&
  !			C O P Y R I G H T &
  !	&
  !	&
  !		      Copyright (C) 1974, 1991 by &
  !		Digital Equipment Corporation, Maynard, Mass. &
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
  !********************************************************************* &

20	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	! &
	! &

21	! VER/ED	EDIT DATE	REASON &
	! 9.0-05A	08-Feb-84	(PRL) Handle 8-bit input properly &
	! 9.5		01-Sep-87	Fix bug - premature exit without &
	!				printing all differences &
	! &

100	! &
	! &
	!	P R O G R A M   D E S C R I P T I O N &
	! &
	! &
	&

200	! &
	! &
	!	D E B U G   D E F I N I T I O N &
	! &
	! &

201	DEBUG%=0% &
	&

300	! &
	! &
	!	I / O    C H A N N E L S &
	! &
	! &

301!		CHANNEL #		USED FOR &
	! &
	!	1%			INPUT FILE #1 &
	!	2%			INPUT FILE #2 &
	!	3%	LOGF%		OUTPUT FILE &
	!	4%	CMDF%		INDIRECT COMMAND INPUT FILE &

400	! &
	! &
	!	V A R I A B L E   D E F I N I T I O N S &
	! &
	! &

401!		VARIABLE NAME	DEFINITION &
	! &
	!	T$		PROGRAM TITLE &
	!	I$		VERSION &
	!	X$		XLATE STRING &
	!	KB%		KEYBOARD NUMBER JOB RUNNING FROM &
	!	DETACHED%	-1% IF RUNNING DETACHED &
	!	CMDF%		COMMAND INPUT CHANNEL (0% OR 4%) &
	!	CMD$		INDIRECT COMMAND STRING "@ " &
	!	LOGF%		OUTPUT CHANNEL (0% OR 3%) &
	!	MCR%		-1% IF ERROR MESSAGE REPORTED &
	!	In%		In$() INDEX &
	!	I9%		XLATE INTEGER &
	!	X9$		XLATE CONSTANT STRING &
	!	A1$		A2$+A2$+A2$ &
	!	A2$		"**********" &
	!	B1$		BASIC+ LINE TERMINATOR <lf><cr><null> &
	!	B2$		BASIC+2 LINE TERMINATOR "&"<cr><lf> &
	!	CR.LF$		LINE TERMINATOR <cr><lf> &
	!	FF$		FORM FEED <FF> &
	!	HT$		HORIZONTAL TAB <tab> &
	!	NL$		"" &
	!	C1$		COMPARE STRING (L1$ OR MID(L1$,C1%,C2%)) &
	!	C2$		COMPARE STRING (L2$ OR MID(L2$,C1%,C2%)) &
	!	COM$		COMMENT CHARACTER (";" OR <EXCLAMATION POINT>) &
	!	O$		INPUT LINE STRING &
	!	A$,B$		TEMPORARY STRINGS &
	!	I%,J%,K%	TEMPORARY INTEGERS &
	!	G1%		GLOBAL FILES COMPARED &
	!	G2%		GLOBAL FILES DIFFERENT &
	!	G3%		GLOBAL DIFFERENCES FOUND &
	!	Xn%		In$() TEMPORARY INDEX &
	!	MATCH%		"/MA[TCH]" FLAG &
	!	BASIC%		"/[NO]BA[SIC]" FLAG &
	!	BLANK%		"/[NO]BL[ANK]" FLAG &
	!	SUMMARY%	"/SU[MMARY]" FLAG &
	!	LIMIT%		"/LI[MIT]" FLAG &
	!	COMPARE%	"/CO[MPARE]" FLAG &
	!	APPENDF%	"/[NO]AP[PEND]" FLAG &
	!	UPPERCASE%	"/UP[PERCASE]" FLAG &
	!	SPACES%		"/SP[ACES]" FLAG &
	!	NOCOMMENTS%	"/NOCOM[MENTS]" FLAG &
	!	PATCH%		"/PA[TCH]" FLAG &
	!	DEBUG%		"/DEB[UG]" FLAG &
	!	PAGES%		"/PAG[ES]" FLAG &
	!	NOFF%		"/NOFF" FLAG &
	!	NOSKIP%		"/NOSK[IP]" FLAG &
	!	M%		LINES TO MATCH &
	!	M0%		BASIC+ LINES &
	!	A%		APPEND OUTPUT &
	!	B%		BLANK LINES &
	!	C%		CVT$$ CONVERSION PARAMETER &
	!	L%		LINES TO LIMIT &
	!	P%		LIST PAGE BREAKS ON COMPARE &
	!	R%		REFORMAT REQUIRED &
	!	C1%		STARTING COLUMN FOR COMPARE &
	!	C2%		CHARACTERS TO COMPARE &
	!	EOFn%		FILE #n END-OF-FILE FLAG &
	!	FFn%		FILE #n NO. OF <FF>'S DETECTED &
	!	E0%		ERR &
	!	      0%	NO ERRORS &
	!	     -1%	ABORT &
	!	     >0%	FIRM ERROR (ABORT COMPARISONS) &
	!	  32768%	SOFT ERROR (ABORT CURRENT COMPARISON) &
	!	S%		SUMMARY ONLY &
	!	T%		PROGRAM FLAGS &
	!	      1%	INPUT #1 FILESPEC ANSWERED &
	!	      2%	INPUT #2 FILESPEC ANSWERED &
	!	      4%	ALL QUESTIONS ANSWERED &
	!	      8%	PROGRAM ENTERED THROUGH CCL &
	!	    128%	OUTPUT #3 FILE = KB: &
	!	    256%	INPUT #1 FILESPEC SCANNED &
	!	    512%	INPUT #2 FILESPEC SCANNED &
	!	   1024%	OUTPUT #3 FILESPEC SCANNED &
	!	   2048%	INPUT #1 HAS WILDCARD FILESPEC &
	!	   4096%	OUTPUT #3 HAS WILDCARD FILESPEC &
	!	   8192%	INPUT #1 NON-DISK FILESPEC &
	!	  16384%	OUTPUT #3 FILE OPENED &
	!	D$		DEFAULT DEVICE "" &
	!	P$		DEFAULT PPN [PROJ,PROG] &
	!	N$		DIRECTORY LOOKUP STRING &
	!	D%		DIFFERENCES FOUND &
	!	Rn%		INPUT #n CHARACTER COUNT &
	!	Ln$		INPUT #n INPUT LINE &
	!	Sn%		FILE #n FSS FLAG WORD &
	!	FSn$		FILE #n FILENAME SPEC &
	!	Dn$		FILE #n DEVICE & PPN PART &
	!	Fn$		FILE #n FILENAME PART &
	!	En$		FILE #n EXTENSION PART &
	!	Mn$		FILE #n MODIFIER PART &
	!	Fn%		FILE #n WILDCARD-ed FILENAME &
	!	En%		FILE #n WILDCARD-ed EXTENSION &
	!	Yn$		FILE #n FILENAME WILDCARD FILESPEC &
	!	Zn$		FILE #n EXTENSION WILDCARD FILESPEC &

800	! &
	! &
	!	F U N C T I O N S   A N D   S U B R O U T I N E S &
	! &
	!
801!		PROGRAM DEFINED SUBROUTINES &
	! &
	!	TITLE	LINE RANGE	DESCRIPTION &
	! &
	!		10050-10090	OPEN OUTPUT FILE & WRITE TITLE &
	!		10100-10190	DEBUGGING DUMP OF VARIABLES &
	!		11000-11090	GET A LINE FROM CH. 1% &
	!		12000-12090	GET A LINE FROM CH. 2% &
	!		13100-13190	GET AN INPUT #1 FILENAME &
	!		13200-13290	GET AN INPUT #2 FILENAME &
	!		13300-13390	GET AN OUTPUT #3 FILENAME &
	&

825!		PROGRAM DEFINED FUNCTIONS &
	! &
	!	TITLE	LINE RANGE	DESCRIPTION &
	! &
	!	FNE%(S$,C%)		RETURNS -1 AFTER WRITING S$ ON C% &
	!	FNF%(I$)		FILENAME STRING SCAN I$ (-23%) &
	!	FNN%(S$,P%,C%)		GET NUMBER AFTER S$ AT POS P% DEFAULT C% &
	!	FNO%(I$,C%)		OPEN I$ FOR INPUT ON CHANNEL C% &
	!	FNR0%(C%)		GET FILE ATTRIBUTES ON CHANNEL C% &
	!	FNS%(S$,C%)		LOOKFOR SWITCH S$ MATCHING C% CHARS &
	!	FNA$(S$,P%,D$)		GET STRING AFTER S$ AT POS P% DEFAULT D$ &
	!	FND$(C%)		RETURNS DEVICE & PPN PART &
	!	FNF$(S$)		RETURNS S$ WITHOUT "_" &
	!	FNGCML$(S$,C%)		GET COMMAND LINE ON CHANNEL C% &
	!	FNP$			RETURN THE CURRENT [PPN] &
	!	FNS$(C%)		RETURNS "s" UNLESS C%=1% &
	!	FNT$(S$)		CONVERTS <TAB> TO SPACES IN S$ &
	!	FNW$(S$,W$)		MASK WILDCARD W$ WITH STRING S$ &
	&

900	! &
	! &
	!	D I M E N S I O N    S T A T E M E N T S &
	! &
	! &

910	DIM I1$(300),R1$(300),I2$(300),R2$(300),M%(30%),N%(30%) &
	!	 DIMENSION STATEMENT DESCRIPTION &
	! &
	!	I1$()		INPUT LINES FROM PRIMARY INPUT (CHANNEL 1) &
	!	R1$()		SAVE ARRAY FOR LINES FROM PRIMARY INPUT &
	!	I2$()		INPUT LINES FROM SECONDARY INPUT (CHANNEL 2) &
	!	R2$()		SAVE ARRAY FOR LINES FROM SECONDARY INPUT &
	! &
	!	M%()		CHANGE ARRAY FOR LOOKING UP ATTRIBUTES &
	!	N%()		CHANGE ARRAY FOR WILDCARD INPUT FILESPECS &

999	! &
	! &
	!	S T A R T    O F    P R O G R A M &
	! &
	! &

1000	ON ERROR GOTO 19000 &
	\ I$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(-1%)) &
		! SET UP STANDARD ERROR TRAP. &
		! DROP TEMPORARY PRIVILEGES. &

1010	I$="V10.1-A" &
		! SET UP VERSION/ EDIT # &

1020	T$=SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
	\ KB%=CVT$%(MID(T$,1%,2%)) AND 255% &
	\ DETACHED%=(KB% AND 1%)<>0% &
	\ T$="FILCOM"+CHR$(9%)+I$+CHR$(9%) &
		+CVT$$(RIGHT(T$,3%),4%)+CHR$(13%)+CHR$(10%) &
		+"File Comparison Program"+CHR$(9%) &
		+" "+DATE$(0%)+" "+TIME$(0%) &
	\ PRINT IF CCPOS(0%) UNLESS DETACHED% &
	\ PRINT T$ UNLESS (T% AND 8%) &
		! DISPLAY HEADER. &
	&
	&
		! INITIALIZE PROGRAM CONSTANTS. &
	\ X9$=STRING$(ASCII(" ")+1%,0%) &
		+STRING$(ASCII("0")-ASCII(" ")-1%,-1%) &
		+"0123456789" &
		+STRING$(127%-ASCII("9"),-1%) &
	\ X9$=X9$+X9$ &
	\ A2$="**********" &
	\ A1$=A2$+A2$+A2$ &
	\ HT$=CHR$(9%) &
	\ FF$=CHR$(12%) &
	\ CR.LF$=CHR$(13%)+CHR$(10%) &
	\ B2$="&"+CR.LF$ &
	\ B1$=CHR$(10%)+CHR$(13%)+CHR$(0%) &
		! INITIALIZE XLATE STRING & LINE TERMINATORS &
	&
		! INITIALIZE PROGRAM PARAMETERS. &
	&
	\ NL$="" &
	\ D$=NL$ &
	\ P$=FNP$ &
	\ CMD$="@ " &
		! SET UP DEFAULT DEVICE NAME (PUBLIC STRUCTURE) &
		! GET THE DEFAULT PPN (CURRENT ACCOUNT) &
		! SET INDIRECT COMMAND CHARACTER &

1100	! &
	! &
	!	D I A L O G U E &
	! &
	! &

1110	GOTO 1115 IF (T% AND 8%) UNLESS CMDF% &
	\ O$=FNGCML$("Output to <KB:>",CMDF%) &

1115	I%=1% &
	\ WHILE I% &
	\	I%=INSTR(I%,O$,"/-") &
	\	O$=LEFT(O$,I%)+"NO"+RIGHT(O$,I%+2%) IF I% &
	\ NEXT &
	&
		! RESET ERROR FLAG AND GLOBAL VARIABLES &
	&
	\ E0%,G1%,G2%,G3%=0% &
	\ T%=T% AND 8% &
	\ GOTO 32710 IF (O$="^Z") OR (O$="^C") &
	\ GOTO 1120 IF CMDF% OR (ASCII(O$)<>64%) &
	\ FS4$=RIGHT(O$,2%) &
	\ FS4$=FS4$+".CMD" UNLESS INSTR(1%,FS4$,".") &
	\ O$=NL$ &
	\ GOTO 9030 IF FNO%(FS4$,4%) &
	\ CMDF%=4% &
	\ GOTO 1110 &

1120	M%=3%\ L%=300% &
	\ M0%,A%,B%,C%,P%,R%,S%=0% &
	\ MATCH%=FNS%("/MATCH",3%) &
	\  M%=FNN%(":",MATCH%,3%) IF MATCH% &
	\ BASIC%=FNS%("/BASIC",3%) &
	\  M0%=-1% IF BASIC% &
	\ BLANK%=FNS%("/BLANK",3%) &
	\  B%=-1% IF BLANK% &
	\ SUMMARY%=FNS%("/SUMMARY",3%) &
	\  S%=-1% IF SUMMARY% &
	\ LIMIT%=FNS%("/LIMIT",3%) &
	\  L%=FNN%(":",LIMIT%,60%) IF LIMIT% &
	\ COMPARE%=FNS%("/COMPARE",3%) &
	\  C1%=FNN%(":",COMPARE%,1%) IF COMPARE% &
	\  C2%=FNN%(":",COMPARE%,72%) IF COMPARE% &
	\  C%=C% OR 4% OR 128% IF COMPARE% &
	\  R%,B%=-1% IF COMPARE% &
	\ APPENDF%=FNS%("/APPEND",3%) &
	\  A%=-1% IF APPENDF% &
	\ UPPERCASE%=FNS%("/UPPERCASE",3%) &
	\  C%=C% OR 32% IF UPPERCASE% &
	\ SPACES%=FNS%("/SPACES",3%) &
	\  C%=C% OR 16% IF SPACES% &
	\ PATCH%=FNS%("/PATCH",3%) &
	\  BASIC%,BLANK%,M0%,B%=-1% IF PATCH% &
	\ NOAPPEND%=FNS%("/NOAPPEND",5%) &
	\  A%=0% IF NOAPPEND% &
	\ NOBASIC%=FNS%("/NOBASIC",5%) &
	\  M0%=0% IF NOBASIC% &
	\  BASIC%=-1% IF NOBASIC% &
	\ NOCOMMENTS%=FNS%("/NOCOMMENTS",6%) &
	\  COM$=FNA$(":",NOCOMMENTS%,";") IF NOCOMMENTS% &
	\  C1%=1% UNLESS COMPARE% IF NOCOMMENTS% &
	\  C2%=256% UNLESS COMPARE% IF NOCOMMENTS% &
	\  C%=C% OR 4% OR 128% IF NOCOMMENTS% &
	\  R%,B%,P%=-1% IF NOCOMMENTS% &
	\ NOBLANK%=FNS%("/NOBLANK",5%) &
	\  B%=0% IF NOBLANK% &
	\  BLANK%=-1% IF NOBLANK% &
	\ I%=FNS%("/DEBUG",4%) &
	\  DEBUG%=I% UNLESS DEBUG%=-1% &
	\ PAGES%=FNS%("/PAGES",4%) &
	\  P%=-1% IF PAGES% &
	\ NOFF%=FNS%("/NOFF",5%) &
	\ NOSKIP%=FNS%("/NOSKIP",5%) &
	\ C%=C% OR 256% IF C% &
	&
		! PARSE OUTPUT FILE NAME PART &
	&
	\ LOGF%=0% &
	\ GOSUB 13300 &
	\ GOTO 9030 IF E0% &
	\ NOSKIP%=NOSKIP% OR (T% AND 4096%) OR (T% AND 128%) &

1200	! &
	! &
	!	E N T R Y   P O I N T  -  W I L D C A R D    F I L E N A M E S &
	! &
	! &

1210	GOTO 1220 IF (T% AND 1%) &
	\ FS1$=FNGCML$("Input File #1",CMDF%) &
	\ T%=T% OR 1% &

1220	GOSUB 13100 &
	\ GOTO 9030 IF E0%<0% &
	\ GOTO 9030 IF FNO%(FS1$,1%) &

1230	GOTO 1240 IF (T% AND 2%) &
	\ FS2$=FNGCML$("Input File #2",CMDF%) &
	\ T%=T% OR 2% &

1240	GOSUB 13200 &
	\ GOTO 9030 IF E0%<0% &
	\ GOTO 9030 IF FNO%(FS2$,2%) &
	\ GOTO 1310 IF (T% AND 4%) &

1250	GOTO 1260 IF MATCH% &
	\ A$=FNGCML$("How Many to Match <"+NUM1$(M%)+">",CMDF%) &
	\ I%=VAL(A$) &
	\ M%=I% IF I% &

1260	GOTO 1270 IF BASIC% &
	\ A$=FNGCML$("BASIC+ Lines <NO>",CMDF%) &
	\ M0%=-1% IF ASCII(A$)=89% &

1270	GOTO 1300 IF BLANK% &
	\ A$=FNGCML$("Blank Lines <NO>",CMDF%) &
	\ B%=-1% IF ASCII(A$)=89% &
	&

1300	! &
	! &
	!	P R I M E    T H E    P U M P &
	! &
	! &

1310	D%,R1%,R2%,EOF1%,EOF2%=0% &
	\ FF1%,FF2%=1% &
	\ T%=T% OR 4% &
	\ GOSUB 10050 &
	\ GOTO 9030 IF E0% &
	&
		! GET THE FIRST RECORD FROM INPUT #1 AND INPUT #2 &
	&
	\ GOSUB 11000 &
	\ GOSUB 12000 &
	\ IF EOF1% OR EOF2% THEN &
		K%=FNE%("?A Null Length File?",LOGF%) &
	\	GOTO 9030 &

2000	! &
	! &
	!	C O M P A R E    L O O P &
	! &
	! &

2010	X1%,X2%,I%,J%=-1%\ C1$=L1$\ C2$=L2$ &
	\ GOSUB 10100 IF DEBUG% &
	\ IF C% THEN &
		C1$=MID(FNT$(C1$),C1%,C2%) IF R% &
	\	C2$=MID(FNT$(C2$),C1%,C2%) IF R% &
	\	C1$=CVT$$(C1$,C%) &
	\	C2$=CVT$$(C2$,C%) &

2015	IF (C1$=C2$) AND (LEN(C1$)=LEN(C2$)) THEN &
		GOSUB 11000 &
	\	GOSUB 12000 &
	\	IF (EOF1% AND EOF2%) &
		AND ((R1% OR R2%)=0%) THEN &
			GOTO 9010 &
		ELSE	GOTO 2010 &

2020	D%=D%+1% &
	\ I1%,I2%=M%-1% &
	\ I1$(0%)=L1$ &
	\ FOR I0%=1% TO I1% &
	\	GOSUB 11000 &
	\	I1$(I0%)=L1$ &
	\ NEXT I0% &
	\ I2$(0%)=L2$ &
	\ FOR I0%=1% TO I2% &
	\	GOSUB 12000 &
	\	I2$(I0%)=L2$ &
	\ NEXT I0% &

2030	GOSUB 11000 &
	\ I1%=I1%+1% &
	\ GOTO 2035 IF I1% <= L% &
	\ E0%=1%+32767% &
	\ GOTO 2140 &

2035	I1$(I1%)=L1$ &
	\ I%,J%=0% &

2040	X1%=(I1%+J%-M%+1%)\ C1$=I1$(X1%) &
	\ X2%=(I%+J%)\ C2$=I2$(X2%) &
	\ GOSUB 10100 IF DEBUG% &
	\ IF C% THEN &
		C1$=MID(FNT$(C1$),C1%,C2%) IF R% &
	\	C2$=MID(FNT$(C2$),C1%,C2%) IF R% &
	\	C1$=CVT$$(C1$,C%) &
	\	C2$=CVT$$(C2$,C%) &

2045	IF (C1$<>C2$) OR (LEN(C1$)<>LEN(C2$)) THEN &
		J%=0% &
	ELSE	J%=J%+1% &
	\	IF J%=M% THEN &
			GOTO 2100 &
		ELSE	GOTO 2040 &

2050	I%=I%+1% &
	\ GOTO 2040 IF I%<=I2%-M%+1% &
	&
		! GET A RECORD FROM INPUT #2 &
	&
	\ GOSUB 12000 &
	\ I2%=I2%+1% &
	\ GOTO 2065 IF I2% <= L% &
	\ E0%=1%+32767% &
	\ GOTO 2140 &

2065	I2$(I2%)=L2$ &
	\ I%,J%=0% &

2070	X1%=(I%+J%)\ C1$=I1$(X1%) &
	\ X2%=(I2%+J%-M%+1%)\ C2$=I2$(X2%) &
	\ GOSUB 10100 IF DEBUG% &
	\ IF C% THEN &
		C1$=MID(FNT$(C1$),C1%,C2%) IF R% &
	\	C2$=MID(FNT$(C2$),C1%,C2%) IF R% &
	\	C1$=CVT$$(C1$,C%) &
	\	C2$=CVT$$(C2$,C%) &

2075	IF (C1$<>C2$) OR (LEN(C1$)<>LEN(C2$)) THEN &
		J%=0% &
	ELSE	J%=J%+1% &
	\	IF J%=M% THEN &
			GOTO 2120 &
		ELSE	GOTO 2070 &

2080	I%=I%+1% &
	\ GOTO 2070 IF I%<=I1%-M%+1% &
	\ GOTO 2030 &
		! TRY TO FIND A MATCH FROM THE TOP &

2100	I%=I%+M%-1% &
	\ R2$(J%+I2%-I%)=R2$(J%) FOR J%=R2%-1% TO 0% STEP -1% IF R2% &
	&
		! ADD IN THOSE RECORDS LEFT IN I2$() &
	&
	\ FOR J%=1% TO I2%-I% &
	\ R2$(J%-1%)=I2$(J%+I%) &
	\ I2$(J%+I%)=NL$ &
	\ NEXT J% &
	&
		! UPDATE POINTER INTO OVERFLOW ARRAY R2$() &
	&
	\ R2%=R2%+I2%-I% &
	\ I2%=I% &
	\ GOTO 2140 &
		! WRITE OUT RESULTS OF COMPARE &

2120	I%=I%+M%-1% &
	\ R1$(J%+I1%-I%)=R1$(J%) FOR J%=R1%-1% TO 0% STEP -1% IF R1% &
	&
		! ADD IN THOSE RECORDS LEFT IN I1$() &
	&
	\ FOR J%=1% TO I1%-I% &
	\ R1$(J%-1%)=I1$(J%+I%) &
	\ I1$(J%+I%)=NL$ &
	\ NEXT J% &
	&
		! UPDATE POINTER INTO OVERFLOW ARRAY R1$() &
	&
	\ R1%=R1%+I1%-I% &
	\ I1%=I% &
	\ GOTO 2140 &
		! WRITE OUT RESULTS OF COMPARE &

2140	GOTO 2160 UNLESS PATCH% &
	\ FOR I%=0% TO I1%-M% &
	\	X$=XLATE(I1$(I%),X9$)\ I1$(I%)=NL$ &
	\	I9%=VAL(LEFT(X$,INSTR(1%,X$,CHR$(-1%))-1%)) &
	\	FOR J%=0% TO I2%-M% &
	\		X$=XLATE(I2$(J%),X9$) &
	\		GOTO 2150 IF VAL(LEFT(X$,INSTR(1%,X$,CHR$(-1%))-1%))=I9% &
	\	NEXT J% &
	\	PRINT #LOGF%, XLATE(NUM$(I9%),X9$) &

2150	NEXT I% &
	\ I2%=I2%-1% &
	\ GOTO 2190 &
		! SKIP HEADER LISTINGS &

2160	GOTO 2170 IF S% &
	\ PRINT #LOGF% IF CCPOS(LOGF%) &
	\ PRINT #LOGF%, A1$ &
	\ PRINT #LOGF%, "1)"; &
	\ PRINT #LOGF%, NUM1$(FF1%); IF P% &
	\ PRINT #LOGF%, " ";FNF$(FS1$) &

2170	J%=I1%-M%+1% &
	\ FOR I%=0% TO J% &
	\ PRINT #LOGF%, I1$(I%); UNLESS S% &
	\ I1$(I%)=NL$ &
	\ NEXT I% &

2180	GOTO 2190 IF S% &
	\ PRINT #LOGF% IF CCPOS(LOGF%) &
	\ PRINT #LOGF%, A2$ &
	\ PRINT #LOGF%, "2)"; &
	\ PRINT #LOGF%, NUM1$(FF2%); IF P% &
	\ PRINT #LOGF%, " ";FNF$(FS2$) &

2190	J%=I2%-M%+1% &
	\ FOR I%=0% TO J% &
	\ PRINT #LOGF%, I2$(I%); UNLESS S% &
	\ I2$(I%)=NL$ &
	\ NEXT I% &
	&
		! REPRIME PUMP AND PRESS ON &
	&
	\ C1$,C2$=NL$ &
	\ GOTO 2015 UNLESS E0% &
	&
		! LIMIT ERROR, CLEAN UP AND TELL USER &
	&
	\ K%=FNE%("?Limit ("+NUM1$(L%)+") reached on last compare, skipping rest of file",LOGF%) &
	\ L1$,L2$=NL$ &
	\ R1$(I%)=NL$ FOR I%=0% TO R1% IF R1% &
	\ R2$(I%)=NL$ FOR I%=0% TO R2% IF R2% &
	\ GOTO 9010 &

9000	! &
	! &
	!	P R O G R A M    C L E A N    U P &
	! &
	! &

9010	GOTO 9030 IF PATCH% &
	\ DIFF$=CR.LF$ &
	\ DIFF$=DIFF$+"?" IF D% &
	\ DIFF$=DIFF$+NUM1$(D%)+" Difference"+FNS$(D%)+" Found."+CR.LF$ &
	\ I%=FNE%(DIFF$,LOGF%) &
	\ PRINT #LOGF%, FF$ UNLESS NOSKIP% &

9020	G1%=G1%+1% &
	\ G2%=G2%+1% IF D% &
	\ G3%=G3%+D% &

9030	ON ERROR GOTO 19000 &
	\ CLOSE 1%,2% &
	\ GOTO 9050 IF PATCH% &
	\ IF (T% AND 4096%) THEN &
		T%=T% AND (NOT 16384%) &
	\	CLOSE 3% &

9040	E0%=E0% AND 255% &
	\ GOTO 1200 IF (E0%=0%) AND (T% AND 2048%) &
	\ GOTO 9050 UNLESS ((T% AND 2048%)<>0%) AND (G1%<>0%) &
	\ DIFF$=CR.LF$+NUM1$(G3%)+" Difference"+FNS$(G3%)+" Found in " &
	\ DIFF$=DIFF$+NUM1$(G2%)+" File"+FNS$(G2%)+" of " IF G2% &
	\ DIFF$=DIFF$+NUM1$(G1%)+" Total Files Compared."+CR.LF$ &
	\ I%=FNE%(DIFF$,LOGF%) &
		! WRITE SUMMARY LINE TO OUTPUT FILE &
		!  AND TO KEYBOARD UNLESS OUTPUT FILE=KB OR JOB IS DETACHED &

9050	CLOSE 3% &
	\ GOTO 9060 UNLESS CMDF% &
	\ J%=((T% AND 1%)<>0%)+1%+((T% AND 2%)<>0%)+1% &
	\ J%=J%+(MATCH%<>0%)+1%+(BASIC%<>0%)+1%+(BLANK%<>0%)+1% UNLESS (T% AND 4%) &
	\ INPUT LINE #CMDF%, A$ FOR I%=1% TO J% &
		! FLUSH INDIRECT COMMAND TO NEXT QUESTION (IF ANY LEFT) &

9060	GOTO 1100 IF CMDF% OR (T% AND 8%)=0% &
	\ GOTO 32710 &
		! ITERATE UNLESS CCL ENTRY &

10000	! &
	! &
	!	P R O G R A M M E R    D E F I N E D &
	!		S U B R O U T I N E S &
	! &

10050	! &
	! &
	!	O P E N   O U T P U T   I F   N E C E S S A R Y &
	! &
	! &

10060	GOTO 10070 IF (T% AND 16384%) &
	\ GOSUB 13300 &
	\ GOTO 10090 IF E0% &
	\ IF LOGF% THEN &
		OPEN FS3$ FOR OUTPUT AS FILE LOGF% UNLESS A% &
	\	OPEN FS3$ AS FILE LOGF%, MODE 2% IF A% &
	\	T%=T% OR 16384% &
	\	GOTO 10090 IF PATCH% &
	\	PRINT #LOGF%, T$ &

10070	GOTO 10090 IF E0% OR (T% AND 3%)<>3% &
	\ PRINT #LOGF% &
	\ PRINT #LOGF%, "Comparing: 1) ";FNF$(FS1$);" to 2) ";FNF$(FS2$) &
	\ PRINT #LOGF% &

10090	RETURN &
	&

10100	! &
	! &
	!	D E B U G G I N G    D U M P &
	! &
	! &

10110	PRINT #LOGF% &
	\ PRINT #LOGF%, "X1%=";X1%;"X2%=";X2%,"I%=";I%;"J%=";J%,"C1%=";C1%;"C2%=";C2% &
	\ PRINT #LOGF%, "C1$=";C1$; &
	\ PRINT #LOGF%, "C2$=";C2$; &
	\ RETURN &

11000	! &
	! &
	!	G E T    A    L I N E    F R O M    C H .    1 &
	! &
	! &

11005	GOTO 11030 IF R1% &
	\ ON ERROR GOTO 11050 &
	\ L1$=NL$ &

11010	INPUT LINE #1%,A$ &
	\ FF1%=FF1%+1% IF INSTR(1%,A$,FF$) &
	\ GOTO 11000 IF A$=FF$ IF NOFF% &
	\ L1$=L1$+A$ &
	\ IF M0% THEN &
		A$=RIGHT(A$,LEN(A$)-2%) &
	\	GOTO 11010 IF A$=B1$ OR A$=B2$ &

11015	A$=CVT$$(L1$,2%) &
	\ GOTO 11020 UNLESS NOCOMMENTS% &
	\ GOTO 11000 IF ASCII(A$)=ASCII(COM$) &

11020	GOTO 11090 IF B% &
	\ GOTO 11000 IF A$=CR.LF$ &
	\ GOTO 11090 &

11030	L1$=R1$(0%) &
	\ R1%=R1%-1% &
	\ R1$(I%)=R1$(I%+1%) FOR I%=0% TO R1%-1% IF R1% &
	\ R1$(R1%+1%)=NL$ &
	\ GOTO 11090 &

11050	IF (ERR<>11% AND ERR<>47%) OR (ERL<>11010) THEN &
		ON ERROR GOTO 0 &
	ELSE	IF ERR<>11% THEN &
			K%=FNE%("?Line too long at line "+NUM1$(ERL),LOGF%) &
		ELSE	L1$=NL$\ EOF1%=-1% &

11060	RESUME 11090 &

11090	ON ERROR GOTO 19000 &
	\ RETURN &

12000	! &
	! &
	!	G E T    A    L I N E    F R O M    C H .    2 &
	! &
	! &

12005	GOTO 12030 IF R2% &
	\ ON ERROR GOTO 12050 &
	\ L2$=NL$ &

12010	INPUT LINE #2%,A$ &
	\ FF2%=FF2%+1% IF INSTR(1%,A$,FF$) &
	\ GOTO 12000 IF A$=FF$ IF NOFF% &
	\ L2$=L2$+A$ &
	\ IF M0% THEN &
		A$=RIGHT(A$,LEN(A$)-2%) &
	\	GOTO 12010 IF A$=B1$ OR A$=B2$ &

12015	A$=CVT$$(L2$,2%) &
	\ GOTO 12020 UNLESS NOCOMMENTS% &
	\ GOTO 12000 IF ASCII(A$)=ASCII(COM$) &

12020	GOTO 12090 IF B% &
	\ GOTO 12000 IF A$=CR.LF$ &
	\ GOTO 12090 &

12030	L2$=R2$(0%) &
	\ R2%=R2%-1% &
	\ R2$(I%)=R2$(I%+1%) FOR I%=0% TO R2%-1% IF R2% &
	\ R2$(R2%+1%)=NL$ &
	\ GOTO 12090 &

12050	IF (ERR<>11% AND ERR<>47%) OR (ERL<>12010) THEN &
		ON ERROR GOTO 0 &
	ELSE	IF ERR<>11% THEN &
			K%=FNE%("?Line too long at line "+NUM1$(ERL),LOGF%) &
		ELSE	L2$=NL$\ EOF2%=-1% &

12060	RESUME 12090 &

12090	ON ERROR GOTO 19000 &
	\ RETURN &

13100	! &
	! &
	!	 G E T   A N   I N P U T   #1%   F I L E    N A M E &
	! &
	! &

13105	GOTO 13130 IF (T% AND 256%) &
	\ T%=T% OR 256% &
	\ FS1$=CVT$$(FS1$,-2%) &
	\ S1%=FNF%(FS1$) &
	\ GOTO 13190 IF E0% &
	\ D1$=FND$(S1%) &
	\ GOTO 13190 IF E0% &
	\ M1$=A$ &
	&
		! CHECK FOR FILENAME SPECIFIED &
	&
	\ GOTO 13115 IF (S1% AND 1%) &
	\ S1%=S1% OR 2% &
	\ M%(7%),M%(9%)=229% &
	\ M%(8%),M%(10%)=185% &

13115	GOTO 13120 IF (S1% AND 8%) &
	\ S1%=S1% OR 32% &
	\ M%(11%)=229% &
	\ M%(12%)=185% &

13120	GOTO 13125 UNLESS RECOUNT &
	\ GOTO 13160 UNLESS MID(FS1$,LEN(FS1$)-RECOUNT+1%,1%)="," &
	\ FS2$=RIGHT(FS1$,LEN(FS1$)-RECOUNT+2%) &
	\ MATCH%,BASIC%,BLANK%=-1% &
	\ T%=T% OR 2% &

13125	F1$=RAD$(M%(7%)+SWAP%(M%(8%)))+RAD$(M%(9%)+SWAP%(M%(10%))) &
	\ E1$=RAD$(M%(11%)+SWAP%(M%(12%))) &
	\ F1%=S1% AND 6% &
	\ E1%=S1% AND 96% &
	\ FS1$=CVT$$(D1$+F1$+"."+E1$+M1$,2%) &
	&
		! CHECK FOR WILDCARDING INPUT #1 &
	&
	\ T%=T% OR 2048% IF (F1% OR E1%) &
	\ T%=T% OR 8192% IF (STATUS AND 255%)<>0% AND (S1% AND 4096%)<>0% &
	\ GOTO 13150 IF (T% AND 2048%)<>0% AND (T% AND 8192%)<>0% &
	\ N%(I%)=M%(I%) FOR I%=0% TO 26% &
	\ N%(1%)=6% &
	\ N%(2%)=17% &
	\ N%(3%),N%(4%)=0% &
	\ N%(I%)=0% FOR I%=13% TO 22% &

13130	GOTO 13140 IF (T% AND 8192%) &
	\ CHANGE N% TO N$ &
	\ ON ERROR GOTO 13180 &
	\ CHANGE SYS(N$) TO M% &
	\ N%(3%)=N%(3%)+1% &
	\ N%(4%)=SWAP%(N%(3%)) AND 255% &

13140	F1$=RAD$(M%(7%)+SWAP%(M%(8%)))+RAD$(M%(9%)+SWAP%(M%(10%))) &
	\ E1$=RAD$(M%(11%)+SWAP%(M%(12%))) &
	\ FS1$=CVT$$(D1$+F1$+"."+E1$+M1$,2%) &
	\ GOTO 13190 &

13150	E0%=FNE%("?Wildcard filename illegal on device "+LEFT(D1$,INSTR(1%,D1$,"[")-1%),0%) &
	\ GOTO 13190 &

13160	E0%=FNE%("?Illegal command line "+RIGHT(FS1$,LEN(FS1$)-RECOUNT+1%),0%) &
	\ GOTO 13190 &

13180	E0%=ERR &
	\ E0%=-1% IF (N%(3%)<>0%) AND (T% AND 2048%) &
	\ RESUME 13190 &

13190	ON ERROR GOTO 19000 &
	\ RETURN &

13200	! &
	! &
	!	 G E T   A N   I N P U T   #2%   F I L E    N A M E &
	! &
	! &

13205	GOTO 13230 IF (T% AND 512%) &
	\ T%=T% OR 512% &
	\ FS2$=CVT$$(FS2$,-2%) &
	\ FS2$="*.*" IF LEN(FS2$)=0% &
	\ S2%=FNF%(FS2$) &
	\ GOTO 13290 IF E0% &
	\ D2$=FND$(S2%) &
	\ GOTO 13290 IF E0% &
	\ M2$=A$ &
	&
		! CHECK FOR FILENAME SPECIFIED &
	&
	\ GOTO 13215 IF (S2% AND 1%) &
	\ S2%=S2% OR 2% &
	\ M%(7%),M%(9%)=229% &
	\ M%(8%),M%(10%)=185% &

13215	GOTO 13220 IF (S2% AND 8%) &
	\ S2%=S2% OR 32% &
	\ M%(11%)=229% &
	\ M%(12%)=185% &

13220	GOTO 13260 IF RECOUNT &
	\ F2$=RAD$(M%(7%)+SWAP%(M%(8%)))+RAD$(M%(9%)+SWAP%(M%(10%))) &
	\ E2$=RAD$(M%(11%)+SWAP%(M%(12%))) &
	\ F2%=S2% AND 6% &
	\ E2%=S2% AND 96% &
	\ Y2$=F2$ &
	\ Z2$=E2$ &

13230	F2$=FNW$(F1$,Y2$) IF F2% &
	\ E2$=FNW$(E1$,Z2$) IF E2% &
	\ FS2$=CVT$$(D2$+F2$+"."+E2$+M2$,2%) &
	\ GOTO 13290 &

13260	E0%=FNE%("?Illegal command line "+RIGHT(FS2$,LEN(FS2$)-RECOUNT+1%),0%) &
	\ GOTO 13290 &

13290	ON ERROR GOTO 19000 &
	\ RETURN &

13300	! &
	! &
	!	 G E T   A N   O U T P U T   #3%   F I L E    N A M E &
	! &
	! &

13305	GOTO 13330 IF (T% AND 1024%) &
	\ T%=T% OR 1024% &
	\ O$="KB:" IF LEN(O$)=0% &
	\ I%=FNF%(O$) &
	\ GOTO 13390 IF E0% &
	\ O$="KB:="+O$ IF (RECOUNT<>0%) AND (MID(O$,LEN(O$)-RECOUNT+1%,1%)=",") &
	\ FS3$=O$ &
	\ S3%=FNF%(FS3$) &
	\ GOTO 13390 IF E0% &
	\ D3$=FND$(S3%) &
	\ GOTO 13390 IF E0% &
	\ M3$=A$ &
	&
		! CHECK FOR FILENAME SPECIFIED &
	&
	\ GOTO 13315 IF (S3% AND 1%) &
	\ S3%=S3% OR 2% &
	\ M%(7%),M%(9%)=229% &
	\ M%(8%),M%(10%)=185% &

13315	GOTO 13320 IF (S3% AND 8%) &
	\ E3$="DIF" &

13320	F3$=RAD$(M%(7%)+SWAP%(M%(8%)))+RAD$(M%(9%)+SWAP%(M%(10%))) &
	\ E3$=RAD$(M%(11%)+SWAP%(M%(12%))) IF (S3% AND 8%) &
	\ F3%=S3% AND 6% &
	\ E3%=S3% AND 96% &
	\ GOTO 13350 IF (F3%<>0%) AND (E3%<>0%) &
	\ Y3$=F3$ &
	\ Z3$=E3$ &
	&
		! CHECK FOR MORE COMMAND TO PARSE &
	&
	\ GOTO 13327 UNLESS RECOUNT &
	\ GOTO 13360 UNLESS MID(FS3$,LEN(FS3$)-RECOUNT+1%,1%)="=" &
	\ FS1$=RIGHT(FS3$,LEN(FS3$)-RECOUNT+2%) &
	\ MATCH%,BASIC%,BLANK%=-1% &
	\ T%=T% OR 1% &
	&

13327	T%=T% OR 128% IF (STATUS AND 16384%) AND (M%(26%)<>255%) &
	\ LOGF%=3% UNLESS T% AND 128% &
	\ IF ((STATUS AND 255%)<>2%) AND ((STATUS AND 255%)<>22%) THEN &
		T%=T% OR 4096% IF (F3% OR E3%) &
	  ELSE	F3$="FILCOM" &
	\	E3$="DIF" &
	\	F3%,E3%=0% &
		! MARK IF DEVICE IS INTERACTIVE (KEYBOARD) &
		! ALLOW WILDCARDING OF OUTPUT ONLY IF DEVICE<>(KB: OR NL:) &

13330	F3$=FNW$(F1$,Y3$) IF F3% &
	\ E3$=FNW$(E1$,Z3$) IF E3% &
	\ FS3$=CVT$$(D3$+F3$+"."+E3$+M3$,2%) &
	\ GOTO 13370 IF ((F3$=F2$) AND (E3$=E2$)) OR ((F3$=F1$) AND (E3$=E1$)) &
	\ GOTO 13290 &

13350	E0%=FNE%("?Wildcard '*.*' illegal for output filename",0%) &
	\ GOTO 13390 &

13360	E0%=FNE%("?Illegal command line "+RIGHT(FS3$,LEN(FS3$)-RECOUNT+1%),0%) &
	\ GOTO 13390 &

13370	E0%=FNE%("?Output filename '"+F3$+"."+E3$+"' would overwrite an input file, skipping compare",LOGF%) &
	\ GOTO 13390 &

13390	ON ERROR GOTO 19000 &
	\ RETURN &

15000	! &
	! &
	!	P R O G R A M M E R    D E F I N E D &
	!		F U N C T I O N S &
	! &

15010	DEF* FNE%(S$,C%) &
	\ GOTO 15030 UNLESS (T% AND 16384%) &
	\ PRINT #C% IF CCPOS(C%) &
	\ PRINT #C% UNLESS MCR% &
	\ PRINT #C%, S$ &
	\ GOTO 15040 UNLESS C%
15030	GOTO 15040 IF DETACHED% &
	\ PRINT IF CCPOS(0%) &
	\ PRINT UNLESS MCR% &
	\ PRINT S$
15040	FNE%,MCR%=-1% &
	\ FNEND &
	&

15050	DEF* FNR0%(J%) &
	\ ON ERROR GOTO 15080 &
	\ CHANGE SYS(CHR$(6%)+CHR$(-25%)+CHR$(J%)) TO M% &
	\ J%=M%(5%)+SWAP%(M%(6%)) &
	\ FNR0%=(J%<>0% AND (J% AND 255%)<>4%) &
	\ GOTO 15090 &

15080	FNR0%=ERR &
	\ RESUME 15090 &

15090	ON ERROR GOTO 19000 &
	\ FNEND &
	&

15095	DEF* FNE$(ERR.NO%)=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+ &
			   CHR$(ERR.NO%)),3%),4%) &

15100	DEF* FNO%(I$,C%) &
	\ FNO%=0% &
	\ ON ERROR GOTO 15150 &
	\ OPEN I$ FOR INPUT AS FILE C%, MODE 8192% &
	\ GOTO 15120 IF FNR0%(C%) UNLESS (STATUS AND 255%) &
	\ GOTO 15190 &

15120	GOSUB 10050 &
	\ FNO%=FNE%("?File has non-ASCII Stream Attribute - Can't Read "+I$,LOGF%) &
	\ GOTO 15190 &

15150	GOSUB 10050 &
	\ TMP$=NUM1$(C%)+" '"+I$+"' " &
	\ FNO%=FNE%(FNE$(ERR)+" Opening input file #"+TMP$,LOGF%) &

15160	RESUME 15190 &

15190	ON ERROR GOTO 19000 &
	\ FNEND &

15200	DEF* FNF%(I$) &
	\ ON ERROR GOTO 15280 &
	\ CHANGE SYS(CHR$(6%)+CHR$(-23%)+I$) TO M% &
	\ GOTO 15290 &

15280	E0%=-1% &
	\ GOTO 15290 UNLESS ERR=2% &
	\ E0%=FNE%("?Illegal Filename "+I$,0%) &
	\ RESUME 15290 &

15290	ON ERROR GOTO 19000 &
	\ FNF%=M%(29%)+SWAP%(M%(30%)) &
	\ FNEND &
	&

15300	DEF* FNS%(S$,C%) &
	\ I%=INSTR(1%,O$,LEFT(S$,C%)) &
	\ GOTO 15390 UNLESS I% &
	\ J%=C% &
	\ J%=J%+1% WHILE MID(O$,I%+J%,1%)>="A" &
	\ I%=INSTR(I%,O$,LEFT(S$,J%)) &
	\ GOTO 15390 UNLESS I% &
	\ O$=LEFT(O$,I%-1%)+RIGHT(O$,I%+J%)
15390	FNS%=I% &
	\ FNEND &
	&

15400	DEF* FNN%(S$,P%,C%) &
	\ FNN%=C% &
	\ GOTO 15440 UNLESS MID(O$,P%,1%)=S$ &
	\ I%=0% &
	\ S$="0" &
	\ WHILE (S$>="0") AND (S$<="9") &
	\	I%=I%+1% &
	\	S$=MID(O$,P%+I%,1%) &
	\ NEXT &
	\ FNN%=VAL(MID(O$,P%+1%,I%-1%)) &
	\ O$=LEFT(O$,P%-1%)+RIGHT(O$,P%+I%)
15440	FNEND &

15450	DEF* FNA$(S$,P%,D$) &
	\ FNA$=D$ &
	\ GOTO 15490 UNLESS MID(O$,P%,1%)=S$ &
	\ I%=0% &
	\ S$="0" &
	\ WHILE (S$>" ") AND (S$<>"/") AND (S$<>",") &
	\	I%=I%+1% &
	\	S$=MID(O$,P%+I%,1%) &
	\ NEXT &
	\ FNA$=MID(O$,P%+1%,I%) &
	\ O$=LEFT(O$,P%-1%)+RIGHT(O$,P%+I%)
15490	FNEND &

15500	DEF* FNP$ &
	\ CHANGE SYS(CHR$(6%)+CHR$(14%)+STRING$(6%,0%)+CHR$(1%)) TO M% &
	\ FNP$="["+NUM1$(M%(8%))+","+NUM1$(M%(7%))+"]" &
	\ FNEND &
	&

15600	DEF* FNW$(S$,W$) &
	\ FNW$=S$ &
	\ GOTO 15690 IF LEFT("??????",LEN(W$))=W$ &
	\ A$=W$ &
	\ I%=1% &
	\ WHILE I% &
	\	I%=INSTR(I%,A$,"?") &
	\	A$=LEFT(A$,I%-1%)+MID(S$,I%,1%)+RIGHT(A$,I%+1%) IF I% &
	\ NEXT &
	\ FNW$=A$ &
	\ I%=INSTR(1%,A$," ") &
	\ GOTO 15690 UNLESS I% &
	\ GOTO 15690 IF MID(A$,I%,LEN(A$)-I%+1%)=" " &
	\ K%=FNE%("?Illegal Filename "+A$+" created from "+S$+" by "+W$,0%) &
	\ E0%=1%+32767%
15690	FNEND &
	&

15700	DEF* FNT$(S$) &
	\ K%=NOCOMMENTS% &
	\ WHILE K% &
	\	K%=INSTR(1%,S$,COM$) &
	\	S$=LEFT(S$,K%-1%) IF K% &
	\	K%=0% &
	\ NEXT &
	\ K%=COMPARE% &
	\ WHILE K% &
	\	K%=INSTR(1%,S$,HT$) &
	\	S$=LEFT(S$,K%-1%)+SPACE$(9%-(K%-(((K%-1%)/8%)*8%)))+RIGHT(S$,K%+1%) IF K% &
	\ NEXT &
	\ FNT$=S$ &
	\ FNEND &
	&

15800	DEF* FNS$(C%) &
	\ FNS$="s" &
	\ FNS$=NL$ IF C%=1% &
	\ FNEND &

15900	DEF* FNF$(S$)=RIGHT(S$,INSTR(1%,S$,"_")+1%) &

15910	DEF* FND$(C%) &
	\ A$=D$ &
	\ A$=CHR$(M%(23%))+CHR$(M%(24%)) IF (C% AND 4096%) &
	\ A$=A$+NUM1$(M%(25%)) IF M%(26%) &
	\ A$="_"+A$ IF (C% AND 8192%) &
	\ A$=RAD$(M%(23%)+SWAP%(M%(24%)))+RAD$(M%(25%)+SWAP%(M%(26%))) IF C%<0% &
	\ A$=A$+":" IF LEN(A$) &
	\ A$=A$+P$ UNLESS (C% AND 128%) &
	\ A$=A$+"["+NUM1$(M%(6%))+","+NUM1$(M%(5%))+"]" IF (C% AND 128%) &
	\ FND$=A$ &
	\ C%=M%(27%)+SWAP%(M%(28%)) &
	\ A$=NL$ &
	\ A$="<"+NUM1$(M%(22%))+">" IF (C% AND 2048%) &
	\ A$=A$+"/CL:"+NUM1$(M%(15%)+SWAP%(M%(16%))) IF (C% AND 1%) &
	\ A$=A$+"/MO:"+NUM1$(M%(17%)+SWAP%(M%(18%) AND 127%)) IF (C% AND 2%) &
	\ A$=A$+"/FI:"+NUM1$(M%(13%)+SWAP%(M%(14%))) IF (C% AND 4%) &
	\ A$=A$+"/PO:"+NUM1$(M%(19%)+SWAP%(M%(20%))) IF (C% AND 8%) &
	\ GOTO 15940 UNLESS (M%(6%)=255%) OR (M%(5%)=255%) &
	\ E0%=FNE%("?Wildcard PPN specification illegal",0%)
15940	FNEND &

15950	DEF* FNGCML$(S$,C%)
15960	PRINT IF CCPOS(0%) UNLESS DETACHED% &
	\ PRINT IF MCR% UNLESS DETACHED%\ MCR%=0% &
	\ PRINT S$; UNLESS DETACHED% &
	\ INPUT LINE #C%, A$ &
	\ PRINT CMD$;A$; IF (C% AND NOT DETACHED%) &
	\ PRINT IF CCPOS(0%) UNLESS DETACHED% &
	\ A$=CVT$$(A$,-2%) &
	\ GOTO 15960 IF ((C%<>0%) AND ((ASCII(A$)=59%) OR (ASCII(A$)=33%))) &
	\ FNGCML$=A$
15990	FNEND &

19000	! &
	! &
	!	E R R O R    T R A P    H A N D L I N G    R O U T I N E &
	! &
	! &

19010	E0%=ERR &
	\ GOTO 19020 UNLESS E0%=11% &
	\ PRINT CHR$(13%);SPACE$(16%);CHR$(13%); IF CMDF% UNLESS DETACHED% &
	\ RESUME 32710 IF (T% AND 8%) OR (CMDF%=0%) &
	\ CLOSE 4% &
	\ CMDF%=0% &
	\ RESUME 1110 &

19020	ON ERROR GOTO 0 &
	\ GOTO 32767 &

30000	O$=CVT$$(SYS(CHR$(7%)),4%+8%+16%+32%+128%+256%) &
	\ O$=CVT$$(RIGHT(O$,7%),-2%) IF LEFT(O$,6%)="FILCOM" &
	\ T%=8% IF LEN(O$) &
	\ GOTO 1000 &
		! CCL ENTRY &

32700	! &
	! &
	!	P R O G R A M    C O M P L E T I O N &
	! &
	! &

32710	IF DETACHED% THEN &
		A$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(0%)) &
	\	A$=SYS(CHR$(6%)+CHR$(5%)) &
		! IF JOB IS RUNNING DETACHED &
		! REGAIN TEMPORARY PRIVILEGES AND LOGOUT &

32767	END
