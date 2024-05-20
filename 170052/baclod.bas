2!		PROGRAM		: BACLOD.BAS
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
  !		      Copyright (C) 1976, 1991 by &
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
	&
	! VER/ED	EDIT DATE	REASON &
	! KMF 01	17-MAY-84	CHANGE VALUE OF A% &
	! 				TO PRIV'D USER &
	! V9.4 REG	08-MAY-87	Fix bug: LOAded index file uses &
	!				"FROM" device not on current system &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	&
!	THIS MODULE TAKES THE INDEX FILE OF A PREVIOUS BACKUP RUN AND &
!	TRANSFERS THE OUTPUT VOLUME INFORMATION OF THIS INDEX FILE TO &
!	THE INPUT VOLUME LIST OF THE RESTORE WORK FILE.  THE RESTORE &
!	WORK FILE THEN HAS ALL NECESSARY INFORMATION TO LOCATE ANY FILE &
!	ON THE BACKUP SET. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
	&
   !	CHANNEL #		USED FOR &
!	   1			WORK FILE FOR RESTORE RUN. &
!	   2			INDEX WORK FILE. &
!	   3			LISTING FILE. &
!	   4			WORK FILE BLOCK I/O CHANNEL. &
!	   6			BACKUP VOLUME FOR PRIMARY LOAD. &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&
	&
!	NAME		USE &
!	A$		ACCOUNT NUMBER STRING. &
!	A%		ACCOUNT NUMBER. &
!	B0%		FREE LIST POINTER. &
!	B1%		BACKUP CLUSTER SIZE IN BYTES. &
!	B2%		BACKUP DISK DEVICE CLUSTER SIZE IN BYTES. &
!	B9%		NUMBER OF BLOCKS ACTUALLY READ. &
!	B3%		BAD BLOCK LIST POINTER. &
!	B4%		BAD BLOCK ENTRY POINTER. &
!	B5%		B1%/B2%-1% &
!	C%(1%)		CURRENT VOLUME. &
!	C%(2%)		CURRENT ACCOUNT. &
!	C%(3%)		CURRENT FILE. &
!	C%(4%)		CURRENT BLOCK. &
!	C(1%)		VOLUME NUMBER. &
!	C(2%)		NUMBER OF ACCOUNTS. &
!	C(3%)		NUMBER OF FILES. &
!	C(4%)		NUMBER OF BLOCKS. &
!	C8$		BELL &
!	C9$		'CRLF' &
!	C2%		FOREIGN BUFFER CHANNEL NUMBER. &
!	C%		CURRENT BACKUP CLUSTER. &
!	D$		DEVICE NAME. &
!	D%()		INTERUPT DECODE ARRAY. &
!	D$()		INTERUPT DECODE ARRAY. &
!	D1%		FILE RECORD POINTER. &
!	D6$()		BACKUP BUFFER BLOCK FIELD ARRAY. &
!	D6$		BAD BLOCK 1 BUFFER FIELD VAR. &
!	D8$		BAD BLOCK 2 BUFFER FIELD VAR. &
!	DEV1%-DEV2%	RADIX-50 FOR DEVICE NAME. &
!	E%		ERROR VARIABLE. &
!	E0%		ENTRY TYPE. &
!	E2%		PACKAGE ERROR COUNT. &
!	F%		SUCCESS FLAG. &
!	F9%		NUMBER OF BLOCKS IN A BACKUP CLUSTER. &
!	I$		VERSION-EDIT. &
!	J$		JOB NUMBER STRING. &
!	J%		JOB NUMBER. &
!	J1%		JOB DATA BLOCK 1. &
!	K%		KB: NUMBER. &
!	K0%		I/O BLOCK. &
!	K1%		KB: LIST FILE FLAG. &
!	L0%		LIST FILE FLAG. &
!	L$()		MAGTAPE RECORD TRAILER ARRAY. &
!	L1$-L8$		USED IN SETUP OF L$(). &
!	L%		ERROR LINE. &
!	M%		MAGTAPE OR DISK FLAG. &
!	N%		NUMBER OF BACKUP CLUSTERS IN PRIMARY AND &
!			SECONDARY INDEX FILES. &
!	P0%		CURRENT RECORD POINTER. &
!	P1%		RECORD POINTER. &
!	P$		CHAIN MODULE NAME. &
!	PRIV.OFF$	SYS(PRIV.OFF$) TEMPORARILY DROPS PRIV'S. &
!	PRIV.ON$	SYS(PRIV.ON$) REGAINS PRIVILEGES. &
!	R%		PACKAGE STATUS WORD. &
!	R$		PACKAGE STATE VARIABLE. &
!	R%()		RESPONSE ARRAY. &
!	R0$		MODULE STATE VARIABLE. &
!	R0%()		VALID RESPONSE ARRAY. &
!	R1%		VALID RESPONSE VAR. &
!	S%		STACK POINTER. &
!	S%()		STACK. &
!	T$		TEXT TO SEND TO KB: OR OPERATOR. &
!	T%-T9%		INDEX LOADING LIST POINTERS. &
!	W$		WORK FILE NAME. &
!	W1$		OLD WORK FILE NAME IN INDEX LOAD. &
!	ALL Z'S		TEMPORARY LOCAL WORK VARIABLES. &
	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&
	&
!	NAME		LINE	USE &
!	10200			CREATE OLD BAD BLOCK LIST. &
!	11000			CREATE A DISK FILE FROM THE PRIMARY &
!				OR SECONDARY FILE. &
!	12000			LOAD THE INDEX. &

830	! &
	&
	!	P A C K A G E    S U B R O U T I N E S &
	&
	! Name			Lines		Use &
	! OPEN THE LISTING FILE	22500-22540	Ensure an open listing &
	!					file &
	! PROCESS ANY ERROR ON LIST FILE &
	!			22550-22580	Process errors and &
	!					minimize effect on &
	!					program &
	! PREPARE FOR DETACH	22600-22690	Do checks and manipulate &
	!					flags preliminary to &
	!					DETACH. &
	! PRINT A LINE TO LIST FILE &
	!			22700-22770	Print a line and trap &
	!					errors. &
	! CHECK FOR ATTACHED	22800-22830	Check KB: DDB for &
	!					ATTACHed state &
	! STATUS		22900-22990	Create a status report &
	!					string. &
	! LOG ERROR		23000-23090	Log an error on a record &
	!					in the work-file. &
	! ISSUE A MESSAGE	23100-23190	Issue a message to KB: &
	!					or to OPSER. &
	! GET/DECODE RESPONSE OR INTERRUPT COMMAND &
	!			23200-23390	Get a (KB:) line or a &
	!					(Send/Receive) message; &
	!					Call the decoder; &
	!					Process internal &
	!					commands. &
	! DECODE A LINE		23400-23490	Do a table-driven decode &
	! PROCESS REQUESTS	23499-23790	A set of routines which &
	!					perform additional &
	!					parsing and processing &
	!					of commands. &

840	! &
	&
	!	P A C K A G E    F U N C T I O N S &
	&
	! Name			Lines		Use &
	! FNN$(A%)		25000		Return the unsigned &
	!					value of A% as a string. &
	! FNN(A%)		25010		Return the unsigned &
	!					value of A% as a &
	!					floating point value. &
	! FNE$(E%,D%,P3%,P0%,E1%) &
	!			25100-25140	Create an error message &
	!					string from parameters. &
	! FNR%(S$,L%)		25200-25250	Match a  keyword. &
	! FNF0%(P0%,N$,D$)	25500-25690	Apply null default or &
	!					default string to a File &
	!					Descriptor Record. &
	! FNP%(P0%,P%,S$,Y0%,Y1%) &
	!			25800-25890	Turn a filename string &
	!					into a File Descriptor &
	!					Record. &
	! FNU$(P0%,C$,Z%,Z0%)	25900-25970	Turn a File Descriptor &
	!					Record into a filename &
	!					string, with switches. &
	! FNU0$(L%)		25980-25990	Return the NUM1$ of the &
	!					low byte of L%, or &
	!					'*', if the low byte is &
	!					255. &
	! FNO%(P0%,A0%,C0%)	26000-26090	Open the file described &
	!					in (work-file) record &
	!					P0% - drops and regains &
	!					temporary privs. &
	! FNA%			26100-26130 	Get the next free-list &
	!					record. &
	! FNA0%(P0%)		26200-26230	Put record P0% back &
	!					into the free-list. &
	! FNP0%(B%,L%,D%)	26300-26320	Set up a list for &
	!					processing. &
	! FNP1%()		26450-26470	Restore a list using the &
	!					data on the stack. &
	&
	&

900	&
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &

901	DIM #1%,Z0%(32767%,31%) &
	\ DIM #1%,Z0$(32767%)=64% &
				! WORK-FILE, PRIMARY.
902	DIM #2%,Z2%(32767%,31%) &
	\ DIM #2%,Z2$(32767%)=64% &
				! WORK-FILE, OLD.
903	DIM S%(30%)		! INTERNAL STACK.
905	DIM C%(4%),C(4%)	! THESE TWO ARRAYS ARE USED IN &
				! THE 'STATUS' INTERRUPT COMMAND, &
				! BELOW.
906	DIM D$(15%),D%(15%,2%)	! THESE TWO ARRAYS ARE USED IN &
				! THE DECODE OF INTERRUPT COMMANDS, &
				! BELOW.
907	DIM R%(2%),R0%(2%)	! THESE TWO ARRAYS ARE USED TO &
				! HOLD THE RESULTS/REQUESTS TO THE &
				! INTERRUPT COMMAND PROCESSOR, BELOW. &

915	DIM L$(8%)	! THIS IS THE MAGTAPE TRAILER LABEL ARRAY.
916	DIM D6$(8%)	! THIS IS THE DATA FIELD ARRAY. &

950	!	DIMENSION STATEMENTS FOR STANDARD ROUTINES
951	DIM Z%(30%)		! USED BY FNP%() FOR RETURN OF DATA
952	DIM Z1%(30%)		! USED BY FNP%() FOR TEMPORARY STORAGE &
				! OF DATA. &
	&
	&

999	&
	&
	&
	!	M A I N    C O D E &

1000	ON ERROR GOTO 19000 &
		! SET UP STANDARD ERROR TRAP. &
	\ Z$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &

1010	I$="BACLOD	V10.1-A" &
		! SET UP HEADER LINE. &

1020	PRINT I$ IF E0%=0% &
		! PRINT THE HEADER LINE PREPARATORY TO GIVING &
		! AN ERROR. &

1030	CHANGE SYS(CHR$(12%)) TO Z% &
	\ IF Z%(3%)+SWAP%(Z%(4%))<>15%*2% OR E0%<>2% THEN &
		PRINT "?ILLEGAL ENTRY - PLEASE RUN THE RESTOR PROGRAM" &
	\	GOTO 32767 &
		! ILLEGAL ENTRY IF WE DIDN'T COME FROM A COMPILED FILE &
		! OR IF NOT CHAINED. &

1035	A5%=SWAP%(Z%(6%))+Z%(5%) &
	\ Z$="SY" IF (Z%(26%) AND 1%)=0% &
	\ Z$=CHR$(Z%(23%))+CHR$(Z%(24%))+NUM1$(Z%(25%)) IF Z%(26%) AND 1% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+Z$) TO Z% &
	\ D5%=SWAP%(Z%(8%))+Z%(7%) &
		! GET DEVICE AND ACCOUNT OF LAST OPENED FILE. &

1040	T0=TIME(0%) &
	\ T1=TIME(1%) &
	\ T3=TIME(3%) &
	\ PRIV.OFF$=CHR$(6%)+CHR$(-21%)+CHR$(255%) &
	\ PRIV.ON$=CHR$(6%)+CHR$(-21%)+CHR$(0%) &
	\ OPEN W$ FOR INPUT AS FILE 1% &
	\ Z$=SYS(PRIV.ON$) &
	\ J1%=PEEK(520%) &
	\ A%=PEEK(PEEK(J1%+8%)+24%) &
	\ A$="["+FNN$(SWAP%(A%) AND 255%)+","+FNN$(A% AND 255%)+"]" &
	\ A%=256%	! Force to [1,0] &
	\ J%=(PEEK(518%) AND 255%)/2% &
	\ J$=FNN$(J%) \ J$="0"+J$ UNTIL LEN(J$)>1% &
	\ K0%=PEEK(J1%) &
	\ B0%=Z0%(0%,0%) &
	\ R9%=Z0%(0%,28%) &
	\ E2%=Z0%(0%,21%) &
	\ R%=Z0%(0%,26%) AND -8193% &
	\ K%=(SWAP%(PEEK(PEEK(K0%)+2%)) AND 255%) OR &
		(R% AND 16384%)=0% &
	\ Z$=SYS(PRIV.OFF$) &
	\ C8$=CHR$(7%) &
	\ C9$=CHR$(13%)+CHR$(10%) &
	\ GOSUB 22600 IF (R% AND 16384%)=0% &
	\ GOSUB 22500 IF L0%=0% AND K1%<>-1% &
		! SET UP ENTRY TIMES FOR STATUS REPORTS; &
		! OPEN THE WORK-FILE, AFTER DROPPING PRIVS; &
		! HOLD THE ADDRESS OF THE JDB; &
		! GET THE PPN FROM THE JDB; &
		! GET THE JOB NUMBER FROM THE EXEC; &
		! SET UP AN ASCII STRING FOR PRINTING THE PPN; &
		! GET THE ADDRESS OF THE JOB'S I/O BLOCK IN &
		!	THE EXEC  (FIRST WORD IN THE JDB); &
		! SET UP THE FREE-LIST POINTER, THE ERROR COUNT, &
		!	AND THE CURRENT PACKAGE STATE (WITHOUT &
		!	THE FLAG SAYING THAT THE DECODE TABLES &
		!	HAVE BEEN LOADED); &
		! IF THE JOB IS ATTACHED, THEN SET UP THE &
		!	KEYBOARD NUMBER OF 'KB:' IN K%, OR SET &
		!	K% TO -1% IF THE JOB IS DETACHED; &
		! SET UP THE TWO STRING CONSTANTS - C8$=<BELL>, &
		!  C9$=<CR><LF>; &
		! IF THE 'DETACHED' FLAG IS SET, THEN GO SET UP &
		!	PROPER STRINGS; &
		! GO OPEN THE LISTING FILE, AS APPROPRIATE. &

1050	R0%(0%)=(4% AND (A% AND -512%)=0% AND (R% AND 16384%)<>0%)+ &
		16%+128%+2048%+4096%+8192% &
	\ R0%(1%)=1% &
		! THE FOLLOWING INTERRUPT COMMANDS ARE PROCESSED &
		! ENTIRELY WITHIN THE GET/DECODE ROUTINE AND ARE &
		! ALWAYS LEGAL : &
		!	DETACH (IF PRIV'D AND NOT ALREADY DETACHED) &
		!	PAUSE &
		!	STATUS &
		!	LAST &
		!	LEGAL &
		!	NOTICE &
		! THE FOLLOWING INTERRUPT COMMANDS ARE PROCESSED BY THE &
		! PROGRAM BUT ARE ALWAYS LEGAL : &
		!	ABORT &

1060	R0$="INDEX LOAD" &

1080	R$="BEGIN "+R0$ &
	\ GOSUB 23200 &
	\ GOTO 9000 IF (R% AND 1%)<>0% &
	\ R$=R0$ &
		! GO SEE IF ANYTHING IS REQUESTED; &
		! END THIS NONSENSE IF 'ABORT' REQUESTED. &
	&

1100	&
	&
	&
	!	C A L L    T H E    N E C E S S A R Y &
	!		R O U T I N E S &

1105	IF (E%<>0%) AND (M%<>0%) THEN E%=0% &
			\ ON ERROR GOTO 1200 &
			\ OPEN D$ AS FILE 6%,RECORDSIZE B1% &
			\ Z%=MAGTAPE(3%,0%,6%) &
			\ Z%=MAGTAPE(4%,32767%,6%) FOR Z1%=1% TO 3% &
			\ CLOSE 6% &
		! IF A SELECT ERROR RUINED OUR FIRST OPEN, &
		! THEN WE MUST POSITION THE MAGTAPE PROPERLY. &
		! WE ARE OK WITH DISKS. &

1110	GOTO 1125 IF (Z0%(0%,29%) AND 4%)<>0% &
	\ C%(1%)=Z0%(0%,27%) &
	\ P0%=C%(1%) &
	\ D$=FNU$(P0%,"",-1%,-1%) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+D$) TO Z% &
	\ M%=STATUS AND 255% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+CHR$(Z%(23%))+CHR$(Z%(24%))) TO Z% &
	\ DEV1%=Z%(7%)+SWAP%(Z%(8%)) &
	\ DEV2%=Z%(9%)+SWAP%(Z%(10%)) &
	\ B1%=Z0%(C%(1%),17%) &
	\ B2%=Z0%(C%(1%),18%) &
	\ B5%=B1%/B2%-1% IF M%=0% &
	\ F9%=B1%/512% &
	\ B3%,Z0%(C%(1%),4%)=FNA% \ B4%=6% &
	\ ON ERROR GOTO 1200 &
	\ OPEN D$ AS FILE 6%, RECORDSIZE B1% &
	\ ON ERROR GOTO 19000 &
	\ FIELD #6%, 512% AS L1$,4% AS L$(1%),508% AS L2$,4% AS L$(2%), &
		508% AS L3$,4% AS L$(3%),508% AS L4$,4% AS L$(4%), &
		508% AS L5$,4% AS L$(5%),508% AS L6$,4% AS L$(6%), &
		508% AS L7$,4% AS L$(7%),508% AS L8$,4% AS L$(8%) IF &
						M%<>0% &
	\ FIELD #6%, 512% AS D6$(1%),512% AS D6$(2%),512% AS D6$(3%), &
			512% AS D6$(4%),512% AS D6$(5%),512% AS D6$(6%), &
			512% AS D6$(7%),512% AS D6$(8%) &
		! FIND THE BACKUP VOLUME RECORD. &
		! PUT THE DEVICE AND UNIT INTO D$. &
		! SCAN THE NAME AND SEE IF IT IS TAPE OR DISK. &
		! PUT THE DEVICE WITHOUT UNIT NUMBER INTO DEV1%-DEV2% &
		! PUT THE BUFFERSIZE INTO B1%. &
		! PUT THE CLUSTERSIZE INTO B2%. &
		! SET UP A CLUSTER INCREMENT FOR DISK. &
		! SET UP A RECORD INCREMENT FOR THE RSTS FILE. &
		! SET UP THE BAD BLOCK LIST FOR CREATION. &
		! OPEN THE BACKUP VOLUME WITH RECORDSIZE B1%. &
		! FIELD THE BUFFER. &

1120	GOSUB 23200 &
	\ F%=-1% &
	\ GOSUB 10200 IF M%=0% UNLESS (Z0%(0%,29%)=16%) AND &
					(Z0%(C%(1%),23%)<>0%) &
	\ GOTO 1250 IF F%=0% &
	\ GOSUB 11000 &
	\ GOTO 1250 IF F%=0% &
	\ GOTO 1400 IF R9%=4% &
	\ GOTO 1130 &
		! SEE IF ANY REQUESTS ARE OUTSTANDING. &
		! CREATE THE BAD BLOCK LIST IF THIS IS DISK. &
		! IF THE BAD BLOCK FILE WAS BAD WE ERRED. &
		! CREATE THE TEMP WORK FILE FROM THE DATA ON THE BACKUP &
		! SET. &
		! IF WE WERE SUCCESSFUL, LETS GO LOAD IT. &
		! IF NOT AND THIS WAS THE PRIMARY THAT FAILED, LETS TRY &
		! THE SECONDARY. IT MIGHT BE ON THIS VOLUME. &
		! IF WE FAILED, AND IT WAS THE SECONDARY THAT &
		! FAILED, WE HAVE NO RECOURSE BUT TO QUIT. &
		! ALL SUBROUTINES CHECK FOR ABORTS AT ENTRY, SO &
		! WE ARE GUARANTEED THAT WE WILL DROP THROUGH THIS &
		! STATEMENT IF AN ABORT WAS REQUESTED. &

1125	W1$=FNU$(Z0%(0%,9%),"",-1%,0%) &
	\ Z%=FNO%(Z0%(0%,9%),1%,2%) IF E%=0% &
	\ IF E% THEN T$=C9$+'DUE TO "'+FNE$(E%,0%,0%,0%,0%)+'" ERROR.' &
		\ T$="AUXILIARY INDEX LOAD FAILED - "+W1$+T$+" RUN ABORTED!!!" &
		\ GOSUB 23100 &
		\ GOSUB 22700 &
		\ Z%=MAGTAPE(1%,0%,6%) &
		\ R%=R% OR 1% &
		\ GOTO 9000 &
		! PUT THE AUXILIARY NAME INTO W1$ &
		! TRY TO OPEN IT ON CHANNEL #2%. &
		! IF THERE WAS AN ERROR, IT IS FATAL &
		! SO PUT OUT A MESSAGE AND END. &

1130	GOSUB 12000 &
	\ GOTO 1400 IF F%<>0% &
	\ GOTO 1125 IF (Z0%(0%,29%) AND 4%)<>0% &
	\ GOTO 1250 &
		! GO DO THE LOAD. &
		! IF EVERYTHING WAS FINE, EXIT SUCCESSFULLY AT 1400. &
		! IF NOT,	AUXILIARY FAILURE GOES TO 1125. &
		! 		PRIMARY OR SECONDARY FAILURE TO 1250. &

1200	E%=ERR &
	\ RESUME 1205 &
		! SET ERROR VARIABLE AND RESUME. &

1205	IF E%=14% OR E%=39% THEN T$=D$+ &
			" - HUNG OR WRITE LOCKED. RETRY LEGAL." &
			\ T$=T$+C9$+CHR$(9%)+"PLEASE REWIND MAGTAPE" IF &
						M%<>0% &
			\ R1%=32% &
			\ GOSUB 23100 \ GOSUB 23200 &
			\ GOTO 1105 IF (R2% AND 32%)<>0% &
			\ GOTO 1205 UNLESS (R% AND 1%)<>0% &
		! HUNG OR WRITE LOCKED ON OPEN. &

1210	GOTO 19000 UNLESS (R% AND 1%)<>0% &
	\ GOTO 9000 &
		! UNEXPECTED ERRORS COME HERE. ABORT COULD HAVE BEEN &
		! REQUESTED. &
	&

1250	! ANY FAILURE IN A PRIMARY OR SECONDARY LOAD COMES HERE. &
	! FAILURES CAN TAKE PLACE BECAUSE OF: &
	! 		A.) FAILURE TO CONSTRUCT A BAD BLOCK LIST FOR &
	! 			DISK. &
	! 		B.) FAILURE IN READING THE FILE FROM THE BACKUP &
	! 			MEDIUM. &
	! 		C.) FAILURE TO LIND THE LISTS FOR SOME REASON. &
	! A PRIMARY FAILURE WILL ALWAYS GO TRY THE SECONDARY. THIS &
	! ROUTINE WILL DETERMINE WHETHER A NEW VOLUME IS HOING TO BE &
	! NEEDED FOR THE SECONDARY. A SECONDARY FAILURE LEAVES US NO &
	! ALTERNATIVE BUT TO ABORT THINGS. &

1260	E%=0% &
	\ GOTO 1300 IF (Z0%(0%,29%) AND 16%)<>0% &
	\ T$="PRIMARY INDEX LOAD FAILED! STARTING SECONDARY LOAD." &
	\ GOSUB 23100 &
	\ GOSUB 22700 &
	\ Z0%(0%,29%)=16% &
	\ IF M%=0% THEN IF Z0%(C%(1%),25%)<>0% THEN GOTO 1120 &
				ELSE Z0%(0%,31%)=Z0%(0%,31%) OR 3% &
					\ GOTO 2000 &
		! RESET THE ERROR VARIABLE. &
		! IF THE SECONDARY FAILED, GO TO 1300 TO DIE. &
		! OTHERWISE, PUT OUT A MESSAGE AND LOOK FOR THE &
		! SECONDARY INDEX FILE IF THIS IS DISK. IF IT IS ON &
		! THIS VOLUME, GO TRY IT. IF NOT CHAIN TO BACMNT. &

1270	ON ERROR GOTO 11095 &
	\ Z%=MAGTAPE(3%,0%,6%) &
	\ Z%=MAGTAPE(4%,32767%,6%) FOR Z1%=1% TO 4% &
	\ GET #6% &
		! POSITION THE TAPE AND GET A RECORD TO SEE IF THE &
		! SECONDARY INDEX FILE IS ALSO ON THIS VOLUME. &

1280	IF E%=11% THEN E%=0% &
			\ Z0%(0%,31%)=Z0%(0%,31%) OR 3% &
			\ GOTO 2000 &
		! IF WE ENCOUNTERED ANOTHER EOF AFTER SKIPPING 4 OF THEM &
		! THE SECONDARY IS ON THE NEXT VOLUME. &

1290	STOP IF CVT$%(RIGHT(L$(RECOUNT/512%),3%))<>3% &
	\ Z%=MAGTAPE(4%,32767%,6%) &
	\ GOTO 1120 &
		! HOLD IT RIGHT HERE AND HAVE SOMEONE CALL US IF THERE &
		! IS SOMETHING OTHER THAN AN INDEX FILE ON THIS TAPE. &
		! SKIP ON MORE EOF AND GO TRY THE SECONDARY. &

1300	T$="SECONDARY INDEX LOAD FAILED. RUN ABORTED!!!" &
	\ GOSUB 23100 &
	\ GOSUB 22700 &
	\ Z%=MAGTAPE(1%,0%,6%) &
	\ R%=R% OR 1% &
	\ GOTO 9000 &
		! ALL OVER IF WE GOT HERE. &

1400	ON ERROR GOTO 1401 &
	\ Z%=MAGTAPE(3%,0%,6%) IF (M%<>0%) AND ((Z0%(0%,29%) AND 4%)=0%) &
		! POSITION MAGTAPE. &

1401	RESUME 1405 &

1405	Z0%(0%,29%)=0% &
	\ Z0%(0%,30%)=Z0%(0%,30%) OR 4% &
	\ GOTO 9000 &
		! SUCCESSFUL LOAD COMES HERE. &
	&

2000	P1%=FNA% &
	\ Z0%(P1%,1%)=Z0%(0%,13%) &
	\ Z0%(0%,13%)=P1% &
	\ Z0%(P1%,6%)=D5% &
	\ Z0%(P1%,8%)=A5% &
	\ Z0%(P1%,9%)=3243% &
	\ Z0%(P1%,10%)=21380% &
	\ Z0%(P1%,13%)=1% &
	\ Z0%(P1%,14%)=12417% &
	\ GOTO 9000 &
		! SET UP A CHAIN TO BACMNT. &
	&

9000	&
	&
	&
	!	C O M P L E T I O N    R O U T I N E S &

9010	R$=R0$+" COMPLETE" &
	\ GOSUB 23200 &
	\ GOSUB 22900 &
	\ IF L0%<>0% THEN &
		GOSUB 22700 &
	  ELSE	IF (R% AND 16384%)<>0% THEN &
			GOSUB 23100 &
		! SET UP TO LOG COMPLETION: &
		!	SET UP PHASE NAME STRING; &
		!	GO SEE IF ANY INTERRUPT COMMANDS ARE &
		!	AROUND; &
		!	GO SET UP STATUS STRING; &
		!	IF THE LISTING FILE IS AROUND, THEN &
		!		SEND THE STATUS STRING THERE; &
		!	ELSE	IF THE PROGRAM IS ATTACHED, THEN &
		!			SEND THE STATUS STRING TO THE &
		!			KB: &
		!		ELSE	NO STATUS IS PRINTED. &

9020	P0%=Z0%(0%,13%) &
	\ IF P0%=0% THEN &
		P$="" &
	  ELSE	P$=FNU$(P0%,"",-1%,-1%) &
	\	Z0%(0%,13%)=Z0%(P0%,1%) &
	\	Z0%(P0%,1%)=0% &
	\	Z%=FNA0%(P0%) &
		! HOLD THE POINTER TO THE NEXT RECORD IN THE CHAIN LIST; &
		! IF THE POINTER IS ZERO, THEN &
		!	RETURN A NULL STRING; &
		! ELSE	MAKE THE FILENAME STRING; &
		!	TAKE THIS ONE OUT OF THE CHAIN LIST; &
		!	RETURN THE RECORD TO THE FREE LIST. &

9030	GOSUB 23200 &
	\ IF (R% AND 1%)<>0% THEN &
		T$="?RUN ABORTED" &
	\	GOSUB 22720 &
	\	GOSUB 23100 &
		! GO SEE IF ANYTHING NEW HAS BEEN TYPED; &
		! IF AN ABORT WAS REQUESTED, THEN &
		!	SET UP AND PRINT A MESSAGE TO THAT EFFECT INTO &
		!		THE LISTING FILE. &

9040	Z0%(0%,0%)=B0% IF B0%<>0% &
	\ Z0%(0%,26%)=R% &
	\ CLOSE Z% FOR Z%=1% TO 12% &
	\ ON ERROR GOTO 19000 &
	\ Z$=SYS(PRIV.ON$) &
	\ CHAIN P$ LINE 31000 IF P$<>"" AND (R% AND 1%)=0% &
	\ Z$=SYS(PRIV.OFF$) &
	\ GOTO 32700 &
		! RESTORE THE FREE-LIST POINTER AND THE PACKAGE STATUS &
		!	WORDS TO THE WORK-FILE; &
		! CLOSE OUT ALL THE FILES; &
		! CHAIN TO NEXT PROGRAM. &
		! IF WE ABORTED OR NO FILENAME IS SET UP, END THINGS. &
	&
	&

10200	! THIS IS A SUBROUTINE WHICH CREATES A BAD BLOCK LIST FROM &
	! THE BAD BLOCK RECORD OF A BACKUP DISK. THIS IS NECESSARY &
	! BECAUSE WE HAVE, THUS FAR, ONLY SPECIFIED &
	! THAT THIS IS THE DISK WE WISH TO USE. WE HAVE NOT FORMATTED IT &
	! IN THIS PROGRAM, AND THEREFORE HAVE NO LIST AT THIS TIME. &

10205	F%=0% &
	\ ON ERROR GOTO 10295 &
	\ GOTO 10290 IF (R% AND 1%)<>0% &
	\ GET #6%, RECORD Z0%(C%(1%),22%) &
	\ FIELD #6%, B2%/2% AS D6$, B2%/2% AS D8$ &
	\ FOR D1%=1% TO B2%/2%-1% STEP 2% &
		\ Z%=CVT$%(RIGHT(D6$,D1%)) &
		\ GOTO 10280 IF Z%=0% &
		\ Z0%(B3%,B4%)=Z% &
		\ B4%=B4%+1% &
		\ B3%,Z0%(B3%,1%)=FNA% IF B4%=32% &
		\ B4%=6% IF B4%=32% &
	\ NEXT D1% &
		! INITIALIZE THE FINE VS. NOT SO FINE FLAG. &
		! SET THE TEMPORARY ERROR TRAP. &
		! EXIT ON ABORT. &
		! GET THE BAD BLOCK RECORD OFF THE DISK. &
		! FIELD THE BUFFER. &
		! CREATE THE LIST. &

10280	F%=-1% &
		! SET THE "EVERYTHING'S FINE" FLAG. &

10290	ON ERROR GOTO 19000 &
	\ RETURN &
		! RESET THE ERROR TRAP AND RETURN. &

10295	RESUME 10300 &
		! RESUME RIGHT AWAY TO GIVE US EXIT FLEXIBILTITY. &

10300	T$="ERROR READING BAD BLOCK FILE OF INDEX DISK!" &
	\ GOSUB 22700 &
	\ GOTO 10290 &
		! ANY ERROR WE GET HERE BLOWS IT. &
	&

11000	! THIS IS THE ROUTINE WHICH CREATES A DISK FILE FROM THE DATA &
	! ON THE BACKUP VOLUME. THE REASON WE DO THIS IS THAT IT IS MUCH &
	! MORE CONVENIENT TO HAVE AN INDEX FILE IN THE SAME FORMAT AS &
	! WHEN THE BACKUP SET ITSELF WAS WRITEN. WE CAN OVERLAY IT &
	! WITH A VIRTUAL CORE ARRAY AND TRANSFER THE INFO OVER TO &
	! THE RESTORE WORK FILE WITH LITTLE DIFFICULTY. &

11005	F%=0% &
	\ ON ERROR GOTO 11095 &
	\ GOTO 11090 IF (R% AND 1%)<>0% &
	\ N%=32767% &
	\ GOTO 11010 IF M%<>0% &
	\ C%=Z0%(C%(1%),23%)-1% IF (Z0%(0%,29%) AND 8%)<>0% &
	\ C%=Z0%(C%(1%),25%)-1% IF (Z0%(0%,29%) AND 16%)<>0% &
	\ N%=Z0%(C%(1%),24%) &
		! INITIALIZE THE FINE VS. NOT SO FINE FLAG. &
		! SET THE LOCAL ERROR TRAP. &
		! SET N% TO A LARGE NUMBER IN CASE ITS MAGTAPE. &
		! GOTO THE NEXT STATEMENT IF THIS IS MAGTAPE. &
		! DEPENDING ON WHETHER WE ARE LOADING THE PRIMARY OR &
		! SECONDARY WE MUST GET THE PROPER STARTING RECORD #. &
		! WE GET THE LENGTH IN ANY CASE. &

11010	W1$=FNU$(Z0%(0%,18%),"",-1%,0%) &
	\ OPEN W1$ FOR OUTPUT AS FILE 4% &
	\ C4%=4%+SWAP%(6%) &
		! WE GIVE THE FILE A NICE TEMP NAME. &
		! OPEN IT FOR OUTPUT. &
		! SET UP A VARIABLE FOR FORIEGN BUFFERING. &

11020	C%=C%+1% &
	\ WHILE B3%<>0% &
		\ FOR B4%=B4% TO 31% &
			\ GOTO 11020 IF C%=Z0%(B3%,B4%) &
			\ GOTO 11030 IF FNN(C%)<FNN(Z0%(B3%,B4%)) &
		\ NEXT B4% &
		\ B3%=Z0%(B3%,1%) &
		\ B4%=6% &
	\ NEXT &
		! CHECK FOR A BAD DISK CLUSTER. WE DIDN'T WRITE ON IT &
		! SO WE SHOULDN'T READ IT EITHER. &

11030	ON ERROR GOTO 11095 &
	\ GET #6%, RECORD C% IF M%=0% &
	\ GET #6% IF M%<>0% &
	\ PUT #C4%, COUNT (B1%/512%)*512% &
	\ N%=N%-1% &
	\ GOTO 11080 IF N%=0% &
	\ C%=C%+B5% &
	\ GOTO 11020 &
		! SET THE ERROR TRAP. &
		! GET THE PROPER IF THIS IS DISK. &
		! GET A RECORD FROM MAGTAPE, WE ARE POSITIONED FOR IT. &
		! DECREMENT THE COUNTER. THIS HAS AN EFFECT ON DISK ONLY &
		! BECAUSE MAGTAPE WILL EVENTUALLY HIT AN EOF MARK TO &
		! TELL US WHEN ITS DONE. THIS DOESN'T HAPPEN WITH DISK &
		! SO WE MUST HAVE A COUNT. &

11080	CLOSE 4% &
	\ OPEN W1$ FOR INPUT AS FILE 2% &
	\ F%=-1% &
		! CLOSE WORK FILE ON BLOCK I/O CHANNEL, &
		! REOPEN ON VIRTUAL ARRAY CHANNEL. &
		! SET THE "EVERYTHING'S FINE" FLAG. &

11090	ON ERROR GOTO 19000 &
	\ RETURN &
		! EXIT. &

11095	E%=ERR &
	\ L%=ERL &
	\ GOTO 19000 IF E%=4% OR L%=11080% &
	\ RESUME 11100 &
		! "NO ROOM" ERROR IS FATAL. &
		! SET THE ERROR VARIABLES AND RESUME. &

11100	IF E%=11% THEN E%=0% &
			\ GOTO 11080 IF L%<>1270% &
			\ GOTO 1280 &
		! WE ALSO USE THIS TRAP WHEN WE ARE LOOKING FOR THE &
		! SECONDARY INDEX FILE SO WE MUST DECIDE WHERE TO GO. &
		! END OF FILE MEANS WE ARE DONE IN THIS ROUTINE. &

11110	IF (E%=14%) OR (E%=39%) THEN E%=0% &
			\ T$=D$+" - HUNG OR WRITE LOCKED. RETRY LEGAL." &
			\ T$=T$+C9$+CHR$(9%)+"PLEASE REWIND MAGTAPE" IF &
						M%<>0% &
			\ R1%=32% &
			\ GOSUB 23100 \ GOSUB 23200 &
			\ GOTO 11090 IF (R2% AND 32%)=0% &
			\ ON ERROR GOTO 11095 &
			\ GOTO 11030 IF M%=0% &
			\ Z%=MAGTAPE(3%,0%,6%) &
			\ Z%=MAGTAPE(4%,32767%,6%) FOR Z1%=1% TO 3% &
			\ Z%=MAGTAPE(4%,32767%,6%) FOR Z1%=1% TO 2% IF &
					(Z0%(0%,29%) AND 16%)<>0% &
			\ GOTO 11000 IF L%=11030 &
			\ GOTO 1270 &
		! HUNG OR WRITE LOCKED ERRORS COME HERE. &

11120	GOTO 11090 IF L%=11030 &
	\ GOTO 19000 &
		! UNEXPECTED ERRORS COME HERE. &
	&

12000	! THIS IS THE ROUTINE WHICH ACTUALLY PUTS THE VOLUME LIST FROM &
	! THE OLD BACKUP WORK FILE INTO THE INPUT VOLUME LIST OF THE &
	! RESTORE WORK FILE. THE LIST IS LOADED IN THE FOLLOWING MANNER &
	! SO AS TO DECREASE DISK ACCESS DURING SELECT AND TRANSFER &
	! PHASES: &
	!	V-EV-B1-B2-V-EV-B1-B2-V-EV-B1-B2-A-EA-F-AF-EF- &
	!	A-EA-F-AF-EF-A-EA-F-AF-EF &
	! &
	! 	WHERE	V=VOLUME RECORD &
	! 		EV=VOLUME ERROR RECORD &
	! 		B1=BAD BLOCK LIST &
	! 		B2=BAD BLOCK 2 LIST &
	! 		A=ACCOUNT RECORD &
	!		EA=ACCOUNT ERROR RECORD &
	!		F=FILE RECORD &
	!		AF=FILE ATTRIBUTE RECORD &
	!		EF=FILE ERROR RECORD &
	! &
	!	THE PRECEDING EXAMPLE HAS 3 VOLUMES EACH WITH ONE ERROR &
	!	RECORD, ONE BAD BLOCK RECORD, ONE BAD BLOCK 2 RECORD, &
	!	AND ONE ACCOUNT RECORD. EACH ACCOUNT HAS ONE ERROR &
	! 	RECORD, AND ONE FILE RECORD. EACH FILE HAS ONE &
	!	ATTRIBUTE RECORD AND ONE ERROR RECORD. &

12010	F%=0% &
	\ ON ERROR GOTO 12095 &
	\ GOTO 12090 IF (R% AND 1%)<>0% &
	\ T0%=C%(1%) &
	\ WHILE T0%<>0% &
	\ Z%=Z2%(8%,0%) &
	\ T%=Z2%(0%,4%) &
	\ WHILE Z2%(T%,11%)<>Z0%(0%,15%) &
		\ T%=Z2%(T%,1%) &
	\ NEXT &
	\ T0%=0% &
	\ Z2%(T%,6%)=Z0%(C%(1%),6%) &
	\ Z2%(T%,30%)=Z0%(C%(1%),30%) &
	\ Z2%(T%,31%)=Z0%(C%(1%),31%) &
	\ NEXT &
	\ Z0%(0%,2%)=FNA% IF Z0%(0%,2%)=0% &
	\ T1%=Z0%(0%,2%) &
	\ T2%=Z2%(0%,4%) &
	\ T0%=-1% &
	\ C%(1%)=0% &
	\ WHILE T2%<>0% &
		\ T1%,Z0%(T1%,1%)=FNA% UNLESS T0%=-1% &
		\ SAVE.DEV%=((DEV1% OR DEV2%)=0%) &
		\ DEV1%=Z0%(T1%,6%) IF SAVE.DEV% &
		\ DEV2%=Z0%(T1%,7%) IF SAVE.DEV% &
	!		Load DEV1&2 if they didn't get loaded in 1110 'cause &
	!		we used a pre-LOAded index file. DEV1&2 hold the input &
	!		device name on THIS system.  (see comments below). &
		\ Z0$(T1%)=Z2$(T2%) &
		\ Z0%(0%,27%),C%(1%)=T1% IF T%=T2% &
		\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+"_" &
			+RAD$(Z0%(T1%,6%))+RAD$(Z0%(T1%,7%))+":") &
			TO Z1% &
		\ Z0%(T1%,6%)=DEV1% IF SWAP%(Z1%(30%))<0% &
		\ Z0%(T1%,7%)=DEV2% IF SWAP%(Z1%(30%))<0% &
		\ FOR T3%=3% TO 5% &
			\ T0%=-1% &
			\ T4%=Z2%(T2%,T3%) &
			\ WHILE T4%<>0% &
				\ T5%,Z0%(T1%,T3%)=FNA% IF T0%=-1% &
				\ T5%,Z0%(T5%,1%)=FNA% IF T0%=0% &
				\ T0%=0% &
				\ Z0$(T5%)=Z2$(T4%) &
				\ T4%=Z2%(T4%,1%) &
			\ NEXT &
		\ NEXT T3% &
		\ T0%=0% &
		\ T2%=Z2%(T2%,1%) &
		\ Z0%(T1%,30%)=Z0%(T1%,30%) OR 3% IF C%(1%)<>T1% &
					IF R9% AND 8% &
		\ Z0%(T1%,Z%)=0% FOR Z%=30% TO 31% IF C%(1%)<>T1% &
					UNLESS R9% AND 8% &
	\ NEXT &
	\ T1%=Z0%(0%,2%) &
	\ T2%=Z2%(0%,4%) &
		! GET ALL THE VR'S AND THEIR ERROR AND BAD BLOCK LISTS &
		! TRANSFERRED. &
		! FOR EACH VR, DO THE FOLLOWING SPECIAL CHECK: &
		!	IF THE DEVICE NAME SPECIFIED IN THE VR IS &
		!	UNTRANSLATABLE (MAY NOT EXIST ON THIS SYSTEM &
		!	SUCH AS MM: OR MS:), CHANGE THE DEVICE NAME &
		!	TO THE DEVICE NAME SPECIFIED IN THE &
		!	"FROM DEVICE" QUESTION. &

12020	WHILE T2%<>0% &
		\ T3%=2% &
		\ T4%=Z2%(T2%,T3%) &
		\ T0%=-1% &
		\ WHILE T4%<>0% &
			\ T5%,Z0%(T1%,T3%)=FNA% IF T0%=-1% &
			\ T5%,Z0%(T5%,1%)=FNA% IF T0%=0% &
			\ Z0$(T5%)=Z2$(T4%) &
			\ T6%=3% &
			\ T7%=Z2%(T4%,T6%) &
			\ T0%=-1% &
			\ WHILE T7%<>0% &
				\ T8%,Z0%(T5%,T6%)=FNA% IF T0%=-1% &
				\ T8%,Z0%(T8%,1%)=FNA% IF T0%=0% &
				\ T0%=0% &
				\ Z0$(T8%)=Z2$(T7%) &
				\ T7%=Z2%(T7%,1%) &
			\ NEXT &
			\ T7%=Z2%(T4%,T3%) &
			\ T0%=-1% &
			\ WHILE T7%<>0% &
				\ T8%,Z0%(T5%,T3%)=FNA% IF T0%=-1% &
				\ T8%,Z0%(T8%,1%)=FNA% IF T0%=0% &
				\ Z0$(T8%)=Z2$(T7%) &
				\ FOR T6%=2% TO 3% &
					\ T0%=-1% &
					\ T9%=Z2%(T7%,T6%) &
					\ WHILE T9%<>0% &
						\ T%,Z0%(T8%,T6%)=FNA% &
							IF T0%=-1% &
						\ T%,Z0%(T%,1%)=FNA% &
							IF T0%=0% &
						\ T0%=0% &
						\ Z0$(T%)=Z2$(T9%) &
						\ T9%=Z2%(T9%,1%) &
					\ NEXT &
				\ NEXT T6% &
				\ T0%=0% &
				\ T7%=Z2%(T7%,1%) &
			\ NEXT &
			\ T0%=0% &
			\ T4%=Z2%(T4%,1%) &
		\ NEXT &
		\ T1%=Z0%(T1%,1%) &
		\ T2%=Z2%(T2%,1%) &
	\ NEXT &
	\ T1%=Z0%(0%,2%) &
	\ T2%=Z2%(0%,4%) &
	\ T5%=Z0%(T1%,2%) &
	\ T4%=Z2%(T2%,2%) &
		! GET ALL THE ACCOUNTS AND THEIR LISTS COPIED. &

12080	F%=-1% &
		! ITS A GOOD LOAD. &

12090	ON ERROR GOTO 19000 &
	\ RETURN &
		! EXIT &

12095	E%=ERR &
	\ GOTO 19000 IF E%=4% &
	\ RESUME 10290 &
		! FATAL ERROR IF "?No room for user on device". &
		! OTHERWISE, WE DON'T REALLY CARE WHY THIS THING FAILED, &
		! SO WE JUST EXIT WITH E% SET. &
	&

19000	&
	&
	&
	!	T R A P    F O R    U N H A N D L E D    E R R O R S &

19010	RESUME 19020 &
		! BE SURE TO CLEAR THE ERROR. &

19020	Z$=SYS(PRIV.OFF$) IF LEN(PRIV.OFF$) &
	\ E%=ERR IF E%=0% &
	\ T$=RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E% AND 63%)),4%) &
	\ T$="?UNEXPECTED ERROR IN "+I$+" - "+T$+" AT LINE"+NUM$(ERL)+ &
		" - FATAL" &
	\ T$=T$+C9$+" ON 'CHAIN' TO '"+P$+"'" IF ERL=9040 &
	\ IF (ERL<23100 OR ERL>23199) AND ERL>1099 THEN &
		GOSUB 23100 &
	  ELSE	PRINT T$ &
		! SET UP TEXT OF ERROR MESSAGE; &
		! IF THE ERROR DID NOT HAPPEN WITHIN THE 'ISSUE A &
		!	MESSAGE' ROUTINE, THEN &
		!		GO ISSUE THE MESSAGE; &
		! ELSE	PRINT IT OUT. &

19030	RESUME 32700 &
		! AND EXIT THE PROGRAM. &
	&

22500	&
	&
	&
	!	O P E N    T H E    L I S T I N G    F I L E &
	&
	! This routine is called at the beginning of the program &
	! to ensure that the listing file is open, if it is possible. &
	! It is also called upon detach/attach to determine if the &
	! explicit file needs to be opened/closed. &
	&
	! The data used by the routine are the following : &
	!	K%	- Keyboard to which job is ATTACHED, or -1, &
	!		  DETACHED. &
	!	Z0%(0%,5%) &
	!		- FHR word 5, pointing to the record holding the &
	!		  listing file name/spec. &
	!	L0%	- Channel # of the listing file - if non-zero, &
	!		  no work is performed, since the file is &
	!		  either already OPEN or has already been &
	!		  determined to be inaccessible. &
	&
	! The data returned by the routine are : &
	!	L0%	- Channel # of the OPENed listing file, or -1. &
	!		  If the OPEN failed (see below), then L0% is &
	!		  returned as -1.  If the OPEN succeeded, but &
	!		  the list file was the user's KB: (ie, the KB: &
	!		  to which the job is ATTACHed), then the &
	!		  channel is CLOSEd, and L0% is returned as 0. &
	!		  Otherwise, L0% is returned as the channel to &
	!		  list to (hardwired as #3). &
	!	K1%	- If the list file is a KB:, then K1% is &
	!		  returned holding the kb#.  If the list file is &
	!		  not a KB:, K1% is returned as -1. &

22510	IF L0%<>0% THEN &
		GOTO 22530 &
	ELSE	Z%=FNO%(Z0%(0%,5%),6%,3%) &
	\	IF E%=0% THEN &
			L0%=3% &
		ELSE	E1%=3% &
	\		E3%=1% &
	\		GOSUB 22550 &
	\		IF L0%>0% THEN &
				GOTO 22510 &
		! IF THE LISTING FILE IS NOT YET OPEN, THEN &
		! SET UP TO OPEN THE LISTING FILE; &
		! TRY OPENING IT; &
		! IF OKAY, SET UP THE CHANNEL NUMBER FOR LATER; &
		! OTHERWISE, ASK FOR EITHER A 'RETRY' OR AN 'IGNORE'; &
		! IF 'RETRY, THEN TRY AGAIN; ELSE IF IGNORE, THEN &
		! NOTIFY THE USER OF THE CONDITION. &

22520	K1%=-1% &
	\ IF L0%>0% AND (STATUS AND 255%)=2% THEN &
		Z$=SYS(PRIV.ON$) &
	\	K1%=(SWAP%(PEEK(PEEK(K0%+2%*L0%)+2%)) AND 255%) &
	\	Z$=SYS(PRIV.OFF$) &
		! SET UP THE KEYBOARD NUMBER OF THE LIST FILE, IF IT &
		! IS A KEYBOARD FILE. &

22530	IF K1%=K% AND K%>-1% THEN &
		CLOSE L0% &
	\	L0%=0% &
		! IF THE KEYBOARD NUMBERS ARE EQUAL (AND BOTH ARE &
		! REALLY KEYBOARD NUMBERS) THEN &
		!	CLOSE THE LISTING FILE, SINCE WE DON'T WANT &
		!	THINGS PUT IN THERE TWICE; &
		!	SET THE FLAG TO NOT OPEN. &

22540	RETURN &
		! AND EXIT. &
	&
	&

22550	&
	&
	&
	!	P R O C E S S    A N Y    E R R O R    O N &
	!		L I S T    F I L E &
	&
	! The purpose of this routine is to keep the program's state as &
	! well isolated as possible from problems with the listing file. &
	! In order to ensure this, all variables which might affect the &
	! outside program must be pushed onto the stack before any &
	! operations are performed, and popped from the stack upon &
	! completion. &

22560	S%=S%+1% \ S%(S%)=R2% &
	\ S%=S%+1% \ S%(S%)=D% &
	\ S%=S%+1% \ S%(S%)=P3% &
	\ S%=S%+1% \ S%(S%)=P0% &
	\ S%=S%+1% \ S%(S%)=L0% &
	\ P0%,D%=0% &
	\ P3%,C%(D%)=Z0%(0%,5%) &
	\ L0%=-1% &
	\ R1%=32%+512% &
	\ GOSUB 23000 &
	\ L0%=S%(S%) \ S%=S%-1% &
	\ P0%=S%(S%) \ S%=S%-1% &
	\ P3%=S%(S%) \ S%=S%-1% &
	\ D%=S%(S%) \ S%=S%-1% &
	\ IF R2%=512% THEN &
		T$="PROCEEDING WITHOUT LISTING FILE" &
	\	GOSUB 23100 &
	\	ON ERROR GOTO 22590 &
	\	CLOSE L0% &
	\	L0%=-1% &
		! PUSH THE CURRENT VALUES OF: &
		!	RESPONSE FROM OPERATION COMMANDS; &
		!	ROUTINE DEPTH; &
		!	SOURCE RECORD; &
		!	CURRENT RECORD; &
		!	LISTING FILE CHANNEL NUMBER; &
		! SET UP THE ERROR RECORD AS THE LISTING FILE NAME &
		! RECORD; &
		! SET UP THE ERROR CHANNEL NUMBER AS THE LISTING &
		! FILE CHANNEL; &
		! SET THE LISTING FILE CHANNEL NUMBER TO -1 SO THE &
		! NOTIFICATION ROUTINES DO NOT TRY TO PRINT OUT &
		! THE MESSAGE ONTO THE LISTING FILE; &
		! SET UP LEGAL &
		! RESPONSES AS 'RETRY' AND 'IGNORE'; &
		! CALL THE NOTIFICATION ROUTINE; &
		! RECOVER THE LIST FILE CHANNEL, THE CURRENT RECORD, &
		! SOURCE RECORD, AND ROUTINE DEPTH; &
		! IF THE RESPONSE WAS 'IGNORE', THEN &
		!	NOTIFY THAT NO LISTING WILL BE MADE; &
		!	SET UP TO TRAP ERROR ON CLOSE; &
		!	EXECUTE THE CLOSE; &
		!	(ONLY GET HERE IF THE CLOSE WAS SUCCESSFUL) &
		!	RESET THE CHANNEL NUMBER TO -1, SO NO MORE LIST &
		!	 ACTIVITY WILL TAKE PLACE. &

22570	R2%=S%(S%) \ S%=S%-1% &
		! POP THE OLD RESPONSE. &

22580	ON ERROR GOTO 19000 &
	\ RETURN &
		! AND EXIT. &

22590	L0%=-1% &
	\ RESUME 22570 &
		! THIS LINE IS EXECUTED WHEN AN ERROR OCCURRED ON THE &
		! LIST FILE CLOSE OPERATION;  THE FILE IS CLOSED, SO &
		! SET THE CHANNEL NUMBER TO -1, AND RESUME. &
	&

22600	&
	&
	!	P R E P A R E    F O R    D E T A C H &
	&
	! In order to DETACH, the following actions must be taken : &
	!	1) The job must be entered as a receiver; &
	!	2) The receiver table entry for this job must be &
	!	   located so the program can do a fast check for &
	!	   messages queued; &
	!	3) An 'ONLINE' command must be sent to OPSER, to inform &
	!	   OPSER of this job's existence; &
	!	4) The 'OPEN LIST FILE' routine must be executed, in &
	!	   case the list file is "KB:". &
	&
	! Errors may occur in steps 1 and 3.  An error in either &
	! place will cause a failure to DETACH. &
	&
	! The routine uses the following data, all of which are flags &
	! in variable R% : &
	!	2048	RECEIVER FLAG &
	!		If this is set, the job has already entered &
	!		itself as a receiver, so no entering will be &
	!		performed. &
	!	4096	ONLINE FLAG &
	!		if this is set, the job has already sent online &
	!		online notification to opser, and no new online &
	!		will be sent. &
	!	J%	- Job number of this job. &
	&
	! The data returned by the routine are : &
	!	E%	- If some error occurred which prevented the &
	!		  DETACH.  (If this is set, the variable T1$ &
	!		  will hold text describing the error.) &
	!	T1$	- Text of the error, if E% is non-zero. &
	!	J0%	- If no error, returns the value such that the &
	!		  high byte of the word PEEK(J0%) gives the &
	!		  number of messages queued to this job. &
	!	R%	- The following values : &
	!		-32768	- 0 => No prompt required &
	!		 16384	- 0 => Not ATTACHed &
	!		 4096	- 1 => Entered in receive table &
	!		 2048	- 1 => ONLINE to OPSER &
	!	T0$	- If no error is encountered, this string will &
	!		  hold the string information necessary to do a &
	!		  'SEND' to OPSER using the command &
	!			Z$=SYS(T0$+<string to send>) &
	!	K%	- Keyboard number of KB: to which job is &
	!		  ATTACHed; if no error, returned as -1. &
	&

22605	IF K1%=K% AND K%<>-1% THEN &
		T1$="KEYBOARD IS LIST FILE" &
	\	E%=10% &
	\	GOTO 22680 &
		! IF THE CURRENT KEYBOARD IS THE LIST FILE, THEN &
		!	DO NOT ALLOW A DETACH, SINCE A DETACH WOULD &
		!	 NOT REALLY FREE THE KEYBOARD, AND IT WOULD &
		!	 BE RATHER CONFUSING. &

22610	T1$="" &
	\ IF (R% AND 2048%)<>0% THEN &
		GOTO 22630 &
	ELSE	ON ERROR GOTO 22620 &
	\	Z$=SYS(PRIV.ON$) &
	\	Z$=SYS(CHR$(6%)+CHR$(18%)+CHR$(1%)+CHR$(128%+5%)+ &
			MID(SYS(CHR$(6%)+CHR$(-10%)+"BACK"+J$),7%,4%)) &
	\	Z$=SYS(PRIV.OFF$) &
	\	E%=-1% &
	\	T1$="OUTSTANDING MESSAGE" &
	\	GOTO 22680 &
		! IF NOT ALREADY DECLARED AS A RECEIVER, THEN DECLARE &
		! NOW; &
		! IF THERE IS ALREADY A MESSAGE OUTSTANDING, THERE IS A &
		! SYNCHRONIZATION ERROR, AND THE DETACH SHOULD NOT BE &
		! ALLOWED, IF IT CAN BE AVOIDED. &

22620	ON ERROR GOTO 19000 &
	\ Z$=SYS(PRIV.OFF$) &
	\ IF ERR=5% THEN &
		RESUME 22630 &
	  ELSE	E%=ERR &
	\	T1$="NO ROOM IN RECEIVE TABLE" IF E%=4% &
	\	T1$="ILLEGAL SYS() USAGE" IF E%=18% &
	\	T1$="UNEXPECTED ERROR - "+RIGHT(SYS(CHR$(6%)+CHR$(9%)+ &
			CHR$(E%)),4%) IF E%<>4% AND E%<>18% &
	\	RESUME 22680 &
		! IF THE ERROR IS 'NO MESSAGES', THEN ALL IS WELL; &
		! IF THE ERROR IS 'NO ROOM' OR 'ILLEGAL SYS()', THEN &
		! DON'T ALLOW DETACH; IF THE ERROR IS SOMETHING ELSE, &
		! DON'T ALLOW A DETACH. &

22630	Z$=SYS(PRIV.ON$) &
	\ J0%=PEEK(PEEK(J1%+8%)+18%) &
	\ Z$=SYS(PRIV.OFF$) &
	\ R%=R% OR (2048% AND J0%<>0%) &
	\ J0%=J0%+14% &
		! THIS LINE IS EXECUTED ONLY IF AN ERROR 5 &
		! OCCURRED ON THE 'RECEIVE' CALL; FIND THE LOCATION &
		! OF THE RECEIVE TABLE ENTRY FOR THIS JOB, AND THE &
		! ADDRESS OF THE QUEUED MESSAGE COUNTER. &

22640	T0$=CHR$(6%)+CHR$(18%)+CHR$(-1%)+CHR$(0%)+ &
		MID(SYS(CHR$(6%)+CHR$(-10%)+"OPSER"),7%,4%) &
	\ IF (R% AND 4096%)=0% THEN &
		R%=R% AND -16385% &
	\	T$=CHR$(128%+64%)+"ONLINE" &
	\	GOSUB 23100 &
	\	IF (R%  AND 4096%)=0% THEN &
			R%=R% OR 16384% &
	\		T1$="OPSER HUNG" &
	\		E%=-1% &
	\		GOTO 22680 &
		! SET UP THE 'SEND TO OPSER' STRING; &
		! IF NOT ALREADY ONLINE TO OPSER, THEN &
		!	SEND AN ONLINE MESSAGE; &
		!	IF IT FAILED TO SEND, THEN &
		!		FLAG US ATTACHED; &
		!		SET UP ERROR; &
		!		GOTO EXIT. &

22670	E%=0% &
	\ R%=(R% AND 16383%) &
	\ K%=-1% &
	\ GOSUB 22500 IF L0%=0% &
		! IF THIS LINE IS REACHED, RESET ERROR, RESET &
		! ATTACHED AND PROMPT NEEDED FLAGS, AND GO SEE &
		! IF THE LISTING FILE NEEDS TO BE OPENED. &

22680	ON ERROR GOTO 19000 &
	\ IF E%<>0% THEN &
		Z$=SYS(PRIV.ON$) &
	\	Z$=SYS(CHR$(6%)+CHR$(18%)) &
	\	Z$=SYS(PRIV.OFF$) &
	\	T$=T$+C9$+"("+T1$+")" &
		! RESET ERROR TRAP; &
		! IF SOME OUTSTANDING ERROR, MAKE SURE THIS &
		! JOB IS REMOVED FROM THE RECEIVER TABLE. &

22690	RETURN &
		! AND EXIT. &
	&

22700	&
	&
	&
	!	P R I N T    A    L I N E    I N T O    T H E &
	!		L I S T I N G    F I L E &

22710	GOSUB 22720 &
	\ GOSUB 23200 &
	\ GOTO 22730 &
		! 'USER ENTRY' : &
		! PRINT THE MESSAGE TO THE LOG FILE; &
		! MAKE SURE THAT THE PROMPT IS RE-PRINTED IF THE LIST &
		!	FILE IS THE KB:; &
		! EXIT. &

22719	&
	&
	&
	!	THIS IS THE 'INTERNAL ENTRY' : &
	!	IT CAN BE USED BY ROUTINES WHICH ARE SURE THAT THE NEXT &
	!	CALL AFTER THIS PRINT IS COMPLETED WILL BE ONE THAT &
	!	ENSURES THAT THE PROMPT IS RE-PRINTED (OR WHICH DO NOT &
	!	WANT THE PROMPT RE-PRINTED, EG, ABORT). &

22720	IF L0%>-1% THEN &
		ON ERROR GOTO 22750 &
	\	PRINT #L0% IF CCPOS(L0%)<>0% &
	\	PRINT #L0%,T$ &
	\	IF L0%=0% THEN &
			R%=R% OR NOT 32767% &
		! IF THERE IS A LISTING FILE, PRINT THE LINE INTO IT. &
		! MAY GET AN ERROR HERE. &
		! IF THE LISTING FILE IS ON CHANNEL 0, THEN IT MUST &
		! BE THE KB:, SO SET 'PROMPT REQUIRED' FLAG. &

22730	ON ERROR GOTO 19000 &
		! RESET ERROR TRAP. &

22740	RETURN &
		! AND EXIT. &
	&

22750	&
	&
	&
	!	ERROR TRAP &

22760	E%=ERR &
	\ RESUME 22770 &
		! SET ERROR NUMBER AND MAKE SURE OF RESUME. &

22770	GOSUB 22550 &
	\ IF L0%<>0% THEN &
		GOTO 22720 &
	  ELSE	GOTO 22730 &
		! GO PROCESS ERROR; &
		! IF NO MORE LISTING FILE, THEN GO EXIT; &
		! ELSE, GO TRY AGAIN. &
	&

22800	&
	&
	&
	!	C H E C K    F O R    A T T A C H E D &

22810	Z$=SYS(PRIV.ON$) &
	\ IF (R% AND 16384%)<>0% THEN &
		GOTO 22830 &
	  ELSE	IF (PEEK(PEEK(K0%)+2%) AND 255%)/2%<>J% OR &
				(PEEK(PEEK(K0%)+6%) AND 8192%)<>8192% &
				THEN &
			GOTO 22830 &
		ELSE	K%=(SWAP%(PEEK(PEEK(K0%)+2%)) AND 255%) &
	\		Z$=SYS(PRIV.OFF$) &
	\		GOSUB 22500 &
		! IF THE STATUS INDICATES THAT THE JOB IS ATTACHED, THEN &
		!	GO TO EXIT; &
		! ELSE	IF THE JOB REALLY IS NOT ATTACHED, ACCORDING TO &
		!		TO THE EXEC, THEN &
		!		GO TO EXIT; &
		!	ELSE	SET UP THE KB: # OF THE CURRENT KB:; &
		!		GO CHECK TO SEE IF THE LIST FILE SHOULD &
		!			BE OPENED OR CLOSED. &

22820	PRINT #0% IF CCPOS(0%)<>0% &
	\ PRINT #0%,"BACKUP ("+J$+") ATTACHED" &
	\ R%=(R% OR -16384%) &
	\ R0%(0%)=R0%(0%) OR 4% &
		! PRINT THE HEADER MESSAGE AND SET THE 'PROMPT REQUIRED' &
		! FLAG (ALSO SET THE 'ATTACHED' FLAG, FOR EASE); &
		! MAKE THE 'DETACH' COMMAND LEGAL (AT THIS POINT, IT &
		! MUST BE LEGAL - THE ONLY WAY THE PROGRAM FOUND OUT &
		! THAT IT WAS DETACHED WAS FROM A 'DETACH' COMMAND, SO &
		! IT WAS LEGAL ORIGINALLY). &

22830	Z$=SYS(PRIV.OFF$) &
	\ RETURN &
	! AND EXIT. &
	&

22900	&
	&
	&
	!	S T A T U S &
	&
	! THIS ROUTINE PRINTS OUT THE CURRENT STATE OF THE PROGRAM, &
	! USING THE TWO ARRAYS C%() AND C().  THE CONTENTS OF THESE &
	! ARRAYS ARE : &
	!	C%(0)	- UNUSED &
	!	C%(1)	- SUBSCRIPT OF THE VOLUME RECORD SHOWING THE &
	!		  CURRENT VOLUME BEING PROCESSED &
	!	C%(2)	- SUBSCRIPT OF THE CURRENT UFD RECORD &
	!	C%(3)	- SUBSCRIPT OF THE CURRENT FILE RECORD &
	!	C%(4)	- BLOCK OF THE CURRENT BLOCK WITHIN THE FILE &
	! &
	!	C(0)	- UNUSED &
	!	C(1)	- TOTAL NUMBER OF VOLUMES IN INPUT VOLUME LIST &
	!	C(2)	- TOTAL NUMBER OF ACCOUNTS SELECTED &
	!	C(3)	- TOTAL NUMBER OF FILES SELECTED &
	!	C(4)	- TOTAL NUMBER OF DATA BLOCKS IN SELECTED FILES &
	! &
	!	R$	- PHASE NAME &
	!	T0	- TIME OF DAY AT WHICH THIS PROGRAM BEGAN &
	!	T1	- CPU TIME FOR THIS JOB AT THE TIME THIS PROGRAM &
	!		  BEGAN &
	!	T3	- KCT'S FOR THIS JOB AT THE TIME THIS PROGRAM &
	!		  BEGAN. &

22920	T$=C9$+ &
	"PHASE	: "+R$+C9$ &
		! SET UP THE PHASE VALUE FOR THIS PROGRAM. &

22940	IF C(1%)>0. THEN &
		T$=T$+"VOLUME # : "+NUM1$(C(1%))+C9$ &
	\	IF C(2%)>0. THEN &
			T$=T$+"ACCOUNTS : "+NUM1$(C(2%))+C9$ &
	\		IF C(3%)>0. THEN &
				T$=T$+"FILES	 : "+NUM1$(C(3%))+C9$ &
	\			T$=T$+"BLOCKS	 : "+NUM1$(C(4%))+C9$ &
		! PRINT OUT THE STATUS INFORMATION. &

22950	T$=T$+"ERRORS	 : "+FNN$(E2%)+C9$+C9$ &
	\ T$=T$+"CURRENT VOLUME	: "+FNU$(C%(1%),"",4096%,0%)+ &
		C9$ IF C%(1%)<>0% &
	\ T$=T$+"CURRENT ACCOUNT	: "+FNU$(C%(2%),"",4224%,0%)+ &
		C9$ IF D%>1% AND C%(2%)<>0% &
	\ T$=T$+"CURRENT FILE	: "+FNU$(C%(3%),"",-1%,0%)+ &
		C9$ IF D%>2% AND C%(3%)<>0% &
	\ T$=T$+"CURRENT BLOCK	: "+FNN$(C%(4%))+C9$ IF &
			D%>3% &
		! PRINT OUT THE 'CURRENT' INFORMATION. &

22960	T0=T0-86400. IF TIME(0%)<T0 &
	\ T$=T$+C9$ &
	\ T$=T$+"ELAPSED TIME : "+NUM1$(TIME(0%)-T0)+" SECONDS"+ &
		C9$ &
	\ T$=T$+"CPU TIME     : "+NUM1$((TIME(1%)-T1)/10%)+" SECONDS"+ &
		C9$ &
	\ T$=T$+"KCTS         : "+NUM1$(TIME(3%)-T3)+C9$ &
	\ T$=T$+C9$ &

22990	RETURN &
	&

23000	&
	&
	&
	!	L O G     E R R O R &
	&
	! THIS ROUTINE PERFORMS THE FOLLOWING FUNCTIONS: &
	!	1. CREATES AN ERROR MESSAGE &
	!	2. DISPATCHES THE MESSAGE TO USER/OPERATOR &
	!	3. (CONDITIONALLY) ADDS THE ERROR TO THE LIST &
	!	    OF ERRORS ASSOCIATED WITH THE RECORD IN THE &
	!	    WORK-FILE POINTED TO BY P3%. &
	!	4. (CONDITIONALLY) PRINTS ERROR INTO THE LISTING &
	!	    FILE. &
	&
	! THE PARAMETERS USED ARE: &
	!	E%	- CONTAINING THE ERROR NUMBER OF THE ERROR &
	!		  TO LOG.  THIS NUMBER IS TRANSFORMED INTO &
	!		  A STRING BY THE FUNCTION FNE$(). &
	!		  E% MUST BE NON-ZERO UPON ENTRY. &
	! &
	!	D%	- DEPTH AT WHICH ERROR IS TO BE LOGGED &
	!		  (AFFECTS ONLY THE TEXT OF THE ERROR &
	!		  MESSAGE). &
	! &
	!	P3%	- RECORD CONTAINING DEVICE/ACCOUNT INFO. &
	!		   EG, IF ERROR OCCURRED WHILE SCANNING &
	!		   ACCOUNT DP0:[1,201], THEN THE RECORD &
	!		   POINTED TO BY P3% SHOULD CONTAIN THE &
	!		   INFORMATION. &
	! &
	!	P0%	- RECORD DESCRIBING ENTITY WHICH CAUSED &
	!		   ERROR;  EG, IF ERROR OCCURRED ON AN &
	!		   ACCOUNT, THEN P0% MUST POINT TO THE UR &
	!		   FOR THE ACCOUNT. &
	! &
	!	E1%	- RECORD NUMBER IN MFD/UFD/FILE AT WHICH ERROR &
	!		  OCCURRED (USED ONLY IN BAD BLOCK ERRORS). &
	! &
	!	R1%	- ACTION TO TAKE WITH THE ERROR.  IF NON-ZERO, &
	!		  THE ERROR TEXT WILL CONTAIN A LIST OF &
	!		  LEGAL RESPONSES. &
	! &
	!	L0%	- LIST FILE CHANNEL # - IF 0, THEN NO LIST &
	!		  FILE ACTIVITY TAKES PLACE. &
	! &
	!	C%(D%)	- POINTS TO THE RECORD TO WHICH THE ERROR &
	!		   SHOULD BE LOGGED. &

23010	T$=FNE$(E%,D%,P3%,P0%,E1%) &
		! SET UP ERROR MESSAGE STRING FROM THE PARAMETERS. &

23020	T$=T$+C9$+"(ON " &
	\ T$=T$+"OPEN)" IF E3%=1% &
	\ T$=T$+"NAME-BLOCK)" IF E3%=2% &
	\ T$=T$+"ACCOUNTING-BLOCK)" IF E3%=3% OR E3%=4% &
	\ T$=T$+"ATTRIBUTE-BLOCK)" IF E3%=5% &
	\ T$=T$+"CLOSE)" IF E3%=6% &
	\ T$=T$+"TRANSFER)" IF E3%=7% &
	\ T$=T$+"COMPARE)" IF E3%=8% &
	\ T$=T$+"DELETE)" IF E3%=9% &
		! APPEND (PROGRAM DEPENDANT) MODIFIERS, IF REQUIRED. &

23030	L$=T$ &
	\ S%=S%+1% \ S%(S%)=E1% &
	\ S%=S%+1% \ S%(S%)=E3% &
	\ S%=S%+1% \ S%(S%)=E% &
	\ IF (R1% AND (32%+64%+512%))<>0% THEN &
		T$=T$+C9$+"( " &
	\	T$=T$+"RETRY " IF (R1% AND 32%)<>0% &
	\	T$=T$+"SKIP " IF (R1% AND 64%)<>0% &
	\	T$=T$+"IGNORE " IF (R1% AND 512%)<>0% &
	\	T$=T$+"LEGAL)" &
		! IF SOME RESPONSE IS REQUESTED, THEN &
		! TELL THE USER WHAT IS LEGAL. &

23040	GOSUB 23100 &
	\ GOSUB 23200 &
		! GO NOTIFY THE USER OF THE ERROR; &
		! CHECK FOR A RESPONSE, EVEN IF ONE IS NOT REQUIRED. &

23050	E%=S%(S%) \ S%=S%-1% &
	\ E3%=S%(S%) \ S%=S%-1% &
	\ E1%=S%(S%) \ S%=S%-1% &
	\ IF (R2% AND 32%)<>0% THEN &
		GOTO 23090 &
	  ELSE	S%=S%+1% \ S%(S%)=P0% &
	\	P0%=C%(D%) &
		! POP THE STUFF OFF THE STACK; &
		! IF THE RESPONSE WAS 'RETRY', THEN &
		!	DO NOT LOG THE ERROR OR &
		!	ISSUE A MESSAGE TO THE LIST &
		!	FILE. &

23060	P1%,P0%=FNP0%(P0%,3%,0%) &
	\ P0%=Z0%(P0%,1%) &
	\ WHILE P0%<>0% AND (E%<Z0%(P0%,6%) OR (Z0%(P0%,7%)>25%)) &
	\	P1%=P0% &
	\	P0%=Z0%(P0%,1%) &
	\ NEXT &
	\ IF P0%=0% OR E%<>Z0%(P0%,6%) THEN &
		S%=S%+1% \ S%(S%)=Z0%(P1%,1%) &
	\	P0%,Z0%(P1%,1%)=FNA% &
	\	Z0%(P0%,1%)=S%(S%) \ S%=S%-1% &
	\	Z0%(P0%,6%)=E% &
	\	Z0%(P0%,7%)=7% &
		! SET UP THE ERROR LIST FOR THE RECORD IN ERROR; &
		! FIND THE RECORD IN THE ERROR LIST WHICH MEETS &
		! THE FOLLOWING QUALIFICATIONS: &
		!	1 - SAME ERROR NUMBER AS THIS ERROR; &
		!	2 - ROOM FOR MORE ERROR ENTRIES IN IT. &
		! IF SUCH A RECORD CAN NOT BE FOUND, THEN &
		!	SET UP A NEW RECORD IN THE ERROR LIST, &
		!	AND LINK IT IN AFTER THE PREVIOUS RECORD; &
		!	(NOTE THAT THE ERROR RECORDS ARE KEPT IN &
		!	ASCENDING ORDER BY ERROR NUMBER.) &
	&

23070	Z%=Z0%(P0%,7%)+1% &
	\ Z0%(P0%,Z%)=E1% &
	\ Z0%(P0%,Z%+1%)=Z0%(P3%,6%) &
	\ Z0%(P0%,Z%+2%)=Z0%(P3%,7%) &
	\ Z0%(P0%,Z%+3%)=Z0%(P3%,8%) &
	\ Z0%(P0%,7%)=Z%+3% &
	\ P0%=FNP1% &
	\ E2%=E2%+1% &
	\ P0%=S%(S%) \ S%=S%-1% &
		! PUT THE DATA FOR THIS ERROR INTO THE SLOT IN THE &
		! FOLLOWING MANNER: &
		!	RECORD NUMBER (MAY BE 0) &
		!	DEVICE NAME; &
		!	ACCOUNT #; &
		! UPDATE THE ENTRY COUNT FOR THIS ERROR RECORD. &
	&

23080	IF L0%<>0% THEN &
		T$=L$ &
	\	GOSUB 22700 &
		! IF THE LIST FILE IS OPEN, THEN &
		!	SET UP TO PRINT A MESSAGE TO &
		!	IT AND GO DO IT. &

23090	E%,E1%=0% \ RETURN &
		! AND EXIT. &
	&

23100	&
	&
	&
	!	I S S U E    A    M E S S A G E &
	&
	! This routine does a small amount of message reformatting and &
	! dispatches the message to, either, the user's KB:, if &
	! ATTACHed, or to OPSER.  It also stores the current outstanding &
	! message dispatched, if no requests are outstanding, in case &
	! the package is later requested to re-print it (LAST command). &
	&
	! If some response is requested, the message is reformatted with &
	! a '%' as the first character. &
	&
	! If one of the legal responses is 'OTHER', then, if the &
	! request is printed on the KB:, no CR-LF is printed (and the &
	! 'CR-LF PROMPT INHIBIT' flag is left set). &
	&
	! The routine expects the following data : &
	!	T$	- Message to dispatch &
	!	R1%,R0%(2%) &
	!		- Mask word showing legal responses &
	!	R%	- Flags as follows : &
	!		16384	- ATTACHed flag &
	!			  If set, message is printed on KB:; &
	!			  Otherwise, a check is made to see that &
	!			  the program is really DETACHed; &
	!			  If so, the message is sent to OPSER; &
	!			  Otherwise, the message is printed. &
	!		4096	- ONLINE flag &
	!			  If set and the job is flagged as &
	!			  DETACHed, a check is made for possible &
	!			  recent ATTACH; &
	!			  If not set, and the job is flagged as &
	!			  DETACHed, the message is sent to OPSER &
	!			  without a check for ATTACHed (this is &
	!			  the way the 'CHECK FOR DETACH' routine &
	!			  sends OPSER an ONLINE). &
	!		1024	- MESSAGE STORED flag &
	!			  If set, the routine will not replace &
	!			  the message  held in R1$ with the new &
	!			  message coming in; &
	!			  If not set, the routine will replace &
	!			  the text in R1$ with the new text. &
	&
	! The routine returns the following data : &
	!	T	- The time at which the routine was called (used &
	!		  for timeout conditions for re-prompting the &
	!		  operator). &
	!	R1$	- The text of the current outstanding request, &
	!		  or, if none, the text of the last message &
	!		  passed. &
	!	R%	- Flags as follows : &
	!		-32768	- 'PROMPT REQUIRED' flag is always set; &
	!		 16384	- Set if the job was found to be &
	!			  ATTACHed; &
	!		 4096	- Set if the job successfully sent the &
	!			  message to OPSER. &
	&
	! Other variables used : &
	!	T1$	- temporary storage of the string to dispatch &
	!		  (Used when the string is to be sent to OPSER.) &
	!	R8%	- Retry counter used to determine how many times &
	!		  a send to OPSER has been attempted. &

23105	IF R1%<>0% THEN &
		R0%(2%)=R1% &
	\	R1%=0% &
		! THIS IS IN TO ALLOW MINIMUM CHANGE TO BACDIR. &

23110	T1$=T$ &
	\ R8%=0% &
	\ R%=R% AND -513% &
	\ IF (R% AND 1024%)=0% THEN &
		R1$=T$ &
	\	IF R0%(2%)<>0% THEN &
			R%=R% OR 1024% &
	\		R%=R% OR 512% IF (R0%(2%) AND 1024%)<>0% &
	\		R1$,T$=C8$+T$ &
		! HOLD THE MESSAGE IN A TEMP LOCATION; &
		! SET THE TIME TO RIGHT NOW, TO AVOID UNNECESSARY &
		!	MESSAGES; &
		! SET THE RETRY COUNTER TO 0; &
		! RESET THE 'CR-LF INHIBIT' FLAG, SINCE THIS IS A NEW &
		!	MESSAGE; &
		! IF A MESSAGE IS NOT ALREADY STORED FOR &
		! THE 'LAST MESSAGE' COMMAND, THEN &
		!	STORE THIS ONE; &
		!	IF THIS ONE REQUIRES A RESPONSE, THEN &
		!		INHIBIT FURTHER STORING OF &
		!		MESSAGES UNTIL THIS ONE IS ANSWERED; &
		!		INHIBIT CR-LF AFTER THIS MESSAGE, FOR &
		!		THIS ONE ONLY, IF THE RESPONSE CAN BE &
		!		'OTHER'; &
		!		SET UP THE WARNING FLAG (A <BELL>) ON &
		!		 THE MESSAGE. &

23120	GOSUB 22800 IF (R% AND 16384%+4096%)=4096% &
	\ IF (R% AND 16384%)=0% THEN &
		ON ERROR GOTO 23150 &
	\	WHILE LEN(T1$)<>0% &
	\		Z%=LEN(T1$)+1% &
	\		Z%=255% IF Z%>20% &
	\		Z$=SYS(PRIV.ON$) &
	\		Z$=SYS(T0$+CHR$(Z%)+T1$) &
	\		Z$=SYS(PRIV.OFF$) &
	\		R8%=0% &
	\		T1$=RIGHT(T1$,20%) &
	\	NEXT &
	\	R%=R% OR 4096% &
		! IF NOT ATTACHED, THEN GO SEE IF THE PROGRAM HAS BEEN &
		! ATTACHED RECENTLY; &
		! IF STILL NOT ATTACHED, THEN &
		!	SEND MESSAGE, 19 BYTES AT A TIME, TO &
		!		OPSER; &
		!	IF SEND WAS SUCCESSFUL, THEN SET 'OPSER &
		!		ONLINE' FLAG. &

23130	IF (R% AND 16384%)<>0% THEN &
		ON ERROR GOTO 19000 &
	\	PRINT #0% IF CCPOS(0%)<>0% &
	\	PRINT #0%,T$; &
	\	PRINT #0% IF (R% AND 512%)=0% &
		! IF ATTACHED, THEN &
		!	PRINT TEXT (WITHOUT CR-LF); &
		!	PRINT A CR-LF IF THE INHIBIT FLAG IS NOT SET. &

23140	R%=R% OR NOT 32767% &
	\ ON ERROR GOTO 19000 &
	\ RETURN &
		! SET THE 'PROMPT NEEDED' FLAG, SINCE SOMETHING WAS &
		!	JUST PRINTED OUT/SENT TO OPSER; &
		! RESET THE ERROR TRAP, LIKE A GOOD SUBROUTINE; &
		! AND EXIT. &

23150	&
	&
	&
	!	ERROR TRAP &

23160	RESUME 23170 &

23170	ON ERROR GOTO 19000 &
	\ Z$=SYS(PRIV.OFF$) &
	\ IF ERR=32% THEN &
		R8%=R8%+1% &
	\	IF R8%<5% THEN &
			GOTO 23190 &
		! IF THE ERROR IS 'NO ROOM' FOR MESSAGE, THEN &
		! IF THE RETRY COUNT IS LESS THAN FIVE, THEN &
		!	SLEEP AND RETRY. &

23180	IF R8%<10% THEN &
		R8%=10% &
	\	Z$=SYS(PRIV.ON$) &
	\	Z$=SYS(CHR$(6%)+CHR$(-5%)+CHR$(0%)+C9$+ &
			"***** BACKUP"+A$+"  JOB "+J$+" - OPSER HUNG"+ &
			C9$+C9$) &
	\	Z$=SYS(PRIV.OFF$) &
	\	GOTO 23140 &
		! IF ANY OTHER ERROR, SEND NOTIFICATION TO THE CONSOLE &
		! KB: (KB0:) THAT OPSER IS HUNG; &
		! ASSUME THAT THE MESSAGE WILL BE ASKED FOR LATER. &

23190	SLEEP 5% &
	\ GOTO 23120 &
		! DO A SLEEP AND GO RETRY. &
	&
	&
	&
	&

23200	&
	&
	&
	!	G E T / D E C O D E    R E S P O N S E    O R &
	!	    I N T E R R U P T    C O M M A N D &
	&
	! This routine polls both the user's keyboard and the &
	! Send/Receive table, if the job has entered itself, to get any &
	! input waiting for it.  If any input is found,even a null line, &
	! it is passed to the 'DECODE A LINE' routine, see below, to &
	! decode it and set flags in the appropriate status words, &
	! R%(0%), R%(1%), and R%(2%). &
	&
	! After the KB: and receive table have been polled, and &
	! whatever input there might have been has been decoded, the &
	! routine checkS the base state (R%(0%)) for any processes &
	! which may have been requested.  If any have been which &
	! require some action by the routine, it dispatches to the &
	! proper dispatch line number. &
	&
	! After all processes have been done, the following  decisions &
	! are made : &
	!	1) Has the program been in an inactive state (in a &
	!	  'PAUSE', waiting for a response, etc.) for more than &
	!	  60 seconds? &
	!	  If so, dispatch a reminder message. &
	!	2) Has some input or output been performed since the &
	!	  last prompt was printed, and, if so, is the program in &
	!	  a state in which a prompt should be printed? &
	!	  If so, go back to the beginning of the routine -  get &
	!	  the prompt printed. &
	!	3) Is the program in a state which requires further &
	!	  input, eg, a 'PAUSE' state, or waiting for a &
	!	  response? &
	!	  If so, SLEEP for 60 seconds and go retry the input &
	!	  phase. &
	&
	! Finally, after these decisions/actions, the routine sets up &
	! some returnable variables and exits. &
	&
	! The routine expects the following variables : &
	!	R%	- The following flags : &
	!		16	-'PAUSE' state &
	!			  If set, the routine does not exit &
	!			  until a 'CONTINUE' command is &
	!			  entered.  Set by the 'PAUSE' command. &
	!		2048	-'Entered to receive table' flag &
	!			  If set, the program has an entry in &
	!			  the receive table, and the routine &
	!			  will attempt to receive data, if none &
	!			  is available at the KB:. &
	!		16384	- 'ATTACHed' flag &
	!			  If set, the KB: is checked for input &
	!			  the receive table is checked.  Note &
	!			  that the KB: has higher precedence &
	!			  than the receive table - all KB: input &
	!			  (immediately available) is processed &
	!			  before the receive table is polled. &
	!		-32768	- 'PROMPT Required' flag &
	!			  If set, the routine will be &
	!			  re-executed, thus causing a prompt, &
	!			  except in the cases outlined in (2), &
	!			  above. &
	!	R0%(2%)	- 'Responses Requested" word - used in decision &
	!		  (3), above. &
	!	J0%	- Queue address word - this must be the &
	!		  address in the receive table of the count of &
	!		  messages queued for this job.  The high byte &
	!		  of PEEK(J0%) is the desired quantity. &
	!	D%(,)	- Dispatch/decode table - D%(X,2%) must contain &
	!		  the subroutine number for Interrupt Command &
	!		  #X.  D%(X,0) must contain the status word &
	!		  index for command #X.  D%(X,1) must contain &
	!		  the flag value for Command #X.  Used by the &
	!		  dispatcher. &
	!	R0%(0)	- 'Internal Interrupt Commands Permission' word; &
	!		  See the variable descriptions in program's &
	!		  preface.  Only commands whose values are &
	!		  specified in this word are legal. &
	!	R%(0)	- 'Internal Interrupt Commands Requested' word; &
	!		  See the variable descriptions in the program's &
	!		  preface. &
	!	R%(2)	- 'Response' word; &
	!		  See the variable descriptions in the program's &
	!		  preface. &
	&
	! The routine returns the following data : &
	!	R%(0)	- 	] &
	!	R%(1)	-	] values of the response(s) given &
	!	R%(2)	-	] &
	!	R2%	- Same value as R%(2) &
	!	R0%(2)	- 0 &
	!	C$	- String entered as a response to an 'OTHER' &
	!		  request. &
	&
	! The routine effects the following other variables : &
	!	R4%	- used as a pointer into the decode/dispatch &
	!		  table; &
	!	C0$	- temporary storage of 'OTHER' string; &
	!	E%	- error value of an unexpected error &
	!	Z$,Z%	- used in the receive a string routine &
	!	T2$	- used in the receive a string routine &
	!	T	- timer for reminder messages &
	!	T$	- text string for reminder messages and &
	!		  Interrupt Command response text. &

23210	IF (R% AND 16384%)=0% THEN &
		GOSUB 22800 &
	\	R4%=0% &
	\	GOSUB 23570 IF (R% AND 17408%)=17408% &
		! IF THE 'ATTACHED' FLAG IS NOT SET, THEN GO SEE IF &
		! RECENTLY ATTACHED.  IF SO, THEN IF SOME MESSAGE WAS &
		! JUST ISSUED, ISSUE IT AGAIN, SO THAT THE USER SEES IT. &
	&
	&
	&

23250	&
	&
	&
	!	G E T    A    L I N E &
	&
	!	T R Y    K E Y B O A R D &

23260	R4%=0% &
	\ R%(2%)=0% IF R0%(2%)<>0% &
	\ IF (R% AND 16384%)=0% THEN &
		GOTO 23300 &
	ELSE	IF R%<0% AND (R% AND 512%)=0% THEN &
			PRINT #0% IF CCPOS(0%)<>0% &
	\		PRINT #0%,"*"; &
	\		R%=R% AND 32767% &
		! IF NOT ATTACHED AT THIS POINT, THEN &
		!	GO SEE IF THIS JOB IS RECEIVING; &
		! ELSE, IF A PROMPT IS NEEDED, THEN &
		!	ISSUE IT IF THE 'CRLF INHIBIT' FLAG IS NOT SET. &

23270	ON ERROR GOTO 23280 &
	\ W%=0% &
	\ W%=120% IF R0%(2%) &
	\ WAIT W% &
	\ GET #0%,RECORD (8192% AND (W%=0%)) &
	\ WAIT 0% &
	\ R%=(R% AND -129%) OR -16384% &
	\ FIELD #0%, RECOUNT AS C$ &
	\ C$=C$+NL$ &
	\ PRINT #0% IF CCPOS(0%)<>0% &
	\ GOTO 23340 &
		! SET UP ERROR TRAP; &
		! TRY (IMMEDIATE RETURN) LINE FROM THE KB:; &
		! SET 'PROMPT REQUIRED' AND 'ATTACHED' FLAGS; &
		! FIELD RESULTING STRING. &

23280	IF ERR=13% OR ERR=11% OR ERR=15% THEN &
		RESUME 23300 &
		! IF THE ERROR MEANT SIMPLY, NO DATA, OR IF IT WAS &
		! 'EOF', THEN SEE IF A MESSAGE IS QUEUED FOR THIS JOB. &

23290	E%=ERR &
	\ RESUME 19000 &
		! ALL OTHER ERRORS ARE FATAL. &

23300	&
	&
	&
	!	R E C E I V E    A    M E S S A G E &

23310	C$="" &
	\ R%=R% AND 32767% &
	\ Z$=SYS(PRIV.ON$) &
	\ IF (R% AND 2048%)=0% THEN &
		GOTO 23350 &
	  ELSE	IF (PEEK(J0%) AND -256%)=0% THEN &
			GOTO 23350 &
		ELSE	ON ERROR GOTO 23320 &
	\		Z$=SYS(CHR$(6%)+CHR$(18%)+CHR$(1%)) &
	\		Z%=ASCII(RIGHT(Z$,9%)) &
	\		Z$=RIGHT(Z$,10%) &
	\		T2$=T2$+Z$ &
	\		IF LEN(Z$)<Z% THEN &
				GOTO 23300 &
			ELSE	C$=T2$ &
	\			T2$="" &
	\			R%=(R% AND -129%) OR -30720% &
	\			Z$=SYS(PRIV.OFF$) &
	\			GOTO 23340 &
		! NULL OUT THE STRING AND CLEAR THE 'PROMPT REQUIRED' &
		! FLAG TO BEGIN WITH; &
		! IF NOT IN RECEIVE TABLE, THEN DON'T TRY &
		! TO RECEIVE A MESSAGE; &
		!	ELSE	IF NO MESSAGES QUEUED FOR &
		!		THIS JOB, THEN DON'T TRY TO RECEIVE; &
		!		ELSE	SET UP RECEIVE, AND TAKE ONLY &
		!			THE TEXT PART; &
		!			IF THE MESSAGE IS NOT COMPLETE, &
		!			THEN	SAVE THE PARTIAL; &
		!			ELSE	RETURN COMPLETED &
		!				MESSAGE; &
		!				NULL OUT THE PARTIAL; &
		!				SET 'PROMPT REQUIRED' &
		!				FLAG. &

23320	IF ERR=5% THEN &
		RESUME 23300 &
	ELSE	RESUME 23350 &
		! IF THE ERROR IS 'NO MORE FOR YOU', THEN &
		!	GO TRY AGAIN, BECAUSE MORE MAY HAVE BEEN &
		!		ENTERED; &
		! ELSE	DON'T WORRY ABOUT THE ERROR. &

23340	ON ERROR GOTO 19000 &
	\ GOSUB 23400 &
		! THE ROUTINES ENTER HERE IF SOME INPUT WAS RECEIVED, &
		! AND THIS LINE DISPATCHES TO DECODE IT.  THE ROUTINES &
		! MUST NEVER COME HERE IF NO INPUT WAS GOTTEN. &

23350	ON ERROR GOTO 19000 &
	\ Z$=SYS(PRIV.OFF$) &
	\ ON D%(R4%,2%) GOSUB &
	    23500,23520,23550,23560,23570,23580,23600 &
		IF (R%(0%) AND (D%(R4%,1%) AND D%(R4%,0%)=0%))<>0% IF &
			D%(R4%,2%)<>0% FOR R4%=1% UNTIL &
				R%(0%)=0% OR R4%=D%(0%,0%) IF &
					R%(0%)<>0% &
		! DISPATCH TO ANY NECESSARY PROCESSES, IF ANY ARE &
		! CURRENTLY OUTSTANDING; &
		! THE PROCESSES FOR THIS PACKAGE ARE : &
		!	ROUTINE	#	NAME &
		!		1	CONTINUE &
		!		2	DETACH &
		!		3	PAUSE &
		!		4	STATUS &
		!		5	LAST &
		!		6	LEGAL &
		!		7	NOTICE &

23360	IF R%<0% THEN &
		IF R%(2%)<>1024% OR R0%(2%)<>0% OR (R% AND 16%)<>0% &
		THEN &
			GOTO 23200 &
		! IF THE 'PROMPT REQUIRED' FLAG IS SET, THEN &
		!	IF THE ROUTINE IS NOT ABOUT TO RETURN AN &
		!		'OTHER' RESPONSE, THEN &
		!		GO RE-ASK, IN ORDER TO GET THE &
		!		PROMPT PRINTED. &

23370	IF (R% AND 16%) OR R0%(2%) THEN &
		T=TIME(0%) &
	\	SLEEP 120% &
	\	GOTO 23200 IF (R% AND 128%) OR (TIME(0%)-T<120.) &
	\	T$="(FURTHER RESPONSE NECESSARY)" &
	\	T$="(IN PAUSE)" IF R% AND 16% &
	\	GOSUB 23100 &
	\	R%=R% OR 128% &
	\	GOTO 23200 &
		! IF THE PROGRAM IS IN 'PAUSE' STATE OR SOME &
		! FURTHER INPUT IS REQUIRED, THEN &
		!	SLEEP AWHILE IF IN PAUSE; &
		!	PRINT MESSAGE IF NOT PROHIBITED; &
		!	GO CHECK FOR FURTHER INPUT. &

23380	C$=C0$ IF R%(2%)=1024% &
		! IF THE RESPONSE IS 'OTHER', THEN RETURN THE &
		! STORED RESPONSE STRING IN C$. &

23390	R%=R% OR (R%(1%) AND 1%+8%+256%) &
	\ R2%=R%(2%) &
	\ RETURN &
		! SET UP THE PROGRAM STATUS, AND THE RETURN VALUE, FOR &
		! THE CONVENIENCE OF BACKUP; &
		! AND EXIT. &
	&

23400	&
	&
	&
	!	D E C O D E    A    L I N E &
	&
	! The function of this routine is to decode the text entered in &
	! the previous routine and to set the flags corresponding to &
	! the command in the appropriate 'Request' word, R%(0), R%(1), &
	! or R%(2).  Incidental to this main function are the resetting &
	! of timers, issuing of 'can't decode' error messages, etc. &
	&
	! The routine expects the following data : &
	!	C$	- input string &
	!	R0%(0)	-	] &
	!	R0%(1)	-	] permissible commands for the three &
	!	R0%(2)	-	] levels &
	!	D%(X,Y),D$(X)	- Decode tables &
	&
	! The routine returns the following : &
	!	C0$	- Response string if it was an 'OTHER' response &
	!	T	- Timer decremented by 15 seconds. &
	!	R%(0)	-	] &
	!	R%(1)	-	] These values with the appropriate &
	!	R%(2)	-	] flags set, see program preface. &
	&

23410	R3%=0% &
	\ C$=CVT$$(C$,4%+8%+16%+32%+128%) &
	\ IF C$<>"" THEN &
		GOTO 23420 &
	  ELSE	IF (R0%(2%) AND 1024%)=0% THEN &
			GOTO 23480 &
		ELSE	R4%=2% &
	\		R6%=0% &
	\		R3%=1024% &
	\		GOTO 23480 &
		! SET THE TIMER BACK 15 SECONDS, SO THE PROMPT MESSAGE &
		! WILL BE PRINTED SOONER; &
		! SET THE RESPONSE VALUE TO ZERO, IN CASE NOTHING IN &
		! HERE SETS IT; &
		! IF THE STRING IS NOT NULL, THEN &
		!	GO PROCESS; &
		! ELSE	IF AN 'OTHER' RESPONSE IS NOT ALLOWED, THEN &
		!		EXIT QUICKLY; &
		!	ELSE	SET UP THE RESPONSE'S LEVEL; &
		!		RESET THE ROUTINE NUMBER, SO NO &
		!		CALL IS MADE; &
		!		SET THE RESPONSE TO 'OTHER'; &
		!		AND GO TO THE CLOSE UP. &

23420	IF (R% AND 8192%)=0% THEN &
		RESTORE \ Z$="" \ Z%=0% &
	\	READ Z$ UNTIL Z$="**DECOP" &
	\	READ Z$ &
	\	WHILE Z$<>"**END" &
	\		Z%=Z%+1% &
	\		D$(Z%)=Z$ &
	\		READ D%(Z%,0%),D%(Z%,1%),D%(Z%,2%) &
	\		READ Z$ &
	\	NEXT &
	\	D%(0%,0%)=Z% \ R%=R% OR 8192% &
		! IF THE DECODE TABLE HAS NOT YET BEEN BUILT, THEN &
		!	BUILD IT. &

23430	Z%,R4%=0% &
	\ WHILE Z%=0% AND R4%<D%(0%,0%) &
	\	R4%=R4%+1% &
	\	P%=0% &
	\	Z%=FNR%(D$(R4%),3%) &
	\ NEXT &
	\ C$=RIGHT(C$,Z%+1%) &
	\ R3%=D%(R4%,1%) &
	\ R6%=D%(R4%,2%) &
	\ R4%=D%(R4%,0%) &
	\ R3%=R3% AND R0%(R4%) &
	\ IF R3%=0% THEN &
		T$="UNRECOGNIZED COMMAND" IF Z%=0% &
	\	T$="ILLEGAL COMMAND" IF Z%<>0% &
	\	GOSUB 23100 &
	\	R3%=0% &
	\	GOTO 23490 &
		! SET THE CHARACTER POINTER AND THE COMMAND POINTER &
		! TO 0; &
		! SCAN THE COMMAND TABLE TO FIND THE ONE ENTERED. &
	&
		! SET THE VALUE FLAG TO THE FLAG VALUE FOR THIS COMMAND &
		!	IF THIS COMMAND IS LEGAL IN THIS CONTEXT; &
	&
		! IF THE OPERAND VALUE IS RETURNED AS ZERO, THEN &
		!	NO LEGAL COMMAND (INCLUDING 'OTHER') WAS &
		!	FOUND, SO PRINT AN ERROR: &
		!		IF JUST PLAIN DOESN'T MATCH, THEN &
		!			SAY 'UNRECOGNIZABLE'; &
		!		OTHERWISE, MUST HAVE BEEN A REAL &
		!			COMMAND WHICH IS NOT LEGAL &
		!			AT THIS POINT. &
	&

23480	IF R3%<>0% THEN &
		R%(R4%)=R%(R4%) OR R3% &
	\	IF R4%<>2% OR (R0%(2%) AND R3%)=0% THEN &
			R0%(2%)=0% IF R%(1%)<>0% &
		ELSE	R0%(2%)=0% &
	\		R%=R% AND NOT(512% OR 1024%) &
	\		IF R3%=1024% THEN &
				C0$=C$ &
	\			Z%=ASCII(C0$) &
	\			IF Z%=ASCII("'") OR Z%=ASCII('"') THEN &
					IF ASCII(RIGHT(C0$,LEN(C0$)))= &
					Z% THEN &
						C0$=RIGHT(LEFT(C0$, &
							LEN(C0$)-1%),2%) &
		! IF THE RESPONSE WAS SOME COMMAND (INCLUDING 'OTHER'), &
		! THEN &
		!	SET WHATEVER VALUES IN THE RESPONSE CORRESPOND &
		!	TO VALUES IN THE OVERALL LEGAL WORD INTO THE &
		!	STATUS WORD; &
		!	IF THE RESPONSE WAS A PURELY INTERRUPT COMMAND, &
		!	IE, IT DID NOT RESPOND TO A PROGRAM REQUEST, &
		!	THEN &
		!		RESET THE 'RESPONSE REQUESTED' FLAG IF &
		!		THE COMMAND WAS 'ABORT'; &
		!	ELSE	SET THE RESPONSE TO THE ENTERRED VALUE; &
		!		RESET THE RESPONSE REQUESTED FLAG; &
		!		RESET THE FLAGS IN THE STATUS WORD WHICH &
		!		RELATE TO THIS OUTSTANDING REQUEST; &
		!		IF THE RESPONSE WAS 'OTHER', THEN &
		!			STORE THE RESPONSE STRING FOR &
		!			SAFE-KEEPING; &
		!			IF THE FIRST CHARACTER OF THE &
		!			STRING IS A QUOTE, AND THE LAST &
		!			CHARACTER IS A MATCHING QUOTE, &
		!			THEN	REMOVE THE QUOTES. &

23490	RETURN &
		! AND EXIT. &
	&

23499	!	P R O C E S S    R E Q U E S T S &
	&
	! The following routines are the process routines for the &
	! Interrupt Commands.  None of them may effect any change to &
	! R4%.  They may effect R%, R0%(), R%(), and T$.  After &
	! execution of the appropriate routine, the flag in R%(0%) which &
	! caused the execution is reset, so the routine will not be &
	! executed again until some external process causes the flag to &
	! be set again. &

23500	R0%(0%)=(R0%(0%) AND -3%) OR 16% &
	\ R%=R% AND -145% &
	\ GOTO 23780 &
		! 'CONTINUE' COMMAND : &
		! RESET THE 'CONTINUE LEGAL' FLAG, SET THE &
		! 'PAUSE LEGAL' FLAG, RESET THE 'PAUSE IN EFFECT' FLAG; &
		! SET THE TIME TO 0, SO THE USER IS IMMEDIATELY TOLD &
		! OF ANY FURTHER RESPONSE NEEDED; &
		! AND EXIT. &

23520	IF R0%(2%)<>0% THEN &
		T$="CAN'T DETACH" &
	\	GOSUB 23100 &
	\	R%=R% AND -129% &
	\	GOTO 23780 &
		! 'DETACH' COMMAND : &
		! IF A QUESTION NEEDS ANSWERING, THEN &
		!	DON'T ALLOW DETACH. &

23530	T$="CAN'T DETACH" &
	\ GOSUB 22600 &
	\ IF E%<>0% THEN &
		E%=0% &
	\	GOSUB 23100 &
	\	GOTO 23780 &
		! CALL THE SET UP FOR DETACH ROUTINE; &
		! IF ANY ERROR RETURNED, THEN ISSUE ERROR &
		! MESSAGE. &

23540	GOSUB 23500 &
	\ PRINT #0%,"DETACHING..."+C9$+CHR$(12%) &
	\ Z$=SYS(PRIV.ON$) &
	\ Z$=SYS(CHR$(6%)+CHR$(7%)+CHR$(128%)) &
	\ Z$=SYS(PRIV.OFF$) &
	\ R0%(0%)=R0%(0%) AND -5% &
	\ GOTO 23780 &
		! DO A 'CONTINUE' OPERATION : &
		! PRINT INFORMATION MESSAGE AND DO THE DETACH; &
		! RESET THE 'DETACHED LEGAL' FLAG. &

23550	R0%(0%)=(R0%(0%) OR 2%) AND -17% &
	\ R%=R% OR 16% &
	\ T$="PAUSING . . ." &
	\ GOSUB 23100 &
	\ GOTO 23780 &
		! 'PAUSE' COMMAND : &
		! MAKE THE 'CONTINUE' COMMAND LEGAL, THE 'PAUSE' COMMAND &
		!	ILLEGAL; &
		! GO ISSUE 'PAUSE' MESSAGE; &
		! RESET TIME TO 0 SO THE USER IS IMMEDIATELY &
		!	INFORMED; &
		! AND EXIT. &

23560	GOSUB 22900 &
	\ GOSUB 23100 &
	\ GOTO 23780 &
		! 'STATUS' COMMAND : &
		! GO SET UP STATUS STRING; &
		! GO PRINT IT TO THE PROPER PLACE; &
		! AND EXIT. &

23570	T$="LAST MESSAGE WAS:"+C9$+R1$ &
	\ R%=R% AND -1537% IF (R% AND 1024%)<>0% &
	\ R2$=R1$ &
	\ GOSUB 23100 &
	\ R1$=R2$ &
	\ GOTO 23780 &
		! 'LAST' COMMAND : &
		! SET UP THE TEXT OF THE LAST MESSAGE; &
		! RESET THE 'STORING UNANSWERED REQUEST' FLAG AND &
		!	'NO CR-LF' FLAG, IF THE RESPONSE IS STORED; &
		! GO PRINT IT TO THE PROPER PLACE; &
		! AND EXIT. &

23580	IF (R0%(0%) OR R0%(1%) OR R0%(2%))=0% THEN &
		T$="<NO LEGAL COMMANDS>" &
	  ELSE	T$="LEGAL COMMANDS FOR THIS PHASE/QUESTION ARE:"+ &
			C9$ &
	\	T$=T$+D$(Z0%)+C9$ IF &
			(R0%(D%(Z0%,0%)) AND D%(Z0%,1%))<>0% FOR &
				Z0%=1% TO D%(0%,0%) &
		! 'LEGAL' COMMAND : &
		! SET UP A FLAG WORD FOR THE LEGAL COMMANDS; &
		! IF THE WORD IS NON-ZERO, THEN &
		!	SET UP A HEADER; &
		!	INCLUDE THE COMMAND NAME FROM THE COMMAND NAME &
		!	TABLE FOR EACH COMMAND WHICH IS LEGAL. &

23590	GOSUB 23100 &
	\ GOTO 23780 &
		! PRINT THE RESPONSE IN THE PROPER PLACE; &
		! EXIT. &

23600	IF L0%<0% THEN &
		T$="'NOTICE' ILLEGAL - NO LIST FILE" &
	\	GOSUB 23100 &
	\	GOTO 23780 &
		! 'NOTICE' COMMAND : &
		! IF NO LIST FILE EXISTS, THEN &
		!	ISSUE AN ERROR MESSAGE. &

23610	T$="***** NOTICE FROM OPERATOR :"+C9$+"***** "+ &
		C$+C9$ &
	\ GOSUB 22720 &
	\ GOTO 23780 &
		! IF THE NOTICE IS LEGAL, THEN &
		!	SET UP A NOTICE STRING; &
		!	GO PRINT IT; &
		!	AND EXIT. &

23780	R%(D%(R4%,0%))=R%(D%(R4%,0%)) AND NOT(D%(R4%,1%)) UNLESS R4%=0% &
		! ALL ROUTINES WHICH DO NOT EFFECT THE PROGRAM'S STATUS &
		! MUST EXIT THROUGH THIS LINE, IN ORDER TO ENSURE THAT &
		! NO EFFECT IS MADE. &

23790	RETURN &
		! AND EXIT. &
	&

23900	DATA	"**DECOP", &
		OFFLINE,	1,	1,	0, &
		ABORT,		1,	1,	0, &
		CONTINUE,	0,	2,	1, &
		DETACH,		0,	4,	2, &
		END,		1,	8,	0, &
		PAUSE,		0,	16,	3, &
		RETRY,		2,	32,	0, &
		SKIP,		2,	64,	0, &
		STATUS,		0,	128,	4, &
		TERMINATE,	1,	256,	0, &
		IGNORE,		2,	512,	0, &
		LAST,		0,	2048,	5, &
		LEGAL,		0,	4096,	6, &
		NOTICE,		0,	8192,	7, &
		**OTHER**,	2,	1024,	0 &

23910	! ORDER OF DATA : &
	!	COMMAND NAME; &
	!	COMMAND LEVEL : &
	!		0 => PROCESSED ENTIRELY BY THE COMMAND DECODER &
	!		1 => PROCESSED BY THE PROGRAM, BUT STILL AN &
	!			INTERRUPT COMMAND &
	!		2 => LEGAL ONLY AS A RESPONSE TO A REQUEST, NOT &
	!			AN INTERRUPT COMMAND &
	!	FLAG(S) TO SET IN RESPONSE WORD; &
	!	DISPATCH LINE # FOR PROCESS ROUTINE (IF ANY) &

23920	DATA	"**END" &
	&
	&

25000	DEF* FNN$(A%)=NUM1$((A% AND 32767%)-32768.*(A%<0%)) &
		! RETURNS AN UNSIGNED INTEGER. &

25010	DEF* FNN(A%)=(A% AND 32767%)-32768.*(A%<0%) &
		! RETURNS AN UNSIGNED INTEGER IN A REAL NUMBER. &
	&

25100	DEF* FNE$(E%,D%,P3%,P0%,E1%) &
		! TURN A SET OF PARAMETERS INTO AN ERROR MESSAGE. &
		!	E%  - ERROR NUMBER &
		!	D%  - DEPTH &

25110	Z$="" &
	\ IF E% THEN &
		Z$="CAN'T FIND FILE" IF (E% AND 63%)=5% &
	\	Z$="BAD BLOCK" IF E%=128%+13% &
	\	Z$="UNEQUAL COMPARE" IF E%=256%+15% &
	\	Z$="BAD BLOCK ON BACKUP VOLUME" IF E%=256%+13% &
	\	Z$="SUPERSEDE FAILURE" IF E%=256%+10% &
	\	Z$="LENGTH CHANGE" IF E%=256%+16% &
	\	Z$="DATA UNRELIABLE - FILE OPENED BY ANOTHER USER" IF &
			E%=256%+17% &
	\	Z$="'END' REQUESTED BY USER" IF E%=2048%+11% &
	\	Z$=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E% AND 127%)) &
			,4%),36%) UNLESS LEN(Z$) &
	\	IF P3%<>0% THEN &
			Z$=Z$+" IN " &
	\		Z$=Z$+"LIST FILE " IF D%=0% &
	\		Z$=Z$+"MFD " IF D%=1% &
	\		Z$=Z$+"UFD " IF D%=2% &
	\		Z$=Z$+"FILE " IF D%=3% &
	\		Z$=Z$+"***** BAD FNE CALL - D%="+FNN$(D%)+" " IF &
				D%<0% OR D%>3% &
	\		Z%=0% &
	\		Z%=4096% IF D%=1% &
	\		Z%=4224% IF D%=2% &
	\		Z%=4233% IF D%=3% &
	\		Z$=Z$+FNU$(P3%,"",Z%,0%) &
	\		IF P0%=0% THEN &
				GOTO 25130 &
			ELSE	Z$=Z$+" ON " &
	\			D%=D%+1% &
		! IF NO ERROR, DON'T GIVE THE ERROR TEXT; &
		! ELSE, SET UP THE APPROPRIATE. &

25120	Z%=4233% &
	\ IF D%>0% THEN &
		Z$=Z$+"RECORD" IF D%>3% &
	\	Z$=Z$+"FILE" IF D%=3% &
	\	Z$=Z$+"ACCOUNT" IF D%=2% &
	\	Z$=Z$+"VOLUME" IF D%=1% &
	\	Z%=4096% IF D%=1% &
	\	Z%=4224% IF D%=2% &
	\	Z%=9% IF D%>2% &
		! IF DEPTH IS NON-ZERO, THEN SET UP STRING ACCORDING &
		! TO DEPTH. &

25130	Z$=CVT$$(Z$,4%+16%) &
	\ Z$=Z$+" '"+FNU$(P0%,"",Z%,0%)+"'" IF P0%<>0% &
	\ Z$=Z$+", RECORD "+FNN$(E1%) IF E1%<>0% AND D%<4% &
	\ Z$="?"+Z$ IF R1% &
	\ FNE$=Z$ &
		! DISCARD TERMINATORS, REDUCE SPACES; &
		! IF THE MESSAGE REQUIRES A RESPONSE, THEN &
		!	PUT ON THE WARNING FLAG. &

25140	FNEND &
		! AND EXIT. &
	&
	&

25200	DEF* FNR%(S$,L%) &
	\ FNR%,Z1%=0% &
	\ Z0%=P% &
	! FUNCTION:	KEYWORD MATCHING ROUTINE. MATCHES A STRING IN &
	!		THE COMMAND STRING C$ STARTING AT POSITION P%+1% &
	!		TO THE DUMMY STRING S$. A MATCH IS MADE WHEN THE &
	!		MINIMUM NUMBER OF CHARACTERS ARE MATCHED. THIS &
	!		MINIMUM NUMBER IS HELD IN THE DUMMY VARIABLE &
	!		L%. &
	! PARAMETERS:	S$	STRING TO MATCH TO. &
	!		L%	MINIMUM LENGTH OF MATCH. &
	! GLOBAL &
	! VARIABLES &
	! AFFECTED:	P%	POSITION POINTER POINTS TO THE LAST CHAR- &
	!			ACTER MATCHED SUCCESSFULLY. &
	! LOCAL &
	! VARIABLES &
	! USED:		Z%	TEMPORARY CHARACTER POINTER. &
	!		Z$	CHARACTER MATCHING VARIABLE FOR PROCESS- &
	!			ING BEYOND THE MINIMUM LENGTH. &
	! RETURNS:	THE NUMBER OF CHARACTERS SUCCESSFULLY MATCHED. &
	! ERRORS:	NONE EXPECTED. &

25210	Z%=ASCII(RIGHT(C$,P%+1%)) &
	\ IF Z%=32% OR Z%=9% THEN P%=P%+1% &
				\ GOTO 25210 &
		! SKIP PAST BLANKS AND TABS. &

25220	IF MID(S$,1%,L%)=MID(C$,P%+1%,L%) THEN Z%=L% &
	ELSE GOTO 25250 &
		! SEARCH FOR MINIMUM MATCH. &

25230	Z$=MID(C$,P%+Z%+1%,1%) &
	\ IF Z$<>MID(S$,Z%+1%,1%) THEN GOTO 25240 &
	  ELSE Z%=Z%+1% UNLESS Z$="" &
	  \ GOTO 25230 UNLESS Z$="" &
		! SEARCH FOR MORE MATCHING CHARACTERS. &

25240	P%=P%+Z% &
	\ FNR%,Z1%=Z% &
		! RETURN WITH P% POINTING TO THE LAST SUCCESSFULLY &
		! MATCHED CHARACTER. FUNCTION WILL RETURN AS THE NUMBER &
		! OF CHARACTERS MATCHED. &

25250	P%=Z0% IF Z1%=0% &
	\ FNEND &
	&

25900	DEF* FNU$(P0%,C$,Z%,Z0%) &
	! FUNCTION :	FNU$	TURN A FILE DESCRIPTOR RECORD INTO &
	!			AN OPENABLE FILENAME STRING. &
	! PARAMETERS :	P0%	RECORD IN WORK-FILE TO CHANGE &
	!		C$	DUMMY STRING TO USE IN CONSTRUCTING &
	!			THE FILENAME STRING. &
	!		Z%	FLAG WORD 2 FORMAT WORD SHOWING WHICH &
	!			FIELDS IN THE RECORD TO CONVERT. &
	!		Z0%	WORD SHOWING WHICH SWITCHES TO CONVERT &
	!			(OF /MODE, /CLU, AND /FILESIZE). &
	! RETURNS :	FUNCTION VALUE &
	!			THE STRING TO USE IN AN OPEN. &
	! USES :	FNU0$(L%) &

25910	Z%=Z% AND Z0%(P0%,14%) &
	\ Z%=Z% AND -3201% IF (Z0%(P0%,13%) AND 1914%)<>0% &
		IF Z0%(P0%,14%)>0% &
	\ C$="" &
		! CREATE A WORD (Z%) WHICH HAS BITS SET FOR A FIELD ONLY &
		! IF THAT FIELD IS BOTH REQUESTED AND PRESENT IN THE &
		! DATA; &
		! IF DEVICE IS NOT DISK, DT, MT, OR LOGICAL, DO NOT &
		! RETURN FILENAME, EXTENSION; &
		! INITIALIZE THE STRING TO NULL. &
	&

25920	C$="_"+RAD$(Z0%(P0%,6%))+RAD$(Z0%(P0%,7%))+":" IF (Z% AND 4096%)<>0% &
	\ C$=C$+"["+FNU0$(SWAP%(Z0%(P0%,8%)))+","+FNU0$(Z0%(P0%,8%))+ &
		"]" IF (Z% AND 128%)<>0% &
	\ C$=C$+RAD$(Z0%(P0%,9%))+RAD$(Z0%(P0%,10%)) IF (Z% AND 1%)<>0% &
	\ C$=C$+"."+RAD$(Z0%(P0%,11%)) IF (Z% AND 8%)<>0% &
	\ C$=C$+"<"+FNU0$(SWAP%(Z0%(P0%,12%)))+">" IF (Z% AND 3072%)<>0% &
		! SET UP Z% AS BITS SET IN BOTH THE REQUESTED ENTRY &
		! WORD AND THOSE SET IN THE ACTUAL FILENAME STRING. &
		! IF DEV: REQ/EXSTS, MAKE IT; &
		! IF [PPN] REQ/EXSTS, MAKE IT; &
		! IF FILENAME REQ/EXSTS, MAKE IT; &
		! IF .EXT REQ/EXSTS, MAKE IT; &
		! IF <PROT> REQ/EXSTS, MAKE IT. &

25930	Z0%=Z0% AND Z0%(P0%,15%) AND 14336% &
	\ IF Z0%<>0% THEN &
		C$=C$+"/CL:"+NUM1$(Z0%(P0%,18%)) IF (Z0% AND 2048%)<>0% &
	\	C$=C$+"/MO:"+NUM1$(Z0%(P0%,16%)) IF (Z0% AND 4096%)<>0% &
	\	C$=C$+"/FI:"+NUM1$(Z0%(P0%,19%)) IF (Z0% AND 8192%)<>0% &
		! SET UP Z0% TO HOLD ONLY THE FLAGS SET BOTH IN THE &
		! OPEERAND IN THE CALL AND IN THE RECORD POINTED TO; &
		! PUT THE "/SWITCH:OPERAND" STRINGS INTO THE RETURNED &
		! STRING IN THE ORDER: &
		!	2048	CLUSTERSIZE &
		!	4096	MODE &
		!	8192	FILESIZE &

25960	FNU$=C$ &
		! SET FUNCTION VALUE. &

25970	FNEND &

25980	DEF* FNU0$(L%) &
	\ L%=L% AND 255% &
	\ IF L%=255% THEN FNU0$="*" ELSE FNU0$=NUM1$(L%) &
		! TAKE THE LOW BYTE OF L%; &
		! IF THAT BYTE IS 255, THEN &
		!	RETURN '*'; &
		! ELSE	RETURN NUM1$ OR BYTE. &

25990	FNEND &
	&
	&
	&

26000	DEF* FNO%(P0%,A0%,C0%) &
	\ FNO%=0% &
		! FUNCTION :	OPEN THE FILE WHOSE NAME IS IN RECORD &
		!		P0% OF THE WORK-FILE, ACCESS TYPE A0%, &
		!		ON CHANNEL C0%, USING SPECIFIED &
		!		CLUSTERSIZE, MODE, FILESIZE, AND &
		!		RECORDSIZE VALUES. &
		! &
		! PARAMETERS : &
		!	P0%	RECORD # OF FILE RECORD IN WORK-FILE &
		!	A0%	ACCESS TYPE : &
		!			1 - READ ONLY &
		!			2 - WRITE ONLY (SUPERSEDE) &
		!			3 - READ/WRITE &
		!			4 - \ PROTECTION &
		!			5 - \ VIOLATION &
		!			6 - OPEN, WRITE-ONLY, FOR EXTEND &
		!			7 - OPEN, READ/WRITE, FOR EXTEND &
		!			8 - OPEN FOR READ-REGARDLESS &
		!				(NO DROP OF PRIVILEGES) &
		!	C0%	CHANNEL ON WHICH TO OPEN IT. &

26010	E%=10% IF ((A0% AND 3%)=2% AND Z0%(P0%,13%)<0%) OR &
			((A0% AND 3%)<2% AND (A0% AND 4%)<>0%) &
	\ GOTO 26090 IF E%<>0% &
	\ Z0%(P0%,16%)=4096% IF A0%=8% &
	\ IF (A0% AND 4%)<>0% THEN &
		A0%=A0% AND 3% &
	\	IF (Z0%(P0%,13%) AND (1%+8%+128%))<>0% THEN &
			Z0%(P0%,15%)=Z0%(P0%,15%) OR 4096% &
	\		Z0%=Z0%(P0%,13%) &
	\		Z%=2% IF (Z0% AND 1%)<>0% &
	\		Z%=Z% OR 8192% IF (Z0% AND 8%)<>0% &
	\		Z%=128% IF (Z0% AND 128%)<>0% &
	\		Z0%(P0%,16%)=Z% &
	\		A0%=3% &
		! IF THIS IS AN ATTEMPT TO SUPERSEDE A FILE ON ANOTHER &
		! ACCOUNT BY A NON-PRIVILEGED USER, GIVE HIM AN ERROR. &
		! THIS CHECK IS NECESSARY TO PREVENT A NON-PRIVILEGED &
		! USER FROM DESTROYING FILES ON OTHER ACCOUNTS TO WHICH &
		! HE HAS WRITE ACCESS. &

26020	ON ERROR GOTO 26070 &
	\ Z$=SYS(PRIV.ON$) IF A0%=8% &
	\ Z$=FNU$(P0%,"",-1%,-1%) &
	\ Z%=(Z0%(P0%,17%) AND (Z0%(P0%,15%) AND 1024%)<>0%) &
	\ OPEN Z$ FOR INPUT AS FILE C0%, RECORDSIZE Z% IF A0%=1% OR &
			A0%=8% &
	\ OPEN Z$ FOR OUTPUT AS FILE C0%, RECORDSIZE Z% IF A0%=2% &
	\ OPEN Z$ AS FILE C0%, RECORDSIZE Z% IF A0%=3% &
		! SET UP TO TRAP AN OPEN ERROR; &
		! DROP PRIVILEGES, SET UP FILE NAME WITH SWITCHES, SET &
		! UP RECORDSIZE; &
		! GIVE HIM THE KIND OF OPEN HE ASKED FOR: &
		!	READ ONLY	OPEN FOR INPUT &
		!	WRITE ONLY	OPEN FOR OUTPUT &
		!	READ/WRITE	OPEN &

26050	Z$=SYS(PRIV.OFF$) &
	\ ON ERROR GOTO 19000 &
		! RECOVER PRIVILEGES; &
		! RESET ERROR TRAP. &

26060	FNEND &
		! AND EXIT. &

26070	&
	!	ERROR HANDLER &

26080	E%=ERR \ RESUME 26090 &
		! THIS IS FOR ERRORS TRAPPED IN THE OPEN. &

26090	E%=E% OR 1024% \ GOTO 26050 &
		! SET THE FLAG BIT SAYING 'ON OPEN', AND EXIT. &
	&
	&

26100	DEF* FNA% &
		! RETURN SUBSCRIPT OF NEXT FREE INDEX ENTRY AND ZERO &
		!  THAT ENTRY. &
		! PARAMETERS:	NONE &
		! RETURNS:	SUBSCRIPT OF THE FIRST ENTRY IN THE FREE &
		!		SPACE LIST. &
		! USES:	Z%	LOOP VARIABLE FOR ZEROING &
		! NOTES:	USES FREE LIST POINTER OUT OF THE INDEX FILE &
		!		HEADER TO GET SUBSCRIPT.  IF FL POINTER IS &
		!		NEGATIVE, THAT PART OF THE FILE HAS NOT YET &
		!		BEEN INITIALIZED, AND THE ROUTINE WILL GO TO &
		!		DO IT. THEN THE ENTRY TO BE RETURNED IS ZEROED. &

26110	B0%=Z0%(0%,0%) IF B0%=0% &
	\ Z0%=B0% &
	\ IF Z0%<0% THEN &
		Z0%=-Z0% &
	\	Z0%(Z%,1%)=Z%+1% FOR Z%=Z0% TO Z0%+7% &
	\	Z0%(Z%,1%)=-Z%-1% &
		! GET NEXT FREE LIST ENTRY; &
		! IF SUBSCRIPT IS NEGATIVE, THIS ENTRY HAS NOT YET BEEN &
		! TOUCHED, SO EXTEND A FEW. &

26120	B0%=Z0%(Z0%,1%) &
	\ Z0$(Z0%)="" &
	\ FNA%=Z0% &
		! ZERO OUT THE RETURNED ENTRY AND SET FUNCTION VALUE. &

26130	FNEND &
		! END OF FNA%. &
	&

26200	DEF* FNA0%(P0%) &

26210	Z0%,Z2%=P0% &
	\ Z0%=Z0%(Z0%,1%) WHILE Z0%(Z0%,1%)<>0% &
	\ FOR Z%=2% TO 5% &
	\	Z2%=P0% &
	\	WHILE Z2%<>0% &
	\		IF Z0%(Z2%,Z%)<>0% THEN &
				Z3%=Z0%(Z2%,Z%) &
	\			Z0%(Z2%,Z%)=0% &
	\			Z0%,Z0%(Z0%,1%)=Z3% &
	\			Z0%=Z0%(Z0%,1%) WHILE Z0%(Z0%,1%)<>0% &

26220			Z2%=Z0%(Z2%,1%) &
	\	NEXT &
	\ NEXT Z% &
	\ Z0%(Z0%,1%)=B0% &
	\ B0%=P0% &
	\ FNA0%=0% &

26230	FNEND &
	&

26300	DEF* FNP0%(B%,L%,D%) &
	! FUNCTION :	FNP0%	PUSH CURRENT LIST AND SET UP LIST &
	!			FROM Z0%(B%,L%).  CLEAR LIST IF D%<>0%. &
	! PARAMETERS :	B%	RECORD NUMBER WHOSE LIST IS TO BE SET UP &
	!		L%	ENTRY NUMBER OF LIST TO SET UP. &
	!		D%	0=>REALLY SET UP LIST &
	!			1=>CLEAR LIST &
	! RETURNS P1% POINTING TO LAST RECORD IN LIST, OR LIST HEADER &
	! (B%) IF NONE. &

26310	S%=S%+1% \ S%(S%)=P1% &
	\ S%=S%+1% \ S%(S%)=Z0%(B%,1%) &
	\ S%=S%+1% \ S%(S%)=L% &
	\ S%=S%+1% \ S%(S%)=B% &
	\ P1%=Z0%(B%,L%) &
	\ P1%=FNA0%(P1%) IF P1%<>0% AND D%<>0% &
	\ Z0%(B%,1%)=P1% &
	\ P1%=B% &
	\ P1%=Z0%(P1%,1%) WHILE Z0%(P1%,1%)<>0% &
	\ FNP0%=B% &
	! PUSH CURRENT LIST, LIST NUMBER TO SET UP, AND CURRENT BASE; &
	! CLEAR LIST IF REQUESTED AND SET UP LIST; SET UP END OF LIST &
	! POINTER. &

26320	FNEND &
	! AND EXIT. &
	&
	&

26450	DEF* FNP1% &
	! FUNCTION :	FNP1%	RESTORE ORIGINAL LIST FROM DATA ON STACK &
	! PARAMETERS :	NONE &

26460	Z%=S%(S%) \ S%=S%-1% &
	\ Z0%(Z%,S%(S%))=Z0%(Z%,1%) &
	\ Z0%(Z%,1%)=S%(S%-1%) &
	\ P1%=S%(S%-2%) &
	\ S%=S%-3% &
	\ FNP1%=Z% &
	! PUT NEW LIST WHERE IT BELONGS; RESTORE OLD LIST; POP ALL &
	! THREE VALUES; SET VALUE TO OLD BASE. &

26470	FNEND &
	! AND EXIT. &
	&
	&

31000	&
	&
	&
	!	C H A I N    E N T R Y &

31010	E0%=2% &
	\ C$=SYS(CHR$(7%)) &
	\ B%=CVT$%(C$) &
	\ W$=RIGHT(C$,3%) &
	\ GOTO 1000 &
		! TAKE THE BASE FHR AND THE WORK-FILE &
		! NAME OUT OF CORE COMMON AND GO TO WORK. &
	&

32700	Z$=SYS(PRIV.ON$) &
	\ Z$=SYS(CHR$(6%)+CHR$(8%)+CHR$(J%)+STRING$(24%,0%)+CHR$(-1%)) IF &
			J% AND (R% AND 16384%)=0% &
		! KILL OURSELVES IF WE ARE DETACHED. &

32767	END
