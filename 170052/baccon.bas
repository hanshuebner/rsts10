2!		PROGRAM		: BACCON.BAS
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
	!				TO PRIV'D USER &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	&
!	THIS MODULE IS THE CONTROL MODULE FOR THE BACKUP PACKAGE.  ITS &
!	PURPOSE IS TO OVERSEE THE FLOW OF THE PACKAGE DEPENDING ON WHICH &
!	PROCESSES HAVE BEEN COMPLETED AND WHICH ARE STILL PENDING.  THIS &
!	MODULE IS CHAINED TO, MAKES A DECISION, AND THEN CHAINS TO SOME &
!	OTHER MODULE. &

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
	&
!	CHANNEL #		USED FOR &
!	   1			WORK FILE. &

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&
	&
!	VARIABLE NAME		USED FOR &
!	A$		ACCOUNT NUMBER STRING. &
!	A%		ACCOUNT NUMBER. &
!	B0%		FREE LIST POINTER. &
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
!	D%		COMPLETED WORD MASK. &
!	D0%		REQUESTED WORD MASK. &
!	E$		ERROR STRING. &
!	E%		ERROR VARIABLE. &
!	E0%		ENTRY TYPE. &
!	E2%		PACKAGE ERROR COUNT. &
!	F0%		PROCESS OUTSTANDING FLAG. &
!	I$		VERSION-EDIT. &
!	J$		JOB NUMBER STRING. &
!	J%		JOB NUMBER. &
!	J1%		JOB DATA BLOCK 1. &
!	K%		KB: NUMBER. &
!	K0%		I/O BLOCK. &
!	K1%		KB: LIST FILE FLAG. &
!	L0%		LIST FILE FLAG. &
!	P%		STRING POSITION POINTER. &
!	P0%		CURRENT RECORD POINTER. &
!	P1%		RECORD POINTER. &
!	P$		CHAIN MODULE NAME. &
!	PRIV.OFF$	SYS(PRIV.OFF$) TURNS PRIV'S OFF TEMPORARILY. &
!	PRIV.ON$	SYS(PRIV.ON$) REGAINS PRIVILEGES. &
!	R%		PACKAGE STATUS WORD. &
!	R$		PACKAGE STATE VARIABLE. &
!	R%()		RESPONSE ARRAY. &
!	S%()		STACK. &
!	T$		STRING TO PARSE. &
!	T0$		USED TO BUILD T$. &
!	W$		WORK FILE NAME. &
!	ALL Z'S		TEMPORARY LOCAL WORK VARIABLES. &

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
	! CHECK FOR ATTACHED	22800-22830		Check KB: DDB for &
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
	! FNC1%(V0%,V1%,C%)	25300		Check for any flags &
	!					indicating a request &
	!					outstanding in the &
	!					PRW/PCW combination &
	!					given by V0%,V1%. &
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
903	DIM S%(30%)		! INTERNAL STACK. &

905	DIM C%(4%),C(4%)	! THESE TWO ARRAYS ARE USED IN &
				! THE 'STATUS' INTERRUPT COMMAND, &
				! BELOW.
906	DIM D$(14%),D%(14%,2%)	! THESE TWO ARRAYS ARE USED IN &
				! THE DECODE OF INTERRUPT COMMANDS, &
				! BELOW.
907	DIM R%(2%),R0%(2%)	! THESE TWO ARRAYS ARE USED TO &
				! HOLD THE RESULTS/REQUESTS TO THE &
				! INTERRUPT COMMAND PROCESSOR, BELOW. &

950	!	DIMENSION STATEMENTS FOR STANDARD ROUTINES
951	DIM Z%(30%)		! USED BY FNP%() FOR RETURN OF DATA
952	DIM Z1%(30%)		! USED BY FNP%() FOR TEMPORARY STORAGE &
				! OF DATA. &
	&
	&
	&

999	&
	&
	&
	!	M A I N    C O D E &

1000	ON ERROR GOTO 19000 &
		! SET UP STANDARD ERROR TRAP. &
	\ Z$=SYS(CHR$(6%)+CHR$(-21%)+CHR$(255%)) &
		! DROP PRIVILEGES TEMPORARILY. &

1010	I$="BACCON	V10.1-A" &
		! SET UP HEADER LINE. &

1020	PRINT I$ IF E0%=0% &
		! PRINT THE HEADER LINE PREPARATORY TO GIVING &
		! AN ERROR. &
	&

1030	CHANGE SYS(CHR$(12%)) TO Z% &
	\ D5$="["+NUM1$(Z%(6%))+","+NUM1$(Z%(5%))+"]" &
	\ D5$="_"+CHR$(Z%(23%))+CHR$(Z%(24%))+NUM1$(Z%(25%))+":"+D5$ &
!			IF Z%(26%) AND 1% &
!	\ IF Z%(3%)+SWAP%(Z%(4%))<>15%*2% OR E0%<>2% THEN &
!		PRINT "?ILLEGAL ENTRY - PLEASE RUN THE RESTOR PROGRAM" &
!	\	GOTO 32767 &
		! SET UP THE NAME OF THE DEVICE AND THE ACCOUNT FROM WHICH WE &
		! WERE RUN. &
		! NOTE : THE BOTTOM TWO BITS OF BYTE 26 OF THE RETURNED STRING &
		! ARE CODED AS : &
		!	BIT 0 = 0 => SOURCE DEVICE IS PUBLIC &
		!		1 => SOURCE DEVICE IS PRIVATE &
		!	BIT 1 = 0 => GENERAL SPECIFICATION (IE, SY:) &
		!		1 => SPECIFIC SPECIFICATION (EG, SY0:, DK0:) &
		! ILLEGAL ENTRY IF WE DIDN'T COME FROM A COMPILED FILE &
		! OR NOT CHAINED. &

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
	\ R9%=Z0%(0%,28%) &
	\ B0%=Z0%(0%,0%) &
	\ E2%=Z0%(0%,21%) &
	\ R%=Z0%(0%,26%) AND -8193% &
	\ K%=(SWAP%(PEEK(PEEK(K0%)+2%)) AND 255%) OR &
		(R% AND 16384%)=0% &
	\ Z$=SYS(PRIV.OFF$) &
	\ T0$=CHR$(6%)+CHR$(18%)+CHR$(-1%)+CHR$(0%)+ &
		MID(SYS(CHR$(6%)+CHR$(-10%)+"OPSER"),7%,4%) &
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
	&

8000	&
	&
	&
	!	S E T    U P    C H A I N    L I S T &

8010	IF (R% AND 1%)<>0% THEN &
		GOTO 9000 &
	ELSE	D%=Z0%(0%,30%) &
	\	D0%=Z0%(0%,31%) &
		! IF AN 'ABORT' WAS REQUESTED, THEN &
		!	GO DO DISMOUNTS, FINAL LIST; &
		! ELSE	HOLD PROCESSES REQUESTED WORD, PROCESSES &
		!		COMPLETED WORDS OF FILE HEADER RECORD &
		!		(AND MAKE SURE THAT THE LIST FLAGS ARE &
		!		SET). &

8020	IF FNC1%(D0%,D%,8%)<>0% THEN &
		T$=D5$+"BACLST" &
	\	GOTO 8400 &
		! IF THE HEADER LIST IS REQUESTED, THEN &
		!	SET UP THE LIST OF PROGRAMS; &
		!	GO TO SET UP THE CHAIN LIST. &

8030	IF R9%<>1% THEN &
		GOTO 8200 &
		! IF THIS IS A RESTORE OPERATION (IE, IF THE 'DUMP &
		!	INDEX' FLAG IS NOT SET), THEN &
		!		GO TO THE RESTORE STREAM. &
	&
	&

8200	&
	&
	&
	!	R E S T O R E    S T R E A M &

8210	IF FNC1%(D0%,D%,4%)<>0% THEN &
		P0%=2% &
	\	F0%=4% &
	\	GOSUB 10100 &
	\	T$=D5$+"BACLOD" &
	\	GOTO 8400 &
		! IF THE NEXT STEP IS TO LOAD AN INDEX, THEN &
		!	SET UP FOR A SCAN OF THE INPUT VOLUME LIST TO &
		!		SEE IF ANY VOLUME WITH SOME &
		!		'LOAD INDEX OUTSTANDING' NEEDS TO BE &
		!		MOUNTED FIRST; &
		!	DO THE SCAN; &
		!	SET UP THE CALL TO LOAD; &
		!	GO DO THE CALL. &

8220	IF FNC1%(D0%,D%,32%)<>0% THEN &
		T$=D5$+"BACDIR" &
	\	GOTO 8400 &
		! IF THE NEXT STEP IS THE SELECT PASS, THEN &
		!	SET UP THE CALL; &
		!	GO DO THE CALL. &

8230	IF FNC1%(D0%,D%,256%)<>0% THEN &
		T$=D5$+"BACENT" &
	\	GOTO 8400 &
		! IF THE NEXT STEP IS 'ENTER ACCOUNTS', THEN &
		!	SET UP THE CALL; &
		!	GO DO THE CALL. &

8240	IF FNC1%(D0%,D%,1024%)<>0% THEN &
		P0%=2% &
	\	F0%=1024% &
	\	GOSUB 10100 &
	\	IF P0%=0% THEN &
			D%,Z0%(0%,30%)=D% OR 1024% &
		ELSE	T$=D5$+"BACFRM" &
	\		GOTO 8400 &
		! IF THE NEXT STEP IS TRANSFER, THEN &
		!	SET UP FOR A SCAN OF THE INPUT VOLUME LIST TO &
		!		DETERMINE IF THE NEXT INPUT VOLUME WITH &
		!		SOME TRANSFER OUTSTANDING NEEDS TO BE &
		!		MOUNTED FIRST; &
		!	DO THE SCAN; &
		!	IF NO OUTSTANDING TRANSFER IS FOUND, THEN &
		!		SET 'TRANSFER COMPLETED' IN THE FHR; &
		!	ELSE	SET UP THE CALL TO THE TRANSFER PROGRAM; &
		!		GO DO THE CALL. &

8250	GOTO 8300 &
		! IF THE PROGRAM DROPS THROUGH ALL OF THE ABOVE &
		!	DECISIONS, THEN &
		!		IT MUST BE TIME TO CLEAN UP. &
	&
	&
	&

8300	&
	&
	&
	!	E N D    O F    P R O C E S S I N G    R O U T I N E &

8310	P0%=2% IF R9%<>1% &
	\ P0%=4% IF R9%=1% &
	\ F0%=1% &
	\ GOSUB 10100 &
	\ GOTO 8500 UNLESS P0% &
	\ D0%,Z0%(0%,31%)=D0% OR 1% &
	\ GOTO 8400 &
		! SET UP TO DO A SCAN ON THE BACKUP VOLUME LIST : &
		!	IF THIS IS A 'RESTORE', THEN THE BACKUP VOLUMES &
		!		ARE IN THE INPUT VOLUME LIST; &
		!	IF THIS IS A 'BACKUP', THEN THE BACKUP VOLUMES &
		!		ARE IN THE OUTPUT VOLUME LIST; &
		!	SET UP THE SCAN FLAG FOR ANY DISMOUNTS &
		!		OUTSTANDING; &
		! DO A SCAN FOR ANY (BACKUP VOLUME) DISMOUNTS &
		! OUTSTANDING; &
		! GO DO FINAL LIST IF NONE FOUND &
		! SET DISMOUNT REQUESTED IN THE FHR &
		! AND GO CALL BACMNT &
	&
	&

8400	&
	&
	&
	!	S E T    U P    C H A I N    L I S T &

8410	T0$=D5$+"BACMNT" &
	\ T0$=T0$+"," IF LEN(T$)<>0% &
	\ T$=T0$+T$ IF FNC1%(D0%,D%,3%)<>0% &
	\ T$=T$+","+D5$+"BACCON" &
	\ P%=0% &
	\ P0%=FNP0%(0%,13%,0%) &
		! SET UP THE CHAIN LIST ACCORDING TO PARAMETERS : &
		!	IF A 'MOUNT REQUESTED' OR 'DISMOUNT REQUESTED' &
		!		IS OUTSTANDING, THEN FIRST CHAIN OFF TO &
		!		BACMNT; &
		!	ALWAYS RETURN TO BACCON; &
		! INITIALIZE THE CHARACTER POINTER TO THE BEGINNING OF &
		! THE CHAIN STRING. &

8420	WHILE P%<LEN(T$) &
	\	P1%=P0% &
	\	P0%=FNA% &
	\	Z0%(P0%,1%)=Z0%(P1%,1%) &
	\	Z0%(P1%,1%)=P0% &
	\	P%=FNP%(P0%,P%,T$,0%,0%)+1% &
	\ NEXT &
		! FOR EACH FILE NAME IN THE STRING : &
		!	MOVE THE 'FOLLOWING' POINTER; &
		!	SET UP A NEW RECORD; &
		!	LINK IT IN; &
		!	DO THE FILENAME CONVERSION; &
		! CONTINUE THE LOOP. &

8430	P0%=FNP1% &
	\ GOTO 9000 &
		! RESTORE THE LIST; &
		! EXIT. &
	&

8500	&
	&
	&
	!	D O    T H E    F I N A L    C L E A N U P &

8510	IF (R% AND 1%)=0% THEN &
		IF FNC1%(Z0%(0%,31%),Z0%(0%,30%),16%)<>0% THEN &
			T$=D5$+"BACLST" &
	\		GOTO 8400 &
		! IF THE RUN WAS NOT ABORTED, THEN &
		!	IF THE FINAL LIST IS NOW REQUIRED, THEN &
		!		SET UP A CALL TO BACLST; &
		!		GO DO THE CALL. &
	&

9000	&
	&
	&
	!	C O M P L E T I O N    R O U T I N E S &

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

10000	&
	&
	&
	!	P R O G R A M    S U B R O U T I N E S &
	&

10100	&
	&
	&
	!	F I N D    N E X T    V O L U M E    W I T H &
	!	T H E    S P E C I F I E D    F L A G ( S )     S E T &
	!	A N D    E N S U R E    T H A T    I T    I S &
	!			M O U N T E D &
	&
	! PARAMETERS :	P0%	ADDRESS IN THE FHR AT WHICH TO FIND THE &
	!			POINTER TO THE LIST. &
	!		F0%	FLAG(S) VALUE(S) TO CHECK &
	&
	! RETURNS :	IF ONE FOUND, THEN IT RETURNS THE SUBSCRIPT OF &
	!		THAT RECORD IN P0%, AND, IF THAT RECORD DOES NOT &
	!		INDICATE THAT A MOUNT IS COMPLETE, THEN SETS THE &
	!		'MOUNT REQUESTED' FLAG IN THE FHR AND ON THE &
	!		VOLUME. &
	!		IF NONE FOUND, RETURNS P0% AS 0%. &

10110	P1%,P0%=Z0%(0%,P0%) &
	\ P0%=Z0%(P0%,1%) WHILE P0%<>0% AND FNC1%(Z0%(P0%,31%), &
		Z0%(P0%,30%),F0%)=0% &
		! SET UP THE REQUESTED LIST; &
		! SCAN THE LIST UNTIL EITHER THE END IS FOUND OR A &
		!	RECORD IS FOUND IN THE LIST WITH ONE OR MORE OF &
		!	THE REQUESTED FLAGS SET. &

10120	IF P0%<>0% THEN &
		Z0%=Z0%(P0%,31%) OR 2% &
	\	P1%=Z0%(P1%,1%) WHILE P1%<>0% AND (P1%=P0% OR &
			(Z0%(P1%,31%) AND 1%)=0%) &
	\	D0%,Z0%(0%,31%)=D0% OR 1% IF P1%<>0% &
	\	IF FNC1%(Z0%,Z0%(P0%,30%),2%)<>0% THEN &
			Z0%(P0%,31%)=Z0% &
	\		D0%,Z0%(0%,31%)=D0% OR 2% &
	\		D%,Z0%(0%,30%)=D% AND -3% &
		! IF A RECORD IS FOUND WITH THE PROPER FLAG(S) SET, THEN &
		!	SET THE 'MOUNT REQUESTED' FLAG ON THAT VOLUME; &
		!	SCAN FOR ANOTHER VOLUME MOUNTED WHICH SHOULD BE &
		!	 DISMOUNTED; &
		!	SET THE 'DISMOUNT REQUESTED' FLAG ON THE FHR IF &
		!	 ONE IS FOUND; &
		!	IF THE VOLUME IN THAT RECORD IS NOT MOUNTED, &
		!	THEN	SET THE 'MOUNT REQUESTED' FLAG IN THE &
		!			VOLUME RECORD; &
		!		SET THE 'MOUNT REQUESTED' FLAG IN THE &
		!			FHR; &
		!		MAKE SURE THAT THE 'MOUNT COMPLETED' &
		!			FLAG IN THE FHR IS NOT SET. &

10130	RETURN &
		! AND EXIT. &
	&
	&

19000	&
	&
	&
	!	T R A P    F O R    U N H A N D L E D    E R R O R S &

19010	RESUME 19020 &
		! BE SURE TO CLEAR THE ERROR. &

19020	E%=ERR IF E%=0% &
	\ T$=RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E% AND 63%)),4%) &
	\ T$="?UNEXPECTED ERROR IN "+I$+" - "+T$+" AT LINE"+NUM$(ERL)+ &
		" - FATAL" &
	\ T$=T$+C9$+" ON 'CHAIN' TO '"+P$+"'" IF ERL=9040 &
	\ IF (ERL<23100 OR ERL>23199) AND ERL>1099 THEN &
		GOSUB 23100 &
	  ELSE	PRINT T$ &
	\	GOTO 32767 &
		! SET UP TEXT OF ERROR MESSAGE; &
		! IF THE ERROR DID NOT HAPPEN WITHIN THE 'ISSUE A &
		!	MESSAGE' ROUTINE, THEN &
		!		GO ISSUE THE MESSAGE; &
		! ELSE	PRINT IT OUT. &

19030	RESUME 32700 &
		! AND EXIT THE PROGRAM. &
	&

22500	RETURN &

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

22620	Z$=SYS(PRIV.OFF$) &
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

22700	RETURN
22800	RETURN
22900	T$="" \ RETURN
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
	\ T=TIME(0%) &
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
	\ IF (R% AND 16384%)=0% OR ASCII(T$)=128%+64% THEN &
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

23130	IF (R% AND 16384%)<>0% AND ASCII(T$)<>128%+64% THEN &
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

23200	RETURN
25000	DEF* FNN$(A%)=NUM1$((A% AND 32767%)-32768.*(A%<0%)) &
		! RETURNS AN UNSIGNED INTEGER. &

25300	DEF* FNC1%(V0%,V1%,C%)= &
		((V0% AND C%)<>0% AND (V1% AND C%)<>(V0% AND C%)) &
		! FUNCTION :	FNC1%	RETURN NON-ZERO IF A FLAG IS SET &
		!			IN THE PROCESS REQUEST WORD AND &
		!			NOT SET IN THE PROCESS COMPLETED &
		!			WORD. &
		! PARAMETERS :	V0%	PROCESS REQUEST WORD &
		!		V1%	PROCESS COMPLETED WORD &
		!		C%	VALUE(S) OF FLAG(S) TO CHECK &
	&
	&

25500	DEF* FNF0%(P0%,N$,D$) &
	! FUNCTION:	FNF0%	APPLY 'NULL REPLACE' AND 'DEFAULTS' &
	!			TO ENTRY IN SUBSCRIPT P0%. &
	! PARAMETERS:	P0%	SUBSCRIPT OF ENTRY TO BE AFFECTED &
	!		N$	NULL REPLACE STRING &
	!		D$	DEFAULT STRING &
	! RETURNS:	FNF0%	????? &
	!		Z0%(P0%,X) &
	!			RECORD WITH NULL REPLACE OR DEFAULTS &
	!			APPLIED &

25510	IF (Z0%(P0%,14%) OR Z0%(P0%,15%))=0% THEN &
		Z%=FNP%(P0%,0%,N$,0%,0%) &
	\	Z0%=Z0%(P0%,14%) &
	\	GOTO 25680 &
		! IF NOTHING IN THE RECORD, APPLY 'NULL REPLACE'. &

25520	Z%=FNP%(-1%,0%,D$,0%,0%) &
	\ Z%=Z%(14%) &
	\ Z0%=Z0%(P0%,14%) &
	\ GOTO 25600 IF Z%=0% &
		! HOLD FW2 OF RECORD TO BE EFFECTED IN Z0%; &
		! DO FSS CALL ON THE 'DEFAULT STRING'; &
		! HOLD FW2 OF 'DEFAULT STRING' IN Z%; &
		! SKIP REPLACE IF DEFAULT STRING IS NULL. &

25530	&
	&
	&
	!	A P P L Y    F I L E N A M E    D E F A U L T S &

25540	IF (Z0% AND 4096%)=0% THEN &
		Z0%(P0%,13%)=(Z%(13%) AND 32767%) OR &
			(Z0%(P0%,13%) AND NOT (32767%) AND &
			((STATUS AND 256%)=0%)) &
	\	Z0%(P0%,Z3%)=Z%(Z3%) FOR Z3%=6% TO 7% &
	\	Z0%=Z0% OR (Z% AND -4096%) &
		! DEFAULT THE DEVICE AND DHI FLAG WORD. &

25550	IF (Z0% AND 128%)=0% THEN &
		Z0%(P0%,8%)=Z%(8%) &
	\	Z0%(P0%,25%)=Z%(25%) &
	\	Z0%=Z0% OR (Z% AND 896%) &
		! DEFAULT THE PPN. &

25560	IF (Z0% AND 1%)=0% THEN &
		Z0%(P0%,Z3%)=Z%(Z3%) FOR Z3%=9% TO 10% &
	\	Z0%=Z0% OR (Z% AND 7%) &
		! DEFAULT THE FILENAME. &

25570	IF (Z0% AND 8%)=0% THEN &
		Z0%(P0%,11%)=Z%(11%) &
	\	Z0%=Z0% OR (Z% AND 120%) &
		! DEFAULT THE EXTENSION. &

25580	IF (Z0% AND 1024%)=0% THEN &
		Z0%(P0%,12%)=Z%(12%) &
	\	Z0%=Z0% OR (Z% AND 3072%) &
		! DEFAULT THE PROT/STATUS. &

25590	Z0%(P0%,14%)=Z0% &
		! PUT IN ALL THE NEW VALUES. &

25600	&
	&
	&
	!	A P P L Y    C H A R A C T E R I S T I C &
	!		D E F A U L T S &

25610	Z0%=Z0%(P0%,15%) &
	\ Z%=Z%(15%) &
		! SET UP THE TWO FLAG WORDS FOR THE CHARACTERISTICS. &

25620	IF (Z0% AND 2048%)=0% THEN &
		Z0%(P0%,18%)=Z%(18%) &
	\	Z0%=Z0% OR (Z% AND 2048%) &
		! DEFAULT THE CLUSTERSIZE. &

25630	IF (Z0% AND 4096%)=0% THEN &
		Z0%(P0%,16%)=Z%(16%) &
	\	Z0%=Z0% OR (Z% AND 4096%) &
		! DEFAULT THE MODE. &

25640	IF (Z0% AND 8192%)=0% THEN &
		Z0%(P0%,19%)=Z%(19%) &
	\	Z0%=Z0% OR (Z% AND 8192%) &
		! DEFAULT THE FILESIZE. &

25670	Z0%(P0%,15%)=Z0% &
		! PUT THE NEW DATA INTO THE SELECTION SPECIFICATION &
		! WORD. &

25680	FNF0%=Z0%(P0%,14%) &
		! SET RETURNED VALUE. &

25690	FNEND &
		! AND EXIT. &
	&
	&
	&
	&

25800	DEF* FNP%(P0%,P%,S$,Y0%,Y1%) &
	! FUNCTION:	FNP%	DO (NEW) FSS ON STRING S$, STARTING FROM &
	!			POSITION P%,AND RETURN THE DATA IN ARRAY &
	!			Z0%(P0%,X%), AND THE NEW CHARACTER POSITION &
	!			IN THE FUNCTION'S VALUE. &
	! PARAMETERS:	P0%	RECORD NUMBER IN Z0%(?,?) IN WHICH TO RETURN &
	!			THE DATA &
	!		P%	CHARACTER POSITION AFTER WHICH TO START THE &
	!			SCAN. &
	!		S$	STRING TO SCAN &
	!		Y0%	FIELD PROHIBIT WORD - SAME LAYOUT AS THAT &
	!			OF FLAG WORD 2 OF THE FSS SYS() CALL - &
	!			ANY BIT SET IN THIS WORD WHOSE CORRESPONDING &
	!			BIT IS SET IN THE FW2 RETURNED BY THE SYS() &
	!			CALL WILL RETURN E%=130%. &
	!		Y1%	DEVICE PROHIBIT/EXTRA FLAG/EXTRA SWITCH PROHIBIT &
	!			WORD - IF ANY BIT IS SET IN THIS WORD AND THE &
	!			CORRESPONDING BIT IS SET IN THE DEVICE HANDLER &
	!			INDEX (BITS 0-13) OF THIS WORD, E%=134% &
	!			IS RETURNED; IF ANY SUCH &
	!			CONDITION OCCURS IN THE TOP TWO BITS, &
	!			E% IS RETURNED AS 130%. &
	! GLOBAL VARIABLES EFFECTED: &
	!		E%	ERROR VALUE, IF ANY. &
	!		Z0%(P0%,X%) &
	!			THIS IS THE ULTIMATE PURPOSE OF THIS FUNCTION - &
	!			THIS WILL BE RETURNED AS AN INDEX-FILE FORMAT &
	!			REPRESENTATION OF THE SCANNED STRING. &
	!		Z%(30%)	USED FOR FSS FORMATTED STRING INTERMEDIATE &
	!			STORAGE; RETURNED IN THE SAME FORMAT AS THE &
	!			Z0%(P0%,X) ENTRY. &
	! LOCAL VARIABLES USED: &
	!		Z%	FOR LOOP COUNTER AND VARIOUS OTHER TEMPORARY &
	!			PURPOSES &
	!		Z1%(30%) &
	!			AS INTERMEDIATE STORAGE FOR THE STRING &
	!			RETURNED BY THE FSS CALL. &
	! RETURNS:	FNP%	POSITION OF THE CHARACTER AFTER THE LAST ONE &
	!			SCANNED.  IF ERROR (E%<>0%), THIS IS THE &
	!			SAME AS THE BEGIN POSITION ON ENTRY TO &
	!			THE FUNCTION (IE, P%, ABOVE). &
	! ERRORS EXPECTED: &
	!	E%=2	ILLEGAL FILENAME &
	!			IF THE FILENAME SPECIFIED DOES NOT &
	!			CONSTITUTE A VALID RSTS/E FILE SPEC. &
	!	E%=6	NOT A VALID DEVICE &
	!			IF THE DEVICE SPECIFIED IS INVALID. &
	!	E%=130	ILLEGAL FILENAME+ &
	!			IF SOME PROHIBITED FIELD, INCLUDING &
	!			ABSOLUTE DEVICE UNIT OR ANOTHER PPN, &
	!			WHEN PROHIBITED, WAS SPECIFIED. &
	!	E%=134	NOT A VALID DEVICE+ &
	!			WHEN A PROHIBITED DEVICE IS SPECIFIED. &
	&

25810	ON ERROR GOTO 25880 &
	\ CHANGE SYS(CHR$(6%)+CHR$(-23%)+RIGHT(S$,P%+1%)) TO Z1% &
	\ Z1%(Z%)=Z1%(Z%)+SWAP%(Z1%(Z%+1%)) FOR Z%=1% TO 29% STEP 2% &
	\ FNP%=LEN(S$)-RECOUNT &
		! SET UP ERROR TRAP; DO THE SYS() CALL FOR FSS; &
		! PACK THE RETURNED DATA INTO THE ODD WORDS IN THE TEMP &
		! ARRAY; AND UPDATE CURRENT POSITION IN THE SOURCE &
		! STRING. &

25820	IF (Z1%(29%) AND -28672%)=4096% THEN &
		S$=CVT%$(SWAP%(Z1%(23%))) &
	\	S$=S$+FNU0$(Z1%(25%)) IF Z1%(25%)<0% &
	\	S$=MID(SYS(CHR$(6%)+CHR$(-10%)+S$+":"+S$),7%,4%) &
	\	Z1%(23%)=SWAP%(CVT$%(S$)) &
	\	Z1%(25%)=SWAP%(CVT$%(RIGHT(S$,3%))) &
		! IF DEVICE NAME EXISTS AND IS NOT LOGICAL (IE, ALREADY IN &
		! RAD50 FORM), THEN CONVERT IT TO RAD50 FORM USING THE &
		! FSS SYS() CALL. &

25830	Z%=(STATUS AND 255%)/2% &
	\ Z%=(2%^Z%) AND (Z1%(29%)>0%) AND 16383% &
	\ Z%=Z% OR NOT(32767%) IF &
		A%<>Z1%(5%) AND (A% AND -512%)<>0% &
		AND (Z1%(29%) AND 896%)=128% &
	\ Z%=Z% OR 16384% IF Z1%(26%)=255% AND (Z1%(29%)>0%) &
	\ Z1%(14%)=Z% &
		! SET UP FLAGS:	Z1%(14%) &
		!		LOW ORDER 14 BITS USED FOR BIT-ENCODED DHI; &
		!		TOP 2 BITS USED FOR FLAGS AS FOLLOWS: &
		!			-32768	ACCOUNT DOESN'T MATCH A% &
		!				AND A% IS NOT PRIV'D &
		!			16384	SPECIFIC DEVICE UNIT REQUESTED &
	&

25835	E%=130% IF (Y0% AND Z1%(29%))<>0% &
	\ E%=134% IF (Y1% AND Z1%(14%) AND 32767%)<>0% &
	\ E%=130% IF (Y1% AND Z1%(14%))<0% AND (STATUS AND 256%)=0% &
	\ GOTO 25890 IF E%<>0% &
		! CHECK FOR PROHIBITED SWITCHES, PROHIBITED DEVICES. &

25840	Z%(Z%)=0% FOR Z%=0% TO 30% &
	\ Z%(6%)=Z1%(23%) \ Z%(7%)=Z1%(25%) &
	\ Z%(8%)=Z1%(5%) &
	\ Z%(9%)=Z1%(7%) \ Z%(10%)=Z1%(9%) &
	\ Z%(11%)=Z1%(11%) &
	\ Z%(12%)=Z1%(21%) &
	\ Z%(13%)=Z1%(14%) &
	\ Z%(14%)=Z1%(29%) &
	\ Z%(16%)=Z1%(17%) AND 32767% &
	\ Z%(18%)=Z1%(15%) &
	\ Z%(19%)=Z1%(13%) &
	\ Z%(15%)=(Z1%(27%) AND 7%)*2048% &
	\ Z%=Z%(8%) &
	\ Z%(25%)=(255% AND (Z% AND 255%)<>255%) OR (-256% AND &
		(Z% AND -256%)<>-256%) &
		! SET UP ARRAY TO RETURN: &
		!	 DATA			WORD(S) &
		!	DEVICE NAME		6,7 &
		!	ACCOUNT			8 &
		!	FILENAME (2 WORDS)	9,10 &
		!	EXTENSION		11 &
		!	PROT/STATUS		12 &
		!	DHI AND FLAGS		13 &
		!	FLAG WORD 2		15 &
		!	/MODE VALUE		16 &
		!	/CLUSTERSIZE VALUE	18 &
		!	/FILESIZE VALUE		19 &
		! SET UP SELECTION SPECIFICATION WORD WITH THE &
		! MODE, CLUSTERSIZE, AND FILESIZE FLAGS AS RETURNED &
		! FROM FILENAME STRING SCAN; &
		! SET UP ACCOUNT SELECTION WORD FOR FUNNY ALGORITHM &
		! FOR ACCOUNT SELECTION. &

25850	IF P0%>-1% THEN &
		Z0%(P0%,Z%)=Z%(Z%) FOR Z%=6% TO 19% &
	\	Z0%(P0%,25%)=Z%(25%) &
		! PUT THE RESULT IN THE WORK-FILE UNLESS INSTRUCTED &
		! OTHERWISE (IE, UNLESS P0%<0%). &

25860	ON ERROR GOTO 19000 &
		! RESET ERROR TRAP. &

25870	FNEND &
		! AND WE'RE DONE. &
	&

25880	E%=ERR &
		! SOME FILENAME ERROR. &

25890	Z0%(P0%,0%)=0% UNLESS P0%<0% &
	\ FNP%=P% &
	\ RESUME 25860 &
		! SET UP ERROR NUMBER AND BEGIN POSITION OF STRING IN &
		! ERROR. &
	&
	&
	&
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

32700	IF J% THEN &
	Z$=SYS(CHR$(6%)+CHR$(22%)+CVT%$(0%)) IF (R% AND 4096%) &
	\ T$=CHR$(128%+64%)+"OFFLINE" IF (R% AND 4096%) &
	\ GOSUB 23100 IF (R% AND 4096%) &
	\ Z$=SYS(PRIV.ON$) &
	\ Z$=SYS(CHR$(6%)+CHR$(8%)+CHR$(J%)+STRING$(24%,0%)+CHR$(-1%)) IF &
			(R% AND 16384%)=0% &
	\ Z$=SYS(PRIV.OFF$) &
	\ ON ERROR GOTO 32750 &
	\ OPEN W$ FOR INPUT AS FILE 1% &
	\ Z%=Z0%(0%,5%) &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+FNU$(Z%,"",4096%,0%)) TO Z% &
	\ Z%(0%)=30% \ Z%(1%)=6% \ Z%(2%)=11% &
	\ CHANGE Z% TO Z$ &
	\ Z$=SYS(Z$) &
	\ CLOSE 1% &
		! IF WE ARE ONLINE, REMOVE OURSELVES. &
		! KILL OURSELVES IF WE ARE DETACHED. &
		! SET UP TO AND DEASSIGN THE LISTING DEVICE IF ATTACHED. &

32750	RESUME 32767 &
		! TRAP ANY ERRORS AND EXIT. &

32767	END
