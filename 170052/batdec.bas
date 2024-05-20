2!		PROGRAM		: BATDEC.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
8!
10		EXTEND
11	! &
	! &
	! &
	!		  C O P Y R I G H T &
  !	&
  !	&
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
  !*******************************************************************
20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! REG		23-OCT-86	FIX BUG CAUSED BY ERROR TRAP IN FNPRV &
	! REG		02-MAR-87	FIX CVT00$ BUG (BATRUN HB's IF RUN &
	!				 FROM DISK OTHER THAN SY:) &
	! &
	&
	&
	&

100	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
	&
	! THIS PROGRAM IS THE PROGRAM WHICH DECODES ALL OF THE INDIRECT &
	! COMMAND FILES FOR A BATCH JOB.  IT THEN CHAINS TO BATRUN, &
	! THE RUN PROGRAM, FOR ACTUAL EXECUTION. &
	&
	&
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
	&
   !	CHANNEL #		USED FOR &
!	   1			WORK FILE &
!	   3			INPUT CONTROL FILES &
!	   9			DECODE FILE &
!	  11			OUTPUT CONTROL FILE &
	&
	&
	! &
	&
	&
	&
!		V A R I A B L E S    D E F I N I T I O N S &
	&
	&
!	VARIABLE NAME		USE &
	&
!	ACC%		ACCOUNT UNDER WHICH BATCH JOB WILL RUN. &
!	AHEAD%		READ AHEAD FLAG. &
!	BP2%		BASIC-PLUS 2 FLAG. &
!	C$		GENERAL COMMAND DECODE STRING. &
!	C8$		BELL &
!	C9$		'CRLF' &
!	C0$()		COMMAND DECODE ARRAY. &
!	CCL%		CCL COMMANDS ALLOWED FLAG. &
!	CL%		LENGTH OF BATCH COMMAND STRING. &
!	CNT%		CONTINUATION LINE FLAG. &
!	COB%		COBOL FLAG. &
!	E%		ERROR VARIABLE. &
!	E0%		ENTRY TYPE. &
!	E1$		DEFAULT EXTENSION. &
!	E9%		RUNNING IN ERROR STATE FLAG. &
!	F%		FOUND FLAG. &
!	F0%		FILE SPEC SEARCH WORD. &
!	F0$		FILE SPEC STRING. &
!	FORT%		FORTRAN FLAG. &
!	H%		SCRATCH VAR. &
!	I1%		DEFAULT INPUT SWITCH. &
!	J9%		CURRENT JOB RECORD POINTER. &
!	JOBCARD%	Job card seen flag% &
!	JOBMOD%		JOB MODIFICATION RECEIVED FLAG. &
!	L$		SCRATCH STRING. &
!	L$()		DECODE MODULE CONSTANT STRING VIRTUAL ARRAY. &
!	L0%		COMMAND DISPATCH VAR. &
!	L9$		FIRST DELIMETER IN FNQ%. &
!	L9%		VALID JOB CARD RECEIVED OR LOGGED IN FLAG. &
!	LL%		COMMAND DECODE INDEX VAR. &
!	M$		BASIC DEFAULT EXTENSION. &
!	O1%		DEFAULT OUTPUT SWITCH VALUE. &
!	OBJ%		OBJECT SPEC FLAG. &
!	PKG.LOC$	DEVICE/PPN OF PACKAGE. &
!	P%		POSITION POINTER FOR COMMAND STRING. &
!	P0%		CURRENT RECORD POINTER. &
!	PP%		STACK FOR P%. &
!	Q9%		DESTINATION VOLUME RECORD POINTER. &
!	R%		PACKAGE STATUS WORD. &
!	R9$		SECOND DELIMETER FOR FNQ%. &
!	S0%		SWITCH VALUE AND SLEEP TIMER. &
!	S0$		SWITCH ARGUMENT VALUE. &
!	S6%		DEFAULT SWITCH. &
!	SECT%		NEXT SECTION OF COMMAND TO GET FROM S$(). &
!	S$()		WHERE COMMAND STRING IS STORED DURING PROCESSING &
!			THIS IS A VIRTUAL STRING. &
!	SEXT%		FILE SPEC EXTENSION INDICATOR. &
!	SRC%		SOURCE SPEC FLAG. &
!	SS1%		SWITCH VALUE. &
!	T$		TEXT TO SEND TO OPERATOR. &
!	T1%		COMMAND VS. FILESPEC. SWITCH FLAG. &
!	TFLNM$		TEMPORARY FILE NAME. &
!	Z0$()		VIRTUAL STRINGS USED AS TEMP VARIABLES. &
!	ALL Z'S		TEMPORARY LOCAL WORK VARIABLES. &
	&
	&
	&
	! &
	&
	&
!		F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&
!	NAME		LINE	USE &
!	10000			DECODE A CONTROL FILE. &
!	11500			GET NEXT LINE FROM CONTROL FILE. &
!	12000			CCL COMMAND. &
!	12100			SEQUENCE COMMAND. &
!	12200			$JOB COMMAND. &
!	12300			$EOJ COMMAND. &
!	12400			$EOD COMMAND. &
!	12450			$DATA COMMAND. &
!	12500			$PRINT COMMAND. &
!	12600			$DELETE COMMAND. &
!	12700			$DIR. &
!	12800			$COPY. &
!	12900			$CREATE. &
!	13000			$BASIC. &
!	13100			$MESSAGE. &
!	13200			$MOUNT. &
!	13300			$DISMOUNT. &
!	13400			$COBOL. &
!	13500			$SORT. &
!	13600			$FORTRAN. &
!	FND$		15700	CREATE A DEFAULT FILENAME STRING. &
!	FNE0%		15500	PUT OUT AN ERROR MESSAGE TO THE FILE. &
!	FNFI%		15900	GET A FILESPEC FROM THE COMMAND STRING. &
!	FNM		15550	REMAINDER OF N/D &
!	FNS0%		16900	MATCH A SWITCH. &
!	FNST%		16000	GET THE NEXT PIECE OF COMMAND STRING &
!				INTO C$ FROM S$(). &
!	FNT$		15600	CREATE A TIME OF DAY STRING. &
	&
	&
	! &
	&
	!	P A C K A G E    S U B R O U T I N E S &
	&
	! Line		Use &
	! 20000		INITIALIZE FOR POLLINT ROUTINES &
	! 20200		SEND A MESSAGE TO QUEMAN. &
	! 20300		ONLINE TO QUEMAN. &
	! 20340		REQUEST NEW JOB FROM QUEMAN. &
	! 20360		REQUEST NEXT PACKET FROM QUEMAN. &
	! 20420		REQUEST REQUE FROM QUEMAN. &
	! 20440		TELL QUEMAN ABOUT END OF JOB. &
	! 20460		REQUEST DEFERRAL OF JOB BY QUEMAN. &
	! 20900		REQUEST A NEW JOB. &
	! 21000		POLLING ROUTINES. &
	! 23050		ENTER RECEIVER TO OPSER. &
	! 23100		SEND ONLINE TO OPSER. &
	! 23140		DELETE LAST MESSAGE TO OPSER. &
	! 23500		SEND TO OPERATOR. &
	! 23600		STORE A MESSAGE INTO THE WORK FILE. &
	! 24010		NEWJOB PACKET RECEIVED. &
	! 24100		FILE PACKET RECEIVED. &
	! 24200		ENDJOB PACKET RECEIVED. &
	! 24300		KILL JOB RECEIVED FROM QUEMAN. &
	! &
	&
	!	P A C K A G E    F U N C T I O N S &
	&
	! Name			Lines		Use &
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
	! FNCHNGE%(S$)		27000-27020	CHANGE THE STRING S$ &
	!					INTO THE INTEGER ARRAY &
	!					Z%(), PADDING TO 30 CHRS &
	!					WITH 0'S AND CONVERT &
	!					EACH TWO BYTES TO ONE &
	!					INTEGER IN THE ODD &
	!					WORDS. &
	! FNN%			27200-27250	RETURN THE UNSIGNED &
	!					INTEGER IN THE STRING C$ &
	!					AT POSITION P%+1%. &
	! FNPUSH%		27400		PUSH A VARIABLE ONTO &
	!					THE STACK. &
	&
	! FNPOP%		27410		POP A VARIABLE FROM &
	!					THE STACK. &
	! FNUNPACK$		27300		UNPACK A STRING OUT OF &
	! 					THE WORK FILE. &
	&
	&
	&

901	DIM #1%,Z0%(32767%,63%) &
	\ DIM #1%,Z0$(32767%)=128% &
	\ DIM #1%,S$(32767%)=512% &
				! WORK-FILE, PRIMARY.
905	DIM C%(4%),C(4%)	! THESE TWO ARRAYS ARE USED IN &
				! THE 'STATUS' INTERRUPT COMMAND, &
				! BELOW. &

906	DIM #9%, C0$(63%)=16% &
	\ DIM #9%, S0$(2%,63%)=16%, L$(39%)=64% &
		! COMMAND DECODE ARRAY. &
		! SWITCH DECODE ARRAY, LITERAL CONSTANT ARRAY. &
		! THESE ARRAYS ARE DESCRIBED FULLY IN BATDCD. &

950	!	DIMENSION STATEMENTS FOR STANDARD ROUTINES
951	DIM Z%(30%)		! USED BY FNP%() FOR RETURN OF DATA
952	DIM Z1%(30%)		! USED BY FNP%() FOR TEMPORARY STORAGE &
				! OF DATA. &
	\ DIM M0%(30%)		! USED BY PRIV ROUTINES &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ UU0$=CHR$(6%)+CHR$(26%)+CHR$(0%)+CHR$(0%) &
	\ UU1$=CHR$(6%)+CHR$(26%)+CHR$(0%)+CHR$(1%) &
		! SET UP STANDARD ERROR TRAP. &

1010	I$="V10.1-A" &
	\ PRINT "BATDEC	"+I$ IF E0%=0% &
		! SET UP HEADER LINE. &
		! PRINT THE HEADER LINE PREPARATORY TO GIVING &
		! AN ERROR. &

1020	CHANGE SYS(CHR$(12%)) TO Z% &
	\ PKG.LOC$="OPSER$:" &
	\ IF Z%(3%)+SWAP%(Z%(4%))<>15%*2% THEN &
		PRINT "?BATDEC must be COMPILED" &
	\	GOTO 32767 &
		! USE LOGICAL FOR PACKAGE LOCATION &
		! ISSUE ERROR IF IT WAS NOT A COMPILED FILE. &

1030	IF E0%<>2% THEN &
		PRINT "?Illegal entry - Please 'RUN " + PKG.LOC$ + "BATCH'" &
	\	GOTO 32767 &
		! THE ONLY LEGAL ENTRY TO THIS PROGRAM IS A CHAIN ENTRY, &
		! SO GIVE AN ERROR IF THAT WASN'T THE WAY WE ENTERED. &
	&

1500	S%=-1% &
	\ CHR6$=CHR$(6%) &
	\ GOSUB 20000 &
	&
	\ BATN0%=((Z0%(Q9%,11%) AND 255%)+48%) &
	\ BATN0%= 0% IF BATN0%=303% &
	\ Z%=FNO%(Z0%(0%,35%),1%,9%,0%) &
	\ GOTO 19000 IF E% &
	\ Z%=FNO%(Z0%(0%,37%),2%,11%,0%) &
	\ IF E% THEN Z0%(J9%,31%)=Z0%(J9%,31%) OR 4% &
			\ R%=R% OR 8% &
			\ GOTO 9000 &
		! OPEN "$BATCH.DCD", IF ERROR - BATCH DIES. &
		! OPEN THE COMMAND FILE, IF ERROR - LOOP FROM MODULE &
		! TO MODULE UNTIL WE SUCCEED. &

1520	Z0%(J9%,36%)=Z0%(0%,36%) &
	\ C%(3%)=Z0%(J9%,2%) &
	\ WHILE C%(3%) &
		\ E%=Z0%(C%(3%),49%) &
		\ Z%=FNO%(C%(3%),1%,3%,Z0%(J9%,8%)) UNLESS E% &
		\ L9%=0% &
		\ GOSUB 10000 UNLESS E% &
		\ Z0%(J9%,50%)=FNE0%(RIGHT(SYS(CHR6$+CHR$(9%)+ &
			CHR$(E% AND 63%)),3%)+" - "+ &
			FNU$(C%(3%),"",-1%,0%)) IF E% &
		\ E%=0% &
		\ GOSUB 21000 &
		\ GOTO 9000 IF JOBMOD% &
		\ C%(3%)=Z0%(C%(3%),1%) &
	\ NEXT &
		! DECODE ALL CONTROL FILES. &

1530	PRINT #11%, L$(28%) &
	\ P%=1% &
	\ ACC%=511% &
	\ C$=" ["+NUM1$(SWAP%(Z0%(J9%,8%)) AND 255%)+","+ &
			NUM1$(Z0%(J9%,8%) AND 255%)+"]" &
	\ GOSUB 12200 &
	\ GOTO 9000 &
		! SET UP LOGIN FOR QUEUEING OF LOG FILE. &
		! ABORT IF THERE WAS SOME SORT OF SYNTAX OR OPEN ERROR. &
	&
	&
	&
	&
	&

2000	GOTO 9000 &
		! FATAL ERROR. &
	&

9000	Z0%(0%,11%)=FNA0%(Z0%(0%,11%)) IF Z0%(0%,11%) &
	\ P0%=Z0%(0%,13%) &
	\ IF P0% THEN &
		P$=FNU$(P0%,NULSTG$,-1%,0%) &
	\	Z0%(0%,13%)=Z0%(P0%,1%) &
	\	Z0%(P0%,1%)=0% &
	\	Z%=FNA0%(P0%) &
		! GET RID OF THE INDIRECT COMMAND LIST; &
		! SET UP THE NEXT ONE TO CHAIN TO IF ONE IS IN THE LIST. &

9020	WHILE M%(4%) &
	\	Z0%(M%(5%),1%)=M%(2%) &
	\	Z0%(M%(2%),0%)=M%(5%) IF M%(2%) &
	\	M%(2%)=M%(4%) &
	\	M%(4%)=0% &
	\ NEXT &
	\ Z0%(0%,9%)=M%(2%) &
	\ Z0%(0%,19%)=M%(0%) &
	&
	\ Z0%(0%,0%)=B0% &
	\ Z0%(0%,26%)=R% &
	\ Z0%(0%,27%)=J9% &
	\ Z0%(0%,49%)=FATAL.ERR% &
	&
	\ W$=SYS(CHR$(8%)+CVT%$(0%)+FNU$(Z0%(0%,12%),NULSTG$,-1%, &
		-1%)) IF P0% &
	\ CLOSE Z% FOR Z% = 1% TO 12% &
	\ ON ERROR GOTO 19000 &
	\ IF P0% THEN &
		CHAIN P$ LINE 31000% + (32767% + 1%) &
	  ELSE	STOP &
	\	GOTO 32767 &
	&
		! THIS LINE PLACES THE HEAD POINTERS OF THE COMPLETE &
		!  MESSAGE AND INCOMPLETE MESSAGE LISTS INTO THE WORK- &
		!  FILE FOR RETRIEVAL BY THE NEXT MODULE.  IF ANY &
		!  'UNPROCESSED' REQUESTS ARE PRESENT, THEY ARE PUT AT &
		!  THE FRONT OF THE 'COMPLETED' MESSAGES LIST. &
	&
		! REPLACE THE FREE-LIST POINTER, THE PACKAGE STATUS, AND &
		!  THE CURRENT JOB NUMBER IN THE FILE HEADER RECORD. &
	&
		! PUT THE WORK-FILE NAME INTO CORE COMMON IF A CHAIN IS &
		!  TO BE DONE; &
		! CLOSE THE FILES; &
		! IF THERE WAS A CHAIN RECORD, THEN &
		!	DO THE CHAIN; &
		! ELSE	GO DO THE FINAL CLOSE OUT. &
	&
	&
	&
	&

10000	GOSUB 21000 &
	\ GOTO 10090 IF JOBMOD% &
	\ ON ERROR GOTO 10095 &
	\ C$,Z$=NULSTG$ &
	\ E9%=0% &
	\ INPUT LINE #3%, Z0$(12%) UNLESS AHEAD% &
	\ AHEAD%=0% UNLESS AHEAD%=5% &
	\ GOSUB 11500 &
	\ C$=Z0$(4%) &
	\ C$=NULSTG$ IF (L0%=6%) OR (L0%=0%) OR (L0%=1%) &
	\ CL%=LEN(C$) &
	\ SECT%=5% &
	\ GOTO 10000 IF L9%=0% UNLESS (L0%=2%) OR (L0%=3%) &
	\ ON L0%+2% GOSUB 12000,21000,21000,12100,12200, &
			  12300,12400,12450,12500,12600, &
			  12600,12700,12800,12900,13000, &
			  13100,13200,13300,13400,13500, &
			  13600 UNLESS E9% OR L0%=0% OR L0%=1% &
	\ E9%,Z0%(J9%,50%)=FNE0%(L$(37%)) IF E9%=1% &
	\ E9%,Z0%(J9%,50%)=FNE0%(L$(36%)) IF (E9%=2%) OR &
				(P%<=CL%) UNLESS E9%=-1% &
	\ Z0%(J9%,50%)=Z0%(J9%,50%) OR E9% &
	\ GOTO 10000 UNLESS AHEAD%=5% &
	\ AHEAD%=0% &
	\ RETURN &
		! GET THE NEXT LINE FROM THE CONTROL FILE. &
		! GO DECODE THE COMMAND PART OF THE LINE. &
		! IF WE HAVEN'T HAD A VALID JOB CARD YET, GET ANOTHER &
		! LINE. &
		! GOSUB TO THE PROPER ROUTINE FOR: &
		! 	CCL,RAW DATA,NOTHING,SEQUENCE,JOB, &
		!	EOJ,EOD,DATA,PRINT,DELETE, &
		!	RUN,DIRECTORY,COPY,CREATE,BASIC, &
		!	MESSAGE,MOUNT,DISMOUNT,COBOL,SORT, &
		!	FORTRAN &
		! SYNTAX ERROR IF E9% IS SET. 1% IS INVALID SPEC FIELD, &
		! 2% IS INVALID SWITCH. &
		! LOOP BACK FOR THE NEXT LINE. EOF GETS US OUT. &

10090	ON ERROR GOTO 19000 &
	\ PRINT #11%, L$(26%) UNLESS E% &
	\ RETURN &
		! PUT OUT "????????????" AS DELIMETER OF CONTROL FILE &
		! AND RETURN. &

10095	E9%,Z0%(J9%,50%)=FNE0%("?No $JOB card found") &
			UNLESS JOBCARD% IF ERR = 11% &
	\ RESUME 10090 IF ERR=11% &
	\ E%=ERR &
	\ RESUME 10090 &
		! IF THE ERROR WAS END OF FILE, GET OUT CLEAN. &
		! IF NOT, SOME OTHER ERROR WAS ENCOUNTERED ON THE CON- &
		! TROL FILE. SET THE ERROR VARIABLE AND GET OUT. &
	&

11500	S$(1%)=NULSTG$ &
	\ C$=NULSTG$ &
	\ L0%=0% &
	\ CNT%=0% &
		! INITIALIZE THE COMMAND STRINGS AND COMMAND DISPATCHER. &

11520	P%,LL%=0% &
	\ C$,Z0$(12%)=CVT$$(Z0$(12%),1%) &
	\ LL%=(ASCII(C$)=36%) AND 1% &
	\ Z%=ASCII(RIGHT(C$,LEN(C$))) &
	\ E9%=38% IF (Z%>27%) OR (LEN(C$)>120%) UNLESS E9% &
	\ GOTO 11570 UNLESS LL% &
	\ P%=1% &
	\ C$=CVT$$(LEFT(C$,120%),4%) &
	\ Z0$(12%)=C$+C9$ IF (Z%=10%) OR (Z%=13%) OR (Z%=0%) OR (E9%) &
	\ Z0$(12%)=C$+CHR$(12%)+CHR$(10%) IF Z%=12% &
	\ Z0$(12%)=C$+"$"+C9$ IF Z%=27% &
		! GET C$ READY TO PARSE. &
		! GET Z0$(12%) READY TO PRINT TO LOG. &
		! CHECK FOR THE "$". &

11530	C$=CVT$$(C$,433%) &
	\ Z1%,Z2%,Z3%,Z4%,Z5%,Z6%,Z7%=0% &
	\ WHILE Z1%<LEN(C$) &
	\	Z1%=Z1%+1% &
	\	Z2%=ASCII(MID(C$,Z1%,1%)) &
	\	Z4%=Z4%+1% IF Z2%=40% UNLESS (Z4%<0% OR Z5% OR Z6%) &
	\	Z4%=Z4%-1% IF Z2%=41% UNLESS (Z4%<0% OR Z5% OR Z6%) &
	\	Z7%=Z7%+1% IF Z2%=91% UNLESS (Z7%<0% OR Z5% OR Z6%) &
	\	Z7%=Z7%-1% IF Z2%=93% UNLESS (Z7%<0% OR Z5% OR Z6%) &
	\	C$=CVT$$(LEFT(C$,Z1%-1%),128%) IF Z2%=33% UNLESS (Z5% OR Z6%) &
	\	C$=LEFT(C$,Z1%-1%)+CHR$(47%+128%)+RIGHT(C$,Z1%+1%) &
			IF (Z2%=47%) AND (Z5% OR Z6%) &
	\	C$=LEFT(C$,Z1%-1%)+CHR$(32%+128%)+RIGHT(C$,Z1%+1%) &
			IF (Z2%=32%) AND (Z5% OR Z6%) &
	\	C$=LEFT(C$,Z1%-1%)+RIGHT(C$,Z1%+1%) IF &
			((Z2%=34% AND Z5%=0% AND Z3%<>34%) OR &
			(Z2%=39% AND Z6%=0% AND Z3%<>39%)) AND &
			DCL%=0% &
	\	Z1%=Z1%-1% IF ((Z2%=34% AND Z5%=0% AND Z3%<>34%) OR &
			(Z2%=39% AND Z6%=0% AND Z3%<>39%)) AND &
			DCL%=0% &
	\	Z5%=Z5% XOR 1% IF Z2%=39% AND Z6%=0% AND Z3%<>39% &
	\	Z6%=Z6% XOR 1% IF Z2%=34% AND Z5%=0% AND Z3%<>34% &
	\	Z3%=Z2% &
	\ NEXT &
	\	E9%=35% IF Z5% OR Z6% UNLESS E9% &
	\	E9%=34% IF Z4% OR Z7% UNLESS E9% &
	\	GOTO 11570 IF MID(C$,2%,1%)=NULSTG$ &
		! CHECK FOR UNMATCHED ",',(). &
		! STRIP OFF OUTER QUOTES AND ALL COMMENTS. &
		! SET PARITY BIT ON "/" IF WITHIN QUOTES. &
		! GOTO 11570 IF THIS MIGHT BE A CONTINUATION LINE. &
		!	Z1%=POINTER TO CURRENT CHARACTER IN C$. &
		!	Z2%=ASCII VALUE OF CURRENT CHARACTER. &
		!	Z3%=ASCII VALUE OF THE PREVIOUS CHARACTER. &
		!	Z4%=COUNTER TO CHECK FOR MATCHING (). &
		!	Z5%=COUNTER TO CHECK FOR MATCHING ''. &
		!	Z6%=COUNTER TO CHECK FOR MATCHING "". &
		!		40%=ASCII VALUE OF (. &
		!		41%=ASCII VALUE OF ). &
		!		34%=ASCII VALUE OF ". &
		!		39%=ASCII VALUE OF '. &
		!		33%=ASCII VALUE OF  THE EXCLAMATION POINT. &
		!		47%=ASCII VALUE OF /. &
		!		32%=ASCII VALUE OF ' '. &

11550	FOR LL%=2% TO 63% &
		\ GOTO 11560 IF C0$(LL%)=NULSTG$ &
		\ GOTO 11560 &
			IF DCL%=0% OR &
			(LL%<7% OR LL%=14%) &
				IF FNR%(C0$(LL%),3%)<>0% &
	\ NEXT LL% &
		! ONLY JOB/EOJ/EOD/DATA/MESSAGE ARE &
		! TO BE HANDLED IF WE ARE IN DCL MODE &
		! TRY TO MATCH A COMMAND. &

11560	 Z%=ASCII(RIGHT(C$,P%+1%)) &
	\ LL%=-1% UNLESS (Z%=0%) OR (Z%=32%) OR (Z%=47%) OR &
				((Z%=45%) AND (LEN(C$)=P%+1%)) &
	\ LL%=-1% IF LL% > 19% &
		! IF THE NEXT CHARACTER TO SCAN IS NOT A BLANK, NULL, &
		! OR SLASH THE BEST THIS COULD BE IS A CCL COMMAND. &

11570	IF CNT%=0% THEN &
		S$(1%)=C$ &
		\ L0%=LL% &
		\ GOTO 11590 &
		! IF THE LAST LINE WAS NOT A CONTINUATION, WE ASSIGN &
		! THE STRING TO SCAN TO S$(1%), AND SET THE DISPATCH &
		! VARIABLE TO ITS PROPER VALUE. &

11580	S$(1%)=LEFT(S$(1%),LEN(S$(1%))-1%)+RIGHT(C$,3%) IF LL%=1% &
	\ P%=PP% IF LL%=1% &
	\ IF LL%<>1% THEN E9%=33% UNLESS E9% &
			\ S$(1%)=C$ &
			\ L0%=LL% &
		! CONCATENATE THE LINES IF WE EXPECTED A CONTINUATION &
		! AND GOT ONE. IF WE GOT ONE AND DIDN'T EXPECT IT, IT IS &
		! AN ERROR. &

11590	P%=P%+1% &
	\ Z$="$6 " &
	\ Z$=L$(39%) IF L0% &
	\ PRINT #11%, Z$+Z0$(12%); &
	\ E9%,Z0%(J9%,50%)=FNE0%(L$(E9%)) IF E9%>28% &
	\ CNT%=RIGHT(C$,LEN(C$))="-" &
	\ S$(1%)=CVT$$(S$(1%),16%) UNLESS CNT% &
	\ RETURN UNLESS CNT% &
	\ INPUT LINE #3%, Z0$(12%) &
	\ PP%=P%-1% &
	\ GOTO 11520 &
		! SET UP AND PRINT THE LINE INTO THE CONTROL FOR &
		! LOGGING.  CHECH FOR CONTINUATION AND GO BACK FOR &
		! MORE IF THERE IS ONE INDICATED. &
	&

12000	E9%=FNE0%(L$(32%)) UNLESS CCL% &
	\ PRINT #11%, "$14 "+RIGHT(S$(1%),2%) UNLESS E9% &
	\ P%=LEN(S$(1%))+1% &
	\ RETURN &
		! IF THE COMMAND MIGHT HAVE BEEN A CCL COMMAND AND &
		! CCL'S ARE NOT ALLOWED THEN PRINT AN ERROR MESSAGE &
		! AND GET OUT. &
	&

12100	Z%=FNE0%(L$(31%)) &
	\ RETURN &
		! SEQUENCE COMMAND IS INVALID. &
	&

12200	L9%,CCL%,DCL%=0% &
	\ JOBCARD% = -1% &
	\ Z0$(14%)="600" &
	\ Z0$(15%)="4294970000" &
	\ ACC%=Z0%(J9%,8%) UNLESS ACC%=511% &
	\ PRI%=-1% &
	\ Z0%(J9%,Z%)=Z0%(0%,Z%) FOR Z%=40% TO 41% &
		! SET UP THE DEFAULTS FOR LIMIT, CPU LIMIT, CCL, DCL COMMANDS &
		! ACCOUNT TO RUN UNDER, PRIORITY, QUEUE, AND ERROR LEVEL. &

12220	ON ERROR GOTO 12350 &
	\ O.PRJ% = (SWAP%(Z0%(J9%,8%)) AND 255%) &
	\ O.PRG% = (Z0%(J9%,8%) AND 255%) &
	\ S0%=FNS0%(1%) &
	\ GOTO 12270 UNLESS S0% &
	\ IF ABS(S0%)=3% THEN GOTO 12220 IF S0%<0% &
			\ Z$=SYS(CHR6$+CHR$(-10%)+S0$) &
			\ Z0%(J9%,51%)=SWAP%(CVT$%(RIGHT(Z$,7%))) &
			\ Z0%(J9%,52%)=SWAP%(CVT$%(RIGHT(Z$,9%))) &
			\ GOTO 12220 &
		! FETCH THE OWNER ACCOUNT NUMBER &
		! CHANGE THE NAME OF THE JOB?. &

12230	IF (ABS(S0%)=4%) OR (ABS(S0%)=25%) THEN &
		Z=VAL(S0$) UNLESS S0%<0% &
		\ Z=Z*10. IF S0%=25% &
		\ Z=Z*60. IF S0%=4% &
		\ Z=4294970000. IF Z>4294970000. OR S0%<0% &
		\ E9%=2% IF Z<0% AND S0%>0% &
		\ Z$=NUM1$(Z) &
		\ Z0$(14%)=Z$ IF ABS(S0%)=4% &
		\ LIM%=-1% IF ABS(S0%)=4% &
		\ Z0$(15%)=Z$ IF ABS(S0%)=25% &
		\ CPU%=-1% IF ABS(S0%)=25% &
		\ Z0$(14%)="4294970000" IF S0%=25% AND LIM%=0% &
		\ GOTO 12220 &
		! SET THE ELAPSED OR CPU TIME LIMIT?. &

12240	IF ABS(S0%)=22% THEN PRI%=VAL(S0$) IF S0%>0% &
			\ E9%=2% IF ABS(PRI%)>127% &
			\ GOTO 12220 &
			  IF ((PRI%>=0%) AND FNPRV%("TUNE",O.PRJ%,O.PRG%)) &
			  OR (PRI%<0%) &
			\ ON ERROR GOTO 12350 &
			\ E9%,Z0%(J9%,50%)=FNE0%(L$(30%)) &
			\ GOTO 12220 &
		! INCREASE THE PRIORITY?. &
		! IF A 'HIGH' PRIORITY IS SPECIFIED, INSURE "TUNE" PRIV &

12245	IF ABS(S0%)=5% THEN CCL%=S0%>0% &
		\ GOTO 12220 &
		! ALLOW CCL COMMANDS?. &

12250	IF ABS(S0%)=42% THEN DCL%=S0%>0% &
		\ CCL%=DCL% IF DCL% &
		\ GOTO 12220 &
		! DCL IMPLIES CCL &
		! ALLOW DCL COMMANDS?. &

12255	IF ABS(S0%)=14% THEN &
		Z0%(J9%,41%)=(Z0%(J9%,41%) AND NOT 8%) OR 4% IF S0%>0% &
		\ Z0%(J9%,41%)=(Z0%(J9%,41%) AND NOT 4%) OR 8% IF S0%<0% &
		\ GOTO 12220 &
		! DELETE/NODELETE THE LOG FILE?. &

12260	IF ABS(S0%)=23% THEN &
		Z0%(J9%,41%)=(Z0%(J9%,41%) AND NOT 2%) OR 1% IF S0%>0% &
		\ Z0%(J9%,41%)=(Z0%(J9%,41%) AND NOT 1%) OR 2% IF S0%<0% &
		\ GOTO 12220 &
		! QUEUE/NOQUEUE THE LOG FILE?. &

12265	IF ABS(S0%)<>24% THEN &
		E9%=2% &
	ELSE Z0%(J9%,40%)=2% &
		\ P%=P%-1%-LEN(S0$) &
		\ Z0%(J9%,40%)=0% IF FNR%(L$(22%),3%) &
		\ Z0%(J9%,40%)=37% IF FNR%(L$(21%),3%) IF Z0%(J9%,40%)=2% &
		\ Z0%(J9%,40%)=63% IF FNR%(L$(23%),3%) IF Z0%(J9%,40%)=2% &
		\ P%=P%+1% &
		\ E9%=2% IF Z0%(J9%,40%)=2% &
		\ GOTO 12220 &
		! SET THE ERROR LEVEL?. &

12270	IF FNFI% THEN E9%=1% IF RIGHT(C$,P%)<>NULSTG$ &
			ELSE Z$=SYS(CHR6$+CHR$(-10%)+F0$) &
			\ ACC%=SWAP%(CVT$%(RIGHT(Z$,5%))) &
			\ ACC%=Z0%(J9%,8%) UNLESS ACC% &
			\ E9%,Z0%(J9%,50%)=FNE0%(L$(29%)) UNLESS &
			    (ACC%=Z0%(J9%,8%)) OR &
			    (FNPRV%("WACNT",O.PRJ%,O.PRG%) OR &
			     (FNPRV%("GACNT",O.PRJ%,O.PRG%) AND &
			      O.PRJ%=(SWAP%(ACC%) AND 255%))) &
			\ ON ERROR GOTO 12350 &
		! CHANGE THE ACCOUNT NUMBER TO RUN UNDER?. &
		! IF	REQUESTING A NEW ACCOUNT NUMBER, &
		! THEN	WE MUST HAVE "WACNT" (World acctng priv) *OR* &
		!	WE MUST HAVE "GACNT" (Group acctng priv) &
		!	 AND A REQUEST TO THE SAME GROUP NUMBER &

12280	L9%=-1% &
	\ Z$=SYS(CHR6$+CHR$(14%)+STRING$(4%,0%)+CVT%$(SWAP%(ACC%))+CHR$(1%)) &
	\ PRINT #11%, L$(27%)+C9$+ &
			NUM1$(SWAP%(ACC%) AND 255%)+","+ &
				NUM1$(ACC% AND 255%) &
	\ PRINT #11%,	'DCL'; IF DCL% &
	\ PRINT #11%,	'RT11'; IF NOT DCL% &
	\ PRINT #11%,	C9$+ &
			"$2 "+NUM1$(Z0%(J9%,40%))+C9$+ &
			"$3 "+NUM1$(Z0%(J9%,41%))+C9$+ &
			"$4 "+Z0$(14%)+C9$+ &
			"$5 "+Z0$(15%)+C9$+ &
			"$7 "+NUM1$(PRI%) &
		! PUT THE STRING OUT TO THE CONTROL FILE. &
		! THESE ARE ALL DISPATCH CODES. THAT IS, THEY ARE &
		! NOT SENT VERBATEM TO THE PSUEDO KEYBOARD IN THE RUN &
		! MODULE, BUT RATHER CAUSE DISPATCHES TO ROUTINES &
		! WHICH CONSTRUCT STRINGS TO SEND TO THE PK. &
		! (DCL ON THE PASSWORD LINE MEANS TO TELL LOGIN TO &
		! SWITCH THE JOB TO DCL IF POSSIBLE) &
		! USE RT11 IF /DCL NOT SPECIFIED; RSX MAY NOT EXIST &

12290	ON ERROR GOTO 19000 &
	\ TFLNM$="SY:["+NUM1$(SWAP%(ACC%)AND 255%)+","+NUM1$(ACC% AND 255%) &
		+ "]BATUN"+CHR$(BATN0%) &
	\ RETURN &
		! RESET ERROR TRAP ,SET UP DEFAULT FILENAME AND RETURN. &

12300	PRINT #11%, L$(26%) &
	\ L9%=0% &
	\ RETURN &
		! SET LOGOUT DISPATCH STRING AND RETURN AFTER PRINTING. &

12350	E9%=1% IF (ERL=12270%) &
	\ E9%=2% IF (ERL=12220%) OR (ERL=12230%) OR (ERL=12240%) &
	\ E9%,Z0%(J9%,50%)=FNE0%(L$(29%)) IF (ERL=12280 AND ERR<>5%) &
	\ E9%,Z0%(J9%,50%)=FNE0%("NO SUCH ACCOUNT") IF (ERL=12280 AND ERR=5%) &
	\ RESUME 12290 &
		! TRAP ERRORS AND RETURN. &
	&

12400	PRINT #11%, L$(24%) &
	\ RETURN &
		! SET 'END OF DATA' DISPATCH CODE AND RETURN. &
	&

12450	PRINT #11%, "$15" &
	\ RETURN &
		! PRINT THE DATA INDICATOR TO THE CONTROL FILE. &
	&

12500	S$(2%)=L$(9%)+C9$+"Q "+RAD$(Z0%(J9%,51%))+RAD$(Z0%(J9%,52%)) &
	\ S$(2%)=S$(2%)+"=" UNLESS INSTR(1%,C$,"=") &
	\ E1$=".LIS" &
	\ E9%=2% IF FNS0%(1%) &
	\ WHILE NOT FNFI% &
		\ NOL%=LEN(C$)+1% &
		\ S$(2%)=S$(2%)+F0$ &
		\ S$(2%)=S$(2%)+E1$ UNLESS (INSTR(1%,F0$,".") OR &
			  INSTR(1%,RIGHT(C$,INSTR(1%,C$,F0$)+LEN(F0$)),"=")) &
			! DON'T TACK ON .LIS IF THERE'S A '.' OR IF WE'RE ON &
			!	THE LEFT OF THE '=' SIGN &
		\ SP%=P% &
		\ WHILE FNS0%(2%) &
			\ P%=SP% &
			\ Z%=INSTR(P%,C$," ") &
			\ NOL%=Z% IF (Z%) AND (Z%<NOL%) &
			\ S$(2%)=S$(2%)+MID(C$,P%,NOL%-P%) &
			\ P%=NOL% &
		\ NEXT &
		\ S$(2%)=S$(2%)+"," &
	\ NEXT &
	\ S$(2%)=LEFT(S$(2%),LEN(S$(2%))-1%)+C9$+L$(24%) &
	\ PRINT #11%, S$(2%) &
		! SET UP THE STRING TO RUN QUE$. &
		! SET UP THE DEFAULT EXTENSION. &
		! GET ALL THE FILES TO BE QUEUED AND ALL SWITCHES THAT &
		! GO WITH THEM.  PRINT THE LINE OUT TO THE CONTROL FILE. &

12590	RETURN &
	&

12600	S$(2%)=L$(8%)+C9$ IF L0%=8% &
	\ S$(2%)=L$(7%) IF L0%=9% &
	\ E9%=2% IF FNS0%(1%) &
	\ WHILE NOT FNFI% &
		\ E9%=1% IF (S$(2%)<>L$(7%)) AND (L0%=9%) &
		\ S$(2%)=S$(2%)+F0$ &
		\ WHILE FNS0%(2%) &
			\ E9%=2% &
		\ NEXT &
		\ S$(2%)=S$(2%)+"," &
	\ NEXT &
	\ E9%=1% UNLESS (INSTR(1%,S$(2%),",")) UNLESS L0%=9% &
	\ S$(2%)=LEFT(S$(2%),LEN(S$(2%))-1%) &
	\ S$(2%)=S$(2%)+"/DE"+C9$+L$(24%) IF L0%=8% &
	\ PRINT #11%,S$(2%) &
		! RUN $PIP FOR DELETE. &
		! RUN FOR RUN. &
		! GET ALL FILES FOR THE COMMAND. ALL SWITCHES ARE INVALID &
		! FOR BOTH COMMANDS. ONLY ONE FILESPEC ALLOWED FOR RUN. &
		! PRINT THE LINE TO THE CONTROL FILE. &

12690	RETURN &
	&

12700	S$(2%)=L$(10%)+C9$+CHR$(255%) &
	\ I1%=2% &
	\ O1%=10% &
	\ S6%=2% &
	\ E1$=".DIR" &
	\ GOTO 12920 &
		! RUN DIRECT FOR DIRECTORY. SET UP DEFAULT SWITCHES AND &
		! DEFAULT EXTENSION. &
		! GOTO TO COMMON ROUTINE AT 12920. &
	&
	! "$COPY" COMMAND SETUP. &

12800	S$(2%)=L$(8%)+C9$+CHR$(255%) &
	\ I1%=2% &
	\ O1%=8% &
	\ S6%=2% &
	\ E1$=NULSTG$ &
	\ GOTO 12920 &
		! RUN PIP FOR COPY. &
		! SET UP DEFAULTS AND GOTO THE COMMON ROUTINE AT 12920. &
	&
	! "$CREATE" COMMAND SETUP. &

12900	S$(2%)=L$(8%)+C9$+CHR$(255%)+"KB:" &
	\ I1%=1% &
	\ O1%=0% &
	\ S6%=0% &
	\ E1$=".DAT" &
		! RUN PIP FROM KEYBOARD FOR CREATE. &
		! SET UP DEFAULTS. COMMON ROUTINE FOLLOWS. &

12920	E9%=2% IF FNS0%(1%) &
	\ WHILE NOT FNFI% &
		\ S0%=FNS0%(2%) &
		\ S0%=S6% UNLESS S0% &
		\ SS1%=S0% &
		\ E9%=2% IF FNS0%(2%) &
		\ GOTO 12930 IF SS1%<>O1% &
		\ E9%=1% IF INSTR(1%,S$(2%),"=") &
		\ Z%=INSTR(5%,S$(2%),CHR$(255%)) &
		\ S$(2%)=LEFT(S$(2%),Z%-1%)+RIGHT(S$(2%),Z%+1%) &
		\ F0$=F0$+E1$ UNLESS INSTR(1%,F0$,".") &
		\ F0$=F0$+"=" &
		\ S$(2%)=LEFT(S$(2%),Z%-1%)+F0$+RIGHT(S$(2%),Z%) &
		\ GOTO 12940 &
		! WE SHOULDN'T HAVE ANY COMMAND SWITCHES. &
		! GET THE FILE SPECS. &
		! GET ONE SWITCH. IF IT IS THE OUTPUT SPEC SET IT UP IN &
		! THE STRING IF IT ISN'T A DUPLICATE. &

12930	E9%=2% IF SS1%<>I1% &
	\ Z%=INSTR(1%,S$(2%),"=") &
	\ Z%=LEN(S$(2%)) UNLESS Z% &
	\ E9%=1% IF Z%<LEN(S$(2%)) &
	\ S$(2%)=S$(2%)+F0$ &
		\ WHILE E9%=0% &
			\ Z%=INSTR(1%,S$(2%),"+") &
			\ GOTO 12940 IF Z%=0% &
			\ S$(2%)=LEFT(S$(2%),Z%-1%)+","+RIGHT(S$(2%),Z%+1%) &
		\ NEXT &
		! HANDLE THE INPUT SPEC HERE. &

12940	NEXT &
		! GET THE NEXT FILE SPEC. &

12990	E9%=1% IF ((INSTR(1%,S$(2%),"=")=0%) AND (L0%<>10%)) &
	\ S$(2%)=S$(2%)+C9$+L$(24%) UNLESS L0%=12% &
	\ PRINT #11%, S$(2%) &
	\ RETURN &
		! ITS AN ERROR IF THERE IS NO OUTPUT SPEC FOR COPY &
		! OR CREATE. &
		! SET UP STRING AND PRINT IT. &
	&

13000	Z0$(14%),Z0$(15%),Z0$(16%),Z0$(17%),Z0$(18%),Z0$(19%)=NULSTG$ &
	\ SRC%,OBJ%,LST%,MAP%,EXE%=0% &
	\ BP2%,RAN%,NOL%,NOC%,FORT%=0% &
	\ M$="BAS"
13015	WHILE FNS0%(1%) &
		\ SWCH%=ABS(S0%) &
		\ E9%=2% IF (BP2%) AND ((SWCH%=29%) OR (SWCH%=28%)) &
		\ Z%=0% UNLESS S0%=28% &
		\ Z%,BP2%=S0% IF (SWCH%=29%) AND (L0%<>19%) &
		\ Z%,FORT%=S0% IF (S0%=26%) AND (L0%=19%) &
		\ Z%,RAN%,OBJ%,LST%,MAP%,EXE%,NOL%,NOC%=S0% IF &
			(S0%=6%) AND (RAN%=0%) &
		\ Z%,RAN%=S0% IF (S0%=-6%) AND (RAN%=0%) &
		\ Z%,OBJ%=S0% IF (SWCH%=7%) AND (OBJ%=0%) &
		\ Z%,LST%=S0% IF (SWCH%=8%) AND (LST%=0%) &
		\ Z%,MAP%=S0% IF (SWCH%=11%) AND (MAP%=0%) &
		\ Z%,EXE%=S0% IF (SWCH%=27%) AND (EXE%=0%) &
		\ Z%,NOL%=S0% IF (S0%=-30%) AND (OBJ%>0%) AND (NOL%=0%) &
					AND (L0%<>19%) &
		\ Z%,NOC%=S0% IF (S0%=-31%) AND (OBJ%>0%) AND (NOC%=0%) &
					AND (L0%<>19%) &
		\ E9%=2% IF Z%=0% &
	\ NEXT &
	\ RETURN IF L0%=19% &
	\ GOSUB 13620 &
	\ GOTO 13090 IF RAN%>0% &
	\ BP2%=29% IF OBJ%>0% &
	\ M$="B2S" IF BP2% &
	\ Z0$(14%)="KB:" IF Z0$(14%)=NULSTG$ &
	\ Z0$(14%)=Z0$(14%)+M$ IF INSTR(1%,Z0$(14%),".")=LEN(Z0$(14%)) &
		! GET ALL THE COMMAND SWITCHES AND GOSUB TO GET THE &
		! FILESPECS. &

13020	S$(2%)=L$(11%)+C9$ IF BP2% &
	\ S$(2%)=S$(2%)+L$(19%)+Z0$(14%)+C9$ IF Z0$(14%)<>"KB:" &
	\ S$(2%)=S$(2%)+L$(20%)+Z0$(14%)+C9$ IF Z0$(14%)="KB:" &
	\ PRINT #11%, S$(2%); &
	\ S$(2%)=L$(24%)+C9$ &
	\ S$(2%)=S$(2%)+L$(17%)+Z0$(16%)+C9$ IF Z0$(16%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+L$(18%)+Z0$(18%)+C9$ IF (Z0$(15%)=NULSTG$) AND (Z0$(18%)<>NULSTG$) &
	\ S$(2%)=S$(2%)+L$(18%)+Z0$(15%)+"/OBJ" IF Z0$(15%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+L$(6%) IF NOC% &
	\ S$(2%)=S$(2%)+L$(5%) IF NOL% &
	\ S$(2%)=S$(2%)+C9$ &
	\ S$(2%)=S$(2%)+L$(12%)+C9$+Z0$(18%) IF OBJ%>0% &
	\ S$(2%)=S$(2%)+","+Z0$(17%) IF MAP%>0% &
	\ S$(2%)=S$(2%)+"="+Z0$(15%)+","+Z0$(19%)+"ASG = SY:5:6:7:8:9:10:11:12"+C9$+"//"+ &
			C9$+L$(24%)+C9$ IF OBJ%>0% &
	\ S$(2%)=S$(2%)+L$(7%)+Z0$(18%)+C9$ UNLESS RAN% &
	\ S$(2%)=S$(2%)+"$14 "+CHR$(3%)+CHR$(3%)+CHR$(3%)+C9$ &
		UNLESS BP2%=0% OR MAP%>0% OR (OBJ%>0% AND RAN%=0%) &
		! SET UP THE STRING TO SEND TO THE PSUEDO KEYBOARD. &
		! WE CONDITIONALIZE THIS STRING ACCORDING TO THE &
		! VARIABLES SET AND NOT SET. &

13080	ON ERROR GOTO 13095 &
	\ L0%=0% &
	\ SP%=P% &
	\ WHILE L0%=0% &
		\ INPUT LINE #3%, Z0$(12%) &
		\ P%=0% &
		\ Z0$(12%)=CVT$$(Z0$(12%),1%+256%) &
		\ C$=CVT$$(Z0$(12%),32%+256%) &
		\ L0%=(ASCII(C$)=36%) AND 1% &
		\ P%=1% IF L0% &
		\ PRINT #11%, "$6 "+Z0$(12%); IF L0%=0% &
	\ NEXT &
	\ AHEAD%=-1% IF L0% &
	\ IF (FNR%("EOD",3%) AND L0%)=0% THEN &
		P%=SP% &
	ELSE &
		P%=SP% &
		\ AHEAD%=5% &
		\ E9%,Z0%(J9%,50%)=FNE0%(L$(37%)) IF E9%=1% &
		\ E9%,Z0%(J9%,50%)=FNE0%(L$(36%)) IF (E9%=2%) OR &
			(P%<=CL%) UNLESS E9%=-1% &
		\ GOSUB 10000 &
		\ E9%,CL%=0% &
		\ P%=1% &
		! WHILE WE HAVE RAW DATA, GET THE LINES FROM THE CON- &
		! TROL FILE. AS SOON AS WE ENCOUNTER ANOTHER COMMAND &
		! STOP. &

13085	PRINT #11%, S$(2%); &
		! PRINT THE LINE INTO THE CONTROL FILE. &

13090	ON ERROR GOTO 19000 &
	\ RETURN &

13095	RESUME 13085 &
		! TRAP FOR LINE 13080. &
	&

13100	WAI%=FNS0%(1%) &
	\ E9%=2% IF (ABS(WAI%)<>2%) AND (WAI%) &
	\ S$(2%)="$11 " IF L0%=14% &
	\ S$(2%)="$13 " IF L0%=16% &
	\ S$(2%)=S$(2%)+"$1 " IF WAI%>0% &
	\ RETURN IF L0%=16% &
	\ S$(2%)=S$(2%)+RIGHT(S$(1%),P%+1%) &
	\ PRINT #11%,S$(2%) &
	\ P%=LEN(S$(1%))+1% &
	\ RETURN &
		! GET A COMMAND SWITCH, THE ONLY VALID ONE IS /WAIT. &
		! CONSTRUCT THE STRING AND PRINT IT TO THE &
		! CONTROL FILE. &

13200	Z0$(14%),Z0$(15%),Z0$(16%),Z0$(17%),Z0$(18%),Z0$(19%)=NULSTG$ &
	\ E9%=2% IF FNS0%(1%)<>0% &
	\ WHILE NOT FNFI% &
		\ SWCH%,SWCHS%=0% &
		\ WHILE FNS0%(2%) &
			\ SWCHS%=SWCHS%+1% &
			\ SWCH%=S0% IF (S0%=11%) AND (Z0$(15%)=NULSTG$) &
			\ Z$=NULSTG$ &
			\ Z$,Z0$(15%)=F0$ IF (S0%=11%) AND (Z0$(15%)=NULSTG$) &
			\ Z$,Z0$(14%)=F0$ IF (S0%=5%) AND (Z0$(14%)=NULSTG$) &
			\ Z$,Z0$(16%)="A" IF (S0%=6%) AND (Z0$(16%)=NULSTG$) &
			\ Z$,Z0$(16%)=L$(16%) IF (S0%=-6%) AND (Z0$(16%)=NULSTG$) &
			\ Z$,Z0$(17%)=S0$ IF (S0%=7%) AND (Z0$(17%)=NULSTG$) &
			\ Z$,Z0$(18%)=S0$ IF (S0%=19%) AND (Z0$(18%)=NULSTG$) &
			\ Z$,Z0$(19%)=S0$ IF (S0%=20%) AND (Z0$(19%)=NULSTG$) &
			\ E9%=2% IF ((SWCH%=11%) AND (SWCHS%>1%)) OR &
					(Z$=NULSTG$) &
		\ NEXT &
	\ NEXT &
	\ E9%=1% IF (Z0$(14%)=NULSTG$) OR (Z0$(15%)=NULSTG$) &
	\ Z0$(16%)=NULSTG$ IF Z0$(16%)="A" &
	\ S$(2%)="$12 "+Z0$(14%)+","+Z0$(15%)+","+Z0$(17%)+","+Z0$(16%)+"," &
	\ E9%=2% IF (Z0$(18%)<>NULSTG$) AND (((Z0$(18%)<>"200") AND (Z0$(18%)<>"556") AND &
			(Z0$(18%)<>"800") AND (Z0$(18%)<>"1600")) OR &
			(Z0$(18%)="1600") AND (Z0$(19%)<>NULSTG$)) &
	\ E9%=2% IF (Z0$(19%)<>NULSTG$) AND ((Z0$(19%)<>"ODD") AND (Z0$(19%)<>"EVEN")) &
	\ S$(2%)=S$(2%)+Z0$(18%)+","+Z0$(19%) &
	\ PRINT #11%, S$(2%) &
		! INITIALIZE THE STRING VARIABLES. &
		! NO COMMAND SWITCHES ARE VALID. &
		! GET THE FILESPECS WITH THE APPROPRIATE SWITCHES AND &
		! BUILD THE DISPATCH STRING. THE RUN MODULE WILL DECODE &
		! THIS STRING WITH EACH ARGUMENT BEING POSITIONAL. &

13290	RETURN &
	&

13300	GOSUB 13100 &
	\ WHILE NOT FNFI% &
		\ E9%=1% IF S$(2%)<>"$13 " AND S$(2%)<>"$13 $1 " &
		\ S$(2%)=S$(2%)+F0$ &
		\ E9%=2% IF FNS0%(2%) &
	\ NEXT &
	\ PRINT #11%, S$(2%) &
	\ RETURN &
		! GET A COMMAND SWITCH, ONLY /WAIT IS VALID. &
		! ONLY ONE FILESPEC IS VALID AND IT CAN'T HAVE ANY SWITCHES. &
	&

13400	P%=LEN(C$)+1% &
	\ Z0%(J9%,50%)=FNE0%('$COBOL IS NO LONGER VALID, PLEASE USE "$RUN $COBOL"') &
		! THE $COBOL COMMAND IS NO LONGER A VALID COMMAND. &
		! SET ERROR AND PRINT MESSAGE &
	&

13490	ON ERROR GOTO 19000 &
	\ RETURN &

13500	Z0$(17%),Z0$(18%)=NULSTG$ &
	\ WHILE FNS0%(1%) &
		\ SWCH%=ABS(S0%) &
		\ SWCH$="/"+LEFT(S0$(1%,SWCH%),3%) &
		\ E9%=2% IF (INSTR(1%,Z0$(17%),SWCH$)<>0%) OR &
			(INSTR(1%,Z0$(18%),SWCH$)<>0%) OR &
			((INSTR(1%,L$(4%),SWCH$)=0%) AND &
			(INSTR(1%,L$(13%),SWCH$)=0%)) OR S0%<0% &
		\ Z0$(17%)=Z0$(17%)+SWCH$ IF INSTR(1%,L$(4%),SWCH$)<>0% &
		\ Z0$(17%)=Z0$(17%)+":"+S0$ IF S0$<>NULSTG$ AND INSTR(1%,L$(4%),SWCH$)<>0% &
		\ Z0$(18%)=Z0$(18%)+SWCH$ IF INSTR(1%,L$(13%),SWCH$)<>0% &
		\ Z0$(18%)=Z0$(18%)+":"+S0$ IF S0$<>NULSTG$ AND INSTR(1%,L$(13%),SWCH$)<>0% &
	\ NEXT &
	\ Z0$(14%),Z0$(15%),Z0$(16%)=NULSTG$ &
	\ WHILE NOT FNFI% &
		\ S0%=FNS0%(2%) &
		\ SWCH%=S0% &
		\ E9%=2% IF FNS0%(2%) OR S0%<0% &
		\ S0%=SWCH% &
		\ Z$=NULSTG$ &
		\ Z$,Z0$(14%)=F0$ IF ((S0%=2%) OR (S0%=0%)) AND (Z0$(14%)=NULSTG$) &
		\ Z$,Z0$(15%)=F0$ IF (S0%=8%) AND (Z0$(15%)=NULSTG$) &
		\ Z$,Z0$(16%)=","+F0$ IF (S0%=14%) AND (Z0$(16%)=NULSTG$) &
		\ E9%=1% IF Z$=NULSTG$ UNLESS E9% &
	\ NEXT &
	\ E9%=2% IF (INSTR(1%,Z0$(17%),"/FOR")=0%) &
	\ E9%=2% IF (INSTR(1%,Z0$(17%),"/KEY")=0%) AND (Z0$(16%)=NULSTG$) &
	\ E9%=1% IF (Z0$(14%)=NULSTG$) OR (Z0$(15%)=NULSTG$) UNLESS E9% &
	\ S$(2%)=L$(0%)+C9$+Z0$(15%)+Z0$(18%)+"="+Z0$(14%)+Z0$(17%)+Z0$(16%)+ &
				C9$+L$(24%) &
	\ PRINT #11%, S$(2%) &
	\ RETURN &
		! DECODE THE COMMAND SWITCHES AND FILE SPECS FOR THE &
		! SORT COMMAND . INDICATE ERRORS FOR CONFLICTING SPECS. &
		! BUILD THE STRING AND EXIT. &
	&
	&

13600	Z0$(14%),Z0$(15%),Z0$(16%),Z0$(17%),Z0$(18%),Z0$(19%)=NULSTG$ &
	\ SRC%,OBJ%,LST%,MAP%,EXE%,FORT%,RAN%,NOL%,NOC%,BP2%=0% &
	\ GOSUB 13015 &
	\ FORT%=26% UNLESS FORT% &
		! GET ALL THE COMMAND SWITCHES FOR THE FORTRAN COMMAND. &

13620	EXE%=0% IF RAN%>0% &
	\ RETURN IF E9% AND FORT%=0% &
	\ GOTO 13690 IF E9% &
	\ WHILE NOT FNFI% &
		\ SWCHS%=0% &
		\ SWCH%=-1% &
		\ WHILE FNS0%(2%) &
			\ SWCHS%=SWCHS%+1% &
			\ SWCH%=S0% IF SWCHS%=1% &
			\ Z$=NULSTG$ \ Z%=0% &
			\ Z0$(19%)=Z0$(19%)+"," IF Z0$(19%)<>NULSTG$ AND FORT% &
				AND S0%=18% AND RAN%<=0% &
			\ Z$,Z0$(19%)=Z0$(19%)+F0$ IF (S0%=18%) AND (RAN%<=0%) &
			\ Z0$(19%)=Z0$(19%)+"/LB," IF (Z$<>NULSTG$) AND (FORT%=0%) &
			\ Z$,Z0$(14%)=F0$ IF ((S0%=3%) OR (((S0%=4%) AND &
						(FORT%=0%)) OR &
					((S0%=23%) AND (FORT%)))) &
					AND (Z0$(14%)=NULSTG$) &
			\ Z$,Z0$(18%)=F0$ IF (S0%=17%) AND (EXE%=0%) AND &
							(Z0$(18%)=NULSTG$) &
			\ Z$,Z0$(16%)=F0$ IF (S0%=12%) AND (LST%=0%) AND &
							(Z0$(16%)=NULSTG$) &
			\ Z$,Z0$(15%)=F0$ IF (S0%=9%) AND (OBJ%=0%) AND &
							(Z0$(15%)=NULSTG$) &
			\ Z$,Z0$(17%)=F0$ IF (S0%=16%) AND (MAP%=0%) AND &
							(Z0$(17%)=NULSTG$) &
			\ Z%,NOL%=S0% IF ((SWCH%=9%) AND (FORT%=0%)) AND &
					(S0%=-21%) AND (NOL%=0%) &
			\ Z%,NOC%=S0% IF ((SWCH%=9%) AND (FORT%=0%)) AND &
					(S0%=-22%) AND (NOC%=0%) &
			\ E9%=1% IF (Z$=NULSTG$) AND (Z%=0%) &
		\ NEXT &
		\ Z0$(14%)=F0$ IF (SWCHS%=0%) AND (RAN%<=0%) &
		\ Z0$(18%),Z0$(14%),Z0$(16%),Z0$(15%),Z0$(17%)=F0$ IF ((SWCHS%=0%) OR &
			(SWCH%=17%)) AND (RAN%>0%) &
	\ NEXT &
	\ SEXT%=INSTR(1%,Z0$(14%),".")-1% &
	\ Z0$(14%)=Z0$(14%)+"." IF (Z0$(14%)<>NULSTG$) AND (SEXT%=-1%) AND (FORT%=0%) &
	\ Z0$(14%)=Z0$(14%)+".FOR" IF (Z0$(14%)<>NULSTG$) AND (SEXT%=-1%) AND (FORT%) &
	\ SEXT%=INSTR(1%,Z0$(14%),".")-1% &
	\ SEXT%=-1% IF RAN%>0% &
	\ Z0$(14%)="KB:" IF (RAN%<=0%) AND (Z0$(14%)=NULSTG$) AND (FORT%) &
	\ Z0$(18%)=LEFT(Z0$(14%),SEXT%) IF (EXE%>0%) AND (SEXT%<>-1%) &
	\ Z0$(18%)=FND$ IF (Z0$(18%)=NULSTG$) AND (EXE%>0%) &
	\ Z0$(16%)=LEFT(Z0$(14%),SEXT%) IF (LST%>0%) AND (SEXT%<>-1%) &
	\ Z0$(16%)=FND$ IF (Z0$(16%)=NULSTG$) AND (LST%>0%) &
	\ Z0$(15%)=LEFT(Z0$(14%),SEXT%) IF (OBJ%>0%) AND (SEXT%<>-1%) &
	\ Z0$(15%)=FND$ IF (OBJ%>0%) AND (Z0$(15%)=NULSTG$) &
	\ Z0$(15%)=TFLNM$+".OBJ" IF ((Z0$(15%)=NULSTG$) AND ((MAP%>0%) OR &
				(Z0$(19%)+Z0$(17%)<>NULSTG$) OR FORT% )) &
	\ OBJ%=-1%*(Z0$(15%)<>NULSTG$) &
	\ Z0$(18%)=TFLNM$+".EXE<124>" IF (Z0$(18%)=NULSTG$) AND (OBJ%>0%) AND FORT% &
	\ Z0$(18%)=TFLNM$+".EXE" IF (Z0$(18%)=NULSTG$) AND (OBJ%>0%)
13625	Z0$(17%)=LEFT(Z0$(14%),SEXT%) IF (MAP%>0%) AND (SEXT%<>-1%) &
	\ Z0$(17%)=FND$ IF (MAP%>0%) AND (Z0$(17%)=NULSTG$) &
	\ MAP%=1% IF Z0$(17%)<>NULSTG$ &
	\ Z0$(19%)=Z0$(19%)+L$(2%) IF (OBJ%>0%) AND &
					(FORT%=0%) &
	\ S$(2%)=NULSTG$ &
	\ F0$=".SAV" IF FORT% &
	\ F0$=".TSK" IF (OBJ%>0%) AND (FORT%=0%) AND (BP2%) &
	\ Z0$(18%)=Z0$(18%)+F0$ UNLESS INSTR(1%,Z0$(18%),".") &
	\ S$(2%)=L$(7%)+Z0$(18%)+C9$ IF RAN%>0% &
	\ PRINT #11%,S$(2%); &
	\ S$(2%)=NULSTG$ &
	\ RETURN UNLESS FORT% &
		! THIS LINE GET ALL THE FILE SPECS AND APPLIES ALL THE &
		! DEFAULTS. IT IS USED AS A SUBROUTINE BY THE "$BASIC" &
		! COMMAND DECODE ROUTINE. &

13630	GOTO 13690 IF RAN%>0% &
	\ S$(2%)=L$(14%)+C9$+Z0$(15%) &
	\ S$(2%)=S$(2%)+","+Z0$(16%) IF Z0$(16%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+"="+Z0$(14%) &
	\ PRINT #11%, S$(2%) &
	\ S$(2%)=L$(24%)+C9$ &
	\ S$(2%)=S$(2%)+L$(15%)+C9$+Z0$(18%) IF Z0$(18%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+","+Z0$(17%) IF Z0$(17%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+"="+Z0$(15%) IF Z0$(18%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+","+Z0$(19%) IF (Z0$(18%)<>NULSTG$ AND Z0$(19%)<>NULSTG$) &
	\ S$(2%)=S$(2%)+C9$ IF Z0$(18%)<>NULSTG$ &
	\ S$(2%)=S$(2%)+L$(24%)+C9$ &
	\ S$(2%)=S$(2%)+L$(7%)+Z0$(18%) +C9$ IF RAN%=0% &
	\ GOSUB 13080 &
		! SET UP THE STRINGS TO PUT TO THE CONTROL FILE AND &
		! GOSUB TO 13080 TO READ AHEAD TO THE NEXT COMMAND. &

13690	RETURN &
	&

15500	DEF* FNE0%(M$) &
	\FNE0%=-1% &
	\ PRINT #11%, "$8 "+L$(28%)+M$ &
	\ PRINT #11%, "$8 "+FNT$+"??" &
	\ FNEND &
		! PRINT ERROR MESSAGE TO LOG &
		! -1% IS ALWAYS RETURNED &
	&

15550	DEF* FNM(N,D)=N-INT(N/D)*D &
		! REMAINDER OF N/D. &

15600	DEF* FNT$ &
	\ Z$=TIME$(0%) &
	\ FNT$=LEFT(Z$,5%)+":"+MID(NUM$(100%+FNM(TIME(0%),60.)),3%,2%)+ &
			RIGHT(Z$,6%) &
	\ FNEND &
		! TOD IN "HH:MM:SS AM" FORMAT.
15700	DEF* FND$ &
	\ Z$=FNT$ &
	\ FND$=CHR$(65%+VAL(LEFT(Z$,2%)))+ &
			MID(Z$,4%,2%)+MID(Z$,7%,2%)+MID(Z$,10%,1%) &
	\ FNEND &
		! CREATE A DEFAULT FILENAME BASED ON TOD. &
	&

15900	DEF* FNFI% &
	\ Z%=FNST% IF P%>95% &
	\FNFI%,F0%=(ASCII(RIGHT(C$,P%))<>32%) &
	\F0$=NULSTG$ &
	\ GOTO 15930 IF F0% &
	\ P%=P%+1% &
	\ Z1%=INSTR(P%,C$," ") &
	\Z1%=LEN(C$)+1% UNLESS Z1% &
	\Z2%=INSTR(P%,C$,"/") &
	\Z1%=Z2% IF (Z2%) AND (Z2%<Z1%) &
	\ F0$=MID(C$,P%,Z1%-P%) &
	\P%=Z1% &
		! THIS IS THE GET NEXT SPEC FIELD FUNCTION. &
		! SEE IF WE ARE SITTING ON A BLANK. &
		! IF WE ARE NOT FNF0% RETURNED AS -1% &
		! IF WE ARE POINT TO FIRST CHARACTER OF SPEC. &
		! LOOK FOR NEXT SPEC FIELD OR SWITCH AND POINT TO IT. &
		! ASSIGN THE SPEC FIELD TO F0$, POINT TO NEXT SPEC OR &
		! SWITCH. &

15930	FNEND &
	&

16000	DEF* FNST% &
	\ C$=RIGHT(C$,P%)+Z0$(SECT%) UNLESS SECT%>7% &
	\ P%=1% UNLESS SECT%>7% &
	\ CL%=0% UNLESS SECT%>7% &
	\ SECT%=SECT%+1% &
	\ FNEND &
		! GET THE NEXT PIECE OF STRING TO DECODE IF THERE IS &
		! SOME MORE. &
	&

16900	DEF* FNS0%(T1%) &
	\ Z%=FNST% IF P%>105% &
	\ FNS0%,Z%=MID(C$,P%,1%)="/" &
		! THIS IS A SWITCH FINDING FUNCTION. &

16910	GOTO 16960 UNLESS Z% &
	\ NEG%=-FNR%("NO",2%)+1% &
	\ FOR S0%=2% TO 63% &
		\ GOTO 16950 IF S0$(T1%,S0%)=NULSTG$ &
		\ GOTO 16920 IF FNR%(S0$(T1%,S0%),3%) &
	\ NEXT S0% &
	\ GOTO 16950 &
		! IF WE WEREN'T SITTING ON A "/" WE DON'T HAVE A SWITCH. &
		! IT IS A NEGATIVE SWITCH IF "NO" IS IN FRONT. &
		! SEARCH FOR THE SWITCH IN THE TABLES. &

16920	FNS0%,S0%=S0%*NEG% &
	\ S0$=NULSTG$ &
	\ GOTO 16950 UNLESS (FNR%(":",1%)) OR (FNR%("=",1%)) &
	\ Z1%=INSTR(P%,C$," ") &
	\ Z1%=LEN(C$)+1% UNLESS Z1% &
	\ Z2%=INSTR(P%,C$,"/") &
	\ Z1%=Z2% IF (Z2%) AND (Z2%<Z1%) &
	\ S0$=MID(C$,P%+1%,Z1%-P%-1%) &
	\ P%=Z1%-1% &
		! LOOK FOR A SWITCH ARGUMENT. &

16950	P%=P%+1% &
		! THIS TAKES CARE OF THE FNR%/BATCH INCOMPATABILITY. &

16960	FNEND &
	&

19000	! &
	&
	&
	!	U N E X P E C T E D    O R    U N R E C O V E R A B L E &
	!		E R R O R    H A N D L E R &
	&

19005	IF ERL=19990% THEN &
	    T$="FATAL ERROR WHILE NO ACCESS TO WORK-FILE: ERR=" &
	     +NUM1$(E%)+"ERL="+NUM1$(E1%) &
	\   PRINT T$ &
	\   STOP &
	!   PUT THE JOB INTO HIBERNATION WHILE THE JOB IS DETACHED &

19010	E%=ERR \ E1%=ERL \ RESUME 19020 &
		! SET UP THE ERROR VALUE AND LINE; &
		! MAKE SURE OF THE RESUME. &

19020	ON ERROR GOTO 19000 &
		! RESET ERROR TRAP. &

19990	FATAL.ERR% = ERR &
	\ Z0%(0%,50%) = ERL &
	\ Z0%(0%,51%) = 3260% &
	\ Z0%(0%,52%) = 6603% &
	\ Z$ = SYS(CHR$(6%)+CHR$(-10%) + MID(I$,2%,3%) + RIGHT(I$,6%)) &
	\ Z0%(0%,53%) = CVT$%(MID(Z$,7%,2%)) &
	\ Z0%(0%,54%) = CVT$%(MID(Z$,9%,2%)) &
	\ GOTO 2000 &
		! PACK IN FATAL ERROR DATA INTO WORK FILE. &
		!	ERROR CODE &
		!	ERROR LINE &
		!	"BAT" IN RAD$() FORM &
		!	"DEC" IN RAD$() FORM &
		!	VERSION/EDIT #'S IN RAD$() FORM &
		!	BACK TO MAIN CONTROL POINT. &
	&

20000	! Z$=SYS(CHR6$+CHR$(-21%)+CHR$(-1%)) &
		OPEN W$ FOR INPUT AS FILE 1% &
	! \	Z$=SYS(CHR6$+CHR$(-21%)+CHR$(0%)) &
	\ R%=Z0%(0%,26%) &
	\ B0%=Z0%(0%,0%) &
	\ J9%=Z0%(0%,27%) &
	\ Q9%=Z0%(0%,4%) &
	\ FATAL.ERR%=Z0%(0%,49%) &
	\ NOT32767%=NOT 32767% &
	&
	\ M%(0%)=Z0%(0%,19%) &
	\ M%(2%)=Z0%(0%,9%) &
	\ FOR Z%=0% TO 2% STEP 2% &
	\	Z0%=M%(Z%) &
	\	IF Z0% THEN &
			Z0%=Z0%(Z0%,1%) WHILE Z0%(Z0%,1%) &
	\		M%(Z%+1%)=Z0%
20030	  NEXT Z% &
	&
	\ J%=(PEEK(518%) AND 255%)/2% &
	\ J$=NUM1$(J%) \ J$="0"+J$ UNTIL LEN(J$)>1% &
	\ J1%=SWAP%(CVT$%(MID(SYS(UU1$),25%,2%))) &
	\ A%=SWAP%(CVT$%(MID(SYS(UU0$),21%,2%))) &
	\ Z$=NUM1$(SWAP%(A%) AND 255%) \ Z$=" "+Z$ UNTIL LEN(Z$)>2% &
	\	A$="["+Z$ \ Z$=NUM1$(A% AND 255%) &
	\	Z$=" "+Z$ UNTIL LEN(Z$)>2% \ A$=A$+","+Z$+"]" &
	\ K0%=PEEK(J1%) &
	\ K%=(SWAP%(PEEK(PEEK(K0%)+2%)) AND 255%) OR &
		(R% AND 16384%)=0% &
	&
	\ C8$=CHR$(7%) &
	\ C9$=CHR$(13%)+CHR$(10%) &
	\ GOSUB 23050 IF (R% AND 2048%) &
	\ GOSUB 23100 IF (R% AND 4096%) &
	\ JOBCARD% = 0% &
	&
	\ RETURN &
	&
		! DROP PRIVILEGES, (TRY TO) OPEN THE WORK-FILE, AND &
		!  REGAIN PRIVILEGES; &
		! GET THE STATUS WORD OUT OF THE WORK-FILE; &
		! GET THE (WORK-FILE) FREE-LIST POINTER, THE CURRENT JOB &
		!  POINTER, AND THE OUTPUT VOLUME POINTER INTO SOME &
		!  CONVENIENT (IN-MEMORY) VARIABLES. &
	&
		! THESE TWO LINES SET UP THE HEADS AND TAILS OF THE &
		!  INCOMPLETE MESSAGES AND COMPLETED MESSAGES LISTS. &
		!  THE HEADS OF THE LISTS ARE TAKEN DIRECTLY OUT OF THE &
		!  FILE HEADER RECORD OF THE WORK-FILE; THE TAILS MUST &
		!  BE FOUND BY A SCAN, IF THE LISTS ARE NON-EMPTY. &
	&
		! SET UP PARAMETERS FROM THE EXEC : &
		!	GET THE JOB NUMBER INTO J%; &
		!	SET UP A JOB NUMBER STRING IN J$; &
		!	GET THE ACCOUNT NUMBER OF THIS JOB OUT OF THE &
		!	 JDB OF THE JOB; &
		!	TURN THE ACCOUNT NUMBER INTO AN ACCOUNT NUMBER &
		!	 STRING; &
		!	STORE IN K0% THE ADDRESS OF THE I/O BLOCK FOR &
		!	 THIS JOB, FOR LOOKING UP SUCH THINGS AS THE &
		!	 CURRENT KEYBOARD, FOR ATTACHED/DETACHED, OR THE &
		!	 STATUS OF AN OPEN CHANNEL; &
		!	AND SET UP THE KEYBOARD NUMBER OF THIS JOB'S &
		!	 KEYBOARD, UNLESS THE STATUS FLAG SHOWS THAT &
		!	 THE JOB IS DETACHED. &
	&
		! SET UP TWO USEFUL STRING CONSTANTS : &
		!	C8$	- A ^G (<BELL>) CHARACTER, FOR &
		!		   NOTIFICATION STRINGS &
		!	C9$	- A <CR><LF> STRING FOR GENERAL USE &
		! CALL THE 'ENTER' ROUTINE IF THE JOB IS ENTERED; &
		! CALL THE 'ONLINE' ROUTINE IF THE JOB IS ONLINE TO &
		!  OPSER; &
		! THESE TWO ROUTINES, ENTER, AND ONLINE, SET &
		!  UP APPROPRIATE CONSTANTS AND STRINGS TO DO THE &
		!  REQUIRED POLLING AND COMMUNICATIONS. &
		! NO JOB CARD SEEN YET &
	&
	&
	&
	&
	&

21000	S1%,S2%,S3%=0% &
		! INIT TIMERS TO 0 TO PREVENT A SLEEP ON THE FIRST &
		! TIME THROUGH THE ROUTINE. &
		! TIMERS ARE : &
		!	S1%	- RECEIVE WITH SLEEP TIMER &
		!	S2%	- GET FROM KB: WITH SLEEP TIMER &
		!	S3%	- PLAIN OLD SLEEP TIMER, IN CASE THE &
		!			JOB IS NEITHER ATTACHED NOR A &
		!			RECEIVER. &
	&
	&

21100	TOD.=TIME(0%) &
		! STORE THE ENTRY TIME OF DAY.
21160	IF (R% AND 2048%) THEN &
		IF (PEEK(J0%) AND -256%) THEN &
			ON ERROR GOTO 19000 &
	\		M$=SYS(CHR6$+CHR$(18%)+CHR$(1%)) &
	\		N1%=ASCII(MID(M$,5%,1%))/2% &
	\		N2%=CVT$%(MID(M$,7%,2%)) &
	\		N%=ASCII(MID(M$,9%,1%)) &
	\		M$=MID(M$,10%, &
			   ((N%-1%) AND N%<21%) OR (19% AND N%>20%)) &
	\		N%=(N%<21%) &
	\		GOTO 21200 &
		! IF THIS JOB IS A RECEIVER, THEN &
		!	IF A MSG IS QUEUED, THEN &
		!		GET IT; &
		!		EXTRACT FROM IT THE JOB # (N1%), PPN &
		!		  PPN (N2%); &
		!		SET UP THE 'COMPLETED' FLAG - -1% IF THE &
		!		 MESSAGE IS COMPLETE, 0 OTHERWISE; &
		!		GO STORE IT AWAY. &

21170	IF S1%=0% THEN &
		GOTO 21400 &
	ELSE	SLEEP S1% &
	\	S1%=0% &
	\	GOTO 21160 &
		! IF NO WAIT TIMER IS SET FOR A SLEEP (S1%), THEN &
		!	GO EXIT FROM THE 'GET A MESSAGE' LOOP; &
		! ELSE	SLEEP THE ALLOTTED TIME; &
		!	RESET THE TIMER, SO NO SLEEP IS DONE NEXT TIME; &
		!	GO SEE IF ANYTHING ARRIVED WHILE YOU WERE OUT. &
	&

21200	GOTO 21400 IF N1%=0% OR (LEN(M$)=0% AND F1%>=0%) &
	\ M%=M%(0%) &
	\ M%=Z0%(M%,1%) WHILE M% AND Z0%(M%,6%)<>N1% &
	\ IF M%=0% THEN &
		M%=FNA% &
	\	Z0%(M%(1%),1%)=M% IF M%(1%) &
	\	Z0%(M%,1%)=M%(1%) &
	\	M%(0%)=M% UNLESS M%(0%) &
	\	M%(1%)=M% &
	\	Z0%(M%,6%)=N1% &
	\	Z0%(M%,7%)=N2% &
	\	N7%,Z0%(M%,12%)=ASCII(M$) &
	\	IF N7%<6% AND N7%>-1% THEN &
			M$=RIGHT(M$,2%) &
		ELSE	N7%,Z0%(M%,12%)=6% &
		! SKIP THIS ROUTINE IF NO MESSAGE; &
		! FIND THE PLACE TO INSERT IN INCOMPLETE LIST; &
		! IF NO PARTIAL MESSAGE IS FOUND FOR THIS JOB, THEN &
		!	SET UP AND LINK IN A NEW MESSAGE RECORD; &
		!	SET UP THE DISPATCH CODE : &
		!	IF THE FIRST CHR IS ALREADY A DISPATCH CODE, &
		!	 THEN &
		!		REMOVE IT FROM THE RECORD; &
		!	ELSE	SET UP A DISPATCH OF 6, THE DECODE &
		!		 DISPATCH. &

21220	Z0%(M%,1%) = FNPUSH%(Z0%(M%,1%)) &
	\ Z0%(M%,1%)=Z0%(M%,2%) &
	\ M% = FNPUSH%(M%) &
	\ M0%=Z0%(M%,13%) &
	\ M1%=28% &
	\ WHILE M0%>128%-M1% &
	\	M0%=M0%-(128%-M1%) &
	\	M1%=12% &
	\	M%=Z0%(M%,1%) &
	\ NEXT &
	\ M1%=M0%+M1% &
	\ M2%=LEN(M$) &
	\ M3%=0% &

21230	Z$=Z0$(M%) &
	\ Z0$(M%)=Z$+STRING$(M1%-LEN(Z$),0%)+MID(M$,M3%+1%,128%-M1%) &
	\ M3%=M3%+128%-M1% &
	\ IF M3%<M2% THEN &
		M%,Z0%(M%,1%)=FNA% &
	\	M1%=12% &
	\	GOTO 21230 &

21240	M% = FNPOP% &
	\ Z0%(M%,13%)=Z0%(M%,13%)+LEN(M$) &
	\ Z0%(M%,2%)=Z0%(M%,1%) &
	\ Z0%(M%,1%) = FNPOP% &
	\ IF N% THEN &
		M9%=0% \ M8%=2% \ GOSUB 21300 &

21250	GOTO 21400 &
	&

21300	M0%=Z0%(M%,0%) &
	\ M1%=Z0%(M%,1%) &
	\ Z0%(M%,1%)=0% &
	\ Z0%(M0%,1%)=M1% IF M0% &
	\ Z0%(M1%,0%)=M0% IF M1% &
	\ M%(M9%)=M1% UNLESS M0% &
	\ M%(M9%+1%)=M0% UNLESS M1% &
	&
	\ WHILE M8% &
	\	M0%=M%(M8%+1%) &
	\	Z0%(M%,0%)=M0% &
	\	Z0%(M0%,1%)=M% IF M0% &
	\	M%(M8%+1%)=M% &
	\	M%(M8%)=M% UNLESS M0% &
	\	M8%=0% &
	\ NEXT &
	\ RETURN &
	&

21350	T$="Illegal command" &
	\ E%=-1% &
	\ RETURN &
		! IF THE COMMAND ENTERED WAS A RECOGNIZABLE COMMAND, BUT &
		!  WAS NOT LEGAL FOR THIS PROGRAM, THEN &
		!	SET UP AND ISSUE A MESSAGE TO THAT EFFECT; &
		!	EXIT. &
	&
	&

21400	WHILE M%(2%) &
	\	M%=M%(2%) &
	\	P% = Z0%(M%,11%) &
	\	N7%=Z0%(M%,12%) &
	\	C$=FNUNPACK$(M%,26%) &
	\	E%=0% &
	\	ON N7% GOSUB &
			23600,23600,24300,23600,23600,21600,23600,21750, &
			21350,21350,21350,23600,21350,23600,21350,23600, &
			23600,23600,21740,21350,21350,21350,21350,21350, &
			21350,21350,21350,21350,21350,23600 &
				IF N7%>0% AND N7%<31% &
	\	WHILE E% AND M%<>0% &
	\		T$="%"+T$+C9$+"'"+C$+"' Ignored" &
	\		GOSUB 23500 &
	\		E%=0% &
	\	NEXT &
	\	WHILE M% &
	\		M9%=2% \ M8%=0% \ GOSUB 21300 &
	\		Z0%(M%,1%)=0% \ M%=FNA0%(M%) &
	\	NEXT &
	\ NEXT &
	&
	\ IF S3% THEN &
		SLEEP S3% &
	\	S3%=0% &
	\	GOTO 21160 &
	&
		! WHILE THERE ARE ANY COMPLETE MESSAGES : &
		!	SET UP THE MESSAGE NUMBER IN M%; &
		!	DISPATCH TO THE PROPER ROUTINE FOR THE MESSAGE'S &
		!	 TOKEN : &
		!	1   24010	R	NEWJOB FROM QUEMAN &
		!	2   24200	R	ENDJOB FROM QUEMAN &
		!	3   24300	P	KILLJOB FROM QUEMAN &
		!	4   24100	R	NEXT (FILE) PACKET FROM &
		!				 QUEMAN &
		!	5		S	ONLINE REQUEST FROM &
		!				 QUEMAN &
		!	6   21600	P	COMMAND DECODE &
		!	7   21890	P	ONLINE FROM KB:/OPSER &
		!	8   21750	P	OFFLINE FROM OPSER &
		!	9   22420	P	PAUSE &
		!	10  22430	P	CONTINUE &
		!	11  21940	R	TRANSFER THE LOG FILE &
		!	12       	R	INSERT A NOTICE TO THE &
		!				 OUTPUT LOG &
		!	13  22000	P	RETYPE LAST MESSAGE &
		!	14  22100	P	RETURN A STATUS REPORT &
		!	15		R	DISALLOWED LEGAL COMMAND &
		!	16  21700	S	ENTER STEP MODE OR STEP &
		!				 JOB &
		!	17  21720	S	EXIT STEP MODE &
		!	18  21730	S	END OPERATIONS (IE, KILL &
		!				 YOURSELF) &
		!	19  21740	P	ABORT THE CURRENT &
		!				 PROCESS &
		!	20  22300	S	CHANGE FORMS &
		!	21  22400	P	RESTART THE CURRENT &
		!				 PROCESS &
		!	22  21760	P	REQUE THE CURRENT &
		!				 PROCESS &
		!	23  21800	R	SEND THE SPECIFIED &
		!				 FILE(S) NEXT &
		!	24  21800	R	INTERRUPT THE CURRENT &
		!				 PROCESS IN ORDER TO &
		!				 SEND THE SPECIFIED &
		!				 FILE(S) &
		!	25		R	CHANGE THE OUTPUT &
		!				 FILE(S) FOR PRINT OR &
		!				 PUNCH OUTPUT &
		!	26  21770	P	RESPONSE TO BE RETURNED &
		!				 TO CALLER &
		!	27  21900	R	TAKE THE JOB OFF LINE &
		!				 FROM OPSER &
		!	28  21910	R	PUT THE JOB ONLINE TO &
		!				 QUEMAN &
		!	29  21920	R	TAKE THE JOB OFF LINE &
		!				 FROM QUEMAN &
		!	30  		R	DETACH THE JOB &
		!	IF ANY ERROR IS SET UPON RETURN, THEN &
		!		SET UP AND PRINT AN ERROR MESSAGE; &
		!	IF, UPON RETURN FROM THE PROCESS, THE MESSAGE &
		!	 POINTER IS NON-ZERO, THEN &
		!		REMOVE THE MESSAGE FROM THE COMPLETE &
		!		 LIST AND REPLACE IT IN THE FREE-LIST; &
		! LOOP UNTIL NO MORE COMPLETED MESSAGES EXIST. &
		! &
		! IN THE ABOVE TABLE, THE ANNOTATION 'S' MEANS THAT ANY &
		!  MESSAGE OF THIS FORM IS STORED IN THE WORK-FILE BY &
		!  PROGRAM FOR LATER PROCESSING BY SOME OTHER MODULE OF &
		!  THE PACKAGE; THE 'P' MEANS THAT THIS MESSAGE TYPE IS &
		!  AT LEAST PARTIALLY PROCESSED BY THIS MODULE OF THE &
		!  PACKAGE; THE 'R' MEANS THAT THE COMMAND IS NOT LEGAL &
		!  FOR THIS PACKAGE, AND HAS BEEN REMOVED FROM THE LEGAL &
		!  COMMAND LIST. &
		! &
		! IF THE 'SLEEP TIMER' (S3%) IS NON-ZERO, THEN &
		!	GO INTO A SLEEP FOR MORE DATA; &
		!	RESET SLEEP TIMER SO NO SLEEP IS DONE NEXT TIME; &
		!	GO SEE IF ANYTHING ARRIVED WHILE YOU WERE OUT. &
	&
	&

21500	TOD.=TIME(0%)-TOD. &
	\ TOD.=TOD.+86400. IF TOD.<0. &
	\ IF TOD.>65535. THEN &
		T0%=S0% &
	  ELSE	T0%=TOD.-32768. &
	\	T0%=T0% EQV 32767% &
		! STORE THE ELAPSED EXECUTION TIME; &
		! ADJUST FOR DATE WRAP-AROUND, IF NECESSARY; &
		! IF THE ELAPSED TIME IS GREATER THAN WHAT CAN BE HELD &
		! IN AN INTEGER, THEN &
		!	SET THE ELAPSED TIME TO THE REQUESTED TIME; &
		! ELSE	PUT IT IN AN INTEGER, ADJUSTING FOR THE SIGN &
		!	BIT.
21520	F2%=F2% AND -2% &
	\ T0%=S0%-T0% &
	\ F2%=F2% OR 1% IF T0%=0% OR (T0%<0% AND S0%>-1%) &
	\ T0%=0% IF (F2% AND 1%) &
	\ S0%=T0% &
		! RESET THE 'SLEEP TIME EXPIRED' FLAG; &
		! CALCULATE THE NEW 'SLEEP TIME' VALUE; &
		! SET THE 'SLEEP TIME EXPIRED' FLAG IF EITHER &
		!  THE NEW SLEEP TIME IS 0 &
		!	OR &
		!  THE OLD SLEEP TIME WAS POSITIVE AND THE &
		!  NEW SLEEP TIME IS NEGATIVE (IE, IT WENT THROUGH &
		!  A TRANSITION THROUGH 0); &
		! SET THE NEW SLEEP TIME TO 0 IF THE SLEEP TIME IS &
		!  EXPIRED. &

21530	S1%,S2%,S3%=0% &
	\ IF F1%=0% OR (F1% AND F2%) THEN &
		R%=R% AND -1025% &
	  ELSE	S0%=-1% IF S0%=0% &
	\	S1%=S0% IF (R% AND 4096%) &
	\	S2%=S0% IF (R% AND 20480%)=16384% &
	\	S3%=S0% IF (R% AND 20480%)=0% &
	\	GOTO 21100 &
		! GO MAKE SURE A NEW PROMPT IS PRINTED BEFORE CHECKING &
		!  FOR EXIT; &
		! IF THE EVENT REQUEST FLAG IS NON-ZERO AND NONE OF THE &
		!  EVENTS MATCH, THEN &
		!	ZERO OUT THE TIMERS, TO START WITH; &
		!	SET THE REQUESTED TIME TO INFINITE, IF IT IS NOT &
		!	 SET (IF THE EXPIRATION OF THE TIMER WAS A &
		!	 DESIRED EVENT, THEN THE EVENT FLAG SHOULD HAVE &
		!	 BEEN SET, AND THIS LINE WOULD NOT HAVE BEEN &
		!	 REACHED); &
		!	SET UP THE PROPER TIMER : &
		!		RECEIVE TIMER (S1%) IF THE PROGRAM IS &
		!		 ONLINE TO OPSER OR QUEMAN; &
		!		KEYBOARD TIMER (S2%) IF THE PROGRAM IS &
		!		 ATTACHED AND NOT ONLINE; &
		!		SLEEP TIMER (S3%) IF THE PROGRAM IS &
		!		 NEITHER ONLINE OR ATTACHED; &
		!	GO BACK AND LOOK FOR ANOTHER MESSAGE. &

21540	ON ERROR GOTO 19000 &
	\ R% = R% AND NOT 1536% &
	&
	\ RETURN &
	&
		! RESET ERROR TRAP. &
	&
		! AND EXIT. &
	&
	&

21600	Z%=0% &
	\ IF C$<>NULSTG$ OR (F1%<0% AND (F1% AND F2%)>=0%) THEN &
		RESTORE \ Z$=NULSTG$ \ READ Z$ UNTIL Z$="*STARTCOM" &
	\	P%=0% &
	\	WHILE P%=0% AND T$<>"*ENDCOM" &
	\		READ T$,N7% &
	\		Z%=FNR%(T$,N7%) &
	\		P%=0% UNLESS Z% &
	\		READ Z% &
	\	NEXT &
	\	IF P%=0% THEN &
			IF F1%<0% AND (F1% AND F2%)>=0% THEN &
				Z%=26% &
			ELSE	T$="Unrecognized command" &
	\			E%=-1% &
	\			RETURN &
		! IF THE STRING IS NON-NULL OR THE CALLER IS WAITING &
		!  FOR A RESPONSE, THEN &
		!	GET TO THE BEGINNING OF THE COMMAND TABLE; &
		!	WHILE A MATCH IS NOT FOUND AND THE TABLE IS NOT &
		!	 EMPTY : &
		!		READ THE NEXT ENTRY; &
		!		SEE IF THIS ONE MATCHES; &
		!		READ THE ROUTINE NUMBER; &
		!	LOOP ON NO MATCH; &
		!	IF NO MATCH WAS FOUND, THEN &
		!		IF A RESPONSE IS EXPECTED, THEN &
		!			ASSUME THAT THIS IS IT, AND SET &
		!			 UP TO RETURN IT; &
		!		ELSE	SET UP AN ERROR MESSAGE AND SET &
		!			 ERROR FLAG; &
		!			EXIT THE ROUTINE WITH THE &
		!			 MESSAGE POINTER STILL POINTING &
		!			 TO THIS ONE, SO IT WILL BE &
		!			 REMOVED. &

21620	Z0%(M%,12%)=Z% \ Z0%(M%,11%) = P% \ M%=0% &
	&
	\ RETURN &
	&
		! PUT THE NEW DISPATCH TOKEN INTO THE MESSAGE RECORD; &
		! RESET THE MESSAGE POINTER SO THAT THE MESSAGE IS &
		!  LEFT IN THE MESSAGE COMPLETED LIST, IF NO ERROR &
		!  OCCURRED. &
	&
		! AND EXIT. &
	&
	&

21650	DATA	"*STARTCOM", &
		ONLINE,		3,	7, &
		OFFLINE,	3,	8, &
		NOTICE,		3,	12, &
		STATUS,		3,	14, &
		STEP,		3,	16, &
		NOSTEP,		5,	17, &
		END,		3,	18, &
		ABORT,		3,	19, &
		FORM,		3,	20, &
		DETACH,		3,	30 &
	&

21660	DATA "*ENDCOM",0,0,
21661		! DEFINE THE COMMANDS AND THEIR ROUTINES : &
		!	THE FORMAT IS : &
		!	  <COMMAND NAME>,<MATCH LENGTH>,<ROUTINE NUMBER> &

21740	GO TO 24970	IF (R% AND 1%) = 0% &
	\	R0%, Z0%(J9%,31%) = R0% OR 1% &
	\	F2%=F2% OR 16% &
	\	JOBMOD%=-1% &
	\	GOTO 24970 &
		! 'ABORT' - &
		! IF NO JOB IS IN PROCESS, THEN &
		!	IGNORE MESSAGE; &
		! ELSE	SET THE 'ABORT REQUESTED' FLAG ON THE JOB'S &
		!	 PROCESSES REQUESTED WORD; &
		!	SET 'SOME JOB MODIFICATION REQUESTED' STATUS; &
		!	EXIT. &

21750	GOSUB 21740 IF (R% AND 1%) &
	\ GOTO 23600 &
		! 'OFFLINE' - &
		! CALL THE 'END' ROUTINE TO SET UP AN 'END'; &
		! CALL THE 'ABORT' ROUTINE IF A JOB IS ACTIVE; &
		! EXIT. &
	&

23050	J0%=SWAP%(CVT$%(MID(SYS(UU1$),29%,2%))) &
	\ RETURN	IF J0% = 0% &
	\	J0% = J0% + 14% &
	\	R%=R% OR 2048% &
	\	RETURN &
		! TAKE THE ADDRESS OF THE MESSAGE RECEIVER ENTRY OUT OF &
		!  JDB2; &
		! IF THE ADDRESS IS 0, THEN &
		!	WE FAILED TO ENTER OURSELVES, SO GO EXIT; &
		! ELSE	SET J0% TO POINT TO THE PLACE IN THE MESSAGE &
		!	 RECEIVER ENTRY WHEREIN IS STORED THE COUNT OF &
		!	 MESSAGES QUEUED FOR THIS JOB (THE COUNT IS IN &
		!	 THE HIGH BYTE OF THIS WORD); &
		!	SET THE 'ENTERED AS RECEIVER' FLAG; &
		!	EXIT. &
	&
	&
	&

23100	T0$=CHR6$+CHR$(18%)+CHR$(-1%)+CHR$(0%)+ &
		MID(SYS(CHR6$+CHR$(-10%)+"OPSER"),7%,4%) &
	\ RETURN	IF (R% AND 4096%) &
	\	R% = FNPUSH%(R%) &
	\	R%=R% AND -16385% &
	\	T$=CHR$(192%)+"ONLINE 1" &
	\	GOSUB 23500 &
	\	R% = FNPOP% &
	\	RETURN &
		! FIRST, SET UP THE STRING TO BE USED IN THE SEND TO &
		!  OPSER, REGARDLESS OF WHETHER OR NOT THE JOB HAS &
		!  ALREADY DONE AN ONLINE TO OPSER OR NOT - THE &
		!  ROUTINE IS CALLED BY THE SET-UP ROUTINES TO ENSURE &
		!  THAT THE COMMUNICATION STRING IS BUILT PROPERLY; &
		! IF THE JOB HAS ALREADY DONE AN ONLINE, THEN &
		!	GOTO THE EXIT; &
		! ELSE	PUSH THE CURRENT PROGRAM STATUS AND RESET THE &
		!	 ATTACHED FLAG TO FORCE A SEND TO OPSER; &
		!	SET UP THE TEXT TO SEND AS 'ONLINE'; &
		!	GO ISSUE AN OPERATOR MESSAGE; &
		!	POP THE OLD VALUE OF THE STATUS WORD; &
		!	AND EXIT. &
		!	(IF THE SEND WAS SUCCESSFUL, THE SEND ROUTINE &
		!	HAS SET THE 'ONLINE' FLAG IN THE STATUS WORD.) &
	&
	&
	&
	&
	&

23150	R% = FNPUSH%(R%) &
	\	R%=R% AND -16385% &
	\	T$=CHR$(192%)+"DELETE #"+J$ &
	\	GOSUB 23500 &
	\	R% = FNPOP% &
	\ RETURN &
		! IF THE 'ONLINE TO OPSER' FLAG IS NOT SET, THEN &
		!	GOTO EXIT; &
		! ELSE	PUSH THE CURRENT STATUS; &
		!	FORCE A SEND TO OPSER OF 'DELETE #<JOB NUMBER>'; &
		!	RESTORE THE STATUS. &

23500	R%=R% AND -513% &
	\ R8%=0% &
	\ P% = FNPUSH%(P%) &
	\ P%=0% &
	\ IF (R% AND 1024%)=0% THEN &
		R1$=T$ &
	\	IF F1%<0% AND (F1% AND F2%)>=0% THEN &
			R%=R% OR 1536% &
	\		T$=C8$+T$ &
		! RESET THE <CR><LF> INHIBIT FLAG; &
		! SET THE RETRY COUNTER FOR THE 'SEND' ROUTINE TO 0; &
		! PUSH THE CURRENT VALUE OF P%, SINCE THE ROUTINE WILL &
		!  USE P% AS A POSITION POINTER; &
		! SET THE VALUE OF P%; &
		! IF A MESSAGE IS NOT CURRENTLY STORED, THEN &
		!	STORE THIS ONE; &
		!	IF THIS MESSAGE IS A REQUEST (IE, THE 'USER &
		!	 RESPONSE' FLAG IN THE EVENT WORD IS SET AND &
		!	 IT IS NOT SET IN THE STATUS WORD), THEN &
		!		SET THE 'MESSAGE STORED' AND &
		!		 <CR><LF> INHIBIT FLAGS; &
		!		PUT A <BELL> ON THE FRONT OF THIS &
		!		 MESSAGE TO SIGNIFY AN ACTION REQUEST. &

23520	RESUME 23550	IF (R% AND 16384%) &
	\	ON ERROR GO TO 23530 &
	\	WHILE P%<LEN(T$) &
	\		Z%=LEN(T$)-P%+1% &
	\		Z%=255% IF Z%>20% &
	\		Z$=SYS(T0$+CHR$(Z%)+RIGHT(T$,P%+1%)) &
	\		R8%=0% &
	\		P%=P%+19% &
	\	NEXT &
	\	R%=R% OR 4096% &
	\	GOTO 23560 &
		! IF THE JOB IS FLAGGED AS ATTACHED, THEN &
		!	GO TO THE 'PRINT' ROUTINE; &
		! ELSE	SET ERROR TRAP; &
		!	SEND THE MESSAGE TO OPSER, 19 BYTES AT A TIME, &
		!	 USING THE STRING, T0$, SET UP BY THE 'ONLINE' &
		!	 ROUTINE; &
		!	SET THE 'ONLINE' FLAG. &

23530	IF ERR=32% THEN &
		R8%=R8%+1% &
	\	IF R8%<60% THEN &
			SLEEP 5% &
	\		RESUME 23520 &
		! IF THE ERROR MEANT 'NO ROOM FOR MESSAGE', THEN &
		!	INCREMENT THE RETRY COUNTER; &
		!	IF THE COUNTER IS STILL LESS THAN 60, THEN &
		!		SLEEP AWHILE; &
		!		TRY AGAIN TO SEND THE MESSAGE. &

23540	RESUME 19000 &
		! IF THE ERROR WAS ANYTHING OTHER THAN 'NO ROOM', OR IF &
		!  THE RETRY COUNTER HAS EXPIRED, THEN &
		!	SET THE ATTACHED FLAG AND RESET THE ONLINE TO &
		!	 OPSER FLAG; &
		!	APPEND AN EXPLANATION TO THE MESSAGE; &
		!	SET AN ERROR; &
		!	GO PROCESS AS IF ATTACHED. &

23550	IF (R% AND 16384%) THEN &
		ON ERROR GOTO 0 &
	\	PRINT #0% IF CCPOS(0%) &
	\	PRINT #0%,T$; &
	\	PRINT #0% IF (R% AND 512%)=0% &
		! IF ATTACHED, THEN &
		!	RESET ERROR TRAP; &
		!	MAKE SURE THAT THE CARRIAGE IS AT THE LEFT &
		!	 MARGIN; &
		!	PRINT THE STRING WITHOUT <CR><LF>; &
		!	PRINT A <CR><LF> IF THE <CR><LF> INHIBIT FLAG IS &
		!	 NOT SET. &

23560	T$=NULSTG$ &
	\ P% = FNPOP% &
	\ R%=R% OR NOT32767% &
	&
	\ ON ERROR GOTO 19000 &
	&
	\ RETURN &
	&
		! NULL OUT THE TEXT STRING TO GET RID OF THE ROOM IT'S &
		!  TAKING; &
		! RESTORE THE ORIGINAL VALUE OF P%; &
		! SET THE 'PROMPT REQUIRED' FLAG. &
	&
		! RESET THE ERROR TRAP; &
	&
		! AND EXIT. &
	&

23600	M9%=2% \ M8%=4% \ GOSUB 21300 \ M%=0% \ RETURN &
		! SET UP THE CALL TO THE 'SHIFT A MESSAGE' ROUTINE : &
		!	TAKE FROM THE 'COMPLETED' LIST; &
		!	ENTER TO THE 'UNPROCESSED' LIST; &
		!	DO THE CALL, AND LET IT RETURN; &
		!	RESET THE 'CURRENT MESSAGE' POINTER; &
		! AND EXIT. &

24300	IF J9% AND Z0%(J9%,17%)=2% THEN &
		GOSUB 21740 &
	\	Z0%(J9%,31%)=Z0%(J9%,31%) OR 16% &
	\	GOTO 24970 &
		! IF A JOB IS ACTIVE AND IT CAME FROM THE QUEUE, THEN &
		!	CALL THE ABORT ROUTINE; &
		!	SET THE 'BY QUEMAN' FLAG FOR THE ABORT MESSAGE; &
		!	GO EXIT. &

24320	GOTO 23600 &
		! IF THE CURRENT JOB IS NOT FROM THE QUEUE, THEN &
		!	GO STORE THE MESSAGE AS AN UNPROCESSED MESSAGE. &
	&

24970	ON ERROR GO TO 19000 &
	\ RETURN &
	! RESET ERROR TRAP AND EXIT &

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
	  ELSE Z%=Z%+1% UNLESS Z$=NULSTG$ &
	  \ GOTO 25230 UNLESS Z$=NULSTG$ &
		! SEARCH FOR MORE MATCHING CHARACTERS. &

25240	P%=P%+Z% &
	\ FNR%,Z1%=Z% &
		! RETURN WITH P% POINTING TO THE LAST SUCCESSFULLY &
		! MATCHED CHARACTER. FUNCTION WILL RETURN AS THE NUMBER &
		! OF CHARACTERS MATCHED. &

25250	P%=Z0% UNLESS Z1% &
	\ FNEND &
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
	\ Z%=Z% AND -3201% IF (Z0%(P0%,13%) AND 1914%) &
		IF Z0%(P0%,14%)>0% &
	\ C$=NULSTG$ &
		! CREATE A WORD (Z%) WHICH HAS BITS SET FOR A FIELD ONLY &
		! IF THAT FIELD IS BOTH REQUESTED AND PRESENT IN THE &
		! DATA; &
		! IF DEVICE IS NOT DISK, DT, MT, OR LOGICAL, DO NOT &
		! RETURN FILENAME, EXTENSION; &
		! INITIALIZE THE STRING TO NULL. &
	&

25920	C$=RAD$(Z0%(P0%,6%))+RAD$(Z0%(P0%,7%))+":" IF (Z% AND 4096%) &
	\ C$=C$+"["+FNU0$(SWAP%(Z0%(P0%,8%)))+","+FNU0$(Z0%(P0%,8%))+ &
		"]" IF (Z% AND 128%) &
	\ C$=C$+RAD$(Z0%(P0%,9%))+RAD$(Z0%(P0%,10%)) IF (Z% AND 1%) &
	\ C$=C$+"."+RAD$(Z0%(P0%,11%)) IF (Z% AND 8%) &
	\ C$=C$+"<"+FNU0$(SWAP%(Z0%(P0%,12%)))+">" IF (Z% AND 3072%) &
		! SET UP Z% AS BITS SET IN BOTH THE REQUESTED ENTRY &
		! WORD AND THOSE SET IN THE ACTUAL FILENAME STRING. &
		! IF DEV: REQ/EXSTS, MAKE IT; &
		! IF [PPN] REQ/EXSTS, MAKE IT; &
		! IF FILENAME REQ/EXSTS, MAKE IT; &
		! IF .EXT REQ/EXSTS, MAKE IT; &
		! IF <PROT> REQ/EXSTS, MAKE IT. &

25930	Z0%=Z0% AND Z0%(P0%,15%) AND 14336% &
	\ IF Z0% THEN &
		C$=C$+"/CL:"+NUM$(Z0%(P0%,18%)) IF (Z0% AND 2048%) &
	\	C$=C$+"/MO:"+NUM$(Z0%(P0%,16%)) IF (Z0% AND 4096%) &
	\	C$=C$+"/FI:"+NUM$(Z0%(P0%,19%)) IF (Z0% AND 8192%) &
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

26000	DEF* FNO%(P0%,A0%,C0%,PPN%) &
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

26010	GOTO 26090 IF E% &
	\ Z0%(P0%,16%)=4096% IF A0%=8% &
	\ IF (A0% AND 4%) THEN &
		A0%=A0% AND 3% &
	\	IF (Z0%(P0%,13%) AND 137%) THEN &
			Z0%(P0%,15%)=Z0%(P0%,15%) OR 4096% &
	\		Z0%=Z0%(P0%,13%) &
	\ Z%=Z0%(P0%,16%) &
	\		Z%=2% IF (Z0% AND 1%) &
	\		Z%=Z% OR 8192% IF (Z0% AND 8%) &
	\		Z%=128% IF (Z0% AND 128%) &
	\		Z0%(P0%,16%)=Z% &
	\		A0%=3% &
		! IF THIS IS AN ATTEMPT TO SUPERSEDE A FILE ON ANOTHER &
		! ACCOUNT BY A NON-PRIVILEGED USER, GIVE HIM AN ERROR. &
		! THIS CHECK IS NECESSARY TO PREVENT A NON-PRIVILEGED &
		! USER FROM DESTROYING FILES ON OTHER ACCOUNTS TO WHICH &
		! HE HAS WRITE ACCESS. &

26012	ON ERROR GOTO 26070 &
	\ GOTO 26020	IF PPN% = 0% &
	\ PRIV.MASK$ = MID(SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(2%) &
		+CVT%$(SWAP%(PPN%))),9%,8%) &
			! DROP OUT IF WE AREN'T USING 3rd PARTY PRIVS &
			! GET THE OWNER PRIV MASK FROM DISK &

26014	DUMMY$ = SYS(CHR$(6%)+CHR$(31%)+STRING$(2%,0%) &
		+CVT%$(SWAP%(PPN%))+PRIV.MASK$) &
			! INSTALL THIRD PARTY PRIV CHECK &

26020	Z$=FNU$(P0%,NULSTG$,-1%,-1%) &
	\ Z%=(Z0%(P0%,17%) AND (Z0%(P0%,15%) AND 1024%)<>0%) &
	\ OPEN Z$ FOR INPUT AS FILE C0%, RECORDSIZE Z% IF (A0% AND 9%) &
	\ OPEN Z$ FOR OUTPUT AS FILE C0%, RECORDSIZE Z% IF A0%=2% &
	\ OPEN Z$ AS FILE C0%, RECORDSIZE Z% IF A0%=3% &
		! SET UP TO TRAP AN OPEN ERROR; &
		! SET UP FILE NAME WITH SWITCHES, SET &
		! UP RECORDSIZE; &
		! GIVE HIM THE KIND OF OPEN HE ASKED FOR: &
		!	READ ONLY	OPEN FOR INPUT &
		!	WRITE ONLY	OPEN FOR OUTPUT &
		!	READ/WRITE	OPEN &

26030	DUMMY$ = SYS(CHR$(6%)+CHR$(31%))	IF PPN% &
			! DROP THIRD PARTY PRIV CHECK &

26050	ON ERROR GOTO 19000 &
		! RECOVER PRIVILEGES; &
		! RESET ERROR TRAP. &

26060	FNEND &
		! AND EXIT. &

26070	IF	ERL = 26012% &
	THEN	PRIV.MASK$ = STRING$(8%,0%) &
		\ RESUME 26014 &
		! TRY READING THIRD PARTY PRIVS, RETURN NULLS IF ERROR &

26080	E%=ERR \ RESUME 26090 &
		! THIS IS FOR ERRORS TRAPPED IN THE OPEN. &

26090	E%=E% OR 1024% \ GOTO 26030 &
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
	\ Z0$(Z0%)=NULSTG$ &
	\ FNA%=Z0% &
		! ZERO OUT THE RETURNED ENTRY AND SET FUNCTION VALUE. &

26130	FNEND &
		! END OF FNA%. &
	&

26200	DEF* FNA0%(P0%) &

26210	Z0%,Z2%=P0% &
	\ Z0%=Z0%(Z0%,1%) WHILE Z0%(Z0%,1%) &
	\ FOR Z%=2% TO 5% &
	\	Z2%=P0% &
	\	WHILE Z2% &
	\		IF Z0%(Z2%,Z%) THEN &
				Z3%=Z0%(Z2%,Z%) &
	\			Z0%(Z2%,Z%)=0% &
	\			Z0%,Z0%(Z0%,1%)=Z3% &
	\			Z0%=Z0%(Z0%,1%) WHILE Z0%(Z0%,1%) &

26220			Z2%=Z0%(Z2%,1%) &
	\	NEXT &
	\ NEXT Z% &
	\ Z0%(Z0%,1%)=B0% &
	\ B0%=P0% &
	\ FNA0%=0% &

26230	FNEND &
	&

27200	DEF* FNN% &
	\ FNN%,F%=0% &
	! FUNCTION:	RETURN A NUMBER ROUTINE. &
	! PARAMETERS:	NONE &
	! GLOBAL &
	! VARIABLES &
	! AFFECTED:	P%	CHARACTERS POSITION POINTER. &
	!		F%	FOUND FLAG. &
	! LOCAL &
	! VARIABLES &
	! USED:		Z%	TEMPORARY POSITION POINTER. &
	!		Z0%	ASCII CHARACTER REPRESENTATION. &
	! RETURNS:	VALUE OF NUMERIC DIGITS FOLLOWING POSITION P% &
	!		IN THE STRING C$. IF NONE ARE FOUND THE FOUND FLAG &
	!		IS RETURNED AS ZERO. &
	! ERRORS:	NONE EXPECTED. &

27210	ON ERROR GOTO 27250 &
	\ Z%=P%-1% &
	\ Z0%=-1% &
		! SET LOCAL ERROR HANDLER. &
		! SET LOCAL CHARACTER POINTER. &
		! INITIALIZE ASCII DIGIT FLAG. &

27220	WHILE (Z0%>=48% AND Z0%<=57%) OR Z0%=-1% &
		\ Z%=Z%+1% &
		\ Z0%=ASCII(RIGHT(C$,Z%+1%)) &
	\ NEXT &
		! CHECK FOR DIGITS, AND EXIT HERE WITH Z%=P% OR Z% &
		! POINTING TO THE LAST DIGIT FOUND. &

27230	IF Z%<>P% THEN FNN%=VAL(MID(C$,P%+1%,Z%-P%)) &
				\ P%=Z% &
				\ F%=-1% &
		! WE HAVE FOUND A NUMBER. SET THE FUNCTION VALUE TO &
		! THE VALUE OF THE NUMBER FOUND AND SET THE FOUND FLAG. &

27240	ON ERROR GOTO 19000 &
	\ FNEND &
		! RESET THE ERROR HANDLER. &
		! CHECK TO SEE IF THERE WAS AN ERROR. &
		! RETURN. &

27250	E%=564% IF ERR/10%=5% &
	\ RESUME 27240 &
		! TRAP ERRORS IN THE VAL FUNCTION. &
	&

27300	DEF* FNUNPACK$(P0%,P1%) &
	! FUNCTION :	FNUNPACK$	UNPACK A STRING OUT OF THE WORK- &
	!				 FILE AND RETURN THE STRING IN &
	!				 THE FUNCTION VALUE. &
	! OPERANDS :	P0%		THE HEAD RECORD OF THE LIST OF &
	!				 RECORDS CONTAINING THE STRING. &
	!		P1%		THE NUMBER OF BYTES TO SKIP AT &
	!				 THE BEGINNING OF THE FIRST &
	!				 RECORD - THESE BYTES ARE &
	!				 SKIPPED IN ORDER TO LEAVE ROOM &
	!				 FOR CONTROL INFORMATION, SUCH &
	!				 SOURCE OF MESSAGE, PPN FROM &
	!				 WHICH IT CAME, DISPATCH CODE, &
	!				 ETC. &
	! RETURNS :	THE UNPACKED STRING IN THE FUNCTION VALUE. &
	&

27310	Z%=Z0%(P0%,P1%/2%) &
	\ P1%=P1%+3% &
	\ Z9%=P0% &
	\ Z8%=Z0%(P0%,1%) &
	\ Z0%(P0%,1%)=Z0%(P0%,2%) \ Z0$=NULSTG$ &

27320	Z0%=129%-P1% \ Z0%=Z% IF Z0%>Z% &
	\ Z$=RIGHT(Z0$(P0%),P1%) &
	\ Z$=Z$+STRING$(Z0%-LEN(Z$),0%) &
	\ Z%=Z%-LEN(Z$) &
	\ Z0$=Z0$+Z$ &
	\ P1%=Z0%(P0%,1%) &
	\ IF P1% THEN &
		P0%=P1% &
	\	P1%=13% &
	\	GOTO 27320 &

27330	Z0%(Z9%,1%)=Z8% &
	\ FNUNPACK$=Z0$ &

27340	FNEND &
	&

27400	DEF* FNPUSH%(VALUE%) &
	\ S%=S%+1% \ S%(S%),FNPUSH%=VALUE% &
	\ FNEND &

27410	DEF* FNPOP% &
	\ FNPOP%=S%(S%) \ S%=S%-1% &
	\ FNEND &
	&

27500	DEF* FNPRV%(PRIV$,PROJ%,PROG%) &
	\ ON ERROR GOTO 27580 &
	\ PRIV.MASK$ = MID(SYS(CHR$(6%)+CHR$(-25%)+CHR$(-1%)+CHR$(2%) &
		+CHR$(PROG%)+CHR$(PROJ%)+STRING$(24%,0%)),9%,8%) &
			! Get the owner priv mask from disk &
		! &
		! Check to see if an account currently has privileged named &
		! If privileged then return -1% &
		! Else return 0% &
		! &
		! Parameters to pass: &
		!   PRIV$:	Privilege name to check &
		!   PROJ%:	Project number for PPN to check &
		!   PROG%:	Programmer number for PPN to check &

27510	DUMMY$ = SYS(CHR$(6%)+CHR$(31%)+STRING$(2%,0%) &
		+CHR$(PROG%)+CHR$(PROJ%)+PRIV.MASK$ &
		+STRING$(26%,0%)) &
			! Install third party priv check &
	\ CHANGE SYS(CHR$(6%)+CHR$(32%)+CHR$(1%)+STRING$(3%,0%)+ &
		PRIV$) TO M0% &
			! Check if account has the specified priv &
	\ FNPRV% = (M0%(3%)=0%) &
			! Set our return bit accordingly &
	\ DUMMY$ = SYS(CHR$(6%)+CHR$(31%)+STRING$(28%,0%)) &
			! Drop third party priv check &
	\ GOTO 27599 &
			! Drop out of the function; we're done &

27580	IF	ERL=27500% &
	THEN	PRIV.MASK$ = STRING$(8%,0%) &
		\ RESUME 27510 &
		! If	We had any problem looking up the privilege mask, &
		! Then	Assume that the account has no privs at all &
		!	Return to finish up our task &

27589	ON ERROR GOTO 0 &
		! An unexpected error, so simply crash &

27599	ON ERROR GOTO 19000 &
	\ FNEND &
		! Reset the program error trap &
		! End of FNPRV%() &
	&

31000	E0%=2% &
	\ C$=SYS(CHR$(7%)) &
	\ B%=CVT$%(C$) &
	\ W$=RIGHT(C$,3%) &
	\ GOTO 1000 &
		! TAKE THE FHR AND THE WORK-FILE &
		!  NAME OUT OF CORE COMMON AND GO TO WORK. &
	&
	&
	&
	&
	&

32767	END
