2!		PROGRAM		: ANALY3.BAS
5!		VERSION		: V10.1
6!		EDIT		: B
7!		EDIT DATE	: 21-JUN-91
10	EXTEND
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
	! 9.0-14/KCG	24-Apr-85	Print real eight bit stuff. &
	! 9.1-05/DLS	28-Aug-85	Offset into satbuf was 150+32 &
	!				Should've been 150+68 for UMRs. &
	! 9.3		10-NOV-86	ADD UNDERSCORE TO NL:'S &
	! 9.4/REG	03-APR-87	EXPAND SYMBOL ARRAYS &
	! 9.4/REG	15-APR-87	ADD BN'S DECNET CHANGES &
	! 9.4/REG	18-MAY-87	EXPAND SILMOD ARRAY TO 50 ENTRIES &
	! 9.6/REG	07-APR-88	Merge FK's changes &
	! 9.6/REG	23-MAY-88	Add final CRLF to output file to &
	!				hide extraneous nulls. &
	! 9.7/REG	21-MAR-89	Enlarge SILMOD array from 50 to 255. &
	! 9.7/FEK	27-Mar-89	Enlarge DEC%() for more DECnet stuff &
	!				and enlarge annotations arrays &
	! 9.7/REG	23-Apr-89	Dump supervisor mode APRs, rework the &
	!				SATBUF dump to make it maintainable &
	! 10.0/REG	27-Mar-90	Fix JOB/FIJOB printouts &
	! 10.0/REG	04-MAY-90	Add KDSAR5,6 &
	! 10.0/REG	08-Jun-90	Fix JHEAD, FJHEAD &
	! 10.0/REG	28-JUN-90	Fix FIJOB headings &
	! 10.1/REG	11-JUL-90	Make FIJOB a byte value &
	! &

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110!	&
   !	 ANALY1 performs the message receiver and core  dump  sections &
   !	 of  the  ANALYS  crash dump analyzer program.  The core dump, &
   !	 which may contain annotations, is preceded by a list  of  all &
   !	 installed  patches  as  well  as a directory of the .SIL file &
   !	 used to extract all necessary ANALYS symbols.  If /ALPHA  was &
   !	 specified,  the  core  dump  will  be annotated, but no octal &
   !	 contents will be printed.  Each alpha line will  represent  a &
   !	 complete buffer (16 words).  If /BUFFERS was  specified,  the &
   !	 message receiver report will not  be printed, the  core  dump &
   !	 will  look  the same  as  /ALPHA, and no error report will be &
   !	 done. &
   !	&
   !	 The program also  extracts  messages  not  yet  processed  by &
   !	 ERRCPY  at  the  time of a crash and places them in an ERRDIS &
   !	 readable file (default is  ERRCRS.FIL  in  the  account  from &
   !	 which  ANALYS  was run).  Note that extended messages can not &
   !	 be extracted and are, therefore, ignored. &
   !	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !	   1			OUTPUT &
   !	   2			CRASH ERROR LOGGING FILE &
   !	   8			NULL DEVICE &
   !	   9			WORK2-FILE &
   !	  12			CRASH FILE &
	&

400	! &
	&
	&
	!	V A R I A B L E    D E F I N I T I O N S &
	&
	&
	&
   !	VARIABLE NAME		USED FOR &
   !	&
   !	A%()		UTILITY ARRAY &
   !	ACCSS%		USED IN PENDING MESSAGE ROUTINE &
   !	AD		USED IN FNPEEK FUNCTION &
   !	ADD.BLOCK%,ADD.OFFSET% USED IN FNPEEK FUNCTION &
   !	ADDR%		ADDRESS TO PEEK AT &
   !	ADDRESS%,ADDRESS1% CURRENT MEM ADDRESSES DURING DUMP &
   !	BASE.BLOCK%	USED IN FNPEEK FUNCTION &
   !	BYT.POS$	BYTE POSITION BLOCK TOT.USED$ WHERE NEXT ERROR &
   !	BYTE%()		HOLDS ASCII OF 64 CHAR DUMP CHUNK &
   !	C$		CARRIAGE RETURN/LINE FEED &
   !	C%		COUNTING VARIABLE IN DUMP ROUTINE &
   !	C0%		COUNTING VARIABLE IN DUMP ROUTINE &
   !	CC%()		STORAGE ARRAY &
   !	CF%		<>0 MEANS COULDN'T FIND ANNOTATION SYMBOL &
   !	CR.LOGFIL$	CRASH ERROR LOGGING FILE &
   !	D$		CRASH FILE I/O BUFFER &
   !	D.UMP%		CORE DUMP/NODUMP INDICATOR &
   !			0 = NO CORE DUMP &
   !			1 = REGULAR, OCTAL DUMP &
   !			2 = Regular, but not XBUF &
   !	DIAGNOSE%	FOR MAINTENANCE ONLY &
   !	DUP%		DUPLICATE LINE COUNT &
   !	E$		ERROR MESSAGE HOLDER &
   !	END.BLOCK%,END.OFFST% END OF SEGMENT &
   !	EPMM		$$EPMM AS AN UNSIGNED INTEGER &
   !	ER.COD%		ERROR CODE &
   !	ER.LOG$		NUMBER OF ERRORS LOGGED AND REPEATED FOR ERROR &
   !			TYPE J% &
   !	ER.RCV$		NUMBER OF ERRORS RECEIVED FOR ERROR TYPE &
   !			J% INCLUDING REPEATS. &
   !	ERR.TIM%	TIME ERRORS RECEIVED &
   !	FILE.NAME$()	HOLDS FILE NAMES &
   !	FPL.SEG%	FIP POOL DUMP SEGMENT NUMBER &
   !	FPLAP6%		MAPPING ADDRESS FOR FIP &
   !	FPL.ST		FIP POOL ADDRESS AS AN UNSIGNED INTEGER &
   !	FPL.END		UPPER FIP POOL LIMIT AS AN UNSIGNED INTEGER &
   !	G%,G$,G%(),G$(),G0%(),G0$(),G0%,G0$,G.FREE%,G1%,G2%,G3%,G4%, &
   !	G5%,G6%,G7%,G8%,G9%,G7$,G8$,G9$ UTILITY VARIABLES IN ANNOTATIONS &
   !	I%,I1%,I2%	UTILITY VARIABLE &
   !	I0$		INPUT FILE &
   !	J%		UTILITY VARIABLE &
   !	K%		UTILITY VARIABLE &
   !	KAR5%		HOLDS CURRENT CONTENTS THEREOF &
   !	LINE.%		CURRENT PRINTING LINE OF 64 CHAR CHUNK &
   !	MAP%()		LAYOUT OF CRASH FILE &
   !	MAX.SEG%	HIGHEST SEGMENT NUMBER, CURRENTLY = XBUF.SEG% &
   !	MSCP.SEG%	MSCP REGION DUMP SEGMENT NUMBER &
   !	OCT.ST%		PARAMETER VARIABLE &
   !	PMB%		CHECK FOR MESSAGES TO ERRCPY FLAG: &
   !			0	MEANS DON'T BOTHER &
   !			<>0	MEANS HAVE TO CHECK AND PMB% IS &
   !				THE POINTER TO THE LIST OF &
   !				PENDING MESSAGES FOR ERRCPY &
   !				(RECEIVER ID 'ERRLOG') &
   !	M0$		UTILITY STRING &
   !			USED IN CRASH ERROR LOGGING ROUTINE &
   !	O0$		OUTPUT FILE &
   !	Q%		ADDRESS TO 'PEEK' AT IN FNP% &
   !	Q$		OCTAL STRING &
   !	Q0%		FUNCTION UTILITY VARIABLE &
   !	Q9%		SIZE OF THE CRASH FILE &
   !	RECRD.ALW$	NUMBER OF ALLOWABLE ERROR RECORDS FOR ERROR TYPE J%. &
   !	RECRD.LOG$	NUMBER OF RECORDS LOGGED FOR THIS ERROR TYPE J%. &
   !	REPEAT%		ERROR REPEAT COUNT &
   !	ROOT.SEG%	MONITOR ROOT DUMP SEGMENT NUMBER &
   !	RPT%		FLAG INDICATING IF WE SHOULD CHAIN TO ERRDIS &
   !			TO APPEND THE CRASH ERROR LOG FILE REPORT &
   !	S$		STRING TO BE PRINTED, UTILTY STRING &
   !	SAT.OFF%	OFFSET INTO SATBUF &
   !	SATBUF%		POINTER TO SATBUF &
   !	START.BLOCK%	BEGINNING OF SEGMENT &
   !	STB%		SYMBOL TABLE DUMP FLAG &
   !	T$		TAB &
   !	T0$		ANLSXX.TMP &
   !	TOO.MANY%	-1 MEANS TOO MANY ANNOTATIONS - SOME POINTER WAS &
   !			 IN LIMBO - PROBABLY "FREE #x" &
   !	TOT.LOG$	TOTAL ERRORS LOGGED (INCLUDING REPEATS). &
   !	TOT.REC$	TOTAL ERRORS RECEIVED (INCLUDING REPEATS). &
   !	TOT.USED$	BLOCK NUMBER FOR START OF NEXT ERROR RECORD. &
   !			RECORD IS TO BE WRITTEN. &
   !	TOT.LIM$	LIMIT ON NUMBER OF ERROR BLOCKS IN FILE. &
   !	VN%		GATE FOR DIFFERENT VERSION PRINTOUTS &
   !	W%		UTILITY VARIABLE &
   !	W$		UTILITY STRING &
   !	WDE%		LINES PER CHUNK TO PRINT &
   !	WORD%()		32-WORD CHUNK ARRAY &
   !	XBUF.SEG%	XBUF DUMP SEGMENT NUMBER &
   !	XCON%		SYSTEM CONFIGURATION WORD &
	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&
	&
   !	FUNCTION/SUBROUTINE		USE &
   !	&
   !	FNB$			GET A 3 POSITION OCTAL STRING &
   !	FNE$			RETURN ERROR MESSAGE &
   !	FNF()			MAKE FLOATING POINT FROM INTEGER &
   !	FNG%			EXTRACT INFO FROM CORE COMMON STRING &
   !	FNO$			GET A 6 POSITION OCTAL STRING &
   !	FNP%			CORRESPONDS TO A 'PEEK' &
   !	FNPEEK%			CALLED BY FNP% - DOES THE PEEK &
   !	LINES 10700-10740	MESSAGE RECEIVERS &
   !	LINES 11100-12900	OCTAL DUMP OF CORE &
   !	LINES 13000-14010	ANNOTATION ROUTINES &
   !	LINES 14500-14700	CRASH ERROR EXTRACTION ROUTINE &
	&

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &
	&
	&

920	DIM A%(30%), BYTE%(64%), WORD%(32%) &

950	DIM #11%,	PAT%(17%),PAT$(17%)=8%, &
			SILMOD%(255%,20%), &
			SILPAT%(24%,63%), &
			G%(128%), G$(128%)=8%, &
			GX%(32767%,1%), DUPLST%(2000%,1%), GX$(15000%)=32% &
	\ DIM #10%,	M%(80%), M2%(80%), DEC%(40%), &
			CC%(30%), FILE.NAME$(5%)=32%, MAP%(10%,3%) &
		! PAT()	- INSTALLED PATCHES, E.G.,$$0301 &
		! SILMOD(,) - ENTRIES FOR SIL DIRECTORY - MAXIMUM OF 255 &
		!	(,0)=SE.NAM &
		!	(,1)=SE.NAM &
		!	(,2)=SE.IDN &
		!	(,3)=SE.IDN &
		!	(,4)=SE.BLK &
		!	(,5)=SE.STB &
		!	(,6)=SE.STN &
		!	(,7)=SE.LOD &
		!	(,8)=SE.SIZ &
		!	(,9)=SE.XFR &
		!	(,10)=SE.SZD &
		!	(,11)=SE.OVB &
		!	(,12)=SE.OVN &
		!	(,13)=SE.OFF &
		!	(,14)=0 &
		!	(,15)=0 &
		!	(,16)=VALUE FOR TERPAT,EMTPAT,FIPPAT ETC. &
		!	(,17-20)=0 &
		! SILPAT(,) - CONTENTS OF XXXPAT PATCH SPACE (64 WORDS) &
		! G%(),G$() - POINTERS AND NAMES USED FOR ANNOTATION &
		! GX%(X%,Y%) - WHERE  X% = MEMORY ADDRESS &
		!		      Y% = 0% -> INTO ANNOTATION TABLE &
		!			   1% -> INTO DUPLICATE LIST &
		! DUPLST%(X%,Y%) -    X% = NEXT FREE ENTRY &
		!		      Y% = 0% -> INTO ANNOTATION TABLE &
		!			   1% LINK -> INTO DUPLST TABLE &
		! GX$(X%)	      X% = NEXT FREE ANNOTATION &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ IF (DIAGNOSE% AND 64%) &
		THEN PRINT "Starting ANALY3 - ";DATE$(0%);TIME$(0%); &
			"CPU time";TIME(1%) &
	\	STOP IF (DIAGNOSE% AND 128%) &
	! SET UP STANDARD ERROR TRAP. &

1010	I$="V10.1-B" &
	! SET UP VERSION/EDIT #. &

1030	IF E0%<>2% THEN &
		PRINT "?Illegal Entry - Please 'RUN ANALYS'" &
	\	GOTO 32760 UNLESS DIAGNOSE% &
	! ONLY CHAINING FROM ANALYS IS ALLOWED. &

1050	CHANGE SYS(CHR$(12%)) TO A% &
	\ PKG.LOC$="["+NUM1$(A%(6%))+","+NUM1$(A%(5%))+"]" &
	\ PKG.LOC$="_"+CHR$(A%(23%))+CHR$(A%(24%))+NUM1$(A%(25%))+":"+PKG.LOC$ &
		IF A%(26%) AND 1% &
	\ IF A%(3%)+SWAP%(A%(4%))<>15%*2% THEN &
		PRINT "?ANALY3 must be compiled" &
	\	GOTO 32760 UNLESS DIAGNOSE% &
		! BUILD NAME OF DEVICE AND ACCOUNT OF LAST OPENED FILE. &
		! WE MUST BE A COMPILED FILE IF WE WANT TO BE SURE THAT &
		! THIS NAME IS REALLY OUR PACKAGE LOCATION. &

1100	OPEN WORK1.FILE$ FOR INPUT AS FILE 10%, MODE 256% &
	\ FIJBDA%=M%(1%) &
	\ FIJOB%=M%(3%) &
	\ SATBUF%=M%(9%) &
	\ JOBTBL%=M%(11%) &
	\ PATSP%=M%(23%) &
	\ SNDLST%=M2%(19%) &
	\ JCR6%=M2%(25%) &
	\ EPMM=FNF(M2%(31%)) &
	\ XTABS%=M2%(35%) &
	\ FPLAP6%=M2%(53%) &
	\ LOWAD%=M2%(59%) &
	\ DECNET%=(DEC%(1%)<>0%) &
	\ NSPLST%=DEC%(4%) &
	\ D.UMP%=CC%(1%) &
	\ FXMONS%=CC%(2%) &
	\ HAND.IDX%=CC%(3%) &
	\ OCT.ST%=CC%(4%) &
	\ WIDE%=CC%(5%) &
	\ STB%=CC%(11%) &
	\ ROOT.SEG%=CC%(12%) &
	\ FPL.SEG%=CC%(13%) &
	\ XBUF.SEG%=CC%(14%) &
	\ MAX.SEG%=CC%(15%) &
	\ FPL.ST=FNF(CC%(16%)) &
	\ FPL.END=FNF(CC%(17%)) &
	\ MSCP.SEG%=CC%(18%) &
	\ JCTRL.SEG%=CC%(19%) &
	\ V97%=CC%(20%) &
	\ JHEAD.SEG%=CC%(21%) &
	\ FJHEAD.SEG%=CC%(22%) &
	\ I0$=FILE.NAME$(1%) &
	\ O0$=FILE.NAME$(2%) &
	\ CR.LOGFIL$=FILE.NAME$(3%) &
	\ WORK2.FILE$=FILE.NAME$(5%) &
	&
	! EXTRACT INFORMATION FROM FILE NAME PASSED IN CORE COMMON. &

1400	C$=CHR$(13%)+CHR$(10%) &
	\ T$=CHR$(9%) &
	\ S$=I0$ &
	\ OPEN S$ FOR INPUT AS FILE 12%, MODE 256%+8192% &
	\ FIELD #12%, 512% AS D$ &
	\ S$=O0$ &
	\ IF HAND.IDX%=0% THEN &
		OPEN S$ FOR INPUT AS FILE 1%, MODE 2% &
	ELSE	IF HAND.IDX%=14% THEN &
			OPEN S$ FOR OUTPUT AS FILE 1%, MODE 128% &
		ELSE	OPEN S$ AS FILE 1% &
	! OPEN THE FILE BASED ON HANDLER INDEX: &
	!	0 OR 14	DISK OR TAPE -- APPEND MODE &
	!	OTHER	PLAIN OPEN &

1420	E%=0% &
	\ GOSUB 10700 &
	UNLESS ((DIAGNOSE% AND 4%)<>0%) AND ((DIAGNOSE% AND 256%)<>0%) &
	\ GOSUB 10900 &
	\ WDS%=7% &
	\ PR.OC%=-1% &
	\ GOSUB 11000 IF ((DIAGNOSE% AND 63%)=0%) OR (DIAGNOSE% AND 4%) &
	UNLESS (((DIAGNOSE% AND 4%)<>0%) AND ((DIAGNOSE% AND 256%)<>0%)) &
		OR (DIAGNOSE% AND 63%)=8% &
	\ GOSUB 12900 IF STB%<>0% AND D.UMP%<>0% &
				 UNLESS (DIAGNOSE% AND 63%)=8% &
	\ GOSUB 14500 IF PMB% &
		UNLESS (DIAGNOSE% AND 4%)<>0% AND (DIAGNOSE% AND 256%)<>0% &
	\ PRINT #1%, C$ &
	\ PRINT #1%,CHR$(12%) IF RPT% &
	\ PRINT #1% &
	\ PRINT "Exiting ANALY3 - ";DATE$(0%);TIME$(0%); &
			"CPU time";TIME(1%) IF (DIAGNOSE% AND 64%) &
	\ GOTO 32767 IF (DIAGNOSE% AND 4%) UNLESS (DIAGNOSE% AND 8%) &
	\ CLOSE 1%,2%,8%,9%,10%,11%,12% &
	\ KILL WORK1.FILE$ UNLESS DIAGNOSE% &
	\ KILL WORK2.FILE$ UNLESS ((DIAGNOSE%<>0%) OR (STB%=0%)) &
	\ KILL 'ANAL'+RIGHT(NUM1$(100%+((PEEK(518%) AND 255%)/2%)),2%)+ &
		'.TMP' UNLESS DIAGNOSE% &
	\ GOTO 32767 UNLESS RPT% &
	\ TEMP$=SYS(CHR$(8%)+"ANALY1"+O0$+"?"+CR.LOGFIL$) &
	\ S$=PKG.LOC$+"ERRDIS" &
	\ CHAIN S$ LINE 31000 &
	! PRINT MESSAGE RECEIVER REPORT UNLESS /BUFFERS SPECIFIED. &
	! SET DEFAULT ENTRIES PER LINE AND PRINT CORE DUMP RELATED STUFF. &
	! ALL DONE IF /BUFFERS SPECIFIED. &
	! INIT/BUILD CRASH ERROR LOG FILE IF 'ERRLOG' WAS A VALID &
	! RECEIVER ID AND HAD MESSAGES PENDING. &
	! SET UP CORE COMMON, ADDING PMD%, AND CHAIN TO ERRDIS. &
	&


10000	! &
	&
	&
	!	S U B R O U T I N E S &
	&

10700	! &
	&
	&
	!	M E S S A G E    R E C E I V E R S &
	&
	&
	&
	!	R E C E I V E R    I D    B L O C K &
	! &
	!	  CONTENTS		OFFSET &
	!	   S.LINK		  0	PTR TO NEXT RIB &
	!	   S.RCID1		  2	RECEIVER ID IN ASCII &
	!	   S.RCID2		  4 &
	!	   S.RCID3		  6 &
	!  	S.OBJT - S.JBNO		  8	OBJ TYPE (NET) - JOB #*2 &
	!	RESRVE - S.ACCS		 10	RESERVED - ACCESS CONTROL &
	!	   S.BMAX 		 12	BUFFER MAXIMUM IN BYTES &
	!	S.MCNT - S.MMAX		 14	MESSAGE COUNT - MESSAGE MAX &
	!	   S.MLST1 		 16	MSG LIST ROOT/TAIL POINTERS &
	!	   S.MLST2		 18 &
	!	S.LCNT - S.LMAX		 20	LINK COUNT - LINK MAX &
	!	   S.LLST1		 22	LINK LIST ROOT/TAIL POINTERS &
	!	   S.LLST2		 24 &
	!	  RESERVED		 26	RESERVED FOR NETWORKS &
	!	  RESERVED		 28	RESERVED FOR NETWORKS &
	!	  RESERVED		 30	RESERVED FOR NETWORKS &
	! &
	!	S.ACCS BITS: &
	!	BIT	MEANING &
	!	0	ALLOW LOCAL SENDERS &
	!	1	ALLOW ONLY PRIVILEGED LOCAL SENDERS &
	!	2	ALLOW NETWORK SENDERS &
	!	3	NETWORK SINGLE LINK MODE &
	!	4-6	RESERVED &
	!	7	FURTHER LOCAL SENDS ARE 'XOFFED' &

10710	HD%,NSP%=0% &
	\ NSP%=-1% UNLESS DECNET% &
	\ RIB%=FNP%(SNDLST%) &
	\ PRINT #1%,C$;"Message Receivers:"; &
	\ IF RIB% THEN &
		HD%=-1% &
	\	PRINT #1%,C$;"Rcvrid   Job    Rib  "; &
	\	PRINT #1%,"Obj   Msgs/Max   Links/InMax/OutMax  Access" &
	! ACCESS THE SNDLST. &
	! PRINT THE HEADING. &
	! IF THE LIST IS NOT EMPTY THEN &
	!	PRINT ADDITIONAL HEADINGS &
	! ELSE	LET THE USER KNOW THERE AREN'T ANY &
	!	VALID RECEIVERS, SET FLAG SO WE WON'T BOTHER WITH CRASH &
	!	ERROR LOGGING STUFF AND GET OUT &

10720	WHILE RIB% &
	\	RCVRID$="" &
	\	RCVRID$=RCVRID$+CVT%$(SWAP%(FNP%(RIB%+I%))) &
			FOR I%=2% TO 6% STEP 2% &
	\	PMB%=FNP%(RIB%+16%) IF RCVRID$="ERRLOG" &
	\	ACCSS%=FNP%(RIB%+10%) &
	\	MCNT.MX%=FNP%(RIB%+14%) &
	\	LCNT.MX%=FNP%(RIB%+20%) &
	\	LOUT.MAX=FNP%(RIB%+26%) AND 255% &
	\	RIB.NUM%=SWAP%(FNP%(RIB%+10%)) AND 255% &
	\	RIB.NUM$=NUM1$(RIB.NUM%) &
	\	RIB.NUM$=" "+RIB.NUM$ WHILE LEN(RIB.NUM$)<3% &
	\	OB.JOB%=FNP%(RIB%+8%) &
	\	IF (OB.JOB% AND 1%)<>0% THEN &
			JOB$=LEFT(RCVRID$,3%) &
		ELSE	JOB$=FNN$(2%,(OB.JOB% AND 255%)/2%)
10730		PRINT #1%,RCVRID$;TAB(9%);JOB$; &
	\	PRINT #1%,TAB(15%);RIB.NUM$;TAB(21%); &
	\	PRINT #1%, FNN$(2%,SWAP%(OB.JOB%) AND 255%); &
	\	PRINT #1%,TAB(27%); &
			FNN$(4%,SWAP%(MCNT.MX%) AND 255%); &
			"/";NUM1$(MCNT.MX% AND 255%); &
	\	ACCSS%=(ACCSS% AND (NOT 1%)) IF (ACCSS% AND 2%) &
	\	TEMP$="" &
	\	TEMP$=TEMP$+", "+ &
		CVT$$(MID("LclPrvNetOneNCS   Evt",I%*3%+1%,3%),2%) &
			IF (ACCSS% AND 2%^I%) &
				FOR I%=3% TO 0% STEP -1% &
	\	TEMP$="  None" UNLESS LEN(TEMP$) &
	\	PRINT #1%,TAB(40%);FNN$(5%,SWAP%(LCNT.MX%) AND 255%); &
				"/";NUM1$(LCNT.MX% AND 255%); &
				"/";NUM1$(LOUT.MX%); &
	\	PRINT #1%,TAB(59%);RIGHT(TEMP$,3%) &
	\	GOTO 10760 IF E% &
	\	RIB%=FNP%(RIB%) &
	\	IF RIB%=0% AND NSP%=0% THEN &
			RIB%=FNP%(NSPLST%) &
	\		NSP%=-1% &

10750	NEXT &
	\ PRINT #1%," None" IF HD%=0% AND E%=0% &
	! UNTIL WE HIT THE END OF THE RECEIVER LIST (0): &
	!	EXTRACT THE ID &
	!	PRINT THE ID, JOB #, NUMBER OF PENDING &
	!	MESSAGES/MAX # OF MESSAGES AND THE TYPE &
	!	OF SENDER ALLOWED FOR THIS RECEIVER. &
	!	IF WE'VE FOUND ERRCPY'S RECEIVER ID ('ERRLOG') IN THE &
	!	LIST OF VALID ID'S THEN &
	!		SAVE THE POINTER TO THE LIST OF OUTSTANDING &
	!		MESSAGES (S.MLST) &
	!		(PMB= PENDING MESSAGE BLOCK) &
	!	IF NO FNP% ERROR THEN POINT TO THE NEXT RECEIVER. &
	! &
	! NOTE THAT IF THE JOB # IS ODD IT INDICATES SOMETHING 'SPECIAL &
	! ('NSP') AND THE JOB IS PRINTED AS THE ID. &
	! OBJECT TYPE AND LINK COUNTS ARE ALSO PRINTED IF DECNET WAS &
	! CONFIGURED. &

10760	RETURN &

10900	! &
	!	S H O W    C A C H E    I N F O R M A T I O N &
	! &

10990	RETURN &

11000	! &
	! 	S T A T I S T I C S    T A B L E &
	! &
	STSTBL%=M2%(49%) &
	\ PRINT #1%, C$;C$;C$;C$;"STSTBL:";C$;C$; &
	\ IF STSTBL%<>0% THEN &
		PRINT #1%, "JSTCTL = ";FNO$(FNP%(STSTBL%+2%)); &
			"    CHECTL = ";FNO$(FNP%(STSTBL%+6%));C$; &
	ELSE PRINT #1%, "Symbol STSTBL not found";C$; &

11100	! &
	&
	&
	!	O C T A L    D U M P    O F    S T A T U S &
	&
	&

11110	PRINT #1%, CHR$(12%); &
	\ XCON%=FNP%(M2%(37%)) &
	\ I.D%=(XCON% AND 8192%)<>0% &
	\ U.22%=(XCON% AND 16384%)<>0% &
	\ VALID%=0% &
	\ VALID%=342% IF FNP%(SATBUF%+342%) = (NOT FNP%(SATBUF%+344%)) &
		IF OCT.ST%=2% &
			! THIS WAS A SNAP IF SATBUF WAS NOT LOADED RIGHT. &
			! LOOK AT 342-344 IF 7.0 SYSTEM. &
	\ VALID%=338% IF FNP%(SATBUF%+338%) = (NOT FNP%(SATBUF%+340%)) &
		IF OCT.ST%=4% &
			! LOOK AT 338-340 IF 7.1 SYSTEM &
	\ VALID%=206% IF FNP%(SATBUF%+206%) = (NOT FNP%(SATBUF%+208%)) &
		IF V97% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
			! Addresses moved by 64 for super & I&D apr's if v9.7 &
			! LOOK AT 206-208 (316-320 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS WERE DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
	\ VALID%=270% IF FNP%(SATBUF%+270%) = (NOT FNP%(SATBUF%+272%)) &
		IF V97%=0% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
			! Not moved by 64 for super & I&D apr's before v9.7 &
			! if .sil is from v9.7 &
			! LOOK AT 270-272 (416-420 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS WERE DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
	\ VALID%=214% IF FNP%(SATBUF%+214%) = (NOT FNP%(SATBUF%+216%)) &
		IF OCT.ST%=4% AND U.22%<>0% &
			! LOOK AT 214-216 IF 7.1 SYSTEM &
			! AND WE HAD 22-BIT ADDRESSING &
	\ VALID%=82% IF FNP%(SATBUF%+82%) = (NOT FNP%(SATBUF%+84%)) &
		IF V97% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
					AND ((XCON% AND 16384%)<>0%) &
			! Addresses moved by 64 for super & I&D apr's if V9.7 &
			! LOOK AT 82-84 (122-124 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
			! AND WE HAD 22-BIT ADDRESSING &
	\ VALID%=146% IF FNP%(SATBUF%+146%) = (NOT FNP%(SATBUF%+148%)) &
		IF V97%=0% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
					   AND ((XCON% AND 16384%)<>0%) &
			! Not moved by 64 for super & I&D apr's if before V9.7 &
			! LOOK AT 146-148 (222-224 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
			! AND WE HAD 22-BIT ADDRESSING &
	\ DSA%=VALID% &
	\ VALID%=VALID%<>0% &
	\ PRINT #1%, "****************************************************"; &
		C$;C$; IF VALID%=0% &
	\ PRINT #1%, "This section is not valid. "; &
		C$;"Crash file probably came from a SNAP DUMP."; &
		C$;C$; IF VALID%=0% &
	\ PRINT #1%,"Octal Dump of Status";C$ &
	\ RESTORE &

11112	PRINT #1%, "Error Code" &
		\ Z%=SATBUF%+510% &
		\ J%=FNP%(Z%) &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(J%); &
		\ PRINT #1%, T$;FNE$(J%); IF (J%>0% AND J%<128%) &
		\ PRINT #1%, T$;"Power Fail"; IF J%=-1% &
		\ PRINT #1%, T$;"Jump to 0"; IF J%=-2% &
		\ PRINT #1%, T$;"Continue from 52"; IF J%=-3% &
		\ PRINT #1%, T$;"Software forced crash"; IF J%=-4% &
		\ PRINT #1%, " (or possible Hardware Failure)"; IF FNP%(0%)<>70% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "Saved R0 to R5" &
		\ Z%=SATBUF%+496% &
		\ PRINT #1%, FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%+10% STEP 2% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "Kernel Stack Pointer" &
		\ Z%=SATBUF%+508% &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(FNP%(Z%));C$ &
	\ PRINT #1%, "Virtual Program Counter" &
		\ Z%=SATBUF%+494% &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(FNP%(Z%));C$ &
	\ PRINT #1%, "Processor Status" &
		\ Z%=SATBUF%+492% &
		\ JP%=FNP%(Z%) &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(JP%);T$; &
		\ BITZ%=FNBITZ%(JP%,15%,14%) &
		\ PRINT #1%, "Current Mode: "; &
		\ PRINT #1%, "Kernel"; IF BITZ%=0% &
		\ PRINT #1%, "Supervisor"; IF BITZ%=1% &
		\ PRINT #1%, "User"; IF BITZ%=3% &
		\ BITZ%=FNBITZ%(JP%,13%,12%) &
		\ PRINT #1%, ", Previous Mode: "; &
		\ PRINT #1%, "Kernel"; IF BITZ%=0% &
		\ PRINT #1%, "Supervisor"; IF BITZ%=1% &
		\ PRINT #1%, "User"; IF BITZ%=3% &
		\ PRINT #1%, C$;T$;T$;"Register Set: "; &
			NUM1$(FNBITZ%(JP%,11%,11%)); &
		\ PRINT #1%, ", Priority: ";NUM1$(FNBITZ%(JP%,7%,5%)); &
		\ PRINT #1%, ", TNZVC: "; &
		\ PRINT #1%, NUM1$(FNBITZ%(JP%,I%,I%)); FOR I%=4% TO 0% STEP -1% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "Top 8 items on Kernel stack" &
		\ Z%=SATBUF%+490% &
		\ PRINT #1%, FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%-14% STEP -2% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "Instruction space -6(PC) thru 8(PC)" &
		\ Z%=SATBUF%+4% &
		\ PRINT #1%, FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%+14% STEP 2% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "User Stack Pointer" &
		\ Z%=SATBUF%+474% &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(FNP%(Z%));C$ &
	\ PRINT #1%, "Job status (at SYSTAK-2)" &
		\ Z%=M%(39%)-2% &
		\ JP%=FNP%(Z%) &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(JP%);T$; &
		\ BITZ%=FNBITZ%(JP%,15%,14%) &
		\ PRINT #1%, "Current Mode: "; &
		\ PRINT #1%, "Kernel"; IF BITZ%=0% &
		\ PRINT #1%, "Supervisor"; IF BITZ%=1% &
		\ PRINT #1%, "User"; IF BITZ%=3% &
		\ BITZ%=FNBITZ%(JP%,13%,12%) &
		\ PRINT #1%, ", Previous Mode: "; &
		\ PRINT #1%, "Kernel"; IF BITZ%=0% &
		\ PRINT #1%, "Supervisor"; IF BITZ%=1% &
		\ PRINT #1%, "User"; IF BITZ%=3% &
		\ PRINT #1%, C$;T$;T$;"Register Set: "; &
			NUM1$(FNBITZ%(JP%,11%,11%)); &
		\ PRINT #1%, ", Priority: ";NUM1$(FNBITZ%(JP%,7%,5%)); &
		\ PRINT #1%, ", TNZVC: "; &
		\ PRINT #1%, NUM1$(FNBITZ%(JP%,I%,I%)); FOR I%=4% TO 0% STEP -1% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "Flag word for current job (JCMFLG)" &
		\ Z%=M%(41%) &
		\ JCRSIZ=FNF(FNP%(M%(33%))) &
		\ PJCR6%=FNP%(JCR6%) &
		\ J%=(FNP%(518%) AND 255%)/2% &
		\ JCR=FNF(J%*2%*JCRSIZ) &
		\ JOB.FLAG%=FNJCR.P%(301%,JCR+Z%,1%) &
		\ PRINT #1%, fnf22$(PJCR6%,JCR,Z%);"/"; &
			T$;FNO$(JOB.FLAG%);T$; &
		\ PRINT #1%, "Super mode ";FNOO$(FNBITZ%(JOB.FLAG%,15%,15%)); &
		\ PRINT #1%, ", I & D ";FNOO$(FNBITZ%(JOB.FLAG%,7%,7%));C$ &
	\ PRINT #1%, "D protection mask (JCAPRM)" &
		\ Z%=48% &
		\ D.PROT.W%=FNJCR.P%(302%,JCR+Z%,1%) &
		\ D.PROT.B%=SWAP%(D.PROT.W%) AND 255% &
		\ PRINT #1%, fnf22$(PJCR6%,JCR,Z%+1%);"\"; &
			T$;FNB$(D.PROT.B%);C$ &
	\ PRINT #1%, "Top 8 items on User stack" &
		\ Z%=SATBUF%+472% &
		\ PRINT #1%, FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%-14% STEP -2% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "User Keyword" &
		\ Z%=SATBUF%+456% &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(FNP%(Z%));C$ &
	\ PRINT #1%, "User FIRQB" &
		\ Z%=SATBUF%+454% &
		\ PRINT #1%, FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%-14% STEP -2% &
		\ Z%=Z%-16% &
		\ PRINT #1%, C$;FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%-14% STEP -2% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "User XRB" &
		\ Z%=SATBUF%+422% &
		\ PRINT #1%, FNO$(Z%);"/"; &
		\ PRINT #1%, T$;FNO$(FNP%(I%)); FOR I%=Z% TO Z%-12% STEP -2% &
		\ PRINT #1%, C$ &
	\ PRINT #1%, "Registers in I-space" &
	\ PRINT #1%, "		Super	Super	User	User	Kernel	Kernel" IF I.D% IF V97% &
	\ PRINT #1%, "		User	User	Kernel	Kernel" IF I.D%=0% OR V97%=0% &
	\ PRINT #1%, "		Addr	Desc	Addr	Desc	Addr	Desc" IF I.D% IF V97% &
	\ PRINT #1%, "		Addr	Desc	Addr	Desc" IF I.D%=0% OR V97%=0% &
	\ PRINT #1%, "		Reg	Reg	Reg	Reg	Reg	Reg" IF I.D% IF V97% &
	\ PRINT #1%, "		Reg	Reg	Reg	Reg" IF I.D%=0% OR V97%=0% &
		\ Z%=SATBUF%+314% &
		\ C%=10% &
		\ C%=6% IF OCT.ST%<>4% OR I.D%=0% OR V97%=0% &
		\ Z%=Z%+32% IF C%=6% &
			! If this is not a 9.0 system or it has no I&D, &
			! or it is pre-V9.7, then &
			! there are no supervisor IAPRs &
		\ KAR5%=FNP%(Z%+(5%*(C%+2%))+C%-2%) &
		\ KAR6%=FNP%(Z%+(6%*(C%+2%))+C%-2%) &
		\ FOR I%=0% TO 7% &
			\ PRINT #1%, FNO$(Z%);"/";T$;"APR";NUM1$(I%); &
			\ PRINT #1%, T$;FNO$(FNP%(J%)); &
				FOR J%=Z% TO Z%+C% STEP 2% &
			\ PRINT #1%, C$; &
			\ Z%=Z%+C%+2% &
		\ NEXT I% &
		\ PRINT #1%, C$; &
	\ PRINT #1%, "Memory Management Registers" &
		\ Z%=SATBUF%+312% &
		\ Z%=Z%+32% IF OCT.ST%<>4% OR I.D%=0% OR V97%=0% &
			! If this is not a 9.0 system or it has no I&D, &
			! or if this is pre-V9.7, then &
			! there are no supervisor IAPRs. &
		\ PRINT #1%, "MMR0" &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(FNP%(Z%));C$ &
		\ PRINT #1%, "MMR2" &
		\ PRINT #1%, FNO$(Z%-2%);"/";T$;FNO$(FNP%(Z%-2%));C$ &
	\ IF I.D% THEN &
			! Skip MMR1,3 and DAPRs if not I&D. &
		  PRINT #1%, "MMR1" &
		\ PRINT #1%, FNO$(Z%-4%);"/";T$;FNO$(FNP%(Z%-4%));C$ &
		\ PRINT #1%, "MMR3" &
		\ SM6%=FNP%(Z%-6%) &
		\ PRINT #1%, FNO$(Z%-6%);"/";T$;FNO$(SM6%);C$; &
		\ PRINT #1%, "User D: ";FNOO$(FNBITZ%(SM6%,0%,0%)); &
		\ PRINT #1%, ", Super D: ";FNOO$(FNBITZ%(SM6%,1%,1%)); &
		\ PRINT #1%, ", Kernel D: ";FNOO$(FNBITZ%(SM6%,2%,2%)); &
		\ PRINT #1%, ", 22-bit"; IF (SM6% AND 16%)<>0% &
		\ PRINT #1%, ", 18-bit"; IF (SM6% AND 16%)=0% &
		\ PRINT #1%, ", Ubus Map relo: ";FNOO$(FNBITZ%(SM6%,5%,5%));C$ &
	\ PRINT #1%, "Registers in D-space" &
	\ PRINT #1%, "		Super	Super	User	User	Kernel	Kernel" IF I.D% IF V97% &
	\ PRINT #1%, "		User	User	Kernel	Kernel" IF I.D%=0% OR V97%=0% &
	\ PRINT #1%, "		Addr	Desc	Addr	Desc	Addr	Desc" IF I.D% IF V97% &
	\ PRINT #1%, "		Addr	Desc	Addr	Desc" IF I.D%=0% OR V97%=0% &
	\ PRINT #1%, "		Reg	Reg	Reg	Reg	Reg	Reg" IF I.D% IF V97% &
	\ PRINT #1%, "		Reg	Reg	Reg	Reg" IF I.D%=0% OR V97%=0% &
		\ Z%=SATBUF%+210% &
		\ Z%=Z%+64% IF V97%=0% &
			! If this is pre-V9.7, then no supervisor I or D APRs. &
		\ C%=10% &
		\ C%=6% IF OCT.ST%<>4% OR I.D%=0% OR V97%=0% &
		\ KDR5%=FNP%(Z%+(5%*(C%+2%))+C%-2%) &
		\ KDR6%=FNP%(Z%+(6%*(C%+2%))+C%-2%) &
		\ FOR I%=0% TO 7% &
			\ PRINT #1%, FNO$(Z%);"/";T$;"APR";NUM1$(I%); &
			\ PRINT #1%, T$;FNO$(FNP%(J%)); FOR J%=Z% TO Z%+C% STEP 2% &
			\ PRINT #1%, C$; &
			\ Z%=Z%+C%+2% &
		\ NEXT I% &
		\ PRINT #1%, C$; &

11114	IF U.22% THEN ! We will do UMRs only if we have 22-bit addresses &
	  PRINT #1%, "UNIBUS Mapping Registers" &
	\ PRINT #1%, "	 UMR's" &
		\ Z%=SATBUF%+86% &
		\ C%=14% &
		\ Z%=Z%+64% IF V97%=0% &
			! If pre-V9.7, then no super DAPRs or super IAPRs. &
		\ Z%=Z%+68% IF I.D%=0% &
			! If no I&D, then no DAPRs, no MMR1,3. &
		\ FOR I%=0% TO 7% &
			\ PRINT #1%, FNO$(Z%);"/";T$; &
			\ PRINT #1%, NUM1$(I%*4%);"-"; &
			\ PRINT #1%, NUM1$((I%*4%)+3%); UNLESS I%=7% &
			\ PRINT #1%, NUM1$((I%*4%)+2%); IF I%=7% &
			\ FOR J%=Z% TO Z%+C% STEP 2% &
				\ PRINT #1%, T$;FNO$(FNP%(J%)); &
					UNLESS I%=7% AND J%>Z%+10% &
			\ NEXT J% &
			\ PRINT #1%, C$; &
			\ Z%=Z%+C%+2% &
		\ NEXT I% &
		\ PRINT #1%, C$; &

11116	  PRINT #1%, "CPU ID Register" &
		\ Z%=SATBUF%+20% &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(FNP%(Z%));C$ &
	\ PRINT #1%, "CPU Error Register" &
		\ Z%=SATBUF%+22% &
		\ CPUE%=FNP%(Z%) &
		\ PRINT #1%, FNO$(Z%);"/";T$;FNO$(CPUE%); &
		\ PRINT #1%, C$;"  Red Zone Stack Limit"; IF CPUE% AND 4% &
		\ PRINT #1%, C$;"  Yellow Zone Stack Limit"; IF CPUE% AND 8% &
		\ PRINT #1%, C$;"  UNIBUS Time Out"; IF CPUE% AND 16% &
		\ PRINT #1%, C$;"  Non-Existent Memory (Cache)"; IF CPUE% AND 32% &
		\ PRINT #1%, C$;"  Odd Address Error"; IF CPUE% AND 64% &
		\ PRINT #1%, C$;"  Illegal Halt"; IF CPUE% AND 128% &
		\ PRINT #1%, C$ &
	\ MEDL%=FNP%(SATBUF%+24%) &
	\ MAX.MEDL%=DSA%-28% IF DSA%<>0% &
		! If SATBUF is valid. &
	\ MAX.MEDL%=342%-28% IF DSA%=0% &
		! If SATBUF is not valid. &
		! The DSA, MEDL%, and MAX.MEDL% stuff is for the purpose of &
		! deciding how much of the MED data to dump in case &
		! the MED data length (MEDL%) is some very large number, as &
		! it could be if SATBUF is invalid. &
		! &
	! Here is the general layout of SATBUF in the low end: &
	! &
	! SATBUF%+DSA%+4%   ... (rest of SATBUF up to SATBUF%+511%) &
	! SATBUF%+DSA%+2%   Size of the dumped monitor image &
	! SATBUF%+DSA%      Complement of size of the dumped monitor image &
	!			The highest this can be is SATBUF%+342% &
	! SATBUF%+26%       ... (MED data up to MEDL% or to SATBUF%+DSA%, &
	!			whichever is smaller. &
	! SATBUF%+24%       Data length of MED data (MEDL%). &
	! SATBUF%           ... (start of SATBUF up to SATBUF%+24% &
		! &
	\ IF MEDL% <> 0% THEN &
		  MEDL%=MAX.MEDL% IF (MEDL%>MAX.MEDL%) OR (MEDL%<0%) &
		\ MEDL.TOP%=SATBUF%+26%+MEDL% &
		\ PRINT #1%, "PDP 11/60 MED Data" &
			\ Z%=SATBUF%+26% &
			\ FOR I%=Z% TO MEDL.TOP% STEP 16% &
				\ PRINT #1%, FNO$(I%);"/"; &
				\ FOR J%=I% TO I%+14% STEP 2% &
				\ PRINT #1%, T$;FNO$(FNP%(J%)); &
					UNLESS (J%) > MEDL.TOP% &
				\ NEXT J% &
				\ PRINT #1%, C$; &
			\ NEXT I% &

11130	J%=FNP%(518%) &
	\ PRINT #1%, C$;C$; &
		"****************************************************"; &
		C$;C$;chr$(12%); IF VALID%=0% &
	\ PRINT #1%,"	Job*2	Job	Job(10)	"; &
			"	Next*2	Next	Next(10)";C$; &
		FNO$(518%);"\";T$;FNB$(J% and 255%);T$; &
		FNB$((J% and 255%)/2%);T$; &
		NUM1$((J% and 255%)/2%);T$;T$; &
		FNB$(SWAP%(J%) and 255%);T$; &
		FNB$((SWAP%(J%) and 255%)/2%);T$; &
		NUM1$((SWAP%(J%) and 255%)/2%);T$; &
	\ PRINT #1%, C$;C$; &
		"	JOBDA	 JOBF	IOSTS	JOBWRK"; &
		"	JOBJD2	JOBRTS	CPUTIM	JOBWDB"; &
		C$;FNO$(520%);"/"; &
	\ PRINT #1%,T$;FNO$(FNP%(K%)); FOR K%=520% TO 534% STEP 2% &
	\ PRINT #1%,C$;C$; &
		"FIJOB	FIPjob*2	FIPjob		FIPjob(10)"; &
		C$;FNO$(FIJOB%);"\"; &
		T$;FNB$(FNP%(FIJOB%) AND 255%); &
		T$;T$;FNB$((FNP%(FIJOB%) AND 255%)/2%); &
		T$;T$;NUM1$((FNP%(FIJOB%) AND 255%)/2%); &
			IF FIJOB% &
	\ PRINT #1%,C$;C$;"FIJBDA";C$;FNO$(FIJBDA%);"/"; &
		T$;FNO$(FNP%(FIJBDA%) AND 32766%); &
			IF FIJBDA% &
	\ IF JOBTBL% THEN &
		PRINT #1%,C$;C$;"JOBTBL"; &
	\	C%=0% &
	\	I%=-1% &
	\	FOR K%=0% TO 126% STEP 2% &
	\		ADDR%=JOBTBL%+K% &
	\		GOSUB 12600 &
	\		GOTO 11140 IF J%=-1% OR E%<>0% &
	\	NEXT K% &

11140	if LOWAD% then &
		print #1%,C$;C$;"Memory parity/ECC log" &
\		print #1%,"        LOWADD  HI ADD  MEMERR  UpeCSR  CSRlow"; &
			"  CSR hi  K Range"; &
\		C% = 0%					! ??? &
\		I% = -1%				! ??? &
\		for K% = 0% to 14% step 2% &
\			ADDR% = LOWAD% + K% &
\			GOSUB 12600			! Print some data &
\			goto 12000 if J%=-1% or E%<>0%	! ??? &
\		next K% &
	! PRINT SECOND BATCH OF INFO &
	&
	&

12000	OPEN 'ANAL'+RIGHT(NUM1$(100%+((PEEK(518%) AND 255%)/2%)),2%)+ &
		'.TMP' FOR INPUT AS FILE 11%, MODE 256% &
	\ PRINT #1%,CHR$(12%);C$;C$;"Installed Patches:"; &
	\ HD%=0% &
	\ PATCH%=PAT%(0%) &
	\ GOTO 12020 UNLESS PATCH% &
	\ FOR I%=1% TO PATCH% &
	\	FOR I1%=0% TO 3% &
	\		J%=FNP%(PAT%(I%)+(I1%*2%)) &
	\		GOTO 12010 UNLESS J% &
	\		HD%=-1% &
	\		PRINT #1% IF I%>1% &
	\		PRINT #1%,TAB(20%);PAT$(I%);"."; &
			 RIGHT(NUM1$(1000%+(I2%+1%)+I1%*16%),3%);C$; &
				IF (J% AND 2%^I2%) FOR I2%=0% TO 15%
12010		NEXT I1% &
	\ NEXT I% &
	\ PRINT #1%," None"; UNLESS HD% &
		! OPEN THE WORK FILE. &
		! SKIP DOWN TO CORE DUMP SECTION IF /BUFFERS SPECIFIED. &

12020	PRINT #1%,C$;C$;"SIL Directory:";C$; &
	"Name	Ident	 Phys    Load	 Size  Transfer Total" &
	\ HD%,PHYS%,SIZBLK%,G8%=0% &
	\ NUM.MOD%=SILMOD%(0%,0%) &
	\ FOR I%=1% TO NUM.MOD% &
	\	PHASE$=RAD$(SILMOD%(I%,0%))+RAD$(SILMOD%(I%,1%)) &
	\	PRINT #1%,PHASE$;T$; &
		RAD$(SILMOD%(I%,2%));RAD$(SILMOD%(I%,3%));T$; &
	\	HD%=-1% IF PHASE$="OVR" UNLESS V97% &
	\	HD%=-1% IF PHASE$="OV2" IF V97% &
	\	PRINT.PHYS%=PHYS% &
	\	PRINT.PHYS%=FNP%(PHYS%) UNLESS (FNF(PHYS%)>EPMM) &
						OR (PHYS%=0%) &
	\	PRINT #1%, FNO$(PRINT.PHYS%); &
			UNLESS HD% &
	\	PRINT #1%, "  "; &
			UNLESS HD% &
	\	PRINT #1%," ";T$; IF HD% &
	\	PRINT #1%,FNO$(SILMOD%(I%,J%));T$; FOR J%=7% TO 9% &
	\	G8%=(SILMOD%(I%,10%) EQV 32767%)+32768. &
	\	G8%=((((SILMOD%(I%,8%) EQV 32767%)+32768.)+511%)/512%) &
			IF G8%=0% &
	\	SIZBLK%=SIZBLK%+G8% &
	\	PRINT #1%,FNN$(3%,(SIZBLK%+3%)/4%);"K"; &
	\	GOTO 12025 UNLESS VALID% &
	\	PRINT #1%, "  <-- KISAR5"; IF KAR5%=PRINT.PHYS% &
	\	PRINT #1%, "  <-- KISAR6"; IF KAR6%=PRINT.PHYS% &
	\	PRINT #1%, "  <-- KDSAR5"; IF KDR5%=PRINT.PHYS% &
	\	PRINT #1%, "  <-- KDSAR6"; IF KDR6%=PRINT.PHYS% &
		! INDICATE WHICH PHASE IS IN THE KERNEL ADDRESS REGISTERS &
		! FOR APR'S 5 AND 6.
12025		PRINT #1% &
	\	PHYS%=SILMOD%(I%+1%,14%) &
!	\	PHYS%=PHYS%+(((SILMOD%(I%,8%) EQV 32767%)+32768.)/64%) &
			UNLESS HD% &
	\ NEXT I% &
	! SILMOD(,0 & 1)= SIL NAME &
	!	(,2 & 3)= IDENT &
	!	(,4)	= BLOCK OFFSET TO MOD IN SIL &
	!	(,5)	= BLOCK OFFSET TO STB &
	!	(,6)	= # OF SYMBOLS IN STB &
	!	(,7)	= LOAD ADDRESS &
	!	(,8)	= SIZE &
	!	(,9)	= TRANSFER &
	!	(,10)	= SIZE IN BLOCKS &
	!	(,11)	= BLOCK OFFSET TO MOD'S OVERLAY DESCRIPTOR &
	!	(,12)	= NUMBER OF OVERLAY DESCRIPTORS &
	!	(,13)	= BYTE OFFSET TO MOD IN SIL &
	!	(,16)	= XXXPAT VALUE &
	&

12030	G9%=0% &
	\ PRINT #1%,C$;"Contents of patch space 'PATCH'"; &
	\ C%=0% &
	\ I%=-1% &
	\ FOR K%=0% TO 254% STEP 2% &
	\	ADDR%=PATSP%+K% &
	\	GOSUB 12600 &
	\	GOTO 12500 IF E% &
	\ NEXT K% &
	\ K%=0% &
	\ FOR I1%=2% TO NUM.MOD% &
	\	GOTO 12060 UNLESS SILMOD%(I1%,16%) &
	\	PRINT #1%,C$,C$;"Contents of patch space '"; &
			RAD$(SILMOD%(I1%,0%))+"PAT";"'" &
	\	C%=0% &
	\	SE.VAL%=SILMOD%(I1%,16%) &
	\	FOR ADDR%=0% TO 126% STEP 2% &
	\		IF (C% AND 7%)=0% THEN &
				C%=0% &
	\			PRINT #1%,C$;FNO$(SE.VAL%+ADDR%);"/";
12040			PRINT #1%,T$;FNO$(SILPAT%(K%,ADDR%/2%)); &
	\		C%=C%+1% &
	\	NEXT ADDR%
12050		K%=K%+1%
12060	NEXT I1% &
	! &
	!	P A T C H    S P A C E    P R I N T E R &
	! &
	! PRINT PATCH SPACE ENTRIES FROM 'PATCH' &
	! THEN PRINT OTHER PATCH SPACE: CURRENTLY TERPAT,EMTPAT,FIPPAT &
	&

12500	 IF D.UMP%=0% THEN &
		PRINT #1%,C$;C$;C$;"Core dump of memory"; &
		" will not be printed (/NODUMP specified)" &
	\	GOTO 12590 &
		! &
		!	C O R E    D U M P &
		! &
		! LIST INSTALLED PATCHES AND SIL DIRECTORY. &
		! SKIP DUMP IF /NODUMP SPECIFIED. &

12510	G9%=0% &
	\ WDE%=8% &
	\	G9$='' &
	\	G9$=G9$+'.' FOR I%=0% TO 31% &
	\	G9$=G9$+CHR$(I%) FOR I%=32% TO 126% &
	\	G9$=G9$+'.' FOR I%=127% TO 160% &
	\	G9$=G9$+CHR$(I%) FOR I%=161% TO 163% &
	\	G9$=G9$+'.'+CHR$(165%)+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=167% TO 171% &
	\	G9$=G9$+'.' FOR I%=172% TO 175% &
	\	G9$=G9$+CHR$(I%) FOR I%=176% TO 179% &
	\	G9$=G9$+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=181% TO 183% &
	\	G9$=G9$+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=185% TO 189% &
	\	G9$=G9$+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=191% TO 207% &
	\	G9$=G9$+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=209% TO 221% &
	\	G9$=G9$+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=223% TO 239% &
	\	G9$=G9$+'.' &
	\	G9$=G9$+CHR$(I%) FOR I%=241% TO 253% &
	\	G9$=G9$+'..' &
	\	G9%=-1% &
	\ IF WIDE% THEN &
		WDE%=4% &
		! PRINT HEADER. &
		! GO SET UP ALL THE ANNOTATIONS. &
		! IF TOO.MANY<>0 THEN SOME POINTER (PROBABLY 'FREE #n') &
		!	WAS IN LIMBO - WARN EM BUT DO THE DUMP &

12515	J%=91% &
	\ J%=51% IF WDE%=8% &
		! SET UP TAB STOP FOR MULTIPLE ANNOTS. &

12520	FOR SEGMENT%=0% TO XTABS% &
			! WALK THROUGH THE SEGMENTS WE ARE GOING TO DUMP. &
		\ IF MAP%(SEGMENT%,0%)<>0% THEN &
		ADDRESS%=MAP%(SEGMENT%,3%) &
			! IF THIS IS MONITOR, THE FIRST ADDRESS IS ZERO &
		\ START.BLOCK%=MAP%(SEGMENT%,0%) &
			! INITIALIZE BLOCK STARTING BLOCK NUMBER. &
		\ END.BLOCK%=MAP%(SEGMENT%,1%) &
			! LAST BLOCK FOR THIS SEGMENT. &
		\ END.OFFST%=MAP%(SEGMENT%,2%) &
			! NUMBER OF BYTES IN LAST BLOCK. &
		\ Z$="" &
			! ERASE MEMORY STRING. &
		\ DUP%=0% &
		\ GOSUB 12530 &
			! PRINT APPROPRIATE HEADER. &
		\ PRINT #1%,C$;C$;C$;"XBUF dump will not be printed "; &
			"(/NOXBUF_DUMP specified)" &
			IF SEGMENT%=XBUF.SEG% AND D.UMP%=2% &
		\ GOTO 12525 IF SEGMENT%=XBUF.SEG% AND D.UMP%=2% &
		\ GOSUB 12540 &
			! DO THE DUMP. &
		\ GOSUB 12580 &
			! PRINT PARTING COMMENTS FOR THIS SEGMENT. &

12525	NEXT SEGMENT% &
	\ PRINT #1%, CHR$(13%);CHR$(10%); &
	\ RETURN &

12530	PRINT #1%,CHR$(12%);C$;C$;C$;"Memory dump of "; &
	\ GOSUB 12585 &
	\ PRINT #1%, C$;C$;C$ &
	\ RETURN &

12540	FOR BL%=START.BLOCK% TO END.BLOCK% &
			! GO THROUGH ALL BLOCKS FOR THIS SEGMENT. &
		\ GET #12%, BLOCK BL% &
			! GO GET ONE. &
		\ FOR BLKT%=0% TO 7% &
				! A "BLKT" IS 64 BYTES (8 PER DISK BLOCK) &
			\ Z1$=MID(D$,BLKT%*64%+1%,64%) &
				! GET 64 BYTES INTO A STRING. &
			\ IF Z1$=Z$ &
!				AND G0$(ADDRESS%*2%)=G0$(ADDRESS1%*2%) &
!				AND G0$(ADDRESS%*2%+1%)=G0$(ADDRESS1%*2%+1%) &
				THEN DUP%=DUP%+4%*(WDE%/4%) &
					\ ADDRESS%=ADDRESS%+1% &
					\ GOTO 12544 &
					! IF NEXT LINE IS DUPLICATE, &
					! AND ANNOTS ARE THE SAME, THEN &
					! UPDATE DUP COUNT AND ADDRESS AND &
					! GO GET ANOTHER LINE. &

12542			GOSUB 12570 IF DUP% &
				! IF WE HAD DUPLICATE LINES THEN TELL 'EM. &
			\ Z$=Z1$ &
			\ CHANGE Z$ TO BYTE% &
				! MAKE ASCII OUT OF THE STRING. &
			\ WORD%((I%+1%)/2%)=BYTE%(I%)+SWAP%(BYTE%(I%+1%)) &
				FOR I%=1% TO BYTE%(0%) STEP 2% &
					! MAKE WORDS OUT OF THE ASCII. &
			\ GOSUB 12550 &
				! GO TRY TO PRINT STUFF. &
			\ BLKT%=7% IF LEN(Z$)=0% &
				! IF THE PRINT ROUTINE AT 12550 DECIDED WE &
				! WERE DONE, THEN NO SENSE DOING MORE. &

12544		NEXT BLKT% &
	\ NEXT BL% &
		! PRINT SOME STUFF AT THE END OF THE SEGMENT. &
	\ GOSUB 12570 IF DUP% &
	\ RETURN &

12550	Z$=LEFT(Z$,END.OFFST%-(BLKT%*64%)) IF BL%=END.BLOCK% &
		! ASSURE THAT Z$ IS VALID IF WE ARE AT END OF SEGMENT. &
	\ GOTO 12562 IF LEN(Z$)=0% &
	\ FOR LINE.%=0% TO WDE%-1% &
			! 64 BYTES DO 4 LINES MAKE IF WE ARE WIDE. &
			! OR 8 LINES OTHERWISE. &
		\ GOTO 12560 IF LINE.%*16%/(WDE%/4%)>LEN(Z$) &
			! IF WE ARE BEYOND THE END THEN DON'T PRINT ANYTHING. &
		\ PRINT #1%, C$;FNO$(ADDRESS%); &
			! PRINT THE HIGH ORDER 6  ADDRESS DIGITS. &
		\ PRINT #1%, NUM1$(20%*LINE.%/(WDE%/4%));"/"; IF LINE.% &
		\ PRINT #1%, "00/"; IF LINE.%=0% &
				! PRINT THE LOW ORDER 2 ADDRESS DIGITS. &
		\ FOR I%=(LINE.%*8%/(WDE%/4%))+1% TO (LINE.%+1%)*8%/(WDE%/4%) &
			\ PRINT #1%, "  ";FNO$(WORD%(I%)); &
				IF I%<=LEN(Z$)/2% &
		\ NEXT I% &
			! PRINT A LINE OF MEMORY CONTENTS. &
			! WDE% DETERMINES HOW MANY PER LINE. &
			! CHECK LENGTH OF Z$ SO WE DON'T PRINT MORE THAN &
			! WE HAVE AT THE END OF IT ALL. &
		\ G8$="" &
		\ G8$=MID(Z$,LINE.%*16%/(WDE%/4%)+1%,16%/(WDE%/4%)) &
			! MAKE WHAT CHARACTERS WE CAN OUT OF THE LINE. &
		\ PRINT #1%, TAB(43%+(32% AND (WDE%=4%))); &
		\ PRINT #1%, XLATE(G8$,G9$); &
			! PRINT THE DOTS. &
		\ GX%=ADDRESS%*2%+LINE.%/2% IF SEGMENT%=ROOT.SEG% &
					    OR SEGMENT%=FPL.SEG% &
		\ GX%=ADDRESS% IF SEGMENT%=MSCP.SEG% &
					    OR SEGMENT%=JCTRL.SEG% &
		\ GX%=FNF(ADDRESS%)-FNF(MAP%(SEGMENT%,3%)) &
					    IF SEGMENT%=XBUF.SEG% &
					    OR SEGMENT%=JHEAD.SEG% &
					    OR SEGMENT%=FJHEAD.SEG% &
		\ PRINT #1%, " (";FNO$(FNR7L%(ADDRESS%));")"; &
			IF (LINE.%=0%) AND (SEGMENT%=XBUF.SEG%) &
!			OR (LINE.%=0%) AND (SEGMENT%=JHEAD.SEG%) &
!			OR (LINE.%=0%) AND (SEGMENT%=FJHEAD.SEG%) &
		\ GOTO 12560 IF ((LINE.% AND 1%)<>0% AND (WDE%=4%)) &
			OR ((LINE.% AND 3%)<>0% AND (WDE%=8%)) &
			OR ((LINE.%<>0%) AND (SEGMENT%=JCTRL.SEG%)) &
			OR ((LINE.%<>0%) AND (SEGMENT%=XBUF.SEG%)) &
			OR ((LINE.%<>0%) AND (SEGMENT%=MSCP.SEG%)) &
			OR ((LINE.%<>0%) AND (SEGMENT%=JHEAD.SEG%)) &
			OR ((LINE.%<>0%) AND (SEGMENT%=FJHEAD.SEG%)) &
		\ PO%=0% &
		\ PO%=-1% IF (LINE.%=0%) AND (SEGMENT%=XBUF.SEG%) &
			! GET ANNOTATION POINTER &
			! DO ANNOTS IF WE ARE AT AN EVEN LINE. &
			! AND WE ARE DOING MONITOR SEGMENT &
			! INITIALIZE FLAG &

12552		GX.TMP%=GX%(GX%,0%) &
		\ DUP.TMP%=GX%(GX%,1%) &
		\ GOTO 12560 UNLESS GX.TMP% &
			! GET THE ANNOTATION LIST AND DUPLICATE &
			! LIST POINTERS. &
			! LEAVE THE ROUTINE IF NO POINTER. &
			! &

12555		GX.TMP$=GX$(GX.TMP%) &
		\ GOTO 12558 UNLESS ASCII(GX.TMP$)=SEGMENT% &
		\ PRINT #1%, C$;TAB(J%); IF PO% &
		\ PRINT #1%, RIGHT(GX.TMP$,2%); &
		\ PO%=-1% &
			! SET FLAG TO INDICATE WE PRINTED ONE. &

12558		GOTO 12560 UNLESS DUP.TMP% &
		\ GX.TMP%=DUPLST%(DUP.TMP%,0%) &
		\ DUP.TMP%=DUPLST%(DUP.TMP%,1%) &
		\ GOTO 12555 &
			! DO WE HAVE DUPLICATE ANNOTATIONS? &
			! GET THE ANNOTATION AND LINK POINTERS &
			! OUT OF THE DUPLICATE LIST TABLE. &
			! &

12560	NEXT LINE.% &
	\ ADDRESS%=ADDRESS%+1% &
	\ ADDRESS1%=ADDRESS% &

12562	RETURN &
		! DO 4 LINES (OR 8 LINES) &
		! BUMP THE ADDRESS &
		! GO DO SOME MORE. &

12570	PRINT #1%, C$;T$;"******  "; &
	\ PRINT #1%, NUM1$(DUP%);" duplicate lines ("; &
	\ PRINT #1%, FNO$(ADDRESS1%);"00";" to ";FNO$(ADDRESS%-1%); &
	\ PRINT #1%, "60"; IF WDE%=4% &
	\ PRINT #1%, "70"; IF WDE%=8% &
	\ PRINT #1%, ")";"  ******"; &
	\ DUP%=0% &
	\ ADDRESS1%=ADDRESS% &
	\ RETURN &

12580	PRINT #1%,C$;C$;C$;"End of memory dump of "; &
	\ GOSUB 12585 &
	\ RETURN &

12585	PRINT #1%, "Monitor (size = X.MONS = "; IF OCT.ST%=4% &
		AND SEGMENT%=ROOT.SEG% &
	\ PRINT #1%, "Monitor (size = $$EPMM = "; IF OCT.ST%=2% &
		AND SEGMENT%=ROOT.SEG% &
	\ PRINT #1%, "Monitor (size = $$CRSZ = "; IF OCT.ST%=1% &
		AND SEGMENT%=ROOT.SEG% &
	\ PRINT #1%, "CRASH file (could not determine Monitor size)"; &
		 IF OCT.ST%=0% AND SEGMENT%=ROOT.SEG% &
	\ PRINT #1%, "XBUF (size = "; IF SEGMENT%=XBUF.SEG% &
	\ PRINT #1%, "FIP POOL (size = "; IF SEGMENT%=FPL.SEG% &
	\ PRINT #1%, "MSCP Region (size = "; IF SEGMENT%=MSCP.SEG% &
	\ PRINT #1%, "JOB Control Region (size = "; IF SEGMENT%=JCTRL.SEG% &
	\ PRINT #1%, "JOB HEADER Region (size = "; IF SEGMENT%=JHEAD.SEG% &
	\ PRINT #1%, "FIJOB HEADER Region (size = "; IF SEGMENT%=FJHEAD.SEG% &
	\ PRINT #1%, "Segment ";NUM1$(SEGMENT%);" (size = "; &
					IF SEGMENT%>MAX.SEG% &
	\ SEG.SIZE=(MAP%(SEGMENT%,1%)-MAP%(SEGMENT%,0%))*512. &
			+MAP%(SEGMENT%,2%) &
	\ SEG.SIZE.D.B.64%=SEG.SIZE/64% &
	\ PRINT #1%, NUM1$(SEG.SIZE);" decimal, "; &
	\ PRINT #1%, FNO$(FXMONS%); IF SEGMENT%=ROOT.SEG% &
	\ PRINT #1%, FNO$(SEG.SIZE.D.B.64%);"00"; IF SEGMENT%>ROOT.SEG% &
	\ PRINT #1%, " octal)"; &
	\ RETURN &

12590	RETURN &
		! GENERAL RETURN FROM DUMP SUBROUTINE. &

12600	IF (C% AND WDS%)=0% THEN &
		C%=0% &
	\	PRINT #1%,C$;FNO$(ADDR%);"/"; &
	\	G8$='' &
	! &
	!	C O R E    P R I N T &
	! &
	! IF WE REACHED THE END OF A LINE (8 or 16 PER LINE) THEN &
	!	JUMP TO A NEW LINE AND PRINT CURRENT ADDRESS. &
	!	PRINT REGISTER # FOR KERNEL/USER REGS. &

12610	J%=FNP%(ADDR% AND -2%) &
	\ GOTO 12640 IF (J%=-1% AND I%=-1%) OR E% &
	\ PRINT #1%,T$;FNO$(J%); IF PR.OC% &
	\ PRINT #1%,T$; IF (C0%=16% AND I%=2%) &
	\ C%=C%+1% &
	! PRINT AN ENTRY. &
	! IF ANNOTATING, DO SO NOW. &

12640	RETURN &
	&

12900	OPEN WORK2.FILE$ FOR INPUT AS FILE 9%, MODE 256%+8192% &
	\ FIELD #9%, 512% AS Z$ &
	\ ON ERROR GOTO 12950 &
	\ PRINT #1%, CHR$(12%) &
	\ UNTIL 0% &
	\ GET #9% &
	\ PRINT #1%, Z$; &
	\ NEXT &

12940	ON ERROR GOTO 19000 &
	\ RETURN &

12950	IF ERR=11% &
	THEN CLOSE 9% &
	\ RESUME 12940 &

12960	ON ERROR GOTO 19000 &
	\ GOTO 19000 &
	&
	! THIS ROUTINE OPENS THE SYMBOL TABLE FILE THAT WAS WRITTEN &
	! BY ANALYS, AND PRINTS IT OUT TO THE OUTPUT FILE. &
	! ONLY EXPECTED ERROR IS EOF, ALL OTHERS GO TO 19000 &
	&

14500	! &
	&
	&
	!	C R A S H    E R R O R    L O G G I N G &
	&

14510	S$=CR.LOGFIL$ &
	\ OPEN S$ FOR OUTPUT AS FILE 2% &
	\ OPEN "_NL:" AS FILE 8%, RECORDSIZE 512% &
	\ E%=0% &
	\ ON ERROR GOTO 14620 &
	\ RPT%=-1% &
	\ FOR W%=0% TO 61% &
	\	FIELD #8%,W%*8% AS W$,2% AS RECRD.LOG$,2% AS ER.LOG$, &
		2% AS ER.RCV$,2% AS RECRD.ALW$ &
	\	LSET RECRD.LOG$=CVT%$(0%) &
	\	LSET ER.LOG$=RECRD.LOG$ &
	\	LSET ER.RCV$=RECRD.LOG$ &
	\	LSET RECRD.ALW$=CVT%$(40%)
14520	NEXT W% &
	\ FIELD #8%,496% AS W$, 2% AS TOT.LOG$, 2% AS TOT.REC$, &
	2% AS TOT.USED$, 2% AS BYT.POS$, 2% AS TOT.LIM$ &
	\ LSET TOT.LOG$=RECRD.LOG$ &
	\ LSET TOT.REC$=RECRD.LOG$ &
	\ LSET TOT.USED$=CVT%$(2%) &
	\ LSET BYT.POS$=RECRD.LOG$ &
	\ LSET TOT.LIM$=CVT%$(30%) &
	\ J%=-1% &
	! OPEN THE CRASH ERROR LOG FILE. &
	! OPEN THE NULL DEVICE TO FIELD/UPDATE THE GENERAL CONTROL &
	! RECORD. &
	! INIT THE HEADER RECORD. &
	! CLEAR THE LOOP CONTROLLER. &
	! &
	! THE ROUTINES WHICH FOLLOW EXTRACT INFO FROM PENDING MESSAGE BLOCKS. &
	! &
	&
	&
	!	P E N D I N G    M E S S A G E    B L O C K &
	&
	&
	&
	!	  CONTENTS		OFFSET &
	!	   P$LINK		  0	LINK TO NEXT BLOCK &
	!	   P$BUFA		  2	"CONTORTED" BUFFER ADDRESS &
	!	 P$SNDR - P$TYPE	  4	JOB #*2 OF SNDR - MSG TYP CODE &
	!	   P$SPPN		  6	PPN OF SENDER &
	!	  RESERVED		  8	RESERVED &
	!	   P$BREM		 10	BYTES REMAINING IN DATA MSG &
	!	   P$PARM		 12	20. BYTES OF PARAMETERS &

14530	WHILE J%<>0% &
	\	J%=FNP%(PMB%) &
	\	BUF.ADDR%=FNP%(PMB%+2%) &
	\	GOTO 14580 IF (BUF.ADDR% AND 31%)<>0% &
	\	BUF.ADDR%=BUF.ADDR%+FNP%(BUF.ADDR%+2%) &
	\	L%=FNP%(PMB%+10%) &
	\	L0%=L%+2%+20% &
	\	P$="" &
	\	REPEAT%=(SWAP%(FNP%(PMB%+14%)) AND 255%) &
	\	B.OF%=12% &
	\	SENDER%=(SWAP%(FNP%(PMB%+4%)) AND 255%) &
	\	ER.COD%=(FNP%(PMB%+12%) AND 255%) &
	\	ER.COD%=55% UNLESS (SENDER%=0%) OR (ER.COD%=56%) &
	\	GOTO 14540 UNLESS ER.COD%=55% &
	\	REPEAT%=0% &
	\	PPN%=FNP%(PMB%+6%) &
	\	ER.DAT%=FNP%(512%) &
	\	ER.TIM%=FNP%(514%) &
	\	P$=CHR$(ER.COD%)+CHR$(SENDER%)+ &
		CHR$(PPN% AND 255%)+CHR$(SWAP%(PPN%) AND 255%)+ &
		CHR$(0%)+CHR$(0%)+ &
		CHR$(ER.DAT% AND 255%)+CHR$(SWAP%(ER.DAT%) AND 255%)+ &
		CHR$(ER.TIM% AND 255%)+CHR$(SWAP%(ER.TIM%) AND 255%)+ &
		CHR$(0%)+CHR$(0%) &
	\	B.OF%=24% &
	! PLOW THROUGH ALL PENDING MESSAGE BLOCKS: &
	!	J%=LINK TO NEXT PENDING MESSAGE BLOCK &
	!	BUF.ADDR%=POINTER TO BUFFER HOLDING MESSAGE DATA &
	!	   CHECK TO SEE IF EXTENDED MESSAGE - IF SO, SKIP IT. &
	!	   IF IT'S OK, UTILIZE THE OFFSET TO GET TO THE REAL &
	!	   BEGINNING OF THE DATA. &
	!	L%=LENGTH OF MESSAGE &
	!	L0%=L%+2 BYTE LENGTH FIELD+20 BYTES OF PARAMETERS &
	!	EXTRACT ERROR CODE AND REPEAT COUNT &
	!	NORMALLLY THE PARAMETER PORTION (12-30) CONTAINS &
	!	JOB*2 - CODE		12 &
	!	REPEAT - LAST FIELD	14 &
	!	   SEQUENCE		16 &
	!	   ERROR DATE		18 &
	!	   ERROR TIME		20 &
	!	   ERROR SECS.		22 &
	!	 REST OF PARAM		24-30 &
	!	ALL THIS IS EXTRACTED AND STORED IN THE ERROR FILE. &
	!	EXTRACT THE SENDING JOB #(*2) - IF IT IS NOT ZERO (MONITOR) &
	!	CONVERT TO CODE=55 AND DO SOME SPECIAL SETUP. &
	!	SEND JOB*2 - '55'	12 &
	!	SEND PPN		14 &
	!	SEQ=0			16 &
	!	   CRASH DATE		18 &
	!	   CRASH TIME		20 &
	!	   0			22 &
	!	REST OF PARAM		24-30 &
	&

14540		FOR I%=B.OF% TO 30% STEP 2% &
	\		K%=FNP%(PMB%+I%) &
	\		P$=P$+CHR$(K% AND 255%)+CHR$(SWAP%(K%) AND 255%)
14550		NEXT I% &
	\	FOR I%=0% TO L%-2% STEP 2% &
	\		K%=FNP%(BUF.ADDR%+I%) &
	\		P$=P$+CHR$(K% AND 255%)+CHR$(SWAP%(K%) AND 255%)
14560		NEXT I% &
	\	E%=128% UNLESS ER.COD%<62% &
	\	GOTO 14600 IF E% &
	\	FIELD #8%,ER.COD%*8% AS W$,2% AS RECRD.LOG$, &
			2% AS ER.LOG$,2% AS ER.RCV$, &
			2% AS RECRD.ALW$ &
	\	LSET ER.RCV$=CVT%$(CVT$%(ER.RCV$)+REPEAT%+1%) &
	\	LSET TOT.REC$=CVT%$(CVT$%(TOT.REC$)+REPEAT%+1%) &
	\	GOTO 14580 IF (CVT$%(RECRD.LOG$)>=CVT$%(RECRD.ALW$)) OR &
		(CVT$%(TOT.USED$)>=CVT$%(TOT.LIM$)) &
	\	B%=CVT$%(TOT.USED$) &
	\	O%=CVT$%(BYT.POS$) &
	\	IF (512%-O%)<L0% THEN &
			B%=B%+1% &
	\		O%=0%
14570		GET #2%,RECORD B% UNLESS O%=0% &
	\	D%=512%-L0%-O% &
	\	FIELD #2%,O% AS W$,L0% AS M0$, D% AS D9$ &
	\	LSET D9$=STRING$(D%,0%) IF O%=0% &
	\	LSET M0$=CVT%$(L0%)+P$ &
	\	PUT #2%, RECORD B% &
	\	LSET TOT.USED$=CVT%$(B%) &
	\	LSET BYT.POS$=CVT%$(O%+L0%) &
	\	LSET RECRD.LOG$=CVT%$(CVT$%(RECRD.LOG$)+1%) &
	\	LSET ER.LOG$=CVT%$(CVT$%(ER.LOG$)+REPEAT%+1%) &
	\	LSET TOT.LOG$=CVT%$(CVT$%(TOT.LOG$)+REPEAT%+1%)
14580		PMB%=J%
14590	NEXT &
	\ PUT #2%+SWAP%(8%),RECORD 1% &
	!	FROM THESE, EXTRACT THE ERROR CODE (ER.COD%) AND THE &
	!	REPEAT COUNT &
	!	GET OUT IF ILLEGAL CODE OR FNP% ERROR. &
	!	BUMP THE # OF ERRORS RECEIVED FOR THIS TYPE AND THE &
	!	TOTAL # OF ERRORS RECEIVED. &
	!	B%=BLOCK # &
	!	O%=OFFSET IN B% WHERE NEXT RECORD STARTS. &
	!	IF THERES NO ROOM FOR THE RECORD THEN &
	!		BUMP THE RECORD # AND RESET O% TO RECORD &
	!		BEGINNING GET THE RECORD IF NECESSARY. &
	! &
	!	FIELD THE RECORD, ZEROING REMAINDER OF RECORD IF THIS &
	!	IS THE FIRST ERROR RECORD IN THE BLOCK. &
	!	SAVE THE MESSAGE AND THE RECORD. &
	!	UPDATE ALL TOTALS &
	!	POINT TO THE NEXT PENDING MESSAGE BLOCK (IF WE JUST DID &
	!	THE LAST ONE, WE'LL DROP THROUGH). &
	! END OF LOOP &
	! &
	! FINALLY, SAVE THE TOTALLY UPDATED CONTROL RECORD
14600	CLOSE 2% &
	\ ON ERROR GOTO 19000 &
	\ GOTO 14610 IF E%=0% &
	\ PRINT #1%,C$;"Invalid Data" IF E%=128% &
	\ PRINT #1%,"Crash Error Log File will be Zeroed" &
	\ S$=CR.LOGFIL$ &
	\ OPEN S$ FOR OUTPUT AS FILE 2% &
	\ CLOSE 2% &
		! CLOSE THE FILE. &
		! IF ANY KIND OF ERROR, LET THE USER KNOW WE &
		! WILL TRY TO ZERO THE FILE. &

14610	RETURN &
	&

14620	E%=ERR &
	\ ON ERROR GOTO 19000 &
	\ RPT%=0% &
	\ PRINT #1%,C$;FNE$(E%);" (at line ";NUM1$(ERL);")" &
	\ RESUME 14600 &
		! SPECIALLY TRAP ERRORS IN THE EXTRACTION ROUTINE. &
		! INDICATE THAT WE WON'T BE CHAINING TO ERRDIS. &

15000	! &
	&
	&
	!	F U N C T I O N S &
	&
	&

15100	DEF* FNBITZ%(X%,H%,L%) &
		! Function to create a mask for bit fields, &
		! given the word and the boundary bits &
		! We give it a word, an upper bit, and a lower bit, &
		! and it shifts the bit range down so lower bit = 0 then &
		! returns us a word whose value = value of bit0 to high bit &
	\ FNBITZ%=0% &
	\ BIT15%=X%<0% AND H%=15% &
		! Set a flag if bit 15 is on and we need it &
	\ X9%=(X% AND 32767%)/(2%^L%) &
		! Shift all but bit 15 down to bit 0 &
	\ H%=H%-L% &
		! Shift the upper limit down by same amount &
	\ X9%=X9% AND (2%^(H%+1%))-1% &
		! Mask off the upper bits &
	\ X9%=(X9% OR 2%^H%) IF BIT15% &
		! OR in the high limit bit if appropriate &
	\ FNBITZ%=X9% &
	\ FNEND &

15150	DEF* FNOO$(X%) &
	\ FNOO$="OFF" &
	\ FNOO$="ON" IF X% &
	\ FNEND &

15160	DEF* FNF22$(PJCR6%,JCR,Z%) &
	\ FNF22$="" &
	\ J64%=(JCR+Z%)/64% &
	\ R64%=JCR+Z%-(J64%*64.) &
	\ JCRADD%=J64%+PJCR6% &
	\ JCRADD$=FNO$(JCRADD%)+RIGHT(FNO$(R64%),5%) &
	\ FNF22$=STRING$(8%-LEN(JCRADD$),ASCII("0"))+JCRADD$ &
	\ FNEND &

15200	DEF* FNB$(Q%) &
	\ FNB$="" &
	! FUNCTION	FNB$	GET A 3 POSITION OCTAL STRING &

15210	Q%=Q% AND 255% &
	\ Q0%=64% &
	\ Q$="" &
	\ WHILE Q0%>0% &
	\	Q1%=Q%/Q0% &
	\	Q%=Q%-Q0%*Q1% &
	\	Q0%=Q0%/8% &
	\	Q$=Q$+CHR$(48%+Q1%) &
	\ NEXT &
	! CONVERT THE NUMBER TO OCTAL, SAVING EACH DIGIT &

15230	FNB$=Q$ &
	! SET THE FUNCTION &

15240	FNEND &
	&

15300	DEF* FNN$(S%,N%) &
	\ FNN$="" &
	! FUNCTION	FNN$	TURN N% INTO A STRING AND LEFT PAD &
	!			THE STRING WITH SPACES &

15310	S$=NUM1$((N% EQV 32767%)+32768.) &
	\ FNN$=FNS$(S%) &
	! GET THE STRING (MAKE SURE IT'S POSITIVE) AND LEFT PAD IT. &

15320	FNEND &

15400	DEF* FNO$(Q%) &
	\ FNO$="" &
	! FUNCTION	FNO$	GET A 6 POSITION OCTAL STRING &

15410	Q$="0" &
	\ Q0%=4096% &
	\ IF Q%<0% THEN &
		Q$="1" &
	\	Q%=Q%+32767%+1% &
	! SET UP TO CONVERT THE NUMBER &
	! TAKE CARE OF NEGATIVE NUMBERS &

15420	WHILE Q0%>0% &
	\	Q1%=Q%/Q0% &
	\	Q%=Q%-Q0%*Q1% &
	\	Q0%=Q0%/8% &
	\	Q$=Q$+CHR$(48%+Q1%) &
	\ NEXT &
	! CONVERT THE NUMBER TO OCTAL, SAVING EACH DIGIT &

15430	FNO$=Q$ &
	! SET THE FUNCTION &

15440	FNEND &
	&

15500	DEF* FNP%(Q%) &
	! FUNCTION	FNP%	THIS FUNCTION IS EQUIVALENT &
	!			TO A PEEK OF AN ADDRESS &
	! &

15510	FNP%,E%=0% &
	\ IF (Q% AND 1%)<>0% THEN &
		E%=-1% &
	\	PRINT #1%, C$;"Odd PEEK Address - ";NUM1$(FNF(Q%)) &
	\	GOTO 15560 &

15520	R=FNF(Q% AND -2%) &
	\ IF R >= 0. AND R <= EPMM THEN &
		SEGMNT%=ROOT.SEG% &
	\	FNP%=FNPEEK%(SEGMNT%,R) &
	\	GOTO 15560 &
		! IF THE ADDRESS IS BETWEEN 0 AND $$EPMM THEN &
		! PEEK AT THE ROOT SEGMENT. &
		! &

15530	GOTO 15540 IF R > EPMM &
		  AND R < FPL.ST &
	\ IF FPLAP6% <> 0% THEN &
		IF R <= FPL.END THEN &
			SEGMNT%=FPL.SEG% &
	\		OFFSET=R-FPL.ST &
	\		R=((MAP%(FPL.SEG%,0%)-1%)*512.)+OFFSET &
	\		FNP%=FNPEEK%(SEGMNT%,R) &
	\		GOTO 15560 &
		! ERROR IF THE ADDRESS IS BETWEEN PERMANENTLY MAPPED &
		! MEMORY AND AND THE START ADDRESS OF FIP POOL. &
		! PEEK INTO THE FIP POOL IF WE HAVE A FIP POOL AND &
		! THE ADDRESS IS NOT BEYOND THE UPPER LIMIT. &

15540	E%=-1% &
	\ PRINT #1%, C$;"Address out of range - ";NUM1$(FNF(Q%)) &

15560	FNEND &
	&

15600	DEF* FNPEEK%(SEG%,AD) &
		! FUNCTION TO TRANSLATE A MEMORY ADDRESS TO &
		! BLOCK AND OFFSET IN A FILE OF INTEGERS. &

15610	FNPEEK%,E%=0% &
	\ E%=-1% IF SEG%<>0% AND MAP%(SEG%,0%)=0% &
	\ GOTO 15630 IF E% &
	\ START.ADDRS=(MAP%(SEG%,0%)-1%)*512. &
	\ END.ADDRS=MAP%(SEG%,1%)*512.+MAP%(SEG%,2%) &
	\ IF (AD<START.ADDRS) OR (AD>END.ADDRS) THEN &
		PRINT #1%, C$;"%Peek address ("; &
			   NUM1$(AD);") out of range, ("; &
			   NUM1$(START.ADDRS);" - "; &
			   NUM1$(END.ADDRS);")" &
	\ E%=-1% &
	\ GOTO 15630 &
		! INVALID ADDRESS &

15620	BASE.BLOCK%=MAP%(SEG%,0%) &
	\ AD=AD-START.ADDRS &
	\ ADD.BLOCK%=(AD/512%) &
	\ ADD.OFFSET%=AD-((ADD.BLOCK%)*512.)+1% &
	\ GET #12%, BLOCK (BASE.BLOCK%+ADD.BLOCK%) &
	\ FNPEEK%=SWAP%(CVT$%(MID(D$,ADD.OFFSET%,2%))) &

15630	FNEND &

15690	DEF* FNF(QQ%)=(QQ% AND 32767%) - (32768. * (QQ% < 0%)) &

15700	DEF* FNE$(E8%)= &
	CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(E8%)),3%),4%) &
	! EXTRACT AN ERROR MESSAGE. &

15750	DEF* FNJCR.P%(S.L%,AD,Q%) &
	\ IF MAP%(JCTRL.SEG%,0%)=0% THEN E%=-1% &
	\	GOTO 15754 &

15752	AD=AD+((MAP%(JCTRL.SEG%,0%)-1%)*512.) &
	!\ S%,FNJCR.P%=FNPEEK%(S.L%,JCTRL.SEG%,AD) &
	\ S%,FNJCR.P%=FNPEEK%(JCTRL.SEG%,AD) &
	!\ S%=FNP%(S.L%,S%) UNLESS Q% &
		! Set the segment # for this address &
	\ S%=FNP%(S%) UNLESS Q% &
		! Set the segment # for this address &

15754	FNEND &

15800	DEF* FNS$(S%)=SPACE$(S%-LEN(S$))+S$ &
	! LEFT PAD A STRING WITH SPACES. &
	&

15900	DEF* FNR7R%(Q%)=(SWAP%(Q%)*2%) OR ((Q% AND 128%)/128%) &
	&
	! ROTATES A 16 BIT INTEGER RIGHT 7 PLACES. &
	! IT'S FOR TURNING A CONTORTED ADDRESS INTO AN MMU ADDRESS. &
	! IF YOU ADD 6 MORE BITS TO THE LOW ORDER END OF THE MMU ADDRESS, &
	! YOU END UP WITH A 22-BIT PHYSICAL XBUF ADDRESS. &

16000	DEF* FNR7L%(Q%)=SWAP%((Q% AND 32767%)/2%+(16384% AND (Q%<0%))) &
		OR (128% AND (Q% AND 1%)<>0%) &
		! THIS ONE LEFT ROTATES A 16=BIT INTEGER. &
		! SHIFT IT RIGHT ONE POSITION, SAVE THE SIGN BIT, SWAP IT, &
		! AND REPLACE THE 1 BIT IN 128 &
		! TRY IT YOURSELF.  MAYBE I'M WRONG. &

19000	! &
	&
	&
	!	E R R O R    H A N D L I N G &
	&
	&
	RESUME 19010 &

19010	IF ERR=11% AND ERL=12540% &
	THEN PRINT #1%, C$;C$;C$;"End of file reached on CRASH file." &
	\ BL%=END.BLOCK% &
	\ GOTO 12544 &

19020	IF ERR=60% AND ERL=12520% &
	THEN GOTO 12530 &
	&
	! HANDLE INTEGER OVERFLOW ERROR DURING CORE DUMP LOOP. &

19030	E$=" (at Line "+NUM1$(ERL)+")" &
	\ E$=E$+" - "+S$ &
		IF ERL=1400% OR ERL=1420% OR ERL=14510% OR ERL=14600% &
	\ PRINT "?ANALY3 - ";FNE$(ERR);E$ &
	\ GOTO 32765 &
	! ONLY EXPECTED, SPECIALLY FLAGGED ERROR IS OPEN ERROR. &
	! FLAG ALL OTHERS WITH LINE # TO AID IN TRACK-DOWN OF REASON. &

31000	! &
	&
	&
	!	C H A I N    E N T R Y &
	&
	&

31010	ON ERROR GOTO 19000 &
	\ E0%=2% &
	\ S$=SYS(CHR$(7%)) &
	\ DIAGNOSE%=CVT$%(S$) &
	\ WORK1.FILE$=RIGHT(S$,3%) &
	\ GOTO 1000 &
	! CHAIN ENTRY FROM ANALYS &
	! SET UP ENTRY TYPE, EXTRACT ALL REQUIRED INFO FROM CORE COMMON &
	! AND GOTO WORK. &

32760	S$=SYS(CHR$(9%)) &
		! REMOVE OURSELVES FROM MEMORY &

32765	CLOSE 1%,12% &
		! CLOSE any open files &

32767	END &

