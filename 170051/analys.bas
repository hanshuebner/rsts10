2!		PROGRAM		: ANALYS.BAS
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
	! &
	! 9.0-14/KCG	14-Apr-85	Corrected DECnet/E disabled bug &
	! 9.3		11-NOV-86	CORRECT PHASE PHYS ADDR BUG &
	! 9.4/REG	03-APR-87	EXPAND SYMBOL ARRAYS &
	! 9.4/REG	15-APR-87	ADD BN'S DECNET CHANGES &
	! 9.4/REG	18-MAY-87	EXPAND SILMOD ARRAY TO 50 ENTRIES &
	!				AND CHANGE CODE TO HANDLE N RECORDS &
	! 9.6/REG	21-DEC-87	Expand A%() array for 47 phases &
	! 9.6/REG	28-JAN-88	Put CSRTBL in m2(7) where it belongs &
	! 9.6/REG	15-MAR-88	Remove DEVTBL and DEVEND from DATA. &
	! 9.6/REG	07-APR-88	Merge FK's changes &
	! 9.7/REG	21-MAR-89	Enlarge SILMOD array from 50 to 255. &
	!				Enlarge A%() array from 1023 to 4095. &
	! 9.7/FEK	27-Mar-89	Enlarge DEC%(), Update DECnet stuff &
	!				and enlarge annotation arrays &
	! 9.7/REG	18-Apr-89	Add JCMFLG, SYSTAK symbols &
	! 10.0/REG	14-Nov-89	Add CHECTL and CHENUE symbols (cache) &
	! 10.0/REG	07-May-90	Add JOB Header, FIJOB Header, CRAAP5 &
	! 10.0/REG	08-Jun-90	Fix JOB/FIJOB header &
	&

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&
   !	 ANALYS is a crash dump analyzer program  which  provides  the &
   !	 system manager with the means to decode/retain information in &
   !	 the CRASH.SYS file. &
   !	&
   !	 After looking up lots of symbols, outputting a temp file with &
   !	 the  STB  stuff (if  requested), and  printing the X.CON info &
   !	 ANALYS chains to ANALY1, which then outputs the equivalent of &
   !	 both a SYSTAT report (/-M) and NCP report, ANALY1 then chains &
   !	 to ANALY2, which figures out the annotations and puts them in &
   !	 a temp file.  ANALY2 then  chains to  ANALY3 which prints the &
   !	 receiver info, a core  dump,  extracts/formats  messages  for &
   !	 ERRCPY  and appends a full crash error log file report to the &
   !	 regular ANALYS report. &
   !	&
   !	 A short report (no core dump of monitor) may be  obtained  by &
   !	 specifying  "/NODUMP"  after the output file name.   A medium &
   !	 length  report  (no XBUF dump) may be obtained by  specifying &
   !	 "/NOXBUFDUMP" after the output file name. &
   !	&
   !	 An  annotated  core  dump (both octal and annotations) may be &
   !	 obtained by not specifying "/NARROW" or by specifying "/WIDE" &
   !	 after the output file name. /WIDE is the default. &
   !	&
   !	 "/NARROW" is included to allow a user to override the default &
   !	 of /WIDE, and produce a dump that will fit in 80 columns. &
   !	&
	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&
   !	CHANNEL #		USED FOR &
   !	   1			OUTPUT &
   !	   3			.SIL FILE &
   !	   8			CRASH ERROR LOG FILE &
   !	   9			WORK2-FILE &
   !	  10			WORK1-FILE &
   !	  11			WORK-FILE &
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
   !	A%()		USED TO EXTRACT .SIL FILE INFO &
   !	AD		USED IN FNPEEK FUNCTION &
   !	ADD.BLOCK%	USED TO CALCULATE PEEK BLOCK NO. &
   !	ADD.OFFSET%	USED TO CALCULATE PEEK BYTE NO. &
   !	ADDR%		USED TO EXTRACT/STORE PATCH INFO &
   !	ADR%		HOLDS NUMBER VALUE IN FNPRINT FUNCTION &
   !	ANNOT%		COUNT OF ANNOTATION SYMBOLS &
   !	B%		UTILITY VARIABLES &
   !	BASE.BLOCK%	USED TO CALCULATE PEEK BLOCK NO. &
   !	BLK%		USED TO EXTRACT PATCH INFO &
   !	BYT.OFF%	USED TO EXTRACT PATCH INFO &
   !	C$		CARRIAGE RETURN/LINE FEED &
   !	CC%()		HOLDS MISC. INFO IN WORK FILE &
   !	CR.LOGFIL$	CRASH ERROR LOGGING FILE (ERRCRS.FIL) &
   !	D$		IO BUFFER FOR CH 12 &
   !	D.UMP%		CORE DUMP/NODUMP INDICATOR &
   !			0 = no core dump &
   !			1 = include a core dump &
   !			2 = include a core dump, but not the XBUF dump &
   !	DEC%(),DEC$()	USED TO EXTRACT DECNET/E INFO &
   !	DECNET%		COUNT OF SYMBOLS NEEDED FOR DECNET/E &
   !	DEF.WIDTH$	DEFAULT WIDTH &
   !	DEVCNT.OFF%	USED TO TRACT DOWN DMC'S &
   !	DEVNAM.OFF%	USED TO TRACT DOWN DMC'S &
   !	DIAGNOSE%	USED FOR MAINTENANCE &
   !	DIFSIL%		SET IF /SIL: WAS SPECIFIED &
   !	E$		ERROR MESSAGE HOLDER &
   !	E%		ERROR VARIABLE &
   !	E8%		HOLDS ERROR CODE FOR FNE$ FUNCTION &
   !	ENTRY.TYP%	TYPE OF ENTRY TO PROGRAM &
   !	EPMM		$$EPMM &
   !	FILE.NAME$()	HOLDS NAMES OF FILES IN WORK FILE &
   !	FPL.SEG%	FIP POOL DUMP SEGMENT NUMBER &
   !	FORM.WIDTH%	WIDTH OF LISTING &
   !	FPLAP6%		MAPPING ADDRESS OF FIP &
   !	FPL.ST%		FIRST VALID VIRTUAL ADDRS OF FIP POOL >= 140000 OCTAL &
   !	FPL.ST		FPL.ST% AS AN UNSIGNED INTEGER. &
   !	FPL.END%	LAST VALID VIRTUAL ADDRS OF FIP POOL <= 160000 OCTAL &
   !	FPL.END		FPL.END% AS AN UNSIGNED INTEGER. &
   !	FXMONS%		LENGTH OF MONITOR SEGMENT &
   !	G%(),G$(),G0%(),G0$()	HOLD SYMBOL AND ANNOTATION INFO &
   !	H0%		OUTPUT DEVICE HANDLER INDEX &
   !	I$		VERSION/EDIT # &
   !	I%		UTILITY VARIABLE &
   !	I0$		INPUT FILE &
   !	I3%		LOOP VARIABLE IN SYMBOL LOOK UP ROUTINE &
   !	I9$		FIELD IN .SIL RECORD &
   !	J%,J0%,J1%,J2%,J3%,J4%	UTILITY VARIABLES &
   !	JCTRL.SEG%	JOB CONTROL REGION DUMP SEGMENT NUMBER &
   !	K%		UTILITY VARIABLE &
   !	K.B%		FLAGS OUTPUT TO KB: &
   !	LLT.FLG%	LOW ORDER LLT.FLG% BITS OF AN LLA CONTAIN THE LINK # &
   !	LNKMAX%		MAXIMUM # OF LINKS ALLOWED ON THE SYSTEM &
   !	M$()		MONITOR TABLE NAMES, PART I &
   !	M%()		MONITOR TABLES, PART I &
   !	M2$()		MONITOR TABLE NAMES, PART II &
   !	M2%()		MONITOR TABLES, PART II, PLUS EXTRAS &
   !	MAIN%		COUNT OF SYMBOLS WE NEED TO FIND/STORE IN M%() &
   !	MAIN2%		COUNT OF SYMBOLS WE NEED TO FIND/STORE IN M2%() &
   !	MAP%()		HOLDS CRASH FILE LAYOUT INFORMATION &
   !	MAX.SEG%	HIGHEST DUMP SEGMENT NUMBER, CURRENTLY = JCTRL.SEG% &
   !	MSCP.SEG%	MSCP REGION DUMP SEGMENT NUMBER &
   !	NEXT.BLOCK%	POINTS TO NEXT DEVICE CLUSTER IN CRASH FILE &
   !	NUM.DECNT.UNITS%	TOTAL NUMBER OF DECNET UNITS &
   !	NUM.MOD%	NUMBER OF MODULES IN .SIL &
   !	O0$		OUTPUT FILE &
   !	OCT.ST%		OCTAL STATUS DUMP/NO OCTAL STATUS DUMP &
   !	OFFSET%		BYTES IN LAST BLOCK OF SEGMENT &
   !	OVRFF%		OVR FLIP-FLOP TELLS WHEN WE HAVE DONE OVR &
   !	P2%		DEVTBL POINTER FOR PK'S &
   !	P3%		KB # OF THE KB FOR PK0 &
   !	PAT%(),PAT$()	USED FOR INSTALLED PATCHES &
   !	PATCH%		COUNT OF INSTALLED PATCHES &
   !	PKG.LOC$	ERROR PACKAGE LOCATION &
   !	Q$		OCTAL STRING &
   !	Q%		ADDRESS TO 'PEEK' AT IN FNP% &
   !	Q0%,Q1%,Q1$,QQ%,QTEMP$ UTILITY VARIABLES &
   !	R		FLOATING POINT ADDRESS &
   !	R.T$		XLATE STRING FOR RAD50 COMPARISONS &
   !	ROOT.SEG%	MONITOR ROOT DUMP SEGMENT NUMBER &
   !	S$		STRING TO BE PRINTED, UTILITY VARIABLE &
   !	S%		USED IN STRING PADDING FUNCTIONS &
   !	S0$		UTILITY STRING &
   !	SE.STB%,SE.STB0%,SE.STN.BLK%,SE.STN.BLK0%,SE.VAL% &
   !			USED WHILE SCURRYING THROUGH THE SYMBOL TABLE &
   !	SEG%		CURRENT SEGMENT NUMBER IN CRASH FILE &
   !	SIL.FILE$	NAME OF .SIL FILE &
   !	SIL.MOD%(),SIL.PAT%() HOLD SIL INFO &
   !	STB%		REMEMBERS WHETHER OR NOT TO DUMP SYMBOL TABLE &
   !	T$		TAB &
   !	TEMP$		THROW AWAY VARIABLE &
   !	TXT$		USED IN FNPRINT &
   !	TYP%		USED IN FNPRINT &
   !	WIDE%		<>0 MEANS DO AN ANNOTATED LISTING &
   !	WIDTH%		NEVER FOUND OUT WHAT THIS WAS USED FOR &
   !	WORD.OFF%	USED TO EXTRACT PATCH SPACE &
   !	WORK.FILE$,WORK1.FILE$,WORK2.FILE$ NAMES OF WORK FILES &
   !	XBUF.SEG%	XBUF DUMP SEGMENT NUMBER &
   !	X.TAB%		POINTS TO BEGINNING OF SEGMENT TABLE (X.TAB) &
   !	XCON%		SYSTEM CONFIGURATION WORD &
   !	XM0.DDB.PTR%	XM0 DDB POINTER &
   !	XSCS%		ORIGINAL CRASH FILE DISK DEVICE CLUSTER SIZE &
   !	XTAB%()		OUR OWN MAP OF THE CRASH FILE SEGMENTS &
   !	XTABS%		NUMBER OF ENTRIES IN X.TAB &
   !	Z%(),Z$,Z1$	UTILITY VARIABLES &
	&

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&
	&
   !	FUNCTION		USE &
   !	&
   !	FNE$			EXTRACT ERROR TEXT &
   !	FNF			CONVERT INTEGER TO FLOATING POINT &
   !	FNO$			MAKE 6 POSITION OCTAL STRING FROM INTEGER &
   !	FNP%			CORRESPONDS TO A 'PEEK' (ACCEPTS INTEGER) &
   !	FNPEEK%			DOES THE ACTUAL PEEK FROM SEG# AND FP ADDRESS &
   !	FNPRINT$		PUTS SYMBOLS TO FILE &
   !	FNS$			LEFT PAD A STRING WITH SPACES &
   !	&
   !	SUBROUTINE LINES	USE &
   !	14100			COMMON PRINT ROUTINE &
   !	14500			FIELD FILE INFO &
   !	14600			Get extra module info from SIL &

900	DIM A%(1536%),M$(80%),M2$(80%),DEC$(40%), XTAB%(20%) &
		! &
		!	D I M E N S I O N    S T A T E M E N T S &
		! &
		! &
		! m%(A%()	USED TO EXTRACT .SIL INFORMATION &
		! &
	&

940	DIM #11%,	PAT%(17%),PAT$(17%)=8%, &
			SILMOD%(255%,20%), &
			SILPAT%(24%,63%), &
			G%(128%), G$(128%)=8%, &
			GX%(32767%,1%), DUPLST%(2000%,1%), GX$(15000%)=32% &
	\ DIM #10%,	M%(80%), M2%(80%), DEC%(40%), &
			CC%(30%), FILE.NAME$(5%)=32%, MAP%(10%,3%) &
	\ DIM #2%,	INIT%(32767%) &
		! &
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
		!	(,13)=SE.OFF (IN WORDS) &
		!	(,14)=0 &
		!	(,15)=0 &
		!	(,16)=0 OR VALUE OF XXXPAT &
		!		E.G., EMTPAT &
		!	(,17-20)=0 &
		! SILPAT(,) - XXXPAT CONTENTS (64. WORDS) &
		! G%(),G$() - POINTERS AND NAMES USED FOR ANNOTATION &
		! GX%(X%,Y%) - WHERE  X% = MEMORY ADDRESS &
		!		      Y% = 0% -> INTO ANNOTATION TABLE &
		!			   1% -> INTO DUPLICATE LIST &
		! DUPLST%(X%,Y%) -    X% = NEXT FREE ENTRY &
		!		      Y% = 0% -> INTO ANNOTATION TABLE &
		!			   1% LINK -> INTO DUPLST TABLE &
		! GX$(X%)	      X% = NEXT FREE ANNOTATION &
		! MONITOR TABLES - PART I STORED IN M%() &
		! WHERE POSSIBLE, THESE SUBSCRIPTS ARE USED FOR &
		! CONSISTENCY WITH SYSTAT - THOSE THAT DIFFER ARE *'D &
		! M%(1)		FIJBDA* &
		! M%(3)		FIJOB* &
		! M%(5)		DEVCNT &
		! M%(7)		DEVPTR &
		! M%(9)		SATBUF* - NOT USED (SEE CRASAV) &
		! M%(11)	JOBTBL &
		! M%(13)	JBSTAT &
		! M%(15)	JBWAIT &
		! M%(17)	UNTCLU &
		! M%(19)	UNTCNT &
		! M%(21)	SATCTL &
		! M%(23)	PATCH* &
		! M%(25)	SATCTM &
		! M%(29)	UNTOPT &
		! M%(31)	MEMLST &
		! M%(33)	(JCRSIZ) - SIZE OF JOB DATA STRUCTURE &
		! M%(35)	LATAP5 &
		! M%(37)	$$LATS &
		! M%(39%)	SYSTAK - System Stack &
		! M%(41%)	JCMFLG - Offset to job flag in JCR &
		! M%(43%)	CRAAP5 - To compare JOB/FIJOB header addrs &
		! &
		! M2%() - PACKAGE LOCATION UTILITY ARRAY (30) AND &
		!   MONITOR TABLES - PART II + EXTRAS &
		! M2%(1)	LRGFIL* &
		! M2%(3)	FREES &
		! M2%(5)	DEVNAM &
		! M2%(7)	CSRTBL &
		! M2%(9)	DEVOKB &
		! M2%(11)	TTYHCT &
		! M2%(13)	JOBCNT &
		! M2%(15)	RTSLST &
		! M2%(17)	ERLCTL &
		! M2%(19)	SNDLST &
		! M2%(21)	DSKLOG &
		! M2%(23)	DEVSYN &
		! M2%(25%)	($$JCR6) - START OF THE JOB CONTROL REGION &
		! M2%(27%) &
		! M2%(29%)	FCBLST &
		! M2%(31%)	$$EPMM - END OF R/W MON AREA, NOT USED &
		! M2%(33%)	X.TAB - TABLE OF DUMP SEGMENTS &
		! M2%(35%)	X.TABS - SIZE OF X.TAB &
		! M2%(37%)	X.CON - FLAG WORD, HAS MANY GOODIES &
		! M2%(39%)	X.MONS - SIZE OF R/W MON DUMP &
		! M2%(41%)	X.SCS - CLUSTER SIZE OF CRASH.SYS DISK &
		! M2%(43%)	CRASAV - POINTS TO BEGINNING OF SATBUF &
		! M2%(45%)	DDCTBL - POINTS TO BEGINNING OF DDCTBL &
		! M2%(47%)	UCTTBL - POINTS TO BEGINNING OF UCTTBL &
		! M2%(49%)	STSTBL - &
		! M2%(51%)	TTFMSB - FMS SUPPORT SYMBOL &
		! M2%(53%)	FIPOOL - ADDRESS OF FIP POOL &
		! M2%(55%)	SATEND - POINTS TO SAT END TABLE &
		! M2%(57%)      UNTLVL - UNIT LEVEL OF DISK &
		! M2%(59%)	LOWAD  - LOW ADDRESS OF MEMORY PARITY ERROR &
		! M2%(61%)	NULRTS - THE NULL RTS BLOCK ADDRESS &
		! M2%(63%)	$$CRSZ - (NOT USED - SEE X.MONS) &
		! M2%(65%)	CSR.KB - USED TO FIND PK JOB NUMBERS &
		! M2%(67%)	FAKDDB - used to find fake devices &
		! M2%(69%)	CHECTL - Points to cache information &
		! M2%(71%)	CHENUE - Points to more cache info &
		! M2%(73%)	CMT    - Points to MSCP data &
		! M2%(75%)	CMTCTL - Points to MSCP data &
		! M2%(77%)	UMT$DU - Points to MSCP data &
		! &
		! ITEMS NEEDED IF DECNET'S CONFIGURED &
		! DEC%(1%)	NSPCCB -- <> MEANS DECNET IS THERE &
		! DEC%(2%)	LNKMAX &
		! DEC%(3%)	NODLST -- <> MEANS NSP ENABLED &
		! DEC%(4%)	NSPLST &
		! DEC%(5%)	SIDBUF &
		! DEC%(6%)	NSPLLB &
		! DEC%(7%)	NSPCDB &
		! DEC%(8%)	LLTBUF -- <> MEANS NSP ENABLED &
		!			Contorted address of LLT buffer &
		! DEC%(9%)	NSPQUE &
		! DEC%(10%)	TRNQUE &
		! DEC%(11%)	CONQUE &
		! DEC%(12%)	NETPOL private buffer pool free list &
		! DEC%(13%)	IDLNOB Idle Node Block list &
		! DEC%(14%)	NSPQ2F Work blocks waiting for FIP &
		! DEC%(15%)	NSPFCQ Work blocks returned from FIP &
		! DEC%(16%)	ENDCHE MMU value of end-node cache &
		! DEC%(17%)	RTEMMU MMU address of cost/hops vector &
		! DEC%(18%)	OAJMMU MMU address of adjacency vector &
		! DEC%(19%)	MTXMMU MMU address of routing matrix &
		! DEC%(20%)	RTMSTA Routing message start address &
		! DEC%(21%) 	EVTMMU MMU value for Event logger database &
		! DEC%(22%)	ADJFLG Start of Adjacency MMU pointers &
		! DEC%(23%)	NSPJDB Pointer to NSP's JDB &

999	! &
	&
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	ON ERROR GOTO 19000 &
	\ S$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "Starting ANALYS - ";DATE$(0%);TIME$(0%); &
		"CPU time";TIME(1%) IF (DIAGNOSE% AND 64%) &
	\	STOP IF (DIAGNOSE% AND 128%) &
	! SET UP STANDARD ERROR TRAP &
	! PERMANENTLY DROP TEMPORARY PRIVILEGES &
	! RETURN KB TO LEFT MARGIN &

1010	I$="V10.1-A" &
		! SET UP VERSION/EDIT #. &

1020	PRINT "ANALYS";CHR$(9%);I$;CHR$(9%);FNE$(0%) &
	! PRINT THE SYSTEM HEADER &

1030	CHANGE SYS(CHR$(12%)) TO A% &
	\ PKG.LOC$="["+NUM1$(A%(6%))+","+NUM1$(A%(5%))+"]" &
	\ PKG.LOC$="_"+CHR$(A%(23%))+CHR$(A%(24%))+NUM1$(A%(25%))+":"+ &
		PKG.LOC$ IF A%(26%) AND 1% &
	\ IF A%(3%)+SWAP%(A%(4%))<>15%*2% THEN &
		PRINT "?Please 'RUN ANALYS'" &
	\	GOTO 32767 UNLESS DIAGNOSE% &
	! BUILD NAME OF DEVICE AND ACCOUNT OF LAST OPENED FILE. &
	! IF WE DIDN'T COME FROM A COMPILED FILE WE CANN'T BE &
	! SURE THAT THIS NAME IS REALLY OUR PACKAGE LOCATION. &
	! SO TELL THEM AND EXIT. &

1050	GOTO 1060 IF ((DIAGNOSE% AND 1%)=0%) AND ((DIAGNOSE% AND 512%)<>0%) &
	\ A%(I%)=0% FOR I%=0% TO 30% &
	\ PRINT "Input <[0,1]CRASH.SYS>"; &
	\ INPUT LINE I0$ &
	\ I0$=CVT$$(I0$,-2%) &
	\ I0$="[0,1]CRASH.SYS" UNLESS LEN(I0$) &
	\ I%=INSTR(1%,I0$,"/SIL:") &
	\ DIFSIL%=I% &
	\ IF I% THEN &
		S$=RIGHT(I0$,I%+5%) &
	\	I0$=LEFT(I0$,I%-1%) &
	\	I0$="[0,1]CRASH.SYS" UNLESS LEN(I0$) &
	\	S0$=I0$+"/SIL:"+S$ &
	! RE-ZERO THE MONITOR TABLE II ARRAY. &
	! GET THE CRASH AND SIL FILE TO BE USED BY ANALYS. &
	! DEFAULT IS [0,1]CRASH.SYS FOR THE CRASH FILE NAME. &
	! CHECK IF WANT TO USE A DIFFERENT SIL THEN THE INSTALLED ONE. &
	! SET THE DIFFERENT SIL FLAG.	 =0--INSTALLED SIL &
	!				<>0--USE A SIL THAT WAS SPECIFIED. &
	! IF A DIFERENT SIL WAS SPECIFIED THEN &
	!	SEPARATE THE 2 FILE NAMES (CRASH FILE AND SIL FILE) &
	!	DEFAULTING THE .SYS FILE IF NONE GIVEN (PROBABLY SHOULD &
	!	HAVE BEEN) &

1060	WORK1.FILE$="ANA1" &
		+RIGHT(NUM1$(100%+((PEEK(518%) AND 255%)/2%)),2%) &
		+".TMP" &
	! SET WORK FILE NAME TO ANA1+JOB NUMBER (ENSURE JOB NUMBER IS 2 DIGITS &
	\ GOTO 5010 IF ((DIAGNOSE% AND 1%)=0%) AND ((DIAGNOSE% AND 512%)<>0%) &
	\ OPEN WORK1.FILE$ FOR OUTPUT AS FILE 10%, MODE 256% &
	\ FXMONS%=-1% &
	\ EPMM=32767% &
		! ARBITRARY VALUE AT THIS TIME. &
		! REAL FXMONS COMES LATER, AND THE &
		! MAP ARRAY WILL BE UPDATED. &
	\ FPL.ST%=-16384% &
	\ FPL.ST=FNF(FPL.ST%) &
	\ FPL.END%=-8192% &
	\ FPL.END=FNF(FPL.END%) &
	\ ROOT.SEG%=0% &
	\ FPL.SEG%=1% &
	\ MSCP.SEG%=2% &
	\ XBUF.SEG%=3% &
	\ JCTRL.SEG%=4% &
	\ JHEAD.SEG%=5% &
	\ MAX.SEG%,FJHEAD.SEG%=6% &
		! SET UP DEFAULT LEGAL LIMITS FOR FIP POOL ADDRESSES. &
		! SET UP SEGMENT ORDER LABLES. &
	\ MAP%(ROOT.SEG%,0%)=1% &
		! THIS IS THE START OF THE MONITOR PART OF THE DUMP &
	\ MAP%(ROOT.SEG%,1%)=((FNF(FXMONS%)-1%)/512%)+1% &
		! ENDING BLOCK IS THE MONITOR SIZE (BYTES) DIVIDED BY &
		! 512 (BYTES/BLOCK). &
	\ MAP%(ROOT.SEG%,2%)=FNF(FXMONS%)-((MAP%(ROOT.SEG%,1%)-1%)*512.) &
		! ENDING OFFSET IS THE ACTUAL MONITOR SIZE MINUS &
		! THE NUMBER OF BYTES IN ALL THE FULL BLOCKS UP TO THERE. &
		! THIS IS THE END OF THE MONITOR PART OF THE DUMP &
	\ MAP%(ROOT.SEG%,3%)=0% &
		! THIS IS THE STARTING MEMORY ADDRESS OF THIS SEGMENT. &

1070	IF DIFSIL%=0% &
	    THEN &
		S$="SY0:[0,1]INIT.SYS/RO" &
	\	 OPEN "_"+S$ FOR INPUT AS FILE 2% &
	\	 I%=(INIT%(29%)/2%) AND 32767% &
	\	 S$="_SY0:[0,1]"+RAD$(INIT%(I%))+RAD$(INIT%(I%+1%))+".SIL" &
	\	 CLOSE 2% &
	\	 S0$=I0$+"/SIL:"+RIGHT(S$,2%) &
	\	 PAST.INIT%=-1% &
	! IF NO .SIL FILE WAS SPECIFIED. &
	!	SET S$=INIT.SYS IN CASE OF ERROR. &
	!	OPEN INIT.SYS &
	!	THE 30th WORD IN INIT.SYS CONTAINS A POINTER * 2 TO THE &
	!	LOCATION WITHIN INIT.SYS OF THE INSTALLED .SIL FILE. &
	!	CLOSE INIT.SYS AND SAVE THE NAME OF THE .SIL FILE &
	!	SET THE FLAG TO SAY WE DID THIS STUFF. &

1090	C$=CHR$(13%)+CHR$(10%) &
	\ OPEN S$ FOR INPUT AS FILE 3%, MODE 256%+8192% &
	\ SIL.FILE$=S$ &
	\ S$=I0$ &
	\ OPEN S$ FOR INPUT AS FILE 12%, MODE 256%+8192% &
	\ FIELD #12%, 512% AS D$ &
	\ DEF.WIDTH$="" &
	\ PRINT IF CCPOS(0%) &
	\ PRINT "Output <ANALYS.DMP<188>";DEF.WIDTH$;">"; &
	\ INPUT LINE O0$ &
	\ O0$=CVT$$(O0$,-2%)+DEF.WIDTH$ &
	\ STB%=(INSTR(1%,O0$,"/NOSTB")=0%) &
		! STB%=0%  MEANS THE "/NOSTB" SWITCH WAS USED &
		! STB%=-1% MEANS THE "/NOSTB" SWITCH WAS NOT USED &
	\ DUMP.REGRDLS%=(INSTR(1%,O0$,"/DUMP")<>0%) &
		! DUMP.REGRDLS%=0%  MEANS THE "/DUMP" SWITCH WAS NOT USED &
		! DUMP.REGRDLS%=-1% MEANS THE "/DUMP" SWITCH WAS USED &
	\ D.UMP%=ABS(INSTR(1%,O0$,"/NODUMP")=0%) &
		! D.UMP%=0%  MEANS THE "/NODUMP" SWITCH WAS USED &
		! D.UMP%=1%  MEANS THE "/NODUMP" SWITCH WAS NOT USED &
	\ D.UMP%=2% IF INSTR(1%,O0$,"/NOXBUF_DUMP")<>0% &
	\ D.UMP%=2% IF INSTR(1%,O0$,"/NOXBUFDUMP")<>0% &
		! D.UMP%=2%  Means the /NOXBUF_DUMP switch was used &
	\ D.UMP%=1% IF INSTR(1%,O0$,"/XBUF_DUMP")<>0% &
	\ D.UMP%=1% IF INSTR(1%,O0$,"/XBUFDUMP")<>0% &
	\ STB%=0% UNLESS D.UMP% &
	\ WIDE%=(INSTR(1%,O0$,"/NARROW")=0%) &
		! WIDE%=0%  MEANS THE "/NARROW" SWITCH WAS USED &
		! WIDE%=-1% MEANS THE "/NARROW" SWITCH WAS NOT USED &
	\ WIDE%=0% UNLESS D.UMP% &
	\ O0$=LEFT(O0$,INSTR(1%,O0$+"/","/")-1%) &
	\ O0$="ANALYS.DMP<188>" UNLESS LEN(O0$) &
	\ S$=O0$ &
	\ H0%=0% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+S$) TO A% &
	\ B%=A%(29%)+SWAP%(A%(30%)) &
	\ H0%=STATUS AND 255% IF (B% AND 4096%)<>0% AND B%>=0% &
	\ IF H0%=6% &
	    THEN &
		OPEN S$ AS FILE 1%, MODE 8192% &
	    ELSE &
		OPEN S$ FOR OUTPUT AS FILE 1% &
	! CREATE <CR> <LF> STRING &
	! TRY TO OPEN THE .SIL FILE (DEFAULT OR USER SPECIFIED) WITH CACHING. &
	! SAVE THE .SIL FILE NAME FOR ANALY1, ANALY2, AND ANALY3 &
	! SET THE DEFAULT INPUT FILE IF NONE WAS SPECIFIED. &
	! OPEN THE CRASH FILE TO VERIFY ITS EXISTENCE. &
	! GET AN ANALYS DUMP FILE NAME PLUS SWITCHES. &
	! SCAN FOR SWITCHES. &
	! STB% (PRINT SYMBOL TABLE) GETS RESET TO 0% IF IF D.UMP%=0% &
	! D.UMP% = 0%	DO NOT INCLUDE CORE DUMP OF THE MONITOR &
	! D.UMP% = 1%	INCLUDE A COMPLETE DUMP &
	! D.UMP% = 2%	Include a dump of all but XBUF &
	! WIDE% INDICATES THAT A 132-COLUMN DUMP IS DESIRED: &
	! CLEAR IT IF /NODUMP SPECIFIED (D.UMP=0) &
	! &
	! NOTE: THE DEFAULT WIDTH FOR DUMPS IS "WIDE" - AN OPTIONAL &
	!	PATCH MAY EXIST TO CHANGE THE DEFAULT TO "NARROW" &
	!	ALSO, /NODUMP OVERRIDES OTHER SWITCHES. &
	! GET THE OUTPUT SPEC. &
	! SET DEFAULT HANDLER INDEX TO DISK. &
	! IF POSSIBLE, RESET IT ACCURATELY. &
	! FOR LP, OPEN WITH NO FORM FEED ON CLOSE, OTHERWISE, NORMAL &
	! OPEN. &

1110	PRINT IF CCPOS(0%) &
	\ PRINT "Crash Error Log Filename <";PKG.LOC$;"ERRCRS.FIL>"; &
	\ INPUT LINE CR.LOGFIL$ &
	\ CR.LOGFIL$=CVT$$(CR.LOGFIL$,-2%) &
	\ K%=INSTR(1%,CR.LOGFIL$,"/DET") &
	\ CR.LOGFIL$=LEFT(CR.LOGFIL$,K%-1%)+RIGHT(CR.LOGFIL$,K%+4%) IF K% &
	\ CR.LOGFIL$=PKG.LOC$+"ERRCRS.FIL" UNLESS LEN(CR.LOGFIL$) &
	\ S$=CR.LOGFIL$ &
	\ OPEN S$ FOR OUTPUT AS FILE 8% &
	\ CLOSE 8% &
	\ PRINT "Detaching ...";C$;C$ IF K% &
	\ WORK.FILE$=SYS(CHR$(6%)+CHR$(7%)) IF K% &
	\ IF STB% THEN &
		K.B%=-1% IF INSTR(1%,O0$,"KB:")<>0% &
	\	FORM.WIDTH%=80% IF K.B% &
	\	FORM.WIDTH%=132% UNLESS K.B% &
	\	FORM.WIDTH%=80% IF WIDE%=0% &
	! GET THE FILE SPEC FOR THE CRASH ERROR LOG FILE &
	! CLEAN IT UP AND SEE IF THEY WANT TO DETACH &
	! OPEN IT TO INSURE LEGAL SPEC ETC., THEN CLOSE IT. &
	! DETACH THE JOB HERE IF THEY WANTED TOO. &
	! IF WE ARE DOING A SYMBOL TABLE DUMP THEN &
	!	SET DUMP TO KEYBOARD FLAG IF THEY WANTED &
	!	SET FORM WIDTH TO 80 COLUMNS IF DUMPING TO A KEYBOARD &
	! 	SET FORM WIDTH TO 132 COLUMNS IF NOT DUMPING TO A KEYBOARD &
	!	SET FORM WIDTH TO 80 COLUMNS IF NOT A WIDE DUMP &

3500	WORK.FILE$="ANAL"+RIGHT(NUM1$(100%+((PEEK(518%) AND 255%)/2%)),2%)+ &
		".TMP" &
	\ OPEN WORK.FILE$ FOR OUTPUT AS FILE 11%, MODE 256% &
	\ WORK2.FILE$="ANA2"+RIGHT(NUM1$(100%+((PEEK(518%) AND 255%)/2%)),2%)+ &
		".TMP" &
	\ OPEN WORK2.FILE$+"/CL:-16" FOR OUTPUT AS FILE 9%, MODE 256% IF STB% &
	\ GOTO 5000 IF (DIAGNOSE% AND 2%) AND NOT (DIAGNOSE% AND 4%) &
	\ M%(I%)=0% FOR I%=0% TO 80% &
	\ M2%(I%)=0% FOR I%=0% TO 80% &
	\ DEC%(I%)=0% FOR I%=0% TO 40% &
	\ GET #3%, RECORD 1% 	! GET FIRST BLOCK OF THE .SIL FILE &
	\ GOSUB 14500 	! CONVERT RECORD TO 256 WORDS OF A%(0%)-A%(255%) &
	\ NUM.MOD%=A%(0%) 	! NUMBER OF MODULES USED IN THIS .SIL FILE &
	\ SE.STB%=A%(6%)    ! STARTING BLOCK NUMBER OF SYMBOL TABLE OF 1ST MOD &
	\ SE.STN.BLK%=INT((A%(7%)/64.)+0.99)   ! NUMBER OF BLOCKS FOR TABLE &
	\ PRINT #9%, C$;C$;"Symbol Table List:";C$;C$ IF STB% &
	\ SILMOD%(0%,0%)=NUM.MOD% &
	\ SILMOD%(J%,I%-1%)=A%((J%-1%)*16%+I%) FOR I%=1% TO 14% &
					       FOR J%=1% TO 15% &
	! SORT THE SIL MODULES ENTRIES OF THE FIRST BLOCK INTO SILMOD ARRAY &
	\ GOSUB 14600 IF NUM.MOD%>15% &
	! DO THE SAME WITH THE SECOND BLOCK IF MORE THAN 15 MODULES ARE USED &
	\ SILMOD%(J%,I%)=0% FOR I%=15% TO 20% &
			    FOR J%=0% TO NUM.MOD% &
	! CLEAR THE UPPER PART OF THE SILMOD ARRAY FOR ALL MODULES &
	\ VERSION$=RAD$(SILMOD%(1%,2%))+LEFT(RAD$(SILMOD%(1%,3%)),1%) &
	\ V97%=(VERSION$>="09.7") &
		! Get the .SIL version number &
	\ P%=0% &
	\ R.T$=STRING$(32%,0%)+"@"+STRING$(3%,0%)+ &
		"["+STRING$(9%,0%)+"\"+ &
		CHR$(0%)+"^_`abcdefg"+STRING$(5,0%)+"]"+ &
		CHR$(0%)+"ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
	! RAD50 TRANSLATE STRING SO WE CAN DO ASCII COMPARES &
	\ FOR J%=1% TO NUM.MOD% &
	\	Q1$=CVT$$((RAD$(SILMOD%(J%,0%))+RAD$(SILMOD%(J%,1%))),-2%) &
		! GET A MODULE NAME &
	\	PATCH.FLG%=0% &
	\	OVRFF%=-1% IF Q1$="OVR" UNLESS V97% &
	\	OVRFF%=-1% IF Q1$="OV2" IF V97% &
	\	GOTO 4000 IF ((Q1$<>"OVR") AND (Q1$<>"OV2")) AND (OVRFF%<>0%) &
		! EXIT PRINTING SYMBOL TABLE STUFF AFTER MODULE "OVR" &
	\	Z$=FNPRINT$(Q1$,0%,1%) &
				IF STB% &
				IF LEN(Q1$) &
				IF ((DIAGNOSE% AND 63%)=0%) &
				OR (DIAGNOSE% AND 1%) &
				OR (DIAGNOSE% AND 4%) &
		! PRINT MODULE NAME IF WE ARE DOING A SYMBOL TABLE DUMP &
	\	FIP.LOAD.ADDRS%=SILMOD%(J%,7%) IF Q1$="FIP" &
		! GET LOAD ADDRESS OF FIP PHASE IF THE MODULE IS FIP &
	\	Q1$=Q1$+"PAT" &
	\	SE.STB0%=SILMOD%(J%,5%) &
		! STARTING BLOCK NUMBER FOR SYMBOL TABLE OF THIS MODULE &
	\	SE.STN.BLK0%=INT((SILMOD%(J%,6%)/64.)+0.99) &
		! NUMBER OF BLOCKS FOR THIS SYMBLE TABLE &
	\	GOTO 3540 IF (SE.STB0%=0%) OR (STB%=0% AND (J%=1% OR &
			LEN(Q1$)>6%)) &
		! GET NEXT MODULE IF NO SYMBOLS OR NOT DOING SYMBOL TABLES &
	\	FOR K%=1% TO SE.STN.BLK0% &
		! FOR ALL OF THE BLOCKS IN THE SYMBOL TABLE OF THIS MODULE &
	\		GOTO 3540 IF (STB%=0%) AND PATCH.FLG% &
	\		GET #3%,RECORD SE.STB0%+K% &
	\		GOSUB 14500 	! PUT BLOCK IN WORD ARRAY A%() &
	\		Q$=CVT$$(RAD$(A%(252%))+RAD$(A%(253%)),-2%) &
			! LAST SYMBOL NAME IN THIS BLOCK &
	\		GOTO 3530 IF (XLATE(Q1$,R.T$) > &
				XLATE(Q$,R.T$)) AND (STB%=0%) &
	\		FOR I3%=0% TO 63% &
			! FOR ALL OF THE SYMBOLS IN THIS BLOCK &
	\			Q$=CVT$$(RAD$(A%(I3%*4%))+ &
				   RAD$(A%(I3%*4%+1%)),-2%)  ! SYMBOL NAME &
	\			SE.VAL%=A%(I3%*4%+3%) 	     ! SYMBOL VALUE &
	\			Z$=FNPRINT$(Q$,SE.VAL%,2%) &
						IF STB% &
						IF LEN(Q$) &
						IF ((DIAGNOSE% AND 63%)=0%) &
						OR (DIAGNOSE% AND 1%) &
						OR (DIAGNOSE% AND 4%) &
				! PRINT SYMBOL NAME AND VALUE &
	\			GOTO 3520 IF J%=1% OR Q1$="OVRPAT" &
	\			GOTO 3520 UNLESS Q$=Q1$ &
	\			GOTO 3520 UNLESS SE.VAL% &
	\			PATCH.FLG%=-1% &
	\			SILMOD%(J%,16%)=SE.VAL% &
	\			BYT.OFF%= &
				((SE.VAL%-SILMOD%(J%,7%)) EQV 32767%)+32768. &
	\			BYT.OFF%=BYT.OFF%+(SILMOD%(J%,13%)*2%) &
	\			BLK%=(BYT.OFF%/512%) &
	\			WORD.OFF%=(BYT.OFF%-(BLK%*512%))/2% &
	\			BLK%=SILMOD%(J%,4%)+BLK%+1% &
	\			GOTO 3534 IF STB%=0% &

3520			NEXT I3% &
			! DO THE NEXT SYMBOL &

3530		NEXT K% &
	\	GOTO 3540 UNLESS PATCH.FLG% &
		! DO THE NEXT BLOCK OF SYMBOLS &

3534		GET #3%,RECORD BLK% &
	\	GOSUB 14500 &
	\	FOR ADDR%=0% TO 63% &
	\		IF WORD.OFF%>255% THEN &
				BLK%=BLK%+1% &
	\			GET #3%,RECORD BLK% &
	\			GOSUB 14500 &
	\			WORD.OFF%=0%
3536			SILPAT%(P%,ADDR%)=A%(WORD.OFF%) &
	\		WORD.OFF%=WORD.OFF%+1% &
	\	NEXT ADDR% &
	\	P%=P%+1% &

3540	NEXT J% &
	\ GOTO 5000 IF DIAGNOSE%=4%+256% &
	&
	! SAVE SIL DIRECTORY INFO. &
	! SCAN FOR MODULES WITH PATCH SPACE - FIND WHERE THE PATCH &
	! SPACE STARTS - GET/STORE 64 WORDS WORTH, READING A NEW BLOCK &
	! IF WE SHOULD SPAN A BLOCK. &

4000	SYM.FND%=-1% &
	\ G%(0%),DEC%(0%)=0% &
	\ PH.O%=-1% &
	\ READ MAIN%,MAIN2%,DECNET%,ANNOT% &
	\ M%(0%)=MAIN% &
	\ M2%(0%)=MAIN2% &
	\ PRINT #1%, C$;C$;CHR$(12%); &

4020	FOR K%=1% TO SE.STN.BLK% &
	\	GET #3%, RECORD SE.STB%+K% &
	\	GOSUB 14500 &
	\	Q0$=XLATE(CVT$$(RAD$(A%(0%))+RAD$(A%(1%)),-2%),R.T$) &
	\	Q$,Q9$=XLATE(CVT$$(RAD$(A%(252%))+RAD$(A%(253%)),-2%),R.T$) &
	\	FOR J%=0% TO 63% &

4030			IF SYM.FND% THEN &
			  READ Q1$,Q1%,Q2% &
	\		  SYM.FND%=0% &
	\		  GOTO 4130 IF Q1$="" &
	\		  GOTO 4040 IF (LEFT(Q0$,4%)<="DQS[" AND &
				LEFT(Q$,4%)>="DQS[") OR &
				(LEFT(Q0$,3%)<="[[0" AND &
				LEFT(Q$,3%)>="[[0") &
	!		NOTE: XLATE("DQS$",R.T$)="DQS[" &
	!		NOTE: XLATE("$$0",R.T$)="[[0" &
	\		GOTO 4125 IF XLATE(Q1$,R.T$)>Q9$ &

4040			Q$=CVT$$(RAD$(A%(J%*4%))+RAD$(A%(J%*4%+1%)),-2%) &
	\		GOTO 4120 UNLESS LEN(Q$) &
	\		GOTO 4050 UNLESS LEFT(Q$,3%)="$$0" &
	\		PATCH%=PATCH%+1% &
	\		PAT$(PATCH%)=RIGHT(Q$,3%) &
	\		PAT%(PATCH%)=A%(J%*4%+3%) &
	\		GOTO 4120 &

4050			GOTO 4060 UNLESS LEFT(Q$,4%)="DQS$" &
	\		G%(0%)=G%(0%)+1% &
	\		G%(G%(0%))=A%(J%*4%+3%) &
	\		G$(G%(0%))=Q$ &
	\		GOTO 4120 &

4060			GOTO 4100 IF XLATE(Q1$,R.T$)<XLATE(Q$,R.T$) &
	\		GOTO 4120 UNLESS Q$=Q1$ &
	\		GOTO 4070 UNLESS Q1%=0% &
	\		ANNOT%=ANNOT%-1% &
	\		GOTO 4075 &

4070			GOTO 4080 UNLESS Q1%=3% &
	\		DECNET%=DECNET%-1% &
	\		DEC%(0%)=DEC%(0%)+1% &
	\		DEC%(Q2%)=A%(J%*4%+3%) &
	\		DEC$(Q2%)=Q$ &

4075			G%(0%)=G%(0%)+1% &
	\		G%(G%(0%))=A%(J%*4%+3%) &
	\		G$(G%(0%))=Q$ &

4080			GOTO 4090 UNLESS Q1%=1% &
	\		MAIN%=MAIN%-1% &
	\		M%(Q2%)=A%(J%*4%+3%) &
	\		M$(Q2%)=Q$ &

4090			GOTO 4100 UNLESS Q1%=2% &
	\		MAIN2%=MAIN2%-1% &
	\		M2%(Q2%)=A%(J%*4%+3%) &
	\		M2$(Q2%)=Q$ &

4100			SYM.FND%=-1%	! WE FOUND IT &
	\		GOTO 4030 IF Q$=Q1$ ! REALLY WE DID &
	\		M$(Q2%)=Q1$ IF Q1%=1% &
	\		M2$(Q2%)=Q1$ IF Q1%=2% &
	\		DEC%(0%)=DEC%(0%)+1% IF Q1%=3% &
	\		DEC$(Q2%)=Q1$ IF Q1%=3% &
	\		IF Q1%=0% THEN &
				G%(0%)=G%(0%)+1% &
	\			G%(G%(0%))=-1% &
	\			G$(G%(0%))=Q1$ &

4110			GOTO 4030	! BUT HERE, WE DIDN'T
4120		NEXT J%
4125	NEXT K% &
	! THE .SIL FILE IS NOW OPEN, SO GET THE INDEX BLOCK. &
	! SE.STN.BLK%=THE # OF BLOCKS REQUIRED FOR THE MODULES &
	!    SYMBOL TABLE (64 SYMBOLS PER BLOCK). &
	! SE.STB%=STARTING BLOCK # OF THE SYMBOL TABLE &
	! INPUT ALL REQUIRED SYMBOLS. &
	! FOR ALL BLOCKS: &
	!	EXTRACT A SYMBOL &
	!	IF IT IS 1 THAT WE NEED THEN &
	!		SAVE THE SYMBOL VALUE &
	!	ELSE	KEEP LOOKING &
	! EXIT THE LOOP IF WE FIND ALL THE SYMBOLS NEEDED. &
	! PUT THE TOTAL NUMBER OF PHASE PHYS ADDR ENTRIES INTO PH%(0%) &

4130	PAT%(0%)=PATCH% &
	\ GOTO 4200 UNLESS ANNOT% &
	\ FOR I%=1% TO G%(0%) &
	\	IF G$(I%)="TTINEC" THEN &
			IF G%(I%)=-1% THEN &
				G%(I%)=0% &
	\			ANNOT%=ANNOT%-1% &
	\			GOTO 4200
4140	NEXT I% &

4200	FOR K%=1% TO SE.STN.BLK% &
	\	GET #3%, RECORD SE.STB%+K% &
	\	GOSUB 14500 &
	\	FOR J%=0% TO 63% &
	\		Q$=CVT$$(RAD$(A%(J%*4%))+RAD$(A%(J%*4%+1%)),-2%) &
	\		GOSUB 4220	IF RIGHT(Q$,4%)="AP5" &
					OR RIGHT(Q$,3%)="PORT" &
					OR Q$="FIPAP6" &
					OR Q$="TERAP6" &
					OR Q$="OVRBUF" &
	\	NEXT J% &
	\ NEXT K% &
	\ CLOSE 3% &
	\ GOTO 4300 &
	&

4220	PHN$=LEFT(Q$,3%) &
	\		IF PHN$="XED" THEN PHN$="UNA" &
		ELSE	IF PHN$="XHD" THEN PHN$="QNA" &
		ELSE	IF PHN$="XMD" THEN PHN$="XVR" &
		ELSE	IF PHN$="XDD" THEN PHN$="DMP" &
		ELSE	IF PHN$="RJD" THEN PHN$="RJ2780" &
		ELSE	IF PHN$="XKD" THEN PHN$="KVR" &
		ELSE	IF PHN$="DLP" THEN PHN$="DLPORT" &
		ELSE	IF PHN$="PKD" THEN PHN$="PKPORT" &
		ELSE	IF PHN$="DHP" THEN PHN$="DHPORT" &
		ELSE	IF PHN$="DZP" THEN PHN$="DZPORT" &
		ELSE	IF PHN$="VHP" THEN PHN$="VHPORT" &
		ELSE	IF PHN$="APT" THEN PHN$="KBX" &
		ELSE	IF PHN$="TER" THEN PHN$="TERCLS" &

4230	FOR SM.PTR%=2% TO NUM.MOD% &
		\ IF CVT$$(RAD$(SILMOD%(SM.PTR%,0%)) &
			+RAD$(SILMOD%(SM.PTR%,1%)),-2%)=PHN$ &
		  THEN SILMOD%(SM.PTR%,14%)=A%(J%*4%+3%) &
		\ GOTO 4250 &

4240	NEXT SM.PTR% &

4250	RETURN &
	! &
	! PHASES ACTUALLY LOADED INTO MEMORY, WHICH ALL THIS PHELDICARB IS &
	! SUPPOSED TO DETERMINE, HAVE PHYSICAL ADDRESSES WHICH CAN BE FOUND &
	! BY LOOKING UP THE PHASE NAME IN THE RSTS SYMBOL TABLE, LOOKING UP &
	! THE ASSOCIATED ADDRESS IN THE RSTS PHASE OF THE DUMP, THE CONTENTS &
	! OF WHICH ARE ONE OF THE FOLLOWING: &
	!	THE PHYSICAL ADDRESS OF THE LOADED PHASE, &
	!	OR A NEGATIVE NUMBER MEANING THAT THE PHASE IS NOT LOADED, &
	!	OR ZERO MEANING THAT THE PHASE WAS NEVER GENNED. &
	! &
	! SYMBOLS ASSOCIATED WITH PHASES ARE: &
	!	PHASE NAME	SYMBOL &
	!	----------	-------- &
	!	"xxx"		"xxxAP5" - THIS IS THE MOST FREQUENT CASE &
	!	"UNA"		"XEDAP5" &
	!	"QNA"		"XHDAP5" &
	!	"XVR"		"XMDAP5" &
	!	"DMP"		"XDDAP5" &
	!	"TER"		"KBDAP5" (Removed in 9.7) &
	!	"KVR"		"XKDAP5" &
	!	"FIP"		"FIPAP6" &
	!	"RJ2780"	"RJDAP5" &
	!	"OVR"		"OVRBUF" &
	! Added in V9.7: &
	!	"DLPORT"	"DLPAP5" &
	!	"PKPORT"	"PKDAP5" &
	!	"DHPORT"	"DHPAP5" &
	!	"DZPORT"	"DZPAP5" &
	!	"VHPAP5"	"VHPORT" &
	!	"LAT"		"LATAP5" &
	!	"TERCLS"	"TERAP6" &
	!	"KBX"		"APTAP5" &
	! &

4300	C$=CHR$(13%)+CHR$(10%) &
	\ T$=CHR$(9%) &
	\ PRINT #1%,C$;"Analysis of ";CVT$$(S0$,-2%);C$; &
		T$;"Taken on ";DATE$(0%);" at ";TIME$(0%) &
	\ PRINT #1%, C$;"ANALYS run from system: ";FNE$(0%) &
	\ PRINT #1%, "The /SIL: switch was specified." IF DIFSIL%<>0% &
	\ PRINT #1%, "The system SIL was used." IF DIFSIL%=0% &
	\ MAIN%=MAIN%+MAIN2% &
	\ IF MAIN% THEN &
		Q$="" &
	\	Z1$="" &
	\	FOR I%=1% TO (M%(0%)+1%)*2% STEP 2% &
	\		Z1$=Z1$+M$(I%)+"   " IF M%(I%)=0% UNLESS M$(I%)="" &
	\		Z1$=Z1$+C$ IF LEN(Z1$)>60% &
	\	NEXT I% &
	\	FOR I%=1% TO (M2%(0%)+2%)*2% STEP 2% &
	\		Z1$=Z1$+M2$(I%)+"   " IF M2%(I%)=0% UNLESS M2$(I%)="" &
	\		Z1$=Z1$+C$ IF LEN(Z1$)>60% &
	\	NEXT I% &
	\	GOSUB 14100 &
	\	IF MAIN%<(M%(0%)+M2%(0%)+3%) THEN &
			PRINT #1%,C$;"ANALYS will continue." &
		ELSE	PRINT #1%,C$;"Crash File cannot be Analyzed" &
	\		CLOSE 1%,12% &
	\		KILL WORK.FILE$ &
	\		GOTO 32767 &
	! ************************************************************ &
	! LET THE USER KNOW IF WE COULDN"T GET ALL THE INFO WE &
	! NEEDED. IF THIS IS THE CASE THEN VARIOUS STATISTICS WILL BE &
	! WRONG OR, MORE LIKELY, FNP% ERRORS WILL OCCUR. &
	! &
	! DON'T EVEN BOTHER TRYING IF WE COULDN'T FIND ANY &
	! ************************************************************ &

4310	IF DECNET% AND DEC%(1%) THEN &
		Q$="DECnet/E " &
	\	Z1$="" &
	\	FOR I%=1% TO DEC%(0%) &
	\		Z1$=Z1$+DEC$(I%)+"   " IF DEC%(I%)=0% &
	\		Z1$=Z1$+C$ IF LEN(Z1$)>60% &
	\	NEXT I% &
	\	GOSUB 14100 &
		! INFORM USER IF COULDN'T FIND ALL THE DECNET STUFF AND &
		! DECNET WAS CONFIGURED. &

4320	IF ANNOT% THEN &
		Q$="annotation " &
	\	Z1$="" &
	\	FOR I%=1% TO G%(0%) &
	\		Z1$=Z1$+G$(I%)+"   " IF G%(I%)=-1% &
	\		G$(I%)="" IF G%(I%)=-1% &
	\		Z1$=Z1$+C$ IF LEN(Z1$)>60% &
	\	NEXT I% &
	\	GOSUB 14100 &
	\	PRINT #1%,C$;"Annotations will not be complete";C$; &
		! INFORM USER IF NOT ALL THE ANNOTATION STUFF WAS FOUND &
		! (ANNOT WILL BE ZERO IF DIDN'T WANT IT) AND DON'T &
		! TRY TO DO THE ANNOTATIONS. &
	&

4400	OCT.ST%, FXMONS%=0% &
	\ EPMM=FNF(M2%(31%)) &
	\ FPLAP6%,M2%(53%)=FNP%(M2%(53%)) &
		! GET THE PHYSICAL ADDRESS OF THE FIP PHASE &
	\ FXMONS%=FNP%(M2%(39%)) IF M2%(39%)<>0% &
	\ OCT.ST%=4% IF FXMONS%<>0% &
	\ FXMONS%=M2%(31%) IF M2%(31%)<>0% UNLESS FXMONS%<>0% &
	\ OCT.ST%=2% IF FXMONS%<>0% UNLESS OCT.ST%<>0% &
	\ FXMONS%=M2%(63%) IF M2%(63%)<>0% UNLESS FXMONS%<>0% &
	\ OCT.ST%=1% IF FXMONS%<>0% UNLESS OCT.ST%<>0% &
	\ OCT.ST%=0% IF FXMONS%=0% &
	\ M%(9%)=M2%(43%) IF M2%(43%)<>0% &
		! FXMONS% IS THE SIZE OF THE READ/WRITE PORTION &
		! OF THE MONITOR. IT COMES FROM THE SYMBOL X.MONS &
		! IF POSSIBLE.  (X.MONS POINTS TO A WORD CONTAINING &
		! THE MONITOR SIZE. &
		! IF WE CAN'T FIND X.MONS, THEN WE USE $$EPMM, WHICH &
		! IS A VALUE, NOT A POINTER. &
		! FAILING THAT WE USE $$CRSZ, WHICH IS ALSO A VALUE. &
		! IF WE CAN'T FIND A MONITOR SIZE, WE THROW IN THE TOWEL. &
		! MAKE SATBUF=CRASAV IF WE FOUND CRASAV. &
	\ IF OCT.ST%=0% &
	THEN PRINT #1%, C$; &
	"Can't determine size of monitor,"; &
	" will do sequential dump of CRASH file";C$ &
		! IF WE CAN'T FIGURE OUT HOW BIG THE MONITOR IS, WE HAVE &
		! NO IDEA WHERE IT ENDS AND ANYTHING ELSE BEGINS. &
		! THEREFORE WE WILL TRY TO DO A STRAIGHT DUMP OF THE &
		! WHOLE CRASH FILE IN HOPES THERE WILL BE SOMETHING &
		! WORTHWHILE THERE. &

4405	XTABS%=M2%(35%) &
		! SIZE OF XTAB TABLE IN PAIRS OF ENTRIES &
	\ XTAB%(I%)=0% FOR I%=0% TO 20% &
	\ XCON%=FNP%(M2%(37%)) IF M2%(37%)<>0% &
		! FLAG WORD. &
		! 100000 - (XC$NEM) X.TAB IS VALID &
		! 040000 - (XC$UMR) UMR'S ARE PRESENT &
		! 020000 - (XC$IDS) HARDWARE HAS I AND D SPACE &
		! 010000 - (XC$OPT) MONITOR IS USING I AND D SPACE &
		! 000040 - (XC$QBU) Q-BUS SYSTEM &
		! 000020 - (XC$22)  MORE THAN 256KB OF PHYSICAL MEMORY &
		! 004000 - (XC$OAT) SET IF ODD ADDRESS TRAPPING &
		! 002000 - (XC$CIS) SET IF CIS PRESENT &
		! 001000 - (XC$FPP) SET IF FPP UNIT PRESENT &
		! FOLLOWING BITS ARE INVALID FOR NOW &
		! 000400 - (XC$PAM) SET IF SYSTEM HAS PARITY MEMORY &
		! 000200 - (XC$ECC) SET IF SYSTEM HAS ECC &
		! 000100 - (XC$CAC) SET IF MEMORY HAS CACHE HARDWARE &
	\ XSCS%=FNP%(M2%(41%)) IF M2%(41%)<>0% &
		! CLUSTER SIZE OF DISK THAT WAS DUMPED TO ORIGINALLY. &
	\ XCON%=32767%+1% IF XCON%=0% AND XSCS%=0% &
		! IF BOTH OF THESE ARE ZERO (OR WE DIDN'T FIND EITHER OF &
		! THEM) THEN WE DON'T WANT TO GO TOO FAR INTO THE MAP%() &
		! BECAUSE WE WILL END UP DIVIDING BY ZERO. &
	\ X.TAB%=M2%(33%) &
	\ XTAB%(I%)=FNP%(X.TAB%+(I%-1%)*2%) FOR I%=1% TO XTABS%*2% IF XTABS% &
		! GET THE XTAB TABLE INTO AN ARRAY. &
	\ MAP%(I%,J%)=0% FOR J%=0% TO 3% FOR I%=0% TO 10% &
	! &
		! FROM HERE ON WE WILL LOOP THROUGH THE XTAB TABLE AND &
		! CALCULATE THE REST OF THE BLOCK/OFFSET STARTING AND &
		! ENDING PAIRS. &
		! GET OUT OF HERE IF THERE ARE NO SEGMENTS BEYOND MONITOR, &
		! OR XTAB SIZE IS ZERO OR CLUSTER SIZE IS ZERO. &
	\ NEXT.BLOCK%=1% &
		! INITIALIZE STARTING BLOCK NUMBER. &
	\ FOR I%=1% TO XTABS% &
		! DO THE LOOP FOR AS MANY ENTRIES AS THERE ARE IN X.TAB. &
	\   IF XTAB%((I%*2%))=0% THEN &
		  BLANKS%=BLANKS%+1%   	! IGNORE ANY ENTRIES WITH 0 LENGTH &
		ELSE &
			MAP%(I%-1%,0%)=NEXT.BLOCK% &
			! STARTING BLOCK NUMBER OF THIS SEGMENT. &
	\		MAP%(I%-1%,1%)=MAP%(I%-1%,0%)+ &
				       ((FNF(XTAB%(I%*2%))-1%)/8%) &
			! ENDING BLOCK NO. = STARTING BLOCK NUMBER + &
			! BLOCKS IN  THIS SEGMENT. &
			! BLOCKS IN SEG. = (BYTES IN SEG. / 8) &
	\		MAP%(I%-1%,2%)= (FNF(XTAB%(I%*2%))-(MAP%(I%-1%,1%)- &
					 MAP%(I%-1%,0%))*8%)*64% &
			! OFFSET = (BLOCKETTES IN SEGMENT - &
			!	   BLOCKETTES IN PRECEDING  BLOCKS) * 64 &
			! (BLOCKETTE IS 64 BYTES) &
	\		MAP%(I%-1%,3%)=XTAB%((I%*2%)-1%) &
					UNLESS (I%-1%)=FPL.SEG% &
			! STARTING MEMORY ADDRESS (/64) FOR THIS SEGMENT. &
			!		UNLESS IT'S THE FIP POOL. &
	\		MAP%(I%-1%,3%)=(FNF(FIP.LOAD.ADDRS%)/64%)+ &
				       (XTAB%((I%*2%)-1%)-FPLAP6%) &
						IF (I%-1%)=FPL.SEG% &
				! SAVE THE VIRTUAL ADDRESS OF THE FIP POOL. &
				! ADDRESS = (FIP PHASE LOAD ADDRESS/64) + &
				! (FIP POOL PHYS. ADDRS - FIP PHASE PHYS. &
				!  ADDRS.) &
	\		FPL.SIZE%=XTAB%(I%*2%) IF (I%-1%)=FPL.SEG% &
				! GET THE FIP POOL SIZE ALSO. &
	\ 		NEXT.BLOCK%=((((MAP%(I%-1%,1%)-1%)/XSCS%)+1%)* &
					XSCS%)+1% &
			! CALCULATE NEXT BLOCK. &

4406	NEXT I% &
	\ IF FPLAP6% THEN &
		FPL.ST%=MAP%(FPL.SEG%,3%)*64% &
	\	FPL.END%=FPL.ST%+(FPL.SIZE%*64%) &
	\	FPL.ST=FNF(FPL.ST%) &
	\	FPL.END=FNF(FPL.END%) &
		! GET THE REAL LIMITS OF THE FIP POOL. &

4410	PRINT #1%, C$;"System was configured for DECnet/E"; &
		IF DEC%(1%)<>0% &
	\ PRINT #1%, C$;"System was not configured for DECnet/E"; &
		IF DEC%(1%)=0% &
	\ IF DEC%(1%)<>0% THEN &
 	PRINT #1%, C$;"DECnet/E was enabled"; UNLESS (FNP%(DEC%(8%))=0%) &
		OR (FNP%(DEC%(3%))=0%) &
	\ IF (FNP%(DEC%(8%))=0%) OR (FNP%(DEC%(3%))=0%) THEN &
		PRINT #1%, C$;"DECnet/E was not enabled"; &
	\	PRINT #1%, " (DECnet/E info will not be printed)"; &
	\	DEC%(1%)=0% &
	! PRINT DECNET INFO &
	! DEC(1)  INDICATES THAT DECNET WAS INSTALLED. &
	! DEC(3) - NODLST POINTER =0 OR &
	! DEC(8) - LLTBUF - INDICATES IF NSP WAS ENABLED; &
	!  IF NOT, PRETEND NO DECNET IN ORDER TO BYPASS DECNET PRINTING. &

4420	M2%(29%)=0% UNLESS M2%(1%) &
	\ PRINT #1%, C$;"System was configured for large files"; &
			IF M2%(29%) &
	\ PRINT #1%, C$;"System was configured for small files"; &
			IF M2%(29%)=0% &
	\ PRINT #1%, C$;"Configuration word (X.CON) = ";FNO$(XCON%); &
	\ PRINT #1%, C$;"Memory segments that were dumped:"; &
	\ PRINT #1%, C$;"	Monitor"; &
	\ PRINT #1%, C$;"	FIP Pool"; IF MAP%(FPL.SEG%,0%) &
	\ PRINT #1%, C$;T$;T$;T$;"*** FIP Pool not dumped ***"; &
					UNLESS MAP%(FPL.SEG%,0%) &
	\ PRINT #1%, C$;"	MSCP Region"; IF MAP%(MSCP.SEG%,0%) &
	\ PRINT #1%, C$;T$;T$;T$;"*** MSCP Region not dumped ***"; &
					UNLESS MAP%(MSCP.SEG%,0%) &
	\ PRINT #1%, C$;"	XBUF"; IF (MAP%(XBUF.SEG%,0%) <> 0% &
				      AND XCON% > 0%) &
	\ PRINT #1%, C$;T$;T$;T$;"*** XBUF not dumped ***"; &
					UNLESS (MAP%(XBUF.SEG%,0%) <> 0% &
						AND XCON% > 0%) &
	\ PRINT #1%, C$;"	JCTRL"; IF MAP%(JCTRL.SEG%,0%) &
	\ PRINT #1%, C$;T$;T$;T$;"*** JCTRL not dumped ***"; &
					UNLESS MAP%(JCTRL.SEG%,0%) &
	\ PRINT #1%, C$;"	JOB Header"; &
			IF FNF(MAP%(JHEAD.SEG%,3%))<>FNF(M%(43%))/64. &
	\ PRINT #1%, C$;T$;T$;T$;"*** JOB Header not dumped ***"; &
			IF FNF(MAP%(JHEAD.SEG%,3%))=FNF(M%(43%))/64. &
	\ PRINT #1%, C$;"	FIJOB Header"; &
			IF FNF(MAP%(FJHEAD.SEG%,3%))<>FNF(M%(43%))/64. &
	\ PRINT #1%, C$;T$;T$;T$;"*** FIJOB Header not dumped ***"; &
			IF FNF(MAP%(FJHEAD.SEG%,3%))=FNF(M%(43%))/64. &
!	\ PRINT #1%, C$;"	JOB Header"; IF MAP%(JHEAD.SEG%,0%) &
!	\ PRINT #1%, C$;T$;T$;T$;"*** JOB Header not dumped ***"; &
!					UNLESS MAP%(JHEAD.SEG%,0%) &
!	\ PRINT #1%, C$;"	FIJOB Header"; IF MAP%(FJHEAD.SEG%,0%) &
!	\ PRINT #1%, C$;T$;T$;T$;"*** FIJOB Header not dumped ***"; &
!					UNLESS MAP%(FJHEAD.SEG%,0%) &
	\ ASEGS%=XTABS%-(MAX.SEG%+1%)-BLANKS% &
	\ PRINT #1%, C$;"	";NUM1$(ASEGS%); &
			" additional segment"; &
			IF ASEGS%>0% &
	\ PRINT #1%, "s"; IF ASEGS%>1% &
	\ PRINT #1%, C$;"UMR's are present"; &
			IF (XCON% AND 16384%) <> 0% &
	\ PRINT #1%, C$;"CPU hardware had I and D space"; &
			IF (XCON% AND 8192%)<>0% &
	\ PRINT #1%, C$;"CPU hardware did not have I and D space"; &
			IF (XCON% AND 8192%)=0% &
	\ PRINT #1%, C$;"Monitor used I and D space"; &
			IF (XCON% AND 4096%)<>0% &
	\ PRINT #1%, C$;"Monitor did not use I and D space"; &
			IF (XCON% AND 4096%)=0% &
	\ PRINT #1%, C$;"This is a Q-bus system"; &
			IF (XCON% AND 32%) <> 0% &
	\ PRINT #1%, C$;"Greater than 248KB of physical memory"; &
			IF (XCON% AND 16%)<>0% &
	\ PRINT #1%, C$;"Odd-address trapping present"; &
			IF (XCON% AND 2048%)<>0% &
	\ PRINT #1%, C$;"Odd-address trapping not present"; &
			IF (XCON% AND 2048%)=0% &
	\ PRINT #1%, C$;"CIS was present"; &
			IF (XCON% AND 1024%)<>0% &
	\ PRINT #1%, C$;"CIS was not present"; &
			IF (XCON% AND 1024%)=0% &
	\ PRINT #1%, C$;"FPP unit was present"; &
			IF (XCON% AND 512%)<>0% &
	\ PRINT #1%, C$;"FPP unit was not present"; &
			IF (XCON% AND 512%)=0% &
	\ IF 0%<>0% THEN &
		! THIS LINE IS INTENDED TO DISABLE THE FOLLOWING: &
	PRINT #1%, C$;"System had Parity Memory"; &
			IF (XCON% AND 256%)<>0% &
	\ PRINT #1%, C$;"System did not have Parity Memory"; &
			IF (XCON% AND 256%)=0% &
	\ PRINT #1%, C$;"ECC was present"; &
			IF (XCON% AND 128%)<>0% &
	\ PRINT #1%, C$;"ECC was not present"; &
			IF (XCON% AND 128%)=0% &
	\ PRINT #1%, C$;"Memory hardware cache"; &
			IF (XCON% AND 64%)<>0% &
	\ PRINT #1%, C$;"No memory hardware cache"; &
			IF (XCON% AND 64%)=0% &

4425	FOR I%=(M2%(5%)+M2%(9%)) TO (M2%(23%)-2%) STEP 2% &
	\	IF CVT%$(SWAP%(FNP%(I%)))="PK" THEN &
			P2%=FNP%(M%(7%)+I%-M2%(5%)) &
	\		P3%=FNP%(FNP%(P2%)+8%)/2% &
	\		GOTO 4440
4430	NEXT I% &
	! PRINT LARGE FILES AND LOGINS INFO. &
	! NOTE: UNLIKE THE MONITOR TABLES PART II CALL, FCBLST WILL &
	!	NOT BE ZERO IF NO LARGE FILES WERE CONFIGURED; &
	!	INSTEAD, MUST USE "LRGFIL" TO SEPARATE SYSTEMS. &
	!	FCBLST (M2%(29)) IS RESET FOR CONSISTENCY WITH SYSTAT. &
	! &
	! SCAN THE DEVNAM TABLE TO SEE IF PK'S WERE LEGAL DEVICES. &
	! IF SO, &
	!	P2% = THE DEVTBL POINTER FOR PK'S &
	!	P3% = THE KB # OF THE KB FOR PK0 &

4440	GOTO 5000 UNLESS DEC%(1%) &
	\ LNKMAX%=FNP%(DEC%(2%)) AND 255% &
	\ IF LNKMAX%>0% THEN &
		GOTO 4450 IF LNKMAX%>=2%^(LLT.FLG%) &
			FOR LLT.FLG%=7% TO 0% STEP -1% &
		! BYPASS SOME DECNET SPECIFIC STUFF UNLESS WE &
		! FOUND NSPCCB. &
		! EXTRACT THE MAXIMUM # OF LOGICAL LINKS ALLOWED AT THIS &
		! TIME SHARING SESSION. &
		! IF THE # OF LINKS ALLOWED IS > 0 (SHOULD BE) THEN &
		!	GET THE NUMBER OF LLT WORD FLAG BITS FOR THIS &
		!	T/S SESSION; &
		!	THE LOW-ORDER N BITS IN THE LOGICAL &
		!	LINK ADDRESS (LLA) ARE THE LOGICAL LINK #. &
		!	LLA AND 2^0+...+2^(N-1) = LINK #, WHERE &
		!	N=INT(LOG2(LNKMAX))+1 &

4450	DEVCNT.OFF%=M%(5%)+M2%(9%) &
	\ NUM.DECNT.UNITS%=-1% &
	\ FIRST.TIME%=-1% &
	\ FOR DEVNAM.OFF%=(M2%(5%)+M2%(9%)) TO (M2%(23%)-2%) STEP 2% &
	\	GOTO 4460 UNLESS CVT%$(SWAP%(FNP%(DEVNAM.OFF%)))="XM" &
			OR CVT%$(SWAP%(FNP%(DEVNAM.OFF%)))="XD" &
	\	NUM.DECNT.UNITS%=NUM.DECNT.UNITS%+FNP%(DEVCNT.OFF%)+1% &
	\	IF FIRST.TIME% THEN &
			FIRST.TIME%=0% &
	\		OFFSET%=DEVNAM.OFF%-M2%(5%)-M2%(9%) &
	\		XM0.DDB.PTR%=FNP%(M%(7%)+M2%(9%)+OFFSET%)
4460		DEVCNT.OFF%=DEVCNT.OFF%+2% &
	\ NEXT DEVNAM.OFF% &
		! GET THE TOTAL NUMBER OF COMM. DEVICES 'XM' AND 'XD'. &
		! IF THE FIRST TIME THROUGH, POINT TO THE FIRST UNIT &
		! IN THE DEVPTR TABLE. &
	&

5000	SATBUF%=M%(9%) &
	\ VALID%=0% &
	\ VALID%=FNP%(SATBUF%+342%) = (NOT FNP%(SATBUF%+344%)) &
		IF OCT.ST%=2% &
			! THIS WAS A SNAP IF SATBUF WAS NOT LOADED RIGHT. &
			! LOOK AT 342-344 (526-530 octal) IF 7.0 SYSTEM. &
	\ VALID%=FNP%(SATBUF%+338%) = (NOT FNP%(SATBUF%+340%)) &
		IF (OCT.ST%=4%) &
			! LOOK AT 338-340 (522-524 octal) IF 7.1+ SYSTEM &
	\ VALID%=FNP%(SATBUF%+206%) = (NOT FNP%(SATBUF%+208%)) &
		IF V97% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
			! Addresses moved by 64 for super & I&D apr's if v9.7 &
			! LOOK AT 206-208 (316-320 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS WERE DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
	\ VALID%=FNP%(SATBUF%+270%) = (NOT FNP%(SATBUF%+272%)) &
		IF V97%=0% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
			! Not moved by 64 for super & I&D apr's before v9.7 &
			! if .sil is from v9.7 &
			! LOOK AT 270-272 (416-420 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS WERE DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
	\ VALID%=FNP%(SATBUF%+214%) = (NOT FNP%(SATBUF%+216%)) &
		IF (OCT.ST%=4%) AND ((XCON% AND 16384%)<>0%) &
			! LOOK AT 214-216 (326-330 octal) IF 7.1+ SYSTEM &
			! AND WE HAD 22-BIT ADDRESSING &
	\ VALID%=FNP%(SATBUF%+82%) = (NOT FNP%(SATBUF%+84%)) &
		IF V97% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
					AND ((XCON% AND 16384%)<>0%) &
			! Addresses moved by 64 for super & I&D apr's if V9.7 &
			! LOOK AT 82-84 (122-124 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
			! AND WE HAD 22-BIT ADDRESSING &
	\ VALID%=FNP%(SATBUF%+146%) = (NOT FNP%(SATBUF%+148%)) &
		IF V97%=0% IF (OCT.ST%=4%) AND ((XCON% AND 8192%)<>0%) &
					   AND ((XCON% AND 16384%)<>0%) &
			! Not moved by 64 for super & I&D apr's if before V9.7 &
			! LOOK AT 146-148 (222-224 octal) IF 9.0 SYSTEM &
			! AND I AND D SPACE REGS DUMPED &
			!  BOTH USER AND KERNEL I/D REGS &
			! AND WE HAD 22-BIT ADDRESSING &
	\ PRINT #1%, C$;C$; &
		"SATBUF is not valid, so this crash file is probably from"; &
		C$;"a SNAP dump.  The following analysis may not be valid"; &
		C$;"if the contents of memory changed during the dump."; &
			IF VALID%=0% &
	\ J%=FNP%(SATBUF%+510%) &
	\ GOTO 5005 IF DUMP.REGRDLS% &
	\ IF J%=-1% AND VALID%<>0% THEN &
		PRINT #1%, C$;C$; &
			   "Recovering from a power failure, memory dump"; &
			   " will be omitted."; &
	\	STB%,D.UMP%=0% &
		! SET /NODUMP IF WE'RE RECOVERING FROM A &
		! POWER FAIL ERROR AND IT'S NOT A SNAP DUMP.
5005	PRINT #1%, C$;C$;"SATBUF is valid for this dump."; &
			IF VALID%<>0% &
	\ PRINT #1%, CHR$(12%); &
	\ PRINT #1%, C$;"Further LOGINS were Disabled" &
		IF (FNP%(M2%(13%)) AND -256%)=256% &
	\ CC%(1%)=D.UMP% &
	\ CC%(2%)=FXMONS% &
	\ CC%(3%)=H0% &
	\ CC%(4%)=OCT.ST% &
	\ CC%(5%)=WIDE% &
	\ CC%(6%)=LLT.FLG% &
	\ CC%(7%)=P2% &
	\ CC%(8%)=P3% &
	\ CC%(9%)=NUM.DECNT.UNITS% &
	\ CC%(10%)=XM0.DDB.PTR% &
	\ CC%(11%)=STB% &
	\ CC%(12%)=ROOT.SEG% &
	\ CC%(13%)=FPL.SEG% &
	\ CC%(14%)=XBUF.SEG% &
	\ CC%(15%)=MAX.SEG% &
	\ CC%(16%)=FPL.ST% &
	\ CC%(17%)=FPL.END% &
	\ CC%(18%)=MSCP.SEG% &
	\ CC%(19%)=JCTRL.SEG% &
	\ CC%(20%)=V97% &
	\ CC%(21%)=JHEAD.SEG% &
	\ CC%(22%)=FJHEAD.SEG% &
	\ FILE.NAME$(1%)=I0$ &
	\ FILE.NAME$(2%)=O0$ &
	\ FILE.NAME$(3%)=CR.LOGFIL$ &
	\ FILE.NAME$(4%)=SIL.FILE$ &
	\ FILE.NAME$(5%)=WORK2.FILE$ &

5010	TEMP$=SYS(CHR$(8%)+CVT%$(DIAGNOSE%)+WORK1.FILE$) &
	\ PRINT "Exiting ANALYS - ";DATE$(0%);TIME$(0%); &
		"CPU time";TIME(1%) IF (DIAGNOSE% AND 64%) &
	\ GOTO 32767 IF (DIAGNOSE% AND 1%) &
	\ CLOSE 1%,3%,8%,9%,10%,11%,12% &
	\ S$=PKG.LOC$+"ANALY1" &
	\ CHAIN S$ LINE 31000 &
	! CLEAR THE ERROR VARIABLE, GOTO EACH ROUTINE. &
	! SAVE SOME NEEDED INFO IN CORE COMMON. &
	! CHAIN TO THE CORE DUMP PROGRAM. &
	! NOTE: IF /BUFFERS OR /ALPHA SPECIFIED BUT FOR SOME REASON WE &
	!  CAN'T DO THE ANNOTATIONS (USER'S BEEN TOLD), DO THE CHAIN ANYWAY. &
	!  /ALPHA WILL GET THE OCTAL INSTEAD, /BUFFERS WILL FIZZLE SOON. &

8500	! &
	&
	&
	!	E X T R A C T S    O F    M O N I T O R    T A B L E S &
	!		P A R T S    1    A N D    2 &
	&
	!	T H E S E     S U B S C R I P T S    U S E D    F O R &
	!		S Y S T A T    C O M P A T I B I L I T Y &
	&
	&
	!	E X T R A C T    D E C N E T    A N D &
	!	A N N O T A T I O N    S Y M B O L S &
	&
	&
	!	"name",A,B &
	!		A=0 for annotation symbol &
	!		A=1 for monitor table part one &
	!		A=2 for monitor table part two &
	!		A=3 for DECnet &
	!		when A=1, 2, or 3 then ... &
	!			B=subscript in the array to store it &
	&
	&

8600	DATA	21,38,24,34 &

8610	!	# OF A=1, A=2, A=3, A=0 &

8700	DATA	"ADJFLG",3,22,		"BAKLRG",0,0, &
		"BAKSML",0,0,		"BGBUFR",0,0, &
		"CCLLST",0,0, 		"CHECTL",2,69, &
		"CHENUE",0,0,		"CHENUE",2,71, &
		"CHESEC",0,0,		"CHRDL3",0,0, &
		"CMT",0,0,		"CMT",2,73, &
		"CMTCTL",2,75, &
		"CONQUE",3,11,		"CRAAP5",1,43, &
		"CRASAV",2,43, &
		"CSRTBL",2,7, 		"CSR.KB",2,65, &
		"DDCTBL",2,45,		"DEVCNT",0,0, &
		"DEVCNT",1,5, &
		"DEVNAM",0,0,		"DEVNAM",2,5, &
		"DEVNKB",0,0, &
		"DEVOKB",0,0,		"DEVOKB",2,9, &
		"DEVPTR",1,7,		"DEVSYN",2,23, &
		"DEV.KB",0,0, &
		"DSKLOG",0,0,		"DSKLOG",2,21, &
		"DSKQPT",0,0,		"ENDCHE",3,16, &
		"ERLCTL",2,17, &
		"EVTMMU",3,21,		"FAKDDB",2,67, &
		"FCBLST",2,29,		"FIBUF",0,0, &
		"FIJBDA",1,1,		"FIJOB",1,3, &
		"FIPROO",0,0,		"FPLAP6",2,53, &
		"FREES",0,0,		"FREES",2,3, &
		"IDLCBF",3,0,		"IDLNOB",3,13, &
		"JBSTAT",1,13,		"JBWAIT",1,15, &
		"JCMFLG",1,41, &
		"JCRSIZ",1,33, &
		"JOBCNT",2,13,		"JOBTBL",1,11, &
		"LATAP5",1,35,		"LATQUE",0,0, &
		"LLTBUF",3,8,		"LNKMAX",3,2, &
		"LOWAD",2,59,		"LRGFIL",2,1, &
		"MEMLST",0,0,		"MEMLST",1,31, &
		"MTXMMU",3,19,		"NETPOL",3,12, &
		"NODLST",3,3,		"NSPACQ",0,0, &
		"NSPCCB",3,1, &
		"NSPCDB",3,7,		"NSPFCQ",3,15, &
		"NSPJDB",3,23,		"NSPLLB",3,6, &
		"NSPLST",3,4,		"NSPQUE",3,9, &
		"NSPQ2F",3,14, &
		"NULRTS",2,61,		"OAJMMU",3,18, &
		"OVBASE",0,0,		"OVBASU",0,0, &
		"O2BASE",0,0, &
		"PATCH",1,23,		"RAWMEM",0,0, &
		"RTEMMU",3,17,		"RTMSTA",3,20, &
		"RTSLST",0,0,		"RTSLST",2,15, &
		"SATBUF",1,9, &
		"SATCTL",1,21,		"SATCTM",1,25, &
		"SATEND",2,55,		"SATMMU",0,0, &
		"SIDBUF",3,5, &
		"SNDLST",2,19,		"STSTBL",2,49, &
		"SYSLOG",0,0,		"SYSTAK",1,39, &
 		"TRNQUE",3,10,		"TTFMSB",2,51, &
		"TTYHCT",2,11,		"UCTTBL",2,47, &
		"UMT$DU",0,0,		"UMT$DU",2,77, &
		"UNTCLU",1,17,		"UNTCNT",1,19, &
		"UNTLVL",2,57,		"UNTOPT",1,29, &
		"XECORE",0,0,		"XHDATA",0,0, &
		"X.CON",2,37,		"X.MONS",2,39, &
		"X.SCS",2,41,		"X.TAB",2,33, &
		"X.TABS",2,35,		"$$CACH",0,0, &
		"$$CBSZ",0,0,		"$$CRSZ",2,63, &
		"$$EPMM",2,31,		"$$JCR6",2,25, &
		"$$LATE",0,0,		"$$LATS",1,37, &
		"$$1",4,0,		"",0,0 &
	&

14100	PRINT #1%,C$;C$; &
	"The following ";Q$; &
	"symbols could not be found:";C$;Z1$; &
	\ RETURN &
		! COULDN'T FIND SOMETHING - Q$="",DECNET/E, OR ANNOTATION &

14500	FOR I%=0% TO 511% STEP 2% &
	\	FIELD #3%, I% AS S$, 2% AS I9$ &
	\	A%(I%/2%)=SWAP%(CVT$%(I9$)) &
	\ NEXT I% &
	\ S$,I9$="" &
	\ RETURN &
	! &
	!	. S I L    R E C O R D    S E T U P &
	! &
	! FIELD .SIL RECORD IN PREPARATION FOR SYMBOL TABLE SCAN. &
	&
	&

14600	SILREC%=2% &
		! POINT TO NEXT RECORD NUMBER TO GET FROM SIL. &
		! WE ARE GOING TO GET ALL THE REST, HOWEVER MANY THERE ARE. &
		! *WARNING* IF WE GET ?SUBSCRIPT OUT OF RANGE ERRORS AT &
		! NEXT LINE, THEN WE WILL HAVE TO INCREASE ARRAY SIZE. &

14610	GET #3%, RECORD SILREC% &
		! GET NEXT SIL RECORD &
	\ GOSUB 14500 &
		! GO SET IT UP &
	\ SPTR%=15%+(16%*(SILREC%-2%)) &
		! COMPUTE SIL RECORD POINTER FOR NEXT LINE &
	\ SILMOD%(J%+SPTR%,I%)=A%((J%-1%)*16%+I%) FOR I%=0% TO 14% &
		FOR J%=1% TO NUM.MOD%-15% &
		! LAY OUT THE DATA &
	\ GOTO 14620 IF NUM.MOD%<SPTR%+16% &
		! WE BE DONE IF THE NUMBER OF MODULES IS LESS THAN THE &
		! FIRST ENTRY IN THE NEXT RECORD. &
	\ SILREC%=SILREC%+1% &
		! INCREMENT POINTER FOR NEXT RECORD. &
	\ GOTO 14610 &
		! PLAY IT AGAIN, SAM. &

14620	RETURN &
	! &
	!	GET THE 2ND-NTH BLOCK OF THE SIL HEADER &
	! &
	!	AND LOAD THE SILMOD ARRAY &
	&
	&

15000	! &
	&
	&
	!	F U N C T I O N S &
	&
	&

15300	DEF* FNPRINT$(TXT$,ADR%,TYP%) &
	\ FNPRINT$="" &
	&
	! FUNCTION TO PRINT SYMBOLS FROM STB. &

15310	IF TYP%=1% &
		THEN PRINT #9%, CHR$(12%); UNLESS J%=1% &
	\	PRINT #9%, "Symbol table contents for module ";TXT$;":"; &
				C$;C$; &
	\	WIDTH%=0% &
	\	GOTO 15340 &

15320	PRINT #9%,	TXT$; &
			STRING$(6%-LEN(TXT$),32%);" "; &
			FNO$(ADR%); &
	\ WIDTH%=WIDTH%+16% &
	\ IF FORM.WIDTH%-WIDTH% >= 13% &
		THEN PRINT #9%, "   "; &
		ELSE PRINT #9%, C$; &
	\		WIDTH%=0% &

15340	FNEND &

15400	DEF* FNO$(Q%) &
	\ FNO$="" &
	! FUNCTION	FNO$	GET A 6 POSITION OCTAL STRING &

15410	QTMP$="0" &
	\ Q0%=4096% &
	\ IF Q%<0% THEN &
		QTMP$="1" &
	\	Q%=Q%+32767%+1% &
	! SET UP TO CONVERT THE NUMBER &
	! TAKE CARE OF NEGATIVE NUMBERS &

15420	WHILE Q0%>0% &
	\	Q1%=Q%/Q0% &
	\	Q%=Q%-Q0%*Q1% &
	\	Q0%=Q0%/8% &
	\	QTMP$=QTMP$+CHR$(48%+Q1%) &
	\ NEXT &
	! CONVERT THE NUMBER TO OCTAL, SAVING EACH DIGIT &

15430	FNO$=QTMP$ &
	! SET THE FUNCTION &

15440	FNEND &
	&

15500	DEF* FNP%(Q%) &
	! FUNCTION	FNP%	THIS FUNCTION IS EQUIVALENT &
	!			TO A PEEK OF AN ADDRESS &
	! &

15510	FNP%,E%=0% &
	\ IF (Q% AND 1%) &
	    THEN &
		E%=-1% &
	\	PRINT #1%, C$;"Odd PEEK Address - ";NUM1$(FNF(Q%)) &
	\	GOTO 15560 &

15520	R=FNF(Q% AND -2%) &
	\ IF R >= 0. AND R <= EPMM &
	    THEN &
		SEGMNT%=ROOT.SEG% &
	\	FNP%=FNPEEK%(SEGMNT%,R) &
	\	GOTO 15560 &
		! IF THE ADDRESS IS BETWEEN 0 AND $$EPMM THEN &
		! PEEK AT THE ROOT SEGMENT. &
		! &

15530	GOTO 15540 IF ((R > EPMM) AND (R < FPL.ST)) &
	\ IF FPLAP6% <> 0% &
	    THEN &
		IF R <= FPL.END &
		    THEN &
			SEGMNT%=FPL.SEG% &
	\		OFFSET=R-FPL.ST &
	\		R=((MAP%(FPL.SEG%,0%)-1%)*512.)+OFFSET &
	\		FNP%=FNPEEK%(SEGMNT%,R) &
	\		GOTO 15560 &
		! ERROR IF THE ADDRESS IS BETWEEN PERMANENTLY MAPPED &
		! MEMORY AND AND THE START OF FIP POOL.  PEEK INTO THE &
		! FIP POOL IF WE HAVE A FIP POOL AND THE ADDRESS IS NOT &
		! GREATER THAN UPPER LIMITS OF THE FIP POOL. &

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
		PRINT #1%, C$;"%PEEK address ("; &
			  NUM1$(AD);") out of range, ("; &
			  NUM1$(START.ADDRS);" - "; &
			  NUM1$(END.ADDRS);")" &
				IF PAST.INIT%=0% &
	\ IF PAST.INIT% THEN &
	PRINT #1%, C$;"%Peek address ("; &
		NUM1$(AD);") out of range, ("; &
		NUM1$(MAP%(SEG%,0%)*512.);" - "; &
		NUM1$(MAP%(SEG%,1%)*512. + MAP%(SEG%,2%));")" &
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

15800	DEF* FNS$(S%)=SPACE$(S%-LEN(S$))+S$ &
	! LEFT PAD A STRING WITH SPACES. &

19000	! &
	&
	&
	!	E R R O R    H A N D L I N G &
	&
	&

19010	RESUME 19020 &

19020	IF ERR=11% AND (ERL=1050% OR ERL=1090% OR ERL=1110%) THEN &
		GOTO 32767 &
	ELSE	IF ERL=1110% OR ERL=1070% OR ERL=1090% OR ERL=5000% THEN &
			E$=" - "+S$ &
		ELSE	E$=" at Line "+NUM1$(ERL) &
	! TAKE CARE OF CONTROL/Z ON 'INPUT LINE'. &
	! ONLY EXPECTED, SPECIALLY FLAGGED ERRORS ARE OPEN ERRORS. &
	! FLAG ALL OTHERS WITH LINE # TO AID IN TRACK-DOWN OF REASON. &

19030	PRINT "?ANALYS - ";FNE$(ERR);E$ &
	! PRINT THE FATAL ERROR MESSAGE. &

19040	STOP IF DIAGNOSE% &
	\ GOTO 32700 &
	! IF WE ARE DEBUGGING, THEN DONT GO AWAY &
	! ELSE EXIT &

31000	! &
	! &
	!	C H A I N   E N T R Y &
	! &
	! &

31010	ENTRY.TYP%=2% &
	\ S$=SYS(CHR$(7%)) &
	\ DIAGNOSE%=CVT$%(S$) &
	\ GOTO 1000 &

32700	CLOSE -I% FOR I%=1% TO 12% &

32767	END
