2!		PROGRAM		: MAPHIS.BAS
5!		VERSION		: V10.1
6!		EDIT		: C
7!		EDIT DATE	: 19-JUL-91
10	EXTEND
11	! &
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
  !******************************************************************* &
  !
20	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	!
21	! VER/ED	EDIT DATE	REASON &
	! &
	!
100	! &
	!	G E N E R A L    D E S C R I P T I O N &
	!
110	!	This program creates the RSTS.SYM file for PRTHIS to look &
	!	up PSECT starting and ending values in. &
	!
300	! &
	!	I / O    C H A N N E L S &
	!
301	!	CHANNEL #		USED FOR &
	!	  1			OUTPUT FILE &
	!	  2			MAP FILES &
	!	  12			TTY INPUT AND OUTPUT
400 	! &
	!	VARIABLE USAGE DESCRIPTIONS &
	!
401 	! VARIABLE NAME		USED FOR
410 	! Q$			COMMAND STRING &
	! O$			OUTPUT FILE &
	! M$			MAP FILE
800	! &
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	! &
	!	FUNCTION/SUBROUTINE		USE &
	! &
	!	FNO$			DECIMAL TO OCTAL &
	!	FNV%			RETURN VALUE OF A STRING &
	!
900	! &
	!	D I M E N S I O N    S T A T E M E N T S &
	!
905 	GOSUB 10000 \ LSET V$=SYS(CHR$(0%)) \ PRINT IF CCPOS(0%)<>0% &
	\ PRINT N9$+" "+V9$; \ PRINT"-"+V0$; IF LEN(V0$)<>0% &
	\ PRINT" - "+RIGHT(SYS(CHR$(6%)+CHR$(9%)),3%)
910 	OPEN "_KB:INPUT.CMD" AS FILE 12%	! OPEN KB WITH NO PROMPT
999	! &
	!	M A I N    C O D I N G    A R E A &
	!
1000	LSET V$=SYS(CHR$(0%)) \ PRINT \ O$="SY:RSTS.SYM" &
	\ PRINT "Output to <";O$;"> ? "; &
	\ INPUT LINE#12%,Q$ \ O9$=CVT$$(Q$,-1%) \ O9$=O$ IF O9$=""
1010	LSET V$=SYS(CHR$(0%)) \ PRINT \ I$="SY:" &
	\ PRINT "Read .MAPs from <";I$;"[CURRENT ACCOUNT]> ? "; &
	\ INPUT LINE#12%,Q$ \ I9$=CVT$$(Q$,-1%) \ I9$=I$ IF I9$=""
1020	PRINT \ H$=STRING$(8%,-1%) \ RESTORE
1030	OPEN O9$ FOR OUTPUT AS FILE 1%
1999	! &
	!	NOW DO IT PLEASE
2000	READ M$ UNTIL M$="*MAPS*"
2010	READ M$ \ IF M$="*END*" THEN GOTO 3000	! DONE NORMAL STUFF
2015	PRINT "Looking for ";I9$+M$+".MAP.....";
2020	OPEN I9$+M$+".MAP/RO" FOR INPUT AS FILE 2%	! OPEN MAP FILE
2030	PRINT "Reading it...."; \ GOSUB 6000	! DO IT
2040	PRINT#1%,H$;	! WRITE EOF PLEASE
2045	PRINT "Done map"
2050	CLOSE 2% \ GOTO 2010
2999	! &
	!	NOW DO OVR PLEASE &

3000	PRINT #1%,H$; &
	\ M$="OVR" &
	\ PRINT "Looking for ";I9$;"OVR.MAP....."; &
	! &
	! Write extra EOF for "other" phases &
	! Set module name to OVR &
	! Tell the user what's going on &

3010	OPEN I9$+"OVR.MAP/RO" FOR INPUT AS FILE 2%	! OPEN IT
3020	PRINT "Reading it...."; &
	\ PRINT#1%,"CTL   ";CVT%$(0%);"OPN   ";CVT%$(SWAP%(512%)); &
	\ GOSUB 6000	! NOW DO IT
3030	PRINT#1%,H$;	! WRITE EOF PLEASE
3040	PRINT "Done map"	! ALL DONE
3050	GOTO 4000
3999	! &
	!	NOW DO FIP CODES PLEASE
4000	RESTORE \ READ C$ UNTIL C$="*FIP*"
4010	A%=0% \ PRINT "Doing FIP function codes ....";
4020	READ C$ \ IF C$="*END*" THEN GOTO 4040	! DONE SPECIAL CODES
4030	PRINT#1%,LEFT(C$+"      ",6%)+CVT%$(SWAP%(A%)); &
	\ A%=A%+512% \ GOTO 4020
4040	PRINT#1%,STRING$(6%,0%)+CVT%$(SWAP%(A%));	! 'HIGH' ADDRESS
4050	PRINT#1%,H$;	! WRITE EOF PLEASE
4060	PRINT "Done function codes"
4070	GOTO 9000
5999	! &
	!	DO THIS MAP
6000	INPUT LINE#2%,L$ \ L1$=CVT$$(L$,-1%) &
	\ GOTO 6000 IF INSTR(1%,L1$,".ABS.")=0%
6005	M9%=-1% \ C9%=-1%	! LOOK FOR MERGE, FIRST TIME THROUGH
6010	INPUT LINE#2%,L$ \ GOTO 6010 IF INSTR(20%,L$,",")=0%
6020	L1$=CVT$$(L$,1%+4%+8%+32%+128%)	! NOW MAKE IT USEABLE
6025	IF C9% THEN C9%=0% \ M9%=0% IF LEFT(L1$,6%)<>"MERGE "
6027	IF M9% THEN T0$=LEFT(L1$,6%) &
	\ M9%=0% IF ((T0$="MERG99") OR (T0$="MERGE9")) \ GOTO 6010
6030	J%=INSTR(20%,L1$,"LIMIT = ") \ GOTO 6080 IF J%<>0%
6040	PRINT#1%,LEFT(L1$,6%)+CVT%$(SWAP%(FNV%(MID(L1$,9%,6%))));
6050	GOTO 6010	! NEXT LINE PLEASE
6060	PRINT \ PRINT "?No .ABS. section in map ";M$;" - Fatal error" \ STOP
6070	PRINT \ PRINT "%No High limit= line in map ";M$ &
	\ J%=-7% \ L1$="177776" ! DUMMY IT PLEASE
6080	PRINT#1%,STRING$(6%,0%)+CVT%$(SWAP%(FNV%(MID(L1$,J%+8%,6%)))); &
	! HIGH LIMIT VALUE PLEASE
6090	RETURN
7999	! &
	!	DATA STATEMENTS
8000	DATA	*MAPS*,RSTS,GEN,EMT,EM2,FIP,KBD,DSK,FMS,SES,NSP,TRN,XVR
8001	DATA	MCP,BBR,OPN,RSX,MVR,KVR,RDM,EVL,UNA,QNA,DMP,NOD,GRD,KIN
8002	DATA	PKPORT,DLPORT,DHPORT,DZPORT,VHPORT,TERCLS,LAT,*END*
8009	! &
	!	FIP FUNCTION CODES
8010	DATA	*FIP*,CLSFQ,OPNFQ,CREFQ,DLNFQ,RENFQ
8020	DATA	DIRFQ,UUOFQ,ERRFQ,RSTFQ,LOKFQ
8030	DATA	ASSFQ,DEAFQ,DALFQ,CRTFQ,CRBFQ
8040	DATA	RUNFQ,PFBFQ,EOVFQ,MTAFQ,WINFQ
8050	DATA	EXTFQ,NETFQ,BYEFQ,REMFQ,DCLFQ
8060	DATA	STAFQ,DECFQ,TRUFQ,DSPFQ,*END*
8999	! &
	!	END IT ALL
9000	CLOSE 1%,2%
9010	PRINT \ PRINT "Done building ";O9$ \ PRINT
9020	GOTO 19990
9998 	! &
	!	INTERNAL FUNCTIONS AND SUBROUTINES &
	!
9999 	!	INIT ROUTINE
10000 	ON ERROR GOTO 19000 &
	\ LSET V$=SYS(CHR$(6%)+CHR$(-7%)) &
	! CONTROL C TRAP AND NORMAL ERROR
10010	N9$="MAPHIS"	! PROGRAM NAME
10020	V9$="V10.1"	! BASE LEVEL
10030	V0$="B"		! EDIT DATE: 07-Jul-91 by Fred Knight
10040 	RETURN
10899	! &
	! 	DECIMAL TO OCTAL STRING
10900	DEF FNO$(X%) &
	\ X5$="0" &
	\ IF X%<0% THEN X5$="1" &
	\ X%=X% AND 32767%
10910	X5$=X5$+CHR$(((X% AND 28672%)/4096%)+48%)+ &
	CHR$(((X% AND 3584%)/512%)+48%)+ &
	CHR$(((X% AND 448%)/64%)+48%)+CHR$(((X% AND 56%)/8%)+48%)+ &
	CHR$((X% AND 7%)+48%) &
	\ FNO$=X5$
10920	FNEND
10999	! &
	!	GET VALUE OF ASCII STRING
11000	DEF FNV%(X$) \ X%=0%
11010	DIM X1%(9%) \ X1$=RIGHT("000000"+X$,1%+LEN(X$)) \ CHANGE X1$ TO X1%
11020	X%=32767%+1% IF X1%(1%)<>48%
11030	X%=X%+	(4096%*(X1%(2%)-48%))+( 512%*(X1%(3%)-48%))+ &
		(  64%*(X1%(4%)-48%))+(   8%*(X1%(5%)-48%))+ &
		(       X1%(6%)-48% )
11040	FNV%=X% \ FNEND
18999 	! &
	!	ERROR HANDLING GOES HERE &
	!
19000 	IF ERR=28% THEN RESUME 19990	! ^C TRAP
19010	IF ERR=11% AND ERL=1000% THEN RESUME 19990
19020	IF ERR=11% AND ERL=1010% THEN RESUME 1000
19030	IF ERR=5% AND ERL=2020% THEN RESUME 2040
19040	IF ERR=11% AND ERL=6000% THEN RESUME 6060
19050	IF ERR=11% AND ERL=6010% THEN RESUME 6070
19060	IF ERR=5% AND ERL=3010% THEN RESUME 3040
19990 	CLOSE I% FOR I%=1% TO 12% \ ON ERROR GOTO 0 &
	\ GOTO 32767
29999 	! &
	!	CCL ENTRY POINT &
	!
30000	GOTO 32767 ! NO CCL ENTRY
32766 	! &
     !******** &
     !	THE  *
32767	END
