2!		PROGRAM		: CONNECT.BAS
5!		VERSION		: V10.1
6!		EDIT		: G
7!		EDIT DATE	: 01-NOV-91
10	EXTEND
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1979, 1991 by &
  !	      Digital Equipment Corporation, Maynard, Mass. &
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

100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301	!	CHANNEL #	USED FOR &
	&
	!	0		terminal output &
	!	1		command input/output &
	!	2		remote keyboard &
	!	3		output transfer file &
	!	4		input transfer file &
	!	9		logging file &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !	&

900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &

910	DIM m%(128%)				!random SYS call array &
	\ DIM speed%(16%)			!interface speeds &

999	! &
	&
	!	M A I N    C O D I N G    A R E A &
	&
	&

1000	!------------------------------------------------------------! &
	!	GET dialer command &
	!------------------------------------------------------------! &
	ON ERROR GOTO 19000 &
	\ PRINT IF CCPOS(0%) &
		! set up standard error trap. &
		! return KB: to left margin. &

1010	i$="V10.1-G"				!set version id &
		! set up version/edit number. &

1020	GOSUB 10000 				!initialize constants &
	\ firstime%=NOT entry%			!TO display herald &
	\ GOTO 9000 				!treat AS IF interrupted &

1100	!------------------------------------------------------------! &
	!	CHAIN back entry point &
	!------------------------------------------------------------!
1110	GOSUB 10000 				!initialize constants
1120	OPEN "_KB:REMOTE.KBD" AS FILE 2%, RECORDSIZE 512%, MODE 1%+xon% &
	\ FIELD #2%, 512% AS f1$		!FIELD DATA &
	\ FIELD #2%, 1% AS kb$,511% AS f0$	!KB #, FIELD DATA &
	\ master%=kb0%				!initialize master &
	\ slave%,kb%,ctrl%=ASCII(MID(a$,2%,1%))	!initialize slave &
	\ z$=SYS(CHR$(6%)+CHR$(10%)+STRING$(20%,0%)+"KB"+CHR$(kb%)+CHR$(-1%)) &
	\ z$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%)+STRING$(4%,0%) &
		+CHR$(255%)+CHR$(128%)+STRING$(6%,0%)	!xon,echo &
		+CHR$(255%)+STRING$(3%,0%)		!stall &
		+CHR$(0%)+STRING$(8%,0%))		!(/ring) &
		! quickly OPEN the keyboard before enabling echo &
		! assign the remote keyboard &
		! set xon,echo,stall(,/ring)
1130	debug%=0%				!normally off &
	\ GOTO 1220 &
		! join main code TO READ INPUT &

1200	!------------------------------------------------------------! &
	!	remote keyboard &
	!------------------------------------------------------------!
1210	ON ERROR GOTO 19000 			!set standard error trap &
	\ OPEN "_KB:REMOTE.KBD" AS FILE 2%, RECORDSIZE 512%, MODE 1%+xon% &
	\ FIELD #2%, 512% AS f1$		!FIELD DATA &
	\ FIELD #2%, 1% AS kb$,511% AS f0$	!KB #, FIELD DATA &
	\ GOTO 1300 IF output.pending%		!OUTPUT ready TO go &
	\ GOTO 1240 				!skip echo clear
1220	!------------------------------------------------------------! &
	!	READ AND process INPUT from terminals &
	!------------------------------------------------------------!
1230	s%=0%					!wait FOR a loooong time &
	\ echo%,next.echo%=-1%
1240	GET #2%, RECORD 32767%+1%+16384%+4096%+s% !GET with timed wait &
	\ t%=0%					!reset error counter &
	\ kb%=ASCII(kb$)			!GET INPUT kb # &
	\ l%=RECOUNT-1% &
	\ f$=LEFT(f0$,l%) &
	\ GOTO 1300
1250	INPUT LINE #4%, f$			!transfer from FILE &
	\ kb%=master%				!set INPUT kb # &
	\ l%=LEN(f$)-1% &
	\ f$=LEFT(f$,l%) IF RIGHT(f$,l%)=cr.lf$ &
	\ l%=LEN(f$)
1300	!------------------------------------------------------------! &
	!	process escape delimiter &
	!------------------------------------------------------------!
1310	IF kb%=master% THEN kb%=slave% ELSE kb%=master%
1320	IF (kb%=slave%) AND ((ASCII(f$) AND 127%)=escape%) THEN 9000
1600	!------------------------------------------------------------! &
	!	write OUTPUT RECORD &
	!------------------------------------------------------------!
1610	GOTO 1630 IF (kb%=master%) AND (NOT echo%) &
	\ LSET f1$=f$ &
	\ PUT #2%, RECORD 32767%+1%+kb%+4096%,COUNT l% !binary multi-terminal &
	\ output.pending%=0% &
	\ GOTO 1630 UNLESS local.echo% &
	\ PRINT #1%, f$; IF kb%<>master%
1630	GOTO 1240 UNLESS (kb%=master%) OR debug% OR local.echo% &
	\ echo%=next.echo% IF fnscan.delimiter%(f$,CHR$(13%)) AND (kb%=master%) &
	\ PRINT #3%, f$; IF (to.me%<>0%) AND (kb%=master%) &
	\ GOTO 1240 UNLESS logging% &
	\ z%=fnlog.it%(f$,0%,-1%) &
	\ GOTO 1240 UNLESS debug% &
	\ CHANGE f$ TO m% &
	\ l%=128% IF l%>128% &
	\ PRINT #9% &
	\ PRINT #9%, "KB"+NUM1$(kb%)+": L="+NUM1$(l%);" F="; &
	\ PRINT #9%, " "+NUM1$(m%(i%)); FOR i% = 1% TO l% &
	\ PRINT #9% &
	\ GOTO 1240 &

9000	!------------------------------------------------------------! &
	!	handle escape MODE &
	!------------------------------------------------------------! &
!	CLOSE 2% &
!	\ OPEN "_KB:CONNECT.CMD" AS FILE 1% &

9010	GOTO 9040 IF entry% &
	\ PRINT #1%, "CONNECT"+CHR$(9%)+i$+CHR$(9%)+rev$ IF firstime% &
	\ firstime%=0% &
	\ PRINT #1%
9020	ON ERROR GOTO 19000 			!set standard error trap &
	\ continue%,e0%=0%
9030	PRINT #1% IF CCPOS(1%) &
	\ a$="" &
	\ GOTO 9080 IF continue% &
	\ GOSUB 10200 				!get a command line &
	\ GOTO 9080 UNLESS LEN(a$) IF slave%
9040	entry%=fnlog.it%(conn.mcr$+a$+cr.lf$,0%,last.logging%) &
	\ GOTO 9200  IF LEFT(a$,2%)="AS"	!assign &
	\ GOTO 9700  IF LEFT(a$,2%)="CL"	!close &
	\ GOTO 9900  IF LEFT(a$,3%)="COM"	!comment &
	\ GOTO 9080  IF LEFT(a$,4%)="CONT"	!continue &
	\ GOTO 9080  IF LEFT(a$,4%)="CONN"	!connect &
	\ GOTO 32000 IF LEFT(a$,3%)="BYE"	!bye &
	\ GOTO 9250  IF LEFT(a$,4%)="DIAL"	!dial &
	\ GOTO 9100  IF LEFT(a$,2%)="EC"	!echo &
	\ GOTO 32000 IF LEFT(a$,2%)="EX"	!exit &
	\ GOTO 9210  IF LEFT(a$,6%)="HANGUP"	!hangup &
	\ GOTO 9050  IF LEFT(a$,2%)="HE"	!help &
	\ GOTO 9150  IF LEFT(a$,3%)="LOG"	!log &
	\ GOTO 9300  IF LEFT(a$,3%)="SEN"	!send &
	\ GOTO 9400  IF LEFT(a$,3%)="SET"	!set &
	\ GOTO 9600  IF LEFT(a$,2%)="TR"	!transfer &
	\ GOTO 9050  UNLESS LEN(a$)
9045	entry%=fnlog.it%("?No such command as '"+a$+"'"+cr.lf$,1%,0%) &
	\ GOTO 9020 &
		! GO TO appropriate routine, ELSE tell user &

9050	PRINT #1%, "Valid commands are:" &
	\ PRINT #1%, " ASsign    - Assign remote keyboard" &
	\ PRINT #1%, " CLose     - Close transfer FILE" &
	\ PRINT #1%, " COMment   - Enter a comment into the log FILE" &
	\ PRINT #1%, " CONTinue  - Return to remote keyboard" &
	\ PRINT #1%, " CONNect   - Dial out or continue to remote keyboard" &
	\ PRINT #1%, " BYE       - Exit program (and hangup KB)" &
	\ PRINT #1%, " DIAL      - Dial out to remote keyboard" &
	\ PRINT #1%, " ECho      - Turn local echo ON or OFF" &
	\ PRINT #1%, " EXit      - Exit program (and hangup KB)" &
	\ PRINT #1%, " HANGUP    - Hangup remote keyboard" &
	\ PRINT #1%, " HElp      - Print this text" &
	\ PRINT #1%, " LOG       - Turn logging ON or OFF" &
	\ PRINT #1%, " SEND      - Send character to remote keyboard" &
	\ PRINT #1%, '  "<chr>"  - Send <chr> to remote keyboard' &
	\ PRINT #1%, "  XON      - Send ^Q to remote keyboard" &
	\ PRINT #1%, " SET       - Set DELAY,DTR,ESCAPE,SPEED,STOPBITS" &
	\ PRINT #1%, "  DElay    - Set delay timeout for GET" &
	\ PRINT #1%, "  DTR      - Set data terminal ready on remote keyboard" &
	\ PRINT #1%, "  ESCape   - Set program escape delimiter" &
	\ PRINT #1%, "  LOcal    - Set local echo for master keyboard" &
	\ PRINT #1%, "  SPeed    - Set speed on remote keyboard" &
	\ PRINT #1%, "  STopbits - Set stopbits on remote keyboard" &
	\ PRINT #1%, " TRansfer  - Transfer TO or FROM a disk FILE" &
	\ PRINT #1%, " <RETURN>  - Return to remote keyboard" &
	\ PRINT #1%, " ^Z        - Exit program (don't hangup KB)" &
	\ GOTO 9030 &
		! display what we can do AND ask again &

9080	PRINT #1% IF CCPOS(1%) &
!	\ CLOSE 1% &
	\ i%=fnextract.kb%(a$) &
	\ GOTO 9090 IF i% > 0% &
	\ GOTO 9250 IF LEFT(a$,4%)="CONN" &
	\ GOTO 1200 &

9090	a$=CHR$(0%)+CHR$(i%) &
	\ GOTO 1120 &
		! continue TO remote keyboard IF KB NOT specified &
		! skip the dial IF the LINE is NOT hungup &

9100	next.echo%=-1% &
	\ next.echo%=0% IF INSTR(1%,a$,"OFF") &
	\ GOTO 9030 &
		! turn local echo ON OR OFF &

9150	logging%=-1% &
	\ logging%=0% IF LEFT(a$,7%)="LOG OFF" &
	\ IF logging% THEN &
		z$=RIGHT(a$,4%) &
	\	z$=RIGHT(z$,4%) IF LEFT(z$,3%)=" ON" &
	\	IF LEN(z$) THEN &
			logger$=z$ &
	\		logger$=logger$+".LOG" UNLESS INSTR(1%,z$,".") &
	\		CLOSE 9% &
	\		z%=fnlog.it%(conn.mcr$+a$+cr.lf$,0%,-1%)
9155	last.logging%=logging% &
	\ GOTO 9030 &
		! turn logging ON OR OFF &

9200	kb%=fnextract.kb%(a$) &
	\ GOTO 9030 UNLESS kb% > 0% &
	\ slave%=kb% &
	\ z$=SYS(CHR$(6%)+CHR$(10%)+STRING$(20%,0%)+"KB"+CHR$(kb%)+CHR$(-1%)) &
	\ GOTO 9030 &
		! assign the remote keyboard &

9210	kb%=fnextract.kb%(a$) &
	\ kb%=slave% UNLESS kb% > 0% &
	\ GOSUB 11020 &
	\ local.echo%=0% &
	\ GOTO 9030 &
		! hangup remote keyboard &

9250	CLOSE 1%,2%,3%,4%,9%			!just in case &
	\ FOR i%=1% TO LEN(a$) &
	\ z$=MID(a$,i%,1%) &
	\ GOTO 9260 IF z$=" " OR z$="/" &
	\ NEXT i%
9260	a$=RIGHT(a$,i%) &
	\ myself$=device.ppn$+"CONNECT" &
	\ dialer$=device.ppn$+"DIALER" &
	\ z$=SYS(CHR$(8%)+'"'+MYSELF$+';31000"'+"CONNECT"+a$) !set core common &
	\ CHAIN dialer$ LINE 31000		!let dialer do it &
		! dial out TO remote site &

9300	i%=fnscan.delimiter%(a$,'" ') &
	\ send.xon%=1% &
	\ IF MID(a$,i%+1%,3%)="XON" THEN &
		f$=CHR$(17%) &
	  ELSE	send.xon%=0% &
	\	IF MID(a$,i%,1%)='"' THEN &
			f$=MID(a$,i%+1%,1%) &
		ELSE	f$=CHR$(VAL(RIGHT(a$,i%+1%)))
9310	l%=LEN(f$) &
	\ kb%=master% &
	\ output.pending%=-1% &
	\ IF send.xon% THEN &
		z$=SYS(CHR$(6%)+CHR$(-4%)+CHR$(master%)+f$) &
	\	z$=SYS(CHR$(6%)+CHR$(-4%)+CHR$(slave%)+f$) &
	\	xon%=32% IF from.me% OR to.me%
9320	GOTO 9030 &
		! Send a control character down the LINE &

9400	kb%=fnextract.kb%(a$) &
	\ kb%=slave% UNLESS kb% > 0% &
	\ a$=RIGHT(a$,5%) &
	\ a$=RIGHT(a$,INSTR(1%,a$," ")+1%) IF LEFT(a$,2%)="KB" &
	\ GOTO 9430 IF LEFT(a$,2%)="DE"		!delay &
	\ GOTO 9440 IF LEFT(a$,3%)="DTR"	!dtr &
	\ GOTO 9470 IF LEFT(a$,3%)="ESC"	!escape &
	\ GOTO 9460 IF LEFT(a$,2%)="LO"		!local echo &
	\ GOTO 9410 IF LEFT(a$,2%)="SP"		!speed &
	\ GOTO 9450 IF LEFT(a$,2%)="ST"		!stopbits &
	\ GOTO 9045 &
		! handle set commands, ELSE report error &

9410	speed%=fnextract.number%(a$," =:",300%) &
	\ CHANGE SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%)+STRING$(26%,0%)) TO m% &
	\ line.type%=(m%(19%)-12%)/2% &
	\ IF line.type% < 0% OR line.type% > 2% THEN &
		entry%=fnlog.it%("?Unsupported line interface type: "+NUM1$(m%(19%))+cr.lf$,1%,last.logging%) &
	  ELSE	IF kb%=0% THEN &
		entry%=fnlog.it%("?NOT connected to remote keyboard"+cr.lf$,1%,last.logging%) &
	  ELSE	RESTORE &
	\	READ speed%(i%) FOR i%=1% TO 16%	! DH11 &
	\	READ speed%(i%) FOR i%=1% TO 16% IF line.type%>=1%  ! DZ11 &
	\	READ speed%(i%) FOR i%=1% TO 16% IF line.type%=2%   ! DHV11 &
	\	i%=16% &
	\	i%=i%-1% WHILE (speed%<>speed%(i%)) AND (i%>0%) &
	\	IF i%=0% THEN &
			entry%=fnlog.it%("?Incorrect speed setting"+cr.lf$,1%,last.logging%) &
		ELSE	z$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%) &
				+STRING$(9%,0%)+CHR$(i%)+STRING$(16%,0%))
9415	GOTO 9030 &
		! set remote keyboard's speed &

9420	DATA 0,50, 75,110,-1,150,200,300, 600,1200,1800,2400,4800,9600,-1,-1
9421	DATA 0,50, 75,110,-1,150,300,600,1200,1800,2000,2400,3600,4800,7200,9600
9422	DATA 0,75,110,-1,150,300,600,1200,1800,2000,2400,4800,-1,9600,19200,-1
9423		! default LINE interface speeds &

9430	delay%=fnextract.number%(a$," =:",1%) &
	\ s%=delay% IF (from.me% OR to.me%) &
	\ GOTO 9030 &
		! set delay factor on multi-terminal GET &

9440	IF kb%=0% THEN &
		entry%=fnlog.it%("?NOT connected to remote keyboard"+cr.lf$,1%,last.logging%) &
	ELSE	z$=SYS(CHR$(6%)+CHR$(-9%)+CHR$(kb%)+CHR$(-1%))
9445	GOTO 9030 &
		! set DATA terminal ready on remote keyboard &

9450	stopbits%=0% &
	\ IF kb%=0% THEN &
		entry%=fnlog.it%("?NOT connected to remote keyboard"+cr.lf$,1%,last.logging%) &
	  ELSE	stopbits%=4% IF INSTR(1%,a$,"2") &
	\	z$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%) &
			+STRING$(15%,0%)+CHR$(8%+3%+stopbits%)+STRING$(9%,0%))
9455	GOTO 9030 &
		! set stopbits FOR remote keyboard &

9460	local.echo%=-1% &
	\ local.echo%=0% IF INSTR(1%,a$,"OFF") &
	\ GOTO 9030 &
		! set local echo ON or OFF &

9470	i%=fnscan.delimiter%(a$,'"=: ') &
	\ escape%=ASCII(MID(a$,i%+1%,1%)) IF i% &
	\ entry%=fnlog.it%(" esc="+CHR$(escape%)+cr.lf$,1%,last.logging%) UNLESS i% &
	\ GOTO 9030 &
		! set/display escape character &

9600	i%=INSTR(1%,a$," TO ") &
	\ IF i% THEN &
		ON ERROR GOTO 9630 &
	\	to.me%=-1% &
	\	logging%=last.logging% IF BUFSIZ(3%) &
	\	file.name$=RIGHT(a$,i%+4%) &
	\	CLOSE 3% &
	\	OPEN file.name$ FOR OUTPUT AS FILE 3% &
	\	logging%=0% &
!	\	next.echo%=echo% &
	\	s%=delay% &
	\	xon%=32% &
		! transfer TO disk FILE from remote keyboard &
		! temporarily turn off logging AND echo
9610	i%=INSTR(1%,a$," FROM ") &
	\ IF i% THEN &
		ON ERROR GOTO 9640 &
	\	from.me%=-1% &
	\	logging%=last.logging% IF BUFSIZ(4%) &
	\	file.name$=RIGHT(a$,i%+6%) &
	\	CLOSE 4% &
	\	OPEN file.name$ FOR INPUT AS FILE 4%, MODE 8192% &
	\	logging%=0% &
!	\	next.echo%=echo% &
	\	s%=delay% &
	\	xon%=32% &
		! transfer from disk FILE TO remote keyboard &
		! temporarily turn off logging AND echo
9615	GOTO 9030 &

9630	to.me%=fnlog.it%("?Protection violation on '"+file.name$+"'"+cr.lf$,1%,last.logging%) &
	\ RESUME 9020
9640	from.me%=fnlog.it%("?Can't find file or account '"+file.name$+"'"+cr.lf$,1%,last.logging%) &
	\ RESUME 9020 &
		! catch OPEN errors AND RESUME &

9700	CLOSE 3%,4% &
	\ to.me%,from.me%,xon%=0% &
	\ logging%=last.logging% &
	\ GOTO 9030 IF BUFSIZ(1%) &
	\ CLOSE 2% &
	\ s%=0% &
	\ echo%,next.echo%=-1% &
	\ GOTO 1200 &
		! CLOSE the INPUT/OUTPUT FILE &
		! go READ remote IF entered by END-of-FILE &

9900	f$=RIGHT(z$,fnscan.delimiter%(z$," =:")+1%) &
	\ PRINT #3%, f$; IF to.me% &
	\ PRINT #9%, f$; IF last.logging% &
	\ GOTO 9030 &
		! comment into the log FILE &

10000	!------------------------------------------------------------! &
	!	common subroutines &
	!------------------------------------------------------------!
10010	!------------------------------------------------------------! &
	!	initialize constants &
	!------------------------------------------------------------!
10020	ON ERROR GOTO 19000 			!set standard error trap &
	\ s$=SYS(CHR$(6%)+CHR$(-7%))		!enable ctrl/c trap &
	\ rev$=SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
	\ kb0%=(CVT$%(rev$) AND 255%)/2% &
	\ job%=(SWAP%(CVT$%(rev$)) AND 255%)/2% &
	\ job$="0"+NUM1$(job%)			!make job$ 2 digits &
	\ job$=RIGHT(job$,LEN(job$)-1%) &
	\ rev$=CVT$$(RIGHT(rev$,3%),4%) &
	\ GOSUB 10100				!GET package location &
	\ OPEN "_KB:CONNECT.CMD" AS FILE 1%, MODE 1% !GET channel OPEN once &
	\ FIELD #1%, 128% AS cmd$		!FIELD DATA &
	\ logger$="CONN"+job$+".LOG"		!construct logfile name &
	\ cr.lf$=CHR$(13%)+CHR$(10%) &
	\ conn.mcr$="CONN>" &
	\ logging%,last.logging%=0%		!start with logging off &
	\ echo%,next.echo%=-1%			!start with echo on &
	\ delay%=1%				!FOR FILE transfers &
	\ escape%=16%				!default escape=^p &
	\ xon%=0%				!start with xon off &
	\ RETURN &

10100	!------------------------------------------------------------! &
	!	GET package device & ppn &
	!------------------------------------------------------------!
10110	CHANGE SYS(CHR$(12%)) TO m%		!last OPEN FILE stats &
	\ device.ppn$="["+NUM1$(m%(6%))+","+NUM1$(m%(5%))+"]" &
	\ device.ppn$="_"+CHR$(m%(23%))+CHR$(m%(24%))+NUM1$(m%(25%))+":"+device.ppn$ &
		IF m%(26%) AND 1% &
	\ IF m%(3%)+SWAP%(m%(4%))<>15%*2% THEN &
		PRINT "?CONNECT must be RUN" &
	\	GOTO 32767 &
		! build name of device AND account of last opened file. &
		! we must have come from a compiled FILE so we can be &
		! sure that this name is really our package location.
10120	RETURN &
	&

10200	!------------------------------------------------------------! &
	!	GET a command LINE &
	!------------------------------------------------------------!
10210	GOTO 10220 UNLESS LEN(next.z$) &
	\ z$=next.z$ &
	\ GOTO 10290
10220	PRINT #1%, conn.mcr$; &
!	\ INPUT LINE #1%, z$ &
!	\ GOTO 10290
10230	z$="" &
	\ eol%=0% &
	\ WHILE eol%=0% &
	\ GET #1% &
	\ kmd$=LEFT(cmd$,RECOUNT) &
	\ IF ASCII(kmd$)=127% THEN			! DELETE/RUBOUT &
		kmd$=CHR$(8%)+" "+CHR$(8%) &
	\	z$=LEFT(z$,LEN(z$)-1%) &
	\	GOTO 10280
10240	IF ASCII(kmd$)=3% OR ASCII(kmd$)=26% THEN	!^C/^Z &
		z$=kmd$ &
	\	kmd$="^Z" UNLESS ASCII(kmd$)=3% &
	\	eol%=1% &
	\	GOTO 10280
10250	IF ASCII(kmd$)=21% THEN				!^U &
		PRINT #1%, kmd$ &
	\	GOTO 10220
10260	IF ASCII(kmd$)=18% THEN				!^R &
		PRINT #1%, kmd$ &
	\	kmd$=z$ &
	\	GOTO 10280
10270	kmd$=cr.lf$ IF ASCII(kmd$)=13% OR ASCII(kmd$)=10% ! CR/LF &
	\ z$=z$+kmd$ &
	\ eol%=fnscan.delimiter%(z$,CHR$(13%)+CHR$(12%)+CHR$(27%))
10280	PRINT #1%, kmd$; &
	\ NEXT
10290	PRINT #1% IF CCPOS(1%) &
	\ z%=INSTR(1%,z$+";",";") &
	\ next.z$=RIGHT(z$,z%+1%) &
	\ z$=LEFT(z$,z%-1%) &
	\ a$=CVT$$(z$,508%)			!256%+128%+64%+32%+16%+8%+4% &
	\ GOTO 32700 IF ASCII(a$)=3%		!^C &
	\ IF ASCII(a$)=26% THEN			!^Z &
		z$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%)+STRING$(4%,0%) &
			+CHR$(255%)+CHR$(255%)+STRING$(6%,0%)	!xon,local &
			+CHR$(255%)+STRING$(3%,0%)		!stall &
			+CHR$(0%)+STRING$(8%,0%))		!(/ring) &
						IF kb%>0% &
	\	GOTO 32710 &
		! set xon,local echo,stall(,/ring)
10295	continue%=(RIGHT(z$,LEN(z$))=CHR$(27%)) &
	\ RETURN &

11000	!------------------------------------------------------------! &
	!	hangup keyboard &
	!------------------------------------------------------------!
11010	kb%=slave%
11020	GOTO 11030 UNLESS kb% &
	\ z%=fnhangup.kb%(kb%) &
	\ z$=SYS(CHR$(6%)+CHR$(11%)+STRING$(20%,0%)+"KB"+CHR$(kb%)+CHR$(-1%)) &
	\ z$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%)+STRING$(4%,0%) &
		+CHR$(255%)+CHR$(128%)+STRING$(6%,0%)	!xon,echo &
		+CHR$(255%)+STRING$(3%,0%)		!stall &
		+CHR$(0%)+STRING$(8%,0%))		!(/ring) &
	\ slave%=0% IF kb%=slave%
11030	RETURN &
		! hangup remote keyboard &
		! deassign remote keyboard &
		! set characteristics TO xon,echo,stall(,/ring) &

15000	!------------------------------------------------------------! &
	!	user defined functions &
	!------------------------------------------------------------!
15100	!------------------------------------------------------------! &
	!	extract keyboard number from string &
	!------------------------------------------------------------!
15110	DEF* fnextract.kb%(a$) &
	\ fnextract.kb%=-1% &
	\ a%=fnscan.delimiter%(a$,":") &
	\ a$=LEFT(a$,a%-1%) IF a% &
	\ a%=INSTR(1%,a$," KB") &
	\ a$=" "+RIGHT(A$,A%+3%) IF A% &
	\ fnextract.kb%=fnextract.number%(a$," ",-1%) IF A% &
	\ FNEND
15200	!------------------------------------------------------------! &
	!	extract number from string &
	!------------------------------------------------------------!
15210	DEF* fnextract.number%(a$,delimiter$,default%) &
	\ fnextract.number%=default% &
	\ a%=fnscan.delimiter%(a$,delimiter$) &
	\ fnextract.number%=VAL(RIGHT(a$,a%+1%)) IF a% &
	\ FNEND
15300	!------------------------------------------------------------! &
	!	PRINT on OUTPUT channel AND log IF desired &
	!------------------------------------------------------------!
15310	DEF* fnlog.it%(a$,chan%,logit%) &
	\ GOTO 15320 UNLESS chan% &
	\ PRINT #chan% IF CCPOS(chan%) &
	\ PRINT #chan%, a$;
15320	GOTO 15340 UNLESS logit% &
	\ GOTO 15330 IF BUFSIZ(9%)		!skip IF already OPEN &
	\ OPEN logger$ AS FILE 9%, MODE 2% &
	\ PRINT #9%, CHR$(12%)			!...at top of form
15330	PRINT #9%, a$;
15340	fnlog.it%=0% &
	\ FNEND
15400	!------------------------------------------------------------! &
	!	scan TO delimiter &
	!------------------------------------------------------------!
15410	DEF* fnscan.delimiter%(a$,delimiter$) &
	\ WHILE LEN(delimiter$) &
	\ i%=INSTR(1%,a$,LEFT(delimiter$,1%)) &
	\ GOTO 15420 IF i% &
	\ delimiter$=RIGHT(delimiter$,2%) &
	\ NEXT
15420	fnscan.delimiter%=i% &
	\ FNEND &

15700	!------------------------------------------------------------! &
	!	hangup a data set &
	!------------------------------------------------------------!
15710	DEF* fnhangup.kb%(kb%) &
	\ z$=SYS(CHR$(6%)+CHR$(-9%)+CHR$(kb%)+CHR$(0%)) &
	\ SLEEP 5%
15720	FNEND &
		! hangup it up &
		! sleep just to make sure &

19000	!------------------------------------------------------------! &
	!	handle all errors &
	!------------------------------------------------------------!
19010	e0%=ERR &
	\ IF e0%=27% AND ERL=1240 THEN		!?I/O to detached keyboard &
		t%=t%+1% &
	\	RESUME 19090 IF t%>3% &
	\	e0%=0% &
	\	SLEEP 10% &
	\	RESUME 1240
19020	IF e0%=13% AND ERL=1240 THEN		!?User data error on device &
		IF from.me% THEN RESUME 1250 ELSE RESUME 1220
19030	IF e0%=11% THEN				!?End of file on device &
	IF ERL=1250 THEN &
		RESUME 9700 &
	ELSE	IF ERL=10220 THEN &
		slave%=0% &
	\	z$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(kb%)+STRING$(4%,0%) &
			+CHR$(255%)+CHR$(255%)+STRING$(6%,0%)	!xon,local &
			+CHR$(255%)+STRING$(3%,0%)		!stall &
			+CHR$(0%)+STRING$(8%,0%))		!(/ring) &
						IF kb%>0% &
		! set xon,local echo,stall(,/ring)
19040	IF e0%=28% THEN RESUME 32000
19090	IF e0%<>11% THEN &
		PRINT #1%, "?ERR =";e0%;"at line";ERL; &
		CVT$$(MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(e0%)),3%,27%),4%);
19100	RESUME 32000 UNLESS (slave%) &
	\ RESUME 9000 &

30000	!------------------------------------------------------------! &
	!	ccl entry point &
	!------------------------------------------------------------!
30010	a$=SYS(CHR$(7%)) &
	\ z$=RIGHT(a$,8%) IF LEFT (a$,7%)="CONNECT" &
	\ entry%=-1% IF LEN(z$) &
	\ GOTO 1000 &
	! GET the ccl core common string, IF nothing in string just &
	! do a normal run. otherwise, set up entry AS ccl. &

31000	!------------------------------------------------------------! &
	!	CHAIN entry point &
	!------------------------------------------------------------!
31010	a$=SYS(CHR$(7%))			!GET core common &
	\ GOTO 1100 UNLESS ASCII(a$)		!we have success &
	\ PRINT "?Connect failure...try again later" &
	\ GOTO 32700 &
		! abort IF dialer didn't make it &

32000	!------------------------------------------------------------! &
	!	exit handler &
	!------------------------------------------------------------!
32700	GOSUB 11000
32710	CLOSE 1%,2%,3%,4%,9%
32767	END
