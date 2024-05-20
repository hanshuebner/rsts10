2!		PROGRAM		: BPLGEN
5!		VERSION		: V10.1
6!		EDIT		: C
7!		EDIT DATE	: 19-JUL-91
8!
10		EXTEND		! PROGRAM IS WRITTEN IN EXTEND MODE
11	! &
	! &
	! &
	!		  C O P Y R I G H T &
  !	&
  !	&
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
  !*******************************************************************
20	! &
	! &
	! &
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! V10.0-C	01-Sep-89	(JJT) Add ability to update all B+ RTS &
	! V10.0-I	15-Feb-90	(WJS) ASSIGN, debugging support &
	! V10.0-K+	18-May-90	(JJT) Make Old Send/Receive a prompt &
	!				      Check size of new .RTS &
	!				      Rename old .RTS to .BAK &
	!				      If updating a pre-V10.0 BASIC, &
	!				      ask Old Send/Receive prompt &
	! V10.1-C	19-Jul-91	(JJT) Restore default on math prompt &

100	! &
	! &
	! &
	!	P R O G R A M   D E S C R I P T I O N &
	&

110!	The BPLGEN program is used to create a .COM file that will create &
   !	a customized Basic-Plus run-time system.  It will ask if the user &
   !	wants to use an existing Basic-Plus run-time system as a template &
   !	for the one being created. &
   !	&

300	! &
	! &
	! &
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
311!		1	USER DIALOGUE &
   !		2	Template BASIC RTS file &
   !		3	BPLGEN.COM file
399!	&

400	! &
	! &
	! &
	!	V A R I A B L E    D E F I N I T I O N S &

401!		VARIABLE   DEFINITION &
   !		--------   ---------- &

700	! &
	! &
	! &
	!	P R O G R A M    L A Y O U T &
	&

701!	STMT#	DESCRIPTION &
   !	-----	----------- &

800	! &
	! &
	! &
	!	D I C T I O N A R I E S &
	&

801!	SUBROUTINE	USE &
   !	----------      ---
802!	FUNCTION	STMT#	DESCRIPTION &
   !	--------	-----	----------- &
   !	FNP%(R$)	16200	DIALOGUE FUNCTION(RETURNS ONE OF 6 RESPONSES) &
   !	&

900	! &
	! &
	! &
	!	D I M E N S I O N     S T A T E M E N T S &
	&

910	DIM	BP.SYM$(21%), &
		BP.VAL%(21%), &
		FIRQB%(30%), &
		FIRQ2%(30%) &
	&


999	! &
	! &
	! &
	!	M A I N   P R O G R A M   L O G I C &
	&

1000	  ON ERROR GOTO 19000 &
		! Set up standard error trap &

1010	  I$="V10.1-C" &
		! Version - Edit #'s. &

1015	S$=SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
\	PRINT IF CCPOS(0%) &
\	PRINT FOR I% = 1% TO 2% &
\	PRINT "Beginning of RSTS/E Basic-Plus generation." &
\	PRINT &
!	Read system header information &
!	Insure we're at the left margin &
!	Print the program header &

1020	bp.sym$(1%) = "DUP2  " &
\	bp.sym$(2%) = "MI.BAD" &
\	bp.sym$(3%) = "DUP4  " &
\	bp.sym$(4%) = "DSCTST" &
\	bp.sym$(5%) = "DEBUG " &
\	bp.sym$(6%) = "PU    " &
\	bp.sym$(7%) = "SF    " &
\	bp.sym$(8%) = "MX    " &
\	bp.sym$(9%) = "MAXL  " &
\	bp.sym$(10%)= "MAXT  " &
\	bp.sym$(17%)= "STR$LI" &
\	bp.sym$(18%)= "NUM$00" &
\	bp.sym$(19%)= "NUM$XX" &
\	bp.sym$(20%)= ".EQ.S " &
\	bp.sym$(21%)= "M18   " &
!	Define the symbols we will look up in the template BASIC RTS file &

1030	OPEN "_KB:" AS FILE 1%		!Open the user's KB on a channel &
\	OPEN "_SY:BPLGEN.COM" FOR OUTPUT AS FILE 3%, MODE 32%+128% &
					!Open the .COM file, &
					!  Tentative and no supersede &
\	GOTO 1040			!Continue &

1035	KILL "_SY:BPLGEN.BAK"		!Kill any existing .BAK file &

1037	NAME "_SY:BPLGEN.COM" AS "_SY:BPLGEN.BAK" !Make a backup file &
\	GOTO 1030			!Try it again &

1040	OUT.CHN% = -3%			!Default to killing the tentative file &
\	GET.SIZE% = 0%			!Clear 'Just get size' flag &
\	TO.DAY$ = MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+ &
		CHR$(-1%)+CHR$(SWAP%(-1%))),7%,9%) &
\	PRINT #3%, "$!	BPLGEN.COM - COMMAND PROCEDURE TO CREATE BASIC PLUS "; &
\	PRINT #3%, TO.DAY$ &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$!" &
\	COPYRIGHT$ = "COPYRIGHT (c) 1974, 19" &
\	PRINT #3%, "$!		" + COPYRIGHT$; &
\	PRINT #3%, RIGHT(TO.DAY$,8%); " BY" &
\	PRINT #3%, "$!	DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS." &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$_deassign/all" &
\	PRINT #3%, "$_set noecho/warning" &
\	PRINT #3%, "$_defi/comm/syst/repl bplgen BASIC$$:BPLGEN.*/line=CCL" &
\	PRINT #3%, "$_set data" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$RETURN := EXIT" &
\	PRINT #3%, "$_on ERROR then _goto ERROR_TRAP" &
\	PRINT #3%, "$_goto START" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$INSBAS:" &
\ PRINT #3%, '$ _if f$search("SYSTEM$$:'+"''BASNAM'"+'.RTS") .nes. "" then -' &
\ PRINT #3%, "	 _rename/nolog/repl/nowarn SYSTEM$$:'BASNAM'.RTS *.BAK" &
\	PRINT #3%, "$ _copy/nolog 'BASNAM'.RTS SYSTEM$$:*.*/cont/rep" &
\	PRINT #3%, "$ _delete/nolog 'BASNAM'.RTS" &
\	PRINT #3%, "$ _set noon" &
\	PRINT #3%, "$ _set noecho/nowarning" &
\	PRINT #3%, "$ _remove/runtime 'BASNAM'" &
\	PRINT #3%, "$ _install/runtime 'BASNAM'" &
\	PRINT #3%, '$ NT = ""' &
\	PRINT #3%, '$ _if $STATUS .ne. 1 then NT = "not "' &
\	PRINT #3%, '$ _write 0 "' + "''BASNAM'" + &
			" is created and is ''NT'installed" + '"' &
\	PRINT #3%, "$ _set echo" &
\	PRINT #3%, "$ bplgen SYSTEM$$:'BASNAM'.RTS" &
\	PRINT #3%, "$ _set noecho/warning" &
\	PRINT #3%, "$ _set on" &
\	PRINT #3%, "$ _return" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$ERROR_TRAP:" &
\	PRINT #3%, "$_on ERROR then _goto ERROR_TRAP" &
\	PRINT #3%, "$_goto 'RETURN'" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$EXIT:" &
\	PRINT #3%, "$_exit $STATUS" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$START:" &
\	PRINT #3%, "$_assign BASIC$$: IN" &
!	Print the .COM file's header information &

1050	GOSUB 14000			!Go initialize the defaults &
\	SYS.ACC$ = "SYSTEM$$:"		!Default to SYSTEM$$: &
\	CHANGE SYS(CHR$(6%)+CHR$(-10%)+SYS.ACC$) TO FIRQB% !Valid logical? &
\	S1% = FIRQB%(29%) + SWAP%(FIRQB%(30%))	!Let's see... &
\	SYS.ACC$ = "_SY0:[0,1]" IF S1% < 0%	!No, hard code it to SY0:[0,1] &
\	GOSUB 10010			!Print a directory of the Basic RTS's &
\	ABASIC$ = CVT$$(N1$,2%) IF HEAD.ER% !Get the filename if we got one &
\	GOTO 1055 IF HEAD.ER%		! and continue &
\	GOTO 1080			!No existing Basic RTS &
!	Print the header and goto the first prompt &

1053	PRINT &
\	PRINT "The BASIC build has  the  capability  of" &
\	PRINT "updating any or all  of  the  BASIC-PLUS" &
\	PRINT "run-time systems listed.  Please enter a" &
\	PRINT "list  of  the  names  of  the   run-time" &
\	PRINT "systems, separated by commas,  that  you" &
\	PRINT "wish to update. Press <return> to update" &
\	PRINT "all of the BASIC-PLUS  RTS's,  or  enter" &
\	PRINT "NONE to update none of them." &
!	Print the help &

1055	LIST.RTS$ = ALL.RTS$ &
\	PRINT &
\	ON FNP%("BASIC-PLUS RTS's to Update","ALL") &
		GOTO &
			1055,	1057,	1057,	1059,	1057,	1053 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &
	&

1057	GOTO 1065 IF W$ = "NONE" &
\	LIST.RTS$ = W$ UNLESS W$ = "ALL" &

1059	LIST.RTS$ = LIST.RTS$ + ","		!Tack on a comma &
		UNLESS RIGHT(LIST.RTS$,LEN(LIST.RTS$)) = ","!Unless not needed &
\	S% = 0% &
\	WHILE -1% &
\		S1% = S%			!Save old position of comma &
\		S% = INSTR(S%+1%,LIST.RTS$,",") &
\		GOTO 1061 UNLESS S% &
\		I$ = MID(LIST.RTS$,S1%+1%,S%-S1%-1%) &
\		I% = INSTR(1%,ALL.RTS$,I$+",") &
\		GOTO 1060 IF (I% <> 0%) AND (LEN(I$) <> 0%)	 !It's valid &
\		PRINT "?'" + I$ + "' is not a BASIC-PLUS run-time system" &
\		GOTO 1055			!No good, ask again &

1060	NEXT &

1061	PRE.V10.M18$ = ""			!Init pre-V10 Old S/R answer &
\	GO.BACK% = 0%				!Clear 'Go back a prompt' flag &
\	WHILE LEN(LIST.RTS$)			!Loop thru the RTS's &
\		S% = INSTR(1%,LIST.RTS$,",")	! Search for comma &
\		I$ = LEFT(LIST.RTS$,S%-1%)	! Get the RTS name &
\		LIST.RTS$ = RIGHT(LIST.RTS$,S%+1%)! Strip it off &
\		BASNAM$ = I$			! Save the filename &
\		I$ = SYS.ACC$ + I$ + ".RTS"	! Get full filespec &
\		PRINT "Working on "+BASNAM$	! Let them what we're doing &
\		GOSUB 12000			! Check it out &
\		GOSUB 14500 IF (PRE.V10% <> 0%)	! Ask 'Old S/R' if pre-V10 &
			   AND (LEN(PRE.V10.M18$) = 0%) !and not asked before &
\		GOTO 1030 IF GO.BACK%		! Go back at user's request &
\		PRE.V10.M18$ = M18$ IF (PRE.V10% <> 0%)	!Store 'Old S/R' answer if pre-V10 &
			   AND (LEN(PRE.V10.M18$) = 0%) !and not asked before &
\		M18$ = PRE.V10.M18$ IF (PRE.V10% <> 0%)	!Set 'Old S/R' if pre-V10 &
\		GOSUB 13000			! Output to BPLGEN.COM &
\	NEXT					!Next RTS &
\	GOTO 1210 				!Ask if anymore to do &

1063	PRINT &
\	PRINT "The BASIC build has  the  capability  of" &
\	PRINT "obtaining  the  necessary  configuration" &
\	PRINT "information  from  a  template  RTS.  If" &
\	PRINT "you wish to copy  the  configuration  of" &
\	PRINT "that template  run-time  system,  answer" &
\	PRINT "YES to this question, otherwise type NO." &
!	Print the help &

1065	PRINT &
\	ON FNP%("Use template run-time system","Y") &
		GOTO &
			1210,	1070,	1080,	1070,	1065,	1063 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &
	&

1067	PRINT &
\	PRINT "A run-time system has a name from 1 to 6" &
\	PRINT "alphanumeric characters and  a  filetype" &
\	PRINT 'of "RTS". Enter the name of the template' &
\	PRINT "run-time  system  whose  parameters  you" &
\	PRINT "wish to use." &
\	GOSUB 10010 &
!	Print help and display directory of all Basic RTS's &

1070	TMPNAM$ = ABASIC$ &
\	PRINT &
\	ON FNP%("Template run-time system's name",ABASIC$) &
		GOTO &
			1065,	1071,	1071,	1072,	1071,	1067 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1071	TMPNAM$ = W$			!Get the answer &

1072	CHANGE SYS(CHR$(6%)+CHR$(-10%)+TMPNAM$) TO FIRQB% !Scan it &
\	S1% = FIRQB%(29%) + SWAP%(FIRQB%(30%))	!Get the scan word &
\	TMPNAM$ = SYS.ACC$ + TMPNAM$ IF ((S1% AND 4096%) = 0%)	!No device &
				    AND ((S1% AND  128%) = 0%)  ! and no PPN &
\	TMPNAM$ = TMPNAM$ + ".RTS" IF (S1% AND 8%) = 0%		!Add filetype &
\	TMPFIL$ = RAD$(FIRQB%(7%)+SWAP%(FIRQB%(8%))) +		!Get filename &
		  RAD$(FIRQB%(9%)+SWAP%(FIRQB%(10%))) &
\	IF S1% < 0% THEN PRINT "?Invalid file specification"	!Error if bad &
\		GOTO 1070					!and ask again &

1073	I$ = TMPNAM$			!Set up to check it out &
\	GOSUB 12000			!Check it out &
\	GOTO 1070 IF INV.BAS%		!Ask again if it was bad &
\	PRINT &
\	PRINT &
\	print "The characteristics are:" &
\	PRINT "	Floating Point Processor" IF AFPP$ = "Y" &
\	PRINT "	Floating Instruction Set" IF AFIS$ = "Y" &
\	PRINT "	Extended Instruction Set" IF (AFPP$ <> "Y") AND (AFIS$ <> "Y") &
					 AND (OLD.MTH$ <> "D") &
\	print "	"+AMATH$+"-word Math precision" &
\	print "	"+FNNO$(bp.val%(9%))+"Log functions" &
\	print "	"+FNNO$(bp.val%(10%))+"Trig functions" &
\	print "	"+FNNO$(bp.val%(6%))+"Print Using" &
\	print "	"+FNNO$(bp.val%(8%))+"Matrices" &
\	print "	"+FNNO$(bp.val%(7%))+"String arithmetic" &
\	print "	"+FNNO$(bp.val%(5%))+"Debug" &
\	print "	"+FNNO$(bp.val%(21%))+"Old-style SEND/RECEIVE" &
\	print &
\	ABASIC$ = CVT$$(TMPFIL$,2%) &
\	GOTO 1080 &
!	Print the characteristics &

1077	PRINT &
\	PRINT "The  BASIC-PLUS Save Image Library (SIL)" &
\	PRINT "will have  a name  of from 1 to 6 alpha-" &
\	PRINT "numeric characters  and  a  filetype  of" &
\	PRINT '"RTS". Please specify the name you want.' &
!	Print the help &

1080	BASNAM$ = ABASIC$ &
\	PRINT &
\	ON FNP%("New BASIC-PLUS RTS name",BASNAM$) &
		GOTO &
			1082,	1090,	1090,	1092,	1090,	1077 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1082	GOTO 1065 IF HEAD.ER%		!Go back to template if asked &
\	GOTO 1080			!We didn't, loop on this prompt &

1090	BASNAM$ = W$			!Get the response &

1092	CHANGE SYS(CHR$(6%)+CHR$(-10%)+BASNAM$) TO FIRQB% !FSS it &
\	S1% = FIRQB%(29%) + SWAP%(FIRQB%(30%)) !Get the scan word &
\	GOTO 1096 IF (S1% AND (1%+2%+4%+8%+128%+1024%+4096%))=1% !Ok if name &
\	PRINT "Please only specify the RTS name" !Let them know what we want &
\	GOTO 1080			!Ask again &

1094	PRINT &
\	PRINT	"The BASIC  build  has  all  the  default" &
\	PRINT	"configuration  information.   If   these" &
\	PRINT	"defaults are valid, answer YES  to  this" &
\	PRINT	"question. If there will  be  changes  to" &
\	PRINT	"the information, type NO." &
!	Print the help &

1096	PRINT &
\	ON FNP%("Accept defaults","N") &
		GOTO &
			1080,	1200,	1106,	1106,	1096,	1094 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1105	PRINT &
\	PRINT	"Will this  software  run  on  a computer" &
\	PRINT	"with  a  floating  point  processor (YES" &
\	PRINT	"or NO)?" &
!	Print the help &

1106	PRINT &
\	ON FNP%("Floating Point Processor",AFPP$) &
		GOTO &
			1096,	1107,	1107,	1108,	1106,	1105 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1107	FPP$ = LEFT(W$,1%)		!"Y" or "N" &

1108	GOTO 1120 IF FPP$ = "Y"		!Skip FIS prompt if FPP &
\	GOTO 1110			!Goto FIS prompt &

1109	PRINT &
\	PRINT	"Will  this  software  run  on a computer" &
\	PRINT	"with   the  floating   instruction   set" &
\	PRINT   "(YES or NO)?" &
!	Print the help &

1110	PRINT &
\	ON FNP%("Floating Instruction Set",AFIS$) &
		GOTO &
			1106,	1114,	1114,	1120,	1110,	1109 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1114	FIS$ = LEFT(W$,1%)		!"Y" or "N" &
\	GOTO 1120 &

1117	PRINT &
\	PRINT	"Floating point numbers  are  represented" &
\	PRINT	"internally  as  two 16-bit words, giving" &
\	PRINT	"seven significant digits. It is possible" &
\	PRINT	"to maintain 17 significant digits by us-" &
\	PRINT	"ing 4 words per number.  The  four  word" &
\	PRINT	"math  packages  also  include the scaled" &
\	PRINT	"arithmetic feature.  Would  this instal-" &
\	PRINT	"lation prefer to use 2 or 4 word math?" &
!	Print the help &

1120	PRINT &
\	ON FNP%("Math precision",MATH$) &
		GOTO &
			1106,	1120,	1120,	1125,	1123,	1117 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1123	MATH$ = W$			!Get the response &

1125	OLD.MTH$ = "" IF (MATH$ = "2") OR (MATH$ = "4") !Clear old math if true &
\	GOTO 1140 IF (MATH$ = "2") OR (MATH$ = "4") !All set, continue &
\	IF (LEFT(MATH$,1%) = "4") AND (RIGHT(MATH$,2%) = "D") THEN !Old math &
		OLD.MTH$ = "D"			!Yes, set it &
\		MATH$ = MATH.PKG$		!Set the default math package &
\		FPP$, FIS$ = "N"		!No FPP or FIS &
\		MATRIX$ = "N"			!No Matrices functions &
\		LOG.$, TRIG$ = "N"		!No log and trig functions &
\		GOTO 1160			!Skip the functions questions &

1127	PRINT "?Invalid math precision"	!Print error &
\	MATH$ = AMATH$			!Restore the default &
\	GOTO 1120			!and ask again &

1137	PRINT &
\	PRINT	"It is possible  to  save  space  in  the" &
\	PRINT	"BASIC-PLUS  system by omitting the loga-" &
\	PRINT	"rithmic functions SQR,EXP,LOG,and LOG10," &
\	PRINT	"if they are not needed.Does this instal-" &
\	PRINT	"lation need to compute  these  functions" &
\	PRINT	"(YES or NO)?" &
!	Print the help &

1140	PRINT &
\	ON FNP%("Log functions",LOG.$) &
		GOTO &
			1120,	1144,	1144,	1150,	1140,	1137 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1144	LOG.$ = LEFT(W$,1%)		!"Y" or "N" &
\	GOTO 1150 &

1147	PRINT &
\	PRINT	"It is possible  to  save  space  in  the" &
\	PRINT	"BASIC-PLUS system by omitting the trigo-" &
\	PRINT	"nometric functions SIN,COS,TAN, and ATN," &
\	PRINT	"if they are not needed.Does this instal-" &
\	PRINT	"lation need to compute  these  functions" &
\	PRINT	"(YES or NO)?" &
!	Print the help &

1150	PRINT &
\	ON FNP%("Trig functions",TRIG$) &
		GOTO &
			1140,	1154,	1154,	1160,	1150,	1147 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1154	TRIG$ = LEFT(W$,1%)		!"Y" or "N" &
\	GOTO 1160 &

1157	PRINT &
\	PRINT	"Special output formatting  can  be  done" &
\	PRINT	'using   the   "PRINT  USING"  statement.' &
\	PRINT	"Would this  installation  like  to  have" &
\	PRINT	"this optional feature (YES or NO)?" &
!	Print the help &

1160	PRINT &
\	ON FNP%("Print using",PRTUSE$) &
		GOTO &
			1162,	1164,	1164,	1165,	1160,	1157 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1162	GOTO 1120 IF OLD.MTH$ = "D"	!Skip over functions if 4D package &
\	GOTO 1150			!Go back to Trig question &

1164	PRTUSE$ = LEFT(W$,1%)		!"Y" or "N" &

1165	GOTO 1180 IF OLD.MTH$ = "D"	!Skip over Matrices if 4D package &
\	GOTO 1170 &

1167	PRINT &
\	PRINT	"BASIC-PLUS permits the user  to  operate" &
\	PRINT	"on  an entire matrix using just a single" &
\	PRINT	"statement.   These  statements  are  the" &
\	PRINT	'"MAT"  statements.  Would this installa-' &
\	PRINT	"tion like to have this optional  feature" &
\	PRINT	"(YES or NO)?" &
!	Print the help &

1170	PRINT &
\	ON FNP%("Matrices",MATRIX$) &
		GOTO &
			1160,	1174,	1174,	1180,	1170,	1167 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1174	MATRIX$ = LEFT(W$,1%)		!"Y" or "N" &
\	GOTO 1180 &

1177	PRINT &
\	PRINT	"An optional feature of BASIC-PLUS allows" &
\	PRINT	"arithmetic operations to be performed on" &
\	PRINT	"numbers  represented  by  strings.  This" &
\	PRINT	"feature  can be used  to obtain  greater" &
\	PRINT	"accuracy  in arithmetic  operations.  Do" &
\	PRINT	"you want string arithmetic (YES or NO)?" &
!	Print the help &

1180	PRINT &
\	ON FNP%("String arithmetic",STRMTH$) &
		GOTO &
			1182,	1184,	1184,	1190,	1180,	1177 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1182	GOTO 1120 IF OLD.MTH$ = "D"	!Skip over functions if 4D package &
\	GOTO 1170			!Go back to Trig question &

1184	STRMTH$ = LEFT(W$,1%)		!"Y" or "N" &
\	GOTO 1190 &

1187	PRINT &
\	PRINT	"An optional feature of BASIC extends  the" &
\	PRINT	"present immediate-mode debugging commands" &
\	PRINT	"by allowing you to trace the flow of your" &
\	PRINT	"program and to set breakpoints.  This  is" &
\	PRINT	"a supported feature of BASIC-PLUS. Do you" &
\	PRINT	"want the DEBUG facility (YES or NO)?" &
!	Print the help &

1190	PRINT &
\	ON FNP%("Debug",DEBUG$) &
		GOTO &
			1180,	1194,	1194,	1197,	1190,	1187 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1194	DEBUG$ = LEFT(W$,1%)		!"Y" or "N" &
\	GOTO 1197 &

1197	GOSUB 14500				!Ask 'Old S/R' prompt &
\	GOTO 1190 IF GO.BACK%			! Go back at user's request &

1200	GOSUB 13000			!Go output to BPLGEN.COM &
\	GOTO 1210 &

1207	PRINT &
\	PRINT	"If you wish to create another BASIC-PLUS" &
\	PRINT	"run-time system, please  answer YES.  If" &
\	PRINT	"you do not, please answer NO." &
!	Print the help &

1210	PRINT &
\	ON FNP%("Create another BASIC-PLUS RTS","N") &
		GOTO &
			1210,	1215,	1220,	1220,	1210,	1207 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

1215	GOSUB 14000				!Go initialize the defaults &
\	GOTO 1065 IF HEAD.ER%			!Back to template if right &
\	GOTO 1080				!Nope &

1220	PRINT #3%, "$_DEASSIGN IN:" &
\	PRINT #3%, "$_EXIT 1" &
\	OUT.CHN% = 3%			!Success, save the tentative file &
\	PRINT				!Give us some space &
\	GOTO 9000			!All done &

9000	! &
	! &
	! &
	!	P R O G R A M    E X I T S    H E R E &
	&

9100	  CLOSE 1%, 2%, OUT.CHN% &
		! CLOSE FILES &

9300	  GOTO 32767 &
		! GO TO THE END OF THE PROGRAM &
	&

10000	! &
	! &
	! &
	!	S U B R O U T I N E S &

10010	CHANGE SYS(CHR$(6%)+CHR$(-10%)+SYS.ACC$+"*.RTS") TO FIRQB% &
\	FILE.INDX% = 0%			!  Reset the file index &
\	HEAD.ER% = 0%			!  No header printed yet &
\	ALL.RTS$ = ""			!Init the var to store the Basic RTS's &
\	WHILE 1% = 1%			!  Loop forever &
\		ON ERROR GOTO 10120	!    Set local error trap &
\		FIRQB%(0%) = 30%	!    30 Bytes &
\		FIRQB%(1%) = 6%		!    SYS Call to FIP &
\		FIRQB%(2%) = 17%	!    Wildcard directory lookup &
\		FIRQB%(3%) = FILE.INDX%	!    Lower half of index &
\		FIRQB%(4%) = SWAP%(FILE.INDX%) !Upper half of index &
\		FIRQB%(I%) = 0% FOR I% = 13% TO 22% !Clear out &
\		CHANGE FIRQB% TO FIRQB$	!    Change array to a string &
\		CHANGE SYS(FIRQB$) TO FIRQ2%!Do the sys call &
\		N$ = "" &
\		N$ =N$+RAD$(FIRQ2%(I%)+SWAP%(FIRQ2%(I%+1%))) !Get &
			FOR I% = 7% TO 9% STEP 2%	  !Filename &
\		F$ =N$+"."+RAD$(FIRQ2%(11%)+SWAP%(FIRQ2%(12%))) &
					!      Add filetype &
\		D$ = MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(FIRQ2%(19%))+ &
			CHR$(FIRQ2%(20%))+CHR$(-1%)+CHR$(SWAP%(-1%))),7%,9%) &
\		I$ = SYS.ACC$ + F$	!Put back device and PPN &
\		GOSUB 11000		!See if it is a Basic RTS &
\		GOTO 10020 IF INV.BAS%	!Get next if it isn't &
\		IF HEAD.ER% = 0% THEN	!If we haven't printed a header yet &
			HEAD.ER% = -1% &
\			PRINT &
\		 PRINT "The following is a list of the BASIC RTS's in [0,1]:" &
\			PRINT &
\			PRINT CHR$(9%)+" File .Typ"+CHR$(9%)+"Creation" &

10015		PRINT CHR$(9%)+F$+CHR$(9%)+D$	!Print the filename and date &
\		N1$ = N$			!Save the filename &
\		ALL.RTS$ = ALL.RTS$ + CVT$$(N1$,2%) + "," ! and append it &

10020		FILE.INDX% = FILE.INDX% + 1% ! Increment the index &
\	NEXT				!    Next file &

10110	ON ERROR GOTO 19000		!Reset Standard Error Trap &
\	RETURN				! and go back &

10120	RESUME 10110 IF ERR = 5%	!Exit if no more files &
\	GOTO 19000			!Let main error trap handle it &

11000	ON ERROR GOTO 11200		!Set local error trap &
\	INV.BAS% = 0%			!Default to success &
\	BP.VAL%(I%) = -1% FOR I% = 1% TO 21% !Init array to no options &
\	OPEN I$ FOR INPUT AS FILE 2%, MODE 8192% !Open the RTS read-only &
\	FIELD #2%, 512% AS A$		!Field out the buffer &

11010	GET #2%				!Get the first block &
\	NAME.1$ = RAD$(SWAP%(CVT$%(MID(A$,3%,2%)))) !1st half of RTS name &
\	NAME.2$ = RAD$(SWAP%(CVT$%(MID(A$,5%,2%)))) !2nd half of RTS name &
!\	GOTO 11050 IF NAME.1$ + NAME.2$ <> "BASIC " Exit if not "BASIC " &
\	IDENT.1% = SWAP%(CVT$%(MID(A$,7%,2%)))	!Major ident &
\	IDENT.2% = SWAP%(CVT$%(MID(A$,9%,2%)))	!Minor ident &
\	BLK% = SWAP%(CVT$%(MID(A$,13%,2%))) + 1% !Calc the block w/symbols &
\	CNT% = SWAP%(CVT$%(MID(A$,15%,2%))) !Calculate the number of symbols &
\	RETURN IF GET.SIZE%		!Exit if we only wanted the size &
\	GOTO 11050 IF (BLK% < 50%) OR (BLK% > 82%) !Exit if .RTS doesn't fit &
\	PRE.V10% = (RAD$(IDENT.1%) < "10.")	!Is it pre-V10.0? &
\	NUM.SYM% = 0%			!Init the counter to zero &
\	FOR I% = 0% TO (CNT%/64%)	!Loop for each block with symbols &
\		GET #2%, BLOCK BLK%	! Get the block &
\		FOR J% = 1% TO 505% STEP 8% !For each symbol &
\			SYM1% = SWAP%(CVT$%(MID(A$,J%,2%))) !1st half of symbol &
\			SYM2% = SWAP%(CVT$%(MID(A$,J%+2%,2%))) !2nd half &
\			SYM$ = RAD$(SYM1%)+RAD$(SYM2%) !Convert to ASCII &
\			GOTO 11020 IF SYM$ = BP.SYM$(K%)!Is it one that &
				FOR K% = 1% TO 21%	! we want? &
\			GOTO 11030			! No, next symbol &

11020			VAL.UE% = SWAP%(CVT$%(MID(A$,J%+6%,2%))) !Get value &
\			BP.VAL%(K%) = VAL.UE%		! and store it &

11030			NUM.SYM% = NUM.SYM% + 1%	!Inc the counter &
\			GOTO 11040 IF NUM.SYM% = CNT%	!Exit if all done &
\		NEXT J%			! Next symbol &
\		BLK% = BLK% + 1%	! Next block &
\	NEXT I%				!Get and process the next block &

11040	GOTO 11050 IF BP.VAL%(I%) = -1%	!Was any verification &
		FOR I% = 17% to 20%	!  symbol not found? &
	&
\	GOTO 11060 IF BP.VAL%(I%) <> -1% !Were any required &
		FOR I% = 1% TO 4%	! symbols found? &

11050	INV.BAS% = -1%			!Invalid RTS, set flag &
\	GOTO 11190			! and exit &

11060	MATH.PKG$ = "2"			!Default to 2 Word EIS &
\	GOTO 11190 IF (BP.VAL%(1%) <> -1%) AND (BP.VAL%(2%) = 0%) !Exit if true &
\	MATH.PKG$ = "2I"		!2 Word FIS &
\	GOTO 11190 IF (BP.VAL%(1%) <> -1%) AND ((BP.VAL%(2%) < -1%) !True? &
					  OR   (BP.VAL%(2%) >  0%)) &
\	MATH.PKG$ = "2F"		!2 Word FPP &
\	GOTO 11190 IF (BP.VAL%(1%) = -1%)  AND ((BP.VAL%(2%) < -1%) !True? &
		  OR (BP.VAL%(2%) >  0%)) AND  (BP.VAL%(3%) = -1%) &
		 AND (BP.VAL%(4%) = -1%) &
\	MATH.PKG$ = "4"			!4 Word EIS &
\	GOTO 11190 IF (BP.VAL%(3%) <> -1%) AND (BP.VAL%(4%) <> -1%) !True? &
\	MATH.PKG$ = "4F"		!4 Word FPP &
\	GOTO 11190 IF (BP.VAL%(3%) = -1%)  AND (BP.VAL%(4%) <> -1%) !True? &
\	MATH.PKG$ = "4D"		!Old 4 Word decimal package &
\	GOTO 11190 IF (BP.VAL%(1%) = -1%) AND (BP.VAL%(2%) = 0%)    !True? &
		 AND (BP.VAL%(3%) = -1%) AND (BP.VAL%(4%) = -1%) &
\	GOTO 11050			!Invalid RTS, go set flag &

11190	ON ERROR GOTO 19000		!Reset standard error trap &
\	RETURN				! and go back &

11200	RESUME 11050 IF ERL = 11010	!Error getting symbols &
\	RESUME 11210 IF ERL = 11000	!Error opening the file &
\	GOTO 19000			!Let standard error trap handle it &

11210	PRINT "?Cannot open file - " + I$ !Print error message &
\	GOTO 11050			 ! and go set flag &

12000	GOSUB 11000			!Check it out &
\	Print "?Not a BASIC-PLUS run-time system" IF INV.BAS% &
\	RETURN IF INV.BAS%		!Return if it was bad &
\	AMATH$ = LEFT(MATH.PKG$,1%)	!Get the Math precision &
\	AFPP$ = MID("NY",ABS(RIGHT(MATH.PKG$,2%) = "F")+1%,1%) !Do we have FPP &
\	AFIS$ = MID("NY",ABS(RIGHT(MATH.PKG$,2%) = "I")+1%,1%) !Do we have FIS &
\	DEBUGB% = (BP.VAL%(5%) <> -1%)	!Set the &
\	OLD.SR% = (BP.VAL%(21%) <> -1%) &
\	PRTUSE% = (BP.VAL%(6%) <> -1%) &
\	STRMTH% = (BP.VAL%(7%) <> -1%)	!  options &
\	MATRIX% = (BP.VAL%(8%) <> -1%) &
\	FCTS.L% = (BP.VAL%(9%) <> -1%) &
\	FCTS.T% = (BP.VAL%(10%) <> -1%)	!     accordingly &
\	MATH$ = AMATH$ &
\	FPP$ = AFPP$ &
\	FIS$ = AFIS$ &
\	LOG.$ = "Y" &
\	LOG.$ = "N" UNLESS FCTS.L% &
\	TRIG$ = "Y" &
\	TRIG$ = "N" UNLESS FCTS.T% &
\	PRTUSE$ = "Y" &
\	PRTUSE$ = "N" UNLESS PRTUSE% &
\	MATRIX$ = "Y" &
\	MATRIX$ = "N" UNLESS MATRIX% &
\	STRMTH$ = "Y" &
\	STRMTH$ = "N" UNLESS STRMTH% &
\	DEBUG$ = "Y" &
\	DEBUG$ = "N" UNLESS DEBUGB% &
\	M18$ = "N" &
\	M18$ = "Y" IF (OLD.SR% <> 0%) AND (PRE.V10% = 0%) &
\	IF RIGHT(MATH.PKG$,2%) = "D" THEN	!Old decimal math package? &
		OLD.MTH$ = "D"			!Yes, set it &
\		MATH$ = MATH.PKG$		!Set the default math package &
\		FPP$, FIS$ = "N"		!No FPP or FIS &
\		MATRIX$ = "N"			!No Matrices functions &
\		LOG.$, TRIG$ = "N"		!No log and trig functions &
\		BP.VAL%(I%) = -1% FOR I% = 8% TO 10% !Clear those functions &

12010	RETURN &

13000	!	Output to BPLGEN.COM &
	&
	PRINT #3%, "$!" &
\	PRINT #3%, "$ _delete/nolog/nowarn BASIC.SAV,BASIC.STB" &
\	MATH$ = MATH$ + "F" IF FPP$ = "Y" &
\	MATH$ = MATH$ + "I" IF FIS$ = "Y" &
\	PRINT #3%, "$ _run $LINK.SAV" &
\	PRINT #3%, "BASIC/Z,,BASIC=IN:RTS,DK:$ERR.STB/X/H:#177776/U:#4000//" &
\	V$ = FNABSENT$(DEBUG$,"IN:LA") &
\	V$ = FNOPT$(DEBUG$,"IN:LAD") &
\	PRINT #3%, "IN:MA" + MATH$ &
\	PRINT #3%, "IN:XL" + MATH$ IF LOG.$ = "Y" &
\	PRINT #3%, "IN:XT" + MATH$ IF TRIG$ = "Y" &
\	V$ = FNOPT$(STRMTH$,"IN:SF") &
\	PRINT #3%, "IN:IO" &
\	V$ = FNOPT$(PRTUSE$,"IN:PU") &
\	V$ = FNOPT$(MATRIX$,"IN:MX") &
\	PRINT #3%, "IN:SN" &
!\	PRINT #3%, "IN:DI" &
\	V$ = FNOPT$(M18$,"IN:M18") &
\	V$ = FNOPT$(DEBUG$,"IN:DBG") &
\	PRINT #3%, "IN:VE" &
\	PRINT #3%, "//" &
\	PRINT #3%, "PA" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$ _run $SILUS.SAV" &
\	PRINT #3%, BASNAM$ + ".RTS,TT:=BASIC" &
\	PRINT #3%, "$!" &
\	PRINT #3%, "$ _delete/nolog BASIC.SAV,BASIC.STB" &
\	PRINT #3%, "$ _run $ONLPAT.SAV" &
\	PRINT #3%, "BASIC$$:BASIC.CMD" &
\	PRINT #3%, BASNAM$ + ".RTS" &
\	PRINT #3%, "$!" &
\	PRINT #3%, '$ BASNAM = "' + BASNAM$ + '"' &
\	PRINT #3%, "$ _gosub INSBAS" &
\	RETURN &

14000	AMATH$ = "2"			!Default to 2 word EIS &
\	FCTS.L% = -1%			!Default to Log functions &
\	FCTS.T% = -1%			!Default to Trig functions &
\	MATRIX% = 0%			!Default to no Matrix functions &
\	PRTUSE% = -1%			!Default to Print Using &
\	STRMTH% = 0%			!Default to no String arithmetic &
\	DEBUGB% = 0%			!Default to no Debug mode &
\	OLD.MTH$ = ""			!Default to no old decimal math pkg &
\	ABASIC$ = "BASIC"		!Default Basic RTS name to "BASIC" &
\	Change Sys(Chr$(6%)+Chr$(-29%)) to FIRQB%  !Monitor tables Part III &
\	Cnfg% = FIRQB%(21%) + Swap%(FIRQB%(22%))   !Get configuration word &
\	Fl.Pt% = ABS((Cnfg% and 512%) = 512%)	   !Get FPP or not &
\	FIS% = ABS((Cnfg% and 8%) = 8%)	   	   !Get FIS or not &
\	AFPP$ = "N"			!Default to no Floating Point Proc. &
\	AFPP$ = "Y" IF FL.PT%		!Set Default to "Yes" if true &
\	AFIS$ = "N"			!Default to no Floating Instr. Set &
\	AFIS$ = "Y" IF FIS%		!Set Default to "Yes" if true &
\	MATH$ = AMATH$			!Save the Math precision &
\	FPP$ = AFPP$			!Save the FPP value &
\	FIS$ = AFIS$			!Save the FIS value &
\	LOG.$ = "Y"			!Default to Log functions &
\	TRIG$ = "Y"			!Default to Trig functions &
\	PRTUSE$ = "Y"			!Default to Print Using &
\	MATRIX$ = "N"			!Default to no Matrix functions &
\	STRMTH$ = "N"			!Default to no String arithmetic &
\	DEBUG$ = "N"			!Default to no Debug mode &
\	M18$ = "N"			!Default to no old Message S/R &
\	RETURN &

14499	PRINT &
\	PRINT	"An optional feature of BASIC-PLUS  allows" &
\	PRINT	"you to use the old-style SEND/RECEIVE SYS" &
\	PRINT	"calls.  It is recommended that you change" &
\	PRINT	"your programs to use the new  SYS  calls," &
\	PRINT	"if possible.  Do you want  the  old-style" &
\	PRINT	"SEND/RECEIVE SYS calls (YES or NO)?" &
!	Print the help &

14500	PRINT &
\	GO.BACK% = 0% &
\	ON FNP%("Old-style SEND/RECEIVE",M18$) &
		GOTO &
			14505,	14510,	14510,	14520,	14500,	14499 &
	! ANSWER WAS	^	YES	NO	DFLT	TEXT	HELP &

14505	GO.BACK% = -1%			!Set 'User requested to go back' flag &
\	RETURN &

14510	M18$ = LEFT(W$,1%)		!"Y" or "N" &

14520	GO.BACK% = 0%			!Clear 'User requested to go back' flag &
\	RETURN &
	&

15000	! &
	!	F U N C T I O N S &
	! &
	DEF* FNOPT$(VAR.$,CMD$)		!Define function for the options &
\	PRINT #3%, CMD$ IF VAR.$ = "Y"	!Include the option if wanted &
\	FNOPT$ = ""			!Set the function to something &
\	FNEND &

15010   DEF* FNABSENT$(VAR.$,CMD$)      !Define function for options-absent &
\       PRINT #3%, CMD$ IF VAR.$ <> "Y"	!Include command if option absent &
\       FNABSENT$ = ""                  !Set the function to something &
\       FNEND &
	&

16200	! &
	! &
	! &
	!	D I A L O G U E    F U N C T I O N &
	&

16201!			FNP%( P$ , D$ ) &
     !	&
     !	FNP% IS USED TO PRINT A DIALOGUE PROMPT P$ AND THEN GET THE USERS &
     !	RESPONSE.  THE FUNCTION RETURNS ONE OF THE FOLLOWING VALUES: &
     !	&
     !		1	USER TYPED '^' (BACKUP) KEY &
     !		2	USER TYPED Y, OR Y<TEXT> RESPONSE &
     !		3	USER TYPED N OR N<TEXT> RESPONSE &
     !		4	USER TYPED DEFAULT RESPONSE(<CR> ONLY &
     !						OR  <LF> ONLY) &
     !		5	UNKNOWN RESPONSE(INVALID RESPONSE) &
     !		6	USER TYPED '?' (HELP) KEY &
     !	&
     !	VARIABLE W$ CONTAINS THE TEXT OF THE USER RESPONSE, WITH ANY &
     !	RECEIVED TERMINATOR(S) REMOVED. &
     !
16202!	THIS ROUTINE ASSUMES THAT THE USER'S KEYBOARD IS OPEN ON CHANNEL &
     !	1%. &
     !
16210	DEF* FNP%(P$,D$) &
		! THE FUNCTION DEFINITION &

16220	FNP% = 5% &
		! DEFAULT TO INVALID RESPONSE RESULT &

16230	GOTO 16250	IF LEN(P$) = 0% &
		! SKIP IF NO PROMPT TEXT &

16240	PRINT  IF CCPOS(0%) &
	\ PRINT P$; " ? "; TAB(32%);"<";D$;"> ";TAB(40%); &
	 	! PRINT PROMPT &

16250	INPUT LINE #1%, W$ &
	\ PRINT IF CCPOS(0%) &
		! GET USER RESPONSE &

16260	  W$ = CVT$$(W$,4%+8%+32%+128%) &
		! REMOVE TERMINATOR &
		! STRIP LEADING/TRAILING BLANKS AND TABS &
		! CONVERT L/C -> U/C &

16270		  IF LEN(W$) = 0% &
		  THEN &
			  FNP% = 4% &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS DEFAULT RESPONSE &
				! (<CR> ONLY OR <LF> ONLY) &

16280		  IF W$ = "^" &
		  THEN &
			  FNP% = 1% &
			\ W$ = "" &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS BACKUP (^) INDICATOR &

16285		  IF W$ = "?" &
		  THEN &
			  FNP% = 6% &
			\ W$ = "" &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS HELP (?) INDICATOR &

16290		  IF INSTR(1%,"YES",W$) = 1% &
		  THEN &
			  FNP% = 2% &
			\ GOTO 16310 &
				! CHECK FOR AND PROCESS YES RESPONSE &

16300		  IF INSTR(1%,"NO",W$) = 1% &
		  THEN &
			  FNP% = 3% &
				!CHECK FOR AND PROCESS NO RESPONSE &

16310	  FNEND &
		! END OF FUNCTION &

16400	DEF FNNO$(FLAG%) &
\	FNNO$ = ""				!Default to positive &
\	FNNO$ = "No " IF FLAG% = -1%		!Set to negative if true &
\	FNEND &
	&


19000	! &
	! &
	! &
	!	E R R O R    H A N D L I N G &
	&

19010	RESUME 9000 IF ERR = 11% AND ERL = 16250 !User typed CTRL/Z &
\	RESUME 1035 IF ERR = 16% AND ERL = 1030  !BPLGEN.COM already exists &
\	RESUME 1037 IF ERR = 5%  AND ERL = 1035  !BPLGEN.BAK does not exist &
\	GOTO 19900				 !Unexpected error &

19900	W$=SYS(CHR$(0%)) &
	\ PRINT &
	\ PRINT "?Program failure in BPLGEN" &
	\ PRINT CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERR)),3%),4%); &
	\ PRINT " at line"; ERL &
	\ CLOSE 1%, 2%, OUT.CHN% &
	\ PRINT &
	\ RESUME 32767 &
		! FOR ALL UNKNOWN ERRORS, PRINT ERROR MESSAGE GIVING ERR &
		! #, TEXT AND LINE WHERE IT OCCURRED.  CLOSE ANY OPEN FILES &
		! AND ABORT PROGRAM &

30000	! &
	!	C C L    E N T R Y &
	! &

30010	V$ = SYS(CHR$(7%))			!Get core common &
\	S% = INSTR(1%,V$," ")			!Find the separator &
\	GOTO 32767 UNLESS S%			!Exit if not there &
\	I$ = RIGHT(V$,S%+1%)			!Extract the RTS name &
\	I$ = CVT$$(I$,38%)			!Get rid of garbage &
\	GET.SIZE% = -1%				!Flag that we want the size &
\	GOSUB 11000				!Go look it up &
\	PRINT "%"+I$+" is larger than 16K"	!Say if it's too big &
		IF BLK% > 66% &
\	GOTO 32767 &

32766	! &
	! &
	! &
	!	E N D   O F   P R O G R A M &
	&

32767	END
