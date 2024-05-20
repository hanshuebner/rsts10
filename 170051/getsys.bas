2!		PROGRAM		: GETSYS.BAS
5!		VERSION		: V10.1
6!		EDIT		: G
7!		EDIT DATE	: 01-NOV-91
10	EXTEND		!USE BASIC-PLUS EXTEND MODE
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !		      Copyright (C) 1985, 1991 by &
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
	!	M o d i f i c a t i o n    H i s t o r y &
	&
	&
	! VER/ED	EDIT DATE	REASON &
	! V2.0-01	29-MAY-85	(PRL) Creation for Micro/RSTS V2.0 &
	! V2.0-02	22-JUN-85	(PRL) Add FL_PT and SWP_SIZ assigns &
	! V2.0-03	27-JUN-85	(PRL) Add single assignment support &
	! V2.0-04	28-JUN-85	(PRL) Add LP_ACCT assign &
	! V2.0-05	08-JUL-85	(PRL) Add DEV assign &
	! V2.0-06	11-JUL-85	(FRL) Remove prompt, add ANSI assign &
	! V2.0-07	22-JUL-85	(PRL) Add CIS assign &
	! V2.0-08	24-JUL-85	(PRL) Add DSK_LVL assign &
	! V2.0-09	05-Aug-85	(PRL) Add terminal inquires for ANSI &
	! V2.0-10	08-Aug-85	(PRL) Fix inquire for LA12 terminals &
	! V2.0-11	12-Aug-85	(PRL) Improve ANSI processor &
	! V2.0-12	13-Aug-85	(PRL) Override SET DATA for ANSI &
	! V2.0-13	16-Aug-85	(PRL) Improve SWP_SIZ computation &
	! V2.0-14	21-Aug-85	(PRL) Add JOB_MAX assignment &
	! V2.0-15	10-Sep-85	(FRL) Change ANSI to ANSI_SCOPE &
	! ================================================================= &
	! V2.1-01	20-May-86	(PRL) Add PKG_xxx assignment &
	! ================================================================= &
	! V9.7-06	08-Mar-89	(REG) Set term char if ANSI_SCOPE &
	! V9.7-07	27-Mar-89	(REG) Lookup installed .SIL name &
	! ================================================================= &
	! V10.0-01	08-Jun-89	(REG) Make temp file delete itself &
	! V10.0-01	08-Aug-89	(REG) Suppress errors during delete &
	! V10.0-I	12-Feb-90	(PRL) Add DEN_xxx assignment &
	! V10.0-G	21-Oct-91	(JJT) Rewind tape, then get density &

30	! &
	&
	&
	!	P r o g r a m   D e s c r i p t i o n &
	&
	&
	! GETSYS is used during installation of Micro/RSTS V2.0 to &
	! determine information about the current system and make it &
	! available to the DCL installation procedure.  It does so by &
	! computing and assigning these values to DCL global symbols. &
	&
	! GETSYS can be RUN or invoked via a CCL.  When run, GETSYS &
	! displays its header and assigns all the defined symbols &
	&
	! If you invoke GETSYS as a CCL, issue the command: &
	&
	!		$ CCL GETSYS [symbol-name] &
	&
	! You can specify a single symbol name or ALL. &
	! If you do not supply a symbol name, all are defined. &
	&
	! For single symbol names, GETSYS computes its value and assigns it &
	! by issuing the "execute CCL" sys call.  For ALL, GETSYS creates a &
	! temporary command file GETSxx.TMP (xx = 2-digit job number) and &
	! issues the "execute CCL" sys call to execute it.  The temporary &
	! file will be deleted upon logout. &
	&
	! The global symbols defined and their meaning are: &
	&
	!    MEM_SIZ	contains the system memory size, in K-words &
	&
	!    I_AND_D	contains 1 if the system supports I&D space, &
	!		or 0 if it does not &
	&
	!    FL_PT      contains 1 if the system contains a floating point &
	!		processor (FPP), or 0 if it does not &
	&
	!    CIS	contains 1 if the system supports the Commercial &
	!		Instruction Set (CIS), or 0 if it does not &
	&
	!    DSK_SIZ	contains the size of the system disk, in blocks &
	&
	!    JOB_MAX	maximum jobs for system, based on memory size and &
	!		processor type: &
	&
	!			 10 for F-11 and 128K memory, &
	!			 20 for J-11 or >128K memory &
	&
	!    SWP_SIZ	required size of SWAP1.SYS in blocks, based on the &
	!		system swap maximum, the computed JOB_MAX value (10 &
	!		or 20) and the current size of SWAP.SYS. Returns 0 &
	!		if SWAP1.SYS already exists &
	&
	!    DSK_FRE	contains free space on the system disk, in blocks &
	&
	!    DSK_LVL	contains as an integer the current revision level &
	!		of the system disk times 10; e.g. level 1.2 returned &
	!		as DSK_LVL = 12. &
	&
	!    LP_ACCT	contains next available [0,*] account for layered &
	!		product software (searches down from [0,199] to &
	!		[0,1].  Returns null string if none available. &
	&
	!    DEV	contains the input device name ("_ddn:") for the &
	!		INIT.SYS COPY option. &
	&
	!    ANSI_SCOPE	contains 1 if the terminal supports ANSI and SCOPE &
	!		mode, or 0 if it does not. &
	&
	!    PKG_xxx	contains the location of package xxx, based on &
	!		its assigment statement found in SYSINI.COM.  No &
	!		PKG_xxx symbol is returned if ALL is specified; &
	!		only a single (requested) PKG_xxx symbol is returned. &
	!		If the requested package is not defined in SYSINI, or &
	!		SYSINI.COM does not exist, then a null string is &
	!		assigned to the symbol. &
	&
	!    I_SIL	contains the name of the installed .SIL found in INIT &
	!		does not exist if an error was encountered in lookup &
	&
	!    MICRO_P	contains 1 if _SY:[0,1]MICRO.SIL is protected, or 0 &
	!		if the file is not protected.  No MICRO_P symbol is &
	!		returned if the file does not exist. &
	&
	!    DEN_xxx	contains the density (as an integer) of magtape xxx. &
	!		No DEN_xxx symbol is returned if ALL is specified; &
	!		only a single (requested) DEN_xxx symbol can be &
	!		returned.  If the requested device is not a magtape, &
	!		or cannot be opened, then the value 0 is returned. &

800	!	D a t a   S t a t e m e n t s &
	&
	&
	! ***** DO NOT alter the position of these data elements ***** &
	&
	&
	Data	"MEM_SIZ", &
		"DSK_SIZ", &
		"DSK_FRE", &
		"DSK_LVL", &
		"I_AND_D", &
		"FL_PT", &
		"CIS", &
		"JOB_MAX", &
		"SWP_SIZ", &
		"LP_ACCT", &
		"PKG_", &
		"DEV", &
		"ANSI_SCOPE", &
		"I_SIL", &
		"MICRO_P", &
		"DEN_", &
		"ALL", &
		"" &

900	!	D i m e n s i o n   S t a t e m e n t s &
	&
	&
	Dim Mon.I%(30), Mon.II%(30), Mon.III%(30) &
		! Define arrays for monitor tables parts I, II, and III &
\	Dim Firqb%(30) &
		! Define general FIRQB array for sys calls &
\	Dim Atr%(50) &
		! Define array to hold terminal chars and attributes &

1000	!	M a i n   P r o g r a m &
	&
	&
	On error goto 19000 &
\	Display%, Prompt% = -1% &
		! Enable standard error handler &
		! Enable header display & prompting &

1010	I$="V10.1-G" &
		! Set up version/edit numbers &

1020	If Display% then &
		Print "GETSYS "; I$; "   "; FnError$(0%) &
\		Print &
		! If display flag set, &
		!	Print the program header &
		!	Skip a line &

1030	Chr.6$ = Chr$(6%) &
\	Chr.0$ = Chr$(0%) &
\	Chr.M1$ = Chr$(-1%) &
		! Define useful strings for SYS calls &

1100	If Prompt% then &
		Print "Command <ALL>"; &
\		Input line Command$ &
\		Command$ = Cvt$$(Command$,-2%) &
		! If prompt mode set, &
		!	Display prompt for command &
		!	Get command response &
		!	Edit the string &
		!	Use ALL if null response &

1200	Command$ = "ALL" &
		Unless Len(Command$) &
\	Restore &
\	Read keyword$ &
\	For Keyword% = 1% while Len(Keyword$) &
\		Goto 1300 if Left(Command$,Len(Keyword$)) = Keyword$ &
\		Read Keyword$ &
\	Next Keyword% &
\	Print "?Command "; Command$; " is invalid" &
\	If Prompt% then &
		Goto 1100 &
	Else	Goto 32767 &
		! Use ALL if null command &
		! Restore data pointer &
		! Read 1st command keyword &
		! While more keywords: &
		!	Exit loop if keyword matches (generic) &
		!	Read next keyword &
		! Next &
		! Display error message &
		! If prompt mode, &
		!	re-prompt &
		! Else	exit &

1300	All% = (Keyword$ = "ALL") &
\	Job.No$ = NUM1$(ASCII(SYS(Chr.6$+CHR$(9%))) / 2%) &
\	Job.No$ = "0" + Job.No$ if LEN(Job.No$) = 1% &
\	Temp.File$ = "_SY:GETS" + Job.No$ + ".TMP" &
\	Start.Keyword%, End.Keyword% = Keyword% If not All% &
\	If not All% then &
		Kill Temp.File$ &
	Else	Start.Keyword% = 1% &
\		End.Keyword% = Keyword% - 1% &
\		Open Temp.File$ + "<60>" for output as file #1% &
		! Set flag for "ALL" &
		! Build 2-digit job number string &
		! Define temporary file-spec &
		! If single symbol selected, &
		!	Set loop boundaries to single keyword &
		!	Delete the previous temporary command file &
		! Else	Set loop boundaries for all keywords &
		!	Open temporary command file GETSnn.TMP &

1400	For Keyword% = Start.Keyword% to End.Keyword% &
\		On Keyword% Gosub	2000,	! Return MEM_SIZ &
					3000,	! Return  DSK_SIZ &
					4000,	! Return DSK_FRE &
					4500,	! Return DSK_LVL &
					5000,	! Return I_&_D &
					6000,	! Return FL_PT &
					6500,	! Return CIS &
					7000,	! Return JOB_MAX &
					7500,	! Return SWP_SIZ &
					8000,	! Return LP_ACCT &
					8500,	! Return PKG_xxx &
					9000,	! Return DEV &
					9500,	! Return ANSI_SCOPE &
					9800,	! Return I_SIL &
					9900,	! Return MICRO_P &
					5500	! Return DEN_xxx &
\		Goto 32767 if Ctrl.C% &
\		Print #1%, Symbol$ &
			If Len(Symbol$) &
				If All% &
\	Next Keyword% &
		! For each keyword to process: &
		!	Call processor for keyword &
		!	Exit if CTRL/C (from ANSI processor) &
		!	Write symbol assignment to com file &
		!		If any returned &
		!			If ALL &
		! Do for all keywords &
\	If All% then &
		Print #1%, "$ _Set noon" &
\		Print #1%, "$ _Set noecho/nowarn" &
\		Print #1%, "$ _Delete/nolog/nowarn " + Temp.File$ &
\		Print #1%, "$ _Set on" &
\		Print #1%, "$ _Set echo" &
\		Close #1% &
\		Symbol$ = "$ _@" + Temp.File$ &
		! If ALL, &
		!	Close the file &
		!	Build @ command to invoke it &

1600	Tmp$ = Sys(Chr$(14%) + Symbol$) &
		If Len(Symbol$) &
\	Goto 32767 &
		! Issue CCL sys call to do the symbol assignment &
		!	If anything there &
		! Exit &

2000	!	C o m p u t e   M e m o r y   S i z e &
	&
	&
	Gosub 10000 unless Mon.Tbl% &
		! Load monitor tables unless already loaded &
\	Mem.Siz = Peek(Mon.II%(25%) + Swap%(Mon.II%(26%))) &
\	Mem.Siz = 65536. + Mem.Siz if Mem.Siz < 0. &
		! Get unsigned memory size in slivers &
\	Mem.Siz% = Mem.Siz / 32. &
		! Convert size to K-words &
\	Symbol$ = "$ MEM_SIZ == " + Num1$(Mem.Siz%) &
		! Build MEM_SIZ symbol assignment &
\	Mem.Siz.Flg% = -1% &
		! Set flag to show MEM_SIZ computed &
\	Return &
		! Exit &

3000	!	C o m p u t e   S y s t e m   D i s k   S i z e &
	&
	&
	Gosub 11000 unless Dsk.Tbl% &
\	Dsk.Siz = FnDisk.Size(Pcs%,Max.Pcn%,Dcs%) &
\	Symbol$ = "$ DSK_SIZ == " + Num1$(Dsk.Siz) &
\	Dsk.Siz.Flg% = -1% &
\	Return &
		! Load disk tables unless already loaded &
		! Compute size of system disk in blocks &
		! Build DSK_SIZ symbol assignment &
		! Set flag to show DSK_SIZ computed &
		! Exit &

4000	!	C o m p u t e   S y s t e m   D i s k   F r e e &
	&
	&
	Gosub 11000 unless Dsk.Tbl% &
\	Dsk.Fre = Peek(Satctl%+Fun%) &
\	Dsk.Fre = Dsk.Fre + 65536. IF Dsk.Fre < 0. &
\	Dsk.Fre = Dsk.Fre + 65536. * Peek(Satctm%+Fun%) &
\	Symbol$ = "$ DSK_FRE == " + Num1$(Dsk.Fre) &
\	Dsk.Fre.Flg% = -1% &
\	Return &
		! Load disk tables unless already loaded &
		! Compute free space as unsigned value &
		! Build DSK_FRE symbol assignment &
		! Set flag to show DSK_FRE computed &
		! Exit &

4500	!	C o m p u t e   S y s t e m   D i s k   L e v e l &
	&
	&
	Change Sys(Chr.6$+Chr$(-25%)+Chr$(-4%)+String$(19%,0%) &
		+"SY"+Chr.0$+Chr.M1$) to Firqb% &
\	Dsk.Lvl% = 10% * Firqb%(10%) + Firqb%(9%) &
\	Symbol$ = "$ DSK_LVL == " + Num1$(Dsk.Lvl%) &
\	Dsk.Lvl.Flg% = -1% &
\	Return &
		! Get pack attributes of system disk &
		! Compute rev level times 10 &
		! Build DSK_LVL symbol assignment &
		! Set flag to DSK_LVL computed &
		! Exit &

5000	!	C o m p u t e   I _ A N D _ D   c o n f i g u r a t i o n &
	&
	&
	Gosub 10000 unless Mon.Tbl% &
\	Cnfg% = Mon.III%(21%) + Swap%(Mon.III%(22%)) &
\	I.and.D% = ABS((Cnfg% and 8192%) = 8192%) &
\	Symbol$ = "$ I_AND_D == " + Num1$(I.and.D%) &
\	I.and.D.Flg% = -1% &
\	Return &
		! Load monitor tables unless already loaded &
		! Get system configuration flag word &
		! Assign I_AND_D symbol to 0 or 1 &
		! Build I_AND_D symbol assignment &
		! Set flag to show I_AND_D computed &
		! Exit &

5500	!	R e t u r n   M a g t a p e   D e n s i t y &
	&
	&
	Symbol$ = "" &
\	Return if All% &
		! Init to show no symbol assignment &
		! Return if ALL specified &
	&
\	On error goto 5550 &
		! Trap own errors &
	&
\	Dev$ = Cvt$$(Right(Command$,Len(Keyword$)+1%),-2%) &
\	Return unless Len(Dev$) &
		! Extract device name from DEN_xxx command &
		! Exit if no device specified &
	&
\	Open "_" + Dev$ + ":" for input as file 2% &
\	Junk% = Magtape(3%,0%,2%) &
\	Junk% = Magtape(9%,0%,2%) &
\	Get #2% &
		! Open tape device (non-file-structured) for input &
		! Rewind the tape now &
		! Rewind the tape on close &
		! Read a record (to establish tape's current density) &

5510	Density% = Magtape(12%,0%,2%) &
		! Get the tape's density &

5520	Close #2% &
\	Symbol$ = "$ DEN_" + Dev$ + " == " + Num1$(Density%) &
\	Den.xxx.Flg% = -1% &
\	On error goto 19000 &
\	Return &
		! Close the device &
		! Return density value assignment &
		! Set flag to show density assigned &
		! Restore standard error trap &
		! Exit &

5550	Resume 5510 if Err = 40% if Erl = 5500% &
\	Density% = 0% &
\	Resume 5520 &
		! Error getting magtape density: &
		! Resume to get density if record length error reading record &
		! Return density value 0 &
		! Resume to exit &

6000	!	C o m p u t e   F L _ P T   c o n f i g u r a t i o n &
	&
	&
	Gosub 10000 unless Mon.Tbl% &
\	Cnfg% = Mon.III%(21%) + Swap%(Mon.III%(22%)) &
\	Fl.Pt% = ABS((Cnfg% and 512%) = 512%) &
\	Symbol$ = "$ FL_PT == " + Num1$(Fl.Pt%) &
\	Fl.Pt.Flg% = -1% &
\	Return &
		! Load monitor tables unless already loaded &
		! Get system configuration flag word &
		! Assign FL_PT symbol to 0 or 1 &
		! Build FL_PT symbol assignment &
		! Set flag to show FL_PT computed &
		! Exit &

6500	!	C o m p u t e   C I S   c o n f i g u r a t i o n &
	&
	&
	Gosub 10000 unless Mon.Tbl% &
\	Cnfg% = Mon.III%(21%) + Swap%(Mon.III%(22%)) &
\	CIS% = ABS((Cnfg% and 1024%) = 1024%) &
\	Symbol$ = "$ CIS == " + Num1$(CIS%) &
\	Cis.Flg% = -1% &
\	Return &
		! Load monitor tables unless already loaded &
		! Get system configuration flag word &
		! Assign CIS symbol to 0 or 1 &
		! Build CIS symbol assignment &
		! Set flag to show CIS computed &
		! Exit &

7000	!	C o m p u t e   J O B _ M A X &
	&
	&
	Gosub 2000 unless Mem.Siz.Flg% &
\	Gosub 3000 unless I.and.D.Flg% &
\	If I.and.D% or (Mem.Siz% > 128%) then &
		Job.Max% = 20% &
	Else	Job.Max% = 10% &
		! Compute memory size unless already computed &
		! Compute I & D configuration unless already computed &
		! If I & D machine or > 128K memory, &
		!	Use job max 20 &
		! Else	Use job max 10 &

7050	Symbol$ = "$ JOB_MAX == " + Num1$(Job.Max%) &
\	Job.Max.Flg% = -1% &
\	Return &
		! Build JOB_MAX symbol assignment &
		! Set flag to show JOB_MAX computed &
		! Exit &

7500	!	C o m p u t e   S W A P 1 . S Y S   F i l e   S i z e &
	&
	&
	If FnLookup%("_SY0:[0,1]SWAP1.SYS") then &
		Swp.Siz% = 0% &
\		Goto 7550 &
		! If SWAP1.SYS already exists, &
		!	Return size 0 &
		!	Skip to exit &

7510	Swap.File.Size% = FnFile.Size%("_SY0:[0,1]SWAP.SYS") &
\	If Swap.File.Size% < 0% then &
		Swp.Siz% = 0% &
\		Goto 7550 &
		! Get size of SWAP.SYS &
		! If too big (>32767 blocks), &
		!	Return size 0 &
		!	Skip to exit &

7520	Gosub 7000 unless Job.Max.Flg% &
\	Null.Rts% = Mon.III%(27%) + Swap%(Mon.III%(28%)) &
\	Swap.Max% = Peek(Null.Rts%+28%) and 255% &
\	Swap.Slots% = Swap.File.Size% / (Swap.Max% * 4%) &
\	Swp.Siz% = (Job.Max% - Swap.Slots%) * Swap.Max% * 4% &
\	Swp.Siz% = 0% If Swp.Siz% < 0% &
		! Compute job max (10 or 20) unless already computed &
		! Get swap maximum (from null RTS) &
		! Compute job slots in SWAP.SYS &
		! Get current swap max &
		! Compute job slots covered in SWAP.SYS &
		! Compute swap space (in blocks) needed in SWAP1.SYS &
		! Force to zero if negative &

7550	Symbol$ = "$ SWP_SIZ == " + Num1$(Swp.Siz%) &
\	Swp.Siz.Flg% = -1% &
\	Return &
		! Build SWP_SIZ symbol assignment &
		! Set flag to show SWP_SIZ computed &
		! Exit &

8000	!	C o m p u t e   L P _ A C C T   A c c o u n t &
	&
	&
	On error goto 8100 &
\	For Prog% = 199% to 1% step -1% &
\		Z$ = Sys(Chr.6$+Chr$(14%)+String$(4%,0%)+ &
				Chr$(Prog%)+Chr.0$+Chr$(1%)) &
\	Next Prog% &
\	Lp.Acct$ = "" &
\	Goto 8200 &
		! Trap own errors &
		! For each account [0,199] -> [0,1]: &
		!	Read account info &
		! Next account &
		! Show all accounts in use &
		! Skip over error handler &

8100	Goto 19000 unless Err = 5% &
\	Lp.Acct$ = "[0," + Num1$(Prog%) + "]" &
\	Resume 8200 &
		! Skip to standard trap if not ?Can't find error &
		! Build PPN string for account &
		! Resume to assign &

8200	Symbol$ = "$ LP_ACCT == " + '"' + Lp.Acct$ + '"' &
\	Lp.Acct.Flg% = -1% &
\	On error goto 19000 &
\	Return &
		! Build LP_ACCT symbol assignment &
		! Set flag to show LP_ACCT computed &
		! Restore standard error trap &
		! Exit &

8500	!	G e t   P K G _ x x x   a s s i g n m e n t &
	&
	&
	Symbol$ = "" &
\	Return if All% &
		! Init to show no symbol assignment &
		! Return if ALL specified &
	&
\	On error goto 8550 &
		! Trap own errors &
	&
\	Tb$ = Chr$(9%) &
\	Cr.Lf$ = Chr$(13%) + Chr$(10%) &
		! Define <tab> character &
		! Define <cr><lf> characters &
	&
\	Start.Lp$ = "! *** BEGIN SYSTEM LOGICALS ***" + Cr.Lf$ &
\	End.Lp$   = "! *** END SYSTEM LOGICALS ***" + Cr.Lf$ &
		! Define start record for LP assignments &
		! Define end record for LP assignments &
	&
\	Sysini.File$ = "_SY0:[0,1]SYSINI.COM" &
		! Define SYSINI command file spec &
	&
\	Pkg$ = Cvt$$(Right(Command$,Len(Keyword$)+1%),-2%) &
		! Extract package name from PKG_ prefix &

8520	Open Sysini.File$ for input as file 2%, mode 4096% &
\	Rec$ = "" &
\	Input line #2%, Rec$ &
		Until Rec$ = Start.Lp$ &
		! Open SYSINI.COM file &
		! Show no record read yet &
		! Read input records &
		! 	Until start of LP assignment section &

8530	Input line #2%, Rec$ &
\	If Rec$ <> End.Lp$ then &
		Rec$ = Cvt$$(Rec$,4%+8%+32%+128%) &
\		Z2% = Instr(1%,Rec$,Tb$) &
\		Goto 8530 unless Z2% &
\		Pkg.Nam$ = Right(Rec$,Z2%+1%) &
\		Goto 8530 unless Len(Pkg.Nam$) &
\		Goto 8530 unless Pkg$ = Pkg.Nam$ &
\		Z1% = Instr(1%,Rec$," ") &
\		Goto 8530 unless Z1% &
\		Pkg.Loc$ = Mid(Rec$,Z1%+1%,Z2%-Z1%-1%) &
\		Goto 8530 unless Len(Pkg.Loc$) &
\		Symbol$ = Symbol$ + Cr.Lf$ &
			If Len(Symbol$) &
\		Symbol$ = Symbol$ + "$ PKG_" + Pkg.Nam$ + &
					' == "' + Pkg.Loc$ + '"' &
		! Read next input record &
		! If not end of LP section, &
		!	Find <tab> in record &
		!	Skip if none found &
		!	Get package name following <tab> &
		!	Skip if null &
		!	Skip if name doesn't match requested name &
		!	Find <blank> in record &
		!	Skip if none found &
		!	Get package location between <blank> and <tab> &
		!	Skip if null &
		!	Append <cr><lf> to command line &
		!		unless null &
		!	Append symbol assignment $ PKG_xxxx = "pkg-loc" &

8540	Close #2% &
\	Symbol$ = "$ PKG_" + Pkg$ + ' == ""' &
		Unless Len(Symbol$) &
\	Pkg.xxx.Flg% = -1% &
\	On error goto 19000 &
\	Return &
		! Close SYSINI.COM file &
		! Return null symbol assignment &
		!	If no symbol assignment was built &
		! Set flag to show PKG_xxx symbol assigned &
		! Restore standard error trap &
		! Exit &

8550	If Err = 11% or Erl = 8520% then &
		Resume 8540 &
	Else	Goto 19000 &
		! If EOF error or error opening SYSINI.COM file, &
		!	Resume to return null assignment &
		! Else	Let standard error trap handle it &

9000	!	G e t   d i s t r i b u t i o n   d e v i c e &
	&
	&
	On error goto 9010 &
\	Change Sys(Chr.6$+Chr$(-10%)+"_SY0:[0,1]INIT.SYS") to Firqb% &
\	Firqb%(1%) = 6% &
\	Firqb%(2%) = -26% &
\	Firqb%(3%) = 0% &
\	Firqb%(4%) = 0% &
\	Firqb%(Z%) = 0% for Z% = 13% to 22% &
\	Firqb%(0%) = 26% &
\	Change Firqb% to Z$ &
\	Change Sys(Z$) to Firqb% &
	&
\	Dev$ = "_" + Cvt$$(Rad$(Firqb%(27%)+Swap%(Firqb%(28%))) + &
			Rad$(Firqb%(29%)+Swap%(Firqb%(30%))),-2%) + ":" &
\	Change Sys(Chr.6$+Chr$(-10%)+Dev$) to Firqb% &
\	Flag.2% = Firqb%(29%) + Swap%(Firqb%(30%)) &
\	Dev.Idx% = Status and 255% &
\	Dev$ = "" if (Flag.2% < 0%) or &
			((Dev.Idx% <> 0%) and (Dev.Idx% <> 14%)) &
\	Goto 9050 &
		! Trap own errors &
		! FSS the INIT.SYS file-spec &
		! Convert to UU.FIL sys call array &
		! Issue UU.FIL sys call &
	&
		! Get device name from INIT's RTS words ("_ddn:") &
		! FSS the device name &
		! Save flag word 2 &
		! Save device handler index &
		! Return null if untranslated logical &
		!			or not a disk or tape &
		! Skip to exit &

9010	Dev$ = "" &
\	Resume 9050 &
		! Return null device name for all errors &
		! Resume to exit &

9050	Symbol$ = "$ DEV == " + '"' + Dev$ + '"' &
\	Dev.Flg% = -1% &
\	On error goto 19000 &
\	Return &
		! Build DEV symbol assignment &
		! Set flag to show DEV computed &
		! Restore standard error handler &
		! Exit &

9500	!	C h e c k   I f   S C O P E + A N S I   T e r m i n a l &
	&
	&
	Esc$ = Chr$(27%) &
\	Cr$ = Chr$(13%) &
\	Ctrl.C$ = Chr$(3%) &
\	Ctrl.X$ = Chr$(11%) + Chr$(2%) &
\	Ansi% = 0% &
		! Define useful string constants &
		! Init to non-ANSI+VIDEO terminal &

9510	Req.Seq$ = Esc$ + "Z" &
\	Gosub 9600 &
\	Goto 9550 if Ctrl.C% &
	&
\	If Len(Reply$) = 0% then &
		Req.Seq$ = Esc$ + "[c" &
\		Gosub 9600 &
\		Goto 9550 unless Len(Reply$) &
		! Cancel typeahead &
		! Send DECID request &
		! Get a response &
		! Exit if CTRL/C &
	&
		! If no response with <esc>Z &
		!	Try it with the ANSI ID sequence &
		!	Get the response &
		!	Give up if still no response &

9520	If Reply$ = "/Z" then &
		Req.Seq$ = Esc$ + "<" + Esc$ + "[c" &
\		Gosub 9600 &
		! If VT100 or VT200 in VT52 mode, &
		!	Kick it into ANSI mode and inquire again &

9530	Goto 9550 &
		If Left(Reply$,2%) <> "[?" &
			Or Right(Reply$,Len(Reply$)) <> "c" &
	&
\	Atr%(I%) = 0% &
		For I% = 0% to 20% &
\	Reply$ = Mid(Reply$,3%,Len(Reply$)-3%) + ";" &
\	Idx% = 0% &
\	Semi% = 1% &
	&
\	Z% = Instr(Semi%,Reply$,";") &
\	On error goto 9560 &
\	While Z% > 0% &
\		Atr%(Idx%) = Val(Mid(Reply$,Semi%,Z%-Semi%)) &
\		Idx% = Idx% + 1% &
\		Semi% = Z% + 1% &
\		Z% = Instr(Semi%,Reply$,";") &
\	Next &
	&
\	Z% = Atr%(0%) &
\	Ansi% = ABS(				!Set ANSI flag (0/1) if: &
		   Z% = 1% or			! VT100 or VT101 or &
		   Z% = 4% or			! VT132 or &
		   Z% = 5% or			! VK100 (GIGI) or VT100J or &
		   Z% = 6% or			! VT102 or &
		   Z% = 7% or			! VT131 or &
		   Z% = 8% or			! VT278 or &
		   Z% = 12% or			! VT125 or &
		   (Z% >=61% and Z% <=69%))	! or VT2xx class terminal &
		! Give up &
		!	If response doesn't begin with "[?" &
		!		Or end with "c" &
	&
		! Clear the parameter array &
		! Get the parameter portion and append ";" delimiter &
		! Init the parameter counter &
		! Init the ";" position counter &
	&
		! Get 1st ";" in string &
		! Trap local errors (to catch VAL errors) &
		! Do until no more ;'s &
		!	Store value between ;'s in next Atr% cell &
		!	Increment Atr% table index &
		!	Get next ; in string &
		! Next &
	&
		! Save 1st atrribute (terminal type) &
		! Set ANSI (0/1) based on terminal type/parameters &

9550	Symbol$ = "$ ANSI_SCOPE == " + Num1$(Ansi%) &
\	Ansi.Flg% = -1% &
\	Junk$ = sys(chr$(6%)+chr$(16%)	! Set terminal characteristics &
			+chr$(0%)	! Part 1 of sys call &
			+chr$(255%)	! Current keyboard &
			+string$(3%,0%)	! Width, tabs, ff vt &
			+chr$(128%)	! u/l case out &
			+chr$(255%)	! Terminal XON &
			+chr$(128%)	! No local echo &
			+chr$(255%)	! Scope &
			+string$(5%,0%)	! u/l in, fill, speed, parity, speed &
			+chr$(255%)	! Host XON &
		      +string$(11%,0%)) ! ^ctrl, n/a, data, perm, esc, delim, &
					!   alt, ^r ^t, resume, break, b'cast &
		if Ansi% = 1% &
\	Junk$ = sys(chr$(6%)+chr$(16%)	! Set terminal characteristics &
			+chr$(1%)	! Part 2 of sys call &
			+chr$(255%)	! Current keyboard &
			+string$(16%,0%)! terminal type + reserved &
			+string$(4%,0%)	! perm, quota, enable and disable ctrl &
			+chr$(1%)	! ANSI &
		       +string$(5%,0%)) ! set and clear attr and reserved &
		if Ansi% = 1% &
\	On error goto 19000 &
\	Return &
		! Return ANSI_SCOPE symbol assignment &
		! Set flag to show ANSI computed &
		! Set term serv appropriately if term is ANSI_SCOPE &
		! Restore standard error trap &
		! Exit &

9560	Resume 9550 &
		! Resume to exit on any error &

9600	!	G e t   r e q u e s t   e s c   s e q   f r o m   K B &
	&
	&
	On error goto 9680 &
\	Open "_KB:" for input as file 2%, mode 1% + 16384% &
\	Z$ = Sys(Ctrl.X$) &
	&
\	Ctrl.C% = 0% &
\	Reply$ = "" &
\	State% = 1% &
	&
\	Print #2%, record 16%, Req.Seq$; Cr$; &
\	While State% > 0% &
		! Show no CTRL/C detected &
		! Init null esc sequence &
		! Init current state &
		! Send request sequence with trailing <cr> &
		! Do while current state is positive: &

9610		Wait 2% &
\		Get #2%, record 256% &
\		Field #2%, recount as Z$ &
\		Change Z$ to Atr% &
		!	Set max secs to wait for KB input &
		!	Get KB input if any (override SET DATA) &
		!	Field input buffer &
		!	Save char values in array &

9630		For Idx% = 1% while (Idx% <= Atr%(0%)) and (State% > 0%) &
\			Char$ = Chr$(Atr%(Idx%) and 127%) &
\			If Char$ = Ctrl.C$ then &
				Print #2%, Char$ &
\				Ctrl.C% = -1% &
\				Return &
		!	For each char received (while state > 0): &
		!		Get next char with 8th bit stripped &
		!		If CTRL/C char, &
		!			Display it &
		!			Set CTRL/C flag &
		!			Exit &

9650			On State% Gosub &
				9700, 	! Get start of esc seq &
				9720,	! Get char after <esc> &
				9760,	! Get char following <esc>/ &
				9780	! Get chars following <esc>[ &
\		Next Idx% &
\	Next &
		!		Call state processor &
		!	Next input char or state = 0 &
		! Repeat until state = 0 &

9670	Z$ = Sys(Ctrl.X$) &
\	Close 2% &
\	On error goto 19000 &
\	Return &
		! Cancel any remaining typeahead &
		! Close KB channel &
		! Restore standard error trap &
		! Exit &

9680	If Err = 15% then &
		Reply$ = "" &
\		Resume 9670 &
		! If KB wait exhausted, &
		!	Null any chars received &
		!	Resume to exit &

9690	Goto 19000 &
		! Skip to standard error handler &

9700	!	P r o c e s s   K B   e s c a p e   s e q   s t a t e s &
	&
	&
	! State 1 - wait for start of escape sequence &
	&
	&
	If Char$ = Esc$ then &
		State% = 2% &
\		Return &
		! If <esc> char, &
		!	Set next state &
		!	Exit &

9710	Return &
		! Ignore anything else &
		! Leave state unchanged &
		! Exit &

9720	! State 2 - Process char following <esc> &
	&
	&
	If Char$ = "/" then &
		Reply$ = Char$ &
\		State% = 3% &
\		Return &
		! If next char is /, &
		!	Save it in esc seq string &
		!	Set next state &
		!	Exit &

9730	If Char$ = "[" then &
		Reply$ = Char$ &
\		State% = 4% &
\		Return &
		! If next char is [, &
		!	Save it in esc seq string &
		!	Set next state &
		!	Exit &

9740	If Char$ = "?" then &
		Reply$ = "[" + Char$ &
\		State% = 4% &
\		Return &
		! If next char is ?, &
		!	Assume [? as start of seq &
		!	Set next state &
		!	Exit &

9750	Reply$ = "" &
\	State% = 0% &
\	Return &
		! Anything else is unexpected &
		! Null any chars received so far &
		! Set state for completion &
		! Exit &

9760	! State 3 - Get alphabetic char following <esc>/ &
	&
	&
	If Char$ >= "A" and Char$ <= "Z" then &
		Reply$ = Reply$ + Char$ &
\		State% = 0% &
\		Return &
		! If A-Z character, &
		!	Append char to esc seq string &
		!	Set state for completion &
		!	Exit &

9770	Reply$ = "" &
\	State% = 0% &
\	Return &
		! Anything else is unexpected &
		! Null chars received so far &
		! Set state for completed &
		! Exit &

9780	! State 4 - Get next char in ANSI escape sequence &
	&
	&
	Reply$ = Reply$ + Char$ &
\	State% = 0% &
		If Char$ = "c" &
\	Return &
		! Append character to esc seq string &
		! Change state for completion &
		!	If escape terminator (c), &
		! Exit &

9800	! &
	!	L o o k u p   i n s t a l l e d   m o n i t o r    n a m e &
	! &
	On error goto 9820 &
\	Symbol$ = "" &
\	Open "_SY0:[0,1]INIT.SYS" for input as file 3% &
\	Field #3%, 58% as JUNK$, 2% as SILPTR$ &
\	Get #3%, block 1% &
\	SILPTR% = swap%(cvt$%(SILPTR$)) &
\	SILBLK% = SILPTR%/512% + 1% &
\	SILOFF% = SILPTR% - ((SILBLK%-1%)*512%) &
\	Get #3%, block SILBLK% &
\	Field #3%, SILOFF% as JUNK$, 2% as SILNAME1$, 2% as SILNAME2$ &
\	SILNAME1% = swap%(cvt$%(SILNAME1$)) &
\	SILNAME2% = swap%(cvt$%(SILNAME2$)) &
\	I.SIL$ = rad$(SILNAME1%)+rad$(SILNAME2%) &
\	I.SIL$ = cvt$$(I.SIL$,-1%) &
\	Symbol$ = "$ I_SIL == " + '"' + I.SIL$ + '"' &
		! The installed .SIL name is in INIT &
		! INIT's first block contains a pointer to the sil name &

9810	Close 3% &
\	On error goto 19000 &
\	Return &

9820	Resume 9810 &

9900	! &
	!	L o o k u p   m i c r o . s i l   "P"   b i t &
	! &
	On error goto 9920 &
\	Symbol$ = "" &
\	F.S.S$ = Sys(Chr.6$+Chr$(-10%)+"_SY:[0,1]MICRO.SIL") &
\	LKN$ = Sys(Chr.6$+Chr$(17%)+Chr$(0%)+Chr$(0%) &
		+Mid(F.S.S$,5%,8%)+String$(10%,0%) &
		+Mid(F.S.S$,23%,4%)+String$(4%,0%)) &
\	MICRO.P$ = "0" &
\	MICRO.P$ = "1" if Ascii(Mid(LKN$,30%,1%)) AND 32% &
\	Symbol$ = "$ MICRO_P == " + '"' + MICRO.P$ + '"' &
		! FSS the file name &
		! Do a wildcard directory lookup with file name &
		! Byte 30, bit 5 is the "protected" bit &

9910	On error goto 19000 &
\	Return &

9920	Resume 9910 &
	&

10000	!	L o a d   M o n i t o r   T a b l e s &
	&
	&
	Change Sys(Chr.6$+Chr$(-3%)) to Mon.I% &
\	Change Sys(Chr.6$+Chr$(-12%)) to Mon.II% &
\	Change Sys(Chr.6$+Chr$(-29%)) to Mon.III% &
\	Mon.tbl% = -1% &
\	Return &
		! Save monitor tables part I &
		! Save monitor tables part II &
		! Save monitor tables part III &
		! Set monitor tables loaded flag &
		! Exit &

11000	!	L o a d   D i s k   T a b l e s &
	&
	&
	Gosub 10000 unless Mon.Tbl% &
\	Devptr% = Mon.I%(7%)    + Swap%(Mon.I%(8%)) &
\	Untclu% = Mon.I%(17%)   + Swap%(Mon.I%(18%)) &
\	Untcnt% = Mon.I%(19%)   + Swap%(Mon.I%(20%)) &
\	Satctl% = Mon.I%(21%)   + Swap%(Mon.I%(22%)) &
\	Satctm% = Mon.I%(25%)   + Swap%(Mon.I%(26%)) &
\	Satend% = Mon.III%(7%)  + Swap%(Mon.III%(8%)) &
\	Devclu% = Mon.III%(25%) + Swap%(Mon.III%(26%)) &
\	Fun% = Peek(Devptr%-2%) - Untcnt% &
\	Dcs% = Peek(Devclu%+Fun%) AND 255% &
\	Pcs% = Peek(Untclu%+Fun%) AND 255% &
\	Max.Pcn% = Peek(Satend%+Fun%) &
\	Dsk.Tbl% = -1% &
\	Return &
		! Get monitor tables unless already loaded &
		! Save pointers to needed monitor I/O tables &
		! Compute FIP unit number * 2 for system disk &
		! Save system disk's device clustersize &
		! Save system disk's pack clustersize &
		! Save system disk's maximum PCN &
		! Set disk tables loaded flag &
		! Exit &

15000	!	F u n c t i o n   F n E r r o r $ &
	&
	! Return error message text &
	&
	&
	Def FnError$(Error%) = &
		Cvt$$(Right(Sys(Chr$(6%)+Chr$(9%)+Chr$(Error%)),3%),4%) &

15100	!	F u n c t i o n   F n D i s k . s i z e &
	&
	! Compute maximum usable size (in blocks) for a disk &
	&
	&
	Def FnDisk.Size(Pcs%,Max.Pcn%,Dcs%) &
	\ Dsize = Max.Pcn% &
	\ Dsize = Dsize + 65536. if Dsize < 0. &
	\ FnDisk.Size = ((Dsize + 1.) * Pcs%) + Dcs% &
	\ Fnend &
		! Compute fl-pt disk size in pcn's &
		! Return size in blocks &

15200	!	F u n c t i o n   F n L o o k u p % &
	&
	&
	Def FnLookup% (File.Spec$) &
\	On error goto 15210 &
\	Change Sys(Chr.6$+Chr$(-10%)+File.Spec$) to Firqb% &
\	Firqb%(1%) = 6% &
\	Firqb%(2%) = 17% &
\	Firqb%(3%),Firqb%(4%) = 255% &
\	Firqb%(Z%) = 0% &
		For Z% = 13% to 22% &
\	Firqb%(0%) = 26% &
\	Change Firqb% to Z$ &
\	Change Sys(Z$) to Firqb% &
\	FnLookup% = -1% &
\	Goto 15250 &
		! Trap own errors &
		! FSS file-spec &
		! Convert to lookup by file name &
		! Do lookup sys call &
		! (Save returned data in Firqb%) &
		! No error, so return success &
		! Skip to exit &

15210	FnLookup% = 0% &
\	Resume 15250 &
		! Treat any error as not found &
		! Resume to exit &

15250	Fnend &
		! End FnLookup% function &

15300	!	F u n c t i o n  F n F i l e . S i z e % &
	&
	&
	Def FnFile.Size% (File.Spec$) &
\	If FnLookup%(File.Spec$) = 0% then &
		FnFile.Size% = -1% &
\		Goto 15350 &
		! If file does not exist, &
		!	Return file size = -1 &
		!	Skip to exit &
		! (otherwise Firqb% contains file info) &

15310	If Firqb%(16%) = 0% then &
		FnFile.Size% = Firqb%(13%) + Swap%(Firqb%(14%)) &
	Else	FnFile.Size% = -1% &
		! If file size <= 65535, &
		!	Return actual file size &
		! Else	Return file size = -1 &

15350	Fnend &
		! End FnFile.Size% function &

19000	!	S t a n d a r d   E r r o r   H a n d l e r &
	&
	&
	If Erl = 1100% then &
		If Err = 11% then &
			Resume 32767 &
		! If error on prompt, &
		!	If CTRL/Z detected, &
		!		Resume to exit &

19100	If Erl = 1300% then &
		Resume 1400 if Err = 5% &
\		Print "?Unable to create temporary command file" &
\		Print FnError$(Err) &
\		Resume 32767 &
		! If error creating CONFIG.TMP, &
		!	Display error and resume to exit &

19999	Print if CCPOS(0%) &
\	Print "??Program failure in GETSYS" &
\	Print FnError$(Err); " at line"; Erl &
\	Resume 32767 &
		! Ensure at left margin &
		! Display fatal error message &
		! Resume to exit &

30000	!	C C L   E n t r y   P o i n t &
	&
	&
	On error goto 19000 &
\	Command$ = Sys(Chr$(7%)) &
\	Z% = Instr(1%,Command$," ") &
\	Z% = Len(Command$) + 1% unless Z% &
\	Command$ = Cvt$$(Right(Command$,Z%+1%),-2%) &
\	Prompt% = (Len(Command$) = 0%) &
\	Display% = 0% &
\	Goto 1010 &
		! Set standard error handler &
		! Read core common &
		! Locate 1st <space> &
		! Position at end if none &
		! Extract command from buffer &
		! Set prompt flag if null command &
		! Disable display header &
		! Merge with main code &

32767	End
