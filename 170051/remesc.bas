2!		PROGRAM		: REMESC.BAS
5!		VERSION		: V10.1
6!		EDIT		: A
7!		EDIT DATE	: 10-MAY-91
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
	! V2.0-01	05-Oct-85	(PRL) Creation for Micro/RSTS V2.0 &

30	! &
	&
	&
	!	P r o g r a m   D e s c r i p t i o n &
	&
	&
	! REMESC is used during installation of Micro/RSTS V2.0 to strip &
	! escape sequences from the installation log file, INSTAL.LOG. &
	&
	! This program can be defined as a system command (CCL).  Once &
	! defined, it can be invoked with the command, &
	&
	!		$ CCL REMESC logfile-spec &
	&
	! REMESC will edit the log file, stripping all valid escape sequences &
	! from it.  The input file-spec will be replaced; it will not be &
	! renamed as a .BAK file. &

1000	!	M a i n   P r o g r a m &
	&
	&
	On error goto 19000 &
		! Enable standard error handler &

1010	I$="V10.1-A" &
\	Display% = -1% &
\	Ccl% = 0% &
		! Set up version/edit numbers &
		! Init to display program header &
		! Clear CCL entry flag &

1020	If Display% then &
		Print "REMESC "; I$; "   "; FnError$(0%) &
\		Print &
		! If display mode set, &
		!	Print the program header &
		!	Skip a line &

1030	Esc$ = Chr$(27%) &
\	Buf.Siz% = 8192% &
\	In.Chnl% = 1% &
\	Out.Chnl% = 2% &
		! Define <esc> char &
		! Define buffer size for I/O channels &
		! Define input/output channels &

1100	If Len(In.Fil$) = 0% then &
		Print if Ccpos(0%) &
\		Print "File <INSTAL.LOG>"; &
\		Input line In.Fil$ &
\		In.Fil$ = Cvt$$(In.Fil$,-2%) &
\		In.Fil$ = "INSTAL.LOG" &
			Unless Len(In.Fil$) &
		! If no input file-spec defined, &
		!	Ensure left margin &
		!	Display prompt &
		!	Get response &
		!	Edit it &
		!	Set to default if null &

1110	In.Fil$ = Cvt$$(In.Fil$,-2%) &
\	Z% = Instr(1%,In.Fil$,".") &
\	If Z% = 0% then &
		Z% = Len(In.Fil$) + 1% &
\		In.Fil$ = In.Fil$ + ".LOG" &
		! Edit the file-spec &
		! Locate start of file type &
		! If none, &
		!	Point past end of file-spec &
		!	Append default type (.LOG) &

1200	Open In.Fil$ for input as file In.Chnl%, &
			Recordsize Buf.Siz%, &
			Mode 8192% &
		! Open input file in read-only mode &

1210	Open In.Fil$ for output as file Out.Chnl%, &
			Recordsize Buf.Siz%, &
			Mode 32% &
		! Open output file in tentative mode &

1220	Len.In.Buf%,Len.Out.Buf% = 0% &
\	Cur.Pos% = 1% &
\	Z% = FnGet.Buffer%(In.Chnl%) &
\	Goto 3000 if Z% &
		! Show no chars in either buffer &
		! Init pointer to current buffer position &
		! Call function to read 1st input buffer &
		! Skip to exit handler if EOF &

2000	Nxt.Pos% = Instr(Cur.Pos%,In.Buffer$,Esc$) &
\	If Nxt.Pos% = 0% then &
		Z% = FnFill.Buffer%(Right(In.Buffer$,Cur.Pos%)) &
\		Z% = FnGet.Buffer%(In.Chnl%) &
\		Goto 3000 if Z% &
\		Cur.Pos% = 1% &
\		Goto 2000 &
		! Locate next <esc> char in input buffer &
		! If none, &
		!	Copy remainder of input buffer to output buffer &
		!	Get next input buffer from file &
		!	Reset pointer to start of buffer &
		!	Repeat &

2100	Z% = FnFill.Buffer%(Mid(In.Buffer$,Cur.Pos%,Nxt.Pos%-Cur.Pos%)) &
\	Cur.Pos% = FnEnd.Esc%(Nxt.Pos%) + 1% &
\	Goto 2000 &
		! Copy chars up to <esc> into output buffer &
		! Find end of esc seq &
		! Skip to repeat &

3000	If Len.Out.Buf% then &
		Z% = 512% * ((Len.Out.Buf% + 511%) / 512%) &
\		Z1% = Z% - Len.Out.Buf% &
\		Field #Out.Chnl%,	Len.Out.Buf% as Z$, &
					Z1% as Z$ &
\		Lset Z$ = String$(Z1%,0%) &
\		Len.Out.Buf% = Z% &
\		Z% = FnPut.Buffer%(Out.Chnl%) &
		! If output buffer not empty, &
		!	Compute no. chars to write full blocks (Z%) &
		!	Compute no. nulls to fill buffer (Z1%) &
		!	Field area to hold trailing nulls &
		!	Load nulls into buffer &
		!	Set new buffer length &
		!	Call function to write buffer to file &

3100	Close In.Chnl% &
\	Close Out.Chnl% &
\	Goto 32767 &
		! Close input channel &
		! Close output channel &
		! Skip to exit &

15000	!	F u n c t i o n   F n E r r o r $ &
	&
	! Return error message text &
	&
	&
	Def FnError$(Error%) = &
		Cvt$$(Right(Sys(Chr$(6%)+Chr$(9%)+Chr$(Error%)),3%),4%) &

15100	!	F u n c t i o n   F n F i l l . B u f f e r % &
	&
	&
	Def* FnFill.Buffer% (Chars$) &
\	On Error goto 15130 &
		! Trap own errors &

15110	Z% = Len(Chars$) &
\	Field #Out.Chnl%,	Len.Out.Buf% as Z$, &
				Z% as Z$ &
\	Lset Z$ = Chars$ &
\	FnFill.Buffer%,Len.Out.Buf% = Len.Out.Buf% + Z% &
\	Goto 15150 &
		! Save no. chars to copy &
		! Field area for new chars to add (may overflow) &
		! Append chars to output buffer &
		! Calculate & return new output buffer length &
		! Skip to exit &

15120	Z% = Bufsiz(Out.Chnl%) - Len.Out.Buf% &
\	Field #Out.Chnl%,	Len.Out.Buf% as Z$, &
				Z% as Z$ &
\	Lset Z$ = Chars$ &
\	Chars$ = Right(Chars$,Z%+1%) &
\	Len.Out.Buf% = Len.Out.Buf% + Z% &
\	Z% = FnPut.Buffer%(Out.Chnl%) &
\	Goto 15110 &
		! Compute no. chars to fill buffer &
		! Field area to append chars &
		! Move chars to buffer &
		! Strip chars from string written to buffer &
		! Call function to write buffer to file &
		! Skip to copy remaining chars to buffer &

15130	If Err = 63% then &
		Resume 15120 &
	Else	Goto 19000 &
		! If ?Field overflows buffer error, &
		!	Resume to handle overflow &
		! Else	Skip to standard error handler &

15150	On error goto 19000 &
\	Fnend &
		! Restore standard error trap &
		! End FnFill.Buffer% function &

16000	!	F u n c t i o n   F n E n d . E s c % &
	&
	&
	Def* FnEnd.Esc% (Esc.Pos%) &
\	Esc.Seq% = 1% &
\	Offset% = Esc.Pos% &
		! Init esc seq state to show we've seen an <esc> char &
		! Init offset to begin at char following <esc> &

16010	If Offset% >= Len.In.Buf% then &
		Z%,FnEnd.Esc% = FnGet.Buffer%(In.Chnl%) &
\		Goto 16050 if Z% &
\		Offset% = 0% &
		! If no more chars to process in buffer, &
		!	Call function to read next buffer from file &
		!	Skip to exit if EOF or unexpected error &
		!	Reset offset &

16020	Field #In.chnl%,	Offset% as Z$, &
				1% as Char$ &
\	Char% = Ascii(Char$) &
\	On Esc.Seq% Gosub	16100, &
				16200, &
				16300, &
				16400 &
\	Offset% = Offset% + 1% &
		If Esc.Seq% >= 0% &
\	Goto 16010 &
		If Esc.Seq% > 0% &
\	FnEnd.Esc% = Offset% &
		! Field next char to examine &
		! Save ascii value of char &
		! Gosub based on current esc seq state &
		! Incr offset to next char in input buffer &
		!	If valid esc seq state &
		! Skip to continue processing &
		!	If esc state shows more chars needed &
		! Return pointer to next char &

16050	Fnend &
		! End FnEnd.Esc% function &

16100	! State 1: Get char following <esc> &
	&
	&
	If Char$ = "O" or Char$ = "P" or Char$ = "?" then &
		Esc.Seq% = 3% &
\		Return &
		! If <esc>O or or <esc>P or <esc>?, &
		!	Set new esc state &
		!	Exit &

16110	If Char$ = "[" then &
		Esc.Seq% = 2% &
\		Return &
		! If <esc>[, &
		!	Set new esc state &
		!	Exit &

16120	If Char% >= 32% and Char% <= 47% then &
		Esc.Seq% = 4% &
\		Return &
		! If <esc><filler>, &
		!	Set new esc state &
		!	Exit &

16130	Esc.Seq% = 0% &
\	Return &
		! Assume anything else is a terminator &
		! (Handles most VT52 esc seqs) &
		! Reset esc seq state &
		! Exit &

16200	! State 2: Process characters following <Esc>[ &
	&
	&
	If Char% >= 64% and Char% <= 128% then &
		Esc.Seq% = 0% &
\		Return &
		! If terminator char, &
		!	Reset esc state &
		!	Exit &

16210	If Char% >= 32% and Char% <= 63% then &
		Return &
		! If filler char, &
		!	Exit &

16220	Esc.Seq% = -1% &
\	Return &
		! Invalid character in esc seq &
		! Reset esc seq state &
		! Exit &

16300	! State 3: Process characters following <esc>O, <Esc>?, or <Esc>P &
	&
	&
	If Char% >= 32% and Char% <= 127% then &
		Esc.Seq% = 0% &
\		Return &
		! If modifier char, &
		!	Reset esc state &
		!	Exit &

16310	Esc.Seq% = -1% &
\	Return &
		! Invalid character in esc seq &
		! Reset esc seq state &
		! Exit &

16400	! State 4: Process characters in <Esc><fillers> sequence &
	&
	&
	If Char% >= 48% and Char% <= 126% then &
		Esc.Seq% = 0% &
\		Return &
		! If terminator char, &
		!	Reset esc state &
		!	Exit &

16410	If Char% >= 32% and Char% <= 47% then &
		Return &
		! If filler char, &
		!	Exit &

16420	Esc.Seq% = -1% &
\	Return &
		! Invalid character in esc seq &
		! Reset esc seq state &
		! Exit &

17000	!	F u n c t i o n   F n G e t . B u f f e r % &
	&
	&
	Def* FnGet.Buffer% (Chnl%) &
\	On error goto 17010 &
\	Get #Chnl% &
\	Len.In.Buf% = Recount &
\	Field #Chnl%, Len.In.Buf% as In.Buffer$ &
\	FnGet.Buffer% = 0% &
\	Goto 17050 &
		! Trap own errors &
		! Get next buffer's worth of data &
		! Save no. chars read &
		! Field the full buffer &
		! Show success &
		! Skip to exit &

17010	If Err = 11% then &
		FnGet.Buffer% = Err &
\		Len.In.Buf% = 0% &
\		Resume 17050 &
		! If EOF error, &
		!	Return error code &
		!	Show no chars read &
		!	Resume to exit &

17020	Goto 19000 &
		! Otherwise treat as an unexpected error &
		! Skip to standard error trap &

17050	On error goto 19000 &
\	Fnend &
		! Restore error trap &
		! End FnGet.Buffer% function &

18000	!	F u n c t i o n   F n P u t . B u f f e r % &
	&
	&
	Def* FnPut.Buffer% (Chnl%) &
\	Put #Chnl%, Count Len.Out.Buf% &
\	Len.Out.Buf% = 0% &
\	FnPut.Buffer% = 0% &
\	Fnend &
		! Put output buffer to file &
		! Show no chars in output buffer &
		! Show success &
		! End FnPut.Buffer% function &

19000	!	S t a n d a r d   E r r o r   H a n d l e r &
	&
	&
	If Erl = 1100% then &
		If Err = 11% then &
			Resume 32767 &
		! If error on prompt, &
		!	If CTRL/Z detected, &
		!		Resume to exit &

19100	If Erl = 17000% then &
		Print "?Error reading from input file" &
\		Print FnError$(Err) &
\		Resume 32767 &
		! If error reading from input file, &
		!	Display error and resume to exit &

19200	If Erl = 17000% then &
		Print "?Error writing to output file" &
\		Print FnError$(Err) &
\		Resume 32767 &
		! If error writing to output file, &
		!	Display error and resume to exit &

19300	If Erl = 1200% or Erl = 1210% then &
		Z$ = "reading" &
\		Z$ = "writing" if Erl = 1210% &
\		Print "?Unable to open file "; In.Fil$; " for "; Z$ &
\		Print Fnerror$(Err) &
\		If Ccl% then &
			Resume 32767 &
		Else	In.Fil$ = "" &
\			Resume 1100 &
		! If error opening file for input or output &
		!	Init display text as open for input error &
		!	Change text if open for output error &
		!	Display error message &
		!	If CCL entry, &
		!		Resume to exit &
		!	Else	Clear input filespec &
		!		Resume to re-prompt &

19999	Print if CCPOS(0%) &
\	Print "??Program failure in REMESC" &
\	Print FnError$(Err); " at line"; Erl &
\	Resume 32767 &
		! Ensure at left margin &
		! Display fatal error message &
		! Resume to exit &

30000	!	C C L   E n t r y   P o i n t &
	&
	&
	On error goto 19000 &
\	Z$ = Sys(Chr$(7%)) &
\	Z% = Instr(1%,Z$," ") &
\	Z% = Len(Z$) + 1% unless Z% &
\	In.Fil$ = Cvt$$(Right(Z$,Z%+1%),-2%) &
\	Ccl% = (Len(In.Fil$) > 0%) &
\	Display% = 0% &
\	Goto 1020 &
		! Set standard error handler &
		! Read core common &
		! Locate 1st <space> &
		! Position at end if none &
		! Extract input file-spec from buffer &
		! Set CCL entry flag if non-null filespec &
		! Suppress displaying program header &
		! Merge with main code &

32767	End
