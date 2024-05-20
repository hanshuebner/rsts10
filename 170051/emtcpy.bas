2	!		Program		: EMTCPY
5	!		Version		: V10.1
6	!		Edit		: A
7	!		Edit date	: 10-MAY-91
10	extend
11	! &
!        ------------------------------------------------------------------ &
!       |                                                                  | &
!       |                       C O P Y R I G H T                          | &
!       |                                                                  | &
!       |                                                                  | &
!       |		      Copyright (C) 1982, 1991 by &
!       |          Digital Equipment Corporation, Maynard, Mass.           | &
!       |                                                                  | &
!       |                                                                  | &
!       |  This software is furnished under a license and may be used and  | &
!       |  copied  only  in accordance with the terms of such license and  | &
!       |  with the  inclusion  of  the  above  copyright  notice.   This  | &
!       |  software  or  any  other copies thereof may not be provided or  | &
!       |  otherwise made available to any other person.  No title to and  | &
!       |  ownership of the software is hereby transferred.                | &
!       |                                                                  | &
!       |  The information in this software is subject to change  without  | &
!       |  notice  and should not be construed as a commitment by Digital  | &
!       |  Equipment Corporation.                                          | &
!       |                                                                  | &
!       |  DIGITAL assumes no responsibility for the use  or  reliability  | &
!       |  of its software on equipment that is not supplied by DIGITAL.   | &
!        ------------------------------------------------------------------ &
!	&
!	&
!	&
!	&
!	This program is not supported by DIGITAL, and its inclusion on the &
!	distribution kit does not imply recommendation or endorsement by &
!	DIGITAL regarding its use.  There is no commitment by DIGITAL to &
!	update this program, nor to continue to include it in future &
!	distributions of RSTS/E. &
!	&

20	!			Modification History &

21	! VER/ED	EDIT DATE	REASON &
	! 9.3		10-NOV-86	ADD UNDERSCORE TO NL:'S &
	! 9.7		09-Sep-88	Update for V9.7 &
	! V9.7-06	06-Mar-89	(JJT) Handle wildcard filetypes &
	! &
	&
	&
	&
	&
	&
	&
	&
	&

100	!			General Description &
	&
	&
	&
!	This program demonstrates general techniques which may be used &
!	to retrieve EMT-Logging information from the RSTS/E Monitor. &
!	&
!	EMT Logging is an optional feature which may be included &
!	in a RSTS/E Monitor at system generation time;  it is described &
!	in the RSTS/E System Manager's Guide, the RSTS/E System &
!	Installation Guide, and the RSTS/E Release Notes.  Additional &
!	information (concerning necessary parameters for SEND/RECEIVE &
!	sys() calls) appears in the RSTS/E Programming Manual and the &
!	RSTS/E Directives Manual. &
!	&
!	This program  illustrates message structure, some parts of message &
!	formats, SEND/RECEIVE protocol, and Monitor interfacing.  It does not, &
!	however, attempt to do anything useful with the retrieved EMT &
!	information, beyond displaying it and/or writing it to a file. &
!	It does not handle special conditions (such as unexpected errors), &
!	nor is it written to optimize performance;  it is a "sample" program. &
!	&
!	The meaning of some data returned by EMT Logging, and the format &
!	of data fields within packets, are dependent upon internal &
!	Monitor algorithms, and is therefore subject to change without &
!	notice, any descriptions in this program notwithstanding. See &
!	the references cited above for more information. &

110	! This program performs three principal functions: &
!	&
!	    o  RECEIVE'ing EMT Logging packets from the Monitor &
!	&
!	    o  Writing packets into a "logging file" &
!	&
!	    o  Interpreting EMT Logging packets into a "display file" &
!	&
!	For the RECEIVE function, the DECLARE sys() parameters are &
!	described, and provision is made to honor a "shutup" message &
!	by issuing a REMOVE (and a KILL, if running detached).   The &
!	RECEIVE routine illustrates the use of the SLEEP and packets- &
!	per-message parameters.  A RECEIVE call will always return a &
!	message if packets are waiting at the time of the call, even &
!	if there be only one packet.  If there are no packets, however, &
!	and the EMT Logger goes into SLEEP/RECIEVE state, it will not &
!	be awakened by arrival of packets until the packets-per-message &
!	quota has been satisfied;  this allows several packets to be &
!	processed in one time slice, and overhead to be reduced.  Use &
!	of a SLEEP time limit insures that the program will get packets &
!	at least once every so many seconds, even if traffic should be &
!	light. &
!	&
!	The RECEIVE function can be terminated by reception of a "shutup" &
!	message (non-Monitor sender, with -1% as first "parameter" byte) or &
!	(if not logging detached) by typing "QUIT" or ^C. &
!	&
!	Information written into the logging file is processed only &
!	minimally, to keep overhead down;  logging-file data is essentially &
!	an image of packet data.  A "window" is provided in the code &
!	(see line 10010, and the skeleton subroutine at line 15000) at a &
!	point where packets might be inspected for possible discard, or &
!	for other processing.  The logging file is presumed to be on disk; &
!	block I/O is used, and the file is opened in "append" mode, &
!	so that consecutive runs of the program build a single, cumulative &
!	logging file.  The file's format is described on the next page. &
!	&
!	The "display file" facility interprets packet data in ascii, &
!	making the binary data "readable".  Interpretation requires &
!	significantly more processor resources than logging.  The &
!	display file can be on disk, too, or can be a character-oriented &
!	device, if the device speed/traffic volume permit the program &
!	to "keep up". &
!	&
!	The functions can be used in certain combinations.  For &
!	instance, RECEIVE'd packets can be either logged, interpreted, &
!	or both.  The interpretation function can be used to display &
!	packet information from a previously-written logging file. &
!	&
!	The program can be run detached, if desired. &
!	&
!	Being a "demonstration", this program does not trap for all &
!	errors, although it handles conditions expected during normal &
!	processing (such as err = 5% on RECEIVE when no messages are &
!	pending, for instance). &

120	! Logging File format - &
!	&
!	PMB's ("parameter" data returned by RECEIVE calls) and packets &
!	are written into the logging file in a variable-length format. &
!	Each "record" is prefixed with a two-byte count field;  each new &
!	record begins on an even boundary (one byte is possibly unused). &
!	PMB records are Loggable.PMB.Bytes% long;  any record longer than &
!	that is one which contains a packet's image. &
!	&
!	To make retrieval more convenient, no record is allowed to span &
!	blocks.  The write routines check for a fit before buffering each &
!	new record;  when the buffer has insufficient room to accept a new &
!	record in its entirety, the current buffer is PUT to disk, and &
!	the new record is buffered to start a new block.  A given block on &
!	disk is likely to contain unused bytes at the end;  these are &
!	discernible by the bytes which would be the next record's count &
!	field containing all one-bits.  This program fills unused bytes &
!	with zeros.  This is for clarity (appropriate in a demo program); &
!	the extra overhead is not really necessary. &
	&
	&

300	!			I/O Channels &
!	&
!	&
!    Name		Channel		Use &
!	&
!			   0		Normal RSTS/E console &
!    Logging.File%	   1		Logging output file &
!    Display.File%	   2		"Display" (interpreted) output file &
!	&
!    Packet.NL.Buffer%	   7		NL: buffer for packet-header decoding &
!    PMB.NL.Buffer%	   8		NL: buffer for returned "parameters" &
!    Receive.NL.Buffer%	   9		NL: buffer for returned messages &
!	&
!	(channel-number variables are defined at line 1040) &
	&
	&

400	!			Definition of Variables &
!	&
!	One- or two-character variable names are used for variables &
!	whose scope is local, and for dummy variables: &
!	&
!			I%, V$, D0$, D1$, etc. &
!	&
!	The variable names described below are those used frequently &
!	enough to justify short names.  All other variables are &
!	given self-explanatory, extend-mode "long" names. &
!	&
!	&
!		Name		Use &
!	&
!		S%()		Array for decoding data returned &
!				  from sys() calls &
!		Tn%		Tab() columns for display function &

800	!	    Line Number/Function/Subroutine Descriptions &
!	&
!	&
!	Line Number(s)	Type, level	Use &
!	&
!	1000		main: init	Entry;  initial stuff &
!	1040		main: init	Constants and general parameters &
!	&
!	1100		main: init	SEND/RECEIVE parameters &
!	1110		main: init	  DECLARE sys() call string &
!	1120		main: init	  RECEIVE sys() call string &
!	1130		main: init	  REMOVE sys() call string &
!	&
!	1200+		main: init	Obtain and lay out buffers &
!	2000+		main: dialogue	Startup - query for desired function &
!	3000+		main: log	Logging function mainline &
!	4000+		main: display	Display function mainline &
!	&
!	10000		1st level subr	Get an EMT Logging packet &
!	&
!	11000		1st level subr	Write a packet to logging-file buffer &
!	  11100		  2nd lvl subr	  Write a block from buffer &
!	    11200	    3rd lvl subr     Zero the buffer &
!	&
!	12000		1st level subr	Interpret a packet to display file &
!	  12100		  2nd lvl subr	  Interpret PMB ("parameters") data &
!	  12200		  2nd lvl subr	  Interpret packet data proper &
!	    12290	    functions	    local support for packet display &
!	&
!	13000		1st level subr	Retrieve packet data from logging file &
!	  13100		  2nd lvl subr	  Read a block into buffer &
!	&
!	14000		1st level subr	Initialize display-function captions &
!	&
!	15000		1st level subr	Skeleton subroutine, "window" on &
!					 packet processing to allow selection &
!					 of packets for logging function &
!	&
!	16000		1st level subr	Skeleton subroutine, "window" on &
!					 packet processing to allow selection &
!					 of packets retrieved from &
!					 for display function &
!	&
!	20000		functions	fnGet.Response$ &
!					  -get a typed string from KB: &
!					fnYes.No% &
!					  -get a yes/no answer from KB: &
!					fnOctal.Word$ &
!					  -interpret a word in ascii &
!					fnOctal.Byte$ &
!					  -interpret a byte in ascii &
!					fnP$ &
!					  -interpret number w/decimal point &

900	!			DIMension Statements &
	&
	&
	dim S%(40%),			! Array for returned sys() call data &
	&
	    FIRQB.Mnemonic$(13%),	! FIRQB mnemonics &
	&
	    FQFUN.Mnemonic$(20%),	! FQFUN mnemonics &
	    FQFUN.Description$(20%),	!         and descriptions &
	    UUO.Mnemonic$(75%),		! UUO mnemonics &
	    UUO.Description$(75%)	!         and descriptions &
	&
	&
!	The data for FIRQB mnemonics, and for the four caption arrays &
!	(FQFUN and UUO mnemonics and descriptions), follow. &
!	&
!	Note:  these captions are dependent upon the Monitor's FQFUN and &
!	UUO values, which could change (or be added to) in future releases &
!	of RSTS/E.  DIGITAL makes no commitment to update this program. &
	&

910	data	FQFIL, FQPPN, FQNAM1, '', FQEXT, FQSIZ, FQBUFL, &
		FQMODE, FQFLAG, '', FQDEV, FQDEVN, FQCLUS, FQNENT, &
	&
	CLS, 'Close a channel',	OPN, 'Open a channel',	CRE, 'Create a file', &
	DLN, 'Delete a file',	REN, 'Rename a file',	DIR, 'Directory info', &
	UUO, 'UUO; sys() call',	ERR, 'Get error text',	RST, 'Reset channels', &
	LOK, 'File lookup',	ASS, 'ASSIGN a device',	DEA, 'DEASSIGN dvice', &
	DAL, 'DEASSIGN all',	CRT, 'Create .TMP',	CRB, 'Create cmpiled', &
	RUN, 'Run a program',	xxx, '', &
	&
	rsv, 'reserved',	rsv, 'reserved',	rsv, 'reserved', &
	rsv, 'reserved',	rsv, 'reserved',	rsv, 'reserved', &
	TB3, 'Tables III',	SPL, 'SPOOL request',	DMP, 'Snapshot dump', &
	FIL, 'File utility',	ATR, 'File attributes',	CCL, 'Add/Delete CCL', &
	rsv, 'reserved',	rsv, 'reserved',	rsv, 'reserved', &
	rsv, 'reserved',	LOG, 'Set Logins',	RTS, 'RTS/reslib ctl', &
	NAM, 'Set RTS name',	DIE, 'SHUTUP system',	ACT, 'Acctng data', &
	DAT, 'Date/time ctl',	PRI, 'Priority, etc.',	TB2, 'Tables II', &
	BCK, 'File stats ctl',	rsv, 'reserved',	HNG, 'Hangup dataset', &
	FCB, 'FCB/DDB info',	rsv, 'reserved',	POK, 'Poke memory', &
	rsv, 'reserved',	rsv, 'reserved',	TB1, 'Tables I', &
	NLG, 'Logins <= 1',	YLG, 'Logins <= max', &
	&
	000, 'offset zero', 	PAS, 'Create PPN', &
	&
	DLU, 'Delete PPN',	CLN, 'Clean a disk',	MNT, 'Mount/dismount', &
	LIN, 'Login',		BYE, 'Logout',		ATT, 'Attach', &
	DET, 'Detach',		CHU, 'Password/quota',	ERR, 'Get error text', &
	ASS, 'ASSIGN',		DEA, 'DEASSIGN',	DAL, 'DEASSIGN all', &
	ZER, 'Zero a device',	RAD, 'Read acctg data',	DIR, 'Directory data', &
	TRM, 'TTYSET',		LOK, 'Wildcard lookup',	rsv, 'reserved', &
	CHE, 'Cache ctl',	CNV, 'Date => ascii',	SLN, 'Logical names', &
	rsv, 'reserved',	SWP, 'Swap+ files ctl',	JOB, 'Spawn a job', &
	PPN, 'PPN lookup',	SYS, 'Job status',	KMC, 'Connect KMC', &
	PRV, 'Privileges',	STL, 'Stall system',	rsv, 'reserved', &
	3PP, 'Third-party priv',CHK, 'File access chk',	ONX, 'Open next file', &
	CFG, 'Configure system',rsv, 'reserved',	rsv, 'reserved', &
	rsv, 'reserved',	rsv, 'reserved',	rsv, 'reserved', &
	xxx, '' &

1000	!	==================   Main Coding Area   ===================== &
	&
	&
!				      Initialization &
	&
	&
	&
	&
	&
! Set up privilege-control sys() call strings;  drop privileges &
	&
	&
	Drop.Privilege$ = chr$(6%) + chr$(-21%) + chr$(255%) &
\	Regain.Privilege$ = chr$(6%) + chr$(-21%) + chr$(0%) &
\	V$ = sys(Drop.Privilege$)	! Really drop them &
	&
	&
	&
	&
	&
	&
	&
\	print if ccpos(0%) <> 0%	! Return keyboard to left margin &
\	on error goto 19000		! Set standard error trap &

1010	Version.Edit$ = 'V10.1-A'	! Set up version/edit #. &

1020	print 'EMTCPY sample program';	! Print program's ID header &
	  chr$(9%); Version.Edit$; chr$(9%); &
	  cvt$$(right(sys(chr$(6%) + chr$(9%) + chr$(0%)) ,3%) ,4%) &

1030	change sys(chr$(12%)) to S%	! Get info on "last-opened file" &
	&
\	Job% = S%(1%) /2%		! Extract the job number &
\	Package.Location$ =		!   and develop "Package Location" &
	  '_' + chr$(S%(23%)) + chr$(S%(24%)) +			! Device &
	  num1$(S%(25%)) + ':' +				! Unit number &
	  '[' + num$(S%(6%)) + ',' + num1$(S%(5%)) + ']'	! PPN &
	&

1040	!		Set up general program parameters &
	&
	&
	Logging.File% = 1% &
\	Display.File% = 2%		! Define &
					!   channel &
\	Packet.NL.Buffer%  = 7%		!   numbers &
\	PMB.NL.Buffer%     = 8% &
\	Receive.NL.Buffer% = 9% &
	&
	&
\	Buffer.Offset% = 0%		! No offset &
\	Message.Byte.Limit% = 2048%	! Size of our RECEIVE buffer &
\	Packet.Byte.Limit% = 256%	! Size of our packet buffer &
	&
\	reserved% = 0%			! Nulls for sys() calls &
\	Zero$ = cvt%$(swap%(0%))	! A word of zero-bits &
\	Minus.One$ = cvt%$(swap%(-1%))	! A word of one-bits &

1100	!	Define parameters for SEND/RECEIVE control &
	&
	Receiver.ID$ = 'EMTCPY'		! Could be anything &
\	Object.Type% = 2%		! Required: &
					!   this indicates "EMT Logger" &
\	Access.Type% = 1%		! Allow local senders, &
		     + 2%		!   and insist they be privileged &
\	Buffer.Maximum% = 0%		! Use no small buffers for messages &
\	Message.Maximum% = 2%		! Allow a couple of non-EMT messages &
\	Link.Maximum% = 0%		! No network links &
	&
\	Packet.Maximum% = 50%	! Allowable packet queue capacity - &
				!  installation-dependent, and can &
				!  affect performance &
	&
\	Outgoing.Link.Maximum% = 0%	! No network links &
\	Packets.per.Message% = 5%	! Also installation-dependent &
\	RIB.Number% = 0%		! Arbitrary, but the normal case &
	&
	&
\	Receive.Modifier% = 1%		! SLEEP if no messages pending &
			  + 4%		!   and specify "local senders" &
\	SLEEP.Time% = 10%		! Time to SLEEP on RECEIVE's &
	&
	&
	&
	&
	&
	&

1110	!	Compose the call string for DECLARE receiver &
	&
	&
	Receiver.ID$ = left(Receiver.ID$ + '      ', 6%) &
					! Insure receiver ID is six characters &
	&
	&
\	Declare.Receiver$ = chr$(6%)		! FIP sys() call code	     1 &
			  + chr$(22%)		! SEND/RECEIVE function	     2 &
			  + chr$(1%)		! DECLARE subfunction code   3 &
	&
			  + chr$(reserved%)			       !     4 &
			  + Receiver.ID$			       !  5-10 &
			  + string$(10%, reserved%)		       ! 11-20 &
			  + chr$(Object.Type%)			       !    21 &
			  + chr$(Access.Type%)			       !    22 &
			  + cvt%$(swap%(Buffer.Maximum%))	       ! 23-24 &
			  + chr$(Message.Maximum%)		       !    25 &
			  + chr$(Link.Maximum%)			       !    26 &
			  + cvt%$(swap%(Packet.Maximum%))	       ! 27-28 &
			  + chr$(Outgoing.Link.Maximum%)	       !    29 &
			  + chr$(Packets.per.Message%)		       !    30 &
			  + string$(4%, reserved%)		       ! 31-34 &
			  + chr$(RIB.Number%)			       !    35 &
			  + string$(5%, reserved%)		       ! 36-40 &

1120	Receive.Some.Packets$	! Sys() call string for RECEIVE &
	&
		= chr$(6%)		! FIP sys() call code	       :     1 &
		+ chr$(22%)		! SEND/RECEIVE function	       :     2 &
		+ chr$(2%)		! RECEIVE subfunction	       :     3 &
		+ chr$(Receive.Modifier%)			       !     4 &
		+ chr$(0%)		! Sender selection:  not       :     5 &
		+ chr$(0%)		!   specific by job number     :     6 &
		+ string$(4%, reserved%)			       !  7-10 &
		+ chr$(Receive.NL.Buffer%)			       !    11 &
		+ chr$(reserved%)				       !    12 &
		+ cvt%$(swap%(Message.Byte.Limit%))		       ! 13-14 &
		+ cvt%$(swap%(Buffer.Offset%))			       ! 15-16 &
		+ string$(10%, reserved%)			       ! 17-26 &
		+ cvt%$(swap%(Sleep.Time%))			       ! 27-28 &
		+ string$(6%, reserved%)			       ! 29-34 &
		+ chr$(RIB.Number%)				       !    35 &
		+ string$(5%, reserved%)			       ! 36-40 &
	&
	&
	&
	&
	&
	&

1130	Remove.Receiver$	! Sys() call string for REMOVE receiver &
	&
		= chr$(6%)		! FIP sys() call code	       :     1 &
		+ chr$(22%)		! SEND/RECEIVE function	       :     2 &
		+ chr$(0%)		! REMOVE subfunction	       :     3 &
		+ chr$(0%)		! Job number: this job	       :     4 &
		+ string$(30%, reserved%)			       !  5-34 &
		+ chr$(0%)		! RIB number (not used)	       :    35 &
		+ chr$(-1%)		! Remove-all-RIB's flag	       :    36 &
		+ string$(4%, reserved%)			       ! 37-40 &

1200	!		Obtain buffers, and define their layouts &
	&
	&
	open '_NL:' as file # PMB.NL.Buffer%, &
	  recordsize 40%				! "Parameters" buffer &
	&
\	open '_NL:' as file # Receive.NL.Buffer%, &
	  recordsize Message.Byte.Limit%		! RECEIVE buffer &
	&
\	open '_NL:' as file # Packet.NL.Buffer%, &
	  recordsize Packet.Byte.Limit%			! Packet buffer &
	&
	&
	&
	&
	&
	&
!		Define the "parameters" (the PMB) returned by a RECEIVE call &
	&
	&
\	field # PMB.NL.Buffer%, 40% as PMB.NL.Buffer$	! The whole buffer &
	&
\	field # PMB.NL.Buffer%,				! Now, its fields.. &
	&
	&
		2% as D0$,		! "Not meaningful" &
		1% as D1$,		! Message subfunction code (constant) &
		1% as PMB.Job$,		! Sender's job number :	byte 4 &
		2% as PMB.PPN$,		! Sender's PPN	      :	bytes 5-6+ &
		1% as PMB.KB$,		! Sender's KB number  :	byte 7 &
		1% as D2$,		! "Not meaningful"    :	byte 8 &
		2% as PMB.Bytes.Remaining$,		      !	bytes 9-10 &
		2% as D3$,		! "Not meaningful"    :	bytes 11-12 &
		2% as PMB.Bytes.Transferred$,		      ! bytes 13-14 &
		6% as D4$,		! "Not meaningful"    :	bytes 15-20 &
	&
				! Beginning of "Parameters" area proper &
	&
		2% as PMB.Packets.Remaining$,		      !	bytes 21-22 &
		2% as PMB.Missed.EMTs$,			      !	bytes 23-24 &
		2% as PMB.Packets.Transferred$,		      !	bytes 25-26 &
		14% as D5$		! Not used	      :	bytes 27-40 &
	&
	&
	&
	&
\	Loggable.PMB.Bytes% = 26%		! Length of PMB data to log &
\	Loggable.PMB.Bytes$ = cvt%$(swap%(Loggable.PMB.Bytes%)) &
	&
\	field # PMB.NL.Buffer%, &
		Loggable.PMB.Bytes% as Loggable.PMB$	! Define PMB data &
	&
	&
	&
	&
\	field # PMB.NL.Buffer%, 20% as D6$,		! Finally, define &
							! the first byte of &
		1% as PMB.First.Parameter.Byte$		! "parameters" sent &
							! by sender other than &
							! the Monitor &

1210	!	Define the most-frequently-used areas of an EMT Logging &
!		packet. &
!	&
!		Note:  in future releases of RSTS/E, changes in the Monitor &
!		may result in changes in the format of EMT Logging packets. &
!		DIGITAL makes no commitment to update this program. &
	&
	&
	&
	field # Packet.NL.Buffer%, Packet.Byte.Limit% &
		as Packet.NL.Buffer$			! The whole buffer &
	&
	&
	&
\	field # Packet.NL.Buffer%,			! Now, its fields.. &
	&
		1% as Packet.Options.Length$, &
		1% as Packet.XRB.Length$, &
		1% as Packet.FIRQB.Length$, &
		1% as Packet.Root.Length$, &
		2% as Packet.Sequence$, &
		2% as Packet.Date$, &
		2% as Packet.Time$, &
		1% as Packet.Seconds$, &
		1% as Packet.Ticks$, &
		1% as Packet.Job$, &
		1% as D0$,			! Reserved &
		1% as Packet.IOSTS$, &
		1% as Packet.Function.Code$, &
		1% as D1$,			! Reserved &
		1% as Packet.KB$, &
		2% as D2$,			! Reserved &
		2% as Packet.PPN$, &
		2% as Packet.PC$, &
		1% as Packet.UUO$, &
		1% as D3$			! Reserved &

2000	!	==========  Startup:  Determine Desired Function  ========== &
	&
	print
2010	print 'EMT Logger demonstration function <HELP> '; &
\	Function$ = fnGet.Response$(-1%)  ! Get typed response &
	&
	&
	&
!			-------   Dispatch   ------- &
	&
\	goto 2020 unless len(Function$) &
\	goto 3000 if Function$ = left('LOG', len(Function$)) &
\	goto 4000 if Function$ = left('DISPLAY', len(Function$)) &
	&
	&

2020	!   Hmm..  evidently, HELP is wanted/needed &
	&
	print &
\	print 'Available demonstration functions are:' &
\	print &
\	print '    LOG       - Copy received EMT packets to file(s)' &
\	print '    DISPLAY   - Display packets from a log file' &
	&
\	goto 2000			! Go prompt again &

3000	!	=============  Logging Demonstration Function  ============ &
	&
	&
	Detach.Requested% = fnYes.No%('Run detached', 'NO') &
				! Give the option of running detached &
	&
\	print 'Logging (packet-image) output file <none> '; &
\	Logging.File$ = fnGet.Response$(-1%)  ! Get typed response &
	&
\	if len(Logging.File$) then	! If logging file wanted, &
	  open Logging.File$		!   open it, &
	    as file Logging.File%,	!     using "append" mode &
	      mode 2% &
\	  Logging.File.Active% = -1%	!   and set flag that it's being used &
	&
\	  field # Logging.File%, 512% as Logging.File.Buffer$ &
\	  gosub 11200			! Define, initially clear the buffer &
	&
	&

3010	print 'Display (interpreted) output file <none> '; &
\	Display.File$ = fnGet.Response$(-1%) &
	&
\	if len(Display.File$) then	! If display file wanted, &
	  open Display.File$		!   Open it, &
	    for output as file		!     creating a new file &
	      Display.File% &
\	  Display.File.Active% = -1%	!   Set flag that it's being used &
\	  gosub 14000			!   Set up descriptive captions &
	&
	&

3020	goto 32767 unless Logging.File.Active% or Display.File.Active% &
			! Quit immediately if no function requested &
	&
\	V$ = sys(Remove.Receiver$)	! Insure no RIB's to start &
\	V$ = sys(Regain.Privilege$)	! Activate privileges &
\	V$ = sys(Declare.Receiver$)	! Declare this program as EMT Logger &
\	V$ = sys(Drop.Privilege$)	! Drop privileges again &
\	V$ = sys(chr$(6%)+chr$(-7%))	! Trap CTRL/C's &
	&
\	goto 3500 unless Detach.Requested%  ! Go start main processing loop &
	&
\	print 'Detaching..' &
\	V$ = sys(Regain.Privilege$)	! Get privileges once more &
\	V$ = sys(chr$(6%) + chr$(7%))	! Do the DETACH &
\	V$ = sys(Drop.Privilege$)	!  and drop privileges &
\	Detached% = -1%			! Set flag: program's running detached &
\	goto 3500			!   and go start main processing loop &

3500	!	    ------  Log Function's Processing Loop  ----- &
	&
	&
	while Logging.File.Active% or Display.File.Active% &
	&
\	  gosub 10000			! Get a packet &
\	  gosub 11000			! Salt it away in logging file &
\	  gosub 12000			! Interpret it &
	&
\	next				! Loop for another &
	&
!		(Logging.File.Active% and Display.File.Active% are &
!		turned off in 10000 upon receipt of a shutup message) &
	&
\	gosub 11100			! Write out any possible final block &
\	goto 32766			! And exit &

4000	!	=============  Display Demonstration Function  ============ &
	&
	&
	print 'Logging (packet-image) input file '; &
\	Logging.File$ = fnGet.Response$(-1%)  ! Get typed response &
\	goto 32766 unless len(Logging.File$)	! Quit if null response &
	&
\	open Logging.File$ for input as file	! Open specified logging file &
	  Logging.File%, mode 8192%		!   using "read-only" mode &
\	field # Logging.File%, 2% as D$		! Preload its buffer with &
\	lset D$ = Zero$				!  "no records here" (yet) &
	&
\	print 'Display (interpreted) output file '; &
\	Display.File$ = fnGet.Response$(-1%) &
\	goto 32766 unless len(Display.File$)	! Quit if null response &
	&
\	open Display.File$ 		! Open the specified display file &
	  for output as file Display.File% &
\	Display.File.Active% = -1%	!   and set flag that it's being used &
\	gosub 14000			! Set up descriptive captions &
	&
	&
	&
	&
	&
	&
	&
	&
	&
	&
!		--------  Display Function's Processing Loop  -------- &
	&
	&
\	while Display.File.Active% &
	&
\	  gosub 13000			! Get a packet back from logging file &
\	  gosub 12000			! Interpret it &
	&
\	next &
	&
!		(Display.File.Active% is turned off in 13000 upon &
!		detection of end-of-file on the input logging file.) &
	&
\	print &
\	print 'DISPLAY demonstration run complete.' &
\	goto 32766 &

10000	!	==================  Subroutines  ================= &
	&
	&
	&
!		First-level subroutine:  get a packet &
!	&
!		returned:  "parameters", in PMB.NL.Buffer$ &
!			      (same data for all packets from the &
!			       message covered by the parameters) &
!			   First.Packet.in.Message% - true or false &
!	&
!			   (next) packet, in Packet.NL.Buffer$ &
!			   Current.Packet.Length%, Current.Packet.Length$ &
!	&
!					-- or -- &
!	&
!			   Logging.File.Active% and Display.File.Active% &
!			   set to 0, if "shutup" message has been received &
	&
	&
	&
	First.Packet.in.Message% = 0%	! Clear "first packet" flag &
	&
\	goto 10020 unless Logging.File.Active% or Display.File.Active% &
		! Exit immmediately if nothing's going on &
	&

10010	goto 10030 unless Packets.to.be.Returned% &
			! Go do RECEIVE if it's time (again); &
			!  otherwise, we have packet(s) remaining &
			!  from a previous RECEIVE: &
	&
\	  field # Receive.NL.Buffer%, Buffer.Previously.Scanned% as D0$, &
		2% as Current.Packet.Length$ &
\	  Current.Packet.Length% = swap%(cvt$%(Current.Packet.Length$)) &
			! Get byte count of (next) packet in message buffer &
	&
\	  stop unless Current.Packet.Length% > Loggable.PMB.Bytes% &
			! The ability to differentiate between PMB records &
			!  and packet records in the logging file depends &
			!  upon packet records being longer &
	&
\	  field # Receive.NL.Buffer%, Buffer.Previously.Scanned% + 2% as D1$, &
		Current.Packet.Length% as Current.Packet$ &
\	  lset Packet.NL.Buffer$ = Current.Packet$ &
			! Load packet to be returned into FIELD'ed buffer &
	&
\	  Buffer.Previously.Scanned% =		! Advance pointer to &
		Buffer.Previously.Scanned%	!  next packet in buffer &
		+2% + Current.Packet.Length% &
	&
\	  Packets.to.be.Returned% = Packets.to.be.Returned% - 1% &
			! One fewer packet waiting to be returned now &
	&
	&
\	  gosub 15000			! Allow a "packet selection" routine &
\	  goto 10010 unless Packet.Wanted%   !  ..discard un-selected packets &
	&

10020	on error goto 19000		! Reset standard error trapping &
\	return				!  and exit &

10030	!			get-a-packet subroutine (10000), continued &
	&
	&
	&
	on error goto 10050		! Set up trap for err 5% &

10040	!			----  The RECEIVE  ---- &
	&
	print '  Executing RECEIVE; '; unless Detached%	! Keep demo log &
\	lset PMB.NL.Buffer$ = sys(Receive.Some.Packets$) &
	&
\	goto 10090 if ascii(PMB.Job$)	! Check for a "normal" message, from &
					! some job instead of the Monitor &
	&
\	Packets.to.be.Returned% =		! Get count of packets &
	  swap%(cvt$%(PMB.Packets.Transferred$))   ! in this message &
	&
\	print Packets.to.be.Returned%; 'packet(s) obtained' unless Detached% &
\	stop unless Packets.to.be.Returned% &
			! Only an (SPR-able) logic error could cause this, &
			!   but better a STOP than a loop &
\	D% = swap%(cvt$%(PMB.Packets.Remaining$))	! Demo log: display &
\	print '  -'; D%; 'more packet(s) pending'	!  packets still "out &
		if D% unless Detached%			!  there" waiting &
\	D% = swap%(cvt$%(PMB.Missed.EMTs$))		! Demo log: display &
\	print '  -'; D%; 'missed EMT(s)'		!  EMT's missed since &
		if D% unless Detached%			!  last RECEIVE &
	&
\	Buffer.Previously.Scanned% = 0%		! Reset deblocking pointer &
\	First.Packet.in.Message% = -1%		! Say next pckt is 1st in msg &
\	goto 10010		! Go strip off and return the first packet &
	&
	&

10050	!	An error:  no-message-pending, most likely &
	&
	on error goto 19000 unless err = 5%	! Something else: complain &
\	goto 19000 if err = 28%			! Handle CTRL/C's &
\	resume 10060				! Nope, that's what it was &

10060	goto 10030 if Detached%	! The following only if still attached &
\	  print tab(21%); unless ccpos(0%) &
\	  print '  ..nothing obtained'		! Keep demo log &
\	  on error goto 10070		! Set up new error trap &
\	  get #0%, record 8192%		!  Has anything been typed ? &
\	  on error goto 19000		!   Yes:  reset error trap &
\	  field #0%, recount as D0$	!         extract typed string &
\	  D1$ = cvt$$(D0$, -1%)		!	  and clean it up &
\	  print tab(21%); unless ccpos(0%) &
\	  print '  ..awakened by '; &
\	  print '"'; D1$; '"'; if len(D1$)		! Log reason for &
\	  print 'typed delimiter'; unless len(D1$)	! being awakened &
\	  print &
\	  goto 10090 if D1$ = left('QUIT', len(D1$)) if len(D1$) &
\	  print tab(23%); 'Type QUIT to terminate demonstration' &
\	  goto 10030
10070	  resume 10090 if err = 11%	! Make ^Z equivalent to QUIT &
\	  resume 10030 if err = 13%	! Go RECEIVE again if nothing typed &
\	  goto 19000 if err = 28%	! Handle CTRL/C's &

10080	!	A message from other than the Monitor &
	&
	PMB.First.Parameter.Byte% = ascii(PMB.First.Parameter.Byte$) &
\	print ' non-Monitor message';			! Keep demo log &
		PMB.First.Parameter.Byte%; 'obtained' unless Detached% &
\	goto 10030 unless PMB.First.Parameter.Byte% = -1% &
			! Inspect first byte from "parameters" area; &
			! -1% is the indicator which will be passed &
			! by SHUTUP.  Any other value indicates some &
			! unsolicited message;  discard it by going &
			! to do another RECEIVE/SLEEP. &
	&
	&
	&
	&

10090	!	An apparent SHUTUP message received &
	&
	print unless Detached% &
\	print 'EMTCPY demo program shutting down' unless Detached% &
\	Logging.File.Active%, Display.File.Active% = 0% &
			! Quitting time - so indicate &
	&
\	goto 10020	!  and go return &

11000	!	First-level subroutine:  buffer packet for logging file &
!	&
!		supplied:  packet, in Packet.NL.Buffer$ &
!			   Current.Packet.Length%, Current.Packet.Length$ &
!			   First.Packet.in.Message% - true or false &
!			   "parameters", in PMB.NL.Buffer$ &
!			      (same data for all packets from the &
!			       message covered by the parameters) &
	&
	&
	&
	&
	return unless Logging.File.Active%	! Quick exit if nothing to do &
	&
	&
!   Load PMB ("parameters" data) in buffer, if this is the first &
!   packet contained in the message being logged &
	&
	&
\	if First.Packet.in.Message% then	! Include PMB data for first &
						!  packet of a new message &
	&
	  gosub 11100 if Logging.Buffer.Next.Byte%	! Write the buffer &
		+ 2% + Loggable.PMB.Bytes% > 510%	!   if space exhausted &
	&
\	  field # Logging.File%, &
		Logging.Buffer.Next.Byte% as D0$,	! Define &
				       2% as C$,	!   PMB count field &
		      Loggable.PMB.Bytes% as R$		!   and PMB "record" &
	&
\	  lset C$ = Loggable.PMB.Bytes$		! Load the PMB's count &
\	  lset R$ = Loggable.PMB$		!   and its data &
	&
\	  Logging.Buffer.Next.Byte% = &
		(Logging.Buffer.Next.Byte%	! Point past data loaded so &
		+ 2% + Loggable.PMB.Bytes%	!   far in logging-file buffer &
		+ 1%) and -2%			!   rounded to next even byte &

11010	! Load packet in logging-file buffer &
	&
	&
	gosub 11100 if Logging.Buffer.Next.Byte%	! Clear buffer &
		+ 2% + Current.Packet.Length% > 510%	!   if space exhausted &
	&
\	field # Logging.File%, &
		Logging.Buffer.Next.Byte% as D0$,	! Define &
				       2% as C$,	!   packet count field &
		   Current.Packet.Length% as R$		!   and its "record" &
	&
\	lset C$ = Current.Packet.Length$	! Load the packet's length &
\	lset R$ = Packet.NL.Buffer$		!   and its data &
	&
\	Logging.Buffer.Next.Byte% = &
		(Logging.Buffer.Next.Byte%	! Point past data loaded so &
		+ 2% + Current.Packet.Length%	!   far in logging-file buffer &
		+ 1%) and -2%			!   rounded to next even byte &
\	return &
	&
	&
	&
	&
	&
	&
	&

11100	!	Second-level subroutine:  write a buffer to disk &
	&
	&
	&
	return unless Logging.Buffer.Next.Byte%	! Exit if nothing to write &
	&
\	field # Logging.File%,				! Define dummy &
		Logging.Buffer.Next.Byte% as D0$,	! count field &
		                       2% as Stopper$ &
\	lset Stopper$ = Minus.One$		! Set "no more" in count field &
	&
\	put # Logging.File%	! The physical WRITE &
	&
\	Logging.Buffer.Next.Byte% = 0%		! Reset buffer "fill" pointer &
\	gosub 11200				! Zero the new buffer &
\	return &
	&
	&
	&

11200	!	Third-level subroutine:  zero the logging-file buffer &
	&
	&
	Five.Twelve.Zeros$ = string$(512%, 0%)	! Create bufferful of zeros &
		unless len(Five.Twelve.Zeros$)	!   upon first entry &
	&
\	lset Logging.File.Buffer$ = Five.Twelve.Zeros$	! Clear the buffer &
\	return &

12000	!	First-level subroutine:  interpret packets, and write ascii &
!					      interpretation into display file &
!	&
!		supplied:  packet, in Packet.NL.Buffer$ &
!			   Current.Packet.Length% &
!			   First.Packet.in.Message% - true or false &
!			   "parameters", in PMB.NL.Buffer$ &
!			      (same data for all packets from the &
!			       message covered by the parameters) &
	&
	&
	&
	return unless Display.File.Active%	! Quick exit if nothing to do &
	&
\	gosub 12100 if First.Packet.in.Message%		! Interpret PMB first &
\	gosub 12200					! Interpret packet &
\	return &

12100	!	Second-level subroutine:  interpret PMB data &
!					    (RECEIVE "parameters") &
	&
	&
	print # Display.File% &
\	print # Display.File% &
\	print # Display.File% &
\	print # Display.File% &
\	print # Display.File% &
\	print # Display.File%, '==== PMB ("parameters") ====' &
\	print # Display.File% &
\	D% = ascii(PMB.Job$) &
\	print # Display.File%, 'PMB.Job:                 '; D% if D% &
\	D% = swap%(cvt$%(PMB.PPN$)) &
\	print # Display.File%, 'PMB.PPN:                  '; &
		'['; num1$(swap%(D%) and 255%); &
		','; num1$(D% and 255%); ']' if D% &
\	D% = ascii(PMB.KB$) &
\	print # Display.File%, 'PMB.KB                    '; &
		'KB'; num1$(D%); ':' if D% &
\	print # Display.File%, 'PMB.Bytes.Remaining:     '; &
		swap%(cvt$%(PMB.Bytes.Remaining$)) &
\	print # Display.File%, 'PMB.Bytes.Transferred:   '; &
		swap%(cvt$%(PMB.Bytes.Transferred$)) &
\	print # Display.File%, '   ----------------------' &
\	print # Display.File%, 'PMB.Packets.Remaining:   '; &
		swap%(cvt$%(PMB.Bytes.Remaining$)) &
\	print # Display.File%, 'PMB.Missed.EMTs:         '; &
		swap%(cvt$%(PMB.Missed.EMTs$)) &
\	PMB.Packets.Transferred% = swap%(cvt$%(PMB.Packets.Transferred$)) &
\	print # Display.File%, 'PMB.Packets.Transferred: '; &
		PMB.Packets.Transferred% &
\	Packet.Index% = 0% &
\	return &

12200	!	Second-level subroutine:  interpret packet data proper &
	&
	&
	Packet.Index% = Packet.Index% + 1% &
\	print # Display.File% &
\	print # Display.File% &
\	print # Display.File%, '  ---------- Packet'; Packet.Index%; 'of'; &
		PMB.Packets.Transferred%; 'in this message ----------' &
\	print # Display.File% &
\	print # Display.File%, 'Packet length:  '; &
		fnOctal.Word$(Current.Packet.Length%); &
		'  ('; fnP$(Current.Packet.Length%); ')' &
\	print # Display.File% &
\	print # Display.File%, '  Field'; tab(T1%); 'Octal'; &
		tab(T2%); 'Decimal'; tab(T3%); 'Comments' &
\	print # Display.File% &
\	Packet.Options.Length% = ascii(Packet.Options.Length$) &
\	print # Display.File%, '  Packet.Options.Length:'; tab(T1%); &
		fnOctal.Byte$(Packets.Options.Length%); &
		tab(T2%); fnP$(Packet.Options.Length%) &
\	Packet.XRB.Length% = ascii(Packet.XRB.Length$) &
\	print # Display.File%, '  Packet.XRB.Length:'; tab(T1%); &
		fnOctal.Byte$(Packet.XRB.Length%); &
		tab(T2%); fnP$(Packet.XRB.Length%) &
\	Packet.FIRQB.Length% = ascii(Packet.FIRQB.Length$) &
\	print # Display.File%, '  Packet.FIRQB.Length:'; tab(T1%); &
		fnOctal.Byte$(Packet.FIRQB.Length%); &
		tab(T2%); fnP$(Packet.FIRQB.Length%) &
\	Packet.Root.Length% = ascii(Packet.Root.Length$) &
\	print # Display.File%, '  Packet.Root.Length:'; tab(T1%); &
		fnOctal.Byte$(Packet.Root.Length%); &
		tab(T2%); fnP$(Packet.Root.Length%) &
	&
	&
	&
\	F% = swap%(cvt$%(Packet.Sequence$)) &
\	print # Display.File%, '  Packet.Sequence:'; tab(T1%); &
		fnOctal.Word$(F%); tab(T2%); fnP$(F%) &
\	F% = swap%(cvt$%(Packet.Date$)) &
\	print # Display.File%, '  Packet.Date:'; tab(T1%); &
		fnOctal.Word$(F%); tab(T2%); fnP$(F%); tab(T3%); date$(F%) &
\	F% = swap%(cvt$%(Packet.Time$)) &
\	print # Display.File%, '  Packet.Time:'; tab(T1%); &
		fnOctal.Word$(F%); tab(T2%); fnP$(F%); tab(T3%); time$(F%) &
\	F% = ascii(Packet.Seconds$) &
\	F$ = '0' + num1$(60% - F%) &
\	print # Display.File%, '  Packet.Seconds:'; tab(T1%); &
		fnOctal.Byte$(F%); tab(T2%); fnP$(F%); &
		tab(T3%); 'hh:mm:'; right(F$, len(F$) - 1%) &
\	F% = ascii(Packet.Ticks$) &
\	print # Display.File%, '  Packet.Ticks:'; tab(T1%); &
		fnOctal.Byte$(F%); tab(T2%); fnP$(F%) &
\	F% = ascii(Packet.Job$) &
\	print # Display.File%, '  Packet.Job:'; tab(T1%); &
		fnOctal.Byte$(F%); tab(T2%); fnP$(F%); &
		tab(T3%); 'Job'; F% / 2% &

12210	!			interpret packet, continued &
!						   (@ IOSTS..) &
	&
	&
	&
	F% = ascii(Packet.IOSTS$) &
\	F$ = '-ok-' &
\	F$ = cvt$$(right(sys(chr$(6%) + chr$(9%) + chr$(F%)), 3%), 133%) if F% &
\	print # Display.File%, '  Packet.IOSTS:'; tab(T1%); &
		fnOctal.Byte$(F%); tab(T2%); fnP$(F%); tab(T3%); F$ &
	&
\	F% = ascii(Packet.Function.Code$)
12220	print # Display.File%, '  Packet.Function.Code:'; tab(T1%); &
		fnOctal.Byte$(F%); tab(T2%); fnP$(F%); tab(T3%); &
\	F2% = F% / 2% &
\	if F2% < 0% or F2% > Maximum.FQFUN% then &
	  print # Display.File%, '?Invalid' &
	else &
	  print # Display.File%, FQFUN.Mnemonic$(F2%); &
		       'FQ    '; FQFUN.Description$(F2%) &

12230	F% = ascii(Packet.KB$) &
\	print # Display.File%, '  Packet.KB:'; tab(T1%); &
		fnOctal.Byte$(F%); tab(T3%); &
\	print # Display.File%, 'KB'; num1$(F%); ':' unless F% and 128% &
\	print # Display.File%, '"detached from" KB'; &
		num1$(not(F%) and 255%); ':' if F% and 128% &
	&
\	F% = swap%(cvt$%(Packet.PPN$)) &
\	print # Display.File%, '  Packet.PPN:   '; &
		fnOctal.Byte$(swap%(F%) and 255%); ' '; &
		fnOctal.Byte$(F% and 255%); tab(T1%); &
		fnOctal.Word$(F%); tab(T3%); &
		'['; num1$(swap%(F%) and 255%); ','; num1$(F% and 255%); ']' &
\	F% = swap%(cvt$%(Packet.PC$)) &
\	print # Display.File%, '  Packet.PC:'; tab(T1%); &
		fnOctal.Word$(F%) &

12240	U% = ascii(Packet.UUO$) &
\	U% = (U% or -256%) if U% and 128%	! Sign extend word <- byte &
\	F% = U% + UUO.Zero.Offset% &
\	print # Display.File%, '  Packet.UUO:'; tab(T1%); &
		fnOctal.Byte$(U%); tab(T2%); &
\	print # Display.File%, '+'; if U% >= 0% &
\	print # Display.File%, fnP$(U%); tab(T3%); &
\	if U% = 127% then &
	  print # Display.File%, '(Not a UUO)' &
	else &
	  if F% < 1% or F% > 75% then &
	    print # Display.File%, '?Invalid UUO code'	! SPR-able error &
	  else &
	    print # Display.File%, 'UU.'; UUO.Mnemonic$(F%); &
				   '   '; UUO.Description$(F%) &

12270	!			interpret packet, continued &
!						   (FIRQB, XRB, Options) &
	&
	print # Display.File% for I% = 1% to 2% &
\	print # Display.File%, 'Offset         FIRQB                  '; &
			       '               XRB                   Options' &
\	print # Display.File% &
	&
	&
\	field # Packet.NL.Buffer%,		! Define the fields &
	    Packet.Root.Length% as Packet.Root$, &
	    Packet.FIRQB.Length% as Packet.FIRQB$, &
	    Packet.XRB.Length% as Packet.XRB$, &
	    Packet.Options.Length% as Packet.Options$ &
	&
\	L% = Packet.FIRQB.Length% + 2%		! Select largest of the three &
\	L% = Packet.XRB.Length% if Packet.XRB.Length > L% &
\	L% = Packet.Options.Length% if Packet.Options.Length% > L% &
	&
\	for I% = 0% to L% step 2%	! Loop through FIRQB-XRB-Options data &
	&
\	  print # Display.File%, fnDisplay.Offset$(I%); '        '; &
				 fnDisplay$(I% - 4%, Packet.FIRQB$); &
					! Display "offset", octal FIRQB word &
					!  (doing so loads W% with the word) &
\	  D% = (I% - 4%) / 2% &
\	  if Word.in.Sub.Field% and D% >= 0% and D% <= 13% then &
	    print # Display.File%, FIRQB.Mnemonic$(D%);	    ! Annotate FIRQB.. &
\	    print # Display.File%, '['; num1$(swap%(W%) and 255%); &
			','; num1$(W% and 255%); ']'; if D% = 1%       ! FQPPN &
\	    print # Display.File%, rad$(W%); if (D% >= 2% and D% <= 4%) &
					    and (W% <> -1%)  ! FQNAM and FQEXT &
\	    print # Display.File%, "*"; if (D% >= 2% and D% <= 4%) &
					    and (W% = -1%)  ! Handle wildcards &
\	    print # Display.File%, fnP$(W% and 255%); if D% = 11%     ! FQDEVN &
			and (swap%(W%) and 255%) = 255% &
\	    if D% = 10% then &
	      W0% = W% and 255%					       ! FQPPN &
\	      W1% = swap%(W%) and 255% &
\	      if W0% > 64% and W0% < 91% and W1% > 64% and W1% < 91% then &
		print # Display.File%, chr$(W0%); chr$(W1%);	       ! FQDEV &

12280	  print # Display.File%, tab(T4%); &
				 fnDisplay$(I%, Packet.XRB$); '        '; &
				 fnDisplay$(I%, Packet.Options$) &
\	next I% &
	&
	&
	&
	&
\	print # Display.File%, chr$(12%);	! Packet complete:  new page &
\	return &

12290	!			interpret packet, continued &
!				  (define local functions used above) &
	&
	&
	&
	def* fnDisplay.Offset$(O%)	! Three-digit "offset" &
\	  stop if O% > 255%				! Only doing 3 digits &
\	  D$ = fnOctal.Byte$(O%)			! Build octal string &
\	  D$ = '  ' + right(D$, 3%) if left(D$, 2%) = '00' &
\	  D$ = ' ' + right(D$, 2%) if left(D$, 1%) = '0' &
\	  fnDisplay.Offset$ = D$	! Leading zeros replaced with spaces &
\	fnend &
	&
	&
	&
	&
\	def* fnDisplay$(Cursor%, Sub.Field$)	! Display a word from subfield &
\	  fnDisplay$ = 			   ! (W% returned containing the word) &
	    fnDisplay.Word$(Cursor%, Sub.Field$) + '   '      ! Word value &
	    + fnDisplay.Byte$(Cursor% + 1%, Sub.Field$) + ' ' ! m/s byte value &
	    + fnDisplay.Byte$(Cursor%, Sub.Field$)	      ! l/s byte value &
\	  fnend &
	&
	&
	&
	&
\	def* fnDisplay.Word$(Cursor%, Sub.Field$)     ! Octal Word subfunction &
\	  D$ = '      '			   ! (W% returned containing the word) &
\	  Word.in.Sub.Field% =			      ! Flag word's "presence" &
		(Cursor% + 1% < len(Sub.Field$) and Cursor% >= 0%) &
\	  W% = swap%(cvt$%(mid(Sub.Field$, Cursor% + 1%, 2%)))  ! Extract word &
		if Word.in.Sub.Field% &
\	  D$ = fnOctal.Word$(W%) if Word.in.Sub.Field%	       ! The real word &
\	  fnDisplay.Word$ = D$					   ! Return it &
\	fnend &
	&
	&
	&
	&
\	def* fnDisplay.Byte$(Cursor%, Sub.Field$)     ! Octal byte subfunction &
\	  D$ = '   ' &
\	  D$ = fnOctal.Byte$(ascii(mid(Sub.Field$, Cursor% + 1%, 1%))) &
		if Cursor% < len(Sub.Field$) unless Cursor% < 0% &
\	  fnDisplay.Byte$ = D$ &
\	fnend &

13000	!	First-level subroutine:  retrieve packet from logging file &
!	&
!	&
!		returned:  PMB/packet information reconstructed from &
!			     logging file, in same buffers and format &
!			     as returned by RECEIVE (in the logging &
!			     function): &
!	&
!			   packet, in Packet.NL.Buffer$ &
!			   Current.Packet.Length% &
!			   First.Packet.in.Message% - true or false &
!			   "parameters", in PMB.NL.Buffer$ &
!			      (same data for all packets from the &
!			       message covered by the parameters) &
	&
	&
	&
	&
	&
	First.Packet.in.Message% = 0%		! Presume not a PMB record &
	&
	&
	&

13010	gosub 13100 unless Logging.Buffer.Filled% &
			! Fill the buffer (with a GET) if it's empty &
\	return unless Display.File.Active%	! Exit if EOF reached &
	&
\	field # Logging.File%, &
		Retrieved.Bytes.Scanned% as D0$, &
				      2% as Retrieved.Record.Length$ &
\	Retrieved.Record.Length% = swap%(cvt$%(Retrieved.Record.Length$)) &
					! Get count bytes for next record &
	&
\	if Retrieved.Record.Length$ = Minus.One$ then	! If no more records, &
	  Logging.Buffer.Filled% = 0%			!   indicate empty &
\	  goto 13010					!   buffer, restart &
	&
	&
	&
	&

13020	field # Logging.File%,			! Define next record in buffer &
	      Retrieved.Bytes.Scanned% + 2% as D1$, &
		   Retrieved.Record.Length% as Retrieved.Record$ &
	&
	&
!   Process PMB ("parameters" data) first, if that's up next in buffer &
	&
\	if Retrieved.Record.Length% <= Loggable.PMB.Bytes% then &
	  First.Packet.in.Message% = -1%	! Indicate PMB data is new &
\	  lset PMB.NL.Buffer$ = Retrieved.Record$    ! Retrieve the PMB data &
\	  Retrieved.Bytes.Scanned% = &
		(Retrieved.Bytes.Scanned%	! Advance buffer pointer past &
		+ 2% + Retrieved.Record.Length%	!   data extracted so far, &
		+ 1%) and -2%			!   rounded to next even byte &
	&
\	  goto 13010				! Go look for packet record &

13030	!	Extract a packet record &
	&
	Current.Packet.Length% = Retrieved.Record.Length%   ! Pass count &
\	lset Packet.NL.Buffer$ = Retrieved.Record$	! Pass the record &
\	Retrieved.Bytes.Scanned% = &
		(Retrieved.Bytes.Scanned%	! Advance buffer pointer past &
		+ 2% + Retrieved.Record.Length%	!   data extracted so far, &
		+ 1%) and -2%			!   rounded to next even byte &
	&
\	gosub 16000			! Allow a "packet-selection" routine &
\	goto 13010 unless Packet.Wanted%     !  ..discard un-selected packets &
\	return &
	&
	&
	&
	&
	&
	&
	&
	&
	&

13100	!	Second-level subroutine:  GET a logging-file block &
	&
	on error goto 13110			! Enable trap for EOF &
\	get # Logging.File%			! Try for a(nother) record &
\	Retrieved.Bytes.Scanned% = 0%		! Indicate new bufferful &
\	Logging.Buffer.Filled% = -1%		!  now ready, &
\	goto 13190				!   and go return with it &

13110	!	End of File &
	Display.File.Active% = 0%		! Indicate we're done &
\	goto 19000 if err = 28%			! Handle CTRL/C's &
\	resume 13190 &

13190	on error goto 19000			! Reset standard error trap &
\	return &

14000	!	First-level subroutine:  load Display-Function captions &
	&
	&
	return if Maximum.FQFUN%		! Do initialization only once &
	&
\	T1% = 27%				! Set up &
\	T2% = 36%				! display tab() &
\	T3% = 45%				! columns &
\	T4% = 48% &
	&
\	for I% = 0% to 13%			! Get FIRQB mnemonics, &
\	  read Mnemonic$			!   expanded to a uniform &
\	  FIRQB.Mnemonic$(I%) =			!   2 + six characters + 2 &
		'  ' + Mnemonic$ + space$(8% - len(Mnemonic$)) &
\	next I% &
	&
\	I% = -1% &
\	until Maximum.FQFUN% &
\	  read Mnemonic$, Description$		! Load FQFUN descriptive &
\	  if len(Description$) = 0% then	! captions from DATA &
	    Maximum.FQFUN% = I% &
	  else &
	    I% = I% + 1% &
\	    FQFUN.Mnemonic$(I%) = Mnemonic$ &
\	    FQFUN.Description$(I%) = Description$
14010	  next &
	&
	&
\	I% = 1% &
\	while I% &
\	  read Mnemonic$, Description$ &
\	  if Mnemonic$ = '000' then			! Load UUO descriptive &
	    UUO.Zero.Offset% = I%			! captions from DATA &
	  else						! (remembering which &
	    if len(Description$) = 0% then		! UUO corresponds to &
	      I% = 0%					! zero) &
	    else &
	      UUO.Mnemonic$(I%) = Mnemonic$ &
\	      UUO.Description$(I%) = Description$ &
\	      I% = I% + 1%
14020	next &
	&
\	return &

15000	!	First-level subroutine:  allow selection of packets, or &
!			auxiliary processing, for logging function &
!	&
!	&
!	&
!		supplied:  candidate "next" packet, freshly stripped &
!			   from RECEIVE's returned data, in Packet.NL.Buffer$ &
!			   (FIELD'ed) &
!	&
!			   the "parameters" gotten with the RECEIVE which got &
!			   the current bufferful of packets, in PMB.NL.Buffer$ &
!			   (FIELD'ed) &
!	&
!		to be &
!		returned:  Packet.Wanted%	= -1% if packet wanted &
!						=  0% if packet not wanted &
	&
	&
	&
	Packet.Wanted% = -1%		! Select all packets &
\	return				!   (this is a skeleton routine) &
	&
	&
	&
	&
	&
	&
	&
	&
	&
	&
	&

16000	!	First-level subroutine:  allow selection of packets, &
!			upon retrieval from logging file, for display function &
!	&
!	&
!	&
!		supplied:  candidate "next" packet, freshly retrieved from &
!			   a previously-written logging file, in &
!			   Packet.NL.Buffer$ (FIELD'ed) &
!	&
!			   the "parameters" gotten with the original RECEIVE &
!			   which got the current bufferful of packets, in &
!			   PMB.NL.Buffer$ (FIELD'ed) &
!	&
!		to be &
!		returned:  Packet.Wanted%	= -1% if packet wanted &
!						=  0% if packet not wanted &
	&
	&
	&
	Packet.Wanted% = -1%		! Select all packets &
\	return				!   (this is a skeleton routine) &

19000	on error goto 0				! Clear error handler &
		unless err = 28%		!  unless CTRL/C was typed &
\	resume 19010				! Clear the error flag &

19010	Detached% = 0%				! Clear the Detached flag &
\	gosub 11100				! Go clean up &
\	goto 32766				! And exit &


20000	!	===================  Function Definitions  ================ &
	&
	&
	def* fnGet.Response$(cvt.Mask%)	  ! Get a typed response &
\	  on error goto 32767			! Just quit upon control-Z &
\	  input line D$				! Get the response string &
\	  on error goto 19000			! Reset standard error trap &
\	  fnGet.Response$ = cvt$$(D$, cvt.Mask%)  ! "Clean up" the string &
\	fnend &
	&
	&
\	def* fnYes.No%(Prompt$, Default$)   ! Get a YES/NO response &
\	  print Prompt$; ' ';			! Paint the prompt &
\	  print '<'; Default$; '> ';		! And the default, &
	    if len(Default$)			!   if appropriate &
\	  D$ = fnGet.Response$(-1%)		! Get typed response &
\	  D$ = Default$ unless len(D$)		! Null string <== default &
\	  fnYes.No% = (D$ = left('YES', len(D$)))  ! Create returned value &
\	fnend &
	&
	&
\	def* fnOctal.Word$(Word%)	! Interpret a word in ascii &
\	  D$ = ''				! Initialize work string &
\	  S% = (Word% < 0%)			! Remember sign &
\	  D1% = Word% and 32767%		! Extract bits 14-0 (mantissa) &
\	  for P% = 1% to 5%			! Loop for 5 non-sign digits &
\	    D$ = num1$(D1% and 7%) + D$		!   append (next) digit &
\	    D1% = D1% / 8%			!   shift down mantissa &
\	  next P% &
\	  D$ = '1' + D$ if S%			! 6th digit is sign bit: &
\	  D$ = '0' + D$ unless S%		!   set it as appropriate &
\	  fnOctal.Word$ = D$			! Return developed value &
\	fnend &
	&
	&
\	def* fnOctal.Byte$(Byte%)	! Interpret a byte in ascii &
\	  D$ = ''				! Initialize work string &
\	  D1% = Byte% and 255%			! Extract bits 7-0 (mantissa) &
\	  for P% = 1% to 3%			! Loop for 3 digits &
\	    D$ = num1$(D1% and 7%) + D$		!    append (next digit) &
\	    D1% = D1% / 8%			!    shift down mantissa &
\	  next P% &
\	  fnOctal.Byte$ = D$			! Return developed value &
\	fnend &
	&
	&
\	def* fnP$(V%)			! Interpret an integer in decimal &
\	  fnP$ = num1$(V%) + '.'		! Tack on decimal point &
\	fnend &

32766	!	====================  Final Close-Out  ==================== &
	&
	&
	&
	close I% for I% = 1% to 12%	! Close all channels &
\	V$ = sys(Remove.Receiver$)	! Remove EMT Logger's RIB &
	&
\	if Detached% then		! If running detached, &
	  V$ = sys(Regain.Privilege$)	!   get privileges back one last time, &
\	  V$ = sys(chr$(6%) + chr$(8%)	!     and execute a KILL JOB call &
		+ chr$(Job%)		!     on this job &
		+ string$(23%, 0%) &
		+ chr$(0%) &
		+ chr$(255%) &
		+ cvt%$(0%)) &
	&

32767	end
