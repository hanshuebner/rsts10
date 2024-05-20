only definitions forth
: sdaversion ." SDA V10.1-0G " ;

vocabulary	user			\ user words
only forth definitions
vocabulary	util			\ utility (internal) words
vocabulary	helptext		\ help items that aren't words
vocabulary	funkeys			\ function (escape sequence) keys
vocabulary	-words			\ multi-keyword operators

\ words to set up the search list and current vocab

also user definitions
: >user only util also user definitions ;
: >util only user also util definitions ;
: >keys only util also user also funkeys definitions ;
: >-words only util also user also -words definitions ;

also util definitions
\ the same but with "forth" also in the search order:
: >userf only util also user definitions also forth ;
: >utilf only user also util definitions also forth ;       
: >keysf only util also user also funkeys definitions also forth ;
: >-wordsf only util also user also -words definitions also forth ;

\ ****************************************************************************
\
\ More detail about the use of various vocabularies, and the
\ search order setup words:
\
\ Vocabularies:
\
\ user		words visible to the user of SDA.  this is also the vocabulary
\		where words defined by the SDA user are put.
\ util		words used in SDA that are not directly visible to the user.
\ helptext	entries in this dictionary are not words (not executable
\		FORTH operators).  Instead, they are pointers to help text
\		defined by the "helpitem" word, such as "introduction".
\ funkeys	words accessed by keyboard function keys (escape sequences).
\		the name of the word is the same as the escape sequence
\		that invokes it, except that the <escape> is replaced by
\		a $ sign.  For example, $[28~ is the funkeys word for the
\		help function.
\ -words	words accessed by two-keyword commands.  the search is
\		performed by the "show" word.  (the other two-keyword 
\		command, "use" invokes the same search.)  words in this
\		vocabulary are of the form "show-xyz" or "use-abc" 
\		corresponding to commands "show xyz" or "use abc".  Note
\		that only the first 3 characters of the second keyword
\		are used (therefore the commands must be unique in 
\		3 characters).
\
\ Search order setup words:
\
\ Refer to the FORTH-83 standard, appendix C, for a description of the
\ vocabulary search order model and operators used in SDA (and in general
\ in the RSTS implementation of FORTH).
\
\ Once SDA is actually executing, the search order (set by "maininit")
\ is simply "only user definitions".  In other words, the only vocabulary
\ visible is "user" and definitions go there too.
\
\ During compilation of SDA, the search order is more complex.  In all
\ cases, both the "user" and "util" vocabularies are in the search order,
\ so the internal-only operators are accessible.  There are two groups
\ of four search setup words -- four basic ones and four additional ones
\ whose names end in "f".  
\
\ The search order setup words ending in "f" each set "forth" as the
\ first vocabulary to be searched (last added to the search order).
\ These are used in the first part of SDA, where the usual FORTH
\ environment of "single precision by default, double precision when
\ called for by d-words" applies.
\
\ In the second part of SDA, once we've switched into the SDA environment
\ where all arithmetic is 32-bit, the search order setup words without "f"
\ are used.  This leaves "forth" out of the search order.  The purpose of
\ doing that is to ensure there will be no inadvertent access to any
\ operators that act on 16-bit quantities.  Any word to be used in the
\ second part of FORTH is (with the obvious exceptions) either part of
\ "util" or of "user", and takes and returns stack arguments in 32-bit pieces.
\
\ The four search orders are:
\
\ >user		search user, then util; definitions are made into user.
\ >util		search util, then user; definitions are made into util.
\ >keys		search funkeys, util, user; definitions go into funkeys.
\ >-words	search -words, util, user; definitions go into -words.
\
\ In other words, the essential difference is which of the vocabularies
\ receives new definitions.  For example, a definition for a word that is
\ to be accessible to the SDA user is made in the context of >user
\ (or >userf).
\
\ The search order setup words without "f" are available to the SDA
\ user, though obviously they (especially >keys and >-words) should only
\ be used by those who fully understand their significance.
\
\ ****************************************************************************

\ ****************************************************************************
\
\ More detail on the two parts of SDA, and the 16-bit vs. 32-bit
\ environment:
\
\ Normally in FORTH, stack data comes in 16-bit words, and the basic
\ operators act on and return 16-bit words.  There are additional
\ operators ("d-words" because their names generally begin with "d" or "2")
\ analogous to the basic operators, that act on and return 32-bit values.
\ Finally, numbers are 16-bit values unless they have a trailing ".", in
\ which case they are 32-bit values.
\
\ For SDA, this is not convenient since the SDA user doesn't want to deal
\ with the distinction.  Since 32-bit values are in general needed to be
\ able to address the entire PDP-11 address space (22 bits), SDA uses
\ 32-bit data at all times in the user interface.  In effect, it creates
\ the appearance that you have a version of FORTH where the basic unit of
\ data is 32 bits and the basic operators act on and return 32-bit quantities.
\
\ We have to go through a fair amount of trouble to create this appearance.
\ To ensure that the user doesn't inadvertently operate on half a quantity
\ (i.e., 16 bits), the standard FORTH operators are completely hidden.
\ Instead, all that is visible are the words we've defined here.  A lot of
\ those are SDA commands, but a lot more are the normal FORTH operators,
\ either referred to by their normal name (for those that don't consume
\ or return data, for example "cr") or renamed (for example "d+" in FORTH
\ is renamed to "+").
\
\ In the first part of SDA, compilation is still in the 16-bit environment
\ of FORTH.  This is where we build the 32-bit environment; it is also
\ where we define operators that need to manipulate 16-bit quantities
\ directly.  (Sometimes the placement is somewhat arbitrary or is controlled
\ by "define before use" requirements; we're not particularly dogmatic
\ about what goes where.)
\
\ In the second part of SDA, compilation is in the 32-bit environment, which
\ at this point has been fully constructed.  Definitions here look just
\ like those an SDA use might write (except that we can reference internal
\ words, those in the "util" vocabulary).
\
\ Additions to FORTH should in general be made in the second part.  The
\ first part is harder to work in, and should not be tinkered with except
\ by those who have plenty of experience with the internals of SDA and with
\ FORTH in general.
\
\ ****************************************************************************

\ ****************************************************************************
\
\	I/O channel use:
\
\ The following are fixed channel assignments:
\
\  1. SIL file
\  2. Crash dump file
\  3. SDAPFX.DEF file (for use in DEFINE command)
\  14. User terminal (FORTH.RTS standard)
\  15. SDA.FTH file (source file, for access to HELP text)
\
\ Channels 13 through 4 (in that order) are used for the FLOAD command.
\ The first FLOAD uses channel 13; any nested FLOAD command use the next
\ lower channel number.
\
\ During compilation of SDA, channel 1 is used to read the SDAPFX.DEF file.
\
\ ****************************************************************************

\ handy debug words
>userf

\ print stack contents, from base to top
: .s sp@ s0 @ swap 2dup -
  if ?cr do i @ . 2 /loop ?cr
  else 2drop
  endif ;

\ print current radix
: .b base @ dup decimal . base ! ;
          
\ print line containing error
: .e linebuf 1 enclose drop over -	\ find null, get length
  rot rot + swap type cr ;		\ find start, type the line
  
\ variables go into "forth" vocabulary for safety

only forth definitions

0 variable start-prefix			\ start of prefix symbols
0 variable start-global			\ start of global symbols
0 variable end-global			\ end of global symbols
0 variable symbol-link			\ pointer to link of last
					\ defined user symbol
0 variable	mscbuf	254 allot	\ Buffer copy
0 variable	jdbbuf	30 allot	\ Copy of JDB
0 variable	jcrbuf  254 allot	\ Copy of JCR, etc.
0 variable	ddbbuf	254 allot	\ Copy of DDB
0 variable	silmod	30 allot	\ Sil module entry
0 variable	modules			\ Count of modules in the SIL
0 variable	sym-pointer		\ Pointer for symbol lookup
0 variable	stb-pointer		\ Index into the STB
0 variable	segment			\ Starting block of crash segment
0 variable	mem-flag		\ flag for accessing memory
0 variable	sil-flag		\ flag for accessing sil
1 variable	segcnt			\ number of segments in dump
1 variable	segtbl	30 allot	\ starting vbn for 16 segments
0 variable	modnum			\ current module number
0 variable	wide-list		\ 132-column mode flag
0 variable	idflag			\ I/D space flag (1 if ID in use)
0 variable	id-offset		\ ID or noID offset in crash buffer
0 variable	umr-offset		\ UMR or noUMR offset in crash buffer
0 variable	sid-offset		\ super-mode offset in crash buffer
0 variable	help-flag		\ 0 if no help available, 1 if it is
0 variable	helpfile 30 allot	\ firqb for opening help file
0 variable	pfx-flag		\ 0 if SDAPFX.DEF file is unavailable
0 variable	pfxfile 30 allot	\ firqb for opening SDAPFX.DEF file
0 variable	cclflag			\ ccl entry flag
0 variable	follow			\ CFA of current structure list routine
0 variable	listflag		\ for printing comma-separated lists
8 variable	mainbase		\ user's radix for trap handling
-1 variable	mainmodnum		\ user's module number, ditto

0. 2variable	bottbl	60 allot	\ bottom phy address of 16 segments
0. 2variable	lentbl	60 allot	\ segment length of 16 segments
0. 2variable	fipbot			\ base virtual address of fip pool
0. 2variable	pos			\ currently open location
0. 2variable	len			\ length of last open
0. 2variable	quan			\ address of next structure in list
0. 2variable	sildev			\ device name of currently open SIL

util
>utilf
\ hold a rad50 character
\ n ==> n/50
: %# 0 40 m/ swap "  ABCDEFGHIJKLMNOPQRSTUVWXYZ$.?0123456789"
  drop + c@ hold ;

>userf

: 2%. <# %# %# %# swap %# %# %# #> type ;

>utilf

\ ****************************************************************************
\
\	Set up permanent symbol tables
\
\ ****************************************************************************

\ ****************************************************************************
\
\	Symbol table structure
\
\ To save space, the symbol tables for prefix (common) and global symbols
\ we need is not constructed as a standard FORTH vocabulary.  Instead,
\ the symbols and values are stored in the most compact form available:
\ RAD50 symbol names, and a 16-bit value.
\
\ There are three parts:
\	1. Prefix symbols (resolved at compile time from SDAPFX.DEF)
\	2. Global symbols (resolved when the SIL is opened)
\	3. User-defined symbols (resolved when defined by the DEFINE command)
\
\ The first two are the "built-in" symbols -- ones needed by SDA to function,
\ or so commonly needed by the user that we predefine them.  The user-defined
\ symbols are ones not built-in that the user wanted and specified in a
\ DEFINE command.
\
\ Tables 1 and 2 are allocated, in that order, as contiguous entries.  Each
\ entry contains a 32-bit RAD50 symbol name followed by a 16-bit symbol value.
\ Entries are packed together one after the other.  Pointers indicate where
\ each table starts and ends:
\	start-prefix	points to the start of table 1
\	start-global	points to the end of table 1, start of table 2
\	end-global	points to the end of table 2, listhead for table 3
\
\ The user-defined symbols are in a linked list.  The listhead immediately
\ follows the built-in global symbol table (i.e., the listhead is pointed
\ to by "end-global").  Each entry in the user-defined symbol table consists
\ of a 32-bit RAD50 symbol name, a 16-bit value, and a 16-bit link and flag.
\ The low order bit of the link/flag word is zero for global symbols (i.e.,
\ symbols resolved from the symbol table of the SIL) and one for prefix
\ symbols (symbols resolved from SDAPFX.DEF).  The rest of the link/flag
\ word is a pointer to the next entry in the user table.  The last entry
\ contains a link/flag word with a link value of zero (i.e., the word contains
\ either 0 or 1).
\
\ When a new SIL is opened, all global symbols are zeroed and then reloaded
\ from the RSTS module symbol table (both built-in and user-defined globals).
\ If any symbols are undefined in the new SIL, their value is left as zero.
\
\ ****************************************************************************

\ generate prefix symbol table from sdapfx.def file
\ ==>
: readdefs 1 fileclose drop		\ ensure channel is closed
  1 fileopen forth:sdapfx.def -dup	\ open the file
  if ." Can't open FORTH:SDAPFX.DEF" cr .err \ abort
  endif
  base @ file @ blk @ line @ in @	\ save our context
  octal 1 file ! 1 blk ! 0 line !	\ initialize radix, file pointers
  start-global @ start-prefix @ do	\ scan the entire prefix area
   begin 0 in ! linebuf getline		\ read a line of input
    dup 1+ 0=				\ check for EOF
    if ." Can't find " i 2@ 2%. ."  reading SDAPFX.DEF" cr quit endif
					\ get out on EOF
    -dup				\ make a copy of count
    if + bl swap c!			\ if not null, insert trailing blank
     [compile] 2%			\ pick up symbol name
     i @ i 2+ @				\ and name we're looking for          
     d=					\ set flag if we found it
    else drop 0				\ if null, say to look for more
    endif
   until				\ loop until we find the match
   -1 word here number drop i 4 + !	\ pick up and store value
   6 /loop				\ and loop for all the symbols
  in ! line ! blk ! file ! base !	\ restore context
  1 fileclose drop			\ close the definition file
  linebuf 80 erase ;			\ wipe out remnants of input

\ define global symbol names
: global [compile] 2% swap , , 0 , ;	\ compile the name and the value

: prefix global ;

\ *********************************************************************
\ set up the prefix symbol table -- entries in ASCII alphabetical order

here start-prefix !			\ symbols start here

prefix	$$$ver
prefix	.clear
prefix	.fss
prefix	.mesag
prefix	.name
prefix	.postn
prefix	.stat
prefix	.xpeek
prefix	adjvir
prefix	af.are
prefix	af.end
prefix	af.ini
prefix	af.ph4
prefix	bufhdr
prefix	c.lcid
prefix	c.len
prefix	c.rcid
prefix	ch$nxt
prefix	ch$par
prefix	ch$sec
prefix	corcmn
prefix	date
prefix	ddcnt
prefix	ddcons
prefix	ddflag
prefix	ddidx
prefix	ddjbno
prefix	ddnfs
prefix	ddsts
prefix	ddunt
prefix	ddwlo
prefix	ec.adr
prefix	ec.hop
prefix	ec.nxt
prefix	ec.siz
prefix	enccur
prefix	encidl
prefix	encold
prefix	f$acnt
prefix	f$clus
prefix	f$nam
prefix	f$ppn
prefix	f$prot
prefix	f$rcnt
prefix	f$sizl
prefix	f$sizm
prefix	f$unt
prefix	f$wcb
prefix	firqb
prefix	fqbsiz
prefix	fqdev
prefix	fqdevn
prefix	fqerno
prefix	fqext
prefix	fqfil
prefix	fqflag
prefix	fqmode
prefix	fqnam1
prefix	fqnent
prefix	fqpflg
prefix	fqppn
prefix	fqsiz
prefix	fqsizm
prefix	j2ppn
prefix	jcbrst
prefix	jccpu
prefix	jccpum
prefix	jchdrs
prefix	jcname
prefix	jcpfb
prefix	jcpri
prefix	jcsizm
prefix	jcswap
prefix	jdflg
prefix	jdiob
prefix	jdiost
prefix	jdjbno
prefix	jdjdb2
prefix	jdmctl
prefix	jdrts
prefix	jdsize
prefix	jdwork
prefix	jfpriv
prefix	jfsys
prefix	jfsyst
prefix	jm.sis
prefix	jm.uds
prefix	job
prefix	js.kb
prefix	jsbuf
prefix	jsfip
prefix	jstel
prefix	jstim
prefix	key
prefix	l.cost
prefix	l.ddep
prefix	l.flag
prefix	l.ppn
prefix	l.prot
prefix	l.stat
prefix	lck
prefix	lf.aen
prefix	lf.ans
prefix	lf.bro
prefix	lf.con
prefix	lf.des
prefix	lf.fip
prefix	lf.lsm
prefix	lf.lss
prefix	lf.rst
prefix	lf.rtm
prefix	lf.rvm
prefix	lf.srm
prefix	lf.sta
prefix	lf.tra
prefix	lf.ver
prefix	lf.vmw
prefix	ll.lfl
prefix	ll.lla
prefix	ll.llb
prefix	ll.llx
prefix	ll.mod
prefix	ll.nob
prefix	ll.rib
prefix	ll.rla
prefix	ll.sta
prefix	ll.try
prefix	ll.ula
prefix	ls.dyn
prefix	ls.lib
prefix	lx.ldr
prefix	lx.lir
prefix	lx.rdr
prefix	lx.rir
prefix	m.ctrl
prefix	m.phya
prefix	m.pnxt
prefix	m.size
prefix	m.tsiz
prefix	mc.lck
prefix	mcbsiz
prefix	mosupi
prefix	mousrd
prefix	n.addr
prefix	n.ddb
prefix	n.dely
prefix	n.flgs
prefix	n.lcnt
prefix	n.link
prefix	n.lllb
prefix	n.name
prefix	n.nhop
prefix	next
prefix	nf.del
prefix	nf.end
prefix	nf.ibp
prefix	nf.lop
prefix	nf.lup
prefix	nf.obp
prefix	oajvir
prefix	oth.sz
prefix	otm.sz
prefix	out
prefix	p$bufa
prefix	pf.1us
prefix	pf.csz
prefix	pf.emt
prefix	pf.kbm
prefix	pf.ner
prefix	pf.rem
prefix	pf.rw
prefix	pf.sla
prefix	pf.wcb
prefix	r.cnt
prefix	r.data
prefix	r.dext
prefix	r.flag
prefix	r.link
prefix	r.mctl
prefix	r.name
prefix	r.size
prefix	rf.p4p
prefix	rf.ph4
prefix	s.accs
prefix	s.jbno
prefix	s.lcnt
prefix	s.link
prefix	s.lmax
prefix	s.mcnt
prefix	s.mmax
prefix	s.objt
prefix	s.omax
prefix	s.rcid
prefix	s.srbn
prefix	sa.1sh
prefix	sa.evt
prefix	sa.lcl
prefix	sa.ncs
prefix	sa.net
prefix	sa.prv
prefix	se.blk
prefix	se.idn
prefix	se.len
prefix	se.lod
prefix	se.nam
prefix	se.off
prefix	se.siz
prefix	se.stb
prefix	se.stn
prefix	se.xfr
prefix	si.ent
prefix	swp
prefix	sysvee
prefix	sysvel
prefix	time
prefix	ttintf
prefix	ttmodm
prefix	uc.cnt
prefix	uc.dlw
prefix	uc.lck
prefix	uc.mnt
prefix	uc.nfs
prefix	uc.pri
prefix	uc.wlo
prefix	uent
prefix	uo.cln
prefix	uo.dp
prefix	uo.ini
prefix	uo.ncd
prefix	uo.ncf
prefix	uo.nqt
prefix	uo.top
prefix	uo.wcf
prefix	uo.wcu
prefix	uu.chk
prefix	uu.cnv
prefix	uu.trm
prefix	w$fcb
prefix	w$flag
prefix	w$idx
prefix	w$jbno
prefix	w$nvbl
prefix	w$nvbm
prefix	w$wcb
prefix	wc$aex
prefix	wc$che
prefix	wc$csq
prefix	wc$ctg
prefix	wc$dlw
prefix	wc$ext
prefix	wc$lck
prefix	wc$llk
prefix	wc$nfc
prefix	wc$rr
prefix	wc$spu
prefix	wc$ufd
prefix	wc$upd
prefix	xc$22
prefix	xc$cac
prefix	xc$cis
prefix	xc$dsp
prefix	xc$ecc
prefix	xc$fis
prefix	xc$fpp
prefix	xc$ids
prefix	xc$nem
prefix	xc$oat
prefix	xc$pam
prefix	xc$qbu
prefix	xc$umr
prefix	xrb
prefix	xrbc
prefix	xrblk
prefix	xrblkm
prefix	xrbsiz
prefix	xrci
prefix	xrlen
prefix	xrloc
prefix	xrtime

here start-global !			\ prefix symbols end here

\ fill in the global symbol table -- entries in RAD50 (!!) alphabetical order

global	adjadr
global	adjcir
global	adjend
global	adjflg
global	adjsiz
global	aptap5
global	axdap5
global	bbrap5
global	bravir
global	ccllst
global	chenue
global	craap5
global	crasav
global	csrtbl
global	csr.dv
global	ddctbl
global	defkbm
global	devclu
global	devcnt
global	devnam
global	devokb
global	devptr
global	devsyn
global	dhpap5
global	dlpap5
global	dskap5
global	dsklog
global	dzpap5
global	emtap5
global	em2ap5
global	endche
global	erlctl
global	evlap5
global	extpol
global	fcblst
global	fibuf
global	fijob
global	fipap6
global	fipfun
global	fipovr
global	fippol
global	fipvbn
global	fmsap5
global	fplap6
global	frees
global	genap5
global	grdap5
global	idlcbf
global	idlnob
global	jbstat
global	jbwait
global	jcmde
global	jcmflg
global	jcrsiz
global	jobcnt
global	jobtbl
global	jsbtbl
global	kbdap5
global	kinap5
global	latap5                               
global	latctl
global	liblst
global	llamsk
global	lltbuf
global	lltend
global	lltvir
global	lmtcnt
global	lrgpol
global	maxadr
global	maxlin
global	maxnod
global	maxunt
global	maxun2
global	mcpap5
global	memlst
global	memsiz
global	monpol
global	mvrap5
global	myarea
global	nodap5
global	nodlst
global	nspap5
global	nsplst
global	nulrts
global	oajmmu
global	opnap5
global	ovbase
global	ovrbuf
global	ovrtbl
global	pcpap5
global	pipap5
global	pi2ap5
global	pkdap5
global	prmlst
global	ptbap6
global	pzpap5
global	rdmap5
global	rjdap5
global	rsxap5
global	rteadr
global	rtslst
global	satctl
global	satctm
global	satmmu
global	sesap5
global	sndlst
global	systak
global	sysunt
global	tblend
global	tblext
global	terap6
global	trnap5
global	ttyhct
global	ucttbl
global	untclu
global	untcnt
global	unterr
global	untlvl
global	untopt
global	untsiz
global	vhpap5
global	xcdap5
global	xddap5
global	xedap5
global	xhdap5
global	xkdap5
global	xmdap5
global	xtpap5
global	xtrap5
global	x.con
global	x.mons
global	x.scs
global	x.tab
global	x.tabs
global	$$chen
global	$$jcr6

here end-global !			\ globals end here
here symbol-link !			\ listhead of linked user symbols here
0 ,					\ this is that listhead

readdefs				\ read in SDAPFX.DEF

\ ****************************************************************************
\
\	low level words for the sda interpreter environment
\
\ ****************************************************************************

\ defining words for code

: code create base @ [ latest pfa ] literal octal ;

: c; decimal ' code ?pairs		\ check for matching "code"
  base ! smudge ;			\ if ok, reset base and make word real

octal
: next, 12403 , 133 , ;			\ go to next thread
decimal

\ push a value -- like LIT but one level of indirection
\ used in compiling prefix/global symbol references
\ ==> value
code ref	13445 , 		\ mov @(ip)+,-(s)
		next, c;

code 2ref	13445 ,			\ mov @(ip)+,-(s)
		5045 ,			\ clr -(s)
		next, c;

\ a few code words that work like REF but return buffer addresses
\ given that the symbol is the buffer offset
code (jdb)	13445 ,			\ mov @(ip)+,-(s)
		62715 , jdbbuf ,	\ add #jdbbuf,(s)
		5045 ,			\ clr -(s)
		next, c;

code (jcr)	13445 ,			\ mov @(ip)+,-(s)
		62715 , jcrbuf ,	\ add #jcrbuf,(s)
		5045 ,			\ clr -(s)
		next, c;

code (buf)	13445 ,			\ mov @(ip)+,-(s)
		62715 , mscbuf ,	\ add #mscbuf,(s)
		5045 ,			\ clr -(s)
		next, c;

code (ddb)	13445 ,			\ mov @(ip)+,-(s)
		62715 , ddbbuf ,	\ add #ddbbuf,(s)
		5045 ,			\ clr -(s)
		next, c;

\ make word just defined a synonym of another word (where that
\ existing word was defined using :
\ pfa ==>
: :synonym
  [ latest pfa cfa @ ] literal -	\ see if it's defined using :
  0 ?error				\ if not, then out of luck
  , ;code				\ otherwise store the pfa
  octal	10446 ,				\ mov ip,-(rp)
	11304 ,				\ mov (w),ip
	next, decimal

\ compile help text pointers and set up cfa for synonym
\ ==>
: helpptr blk @ , line @ , ;code
  octal	10446 ,				\ mov ip,-(rp)
	22323 ,				\ cmp (w)+,(w)+
	11304 ,				\ mov (w),ip
	next, decimal

\ define a word, with help text
: :h [compile] : helpptr ;code
  octal 10446 ,				\ mov ip,-(rp)
	22323 ,				\ cmp (w)+,(w)+
	10304 ,				\ mov w,ip
	next, decimal

\ define a synonym of a word that's defined either by : or in code
\ pfa ==>
: synonym dup cfa @ 2dup =		\ check for being in code
  if drop latest pfa cfa !		\ if so point new cfa to old code
  else :synonym				\ else it should be a : definition
  endif ;

\ multiway conditional branch words

only forth definitions			\ these go into forth vocabulary
					\ to avoid conflict with later
					\ redefinition

: cond
	0 compile dup ;		immediate

: <<
	1+ [compile] if
	compile drop ;		immediate

: >>
	[compile] else compile dup
	rot ;			immediate

: nocond
	minus compile 2drop ;	immediate

: condend
	dup 0>=
	if
		[compile] nocond
	endif
	minus 0 do
		[compile] endif
	loop ;			immediate

util >utilf

\ ****************************************************************************
\
\	symbol table management
\
\ ****************************************************************************

\ functions for handling the prefix/global symbol table

\ get one rad50 character, accumulate it
\ n address ==> n*40+x address+1
: (%c) swap over c@
  dup ascii $ = if drop 27 else
  dup ascii . = if drop 28 else
  dup bl      = if drop 0  else
  dup 36 digit  if swap drop dup 10 < if 30 + else 9 - endif
  else drop 1 dpl ! 0 endif endif endif endif
  swap 40 * + swap 1+ ;

\ convert the symbol name at HERE to doubleword rad50 value
\ dpl is left zero if the name is valid, 1 otherwise
\ ==> d-symbol
: getsym 0 dpl !
  0 here 1+ (%c) (%c) (%c)		\ do first 3 characters
  0 swap (%c) (%c) (%c)			\ and second three
  begin 0 swap (%c) swap 0=		\ scan redundant characters
  until					\ exit on blank or invalid
  drop ;				\ drop char pointer, leave

\ scan the prefix symbol table, the built-in global table, and
\ the user-defined global table for a symbol name match
\ d-symbol ==> value-address true	if found
\ d-symbol ==> false			if not found
: findsym over % 0 u<			\ check for leading digit
  if 2>r start-prefix @ dup		\ if not, then we should search
   begin 
    while dup 2@ 2r d- or		\ check for match
     if 6 +				\ bump pointer if mismatch
      dup end-global @ u>=		\ are we in user-defined symbols?
      if @ -2 and			\ yes, follow link word
      endif     
      dup				\ and another copy for the endtest
     else				\ if match
      4 + 1				\ return value address, true
      0					\ and false to exit loop
     endif
   repeat 2r> 2drop
  else 2drop 0				\ begins with a digit, can't match
  endif ;

\ ditto, but scan only the user-define global table
\ d-symbol ==> value-address true	if found
\ d-symbol ==> false			if not found
: finduser over % 0 u<			\ check for no leading digit 
  if 2>r end-global @ @ dup		\ if not, then we should search
   begin 
    while dup 2@ 2r d- or		\ check for match
     if 6 + @ -2 and			\ follow link word if no match
      dup				\ and another copy for the endtest
     else				\ if match
      4 + 1				\ return value address, true
      0					\ and false to exit loop
     endif
   repeat 2r> 2drop
  else 2drop 0				\ begins with a digit, can't match
  endif ;

\ check if the string at HERE matches a symbol
\ ==> value-address true		if found
\ ==> false				if not found
: symbol getsym dpl @			\ get symbol, check valid flag
  if 2drop 0				\ no match if invalid
  else findsym				\ otherwise look for it
  endif ;

\ define new word as a forward reference
\ forward: name
: forward: ?exec create smudge ' abort synonym ;

\ resolve a forward reference (updates execution pointer in the forward
\ word; can be used more than once)
\ resolve: name
: resolve: ?exec !csp [compile] '	\ find the forward definition
  here swap !				\ update the pfa to point here
  [compile] ]				\ enter compilation state
  smudge ;				\ smudge previous word so ; will undo

\ ****************************************************************************
\
\ DECwindows definitions
\
\ ****************************************************************************

>utilf

\ 1remote - send a single word to the remote system
\ defining word; syntax is <data> 1remote <word>
: 1remote <builds ,	\ compile time, pick up word on stack
  does> 1 rembuf ;	\ run time, set length and send it

0 1remote start-popup	\ start-popup - indicates title bar data follows

1 1remote begin-popup	\ begin-popup - indicates popup data follows

2 1remote end-popup	\ end-popup - revert to normal output (function 2)

3 1remote start-error	\ start-error - signal that this is the start of an
			\ error message (function 3)

4 1remote end-error	\ end-error - signal end of error (function 4)

\ Note: message 5 is specify state - see mainloop for details

: send-query		\ Request specific data - message 6
  cswap			\ get request in the high byte
  6 +			\ add the function code
  1 remote ;		\ now send the message

7 1remote dismiss-modal	\ Dismiss the current modal dialog box (function 7)

: local: ?remote 0= if ." :" cr cr endif ;

\ ****************************************************************************
\
\	error handling words
\
\ ****************************************************************************

\ define needed forward references

>userf
forward: mainloop			\ main loop of sda
>utilf
forward: inumber			\ number interpreter/compiler
forward: isymbol			\ symbol interpreter/compiler
forward: rmod				\ sil module header reader

\ interrupt handler, used for control/c and various aborts such as
\ internally detected errors

: interrupt ' mainloop cfa 'interrupt !	\ temporary, in case of trouble here
  mainbase @ base !			\ restore user's radix
  mainmodnum @ dup 0>=			\ check if module number was set yet
  if rmod				\ if so, read user's selected module
  else drop
  endif
  mainloop ;				\ and back to main processing

\ same as usual error, but go to our interrupt handler
: sdaerror
  start-error
  restore ?cr here count type
  ."  ? " message sp! 
  end-error
  interrupt ;

: ?sdaerror swap if sdaerror endif drop ;
: ?sdastack s0 @ 2 - sp@ u< 1 ?sdaerror
  sp@ here 128 + u< 2 ?sdaerror ;

\ compile-time checkers, same as usual but abort to interrupt not to quit
: ?pairs swap over -			\ matching brackets?
  if					\ if so:
    start-error				\ start error mode
    restore ?cr here count type		\ print offending token
    ."  ?Missing "			\ and the message
    nfa id. 				\ now print what we missed
    end-error
    interrupt
  endif
  drop ;
: ?comp state @ 0= 17 ?sdaerror ;
: ?exec state @ 18 ?sdaerror ;
: ?csp sp@ csp @ - 20 ?sdaerror ;

\ print an error message, then abort
: (error")
  start-error
  r count dup
  1+ =cells r> + >r			\ fix up return stack
  ?cr type 				\ print the error
  end-error interrupt ;			\ and we're done

\ ditto but conditional
: (?error") r count dup
  1+ =cells r> + >r rot			\ fix up return stack
  if                                    \ if error:
   start-error				\ set error mode
   ?cr type                             \ print it out
   end-error interrupt			\ end error mode and quit
  else 2drop				\ else drop string pointer
  endif ;

\ compiling words for error messages, compiletime only

: error" ?comp compile (error")		\ check state, compile code
  ascii " word				\ get string
  here c@ 1+ =cells allot ;  immediate	\  and compile it

forth definitions	\ next operator doesn't work in 32-bit mode

: ?error" ?comp compile (?error")	\ check state, compile code
  ascii " word				\ get string
  here c@ 1+ =cells allot ;  immediate	\  and compile it

>utilf

\ similar to resolve: but simply sets the execution pointer
\ pfa ==>
: is [compile] ' state @		\ get pfa, check state
  if compile !				\ if compiling
  else !				\ if executing
  endif ;			immediate

\ define new word to be a synonym for an existing one
\ := new old
: := ?exec create -find 0= 0 ?error	\ find the old word
  smudge synonym ;			\ make the synonym

\ likewise but with help text
: :=h ?exec create -find 0= 0 ?error	\ find the old word
  smudge helpptr , ;			\ make the synonym

: helpitem current @ context @		\ save search info
  helptext definitions create smudge	\ create the header
  helpptr ' abort , 			\ set cfa for help searcher to see
  context ! current ! ;			\ and restore search pointers

\ ****************************************************************************
\
\	number processing
\                                                      
\ ****************************************************************************

>utilf
\ test for oneshot-radix operators ^o, ^x, ^d, ^b
\ the string-pointers passed are biased by -1 as if a count byte
\ is present
\ string-address ==> perm-radix updated-string-address
: ^radix 1+ dup c@ ascii ^ =
  if 1+ dup c@
   dup ascii B = if drop 2 else
   dup ascii O = if drop 8 else
   dup ascii D = if drop 10 else
   dup ascii X = if drop 16 else
		    drop 2- base @
   endif endif endif endif
  else 1- base @
  endif base @ swap base ! swap ;

\ convert a number with optional leading radix operator
\ address ==> d-number
: ^number ^radix 0. rot
  dup 1+ c@ ascii - = dup >r + -1	\ check for sign
  begin dpl ! (number)			\ convert the number
   dup c@ bl -				\ check for end of number
   while dup c@ ascii . -		\ not end, check for "."
    if 2drop 2drop base ! 0 sdaerror	\ restore base, issue error
    endif 0				\ was ".", flag that
  repeat
  drop r> if dminus endif		\ drop address, process "-" flag
  rot base ! ;				\ restore base, leave

\ accept number with radix operator, always treat as double,
\ default to octal if no trailing ".", decimal if trailing "."
\ address ==> d-number                                 
: ^number. >r base @ r decimal ^number	\ try decimal first
  dpl @ -1 =			\ check for decimal point
  if 2drop r octal ^number	\ if not, try octal
  endif r> drop rot base ! ;	\ get rid of address, restore radix

helpitem numbers
\ SDA interprets numbers in roughly the same way as the MACRO-11 assembler.
\ Since many of its operations require 32-bit values, all numbers in
\ SDA are interpreted as 32-bit values.
\
\ The default radix is octal.  Decimal numbers may be specified by entering
\ the number followed by a decimal point.  Numbers may also be preceded by
\ an explicit radix indicator:
\	^B	binary
\	^O	octal
\	^D	decimal
\	^X	hexadecimal
\ These may be specified either in uppercase or lowercase.  Hex "digits"
\ A-F may also be specified in either case.
\ A leading minus sign is always allowed (leading plus is not).  If both a
\ minus sign and a radix indicator are used, the sign must follow the
\ radix operator.

: s-inumber ^number dpl @ 1+	\ scan number, check "double" flag
  if [compile] dliteral
  else drop [compile] literal
  endif ;
: d-inumber ^number. [compile] dliteral ;

: s-isymbol state @		\ check state
  if compile ref ,		\ if compiling
  else @			\ if executing
  endif ;                                              
: d-isymbol state @		\ check state
  if compile 2ref ,		\ if compiling
  else @ 0			\ if executing
  endif ;

\ define a word to help find multi-keyword command operators
\ this word works just like -find but it prefixes the word currently
\ stored at "here" plus a "-" on the name it looks for.  this is used
\ to parse multi-keyword operator names such as "show memory".
\ ==> pfa 1	(if found)
\ ==> 0		(if not found)
: -2find here dup >r		\ save "here"
  c@ 1+ allot			\ temporarily allot what we have so far
  -1 word here count upper	\ get next name, upcase it
  here c@ here over + c@ 0= -	\ pick up length, fix it if zero
  3 min				\ truncate to 3 characters
  ascii - here c! r> dp !	\ put hyphen in, reset "here"
  here c@ + 1+ here		\ find new total length
  2dup + bl swap 1+ c!		\ put in a blank after 3rd character
  c! only -words also		\ search different vocabulary
  (-find) only user also ;	\ search for match, then reset search order

\ now define the first word of multi-keyword operators

>userf
:h show
\ The SHOW operator is used to show a variety of information.
\ In many cases, the SHOW command takes the same or a similar form
\ as in DCL.  SHOW is followed by a second keyword, which indicates
\ what is to be shown.  Unlike most SDA operators, the second keyword
\ of a SHOW command may be abbreviated to as little as 3 characters.
\ For example, the following commands are equivalent:
\	SHOW SYMBOLTABLE       
\	SHOW SYM                                       
\ 
\ Additional information is available for:
\	ADJACENCIES	CRASH		RUNTIMESYSTEMS
\	LIBRARIES	USERS		JOB
\	SYMBOLTABLE	DISKS		CIRCUITS
\	NODES		RECEIVERS	LINKS
\	ROUTES		SIL		SMALL_BUFFERS
\	CONFIGURATION	XBUF		TAGS
\	FILES		REGISTERS	VERSION
\	OVERLAYS	MMU		UMR
\	COMMANDS	KNOWN_SYMBOLS
  -2find 0= 0 ?sdaerror		\ search for it, error if not found
  cfa				\ point to code field
  state @			\ see if compiling
  if ,				\ if so, compile it
  else execute			\ otherwise execute
  endif ; immediate		\ and all set now

>-wordsf
:h show-mmu [compile] show ; immediate
\ The SHOW MMU operator displays the memory management registers as saved
\ in the crash dump.  This command is only valid when
\ you're looking at a crash dump file, not when you're looking
\ at current memory.
\ You can display all MMU registers, or just the ones for one of the
\ three modes:
\	SHOW MMU ALL		display all MMU registers
\	SHOW MMU <return>	same as SHOW MMU ALL
\	SHOW MMU KERNEL		display MMU CSRs and Kernel APRs
\	SHOW MMU SUPERVISOR	display MMU CSRs and Supervisor APRs
\	SHOW MMU USER		display MMU CSRs and User APRs
\ The keywords KERNEL, SUPERVISOR, and USER may be abbreviated to 3 characters.

>utilf
helpitem symbols
\ SDA has built into its symbol table many of the symbols from COMMON,
\ KERNEL, FLDEF, INIPFX, PFBDEF, and NETDEF.  You can reference these
\ symbols simply by using the name (case insensitive).  Since SDA treats
\ all numeric values as 32-bit data, the built-in symbols are extended with
\ a MSB of zero.  In other words, they are treated as unsigned 16-bit
\ values.
\ In addition, a number of global symbols from the SIL symbol table
\ are accessible in the same manner.
\ If you attempt to use a symbol that is not known to SDA, you will get an
\ error message similar to:
\	<symbol> ?
\ You can use the DEFINE command to have SDA define additional global
\ symbols.
\ The SDA command SHOW SYMBOLS can be used to display the SIL symbol table
\ for the currently selected module of the SIL.  SHOW KNOWN_SYMBOLS
\ displays the symbols that are already known to SDA.

: single ' s-inumber is inumber ' s-isymbol is isymbol ;
: double ' d-inumber is inumber ' d-isymbol is isymbol ;

\ defining word for referencing prefix symbol table symbol names
\ that conflict with Forth word names (like "OUT")
\ similar to ' but looks in the prefix table, generating a symbol value
\ reference if found and an error if not found.
\ ==>		(compiling)
\ ==> value	(executing)
: common -1 word symbol 0= 0 ?sdaerror isymbol ;	immediate

\ ****************************************************************************
\
\	main interpretation loop
\
\ ****************************************************************************

single				\ initially use single length values

\ interpretation loop for sda.  similar to the usual one but
\ includes an additional search of the prefix/global table
: sdainterpret
  begin -find				\ look for forth word
  if dup lfa @ 1 and state @ <		\ check immediate or not compiling
    if cfa ,				\ compile if not
    else cfa execute			\ else execute the word
    endif
   else symbol				\ next try prefix/global table
    if isymbol				\ if found, do compile/execute
    else here inumber			\ else compile/push a number
    endif
   endif                                               
   ?sdastack				\ check stack underflow
  again ;				\  and do this forever

\ define inner loop of fload function
: floadloop
  begin 0 in ! linebuf getline >r drop	\ get line, save eof flag
   r minus 0<				\ if not eof
   if sdainterpret			\  interpret the line (our way)
   endif r> 0<
  until linebuf 128 erase ;		\ until done, then zap the line

\ use our interpreters from this point onwards:
floadloop

>userf

\ standard "clear" routines

: clrfqb firqb fqbsiz erase ;
: clrxrb xrb xrbsiz erase ;

>utilf
\ issue .xpeek
\ ==>
code (xpeek) .xpeek , next, c;

\ issue .mesag
\ ==>
code mesag .mesag , next, c;

\ issue .clear
\ ==>
code (clear) .clear , next, c;

\ issue .fss
\ ==>
code (fss) .fss , next, c;                             

\ ditto, with arguments
\ addr length ==> status
: fss clrxrb dup xrlen xrb + !
  xrbc xrb + !
  xrloc xrb + ! (fss) firqb c@ ;

\ issue .postn
code (postn) .postn , next, c;

\ check whether channel is close
\ channel ==> 0		if open
\ channel ==> non-0	if closed
: ?closed clrxrb 2* xrci xrb + c! (postn) firqb c@ ;

\ check if a SIL has been opened
\ ==>
: ?sil 1 ?closed			\ see if sil channel not open yet
  ?error" ?Please select a SIL first" ;	\ error if closed

\ check if a crash file has been opened
\ ==>
: ?crash 2 ?closed			\ see if crash file channel not open
  ?error" ?Please select a crash file first" ;	\ error if closed

: +f firqb + ;

\ display the current version of SDA and the SDAPFX.DEF we're built with
\ ==>
: version sdaversion ." for RSTS/E V" sysvel emit sysvel cswap emit
  sysvee emit sysvee cswap emit ascii - emit $$$ver emit $$$ver cswap emit ;

>userf                                                 
\ define our flavor of fload
: fload filename _sy:.fth drop		\ parse the default filespec
  -1 word here count fss		\ then parse the user's filespec
  ?error" ?Invalid file name"
  file @ 1- dup 3 =			\ decrement file number, check nesting
  11 ?sdaerror				\ error if too deep
  dup file ! dup fileclose drop		\ set channel, close it to be sure
  0 (fileopen) 9 ?sdaerror		\ try to open the file
  blk @ >r line @ >r in @ >r		\ stack current pointers
  1 blk ! 0 line !			\ initialize for new file
  floadloop				\ process the file
  r> in ! r> line ! r> blk !		\ restore pointers
  file @ fileclose drop			\ close the file we just read
  1 file +! ;				\ and increment the file number

>utilf
\ define crash data buffer names
: cradef <builds ,
  does> @ crasav + 0 ;
: iddef <builds ,
  does> @ crasav + id-offset @ + 0 ;
: umrdef <builds ,
  does> @ crasav + umr-offset @ + 0 ;
: siddef <builds ,
  does> @ crasav + sid-offset @ + 0 ;

\ CRASAV area allocation map

octal

>userf
0	cradef	kisar5
2	cradef	kdsar5
4	cradef	atpc
24	cradef	cpuid                                  
26	cradef	cpuerr
30	cradef	medlen
32	cradef	meddat
122	umrdef	dumpcheck
124	umrdef	dumpsize
126	iddef	umr0
322	cradef	udsar0
330	cradef	udsdr0
332	cradef	kdsar0
334	cradef	kdsdr0
462	cradef	mmsr3
464	cradef	mmsr1
466	siddef	mmsr2
470	siddef	mmsr0
472	siddef	sisar0
474	siddef	sisdr0
476 	siddef	kisar0
500	siddef	kisdr0
\ kisar6 needs special handling:
: kisar6 crasav 612 + sid-offset @ 10 / + 0 ;
646	cradef	userxrb		\ Backwards in memory
706	cradef	userfirqb	\ Backwards in memory
710	cradef	userkey
730	cradef	userstack	\ Backwards in memory
732	cradef	usersp
752	cradef	kernelstack	\ Backwards in memory
754	cradef	psw
756	cradef	pc
760	cradef	r0
774	cradef	kernelsp
776	cradef	errorcode

decimal
                                                       
\ sda utility and user definitions

\ assorted utilities for later on
>utilf

\ start a comma-separated list
: startlist 0 listflag ! ;

\ print a comma if necessary in a list
: ?, listflag @
  if ." , "
  endif 
  1 listflag ! ;

\ same as rot but other direction
: -rot rot rot ;

\ drop 2 double length values down to 2 single length values
\ d1 d2 => n1 n2
code drop2	5725 ,			\ tst (s)+
		12515 ,			\ mov (s)+,(s)
		next, c;		\ next

\ defining words to define things that are just like single-length
\ words but return a double-length value, or take one or two
\ double-length values

: :d <builds [compile] ' cfa ,
  does> @ execute 0 ;
: d: <builds [compile] ' cfa ,
  does> swap drop @ execute ;
: d2: <builds [compile] ' cfa ,
  does> >r drop2 r> @ execute ;

\ define lots of double-length words

\ just like regular 0branch but checks for double-length zero

>utilf
code 0branch	52525 ,			\	bis (s)+,(s)+
		1003 ,			\	beq 10$
		61404 ,			\	add (ip),ip
		next,			\	next
		5724 ,			\ 10$:	tst (ip)+
		next, c;		\	next

\ like the usual vocabulary names but immediate
: [forth] forth ;					immediate
: [fuser] only forth also util also user definitions ;	immediate
: [futil] only forth also user also util definitions ;	immediate

[fuser]					\ first few search user first
: if ?comp compile 0branch
  here 0 , [ latest pfa ] literal ;	immediate
forward: endif				immediate
: else ' if ?pairs compile branch
  here 0 , swap ' if [compile] endif
  [ latest pfa ] literal ;		immediate

: do ?comp [compile] 2do drop 
  [ latest pfa ] literal ;		immediate
: loop ' do ?pairs
  ' 2do [compile] 2loop ;		immediate
: +loop ' do ?pairs
  ' 2do [compile] 2+loop ;		immediate
: /loop ' do ?pairs
  ' 2do [compile] 2/loop ;		immediate

: begin ?comp [compile] begin
  drop [ latest pfa ] literal ;		immediate
: again ' begin ?pairs [forth] ' begin
  [compile] again [fuser] ;		immediate
: until ?comp ' begin ?pairs
  compile 0branch back ;		immediate
: while [compile] if drop 
  [ latest pfa ] literal ;		immediate
: repeat 2>r [compile] again 2r>
  ' while ?pairs
  here over - swap ! ;			immediate

resolve: endif ?comp dup ' else =
  [forth] if drop                                      
  else [fuser] ' if ?pairs
  [forth] endif [fuser]
  here over - swap ! ;

: : ?exec [compile] : ;			immediate
: ; ?comp ?csp [compile] ; ;		immediate

>userf
:= i 2r
:= i' 2i'
:= j 2j
: emit drop emitc ;

: or rot or -rot or swap ;
: xor rot xor -rot xor swap ;
: and rot and -rot and swap ;

:= sp! sp!

:= leave 2leave
:= >r (do)
:= r> 2r>
:= r 2r

: pick drop 2* 1+ >r r pick r> pick ;

:= + d+
:= - d-
d2: * m*
: / drop m/ swap drop 0 ;
d2: u* u*
: u/ drop u/ swap drop 0 ;                             
:= minus dminus
:= over 2over
:= drop 2drop
:= swap 2swap
:= dup 2dup
code -dup	11500 ,			\	mov (s),r0
		56500 , 2 ,		\	bis 2(s),r0
		1404 ,			\	beq 10$
		16545 , 2 ,		\	mov 2(s),-(s)
		16545 , 2 ,		\	mov 2(s),-(s)
		next, c;		\ 10$:	next
:= rot 2rot

: +! drop >r r 2@ d+ r> 2! ;
d: @ 2@
d: ! 2!
: w@ drop @ 0 ;
d2: w! !
: c@ drop c@ 0 ;
d2: c! c!

:= constant 2constant
: variable 0. 2variable ;code
  octal	10345 ,				\ mov w,-(s)
	5045 ,				\ clr -(s)
	next, decimal

:= 1+ d1+
:= 1- d1-
:= 2+ d2+
:= 2- d2-
:= 2* d2*
:= 2/ d2/                                              
: cswap swap cswap swap cswap ;		\ (swap characters)
:= wswap swap				\ (swap words)

:d here here
d: allot allot
: , swap , , ;           

:d = d=
:d 0= d0=
:d 0< d0<
:d 0> d0>
:d < d<
:d > d>
:d <= d<=
:d >= d>=
:d u< du<
:d u> du>
:d u<= du<=
:d u>= du>=

:d ?remote ?remote

: -dup 2dup 2dup or 0= if 2drop endif ;

d2: type type
d2: dump dump
: " [compile] " 0 swap 0 ;		immediate
:= <# <#
:= # #
:= #s #s
d: hold hold
: #> #> 0 swap 0 ;
: erase drop2 erase ;

: ascii (cin) 0 [compile] dliteral ;	immediate      
: 2ascii (cin) (cin) cswap or
  0 [compile] dliteral ;		immediate
:= % 2%					immediate

:= literal dliteral			immediate
: quit mainloop ;

:= abs dabs
:= min dmin
:= max dmax

: ' -find 0= 0 ?sdaerror 0
  [compile] dliteral ;			immediate

\ define words that are the same, just to make them visible

: cr cr ;
: ?cr ?cr ;
: restore restore ;
:= cmove cmove
:= space space                      
:= hex hex
:= octal octal
:= decimal decimal
:= ." ."				immediate
:= [compile] [compile]			immediate
: compile ?comp compile ;
:= immediate immediate
:= \ \					immediate

>utilf                                                 
: (quote) r count dup 1+ =cells
  r> + >r 0 swap 0 ;

>userf
: " ?comp ascii " compile (quote) word
  here c@ 1+ =cells allot ;		immediate

:d out out
: (uuo) drop (uuo) 0 ;
:= run run
:= ccl ccl
>keysf
:= $[29~ $[29~				\ "do" key
>userf
helpitem do
\ The "DO" key on the LK201 keyboard is used to execute
\ a DCL command.  Type the DCL command, and then press "DO" rather
\ than "RETURN".  Note that this exits SDA.

:h exit bye ;
\ The EXIT command is used to leave SDA.

:h tab 9 emit ;
\ The TAB operator prints a TAB character.

:h .r drop d.r ;
\ The .R operator prints a value in the current radix (usually octal),
\ as a signed number for tabular output.  It expects two numbers on the
\ stack: the top (last) argument is the number of columns to allow for,
\ the next (first) argument is the value to print.
\ The number is printed right-justified in a field of the specified
\ size.  If the number is too large, it is printed in the number of
\ characters needed, without leading or trailing blanks.  Specifying 0
\ for the field size (top of stack argument) is the standard way to
\ print a number without the trailing space you'd get from the . operator.
\ Examples:
\	-2 4 .r		(this will print "  -2")
\	22 0 .r		(this will print "22")

:=h . d.
\ The . operator prints a value in the current radix (usually octal),
\ as a signed number.  It expects the value to print on the stack.
\ A single space is printed after the number.  Example:
\	-2 .		(this will print "-2 ")

:=h u. du.
\ The U. operator prints a value in the current radix (usually octal),
\ as an unsigned number.  It expects the value to print on the stack.
\ A single space is printed after the number.  Example:
\	-2 u.	(this will print "37777777776 ")

:h u.r drop du.r ;
\ The U.R operator prints a value in the current radix (usually octal),
\ as an unsigned number for tabular output.  It expects two numbers on the
\ stack: the top (last) argument is the number of columns to allow for,
\ the next (first) argument is the value to print.
\ The number is printed right-justified in a field of the specified
\ size.  If the number is too large, it is printed in the number of
\ characters needed, without leading or trailing blanks.  Specifying 0
\ for the field size (top of stack argument) is the standard way to
\ print a number without the trailing space you'd get from the . operator.
\ Examples:
\	-2 14. u.r	(this will print "   37777777776")
\	22 0 u.r	(this will print "22")

:h wo. drop o. ;
\ The WO. operator prints a 16-bit value in octal.  It always prints
\ 6 digits; the high-order 16 bits are ignored.  Example:
\	-2 wo.		(this will print "177776")

:h o. base @ -rot <# # # # # # # # # # # # #> type base ! ;
\ The O. operator prints a 32-bit value in octal.  It always prints
\ 11 digits.  Example:
\	-2 o.		(this will print "37777777776")

:h spaces 
\ The SPACES operator prints a number of spaces; the argument specifies
\ how many to print.  Example:
\	5 spaces	(this will print "     ")
  drop dup 0>
  if "                                                                      "
   drop swap type 
  else drop
  endif ;

:h .pos drop out 1+ - spaces ;
\ The .POS operator is used for tabular output; it outputs enough spaces
\ to reach the specified position on the line.  The left margin is
\ position 1.  For example, after a carriage return, 5 .POS is
\ equivalent to 4 SPACES.
        
:h co. drop ^o377 and base @ octal swap 0 <# # # # #> type base ! ;
\ The CO. operator prints an 8-bit value in octal.  It always prints
\ 3 digits; the high-order 24 bits are ignored.  Example:
\	-2 co.		(this will print "376")

:h ao. ^o77 and base @ octal -rot <# # # # # # # # # #> type base ! ;
\ The AO. operator prints a 22-bit value (a memory address)in octal.  
\ It always prints 8 digits; the high-order 10 bits are ignored.  Example:
\	-2 ao.		(this will print "17777776")

\ ditto, but one digit less
\ d-value ==>
: ao.s base @ octal -rot <# # # # # # # # #> type base ! ;
   
>utilf
\ display address in 16 or 22 bit style as appropriate
\ d-value ==>
: ?o. dup if ao. else wo. endif ;

\ same as above, but used when space is limited, and prints the "/"
\ after the number.
\ d-value ==>
: ?o.s wide-list @
  if ?o. ." / "
  else dup
   if dup 32 and			\ check for 22nd bit being set
    if ao.				\ if so that's too bad
    else ao.s				\ else shave off a digit
    endif
    ." /"
   else wo. ." / "
   endif
  endif ;

\ hold an ascii byte
\ n ==> cswap(n)
: a# cswap dup 255 and dup 96 and 0= over 127 and 127 = or
  if drop ascii .
  endif hold ;

>userf
:h ca. swap cswap <# a# #> type ;
\ The CA. operator prints the character representation of the value
\ passed.  It prints only one character corresponding to the bottom 8
\ bits of the argument.  Non-printable codes are printed as a "."
   
:h a. swap <# a# a# #> type ;
\ The A. operator prints the character representation of the value
\ passed.  It prints only two characters corresponding to the bottom 16
\ bits of the argument.  The first character printed corresponds to
\ the bottom 8 bits (least significant byte) of the argument. Non-printable
\ codes are printed as a "."
        
:h %. swap <# %# %# %# #> type ;
\ The %. operator prints the RAD50 representation of the value passed.
\ It prints three characters corresponding to the bottom 16 bits of the 
\ argument.

:=h 2%. 2%.
\ The 2%. operator prints the RAD50 representation of the value passed.
\ It prints six characters (the first three for the low order 16 bits
\ of the argument, the next three for the high order 16 bits).

>utilf
\ hold a word
\ address ==> address-2
: w# dup @ a# a# drop 2- ;

\ hold a rad50 word
\ address ==> address-2
: w%# dup @ %# %# %# bl hold drop 2- ;

\ turns non-printable chararacters into spaces
\ d-start d-length ==>
: clean drop2 over + swap
  do i c@ dup 32 < swap 126 > or
   if bl i c!
   endif
  loop ;

>userf
\ dump 16 bytes from mscbuf
: .buf 16 0
  do i mscbuf + @ o. space
  2 /loop
  0 mscbuf 14 + <# w# w# w# w# w# w# w# w# #> type
  wide-list @
   if 0 mscbuf 14 + <# w%# w%# w%# w%# w%# w%# w%# w%# bl hold #> type
  endif cr ;

:h .date
\ The .DATE operator prints a date.  It interprets the argument passed
\ to it as a standard RSTS date value.  You can also pass 0 to print
\ the current date.
  clrfqb drop fqerno +f ! -1 fqppn +f !
  uu.cnv (uuo) drop fqnam1 +f 9 type ;

:h .time
\ The .TIME operator prints a time.  It interprets the argument passed
\ to it as a standard RSTS time value.  You can also pass 0 to print
\ the current time.
  clrfqb drop fqmode +f ! -1 fqflag +f !
  uu.cnv (uuo) drop fqpflg +f 8 type ;

:h .nodeid
\ The .NODEID operator prints a node number.  The argument is the node
\ address; the result is printed in the standard area number and node
\ within area.  Example:
\	3086 .nodeid		(this prints " 3.14")
   drop base @ swap decimal dup -1024 and 1024 / 63 and 2 .r
   ascii . emit 1023 and 0 .r base ! ;
        
:h .node
\ The .NODE operator prints a node identification, i.e. node number and,
\ if available, the corresponding node name.  If DECnet is not on, or
\ the node number in question does not match any defined node on the system
\ you're running SDA on, only the node number is printed.  Examples:
\	3086 .node		(this prints " 3.14 (FKPK)")
\	1027 .node		(this prints " 1.3")
  2dup .nodeid drop
  clrfqb -22 fqfil +f c! 2 fqsizm +f c!
  fqext +f ! -1 fqsiz +f ! mesag firqb c@ 0=
  if ."  (" fqppn +f 6 2dup 0 swap 0 clean -trailing type ascii ) emit
  endif ;

>utilf
\ print the list of all words that have associated helptext
\ in the first vocabulary of the current search order
: listhelp 4 0
  do context @ i 2* + @ -dup
   if
    begin out 64 >
     if cr
     endif
     dup dup c@ 128 <
     if dup pfa cfa @ 2+ @ ^o22323 =
      if tab id.
      else drop
      endif
     endif
     pfa lfa @ -2 and -dup 0=
    until
   endif
  loop cr ;
        
>utilf
\ trap handler
: trap
  start-error				\ set error mode
  restore sp! 				\ reset things
  rp@ 14 + @ dup 0<			\ get error code
  if 255 and (err) type			\ if error code, print it
  else " FIS FPP BPT IOT EMT TRAP" drop	\ else select right message
    + 4 -trailing type ."  trap"	\ and print it
  endif
  end-error
  interrupt ;

\ initialization for mainloop and ccl handler
: maininit sp! 2drop 0 0 ' bye cfa 'trap ! \ set worst-case trap handler
  14 file ! 0 blk ! [compile] [		\ reset some other stuff
  only user definitions			\ reset the search list
  double				\ always double length numbers
  ' interrupt cfa 'interrupt ! ;	\ set control/c trap

>userf
\ print a help message, either general help text or help for
\ some particular subject
\ 	help		or
\	help subject
: help -1 word here @ 1-		\ check for argument
  if also helptext			\ set a different search order
   (in) c@				\ see if we're at end of line
   if -2find				\ if not, find a two-word entry
   else (-find)				\ else find a single-word entry
   endif
   help-flag @ 0= ?error" ?No help available"
   0= 0 ?sdaerror			\ error if no match
   dup cfa @ 2+ @ ^o22323 =
   if
    ?remote
     if start-popup
        ." Help on " here count type
	begin-popup
     endif
    >r file @ blk @ line @ in @ 15 file ! r> 2@ line ! blk ! 0 in !
    begin linebuf getline swap c@ ascii \ =
     while 1- linebuf 1+ swap type cr
    repeat
    drop in ! line ! blk ! file ! 
    end-popup
   else drop ." ?No help available for "
    here count type cr mainloop
   endif
  else help-flag @ 0= ?error" ?No help available"
   ." Type 'HELP <name>' to get help for <name>" cr
   ." Type 'HELP INTRODUCTION' for an introduction to SDA" cr cr
   ." HELP is available for the following functions:" cr 
   only user listhelp cr
   ." HELP is also available for:" cr
   helptext listhelp cr
  endif only user ;

>keysf
\ some synonyms for the various help keys
:= $[28~ help				\ "help" key
:= $OQ   help				\ PF2 key
:= $Q    help				\ "red" key (or PF2 in VT52 mode)

\ the sda command interpreter mainline
>userf
resolve: mainloop rp!			\ reset return stack
  maininit				\ do other initialization
  run-flag @				\ check for ccl invocation case
  if 0 fileclose drop bye		\ if so we exit
  endif
  15 ?closed				\ is channel 15 no longer open?
  if helpfile firqb fqbsiz cmove	\ right, go set up firqb
   15 0 (fileopen)			\ right, try to re-open the help file
   0= help-flag !			\ set help file flag accordingly
  endif
  3 ?closed				\ is channel 3 no longer open?
  if pfxfile firqb fqbsiz cmove		\ right, go set up firqb
   3 0 (fileopen)			\ right, try to re-open prefix file
   0= pfx-flag !			\ set prefix file flag accordingly
  endif
  begin rp! 14 file !			\ reset some more
   restore ?cr
   mem-flag @				\ figure out prompt to use
   if 0					\ memory
   else sil-flag @			\ check for sil
    if 1				\ was sil
    else 2				\ other, must be dump
    endif
   endif
   ?remote 				\ check if this is a remote session
   if      				\ if it is,
    state @				\ check state
    if					\ if compiling
     128 +				\ set the flag
    endif
    256 * 				\ get flags into the high byte
    5 + 1 remote			\ set function code 5 (set prompt)
   else
    3 *					\ if local, make prompt ID a byte index
    " SmaSsaSda" drop + 3 type		\ and print it out
    state @ 0=				\ check state
    if ." > "				\ prompt if exec state
     base @ mainbase !			\ save user's chosen radix
     modnum @ mainmodnum !		\ ditto for module
    else ." : "				\ else prompt differently
    endif
   endif
   tib @ dup 128 (expect) +		\ read a command line
   ' trap cfa 'trap !			\ set normal trap handler
   dup c@ 26 =				\ check for control/z
   if bye				\ yes, exit
   endif
   dup c@ 128 =				\ check for escape sequence
   if ascii $ over c! dup tib @ - in !	\ got one, insert "$"
    only funkeys			\ search function keys list
    155 word (-find)			\ look for the end
    only user definitions		\ restore standard search order
    if 0 rot c! 0 in ! ?cr		\ put in terminator
     cfa execute ?sdastack		\ invoke whichever word that is
    else 0 sdaerror			\ otherwise complain
    endif
   else 0 swap 2dup 1+ c! c!		\ put in terminating null
    0 in ! sdainterpret			\  and interpret the line sda style
   endif
  again ;				\ and keep doing it

\ now some handy internal words to reference buffers by offset names

>utilf
\ look for symbol name, return address of value cell
: 'sym -1 word symbol			\ get name, search for it
  0= 0 ?sdaerror ;			\ if not found, error

\ each of the 4 following words are used as follows:
\	jdb jdxxxx
\ in other words, the next token is a KERNEL.MAC symbol naming an
\ offset into the appropriate datastructure.  when executed, these
\ words produce a value on the stack which points into one of the
\ block data buffers (jddbuf, etc.) at the named offset.  given that
\ the appropriate data block has first been read into the buffer, this
\ allows quick access to any part of a data structure.

: jdb  compile (jdb)  'sym , ;		immediate
: jcr  compile (jcr)  'sym , ;		immediate
: buf  compile (buf)  'sym , ;		immediate
: ddb  compile (ddb)  'sym , ;		immediate

\ check for value being in a range (unsigned, double)
\ d-value lower-addr length-addr ==> d-value false	(if out of range)
\ d-value lower-addr length-addr ==> d-offset true	(if in range)
: range swap 2@ rot 2@			\ fetch lower bound and length
  2over d+ 2>r 2over 2r> du< >r		\ check upper bound, save flag
  2over 2over du>= r> and >r r		\ check lower range, combine
  if d-					\ if in range, get offset
  else 2drop				\ else drop lower bound
  endif r> ;				\ push flag and exit

\ print address error message
\ d-address ==>
: .aerror
   start-error
   restore ?cr ." ?Invalid address " ao.
   end-error
   interrupt ;

\ map a crash file reference to the proper base and segment
\ d-address ==> d-offset
: mapseg
  0 segment !
  segcnt @ 2* 0
  do bottbl i 2* + lentbl i 2* + range
   if segtbl i + @ segment ! leave
   endif
  2 /loop
  segment @ 0=
  if .aerror
  endif ;

\ map a memory reference to the proper physical address
\ addresses in the fip pool virtual address range are mapped into
\ the fip pool's physical address range.  other addresses are unchanged.
\ this routine assumes fippool is the second crash dump segment
\ d-address ==> d-address
: mapmem fipbot lentbl 4 + range
  if bottbl 4 + 2@ d+
  endif ;

>userf
\ fetch a block out of the crash file or memory
\ d-address d-buffer d-bytes ==>
: getbuf drop2 2swap swap -2 and swap mem-flag @
  if 2>r 2r mapmem clrxrb xrblkm xrb + c!
   xrblk xrb + ! xrlen xrb + ! xrloc xrb + ! (xpeek)
   2r> firqb c@
   if .aerror
   else 2drop
   endif
  else sil-flag @			\ check for sil reference
   if ?sil 2dup silmod se.lod + @ 0 du<	\ check against start address
    if .aerror				\ too low
    endif
    silmod se.lod + @ 0 d-		\ compute offset
    2dup silmod se.siz + @ 0 du>=	\ check against size
    if .aerror
    endif
    silmod se.off + @ 2* 0 d+		\ adjust for module offset
    silmod se.blk + @ 1+ segment !	\ set starting block
   else ?crash mapmem mapseg		\ make sure crash file is open
   endif
   begin 2dup over swap d2/ cswap swap cswap 255 and or
    segment @ + 0 sil-flag @
    if 1
    else 2
    endif
    fblock swap 510 and 512 over - >r + >r
    i' 0 d+ 2swap 2dup i' min r> -rot cmove
    swap r + swap r> - dup 0< >r 2swap r>
   until 2drop 2drop
  endif ;

:h k@
\ The K@ operator fetches a 16-bit value and returns the result.  The
\ data is obtained from the source selected with the most recent
\ USE command (crash dump, memory or SIL); initially the crash dump is
\ selected.  The argument is the address; typically this is the physical
\ address of the data.  For FIP pool data, it is the virtual (FIP mapping)
\ address instead.  You normally use this operator inside a more complex
\ operation; if you just want to see the value, use the E operator instead.
\ In the case of access to the SIL, data is read from the currently
\ selected module; the address is interpreted as a virtual address
\ within that module.
\ Example:
\	60 K@ .		(pick up the console vector, print its value)
  0. 2swap sp@ 6 + 0 2. getbuf ;

>utilf
\ same as k@ but single-length input and output (for built-in functions to use)
: 1k@ 0 k@ drop ;

\ same as k@ but always references the sil and preserves the source
\ selection flags.  takes single-length address
: s@ mem-flag @ sil-flag @ 2>r		\ save current source
  0 mem-flag ! 1 sil-flag !		\ select sil as source
  0 k@					\ fetch the data
  2r> sil-flag ! mem-flag ! ;		\ restore source flags

>userf
:h 2k@
\ The 2K@ operator fetches a 32-bit value and returns the result.  The
\ data is obtained from the source selected with the most recent
\ USE command (crash dump, memory or SIL); initially the crash dump is
\ selected.  The argument is the address; typically this is the physical
\ address of the data.  For FIP pool data, it is the virtual (FIP mapping)
\ address instead.  You normally use this operator inside a more complex
\ operation; if you just want to see the value, use the E operator instead.
\ In the case of access to the SIL, data is read from the currently
\ selected module; the address is interpreted as a virtual address
\ within that module.
\ Example:
\	nulrts r.name + 2K@ %.	(print out the name of the null RTS)
  0. 2swap sp@ 4 + 0 4. getbuf swap ;

:h m->a
\ The M->A operator converts an MMU address ("sliver" number) to a memory
\ (byte) address.  Example:
\	177 M->A .		(prints "17700")
  6 0
  do d2*
  loop ;

:h c->a
\ The C->A operator converts a "contorted" address to a memory (byte)
\ address.  Examples:
\	40 C->A .	(prints "40")
\	41 C->A .	(prints "4100000")
  drop dup 31 and
  if dup -128 and swap 127 and d2/
  else 0
  endif ;

>utilf
\ define words that dump memory in bytes and/or words
\ used as follows:
\	.wb foo 2 -4 3 -6 0
\ Where "foo" is the name of the word, and the number following indicate
\ words or bytes to dump.  negative numbers are counts of bytes, positive
\ number are counts of words.  a 0 must be present to act as terminator.
\ the words being defined take the address to dump as an argument.
: .wb
  <builds
   begin -1 word here number drop dup 0<	\ parse next number
   if =cells					\ force negative ones even
   endif dup , 0=				\ store and check done
   until
  does>				\ d-address ==>
   ?cr
   0					\ flag to fetch first descriptor
   begin 2swap 2dup mscbuf 0 16. getbuf	\ fetch data
    2dup ?o. ascii / emit		\ display address
    16. d+ 2swap			\ advance address
    16 0 do				\ go through one line
     -dup 0=				\ if done with descriptor ...
     if dup 2+ swap @			\ fetch the next one
     endif
     -dup 0=				\ if no more descriptors ...
     if 2- leave 0 1			\ leave the loop
     else i mscbuf + over 0<		\ else point to data, check byte flag
      if c@ space 0 co. 1		\ if byte, fetch, display, count
      else @ space space 0 wo. 2	\ ditto for word
      endif
      swap dup 0<			\ get the descriptor
      if 1+				\ count towards 0
      else 1-
      endif swap
     endif
    /loop				\ do next word/byte
    wide-list @				\ check wide printout
    if 75. .pos 0 mscbuf 14 + <# w# w# w# w# w# w# w# w# #> type
     95. .pos 0 mscbuf 14 + <# w%# w%# w%# w%# w%# w%# w%# w%# #> type
    endif cr
    dup 0=				\ done with descriptor?
    if over @				\ yes, last descriptor?
    else dup
    endif 0=
   until 2drop 2drop ;			\ continue until that time

\ print device name
\ d-device ==>
: .device base @ -rot decimal 2dup or
  if swap 0 a. dup -256 and
   if 255 and 0 .r
   else drop
   endif ascii : emit
  else 2drop
  endif base ! ;

\ print file name
\ d-type d-name d-ppn d-device ==>
: .fname base @ >r decimal .device
  drop -dup
  if ascii [ emit dup cswap 255 and 0 .r ascii , emit
   255 and 0 .r ascii ] emit
  endif 
  2%. ascii . emit drop dup 1+
  if 0 %.
  else drop ascii * emit
  endif r> base ! ;

\ likewise, but with the arguments in the firqb
: .firqb fqext +f @ 0 fqnam1 +f 2@ fqppn +f @ 0 fqdev +f 2@ .fname ;

\ print a percentage amount
\ prints as (d2/d1)*100
\ d1 d2 ==>
: .perc 100 *				\ multiply high order by 100
  swap 100 u*				\ and low order, unsigned
  rot +					\ add in high order product
  0 >r					\ start at zero percent
  begin
   2over 2over				\ copy d1 and 100*d2
   d<=					\ end of divide loop?
  while 2over d-			\ no, subtract out divisor
   r> 1+ >r				\ and increment quotient
  repeat
  2drop 2drop r> 4 .r ascii % emit ;	\ print quotient

\ read module entry from sil header into silmod
\ module-number ==>
resolve: rmod 1. 1 fblock 2dup @ <
  if si.ent + over dup modnum ! 14 >
   if drop 1+ dup 16 / swap over 16 * - swap 1+ 0 1 fblock
   endif
  swap se.len * + silmod se.len cmove
  else silmod se.len erase 2drop
  endif ;

\ find a module by name
\ d-name ==> flag
: (module) modules @ 0
  do i rmod silmod se.nam + 2@ 2over d=
   if leave
   endif
  loop silmod se.nam + 2@ d= ;

>userf
\ :h module [compile] 2% (module) 0= ?error" ?Not found" ; immediate
:h module
\ The MODULE operator searches the SIL index for the specified module
\ name and makes that module the current one.  Operations that reference
\ the symbol table, such as DEFINE, always look in the symbol table of
\ the current module.
\ Example:
\	module xvr
  [compile] 2%			\ get the module name
  state @			\ see if compiling
  if compile (module)		\ if so, compile the call to (module)
  else (module)			\ otherwise look for it now
  endif ; immediate		\ and all set now

>utilf
\ get next symbol in STB
\ ==> pointer
: next-symbol stb-pointer @ dup silmod se.stn + @ u<
  if dup 64 / 1+ silmod se.stb + @ + 0 1 fblock 
   swap 63 and 8 * + 1 stb-pointer +!
  else drop 0
  endif ;

\ match a symbol against the table
\ d-symbol ==> address (or 0 if no match)
: match swap
  begin sym-pointer @ end-global @ u< dup	\ check for end of globals
   if drop sym-pointer @ 2@ swap 2over du<	\ if not end, check name
   endif
  while 6 sym-pointer +!			\ advance if not far enough
  repeat
  sym-pointer @ dup end-global @ u<		\ if not stopped by end
  if 2@ swap d=					\ compare names
   if sym-pointer @ 4 +				\ if match, point to value cell
   else 0					\ otherwise return 0
   endif
  else drop 2drop 0				\ return 0 for end
  endif ;

\ scan symbol table of current module for symbol matches
\ ==>
: scan-stb 0 stb-pointer !		\ initialize stb scan
  start-global @ dup sym-pointer !	\  and global table scan
  begin 4 + 0 over !			\ initialize all builtin globals to 0
   2+ dup end-global @ =		\ point to next entry, check for done
  until 
  begin 
   @ -2 and -dup			\ get link to next symbol
  while					\ if not end yet
   6 + dup @ 1 and 0=			\ user symbols, get link/flag
   if 0 over 2- !			\ global, zero it
   endif
  repeat
  begin next-symbol -dup		\ get next stb symbol pointer, if any
  while dup 2@ match -dup		\ look for global table match
   if swap 6 + @ swap !			\ if match, store value from stb
   else
    dup 2@				\ else get symbol again
    finduser				\ mentioned in user-defined globals?
    if swap 6 + @ swap !		\ yes, update it
    else drop				\ else drop stb pointer
    endif
   endif
  repeat ;

>-wordsf
:h show-ver
\ SHOW VERSION displays the current version number of SDA.
  version ;

:h show-kno
\ The SHOW KNOWN_SYMBOLS operator prints out the RSTS symbols known to
\ SDA: user-defined symbols (from DEFINE command), built-in global symbols,
\ and built-in local (COMMON, KERNEL, etc.) symbols.
  ?cr
  start-popup
  ." Known symbols"
  local:
  begin-popup
  ." User-defined symbols:" cr
  end-global @ @				\ get address of first symbol
  begin -dup
   while dup 2@ 2%. tab dup 4 + @ o.		\ print symbol and value
    6 + @ dup 1 and 0=				\ get link to next, check flag
    if ." G"					\ "G" if a global symbol
    endif
    -2 and					\ clear out flag bit
    out 70 > if cr else tab endif		\ next column
  repeat 
  ?cr cr ." Built-in global symbols:" cr
  start-global @
  begin dup end-global @ u<			\ check for end of list
   while dup 2@ 2%. tab dup 4 + @ o.		\ print symbol and value
    out 70 > if cr else tab endif		\ next column
    6 +						\ point to next symbol
  repeat 
  drop ?cr cr ." Built-in local (prefix) symbols:" cr
  start-prefix @
  begin dup start-global @ u<			\ check for end of list
   while dup 2@ 2%. tab dup 4 + @ o.		\ print symbol and value
    out 70 > if cr else tab endif		\ next column
    6 +						\ point to next symbol
  repeat 
  drop ?cr ;

:h show-sym
\ The SHOW SYMBOLS operator prints out the complete symbol table of the
\ current SIL module, in alphabetical order (i.e. the order of the
\ symbol table in the SIL).
  ?cr
  start-popup
  ." Symbol table for module " silmod se.nam + 2@ 2%.
  local:
  begin-popup
  0 stb-pointer !
  begin next-symbol -dup
  while dup 2@ 2%. tab 6 + @ o.
   out 70 > if cr else tab endif
  repeat ?cr
  end-popup ;

:h show-stb show-sym ;
\ The SHOW STB operator is a synonym of SHOW SYMBOLS.

\ word to define a symbol in the symbol table and search for it

>userf
:h define
\ The DEFINE operator tells SDA to search for the specified symbol.
\ SDA first searches the symbol table of the currently selected 
\ SIL module, and next the prefix symbol table (SDAPFX.DEF).  If the
\ symbol is found, SDA enters the symbol table into its internal
\ symbol table and gives it the value found in the symbol table.
\ You can then use that symbol name anywhere in an SDA numeric
\ argument, for example as part of an expression.
\ Note that you cannot define a symbol whose name duplicates that
\ of an SDA built-in function.  Also keep in mind that SDA already
\ has many symbols pre-defined.  If you try to define a symbol
\ that is already defined, you get a message, and the old definition
\ is retained.
\ Example:
\	define baksml		(finds symbol "BAKSML")
\	baksml 10 L		(prints 10 bytes starting at BAKSML)
  -find ?error" ?Can't redefine SDA built-in function"
  getsym dpl @				\ pick up the symbol in RAD50
  ?error" ?Invalid symbol name"		\ error if not valid RAD50
  2dup findsym				\ see if already defined
  if start-error			\ enter error mode
   ." %Already defined, value is "	\ if so say so
   @ o. cr 2drop			\ and print value
   end-error				\ exit error mode
  else ?sil 2>r 0 stb-pointer !		\ else initialize search
   begin next-symbol -dup		\ get next STB entry
    if dup 2@ 2r d=			\ check for equality
     if symbol-link @ @			\ get previous entry's type flag
      here or				\ merge with pointer to this entry
      symbol-link @ !			\ link this definition to previous
      2r swap , , 6 + @ dup ,		\ save the rad50 name, value
      here symbol-link ! 0 ,		\ make this new last one, store 0 link
      2r> 2%. ."  = " o. 		\ print the value
      ."  (global)" cr 1		\ and the type, set for exit
     else drop 0			\ otherwise loop
     endif
    else				\ not in STB, try SDAPFX.DEF
     pfx-flag @				\ check if we have it
     if base @ file @ blk @ line @ in @	\ save radix and file reading context
      octal 3 file ! 1 blk ! 0 line !	\ initialize radix, file pointers
      begin 0 in ! linebuf getline	\ read a line of input
       dup 1+ 0=			\ check for EOF
       ?error" ?Not found"		\ if EOF, error finding symbol
       + bl swap c!			\ insert trailing blank
       [compile] 2%			\ pick up symbol name
       2r d=				\ compare with name we're looking for
      until				\ loop until we find the match
      -1 word here number drop		\ pick up the value
      symbol-link @ @			\ get previous entry's type flag
      here or				\ merge with pointer to this entry
      symbol-link @ !			\ link this definition to previous
      2r swap , , dup ,			\ save the rad50 name, value
      here symbol-link ! 1 ,		\ make this new last one, store 0 link
					\ with "prefix" flag
      2r> 2%. ."  = " o. 		\ print the value
      ."  (prefix)" cr			\ and the type
      in ! line ! blk ! file ! base !	\ restore context
      1					\ flag to exit main loop
     else
       error" ?Not found"		\ if no SDAPFX.DEF, error finding symbol
     endif
    endif
   until
  endif ;

>utilf
\ <# with zero suppression
: <#z compile <# 0 [ latest pfa ] literal ;		immediate

\ #> with zero suppression
: #z> ' <#z ?pairs 0
  do ' if [compile] endif
  loop compile #> ;		immediate

\ do #, then exit from <#z #z> if zero
\ d ==> d
: (#z) # 2dup or
  if 2
  else r @
  endif r> + >r ;

\ compile (#z)
\ count ==> here count+1
: #z ' <#z ?pairs compile (#z) here 0 , swap 1+ ' <#z ;	immediate

\ print a time in seconds and tenths
\ d-time ==>
: .times base @ -rot decimal
  <#z # ascii . hold #z 6 base ! #z ascii : hold
  decimal #z 6 base ! #z ascii : hold
  decimal #s #z> 10 over - spaces type base ! ;

\ terminal width control

: set-width
  ?remote				\ check for network request
  if drop				\ if so, function is a no-op
  else
   clrfqb -1 fqsizm +f c!		\ do our own terminal
   1 fqfil +f c! uu.trm (uuo) drop	\ read characteristics part 2
   fqdevn +f @ 5 and 5 -		\ check for ansi and 132-column mode
   ?error" ?Width not settable"		\ error if not
   clrfqb dup 1+ fqppn +f c!
   -1 fqsizm +f c!			\ set up for new width
   uu.trm (uuo) drop			\ execute the uuo
   27 emitc ascii > emitc		\ force Ansi mode
   27 emitc ." [?3" 80 >		\ set up the terminal
   if ." h"				\ wide screen if > 80 width
   else ." l"				\ else narrow screen
   endif
  endif ;

>userf
:h wide
\ The WIDE operator sets SDA for wide screen (132-column) mode
\ displays.  When wide display is used, some operations display
\ additional information; for example the L operator displays not
\ only octal and ASCII but RAD50 as well.
\ This option is only available on terminals that support
\ ANSI escape sequences and have 132-column mode, such as the VT100.
  132 set-width				\ set up new width
  1 wide-list ! ;			\ and flag it

:h narrow
\ The NARROW operator sets SDA for narrow screen (80-column) mode
\ displays.  This is the opposite of the WIDE operator.
\ This option is only available on terminals that support
\ ANSI escape sequences and have 132-column mode, such as the VT100.
  80 set-width				\ set up new width
  0 wide-list ! ;			\ and flag it

>utilf
\ print module description.  if "current-flag" is set, indicate that this
\ module is "current".
\ current-flag module-number ==>
: .module rmod silmod se.nam + 2@ 2%. tab
  silmod se.idn + 2@ 2%. tab
  silmod se.nam + @
  cond
	% RST = << 0 >r >>			\ check for specials
	% UNA = << xedap5 >r >>
	% QNA = << xhdap5 >r >>
	% CNA = << xcdap5 >r >>
	% KVR = << xkdap5 >r >>
	% XVR = << xmdap5 >r >>
	% DMP = << xddap5 >r >>
	% TER = << terap6 >r >>
	% KBX = << aptap5 >r >>
	% FIP = << fipap6 >r >>
	% RJ2 = << rjdap5 >r >>
  	% PKP = << pkdap5 >r >>
	% PIP = << silmod se.nam + 2+ @ % ORT =
		   if pipap5 >r
		   else ptbap6 >r
		   endif     >>
	% AX2 = << axdap5 >r >>
	% OVR = << ovrbuf >r >>			\ overlay buffer
  nocond
	silmod se.nam + @ % AP5 findsym		\ try xxxAP5
	if @ >r					\ if found use it
	else -1 >r				\ otherwise leave it blank
	endif
  condend
  sil-flag @					\ examining SIL currently?
  if r> drop -1 >r				\ yes, don't show/use mmu
  endif
  r> -dup 0=
  if ^o1200 0 o.				\ point to RSTS if 1200 mapped
  else dup 1+
   if 1k@ dup o.
   endif
  endif
  tab silmod se.lod + @ o.
  tab silmod se.siz + @ o.
  dup 1+
  if kisar5 drop 1k@ over =
   if ."   <-- KISAR5"
   endif
   kisar6 drop 1k@ =
   if ."   <-- KISAR6"
   endif
  else drop
  endif 
  if ."   <-- Current"				\ indicate "current" if needed
  endif cr ;

\ check whether you're looking at a crash file
\ ==>
: ?ecrash mem-flag @ sil-flag @ or	\ check both flags
  ?error" ?Only valid when examining a crash dump" ;

\ check that we're not looking at the sil
\ ==>
: ?nosil sil-flag @ ?error" ?Not valid when examining the SIL" ;

>-wordsf
:h show-sil
\ The SHOW SIL operator prints the SIL directory of the monitor SIL,
\ including the module's physical address, and marks which
\ module is currently mapped via I-APR 5 and I-APR 6.
  ?sil mem-flag @ sil-flag @ or 0= 
  if ?crash
  endif
  ?cr
  start-popup
  ." Directory of SIL"
  local:
  begin-popup
  ."  Name	Ident	 Phys	 Load	 Size" cr
  modnum @ modules @ 0
  do dup i = i .module
  loop cr rmod
  end-popup ;

:h show-mod show-sil ;
\ The SHOW MODULES operator is a synonym for SHOW SIL.

:h show-ove
\ The SHOW OVERLAYS command displays a list of each FIP overlay,
\ including the overlay name, start virtual address, and number
\ of overlays.
  ?cr
  start-popup
  ." Overlay directory"			\ set popup name
  local:
  begin-popup
  ." Overlay   Start Virtual  Number of" cr
  ." Name      Address        Overlays" cr
  modnum @				\ save current module name
  base @ decimal			\ and save current base
  2% OVR (module) drop			\ set module to OVR
  tblext				\ get start virtual address
  tblend ovrtbl oth.sz + do 		\ for the entire table:
   i s@ %. 11. .pos			\ get overlay name
   i 4 + s@ drop >r			\ get count of overlays
   dup o. 28. .pos			\ display virtual address
   r 512 u* drop + 			\ and update for next time
   r . cr				\ display number of overlays
   r> 2* otm.sz + 			\ calculate next overlay start
  /loop					\ and loop
  drop					\ dump virtual address
  base !				\ restore radix
  rmod 					\ and restore module
  end-popup ;				\ and end popup mode

>utilf
\ locate a FIP overlay by overlay virtual address
\ d-virtual address ==> d-real flag (1 = real)
: find-over
  modnum @ >r				\ save current selected module
  2% OVR (module) drop			\ set module to OVR
  base @ decimal >r			\ save base and set to decimal
  drop tblext - 0 512 u/ swap drop	\ create overlay number
  tblend ovrtbl oth.sz + do		\ for the entire table:
    i 4 + s@ drop dup			\ get overlay count twice
    rot swap -				\ get number of overlays left
    swap 2* otm.sz + 			\ get loop increment amount
    over 0< if				\ check if we're done
     i s@ %.				\ if so, print out our name
     over i 4 + s@ drop + .		\ and overlay number
     leave endif			\ and set to leave the loop
  /loop 				\ loop until done
  0< 0 swap				\ now return the real flag
  r> base !				\ restore the radix
  r> rmod ;				\ and the previous module

\ initialize after opening the sil
: setsil 1. 1 fblock @ modules !	\ get module count
  2% OVR (module) drop scan-stb		\ set module to OVR and set up globals
  0 rmod ;				\ now read RSTS module

\ pick up next mmu value, store as byte pointer in specified variable
\ pointer address ==> pointer+2
: gmmu >r dup 2+ swap 1k@ 64 u* r> 2! ;

\ pick up next size, accumulate segment start block number, store
\ in indicated variable
\ block pointer address ==> next-block pointer+4
: gseg >r dup 4 + swap 1k@ 7 + 0 8 u/	\ get next size, compute block count
  swap drop				\ get only the quotient
  1- x.scs 1k@ dup minus -rot + and	\ round up to a dcs multiple
  rot + dup r> ! swap ;
        
\ set up segment descriptions from dump file
: setcrash ^o17777776. lentbl 2! 	\ temporarily set up infinite 1st seg
  ^o121000. fipbot 2!			\ set up fake fip pool start
  1 segcnt !				\ 1 segment for now
  1 segtbl !				\ initialize 1st segment pointer
  x.tab 4 + 1k@ fplap6 1k@ -		\ get displacement of fippool from apr6
  ^o1400 + ^o100 u* fipbot 2!		\ use that to get fippool virtual base
  0 x.con 1k@ xc$ids and 0=		\ check d-space hardware flag
  if drop ^o204
  endif dup id-offset !
  x.con 1k@ xc$umr and 0=		\ next check umr flag
  if ^o174 +
  endif umr-offset !
  0 x.con 1k@ xc$ids and 0=		\ check super-mode hardware flag
  if drop ^o40
  endif sid-offset !
  x.tabs segcnt !			\ set actual segment count
  x.tab 2+				\ start with rsts segment length
  lentbl gmmu				\ get length of first segment
  x.tabs 2* 2
  do bottbl i 2* + gmmu			\ get another segment start
   lentbl i 2* + gmmu			\  and segment length
  2 /loop
  drop
  1 x.tab 2+				\ set up to get segment starts
  x.tabs 2* 2
  do segtbl i + gseg			\ set up a segment table entry
  2 /loop
  2drop ;

\ file access functions

\ find the installed sil's name on a given disk
\ enter with device name (or 0 for system disk) in firqb, leave
\ with entire filespec in firqb
\ ==>   
: defsil fqdev +f @ 0=
  if -256 fqdevn +f ! 2ascii SY fqdev +f ! \ default to _SY0:
  endif
  fqdev +f 2@ 1 fileclose drop		\ make sure SIL channel is closed
  filename [0,1]init.sys/ro drop
  2dup fqdev +f 2! 1 0 (fileopen)	\ open INIT.SYS for a moment
  if filename [0,1].sil/mo:#20400 drop	\ supply the part we can guess
   fqdev +f 2!				\ set the device name
  else 1 modules ! 0 rmod		\ else read the first (only) module
   ^o72 s@ drop dup s@ drop
   swap 2+ s@ drop			\ get installed SIL name
   1 fileclose drop			\ close INIT.SYS
   filename [0,1].sil/mo:#20400 drop	\ set up rest of FIRQB
   fqnam1 +f 2! fqdev +f 2!		\ fill in name and device
  endif ;

\ set up the default crash file name on a given disk
\ enter with device name in firqb, leave with complete filespec
\ in firqb
\ ==>
: defcrash fqdev +f 2@ filename [0,1]crash.sys/mo:#20400 drop fqdev +f 2! ;

\ print an error message for getfile
\ error ==>
: .ferror
  start-error
  ?cr restore (err) type cr 
  end-error ;

\ pick up a filename from "here", parse it, try to open it,
\ and print appropriate messages if something goes wrong.
\ enter with the default filespec data set up in the firqb.
\ channel ==> status
: getfile here count fss dup	\ parse the name
  if .ferror swap drop		\ if bad name, print message, toss channel #
  else over fileclose drop	\ else close the channel to be sure
   fqnam1 +f @ 0=		\ check for missing filename
   if
    start-error			\ set error mode
    2drop 1 ?cr restore		\ set error status
    ." ?Missing file name" cr	\  and print the error
    end-error			\ and end error mode
   else (fileopen) dup		\ try to open the file
    if dup .ferror		\ if it failed, say why
    else ?cr ." [Using "	\ else report what file we picked
     .firqb ." ]" cr		\  and show the file name
    endif
   endif
  endif ;

>userf
:h sil
\ The SIL operator is used to select a new SIL without having
\ to restart SDA.  It takes the filename to use as argument;
\ by default it opens the installed SIL on the system disk.
\ If you specify only a disk name, the installed SIL on that
\ disk is used.
\ When the new SIL is opened, SDA scans the SIL's symbol table
\ to update its internal table of global symbol values.
\
\ Examples:
\	SIL		(opens the installed SIL on SY0:)
\	SIL DU3:	(opens the installed SIL on DU3:)
\	SIL FOO		(opens SY0:[0,1]FOO.SIL)
  -1 word			\ pick up the name
  here count (filename) drop	\ fss it to get device name for defaults
  defsil			\ set up defaults given device name
  1 getfile 0=			\ now open it for real
  if fqdev +f 2@ sildev 2!	\ save SIL's device spec
   setsil			\ set things up and scan the STB
  endif ;

:h crash
\ The CRASH operator is used to open a new crash dump file. 
\ It can be used to start analyzing a second crash dump using
\ the same SIL you were using already.  This operator also
\ selects the crash dump as the current source of data, just
\ as the USE CRASH command would.  The CRASH operator takes
\ the filename of the crash dump file as argument; by default
\ it opens CRASH.SYS on the same disk as the SIL you currently
\ have open.
\ Examples:
\	CRASH			(opens xxn:[0,1]CRASH.SYS)
\	SIL DU3:		(pick a different SIL)
\	CRASH FOO		(opens DU3:[0,1]FOO.SYS)
  ?sil				\ check for open SIL
  -1 word			\ pick up the specified name if any
  clrfqb			\ start out with no defaults in firqb
  sildev 2@ fqdev +f 2!		\ set default device in firqb
  defcrash			\ add in the rest of the defaults
  2 getfile 0=			\ now open the specified file
  if 0 mem-flag ! 0 sil-flag !	\ reset source to be crash file
   setcrash			\ set things up segment descriptors
  endif ;

:h use ?sil [compile] show ; immediate
\ The USE operator is used to select the source of the data being
\ examined with the various display operators.  The source to be used
\ is specified with a keyword.  The choices are the crash dump file
\ (as selected when you started SDA or using the CRASH operator);
\ the contents of memory of your running system; or the contents of
\ the SIL (as selected when you started SDA or using the SIL operator).
\ Certain display operations are only valid for one or two of the
\ possible data sources; if you use them when they are not valid and
\ error message is printed.  The keyword selecting the source can
\ be abbreviated to 3 characters.  SDA changes its prompt to indicate
\ the currently used source of data ("sda" for crash dump, "sma" for
\ memory, "ssa" for SIL).
\ Examples:
\	USE CRASH		(select crash dump)
\	USE MEMORY		(select current memory)
\	USE SIL			(select SIL)

>-wordsf
:h use-mem
\ The USE MEMORY command selects your running system memory as the
\ source of data to be used by SDA.
\ Note that system memory is dynamic data.  Data structures can
\ change in the middle of SDA commands, which may produce strange
\ output and/or SDA errors.
  clrfqb 1 fqfil +f c!		\ subfunction 1: look up priv by name
  " RDMEM" fqnam1 +f swap cmove	\ set name to find
  uu.chk (uuo) drop		\ look it up
  fqerno +f c@ ?error" ?RDMEM privilege required"  \ error if no privs
  1 mem-flag !			\ indicate from memory
  0 sil-flag !			\ set source flags appropriately
  setcrash ;			\ and initialize segment descriptors

:h use-cra ?crash 0 mem-flag !
\ The USE CRASH command selects the crash dump file as the source
\ of data to be used by SDA.
  0 sil-flag !			\ set source flags for dump file
  setcrash ;			\ reinitialize segment descriptors

:h use-sil ?sil 0 mem-flag !
\ The USE SIL command selects the SIL as the source of data to be
\ used by SDA.
  1 sil-flag ! ;		\ select sil as source

>utilf
\ routine for processing the SDA.INI file by setting it up as
\ for an fload command
: iniload file @ 1-			\ decrement file number
  fileopen _sy:[]sda.ini 0=		\ attempt to open sda.ini
  if blk @ >r line @ >r in @ >r		\ stack current pointers
   -1 file +! 1 blk ! 0 line !		\ initialize for new file
   floadloop				\ process the file
   r> in ! r> line ! r> blk !		\ restore pointers
   file @ fileclose drop		\ close the file we just read
   1 file +!				\ and increment the file number
  endif ;

\ initalization routine.  this is the entry point to the sda program;
\ it sets up a few things, prompts the user for sil and crash file
\ names, and sends him/her off to the main loop
: start cr ' bye cfa 'interrupt !	\ ^c will get us out initially
  clrxrb jfpriv xrb ! (clear)		\ drop privs
  ^o120400 fqmode +f !			\ force mode to read/only and cached
  % fth fqext +f !			\ put source file extension in
  firqb helpfile fqbsiz cmove		\ save firqb for help file open
  2% sdapfx fqnam1 +f 2!		\ now load filename SDAPFX.DEF
  % def fqext +f !
  firqb pfxfile fqbsiz cmove		\ and save that firqb
  fqnent +f @ ^o77777 and		\ get "line" number, ignore priv flag
  30000 = dup cclflag !			\ see if ccl entry
  if 0 corcmn c@ corcmn + 1+		\ if so point to end of core cmn
   2dup 1+ c! c!			\ put in double null terminator
   corcmn 1+ tib !			\ make that our temp input buffer
   0 in !				\ and initialize scan
   -1 word				\ get rid of the invoking ccl
   (in) c@ 0=				\ test for end of line
   if 0 run-flag !			\ if so, plan to stay around
   endif
  else 0 run-flag !			\ if not ccl, stay around
   version tab				\ print version number
   0 (err) type cr cr			\  and the system name as well
  endif
  15 fileclose drop			\ close helpfile channel
  helpfile firqb fqbsiz cmove		\ pick up helpfile spec
  15 0 (fileopen)			\ try to open the help file
  0= help-flag !			\ save the resulting help file flag
  3 fileclose drop			\ make sure channel 3 is closed
  pfxfile firqb fqbsiz cmove		\ get prefix file firqb
  3 0 (fileopen)			\ open that
  0= pfx-flag !				\ save the resulting flag
  ?remote				\ check for network
  if					\ if so,
   0 wide-list !			\ initially give wide lists **temp narrow **
  else
   clrfqb -1 fqsizm +f c! uu.trm (uuo)	\ get terminal characteristics part 1
   drop fqppn +f c@ 81 >		\ get width, see if "wide"
   wide-list !				\ save result as a flag
  endif
  ^o17777776. lentbl 2! 		\ temporarily set up infinite 1st seg
  1 segcnt !				\ 1 segment for now
  1 segtbl !				\ initialize 1st segment pointer
  ?remote				\ check for network
  if
   begin				\ if so,
    8 send-query			\ request the sil name
    query 1 word			\ get answer, fetch it to "here"
    clrfqb defsil			\ Default is the system disk
    1 getfile				\ open specified file on channel 1
    dup 0= if				\ if operation was successful
    9 send-query			\ request the crash name
     query 1 word			\ get answer, fetch it to "here"
     clrfqb defcrash			\ set up default crash file name
     2 getfile				\ and try to open it
     or					\ Set flag to see if data is good
     endif
   while				\ Check to see if we need to wait again
    query				\ If so, wait for new data
   repeat				\ Loop until we're happy
   dismiss-modal			\ they are ok, dismiss the box
   setsil				\ and set up the sil
  else					\ if local:  
   begin clrfqb defsil			\ get default SIL name
    cclflag @ 0=			\ if not ccl entry
    fqnam1 +f @ 0= or			\ or we couldn't determine SIL name
    if ." Sil <" .firqb ." > ? "	\ prompt for SIL to use
     query 1 word			\ get answer, fetch it to "here"
     clrfqb here count fss drop		\ parse the answer once for defaults
     defsil				\ re-set default SIL based on that
     1 getfile				\ open specified file on channel 1
    else 1 0 (fileopen) dup		\ else open default file
     if ?cr dup (err) type cr		\ print error message
     endif
    endif
    while cclflag @			\ if error, check ccl entry
     if bye				\ if ccl, just leave
     endif
   repeat				\ keep at it until answer is valid
   fqdev +f 2@ sildev 2!		\ save sil device name
   setsil				\ set up sil related data
   begin clrfqb				\ start out with no defaults
    sildev 2@ fqdev +f 2!		\ set up default device for crash
    defcrash				\ fill in default crash filespec
    cclflag @ 0=			\ if not ccl entry
    if ." Crash dump file <" .firqb ." > ? "  \ prompt for dump file
     query 1 word			\ get answer, fetch it to "here"
     clrfqb sildev 2@ fqdev +f 2!	\ put in default device
     here count fss drop		\ parse the answer once for defaults
     defcrash				\ re-set default crash file from that
     2 getfile				\ open specified file on channel 2
    else 2 0 (fileopen) dup		\ else open default file
     if ?cr dup (err) type cr		\ print error message
     endif
    endif
    while cclflag @			\ if error, check ccl entry
     if bye				\ if ccl, just leave
     endif
   repeat				\ keep at it until answer is valid
  endif
  0 mem-flag ! setcrash			\ indicate using file, and set it up
  0 sil-flag !				\ indicate not looking at sil
  run-flag @				\ see if ccl with arguments
  if rp! maininit restore ?cr		\ reset things before execution
   sdainterpret bye			\ interpret the line, and exit
  else cr help-flag @			\ see if we have help
   if ." [Help is available]"		\ yes we do
   else ." %No help available"		\ sorry not this time
   endif
   cr pfx-flag @ 0=			\ see if we found the prefix file
   if ." %SDAPFX.DEF file unavailable" cr
   endif
   cr maininit iniload			\ all set, execute sda.ini file if any
   mainloop				\ finally enter main processing loop
  endif ;

\ words to save and restore current radix

\ get current radix
\ ==> d-radix
: base@ base @ 0 ;

\ restore current radix
\ d-radix ==>
: base! drop base ! ;

\ check for decnet being installed and active
\ also checks to make sure we're not looking at the sil
\ ==>
: ?net ?nosil nodlst dup 0= ?error" ?DECnet not installed"
  1k@ 0= ?error" ?DECnet not active" ;

\ utility routines for list following functions

>utilf
\ follow! - set follow address to just after call to follow!
\
: follow! 
  ?comp					\ check for compiling
  latest pfa cfa			\ get latest CFA
  [compile] literal			\ and compile it into definition
  compile follow			\ compile follow variable
  compile !				\ and compile store operator
  ; immediate

\ dofollow - execute the word in follow
: dofollow
  follow @				\ get address to execute
  -dup if				\ check to see if there is one
   execute				\ if so, execute it
  endif ;

\ ****************************************************************************
\       
\	At this point we stop using the single-length environment
\	and switch to the double-length stuff.  But before going there,
\	we need some final definitions to set things up that could
\	not be done before this point.
\
\ ****************************************************************************

\ redefine the multiway conditional branch words for the double-length
\ definition environment

>utilf
: [userf] >userf ;				immediate

>userf
: cond
	0 compile 2dup ;			immediate

: <<
	1+
	[fuser] [compile] if [userf]
	compile 2drop ;				immediate

: >>
	[fuser] [compile] else [userf]
	compile 2dup
	rot ;					immediate

: nocond
	minus
	compile 2drop compile 2drop
	;					immediate

: condend
	dup 0>=
	if
		[fuser] [compile] nocond [userf]
	endif
	minus 0 do
		[fuser] [compile] endif [userf]
	loop ;					immediate

\ redefine the variables we need so they can be accessed in 
\ the double-length environment

>userf
: mscbuf mscbuf 0 ;
: jdbbuf jdbbuf 0 ;
: jcrbuf jcrbuf 0 ;
: ddbbuf ddbbuf 0 ;
: silmod silmod 0 ;

>utilf
: pos pos 0 ;
: len len 0 ;
: quan quan 0 ;
: 1@ drop @ 0 ;			\ to access 16-bit variables
: (err) drop (err) 0 swap 0 ;	\ to display RSTS error messages

\ ****************************************************************************
\
\	From this point onwards all definitions are written
\	exclusively in double-length arithmetic (using the SDA
\	flavor interpreter loop)
\	Note that the only vocabularies searched are User, Util
\	and Only.  Vocabulary Forth is not searched, because
\	everything is double-length now and using Forth words
\	gives rise to errors.  To put it differently, any Forth
\	word needed past this point should be redefined for the
\	double-length world at some point above.
\
\ ****************************************************************************

>user	double

\ 2dup, as defined here, means "duplicate top 2 values"
\ d1 d2 => d1 d2 d1 d2
: 2dup over over ;

\ define some monitor structure printout words

\ note: mcsb is defined by 3 -2 1 wherever it occurs

decimal
.wb .jdb	2 -2 2 -2 2 3 -2 1 -2 2 0
.wb .jcr	7 -2 2 1 3 3 3 3 1 -6 1 -2 2 -2 16 12 0
.wb .rts	4 3 -2 1 -2 1 -2 1 -6 0
.wb .lib	3 -2 3 -2 1 -2 1 -2 1 -4 1 0
.wb .rib	1 -6 -4 1 -2 2 -2 2 -2 0
.wb .pmbb	2 -2 1 -2 1 10 0
.wb .ccbb	2 -2 5 4 4 0
.wb .ddb	-4 3 3 -2 0
.wb .dsq	1 -2 1 -2 1 -2 2 -2 4 -2 1 -2 0
.wb .ttybuf	1 -30 0
.wb .wcbb	-6 4 -2 1 7 0
.wb .fcbb	2 -2 3 -4 -2 1 -2 1 -2 3 0
.wb .cclb	3 -2 3 -10 1 -2 2 0
.wb .nob	2 1 -6 1 -6 1 -2 2 2 0
.wb .llb	4 1 1 -6 7 0
.wb .llx	1 1 2 -4 2 1 1 2 -4 2 0
.wb .netddb	-4 3 8 8 4 -2 2 -2 4 0
.wb .mcsb	3 -2 1 0

octal			\ octal numbers from here on unless stated

40 constant bl			\ blank

>util
\ convert fip unit number to device name
\ d-fun ==> d-devnam
: fun->dev 12 * dsklog + 6 + 2k@ ;

\ print a job state; assumes job's jdb is in jbddbuf
\ job*2 ==>
: .state dup jbwait + k@ swap jbstat + k@ over and
  if drop ." RN"
  else
   cond
	0=		<< ." HB" >>
	js.kb =		<< jdb jdwork w@ xrtime + k@ 0<
			   if ." ^C" else ." KB" endif >>
	jstel and	<< ." TT" >>
	jsfip and	<< ." FP" >>
	jstim and	<< jdb jdiost c@ if ." SR" else ." SL" endif >>
	jsbuf and	<< ." BF" >>
   nocond				\ some kind of I/O wait
	jdb jdwork w@ xrci + k@ 376 and	\ get current channel * 2
	jdb jdiob w@ + k@		\ get ddb/wcb pointer
	ddbbuf 40 getbuf	     	\ read ddb/wcb
	ddb ddidx c@ -dup		\ get device index
	if devokb + 2- devnam + k@	\ if not disk, pick up name from devnam
	else ddb w$fcb w@		\ else get fcb pointer
	 f$clus - f$unt + k@ 377 and	\ get fip unit number
	 fun->dev			\  and convert it to device name
	endif
	a.				\ print the result (lsw only)
   condend
  endif ;

\ print a swap slot
\ jcswap m.ctrl ==>
: .slot dup
  cond
	swp and		<< dup common out and
	    		   if ." Swo"
			   else ." Swi"
			   endif >>
	common out and	<< over dup -1 =
			   if ." Out"
			   else 377 and dup 77 and <# # #
			    swap 100 / ascii A + hold #> type
			   endif >>
	lck and		<< ." L" dup mc.lck / <# # # #> type >>
  condend drop drop ;

\ calculate the JCR address for a job passed job # * 2
\ job number times two ==>
: j2->jcr jcrsiz k@ u* $$jcr6 k@ 100 u* + ;

>user
\ print a ppn
\ ppn ==>
: .ppn -dup
  if dup cswap 377 and 3 .r ascii , emit
   377 and 0 .r
  else ."  **,**"
  endif ;

:h j->jcr
\ The J->JCR operator calculates the starting address for a job's
\ job control region entry. It takes one operand, the job number.
\ Example:
\	1 J->JCR .		(displays the JCR start address for job 1)
  2* j2->jcr ;				\ get the JCR address

>util
\ .l - internal word for list function
\
\ address ==>
\ In order to use the list following mechanism, only a single parameter
\ can be used (the start), so the L command sets up the length and then
\ comes here.
: .l
  ?cr
  follow!				\ set the restart point
  start-popup
  dup pos !				\ save start address
  -2 and				\ round start down to even
  dup k@ quan !				\ save first word in quantity
  dup len @ + 2- swap			\ compute end-2 address
  ?remote
  if
   ." Memory contents from " dup o. ."  to " over o.
  endif
  begin-popup
  do i ?o.s				\ display the address
   i mscbuf 20 getbuf .buf		\ read the data and display it
  20 /loop
  end-popup ;

>user

:h l
\ The L operator displays the contents of a block of memory.  It takes
\ two operands, the starting address and length.  Length is rounded
\ up to a multiple of 32.  In narrow-screen mode, this operator displays
\ the address and memory contents in octal and ASCII.  In wide-screen
\ mode, the RAD50 interpretation is also shown.
\ Example:
\	FIQUE @ 40 L		(displays the work block FIP is working on)
  len !					\ store the length
  .l ;					\ do the list

:h b
\ The B operator display the contents of the specified byte
\ in octal and ASCII.
\ Example:
\	NEXT B			(displays the contents of "NEXT")
  follow!				\ set the re-execute location
  dup pos ! 1 len !			\ save position/length
  ?cr dup ?o. ." \ " 			\ display address
  dup -2 and k@ swap 1 and		\ fetch 16 bits, check odd address
  if cswap				\ if odd, swap it
  endif
  377 and dup co. tab dup ca. 		\ and display the value two ways
  quan ! ;				\ and save it for later

:h e
\ The E operator display the contents of the specified word
\ in octal, ASCII and RAD50.  If an odd address is specified,
\ a B (display byte) operation is done instead.
\ Example:
\	JOBDA E		(display the JDB pointer for the current job)
  follow!				\ set the re-execute address
  dup 1 and				\ check for odd address
  if b					\ if so display byte
  else dup pos ! 2 len !		\ else save position/length
   ?cr dup ?o. ." / " 			\ display address
   k@ dup wo. tab dup a. tab dup %.	\ and display the value three ways
   quan !				\ save it in 'quantity'
  endif ;

:h ls -40 and 40 l ;
\ The LS operator displays the contents of a small buffer. It takes
\ as operand the starting address of the buffer, and display 32 bytes.
\ In narrow-screen mode, this operator displays the address and memory
\ contents in octal and ASCII.  In wide-screen mode, the RAD50
\ interpretation is also shown.
\ Example:
\	FIQUE @ LS		(displays the work block FIP is working on)

:h lx ?nosil c->a 100 l ;
\ The LX operator displays the first 64 bytes of an XBUF-style buffer.
\ It takes the contorted address of the buffer as an argument.
\ In narrow-screen mode, this operator displays the address and memory
\ contents in octal and ASCII.  In wide-screen mode, the RAD50
\ interpretation is also shown.
\ Example:
\	lltbuf k@ LX		(displays the DECnet logical link table)

:h lb ?nosil c->a dup k@ l ;
\ The LB operator displays an entire XBUF-style buffer.  It takes the
\ contorted address of the buffer as argument, and picks up the length
\ to be displayed from the first word (PF.SIZ) of the buffer.
\ In narrow-screen mode, this operator displays the address and memory
\ contents in octal and ASCII.  In wide-screen mode, the RAD50
\ interpretation is also shown.
\ Example:
\	lltbuf k@ LB		(displays the DECnet logical link table)

>keys
\ display previous block/location (uparrow key on VT100)
\ ==>
: $[a pos @ len @ - 0 max		\ back up by length, but not past 0
  dofollow ;				\ now execute the function again

:= $oa $[a				\ uparrow in application keypad mode
:= $a $[a				\ uparrow in vt52 mode
:= $[5~ $[a				\ "prev screen" on VT2xx

\ display next block/location (downarrow key on VT100)
\ ==>
: $[b pos @ len @ +			\ advance by length
  dofollow ;				\ now execute the word again

:= $ob $[b				\ downarrow in application keypad mode
:= $b $[b				\ downarrow in vt52 mode
:= $[6~ $[b				\ "next screen" on VT2xx

: $[4~ len @ pos +! $[a ;		\ VT2xx select, just redisplay

\ display contents of address in quantity register (right arrow on VT100)
\ ==>
: $[c quan @				\ Get the address
  -dup if dofollow endif ;		\ Call routine if there is a next
:= $0c $[c				\ rightarrow in application keypad mode
:= $c $[c				\ rightarrow in vt52 mode

>util
\ print a job's status
\ jdb-address job ==>
: .jobstat 2* >r dup wo. ." / "		\ print jdb address
  jdbbuf 40 getbuf			\ read jdb
  jdb jdjbno c@ j2->jcr jcrbuf jcrsiz k@ 2* getbuf \  and jcr
  base@ decimal r 2/ 2 .r		\ set radix, print job number
  jdb jdflg w@ dup jfsys and		\ check for temp privs active
  if ."  + " drop			\ if so flag that
  else jfsyst and			\ else check for temp privs available
   if ."  - "
   else ."    "
   endif
  endif
  jdb jdjdb2 w@ j2ppn + k@ .ppn ^d23 .pos \ display ppn
  jdb jdiob w@ k@ ddbbuf 40 getbuf	\ read channel 0 ddb
  0 r ddb ddjbno c@ =			\ is this job attached?
  if ddb ddcnt w@ ddcons and		\ maybe, check console bit
   if drop -1				\ yes, flag that
   endif
  endif
  if ." KB" ddb ddunt c@ 0 .r		\ attached job, display kb number
   ddb ttintf w@ ttmodm and		\ check dialup flag
   if ascii * emit			\ flag dialups
   endif
  else ." Det "				\ otherwise say it's detached
  endif ^d31 .pos
  jcr jcname @ 2%. ^d39 .pos		\ print out program name
  jcr jchdrs c@ jdb jdsize c@ -dup	\ get header size and total size
  if over -				\ if size non-0, deduct header
  else drop 0 0				\ else use 0 for both
  endif
  2 .r ascii + emit 1 .r		\ display job and header sizes
  ascii / emit				\  and separator
  jcr jcsizm c@ 2 .r ." K  "		\ display max size
  r> .state				\ display job state
  jdb jdsize c@				\ get total size again
  if space jcr jcswap c@		\ if job has a size, get swap slot
   jdb jdmctl m.ctrl + c@ .slot		\ get m.ctrl also, and print it
  endif
  ^d56 .pos jcr jccpu w@		\ get low order cpu time
  jcr jccpum c@ wswap or		\  and high order, and combine them
  .times space				\ print the cpu time
  jcr jcpri c@				\ get priority
  200 xor 200 -				\ hack it into a signed valud
  4 .r ascii / emit			\ print priority
  jcr jcbrst c@ 0 .r ^d75 .pos		\ get and print run burst
  jdb jdrts w@ r.name + 2k@ 2%.		\ finally, print the rts name
  cr base! ;				\ restore base and exit

>-words
:h show-use
\ The SHOW USERS operator displays a list of job status information
\ for all jobs, in a format roughly equivalent to that of the DCL
\ command SHOW USERS/ALL.
  ?nosil
  ?cr
  start-popup
  ?remote if ." System users" endif
  begin-popup
  ."   JDB   Job    Who    Where    What     Size    State    "
  ." Run-Time  Pri/RB   RTS"
  cr 100 1				\ loop through all possible jobs
  do i 2* jobtbl + k@ dup 177777 =	\ check for end of jobtbl marker
   if drop leave			\ leave if end
   else -dup				\ else is there a job here?
    if i .jobstat			\ if there is, display it
    endif
   endif
  loop
  end-popup ;
  
:h show-cra
\ The SHOW CRASH operator shows summary information about the
\ crash dump.  This command is only valid if you're examining a
\ crash dump file, not if you're looking at current memory.
  ?ecrash 
  start-popup
  ." Crash summary"
  local:
  begin-popup
  base@ decimal cr ." Crash dump taken "
  date k@ .date space time k@ .time	\ show date and time
  cr dumpsize k@ dumpcheck k@ xor 177777 - \ check the checksum
  if ." Dump taken by SNAP"			\ no match -> snap dump
  else ." Error code : " errorcode k@ dup	\ else get the error code
   cond
	177777 =	<< drop ." Power failure" >>
	177776 =	<< drop ." Jump to 0" >>
	177775 =	<< drop ." Crash forced by operator" >>
	177774 =	<< drop ." Crash forced by software" >>
   nocond
	(err) type
   condend
  endif cr
  ." Last FIP function was " fipfun k@ 2/ 3 *
  " CLSOPNCREDLNRENDIRUUOERRRSTLOKASSDEADALCRTCRBRUNPFBEOVMTAWINEXTNETBYEREMDCLSTADECTRUDSP"
	drop + 3 type ." FQ" cr
  ." Last FIP overlay was "
     fipovr k@ 4 / 1000 * dup find-over if ."  (" wo. ." )"
     else ." ID " wo. endif cr
  ." Last job in FIP was " fijob k@ 377 and 2/ 0 .r cr
  job k@ 377 and -dup if ." Job running was " 2/ 0 .r cr endif
  job k@ cswap 377 and -dup if ." Next job was " 2/ 0 .r cr endif
  ." Crash occured on the " systak 10 + kernelsp k@ u> if ." system"
      else ." FIP" endif ."  stack" cr
  ." Priority was PR" psw k@ 340 and 40 / 0 .r cr
  end-popup
  base! ;

:h show-dis
\ The SHOW DISKS operator shows a listing of disk-related information
\ for all disks currently mounted, open or allocated, in a format
\ similar to that of the DCL command SHOW DISKS.
  ?nosil
  ?cr
  start-popup
  ?remote if ." Disk summary" endif
  begin-popup
  ." FUN Dsk  Open    Size       Free   Clu   Err   Name     "
  ." Level   Comments"
  base@ decimal cr			\ save radix, set for decimal display
  sysunt k@ 377 and 2*			\ pick up SY0: unit number
  maxun2 2+ 0				\ loop through all units
  do untcnt i + k@ dup uc.mnt and 0=	\ get opens & flags; mounted?
   if
    i 2/ 2 .r ." / "			\ print FUN
    dsklog i 5 * + mscbuf 12 getbuf	\ read dsklog table entry for unit
    mscbuf 6 + w@ a.			\ get and display device name
    mscbuf 10 + c@ dup 0 .r		\  and unit number
    10. - 177700 and if ."  " endif	\ unit - 10 is negative so space over
    dup uc.cnt and 5 .r			\ display open file count
    untsiz i + k@			\ get unit size in dcn's
    devclu i + k@ 377 and		\ get unit's dcs
    u* dup 10 u.r			\ compute and print size in blocks
    satctl i + k@ satctm i + k@ 377 and	\ get low and high order free count
    wswap or dup 10 u.r			\ combine them and print free count
    .perc				\ print percentage freespace
    untclu i + k@ 377 and 4 .r		\ print pack cluster size
    unterr i + k@ 6 .r			\ display error count
    2 spaces mscbuf @ 2%. mscbuf 4 + w@ %.  \ display pack id
    untlvl i + k@			\ get the revision level
    dup cswap 377 and 3 .r		\ print major rev
    ascii . emit 377 and 0 .r		\  and minor rev
    ^d65 .pos dup uc.nfs and		\ now check the pack flags
    if ." NFS, Job "			\ NFS is a special case
     untclu i + k@ cswap 377 and 2/ 0 .r \ print job number
    else dup uc.pri and			\ private or noshare?
     if untclu i + k@ cswap 377 and -dup \ yes, is it owned?
      if ." Job " 2/ 0 .r		\ if so, print job number
      else ." Pri"			\ else it's private
      endif
     else over i =			\ public, check for sy0:
      if ." System"			\ if sy0:, say so
      else ." Pub"			\ otherwise it's public
      endif
     endif
    endif
    dup uc.lck and if ." , Lock" endif	\ interpret all those flags
    dup uc.dlw and if ." , DLW" endif
    uc.wlo and if ." , R-O" endif
    untopt i + k@
    dup uo.top and if ." , NFF" endif
    dup uo.cln and if ." , Dirty" endif
    dup uo.ini and if ." , Ini" endif
    dup uo.dp and if ." , DP" endif
    dup uo.ncf and if ." , NCF" endif
    dup uo.ncd and if ." , NCD" endif
    dup uo.wcf and if ." , WCF" endif
    dup uo.wcu and if ." , WCU" endif
    uo.nqt and if ." , NQT" endif
    satmmu i + k@ if ." , LDX" endif	\ check if SATT loaded into memory
    cr
   else drop
   endif
  2 /loop drop base! 			\ finally restore base
  end-popup ;

>util
\ display xbuf free buffer chain
\ pool-base ==>
: (freex) 2+ k@ 0 swap			\ get starting mmu pointer
  ."   Address   Next   Size" cr	\ print header
  begin -dup
   while m->a				\ convert to byte address
    dup ."  " ao. ." / "		\ display it
    dup k@ wo. space			\ display link word
    dup 2+ k@ dup wo. dup 0=		\ display size, check for no size
    if ."    List head"			\  which means list head
    endif rot + swap			\ accumulate pool size
    k@ dup 0=				\ get link to next, check for end
    if ."    End of list"
    endif cr
  repeat
  base@ swap decimal cr			\ save base, force decimal
  ."  Total free space: " m->a dup .	\ display total free count
  ." (" dup ao. ." ) bytes "		\  (in decimal and octal)
  ." = " 64. / . ." buffers"		\  (and the number of buffers)
  base! cr ;

>-words
:h show-xbu
\ The SHOW XBUF operator shows the xbuf pool freelists (both for
\ EXTPOL and LRGPOL).
  ?nosil 
  start-popup
  ?remote if ." Extended buffer pool" endif
  begin-popup
  ." EXTPOL (mapped XBUF pool)" cr cr extpol (freex) cr
  ." LRGPOL (unmapped XBUF pool)" cr cr lrgpol (freex)
  end-popup ;

:h show-lar show xbuf ;
\ The SHOW LARGE_BUFFERS command is a synonym of SHOW XBUF.

>util
\ display free small buffer chain
\ pool-base ==>
: (free) 2+ k@ 0 swap			\ get starting mmu pointer
  ."   Addr    Next   Size" cr		\ print header
  begin -dup
   while dup ."  " wo. ." / "		\ display address
    dup k@ wo. space			\ display link word
    dup 2+ k@ dup wo. dup 0=		\ display size, check for no size
    if ."    List head"			\  which means list head
    endif rot + swap			\ accumulate pool size
    k@ dup 0=				\ get link to next, check for end
    if ."    End of list"
    endif cr
  repeat
  base@ swap decimal cr			\ save base, force decimal
  ."  Total free space: " dup .		\ display total free count
  ." (" dup wo. ." ) bytes "		\  (in decimal and octal)
  ." = " 32. / . ." buffers"		\  (and the number of buffers)
  base! cr ;

>-words
:h show-sma
\ The SHOW SMALL_BUFFERS operator display the state of the small
\ buffer freelists, both the general pool (MONPOL) and the FIP
\ buffer pool.
  ?nosil
  ?cr
  start-popup
  ?remote if ." Small buffer pool" endif
  begin-popup
  ." MONPOL (permanently mapped small buffer pool)" cr cr monpol (free) cr
  ." FIPPOL (FIP buffer pool)" cr cr fippol (free) cr
  end-popup ;

:h show-con
\ The SHOW CONFIGURATION operator shows the interpretation
\ of the system configuration word (X.CON).
  ?nosil
  ?cr
  start-popup
  ." Configuration data"
  local:
  begin-popup
  x.con k@ 
  dup xc$22  and if ." 22-bit addressing" cr endif
  dup xc$qbu and if ." Q-bus" cr endif
  dup xc$cac and if ." Cache memory" cr endif
  dup xc$ecc and if ." ECC memory" cr endif
  dup xc$pam and if ." Parity memory" cr endif
  dup xc$fpp and if ." FPP present" cr endif
  dup xc$fis and if ." 11/40 FIS present" cr endif
  dup xc$cis and if ." CIS present" cr endif
  dup xc$oat and if ." Odd address trap works" cr endif
  dup xc$dsp and if ." Data space in use" cr endif
  dup xc$ids and if ." I/D space hardware present" cr endif
  dup xc$umr and if ." UMRs present" cr endif
      xc$nem and if ." Root-only dump" cr endif
  end-popup ;

>user
:h .fcb
\ interpret an fcb
\ fcbaddress ==>
  ?nosil
  base@
  swap dup
  decimal dup .fcbb cr tab
  mscbuf 40 getbuf buf f$nam 4 + @ buf f$nam @
  buf f$ppn w@ buf f$unt c@ fun->dev .fname
  ascii < emit buf f$prot c@ 0 .r 
  ." >   Size: " buf f$sizl w@ buf f$sizm c@ wswap or 0 u.r
  ." , Clu: " buf f$clus w@ 0 u.r
  ." , Open: " buf f$acnt c@ 0 .r ascii / emit buf f$rcnt c@ 0 .r cr
  follow!				\ set up link following
  40 len !				\ store length
  dup pos !				\ and position
  k@ quan !				\ and link to next
  base! ;

:h .wcb
\ interpret a wcb
\ wcbaddress ==> nextwcbaddress
  ?nosil
  follow!
  40 len !				\ set up structure length
  dup pos !				\ and position
  base@ swap decimal dup .wcbb cr mscbuf 40 getbuf
   ."  Job " buf w$jbno c@ 2/ 0 .r
   ." , Next VBN " buf w$nvbl w@ buf w$nvbm c@ wswap or 0 u.r
   buf w$idx w@
   dup ddnfs and if ." , NFS" endif
   dup ddwlo and if ." , R/O" endif
   dup wc$upd and if ." , Update" endif
   dup wc$ctg and if ." , Contig" endif
   dup wc$lck and 
   if ." , Lock (count " buf w$flag c@ wc$llk and 0 .r ." )"
   endif
       wc$ufd and if ." , UFD" endif
   buf w$flag c@ 
   dup wc$ext and if ." , Extended WCB" endif
   dup wc$dlw and if ." , Update DLW" endif
       wc$nfc and if ." , NFS Clusters" endif
   buf w$wcb w@
   dup wc$rr and if ." , Read-regardless" endif
   dup wc$spu and if
       buf w$idx w@ wc$upd and if ." , Guarded update" else
       ." , Tentative" endif endif
   dup wc$aex and if ." , Special extend" endif
   dup wc$che and if ." , Cache" endif
   dup wc$csq and if ."  sequential" endif cr cr -40 and 
  dup quan !				\ Set address of next
  swap base! ;

:h .file
\ print fcb/wcb data
\ fcbaddress ==>
  ?nosil .fcb cr buf f$wcb w@ -40 and
  begin -dup
   while .wcb
  repeat ;

\ interpret a single mde (mapping descriptor) entry from the jcr
\ assumes that the jcr has been read into jcrbuf first
\ mdeoffset ==>
: .mde
  jcrbuf + dup w@ wo. tab		\ show pointer to base address
  dup 2+ w@ wo. tab			\ show offset
  dup 4 + w@ dup wo. 			\ and descriptor register value
  if tab				\ if APR is active, say what it is
    dup w@ m.phya - dup 20 and		\ get base pointer again, check type
    if ." Job" drop			\ part of job memory
    else r.mctl -			\ RTS or LIB, get base of RTS/LIB block
      dup l.stat + k@ ls.lib and	\ check LIB flag
      if ." LIB "
      else ." RTS "
      endif
      r.name + 2k@ 2%.			\ show the name
    endif
    ^d37 .pos
    dup w@ k@				\ get actual base address
    swap 2+ w@ + m->a ao. 		\ add in offset, display result
  else drop				\ toss MDE offset
  endif cr ;

\ interpret a group of 8 mde's from the jcr
\ assumes that the jcr has been read into jcrbuf first
\ mdeoffset ==>
: .mdes
  ."   Base	Offset	Desc	 What	    Address"
  cr 60 0
  do dup i + .mde 6
  /loop drop ;

>-words
:h show-job
\ The SHOW JOB operator shows detailed information about
\ a specific job.  It produces an interpreted job status
\ display, dumps of the job data structure, and information
\ about files the job has open.  Example:
\	1 SHOW JOB		(shows job 1)
  ?nosil
  dup					\ Save job number for much later
  dup 2* jobtbl + k@ dup 0= 
  if error" ?No such job"
  endif
  start-popup
  ?remote if
    ." Job status for job "
    over base@ decimal swap . base! 
  endif
  begin-popup
  dup rot .jobstat cr
  ." JDB:" cr .jdb cr
  ." JCR:" cr jdb jdjbno c@ j2->jcr .jcr cr
  ." User I MDEs:" cr jcmde .mdes cr
  jcr jcmflg @ dup jm.uds and
  if ." User D MDEs:" cr jcmde mousrd + .mdes cr
    jm.sis and
    if ." Supervisor MDEs:" cr jcmde mosupi + .mdes cr
    endif
  else drop
  endif
  ." Work block:" cr jdb jdwork w@ ls cr
  ." IOB:" cr jdb jdiob w@ dup ls cr
  ." Open files:" cr 20 0
  do dup i 2* + k@ -dup
   if ." Channel " i . dup k@ 377 and
    if dup ddbbuf 40 getbuf
     ddb ddidx c@ 2- devokb + devnam + k@ a. ddb ddunt c@ 0 .r ascii : emit
     cr .ddb cr
    else cr dup w$fcb + k@ -40 and .fcb cr .wcb drop
    endif
   endif
  loop drop
  ." PFB files:" cr
  jdb jdjbno c@ j2->jcr jcpfb + 20 0 
   do dup i 2* + k@ -dup
    if ." Channel " i . i 2 <
     if c->a pf.wcb + k@
     endif dup k@ 377 and
     if dup ddbbuf 40 getbuf
      ddb ddidx c@ 2- devokb + devnam + k@ a. ddb ddunt c@ 0 .r ascii : emit
      cr .ddb cr
     else cr dup w$fcb + k@ -40 and .fcb cr .wcb drop
     endif
    endif
   loop drop 
  end-popup
  follow!				\ Set up for list following
  pos !					\ Save the job number
  1 len !				\ Length is 1
  0 quan !				\ no list following please
 ;

>user
:h .fcblist
\ print fcb list on a given FIP unit
\ fun ==>
  ?nosil dup base@ swap decimal cr dup fun->dev
  dup a. wswap 377 and 0 .r ascii : emit 2* fcblst + k@ -dup 0=
  if ."  None" cr
  else
   begin cr dup k@ swap .file -dup 0=
   until
  endif base!
  follow!				\ set up list following
  pos !					\ set position
  1 len !				\ length
  0 quan ! ;				\ no next

>-words
:h show-fil
\ The SHOW FILES operator shows the list of files currently
\ open on the various disks.
  ?nosil
  start-popup
  ." Open files"
  local:
  begin-popup
  maxunt 1+ 0 do i .fcblist cr loop
  end-popup ;

>user
:h .ccldef
\ print a ccl definition
\ ccl-address ==>
  ?nosil
  follow!				\ set up list following
  dup pos !				\ save position
  40 len !				\ set structure length
  dup k@ quan !				\ and link to next
  dup ?o.s mscbuf 40 getbuf base@ decimal
  mscbuf 4 + w@ mscbuf 2 + w@ - mscbuf 16 + over type
  ascii - emit mscbuf 16 +
  begin 1+ dup c@ 377 =
  until mscbuf 16 + - 2dup =
  if drop drop
  else over - mscbuf 16 + rot + swap type
  endif 
  20. .pos ." = "
  mscbuf fqext + w@ mscbuf fqnam1 + @ mscbuf fqppn + w@
  mscbuf fqdev + @ .fname
  mscbuf fqnent + @ dup 
  42. .pos ." /LINE:" 77777 and . 
  100000 and if  55. .pos ." /PRIVILEGE" endif
  cr base! ;  

\ print a ccl block
\ address ==>
: .ccl ?nosil dup .cclb cr .ccldef follow! ;

>-words
:h show-com
\ The SHOW COMMANDS command displays the list of CCL commands defined
\ on the system.
  ?nosil
  ?cr
  start-popup
  ." CCL List"
  local:
  begin-popup
  ."   Addr   CCL Name      File Name            Line         Privs" cr
  ccllst k@
  begin -dup
   while .ccldef mscbuf w@
  repeat
  end-popup ;

>user
:h .ccls show commands ;
\ The .CCLS command is a synonym of SHOW COMMANDS.

>-words
:h show-ccl show commands ;
\ The SHOW CCLS command is a synonym of SHOW COMMANDS.

>util
\ display pieces of a string according to a mask.  each piece is 7 characters.
\ mask address length ==> upd-mask
: mask.
  0 do
   swap dup 1 and
   if over 7 type
   endif
   2/ swap 7 +
  7 /loop
  drop ;

\ display some or all APRs, according to a mask
\ nonid-mask id-mask ==>
: .mmu
  ?ecrash
  x.con k@ xc$ids and
  if swap
  endif
  drop >r				\ save mask on return stack
  ?cr
  start-popup
  ." Memory management registers"
  local:
  begin-popup
  x.con k@ xc$ids and
  if ." MMR0-3:" mmsr3 mmsr2 mmsr1 mmsr0 4
  else ." MMR0,2:" mmsr2 mmsr0 2
  endif
  0 do tab k@ wo.
  loop cr
  x.con k@ xc$ids and r over cr
  if " SISARx	SISDRx	UISARx	UISDRx	KISARx	KISDRx	" mask.
   " SDSARx	SDSDRx	UDSARx	UDSDRx	KDSARx	KDSDRx " mask.
  else
   " UISARx	UISDRx	KISARx	KISDRx " mask. 
  endif drop
  dup if 140 else 100 endif
  0 do cr
   dup if 14 else 10 endif
   j swap				\ get mask
   0 do
    dup 1 and
    if i j + sisar0 + k@ wo. tab
    endif
    2/
   2 /loop 
   over
   if
    14 0 do
     dup 1 and 
     if i j + udsar0 + k@ wo. tab
     endif
     2/
    2 /loop
    drop
   endif
  dup if 14 else 10 endif
  /loop
  drop cr cr r> drop
  end-popup ;

>-words
: show-mmu-all
  -1 -1 .mmu ;				\ show mmu, all registers

: show-mmu-ker
  ^b1100 ^b110000110000 .mmu ;		\ show mmu, kernel registers only

: show-mmu-sup
  x.con k@ xc$ids 0= 
  if error" ?No supervisor mode hardware"
  endif
  0 ^b000011000011 .mmu ;		\ show mmu, super registers only

: show-mmu-use
  ^b0011 ^b001100001100 .mmu ;		\ show mmu, user registers only

: show-mmu- show-mmu-all ;		\ SHOW MMU <cr> is SHOW MMU ALL

:h show-umr
\ The SHOW UMR operator displays the contents of the Unibus Mapping
\ Registers (UMRs) as saved in the crash dump.  This command is only
\ valid if you're looking at a crash dump file, not if you're looking 
\ at the current memory.
  ?ecrash
  x.con k@ dup xc$umr and
  if
   ?cr
   start-popup
   ." Unibus Mapping Registers"
   local:
   begin-popup
   200 0 do
    i 160 = 
    if 14			\ last line only has 3 UMRs (31. total)
    else 20 
    endif
    0 do
     i j + umr0 + dup 2k@ ao. tab
    4 /loop cr
   20 /loop cr
   end-popup
  else error" ?No UMRs"
  endif ;

:h show-reg
\ The SHOW REGISTERS operator displays the contents of the general
\ registers (R0-R5), stack pointer, part of the stack, and assorted
\ other similar information.  This command is only valid when
\ you're examining a crash dump file, not when you're looking at
\ current memory.
  ?ecrash 
  ?cr
  start-popup
  ." Registers at crash time"
  local:
  begin-popup
  ." R0-R5:" 14 0
  do i r0 + k@ tab wo. 2
  /loop
  cr cr ." CPU ID	CPUERR	  PC     PS	 KSP	 USP" cr
  usersp kernelsp psw pc cpuerr cpuid
  6 0
  do k@ wo. tab
  loop
  cr cr ." Kernel stack:" cr
  kernelsp k@ 4 + wo. ascii / emit
  20 0
  do kernelstack i - k@ tab wo. 2
  /loop
  cr cr ." Code at -6(PC) - 8.(PC):" cr
  pc k@ -2 and 6 max 6 - wo. ascii / emit
  20 0
  do atpc i + k@ tab wo. 2
  /loop
  cr 
  end-popup ;

>user
\ print a decnet device name
\ ddb-address ==>
: .netdev dup ddunt 1- + k@ cswap 377 and swap dup ddflag + k@ 200 and >r
  k@ 377 and >r r devnam + devokb + 2- k@
  dup 2ascii XM =
   if r> drop r> if ." DMR-" else ." DMC-" endif endif
  dup 2ascii XD =
   if swap r> r> if ." DMV-" else ." DMP-" endif
    ddctbl + k@ cswap 377 and ucttbl + 0 >r
    begin 2dup k@ >=
    while swap over k@ - swap 2+ r> 1+ >r
    repeat drop r> 0 .r ascii . emit swap endif
  dup 2ascii XE =
   if r> drop r> drop ." UNA-" endif
  dup 2ascii XH =
   if r> drop r> drop ." QNA-" endif
  dup 2ascii NO =
   if r> drop r> drop ." TTY-" endif
  drop 0 .r ;

>util
\ print out flow control data
\ flags ==>
: .flow 3 and 3 * " No SegMsg???" drop + 3 type ;

\ interpret a pmb, short listing
\ address ==>
: .pmb dup .pmbb p$bufa + k@ -dup
  if cr lx endif ;

\ interpret a pmb, long listing
\ address ==>
: .pmbl dup .pmbb p$bufa + k@ -dup
  if cr lb endif ;

\ display a node list
\ idle-flag first-nob ==>
: .nodelist 
  begin -dup
  while cr dup wo. ." / " mscbuf 40 getbuf
   buf n.name 6 clean 
   buf n.name 6 type buf n.flgs w@ nf.lop and 0=
   if space buf n.addr w@ .nodeid
   endif
   ^d25 .pos buf n.lcnt c@ 5 .r buf n.dely w@ 7 .r ."   "
   startlist
   buf n.flgs w@ dup nf.lop and
   if ." Loop: " buf n.ddb w@ .netdev space
    buf n.nhop w@ -dup if .node space endif
   endif
   swap dup if ?, ." Idle" endif swap
   dup nf.lup and if ?, ." Lookup" endif
   dup nf.end and if ?, ." End" endif
   dup nf.ibp and if ?, ." IBP" endif   \ Inbound proxy
   dup nf.obp and if ?, ." OBP" endif   \ Outbound proxy
       nf.del and if ?, ." Mdl" endif
   buf n.lllb w@ -dup
   if ll.llb - ll.lfl + k@ cswap dup rf.ph4 and
    if ?, ." Phase 4" rf.p4p and if ascii + emit endif space else drop
   endif endif buf n.link w@
  repeat drop ; 

>-words
:h show-nod
\ The SHOW NODES operator displays the active and idle node lists,
\ i.e. the local node, any defined loop nodes, any nodes that have
\ currently active links, and any nodes which have recently had
\ logical links.
  ?net
  ?cr
  start-popup
  ." Node block list"
  local:
  begin-popup
  base@ decimal
  ."   NOB    Name   Addr    Links  Delay  Flags"
  0 nodlst k@ .nodelist 1 idlnob k@ .nodelist
  cr base!
  end-popup ;

:h show-lin        
\ The SHOW LINKS operator displays the list of currently active
\ DECnet links.
  ?net
  ?cr
  base@ decimal
  start-popup
  ." Logical links"
  local:
  begin-popup
  ."   LLB   Link  Node   ULA  LLA   RLA  State Con  "
  ." RIB  Job Lcl Rem LDR RDR LIR RIR" cr
  lltbuf k@ c->a 12 + jcrbuf
  lltend k@ lltvir k@ - 2-		\ compute length of llt
  >r r getbuf llamsk k@			\ pick the mask
  r> 0					\ set up the loop
  do i jcrbuf + w@ dup 1 and		\ check for active link entry
   if -40 and dup wo. ." / "		\ clean up the lsb, display llb ptr
    mscbuf 40 getbuf			\ read the llb
    i 2/ 1+ 2 .r ^d14 .pos		\ display the link number
    buf ll.nob w@ jdbbuf 40 getbuf	\ read the node block
    jdb n.name w@			\ see if we have a name
    if jdb n.name 6 clean		\ if so clean it up
     jdb n.name 6 type			\ and type it
    else jdb n.addr w@ .nodeid		\ otherwise use the node number
    endif
    ^d21 .pos buf ll.ula c@ 4 .r	\ display the ula
    buf ll.lla w@ 6 u.r			\  and the lla
    buf ll.rla w@ 6 u.r			\   and the rla
    ."   " buf ll.sta c@		\ pick up the link state
    10 0 do				\ 8 bits to check
     dup 1 and				\ next bit set?
     if " ???CIDCISCIRCCSRunDIPDIS"	\ string of state settings
      drop i 3 * + 3 type		\ pick out the right one and print it
     endif 2/
    loop
    drop buf ll.try c@ 4 .r		\ show the retry counter (confidence)
    buf ll.lfl w@ buf ll.llx w@		\ pick up 2 things for later
    buf ll.mod c@			\  and another
    buf ll.rib w@ mscbuf 40 getbuf	\ read the rib for this link
    ."   " buf s.rcid 6 type		\ display the receiver id
    buf s.jbno c@ dup 3 =		\ check the job number
    if drop ."  NSP"			\ 3 (1.5) is NSP
    else 2/ 3 .r space			\ else display the real number
    endif
    ."  " dup .flow space 4 / .flow	\ print local and remote flow types
    mscbuf 40 getbuf			\ read the llx block
    buf lx.ldr c@ 3 .r			\ print local request count
    dup 200 and				\ check for local backpressure
    if ascii * emit			\ if so flag it
    else space
    endif
    buf lx.rdr c@ 3 .r			\ print remote request count
    100000 and				\ check for remote backpressure
    if ascii * emit			\ if so flag it
    else space
    endif
    buf lx.lir c@ 3 .r			\ show the local interrupt req count
    buf lx.rir c@ 4 .r cr		\ and the remote count
   else drop
   endif
  2 /loop drop
  cr base!
  end-popup ;

:h show-adj
\ The SHOW ADJACENCIES operator displays the lists of DECnet adjacencies
\ that are currently active, i.e. running or initializing
  ?net
  base@ decimal
  start-popup
  ." DECnet adjacencies"
  local:
  begin-popup
  ."   AVA   Circuit      Node        Bufsiz  Flags"
  adjend k@ adjvir do    \ for all adjacencies,
    i 140000 - 	 \ get adjacency offset
    adjcir k@ 100 u* + k@ -dup \ get circuit 
    if cr i wo. ." / " .netdev ^d16 .pos
     i 140000 - adjadr k@ 100 u* + k@ .node ^d33 .pos
     i 140000 - adjsiz k@ 100 u* + k@ dup 177777 =
     if drop 
     else 6 .r 
     endif
     ^d42 .pos i 140000 - adjflg k@ 100 u* + k@
     startlist
     dup af.are and if ?, ." Area" endif
     dup af.end and if ?, ." End" endif
     dup af.ph4 and if ?, ." Phase 4" endif
     dup af.ini and if ?, ." Init" endif
     drop
   endif
  2 /loop
  cr base!
  end-popup ;

:h show-cir
\ The SHOW CIRCUITS operator displays the lists of DECnet circuits
\ that are currently active, i.e. on or attempting to start.
  ?net
  ?cr
  base@ decimal
  start-popup
  ." DECnet circuits"
  local:
  begin-popup
  ."   DDB   Circuit  State      Node       Bufsiz  Cost  Flags"
  bravir k@ adjvir do    \ for all circuits,
    i 140000 - 	 \ get adjacency offset
    adjcir k@ 100 u* + k@ -dup \ get circuit 
    if cr dup wo. ." / " dup .netdev
     ddbbuf l.ddep getbuf ddb l.flag w@ ^d16 .pos
     dup lf.sta and 2/ " Off- On-Res-???-" drop + 4 type
     dup dup lf.bro and 200 / ^d12 *
     swap lf.lss and 2/ 3 * + " RunIn3VfyIniRunAdrMul???" drop + 3 type
     i 140000 - adjadr k@ 100 u* + k@ .node ^d39 .pos
     i 140000 - adjsiz k@ 100 u* + k@ dup 177777 =
     if drop 
     else 6 .r 
     endif
     ^d48 .pos ddb l.cost c@ 3 .r ."    "
     startlist
     dup lf.ver and if ?, ." Verify" endif
     dup lf.fip and if ?, ." FIP" endif
     dup lf.rst and if ?, ." Restart" endif
     dup lf.ans and if ?, ." Answer" endif
     dup lf.con and if ?, ." Congested" endif
     dup lf.rtm and if ?, ." Route msg pnd" endif
     dup lf.tra and if ?, ." Traced" endif       
     dup lf.lsm and if ?, ." Link sub msg" endif
     dup lf.srm and if ?, ." Send rte msg" endif
     dup lf.bro and if
      dup lf.aen and if ?, ." Send all end" endif
          lf.des and if ?, ." Desig router" endif else
      dup lf.vmw and if ?, ." Verify-wait" endif
          lf.rvm and if ?, ." Rem Vfy" endif     
     endif
   endif
  2 /loop
  cr base!
  end-popup ;

>util
\ print one of the lists that makes up the endnode cache
\ listhead --
: .ecache
  cr ."  Addr        Node          Next Node" cr
  endche k@ m->a swap			\ get base address of cache
  140000 - over + k@			\ get link to first entry
  begin
   -dup					\ end check
  while
   over over 140000 - +			\ form physical address of entry
   mscbuf ec.siz getbuf			\ read the cache entry
   buf ec.adr w@ -dup
   if swap wo. ." / "			\ print cache block virtual address
    .node ^d26 .pos			\ print what this is for
    buf ec.hop w@ .node cr		\ and how to get there
   else drop				\ get rid of cache block address
   endif
   buf ec.nxt w@ 			\ follow the link
  repeat
  drop cr ;				\ pop cache base address

>-words
:h show-rou
\ The SHOW ROUTES operator displays the contents of the DECnet
\ routing database.  For each reachable node, it shows the cost
\ and number of hops to that node, and the adjacency DECnet uses
\ to get there.  The first entry (marked "area") shows that same
\ information for the nearest level 2 (area) router, which is
\ what DECnet uses for traffic out of your area.
\ For endnodes, the content of the endnode cache is displayed.
  ?net
  ?cr
  base@ decimal
  start-popup
  oajmmu k@
  if
   ." DECnet routing database"
  else
   ." DECnet endnode cache"
  endif
  local:
  begin-popup
  oajmmu k@
  if
   ."      Node      Cost Hops    Next Node      Circuit " cr
   maxadr k@ 1+ 2* 0
   do oajmmu k@ 100 u* 
    i + oajvir 140000 - + k@ -dup
    if i
     if myarea k@ i 2/ or .node                 
     else ."      (area) "
     endif ^d16 .pos rteadr k@ c->a i + bufhdr + k@
     dup 1777 and 4 .r 2000 / 4 .r 2 spaces
     dup 140000 - adjadr k@ 100 u* + k@ .node ^d44 .pos
     140000 - adjcir k@ 100 u* + k@ .netdev cr
    endif
   2 /loop
  else
   ." Current cache entries" enccur .ecache
   ." Older cache entries" encold .ecache
   ." Idle cache entries" encidl .ecache
  endif
  ?cr base!
  end-popup ;

>util
\ print rts flags
\ d-flags ==>
: .rtsflags buf r.cnt w@ 100000 and	\ check for "stay"
  if ." , Stay"				\ yup
  endif
  dup pf.kbm and			\ keyboard monitor?
  if ." , KBM"
  endif
  dup pf.1us and			\ single-user?
  if ." , 1-User"
  endif
  dup pf.rw and				\ read-write?
  if ." , R/W"
  endif         
  dup pf.ner and			\ no-errorlogging?
  if ." , No-error"
  endif
  dup pf.rem and			\ remove when not in use?
  if ." , Rem"
  endif
  dup pf.csz and			\ compute-size?
  if ." , CSZ"
  endif
  buf r.mctl m.ctrl + c@		\ get memory control flags
  common out -				\ currently resident?
  over pf.sla and or			\  or assigned specific address?
  if buf r.mctl m.phya + w@ 40 / -dup	\ yes, get the address
   if ." , Addr:" 0 .r			\  and print it, if not zero
   endif
  endif
  dup pf.emt and			\ using prefix EMT?
  if dup 377 and ." , EMT:" co.		\ yes, display the EMT in question
  endif
  drop ;				\ that's all

>-words
:h show-lib
\ The SHOW LIBRARY operator displays the list of installed
\ resident libraries and dynamic regions, showing the library
\ block pointers plus essentially the same information as is
\ displayed by the corresponding DCL command.
  ?nosil
  base@ decimal ?cr
  start-popup
  ." Resident libraries"
  local:
  begin-popup
  ."   Addr   Name   Prot       Acct      Size  Users  Comments" cr
  liblst k@
  begin -dup				\ check if any more
   while dup mscbuf 40 getbuf		\ read the library block
    dup ?o.s				\ print the address
    buf r.name @ 2%.			\ print the name
    ."   <" buf l.prot c@ 3 .r ." >  "	\  and protection code
    buf r.data @ -dup			\ get fbb for library
    if 377 and				\ if it has one, get unit number
     fun->dev .device			\ print device name
    else 4 spaces			\ else blanks
    endif
    buf l.ppn w@ ." [" .ppn
    ^d36 .pos ." ]"			\ print owning ppn
    buf r.mctl m.size + c@ 4 .r ." K"	\ print size in K
    buf r.cnt w@ dup 377 and 4 .r	\ print attach count
    ." /" cswap 177 and 0 .r		\  and map count
    ^d51 .pos buf l.stat w@ ls.dyn and	\ check "dynamic region" flag
    if buf r.flag w@ pf.emt and		\ region, histog style?
     if ." Region (Histog), "		\ yes, show it
     else ." Region, "			\ otherwise just plain region
     endif
    endif
    buf r.mctl m.ctrl + c@ common out =	\ see if not in memory
    if ." Non-Res"			\ it's out
    else ." Temp"			\ otherwise it's temp
    endif
    buf r.flag w@ dup pf.sla and 0=	\ get Rts/lib flags, check for float
    if ." , Floating"
    endif
    pf.1us pf.rw or pf.ner or
    pf.rem or pf.sla or and		\ keep only meaningful flags
    .rtsflags				\ and print those flags
    drop cr buf r.link w@		\ drop pointer, follow link to next
  repeat base!
  end-popup ;

:h show-run
\ The SHOW RUNTIME-SYSTEMS operator displays the list of installed
\ runtime systems, showing the RTS block pointers plus essentially
\ the same information as is displayed by the corresponding DCL
\ command.
  ?nosil
  base@ decimal ?cr
  start-popup
  ." Run-time systems"
  local:
  begin-popup
  ."   Addr   Name   Typ   Dev     Size   Users  Comments" cr
  nulrts
  begin -dup				\ see if any more
   while dup mscbuf 40 getbuf		\ read the rts block
    dup ?o.s				\ print the address
    buf r.name @ 2%.			\ print the name
    buf r.dext w@ space space %.	\  and default extension
    3 spaces buf r.data @ -dup		\ get fbb data for rts
    if 377 and				\ if it has one, get unit number
     fun->dev .device			\ print device name
    endif
    ^d29 .pos
    buf r.mctl m.size + c@ 2 .r		\ print rts size
    buf r.size c@ ." (" 2 .r ." )K"	\  and max user size
    buf r.cnt w@ dup 377 and 4 .r	\ print user count
    ." /" cswap 177 and 0 .r		\  and resident-user count
    ^d45 .pos				\ prepare to print flags
    buf r.mctl m.ctrl + c@ common out =	\ see if not in memory
    if ." Non-Res"			\ it's out
    else dup nulrts =			\ see if this is the fake rts
     if ." Monitor"			\ it is
     else dup rtslst k@ =		\ no, perhaps primary?
      if ." Primary"			\ yes
       dup defkbm k@ =			\  but is it defkbm also?
       if ." , Def KBM"			\ yes, report that too
       endif
      else dup defkbm k@ =		\ not primary, perhaps default kbm?
       if ." Def KBM"			\ yes
       else ." Temp"			\ otherwise it's temp
       endif
      endif
     endif
    endif
    buf r.flag w@			\ get rts flags
    over defkbm k@ =			\ working on default kbm?
    if pf.kbm -				\ if so, don't say KBM twice
    endif
    .rtsflags				\ display the flags
    drop cr buf r.link w@		\ drop pointer, follow link to next
  repeat base!
  end-popup ;

:h show-rec
\ The SHOW RECEIVERS operator displays the lists of declared
\ message receivers.
  ?nosil
  base@ decimal ?cr
  start-popup
  ." Message receivers"
  local:
  begin-popup
  ."   RIB    Name   Job   RIB   Obj  Msgs/Max   "
  ." Links/Inmax/Outmax   Access" cr
  0 nsplst -dup if k@ endif sndlst k@
  begin -dup
  while dup wo. ." / " mscbuf 40 getbuf buf s.rcid 6 type
   buf s.jbno c@ dup 1 and
   if drop ."   NSP"
   else 2/ 4 .r space
   endif buf s.srbn c@ 5 .r buf s.objt c@ 6 .r
   buf s.mcnt c@ 7 .r ascii / emit buf s.mmax c@ 0 .r ^d49 .pos
   buf s.lcnt c@ 3 .r ascii / emit buf s.lmax c@ 0 .r
   ascii / emit buf s.omax c@ 0 .r
   ^d66 .pos buf s.accs c@ dup sa.prv and
   if -2 and
   endif
   startlist
   dup sa.lcl and if ?, ." Local" endif
   dup sa.prv and if ?, ." Priv" endif
   dup sa.net and if ?, ." Net" endif
   dup sa.1sh and if ?, ." Oneshot" endif
   dup sa.ncs and if ?, ." NoCS" endif
   dup 40     and if ?, ." ???" endif
   dup sa.evt and if ?, ." Evtlog" endif
       0=	  if ?, ." None" endif
   buf s.link w@ -dup drop cr
  repeat cr base!
  end-popup ;           

:h show-mem  
\ The SHOW MEMORY command displays the memory list, including
\ both permanent entries (such as the monitor and the primary
\ runtime system) and transient entries (entries that are currently
\ resident but may be swapped out if needed).
  ?nosil
  ?cr
  start-popup
  ." Memory list"
  local:
  begin-popup
  base@ decimal memlst
  begin -dup
  while cr dup wo. ." / " dup mscbuf mcbsiz getbuf
   dup 37 and >r -40 and r
   cond
	0=	<< buf m.phya w@
		   if buf m.pnxt w@
		    if buf m.phya w@ 4000 u<	\ a bit of a hack...
		     if ." Odt"
		     else ." CNA reg" endif
		    else ." Tail" endif
		   else ." Monitor"
	     	   endif >>
	2 =	<< ." Xbuf" >>
	4 =	<< buf m.phya w@ csr.dv w@ =
		   if ." DV0: "
		   else ." Locked" endif >>
	6 =	<< ." NXM" >>
	10 =	<< dup l.stat + k@ ls.lib and
		   if ." Lib" else ." Rts" endif >>
   nocond
	0 lmtcnt k@ 1+ 1
	do over i 2* jobtbl + k@ =
	 if drop i leave
	 endif
	loop -dup
	if ." Job " 0 .r
	else ." ???"
	endif
   condend
   tab buf m.size c@ 4 .r ." K  at " buf m.phya w@ dup 40 / 4 .r
   ." K (" 100 u* ao. ." )  " buf m.ctrl c@ swap jdbbuf 40 getbuf 
   tab r 10 =
   if jdb r.name @ 2%.
   endif r> 20 =
   if jdb jdjbno c@ j2->jcr dup jcname + 2k@ 2%. jcswap +
   else -1
   endif
   swap tab .slot buf m.tsiz w@ buf m.size c@ - -dup
   if cr ." 	Free	" 4 .r ." K"
   endif
   buf m.pnxt w@
  repeat base! cr 
  end-popup ;

>user
:h tag->a
\ The tag->a operator converts a cache tag virtual address into
\ a physical address. For example, 140200 tag->a . will display
\ the memory address referenced by tag address 140200.
 dup 140000 u>= if 140000 - $$chen ch$par + k@ 100 u* + endif ;

>-words
:h show-tag
\ The SHOW TAGS operator shows the contents of the disk data cache.
 ?nosil
 $$chen k@ if				\ if caching is enabled
  start-popup
  ." Cache tags"			\ enter popup mode
  local:
  begin-popup
  base@ decimal				\ save base and set to decimal
."   Tag     Phys    Next   Prev  Dev     Block   Data    DSQ    Type  Age" cr
  $$chen ch$nxt + dup			\ get the base of the cache tags
  begin k@ 2- 2dup -			\ get the next tag and check for the end
   while dup wo. ." / " 		\ display tag virtual address
   tag->a dup ao. space 		\ display physical address
      dup k@ wo. space			\ display link to next @ ch.prv
   2+ dup k@ wo. space			\ display link to previous @ ch.nxt
   2+ dup k@				\ get unit (low) and block msb (high)
    dup 377 and dup 377 -		\ check for "invalidated" fun value
    if fun->dev	.device			\ if not, display device name
    else drop
    endif
    38. .pos
    cswap 377 and wswap			\ get MSB into LSB of high word
   over 2+ k@ or 8. .r space		\ display block number
   4 + dup k@ dup wo. 			\ display address of data
    extpol 2+ k@ u>=			\ check to see what pool
     if ." E " else ." L " endif	\ and display Extpol or Lrgpol
   2+ dup k@ wo. space			\ display root of DSQ busy list
   2+ dup k@				\ get tag type pointer
   dup chenue 2+ =			\ check which tag counter it points to
   if drop ."  Dir   "			\ directory tag
   else dup chenue 4 + =
    if drop ."  Data  "			\ data tag
    else dup chenue 6 + =
     if drop ."  Inv   "		\ invalidated tag
     else wo. space			\ unknown, display the pointer itself
     endif
    endif
   endif
   2+ dup k@ $$chen ch$sec + k@ swap -	\ get the age in seconds
     177777 and 0 u.r cr		\ display the age of the tag
   16 - repeat				\ now loop
  drop drop base!			\ restore previous base
  end-popup				\ end popup mode
 else error" ?Caching disabled" endif ;	\ if no caching, report that

>user
:h find
\ The find operator searches a range of addresses for a specified
\ word in memory. The syntax is: VALUE START END FIND. For example,
\ to search for the value 1234 between locations FOOBAR and BARFOO,
\ type:
\	1234 FOOBAR BARFOO FIND
   swap					\ set up stack for DO loop
   do					\ for the range:
    dup i k@ - 0= if i e cr endif	\ pick up a word and check it
   2 /loop drop ;			\ now loop

>keys
\ Define find key as a synonym for find command.
: $[1~ sdainterpret find ;

helpitem forth
\ SDA is a utility written in the programming language FORTH,
\ and many of the features and command syntax it provides are
\ directly copied from FORTH.  The following is intended to
\ explain some of the basic language features; if you want to
\ do advanced work (for example defining your own SDA operators)
\ you should get a good FORTH textbook to learn more details.
\
\ FORTH is a stack-oriented language that uses postfix operator
\ notation.  This means that (unlike most languages) operators
\ that take numeric operands are placed AFTER the operands, rather
\ than between the two operands.  For example, to add 1 and 2 you
\ would write:
\	1 2 +
\ Operands and operators are separated by spaces.  Operands, such
\ as numbers, symbol names, and results of operators, are pushed
\ on a stack; operators take the top few entries on the stack and
\ operate on them.  If you supply too few operands, you usually
\ get a stack underflow error message (and in any case you'll get
\ unexpected answers!).  If you supply too many, some stuff will
\ just be left on the stack.
\
\ For example, if you have two symbols FOO and BAR, and you
\ want to list the contents of memory from two words after FOO
\ up to BAR, you'd use the L operator.  That takes a starting 
\ address and a byte count.  The starting address is FOO+4, and
\ the count is BAR-FOO-4 or BAR-(FOO+4), so you'd write:
\	FOO 4 + BAR FOO - 4 - L		or
\	FOO 4 + BAR FOO 4 + - L

helpitem introduction
\ SDA is an interactive crash dump analyzer.  It uses the symbols
\ in the SIL you've just specified to pick up data from the
\ crash dump file.  You can ask for interpreted dumps of various
\ information (for example SHOW USERS produces output very much
\ like the corresponding DCL command) or raw or medium-rare types
\ of information (for example 0 E means "examine location 0").
\
\ To get started, you should read about SDA's handling of numbers
\ and symbols (help items NUMBERS and SYMBOLS).  To get a very
\ brief intro to FORTH expression syntax (which is what SDA uses)
\ try HELP FORTH.  Apart from that, all the major operators that
\ display information are provided with help text.  For starters,
\ look at:
\	E (examine word)
\	B (examine byte)
\	L (list block of memory)
\	SHOW (an assortment of commands to display useful data)
\
\ To leave SDA, type EXIT or use Control/Z as usual.
\ You can use ^C anytime to interrupt a display operation in
\ progress.
\
\ Please submit problem reports via PRIORITY 5, "FYI" SPRs to
\ SPR administraction.  Note that SDA is an UNSUPPORTED utility.

>utilf
