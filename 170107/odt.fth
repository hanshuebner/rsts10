\
\			O D T . F T H
\
\ This program is a FORTH implementation of the standard RSTS utility
\ ODT.TSK, with some enhancements.  The additional features are:
\
\	1. Double-length arithmetic -- all arithmetic (addressing
\	   and expression evaluation) is done in 32-bits
\	2. Access to all of memory -- you get both read and write (CAUTION!!!)
\	   access to all of memory (via .PLAS), not just to lowcore.
\	3. Interpretation of directory links for NFS access.  The D command
\	   (was Control/D in ODT.TSK) works no matter how you're accessing
\	   the directory.
\	4. New K command.  The K command takes a DCN, and prints the
\	   corresponding byte address.  Alternatively, you can type K
\	   when a location is open; this causes the first word of the
\	   DCN addressed by that word to be opened.  Handy when following
\	   DCN pointers in NFS access to directories.
\
\ Edit history:
\
\	10-Jul-91	GPK	Modified to do FBN addressing of NFS disk
\				by default.  (Earlier approach, which is
\				block mode NFS addressing, is still available
\				by /MODE:128 on the open.)
\	23-Jul-91	GPK	Changed the above slightly to accommodate
\				change of plans for ODT.B2S -- addressing
\				now matches the traditional addressing of
\				ODT (with pack label at DCS * 1000(8)) but
\				all of boot cluster starting at 1000 is
\				inaccessible.

\ To compile ODT, you also need COMMON.FTH (a FORTH translation of COMMON.MAC)
\ and CASE.FTH (defines the CASE statement and a few others) in FORTH:

fload forth:common 		\ load rsts definitions 
fload forth:case		\ load case statement definitions 
decimal

\ variables 
0 variable ff		\ file flag, 0 => memory, 1 => file 
0 variable rw		\ read/write flag, 0 => read-only, 1 => r/w
0 variable openmode	\ 0 = nothing open, 1 = byte, 2 = word 
0 variable argflag	\ 0 = no argument, 1 = argument present 
0 variable ;flag	\ 0 = ac2 empty, 1 = ac2 full 
0 variable typemode	\ 0 = number, 2 = numeric byte, 4 = ascii 
			\ 6 = ascii byte, 8 = rad50 word 
0 variable v		\ value of currently open location 
0 variable bufloc	\ place in buffer where that location lives
0 variable operator	\ last operator used 
0 variable relocreg	\ relocation register to use in address display 
0 variable nfsflag	\ 0 = file or ufd open, 1 = nfs device open 
0 variable devclu	\ device clustersize for nfs disk 
0 variable filemode	\ open mode for file
0 variable clumode	\ ditto but forced cluster mode for boot block
0 variable blkmode	\ block mode (/MO:128) specified for nfs disk open
0 variable windowid	\ window id of memory mapping window 
-1 variable currentk	\ mmu address of currently mapped 1k segment 
-1 variable currentrw	\ current r/w flag 

0. 2variable dot	\ currently open location 
0. 2variable ac		\ input number accumulator 
0. 2variable ac2	\ second argument, preceding the ; 
	20 allot	\ room for a bunch of ; arguments 
0. 2variable term	\ current term being processed, to go into ac 
0. 2variable q		\ last value typed 
0. 2variable low	\ low limit of l command 
0. 2variable high	\ high limit of l command 
0. 2variable devname	\ saved nfs device name

0 variable blkbuf 510 allot \ file I/O block buffer
-1. 2variable curblk	\ block currently in buffer

\ pointer to get us back to mainline, 
\ done this way to avoid forward reference 
' quit variable 'main

\ arrays 
0. 2variable reloc 28 allot	\ relocation registers 

\ store string data into the dictionary 
: ," -1 word here c@ 1+ =cells allot ;

\ The next bunch of lines define a data table for the C command
\ (which lists the values returned by Monitor tables parts 1,2,3).
\ Each entry consists of 3 parts: the UUO subcode, the offset
\ in the FIRQB at which the data is returned, and the name of the
\ field.  Negative offsets indicate a byte value, positive offsets
\ a word value.  The data is stored in the dictionary at compile time
\ (with some help from the ," word, above) and interpreted at run-time
\ by the C command handler.  Entries corresponding to a particular
\ UUO subcode must be grouped together; to avoid confusing users,
\ they should be in ascending order of offset, and the UUO codes
\ should be in ascending order of table number.

-3 variable cdata
\ -3 ,	-4 ,	," NULINE
-3 ,	-5 ,	," MAXCNT
-3 ,	6 ,	," DEVCNT
-3 ,	8 ,	," DEVPTR
-3 ,	10 ,	," MEMLST
-3 ,	12 ,	," JOBTBL
-3 ,	14 ,	," JBSTAT
-3 ,	16 ,	," JBWAIT
-3 ,	18 ,	," UNTCLU
-3 ,	20 ,	," UNTCNT
-3 ,	22 ,	," SATCTL
-3 ,	26 ,	," SATCTM
-3 ,	24 ,	," JSBTBL
-3 ,	30 ,	," UNTOWN
-12 ,	4 ,	," FREES
-12 ,	6 ,	," DEVNAM
-12 ,	8 ,	," CSRTBL
-12 ,	10 ,	," DEVOKB
-12 ,	12 ,	," TTYHCT
-12 ,	14 ,	," JOBCNT
-12 ,	16 ,	," RTSLST
-12 ,	18 ,	," ERRCTL
-12 ,	20 ,	," SNDLST
-12 ,	22 ,	," LOGNAM
-12 ,	24 ,	," DEVSYN
-12 ,	26 ,	," MEMSIZ
-29 ,	4 ,	," DDCTBL
-29 ,	6 ,	," UCTTBL
-29 ,	8 ,	," SATEND
-29 ,	10 ,	," UNTLVL
-29 ,	12 ,	," MFDPTR
-29 ,	14 ,	," MAGLBL
-29 ,	18 ,	," LOTTBL
-29 ,	20 ,	," EMLCTL
0 , 0 , 0 ,

\ print the standard ODT error message and abort back to mainline 
\ ==> 
: err ."  ?" sp! rp! cr 'main @ cfa execute ;

\ printout primitives 

\ print an octal byte 
\ value ==> 
: co. 0 <# # # # #> type ;

\ print an octal longword 
\ d-value ==> 
: do. <# 11 0 do # loop #> type ;

\ print an ascii byte 
\ value ==> 
: ca. dup 96 and 0=
  if drop ascii .
  endif emit ;

\ print an ascii word 
\ value ==> 
: a. dup ca. cswap ca. ;

\ print a rad50 word 
\ value ==> 
: %. 0 40 m/ 40 /mod			\ pick up the 3 character codes
  3 0
  do "  ABCDEFGHIJKLMNOPQRSTUVWXYZ$.?0123456789"
   drop + c@ emit
  loop ;

\ The table below is used to dispatch to the appropriate printout
\ routine given the current open mode (as set by the last explicit
\ open command, such as / or \ ).  The dispatching is done by
\ the ".." word, defined next.

' o.	variable '.	\ 0 = print numeric word 
' co.	,		\ 2 = print numeric byte 
' a.	,		\ 4 = print ascii word 
' ca.	,		\ 6 = print ascii byte 
' %.	,		\ 8 = print rad50 word 
' err	,		\ 10 = error 

\ print word/byte in selected mode 
\ value mode ==> 
: .. 2dup 2 and			\ check for a byte mode (2 or 6)
  if 255 and			\ if byte mode, keep only low byte
  endif
  0 q 2!			\ set up Q (last value displayed)
  '. + @ cfa			\ pick up address of display routine
  execute space ;		\  and execute it

\ print mode identifying character.  This routine displays the appropriate
\ address delimiter given the current open mode.  In other words, if you
\ type, say, 100% to open in RAD50 mode, and then do a LF, you'll see
\ 00000000102% XYZ  with % as the address delimiter.

\ mode ==> 
: .mode dup 4 =
  if drop ascii "		\ special-case " (can't be in a string)
  else 2/ " /\~'%?" drop + c@	\ otherwise pick up the appropriate char
  endif emit ;			\ and print it

\ define "next" for machine code definitions
				octal
: next, 12403 , 133 , ;		decimal

\ define word in machine code.  this word is used to define other
\ words whose code is in machine language.  it is followed by the name
\ of the word to define, and the code to generate (each in the form
\ value , ) terminated by next, .

: code create smudge [compile] [ ;

\ perform .xpeek directive 
code xpeek .xpeek , next,

\ perform mapping 
code plas .plas , next,

\ perform .ttddt directive 
code ddt .ttddt , next,

\ perform .read directive 
code read .read , next,

\ perform .write directive 
code write .write , next,

\ perform .uuo directive 
code uuo .uuo , next,

\ conditional error message.  issue an error message if the argument
\ passed to it is true.
\ flag ==> 
: ?err if err endif ;

\ error check after rsts directive.  issue an error message if the
\ byte at FIRQB is non-zero.
\ ==> 
: ?firqb firqb c@ -dup 
  if (err) type
   sp! rp! cr 'main @ cfa execute
  endif ;

\ check for required second argument.  issues an error message if
\ the second argument was not specified, i.e. no ; has been seen.
\ ==> 
: ?; ;flag @ 0= ?err ;

\ check for required argument.  issues an error message if no argument
\ was specified.
\ ==> 
: ?arg argflag @ 0= ?err ;

\ check that a location is currently open.  issues an error message
\ if no location is open.
\ ==> 
: ?open openmode @ 0= ?err ;

\ hairy arithmetic

\ the following routines are used to define the double-length arithmetic
\ needed by ODT that is not part of the standard FORTH double-length
\ arithmetic set.

\ unsigned double multiply, double result.  checks for overflow, i.e.
\ results exceeding 32 bits.
\ du1 du2 ==> du 
: du* >r swap 2dup r u* or ?err u* 2swap over >r u*
  2swap r> r> u* d+ ?err 0 swap d+ ;

\ double and 
\ du1 du2 ==> du 
: dand rot and rot rot and swap ;

\ double or 
\ du1 du2 ==> du 
: dor rot or rot rot or swap ;

\ process the pending operation, yielding result in ac.  this routine is
\ used to deal with the fact that we're using infix operators in odt.
\ it is called whenever an expression value is needed.  it takes the
\ last operator specified (if any), the value preceding it, and the
\ value following it, and invokes the execution routine for that
\ operator, yielding the intended expression result.  if no operator
\ is pending, the result is simply the numeric value just typed.
\ ==> d-ac 
: doterm operator @ -dup		\ get pending operator, if any
  if >r ac 2@ term 2@			\ if we have one, push its arguments
   r> cfa execute			\ and execute it
  else term 2@				\ otherwise pick up the last number
  endif
  2dup ac 2!				\ store that in the accumulator
  0 operator !				\ indicate no pending operator
  0. term 2! ;				\  and no number typed yet

\ get a byte from the console.  this is the low-level character input
\ routine.  it reads a single byte and returns the value.  if end of
\ file is seen, that's turned into a Control/z code.
\ ==> byte 
: getbyt ddt				\ kick into ddt mode
  0 sp@ xrloc !				\ put a 0 on the stack for a buffer
  1 xrlen ! 0 xrci c!			\ we want 1 byte from channel 0
  0 xrmod ! 0 xrbc ! read		\ and read that with no modifier
  firqb c@ 11 =				\ check for eof error
  if drop 26				\ if so, return ^Z character code
  else ?firqb				\ check for any other error
  endif ;

\ map physical memory.  inputs are the physical address and the
\ desired mode of access; output is the virtual address at which
\ that physical address has been mapped.
\ d-address rwflag ==> v-address 
: mapmem firqb fqbsiz erase fqmode ! 2dup 6 0
  do d2/
  loop drop -32 and dup fqsiz ! currentk @ -
  fqmode @ currentrw @ - or
  if 8 fqfil c! windowid @ fqppn c!
   fqsiz @ currentk ! fqmode @ currentrw !
   -4 fqext ! 32 fqbufl ! plas ?firqb 
  endif
  drop 2047 and -24576 + ;

\ get disk data.  input is the byte address; output is the memory
\ address (in the buffer) where the data can be found.
\ if necessary, I/O is done to obtain the data, but if the right
\ block is already in memory this is optimized away.
\ d-address ==> v-address
: mapfile over 511 and			\ compute offset within block
  blkbuf + bufloc !			\ save pointer to data item
  9 0
  do d2/				\ divide by 512 (crude but it works)
  loop
  127 and				\ clear out stray high order bits
  2 xrci c!				\ assume we're read channel 1
  blkmode @				\ see if block mode was selected
  if d1+				\ yes, so 0 = FBN 1
  else
   2dup d0>				\ other than boot block?
   if devclu @ 0 d-			\ change LBN to FBN-1
    2dup d0< ?err			\ must be in range
    d1+					\ now make it FBN
   endif
   2dup curblk 2@ d- or			\ see if we already have that block
   if 2dup d0=				\ nope; check for boot block
    if firqb fqbsiz erase		\ clean up firqb
     devname 2@ fqdev 2!
     clumode @ fqmode !			\ set up firqb
     2 0 (fileopen) ?err		\ open cluster mode on channel 2
     4 xrci c!				\ and read from channel 2 later
    endif
   endif
  endif
  2dup curblk 2@ d- or			\ do we already have that block?
  if 2dup xrblkm c! xrblk !		\ no, prepare to read it
   512 xrlen ! 0 xrbc ! 0 xrmod !
   blkbuf xrloc !			\ set up xrb
   -1. curblk 2!			\ in case the read fails
   read ?firqb				\ try the read
   2dup curblk 2!			\ worked, so save current block number
   d0=					\ was it the boot block?
   if 2 fileclose drop			\ yes, close that channel
   endif
  else 2drop				\ toss the block number
  endif
  bufloc @ ;				\ return address of data item

\ get memory or file contents.  this routine returns the contents of
\ the specified word or byte from whatever we're looking at.  if the
\ address specified is odd, a byte value is returned.  if it is even,
\ a word value is returned (and it's up to the caller to mask out
\ the high byte if a byte value is desired).
\ d-address ==> contents 
: m@ swap dup 1 and >r			\ save the "odd address" flag
  r - swap				\ make address even
  ff @					\ are we looking at disk data?
  if mapfile @				\ yes, get the data from the file
  else 0 mapmem @			\ memory, map it and fetch the data
  endif
  r>					\ check the "odd address" flag
  if cswap 255 and			\ if set, make high low and clear rest
  endif
  dup v ! ;				\ save the result in V

\ store into memory or file.  this routine goes through essentially the
\ same work as the "fetch" routine above, but stores a value instead.
\ it stores a byte or word depending on the current openmode.
\ value d-address ==> 
: m! rw @ 0= ?err			\ check that write access was selected
  ff @					\ accessing disk data?
  if mapfile				\ find data in the file
   curblk 2@ >r >r -1. curblk 2! 	\ save block number, invalidate it
   openmode @ 1-			\ check the open mode
   if !					\ word (2)
   else c!				\ byte (1)
   endif
   r> r> 2dup d0=			\ was this the boot block?
   if firqb fqbsiz erase		\ yes, reopen nfs disk to write it
    devname 2@ fqdev 2!
    clumode @ fqmode !			\ set up firqb
    2 0 (fileopen) ?err			\ open cluster mode on channel 2
    4 xrci c!				\ and write using channel 2
   else 2 xrci c!			\ otherwise use channel 1
   endif
   2dup xrblkm c! xrblk !		\ set up block to write
   512 xrlen ! 512 xrbc ! 0 xrmod !
   blkbuf xrloc !			\ set up xrb
   write ?firqb				\ try the read
   2dup curblk 2!			\ restore block number
   d0=					\ was it the boot block?
   if 2 fileclose drop			\ yes, close that channel
   endif
  else 2 mapmem				\ memory, map it r/w
   openmode @ 1-			\ check the openmode
   if !					\ word (2)
   else c!				\ byte (1)
   endif
  endif ;

\ initialize arrays at start of program 
\ ==> 
: init-arrays reloc 32 erase ;

\ resets of various levels 

\ reset ac etc. upon completion of one argument.  this is used by 
\ things like ; that consume an argument.
: resetac 0 argflag ! 0. ac 2! 0. term 2! 0 operator ! ;

\ reset all arguments upon completion of a command 
: reset resetac 0 ;flag ! ac2 22 erase ;

\ prompt the user, reset state 
\ ==> 
: prompt reset 0 openmode !		\ indicate nothing is open
  restore ?cr ascii * emit ;

\ process operations in input stream.  these are the routines that
\ are invoked by the command dispatcher.  by convention, each has a
\ name of the form X-cmd where X is the command character that
\ invokes this routine.

\ arithmetic routines are all alike.  they invoke doterm to process
\ the previous pending operator (if any), and set up a new pending
\ operator.  the value in "operator" is actually the PFA of the
\ word that does the actual arithmetic (such as d+); doterm will
\ pick up that value, and use execute to perform the operation.

\ + 
: +-cmd doterm 2drop ' d+ operator ! ;

\ - 
: --cmd doterm 2drop ' d- operator ! ;

\ * 
: *-cmd doterm 2drop ' du* operator ! ;

\ & 
: &-cmd doterm 2drop ' dand operator ! ;

\ ! 
: !-cmd doterm 2drop ' dor operator ! ;

\ process a relocation register reference.  this uses the argument just
\ typed to index into the relocation register array, and returns the
\ address of the relocation register.
\ ==> address 
: relocref doterm ?err 8 over u< ?err 2* 2* reloc + ;

\ relocation register reference 
\ a , is processed as <contents of relocation register a> + 
: ,-cmd relocref 2@ ac 2!		\ store register value as lefthand arg
  ' d+ operator ! ;			\ and set + as pending operator

\ display an address using relocation registers.  this routine
\ takes an address and attempts to match it with one of the relocation
\ registers.  if a matching relocation register is found, the address
\ is displayed in r,ooo format; otherwise, the address is displayed
\ by itself.  the rule for choosing a relocation register is: if any
\ relocation register (with a defined, i.e. non-zero, contents) is 
\ <= the argument, it can be used to display the address.  if more than
\ one relocation register is found, the closest one (yielding the smallest
\ offset) is used.
\ d-address ==> 
: .address -1 relocreg !		\ default to nothing found
  2dup 8 0
  do 2over				\ get a copy of the address
   i 2* 2* reloc + 2@			\ pick up the next reloc reg contents
   2over 2over du>=			\ see if arg is >= reloc register
   if d-				\ yes, so compute offset
    2over 2over 2swap du<		\ see if this is a better offset
    if 2swap				\ yes, make it new best offset
     i relocreg !			\  and save register number
    endif 2drop
   else 2drop 2drop
   endif
  loop
  relocreg @ 0>=			\ did we find a reloc register?
  if relocreg @ 0 .r ascii , emit	\ yes, print its number and the ,
  endif
  do. 2drop				\ print the address/offset
  typemode @ .mode space ;		\  and the separator

\ close current location 
: close openmode @			\ is anything open?
  if argflag @				\ yes, was a new value typed?
   if doterm ?err			\ yes, check for being 16-bit value
    dot 2@ m!				\  and store it at current location
   endif
  endif
  reset ;				\ now back to initial state

\ open a location given the address.  the openmode and typemode have
\ already been set up by the caller.
\ d-address ==> 
: open reset				\ first reset all arguments
  2dup dot 2!				\ save the address
  over 1 and minus 2+			\ compute 1 for odd address, else 2
  openmode @ 0=				\ was openmode defaulted?
  if dup openmode !			\ yes, make it word unless odd address
  endif
  openmode @ 1 =			\ opening in byte mode?
  if drop 1				\ yes, force byte size data
  endif
  2- minus 2*				\ word = 0, byte = 2
  typemode @ or typemode !		\ force byte-style output if byte mode
  m@					\ fetch the data
  openmode @ 1 =			\ check for byte mode
  if 255 and				\ yes, isolate low byte
  endif typemode @ .. ;			\ now display the result

\ open location with address display.  same as open, but displays the
\ address first.
\ d-address ==> 
: aopen ?cr 2dup .address open ;

\ open location at given offset from current one, with address display.
\ same as aopen, but the location is specified in terms of offset from
\ the current location, not the actual address.
\ offset ==> 
: +aopen s->d dot 2@ d+ aopen ;

\ convert DCN to block number (NFS block mode block number or LBN, according
\ to current NFS open mode)
\ clustersize d-dcn => d-bn 
: dcn->bn
  blkmode @ if d1- endif		\ in block mode, subtract 1
  ?err					\ check for 16 bit value
  u* ;					\  and multiply by DCS

\ convert directory link value to an address.  this code needs to access
\ the directory cluster map.  it assumes that the the link specified was
\ obtained from the current disk block (i.e. the block of disk data most
\ recently read).  this block must be either a standard directory block
\ or a "table" block (as found in MFD and GFD).  for table blocks, this
\ routine will first read the regular directory block following that table
\ block.  note that the test for table blocks is a little "iffy"; it
\ depends on the assumption ("hope") that the value in the directory
\ clustersize field of the block is a valid clustersize for normal
\ directory blocks, and is NOT a valid clustersize for table blocks.
\ the translation of link word to byte address takes into account the
\ way we're accessing this directory; if the directory is open as a file,
\ the output is an offset within the directory.  if we're looking at
\ a non-file-structured disk, the output is a byte address on the disk.
\ this is based on the nfsflag, set at program startup.  note that
\ calling this routine when you're not looking at a disk or a directory
\ will give nonsensical results.
\ this routine will check the blkmode (/MO:128) flag and adjust the answer
\ it gives accordingly.
\ link ==> d-address
: l->a
  blkbuf 496 + @ 32767 and		\ get the clustersize, clear RDS1 bit
  dup -128 and swap dup 0=		\ see if it's out of range
  swap dup dup minus and - or or	\  or not a power of two
					\ this test uses the fact that:
					\   (x AND (-x)) = x
					\ only if x is a power of 2
  if curblk 2@ d1+ 2dup			\ get current block number, increment
   xrblkm c! xrblk ! 2 xrci c!		\ set up block and channel
   512 xrlen ! 0 xrbc ! 0 xrmod !
   blkbuf xrloc !			\ set up xrb
   -1. curblk 2!			\ in case read fails
   read ?firqb				\ try the read
   curblk 2!				\ save block number we read
  endif
  16 blkbuf 496 + c@			\ assume small clusters, get clusiz
  begin over 16 >			\ if large clustersize
   while 2/ swap 2/ swap		\ halve clusiz, halve divisor
  repeat
  drop					\ get rid of clustersize
  over cswap 2/				\ move cluster number to bottom
  over 2/ 1- and			\ turn divisor into mask, get cluster
  nfsflag @				\ doing nfs disk access?
  if 2* 498 blkbuf + + @		\ yes, get cluster map entry
   devclu @ swap 0 dcn->bn		\  and compute the desired bn
  else blkbuf 496 + c@ * 0		\ else vbn is cluster # * clusiz
  endif
  2over swap cswap 240 and swap /	\ get block within cluster
  0 d+ 512. du*				\ make that into byte offset
  2swap drop 496 and 0 d+ ;		\ add in byte offset for final result

\ open/display in specified mode.  this is used when an open-type
\ operator (such as / ) is used.  if an argument is specified, that 
\ location is opened; if no argument is used we merely display the
\ currently open location in the format indicated by the operator used.
\ mode ==> 
: display argflag @			\ was argument specified?
  if dup typemode !			\ yes, establish typeout mode
   2 and 2/ minus 2+			\ compute 1 for byte mode, 2 for word
   openmode !				\ save that as open mode
   doterm open				\ do pending operator, and open loc
  else ?open				\ else make sure something is open
   v @					\ get the saved value there
   swap ..				\ display in requested mode
  endif ;

\ relocation register setup 
: r-cmd ?;				\ R requires two arguments
  relocref				\ convert second arg to reg pointer
  ac2 2@ rot 2!				\ store first arg value there
  prompt ;				\ and prompt again

\ C command uses the CDATA table to display all the values from
\ the monitor tables calls.  it wanders through the table, issuing
\ a new SYS call whenever a new function code is specified, and
\ then picks up the field at the specified offset and displays it.
: c-cmd 0 cdata				\ initialize column, table pointer
  0 >r					\ push initial (none yet) FQFUN
  begin >r				\ save table pointer
   1- dup 0<				\ decrement column, check for eol
   if drop 3 cr				\ if eol, reinit column and do CRLF
   else 9 emit				\ else send a tab
   endif
   r>					\ retrieve table pointer
   dup 2+ swap @			\ get next table entry, advance ptr
   -dup					\ end of table?
   while dup r> -			\ no, new function code?
    if dup fqfun c! uuo ?firqb		\ yes, issue the UUO
    endif
    >r					\ save away function code
    dup 2+ swap @			\ get next entry, advance ptr
    dup abs firqb +			\ compute address in firqb
    swap 0<				\ byte mode (negative offset) ?
    if c@				\ yes, fetch byte
    else @				\ no, fetch a word
    endif
    over count				\ get name pointer, get length
    dup 1+ =cells >r			\ make it even, save that
    swap over type			\ type the name
    7 swap - spaces			\ some spaces to line it up
    o.					\  and type the value
    r> +				\ point beyond the name
  repeat				\ and repeat until end of table
  r> drop				\ drop the function code
  prompt ;				\ done, reprompt

\ the K command operates in two different ways depending on whether
\ an argument is supplied.  if an argument is supplied, it interprets
\ that argument as a DCN and displays the corresponding byte offset.
\ the DCN used is that of the currently open disk (assuming a disk is
\ open in nfs mode) if one argument was supplied, or the second
\ argument if two arguments were supplied.
\ if no argument is supplied, a location must be open, and a disk must
\ be open in NFS mode.  the contents of the location is interpreted
\ as a DCN, using the disk's DCS.  this is converted to a byte address
\ (the first word of that block) and that location is then opened.
\ this operation checks the blkmode (/MO:128) flag and adjusts the
\ address used accordingly.
: k-cmd argflag @			\ any arguments at all?
  if ;flag @				\ yes, two arguments supplied?
   if doterm ?err			\ yes, second is dcs, must be 16 bit
   else doterm ac2 2!			\ else get argument and copy it
    devclu @				\ and use disk's dcs
   endif
   dup 128 u>= ?err			\ error if dcs >= 16
   dup 2dup minus and - ?err		\ error if dcs not a power of 2
   ac2 2@ dcn->bn			\ now convert argument to bn
   512. du*				\ times 512 makes it a byte offset
   2dup q 2!				\ save that as last value printed
   space do.				\  and print it
   prompt				\   and finally prompt again
  else ?open				\ no argument, require open location
   nfsflag @ 0= ?err			\ error if not nfs disk
   devclu @				\ get the disk dcs
   v @ 0 dcn->bn			\ get saved value, convert to bn
   512. du* aopen			\ convert to byte address, and open
  endif ;

\ special values 

\ q command  -- last value displayed
: q-cmd q 2@ term 2!			\ make that the current value
  1 argflag ! ;				\ and indicate an argument is present

\ . command -- last open location
: .-cmd dot 2@ term 2!			\ make it current value
  1 argflag ! ;				\ and indicate argument present

\ display value of expression 
: =-cmd doterm				\ get the expression value
  space 2dup do.			\ print it in octal
  9 emit decimal			\ set decimal radix
  dup 0<				\ is it negative?
  if 2dup 0 du.r 9 emit			\ yes, so print unsigned value first
  endif
  0 d.r					\ print signed decimal value
  octal prompt ;			\ reset radix and prompt

\ the D command follows a directory link.  if an argument is given that
\ value is taken as the link value.  if not, a location must be open.
\ that location's contents is used as the link value.  in either case,
\ the currently (or last) open location must be within a directory block.
: d-cmd argflag @			\ argument present?
  if doterm ?err			\ yes, get it and check for 16 bits
  else ?open v @			\ otherwise, check for open, get value
  endif
  ff @ 0= ?err				\ error if not looking at a file
  l->a aopen ;				\ convert link to address and open it

\ list a range of addresses in the current open mode.  if no arguments
\ are specified, the same pair of addresses is used as on the last L command.
\ if only one argument is supplied, that is the new high limit.
: l-cmd ?cr ;flag @			\ two arguments supplied?
  if ac2 2@ low 2!			\ yes, first arg is low address
  endif
  argflag @				\ any arguments?
  if doterm high 2!			\ yes, second (or only) is top address
  endif
  high 2@ d1+ low 2@			\ get end +1, start
  2do 2i .address			\ display first address for this line
   2i' 2i d-				\ see how many bytes are left
   2dup 16. du>				\ more than one line's worth?
   if 2drop 16.				\ yes, use 16 then
   endif
   0.					\ loop from 0 to bytes/line
   2do 2i 2j d+				\ compute address of data item
    m@					\ fetch it
    typemode @ ..			\ type it in the current mode
    2 typemode @ 2 and 2/ - 0		\ compute 1 if byte mode, 2 if word
   2/loop				\ loop for next item on the line
   cr 16.				\ new line, and loop for next line
  2/loop prompt ;			\ finally prompt

\ the W command expects three arguments.  the first argument is the
\ starting address; the second the ending address, and the third the
\ value to look for.  the comparison is done on words (16 bits) without
\ any mask.
: w-cmd ?arg ?;				\ argument and ; are required
  doterm ?err				\ get last arg, check for 16 bits
  ?cr ac2 2@ d1+ ac2 4 + 2@		\ get end +1 and start addresses
  2do 2i m@ 2dup =			\ fetch a word, compare
					\ note that we still have both the
					\ desired value and what was found
					\ on the stack.  this will be
					\ needed when we implement masked
					\ searches...
   if ?cr 2i .address			\ if equal, print the address
    typemode @ ..			\ and display what we found
   else drop				\ otherwise discard what we found
   endif 2.				\ and go to next word
  2/loop prompt ;

\ each of the following "display" commands use the common display
\ routine, simply passing it a different value for the desired typemode.

\ open/display in word mode 
: /-cmd 0 display ;

\ open/display in byte mode 
: \-cmd 2 display ;

\ open/display in ascii word mode 
: "-cmd 4 display ;

\ open/display in ascii byte mode 
: '-cmd 6 display ;

\ open/display in rad50 mode 
: %-cmd 8 display ;

\ open location pointed at by current one 
: @-cmd ?open				\ make sure something is open
  v @ 0					\ get the address to open
  aopen ;				\  and open it

\ open location using pc-relative displacement 
: _-cmd ?open				\ make sure something is open
  v @ 2+				\ get current value + 2
  +aopen ;				\ use as displacement

\ open location using branch displacement 
: >-cmd ?open				\ make sure something is open
  v c@					\ get the low byte
  cswap 128 /				\ sign extended * 2
  2+ +aopen ;				\ plus 2 is the displacement to use

\ open previous location 
: ^-cmd ?open openmode @ minus +aopen ;	\ back up by 1 or 2 depending on mode

\ open next location 
: lf-cmd close openmode @ dup +aopen ;	\ forward by 1 or 2 depending on mode

\ close current location 
: cr-cmd close				\ close current location
  getbyt drop prompt ;			\  "eat" the line feed, and prompt

\ the ; command takes the argument just typed and puts it in the
\ "second" accumulator (ac2), moving things already in there out of
\ the way.  ac2 is actually an array that can hold 10 items...
: ;-cmd doterm				\ get the value typed
  ac2 dup 4 + 20 <cmove			\ move current ac2 contents over
  ac2 2!				\ store new value at start of ac2 list
  1 ;flag !				\ indicate ; was seen
  resetac ;				\ and init argument accumulation

\ getcmd is the main command dispatcher.  it picks up a single character
\ from the terminal and acts on it.  control/z causes an exit.
\ escape is handled as if it were a $ (which by the way is not currently
\ an odt command but might become one some day).  rubout cancels
\ the incomplete command and reset to prompt in the inimitable
\ odt way.  digits (octal ones, of course) are accumulated into a
\ number.  letters are converted to lower case.  letters and punctuation
\ characters are then dispatched to the appropriate command dispatch
\ routine via a giant select statement.
: getcmd octal prompt			\ prompt for input
  begin getbyt				\ get a character
   dup 26 =				\ control/z?
   if flush -14 fileclose drop bye	\ it was, close files and exit
   endif
   dup 27 =				\ escape?
   if drop ascii $			\ yes, translate to $
   endif
   dup ascii ` =			\ alternate (VT200 series) "escape"?
   if drop ascii $			\ yes, translate to $ also
   endif		
   dup 32 =				\ space?
   if drop ascii +			\ yes, that's + by odt rules
   endif
   dup 127 = ?err			\ rubout is the standard reset
   dup base @ digit			\ is it an (octal) digit?
   if 0 term 2@ dup			\ yes, get term accumulator
    -8192 and ?err			\ error if we're going to overflow
    d2* d2* d2*				\ old value times 8 ...
    d+ term 2!				\  plus new digit makes new value
    1 argflag ! drop			\ mark argument present
   else					\ not a digit, should be command
    dup 96 u>=				\ check for lowercase
    if 32 -				\ yup, upcase it
    endif
    sel					\ massive dispatch time...
     13 -> cr-cmd			\ close a location
     10 -> lf-cmd			\ open next
     ascii C -> c-cmd			\ display monitor table pointers
     ascii D -> d-cmd			\ follow directory link
     ascii K -> k-cmd			\ follow dcn pointer
     ascii L -> l-cmd			\ list a range of addresses
     ascii Q -> q-cmd			\ last value typed
     ascii R -> r-cmd			\ set relocation register
     ascii W -> w-cmd			\ find something ("?where?")
     ascii , -> ,-cmd			\ reference relocation register
     ascii . -> .-cmd			\ currently open location
     ascii / -> /-cmd			\ open a location
     ascii \ -> \-cmd			\ open in byte mode
     ascii " -> "-cmd			\ open in ascii word mode
     ascii ' -> '-cmd			\ open in ascii byte mode
     ascii ! -> !-cmd			\ "or" operator
     ascii & -> &-cmd			\ "and" operator
     ascii % -> %-cmd			\ open in rad50 word mode
     ascii ^ -> ^-cmd			\ open previous location
     ascii @ -> @-cmd			\ open indirect
     ascii _ -> _-cmd			\ open pc relative
     ascii > -> >-cmd			\ open by branch displacement
     ascii + -> +-cmd			\ add operator
     ascii - -> --cmd			\ subtract operator
     ascii * -> *-cmd			\ multiply operator
     ascii = -> =-cmd			\ display value of argument
     ascii ; -> ;-cmd			\ argument delimiter
    nosel err				\ no match -- error
    selend
   endif
  again ;				\ and repeat until we exit!

\ get input line from console 
\ ==> delimiter
: getkb tib @ 80 erase			\ clear out the buffer
  xrb xrbsiz erase			\ initialize the xrb
  tib @ xrloc ! 79 xrlen !		\ read into tib buffer, 79 chars max
  read					\ do the .read
  firqb c@ 11 =				\ end of file (control/z) ?
  if bye				\ if so, exit odt
  endif
  ?firqb				\ check for other error
  xrbc @ 1- tib @ + c@			\ look at last char on the line
  dup 10 =				\ line feed?
  if xrbc @ 2- tib @ + c@		\ yes, get char before it
   13 =					\ is that carriage return?
   if drop 13				\ yes, so cr was the delimiter
   endif
  endif 0 in ! ;			\ initialize line buffer pointer

\ prompt for input file spec.  if none is specified, we look at memory.
\ if the delimiter is carriage return, read only access is used; if
\ line feed, read-write access is used.
: getfile 1 fileclose drop		\ close channel 1 to be sure
  restore cr				\ cancel ^O, new line
  ." ODT	" 0 (err) type cr cr	\ print out banner
  ." File <MEMORY>? "			\ prompt
  0 rw ! 0 nfsflag !			\ assume read only, not a nfs disk
  getkb dup 10 =			\ read a line, check for line feed
  if 1 rw !				\ yes, set read/write flag
  endif
  1 word here count over		\ pick up what was typed
  c@ 32 <				\ first byte = control char?
  if 0 ff ! 2drop			\ yes, so null answer => memory
   firqb fqbsiz erase			\ clear out the firqb
   4 fqfil c!				\ function = crafq
   5 fqppn 1+ c!			\ base apr = 5 (just below forth.rts)
   32 fqnam1 2+ !			\ window size = 1 apr
   rw @ 2* fqmode !			\ set window mode (r/w = 2, r/o = 0)
   plas					\ set up the window for memory access
   ?firqb				\ ... it'd better work
   fqppn c@ windowid !			\ save the window id
  else (filename) ?err			\ filename specified, parse it
   xrbc @ ?err				\ quit if partly unparsed
   xrblk @ dup -30908 and ?err		\ error if wildcards or other junk
   4096 and
   if xrmod @ dup 29184 and ?err 0< 0= ?err
   endif
   1 devclu !		    		\ default to fake DCS of 1
   1 blkmode !		  		\ and don't adjust addresses
   fqmode @ dup clumode ! filemode !	\ and save specified mode, twice
   xrbc @ ?err xrmod c@ 0=
   if xrblk @ 129 and 0= dup nfsflag !	\ check directory and name flags
    if 
     fqmode @ dup 128 and blkmode !	\ save "/MO:128" flag
     dup -127 and dup clumode !		\ save mode value for cluster mode open
     fqmode !				\ prepare to open in cluster mode
     -32640 or filemode !		\ save mode value we want for later
     fqdev 2@ devname 2!		\ save device name
     1 0 (fileopen) ?err fqbufl @ 0 512 u/ devclu !
     0 fileclose drop 
    endif
   endif 1 0 (fileopen) ?err 1 ff !
  endif ;

\ mainline 
: main init-arrays ' getfile 'main ! getfile ' getcmd 'main ! getcmd ;
