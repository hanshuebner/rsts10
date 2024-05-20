\ Common Definitions

\ Edit:		Date:		By:
\ 05	      26-JAN-83		MHB/ABC/SJK/SHG/MJG/RTW/GPK/MND/AJK/AWL/WJS

\ COPYRIGHT 1974, 1983 BY
\ DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS.

\ THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND  COPIED
\ ONLY  IN  ACCORDANCE  WITH  THE  TERMS  OF  SUCH  LICENSE AND WITH THE
\ INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR  ANY  OTHER
\ COPIES  THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE AVAILABLE TO ANY
\ OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF  THE  SOFTWARE  IS  HEREBY
\ TRANSFERRED.

\ THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE  WITHOUT  NOTICE
\ AND  SHOULD  NOT  BE  CONSTRUED  AS  A COMMITMENT BY DIGITAL EQUIPMENT
\ CORPORATION.

\ DIGITAL ASSUMES NO RESPONSIBILITY FOR THE USE  OR  RELIABILITY  OF ITS
\ SOFTWARE ON EQUIPMENT WHICH IS NOT SUPPLIED BY DIGITAL.

base @

0 variable dot
: .dsect dot ! ;
: .bsect 1 .dsect ;
: ?b dot @ 1 and -dup
  if dot +! ." Boundary error in " latest id. ."  at " dot @ o. cr
  endif ;
: .fillb dot +! ;
: .fillw ?b 2* .fillb ;
: .val dot @ constant ;
: .blkb .val .fillb ;
: .blkw ?b 2* .blkb ;
: .byte 1 .blkb ;
: .word 1 .blkw ;
: .bit dot @ .blkb ;
: .nobit dot @ .fillb ;

octal

\ XRB and FIRQB sizes

40 constant	FQBSIZ	\ Size of FIRQB in bytes
14 constant	XRBSIZ	\ Size of XRB in bytes

\ Monitor calls

104000	.dsect

	.word	CALFIP	\ Call FIP, with FIRQB loaded
	.word	.READ	\ READ
	.word	.WRITE	\ WRITE
	.word	.CORE	\ Change user memory size
	.word	.SLEEP	\ SLEEP job for n seconds
	.word	.PEEK	\ PEEK at memory
	.word	.SPEC	\ Special function
	.word	.TTAPE	\ Enter TAPE mode
	.word	.TTECH	\ Enable echo
	.word	.TTNCH	\ Disable echo
	.word	.TTDDT	\ DDT submode
	.word	.TTRST	\ Cancel ^O effect
	.word	.TIME	\ Get timing information
	.word	.POSTN	\ Get device's horizontal position
	.word	.DATE	\ Get current date & time
	.word	.SET	\ Set keyword bits
	.word	.STAT	\ Get my statistics
	.word	.RUN	\ RUN a new program
	.word	.NAME	\ Install a new program name
	.word	.EXIT	\ Exit to default run-time system
	.word	.RTS	\ Change to a new run-time system
	.word	.ERLOG	\ Log an error from the run-time system
	.word	.LOGS	\ Check for logical devices
	.word	.CLEAR	\ Clear keyword bits
	.word	.MESAG	\ Message send/receive
	.word	.CCL	\ CCL checker
	.word	.FSS	\ File String Scanner
	.word	.UUO	\ UUO hook
	.word	.CHAIN	\ CHAIN to a new program
	.word	.PLAS	\ Resident library control
	.word	.RSX	\ Enter RSX emulation
	.word	.ULOG	\ ASSIGN/REASSIGN/DEASSIGN device/user logical
	.word	.XPEEK	\ Extended block-mode PEEK
	.word	.READA	\ Asynch read
	.word	.WRITA	\ Asynch write
	.word	.HDRSZ	\ Set job header size

\ FIP (FIRQB @ FQFUN) function code

0	.dsect

	.word	CLSFQ	\ CLOSE an open channel
	.word	OPNFQ	\ OPEN a channel
	.word	CREFQ	\ Create/extend/OPEN a channel
	.word	DLNFQ	\ Delete a file by name
	.word	RENFQ	\ Rename a file
	.word	DIRFQ	\ Directory information
	.word	UUOFQ	\ Process UUO
	.word	ERRFQ	\ Get error message text
	.word	RSTFQ	\ Reset close [all] channel[s except 0]
	.word	LOKFQ	\ Look up a file
	.word	ASSFQ	\ ASSIGN a device
	.word	DEAFQ	\ DEASSIGN a device
	.word	DALFQ	\ DEASSIGN all devices
	.word	CRTFQ	\ Create/extend/OPEN a unique .TMP file on disk
	.word	CRBFQ	\ Create/extend/OPEN A compiled image file on disk

\ .PLAS (FIRQB @ FQERNO) function codes

0	.dsect

	.word	ATRFQ	\ Attach region
	.word	DTRFQ	\ Detach region
	.word	CRAFQ	\ Create address window
	.word	ELAFQ	\ Eliminate address window
	.word	MAPFQ	\ Map address window
	.word	UMPFQ	\ Unmap address window

\ .UUO/UUOFQ Subfunction Codes

decimal -29 octal .dsect

	.byte	UU.TB3	\ -29., Monitor tables part 3
	.byte	UU.SPL	\ -28., One-shot spooling request
	.byte	UU.DMP	\ -27., Online monitor snapshot
	.byte	UU.FIL	\ -26., File utility
	.byte	UU.ATR	\ -25., Read/write file attributes
	.byte	UU.CCL	\ -24., CCL command add/delete
1	.fillb		\ -23., Reserved, Basic-Plus terminating file scan
1	.fillb		\ -22., Reserved, Basic-Plus special run priority
1	.fillb		\ -21., Reserved, Basic-Plus privilege drop/restore
1	.fillb		\ -20., Reserved, Basic-Plus memory lock/unlock
	.byte	UU.LOG	\ -19., Set number of allowed LOGIN's
	.byte	UU.RTS	\ -18., Run-time system & resident library control
	.byte	UU.NAM	\ -17., Set file's run-time system name
	.byte	UU.DIE	\ -16., Special SHUTUP logout
	.byte	UU.ACT	\ -15., Accounting information dump
	.byte	UU.DAT	\ -14., Date/Time changer
	.byte	UU.PRI	\ -13., Priority, run burst, core-maximum changer
	.byte	UU.TB2	\ -12., 2nd part of monitor tables
	.byte	UU.BCK	\ -11., Backup file accounting changer
1	.fillb		\ -10., Reserved, Basic-Plus file string scan
	.byte	UU.HNG	\  -9., Hangup/enable a dataset
	.byte	UU.FCB	\  -8., Get FCB/DDB information
1	.fillb		\  -7., Reserved, Basic-Plus Control/C trap enable
	.byte	UU.POK	\  -6., POKE monitor memory
1	.fillb		\  -5., Reserved, Basic-Plus SEND to terminal
1	.fillb		\  -4., Reserved, Basic-Plus FORCE to terminal
	.byte	UU.TB1	\  -3., 1st part of monitor tables
	.byte	UU.NLG	\  -2., Set number of allowed LOGIN's to 1
	.byte	UU.YLG	\  -1., Set number of allowed LOGIN's to maximum
	.byte	UU.PAS	\  +0., Create an account
	.byte	UU.DLU	\  +1., Delete an account
	.byte	UU.CLN	\  +2., "Clean" a disk pack
	.byte	UU.MNT	\  +3., Disk pack mount/dismount
	.byte	UU.LIN	\  +4., LOGIN
	.byte	UU.BYE	\  +5., LOGOUT
	.byte	UU.ATT	\  +6., ATTACH
	.byte	UU.DET	\  +7., DETACH
	.byte	UU.CHU	\  +8., Change password/quota
	.byte	UU.ERR	\  +9., Get error message text
	.byte	UU.ASS	\ +10., ASSIGN
	.byte	UU.DEA	\ +11., DEASSIGN
	.byte	UU.DAL	\ +12., DEASSIGN all
	.byte	UU.ZER	\ +13., Zero device
	.byte	UU.RAD	\ +14., Read accounting information
	.byte	UU.DIR	\ +15., Get directory information
	.byte	UU.TRM	\ +16., Set terminal characteristics
	.byte	UU.LOK	\ +17., Wildcard directory lookup
1	.fillb		\ +18., Reserved, Basic-Plus old message send
	.byte	UU.CHE	\ +19., Cache enable/disable
	.byte	UU.CNV	\ +20., Convert date/time to ascii
	.byte	UU.SLN	\ +21., Set/clear system-wide logical names
1	.fillb		\ +22., Reserved, Basic-Plus message send/receive
	.byte	UU.SWP	\ +23., Add/remove swap, overlay, error msg files
	.byte	UU.JOB	\ +24., Job creation
	.byte	UU.PPN	\ +25., Wildcard PPN lookup
	.byte	UU.SYS	\ +26., Return job status information
	.byte	UU.KMC	\ +27., Connect KMC-11 to another device
	.byte	UU.PRV	\ +28., Set/Clear/Read privilege bits
	.byte	UU.STL	\ +29., Stall/unstall-system functions
	.byte	UU.PRM	\ +30., Add/remove permanent file, priv'd program
	.byte	UU.3PP	\ +31., Set/clear third-party privilege check
	.byte	UU.CHK	\ +32., Check file access/privilege bit name

\ I/O Related Definitions

\ Handler Indexes

0	.dsect

	.word	DSKHND	\ Disk
	.word	TTYHND	\ Terminal
	.word	DTAHND	\ DECtape
	.word	LPTHND	\ Line printer
	.word	PTRHND	\ Paper tape reader
	.word	PTPHND	\ Paper tape punch
	.word	CDRHND	\ Card reader
	.word	MTAHND	\ Magtape
	.word	PKBHND	\ Pseudo-keyboards
	.word	RXDHND	\ Floppy disk
	.word	RJEHND	\ 2780 RJE
	.word	NULHND	\ Null device
	.word	DMCHND	\ DMC11 device
	.word	AUDHND	\ Auto-dial device -- DN11
	.word	PLTHND	\ X-Y plotter
	.word	DT2HND	\ DECtape II
	.word	KMCHND	\ KMC11 microprocessor
	.word	IBMHND	\ IBM interconnect
1	.fillw		\ Reserved
	.word	DMPHND	\ DMP11/DMV11 device

400	.dsect

	.bit	DDNFS	\ File is non-file-structured
	.bit	DDRLO	\ File is read-locked
	.bit	DDWLO	\ File is write-locked
	.bit	FLGPOS	\ File keeps its own position
	.bit	FLGMOD	\ File accepts modifiers
	.bit	FLGFRC	\ File is byte oriented
	.bit	FLGKB	\ File is interactive
	.bit	FLGRND	\ File is random access

\ Job Unique Low Memory Layout

0	.dsect

30	.fillw		\ Job controlled
15	.fillw		\ Reserved for monitor context use
30	.fillw		\ Reserved for monitor FPP context use
103	.fillw		\ Job's SP stack area
	.val	USRSP	\ Default job SP stack setting
	.word	RSTS-KEY \ Keyword of job's current status
fqbsiz	.blkb	FIRQB	\ File request queue block
xrbsiz	.blkb	XRB	\ Transfer control block
200	.blkb	CORCMN	\ CCL line COMMON
26	.fillw		\ Job controlled
	.word	USRPPN	\ User's assignable PPN
	.word	USRPRT	\ User's assignable protection code
4 4 *	.blkw	USRLOG	\ User's logical device table
	.val	NSTORG	\ End of low memory fixed layout

400	.dsect

	.bit	JFSPRI	\ Job is at special run priority
	.bit	JFFPP	\ Save/restore async FPP context
	.bit	JFPRIV	\ Job has permanent privileges
	.bit	JFSYS	\ Job has temporary privivileges active
	.bit	JFNOPR	\ Job is not yet logged in
	.bit	JFBIG	\ Job can exceed its private memory max
	.bit	JFLOCK	\ Job is not to be swapped

\ Transfer Control Block -- XRB

\ Used by user to initiate an I/O request
(	and for monitor/user data requests. )

xrb	.dsect

	.word	XRLEN	\ Length of I/O buffer in bytes
	.word	XRBC	\ Byte count for transfer
	.word	XRLOC	\ Pointer to I/O buffer
	.byte	XRCI	\ Channel number times 2 for transfer
	.byte	XRBLKM	\ Random access block number -- msb
	.word	XRBLK	\ Random access block number -- lsb
	.word	XRTIME	\ Wait time for terminal input
	.word	XRMOD	\ Modifiers

\ File Request Queue Block

firqb	.dsect

1	.fillb		\ Reserved for returned error code
1	.fillb		\ Reserved byte
	.byte	FQJOB	\ Holds your job number times 2
	.byte	FQFUN	\ Function requested
	.val	FQERNO	\ Error message code and text begin
	.byte	FQFIL	\ Channel number times 2
	.byte	FQSIZM	\ File size in blocks -- msb
	.word	FQPPN	\ Project-programmer number
2	.blkw	FQNAM1	\ 2 word filename in radix 50
	.word	FQEXT	\ 1 word filetype in radix 50
	.word	FQSIZ	\ File size in blocks -- lsb
	.val	FQNAM2	\ 3 word new FILNAM.TYP in radix 50
	.word	FQBUFL	\ Default buffer length
	.word	FQMODE	\ MODE indicator
	.word	FQFLAG	\ Opened file's flag word as returned
	.byte	FQPFLG	\ "Protection code real" indicator
	.byte	FQPROT	\ New protection code
	.word	FQDEV	\ 2 byte ascii device name
	.byte	FQDEVN	\ 1 byte unit number
1	.fillb		\ "Unit number real" indicator
	.word	FQCLUS	\ File cluster size for file creates
	.word	FQNENT	\ Number of entries on directory lookup

\ Run-Time System Unique High Memory Layout

177776	.dsect

-1	.fillw		\ Unavailable word
-1	.blkw	P.SIZE	\ Max size for a user's image in K
-1	.blkw	P.2CC	\ Address of 2 quick Control/C's
-1	.blkw	P.CC	\  "      "  Control/C trapping
-1	.blkw	P.FPP	\  "      "  @244 traps -- FPP
-1	.blkw	P.TRAP	\  "      "  @34 traps -- TRAP
-1	.blkw	P.EMT	\  "      "  @30 traps -- non-monitor call
-1	.blkw	P.IOT	\  "      "  @20 traps -- IOT
-1	.blkw	P.BPT	\  "      "  @14 traps -- ODT or T-bit
-1	.blkw	P.BAD	\  "      "  bad error recovery
-1	.blkw	P.RUN	\  "      "  entry to run a new program
-1	.blkw	P.NEW	\  "      "  a new entry
-1	.blkw	P.STRT	\  "      "  system start
-1	.blkw	P.CRAS	\  "      "  crash recovery
-1	.blkw	P.FIS	\  "      "  @244 traps -- FIS
-1	.blkw	P.MSIZ	\ Min size for a user's image in K
-1	.blkw	P.ISIZ	\ historical ...
-1	.blkw	P.DEXT	\ Default executable extension -- RAD50
-1	.blkw	P.FLAG	\ Flag word describing the RTS
1	.fillw		\ Dummy to get back to the
	.val	p.off	\  starting address of these "vectors"

400	.dsect	\ Flag bits in P.FLAG word

	.bit	PF.KBM	\ RTS is a keyboard monitor
	.bit	PF.1US	\ RTS allows only 1 user -- no sharing
	.bit	PF.RW	\ RTS wants to run mapped read/write
	.bit	PF.NER	\ RTS doesn't want its errors logged
	.bit	PF.REM	\ Unconditional remove from memory on exit
	.bit	PF.CSZ	\ Run size is computed from file size
	.bit	PF.SLA	\ save the load address, M.PHYA
	.bit	PF.EMT	\ RTS wants EMT code in low byte as a prefix

base !
