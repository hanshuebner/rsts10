$!
$!	BLDBAS.COM - This command procedure will
$!		     build BASIC-PLUS.
$!
$!	NOTE - This procedure must be run from [170,31]
$!
$ _On ERROR then _Exit
$! _Login [170,31]
$ _Deassign/All
$ _Assign D: SY
$ _Set Noon
$ _Set Noecho
$ _Rename/Nolog/Noreplace SY:COMMON.MAC COMMON.BPL
$ _Set Echo
$ _Copy/Replace CMN:COMMON.MAC SY:*.*
$ _@SY:ASMBAS.CTL
$ _Set Job/Keyboard=DCL
$ _@SY:ASMATH.CTL
$ _Set Job/Keyboard=DCL
$ _@SY:LBRBAS.CTL
$ _Set Job/Keyboard=DCL
$ _@SY:LNKBAS.CTL
$ _Set Job/Keyboard=DCL
$ _Exit
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            