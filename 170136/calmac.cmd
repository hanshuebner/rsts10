	.ENABLE SUBSTITUTION, GLOBAL
	.SETS $CL "SY:"			!Command file device/directory
	.SETS $TK "SY:"			!Task image device/directory
	.SETS $OD "SY:"			!ODL file device/directory
	.SETS $LI "SY:"			!OLB device/directory
	.SETS $MP "SY:"			!MAP device/directory
	.SETS $BLDID "MACRO-11 baselevel procedure"	!Created by ""
	.SETS $MMSW "/MM"		!Always have memory management
	.SETT $MAPM			!"                           "

.;
.; Following setting for MAC.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For an RSX-11M/PLUS system"
	.SETT $11MPL
	@MACBLD.BLD

.;
.; Following setting for MACRES.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For an RSX-11M/PLUS system"
	.SETT $11MPL
	.SETF $FCSTK			!Resident-library version if FALSE
	.SETS $GEN "GEN:0:0"		!Resident library parition
	.SETS $LIBOP "RESLIB=LB:[1,1]FCSRES/RO"	!LIBR= line for res versions
	.SETS $MUSW "/MU"		!/MU (MPLUS versions) or /-MU (Others)
	.SETS $TYP1 "RES"		!CMD/ODL file ident "","RES","FSL","VMS"
	.SETS $TYP2 "RES"		!Task/Map suffix "","RES","FSL","VMS"
	.SETS $RSLIB "FCSRES"		!Res library "","FCSRES","FCSFSL"

	@MACBLD.BLD

.;
.; Following setting for MACFSL.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For an RSX-11M/PLUS system"
	.SETT $11MPL
	.SETF $FCSTK			!Resident-library version if FALSE
	.SETS $GEN "GEN:0:0"		!Resident library parition
	.SETS $LIBOP "SUPLIB=FCSFSL:SV"	!LIBR= line for res versions
	.SETS $MUSW "/MU"		!/MU (MPLUS versions) or /-MU (Others)
	.SETS $TYP1 "FSL"		!CMD/ODL file ident "","RES","FSL","VMS"
	.SETS $TYP2 "FSL"		!Task/Map suffix "","RES","FSL","VMS"
	.SETS $RSLIB "FCSFSL"		!Res library "","FCSRES","FCSFSL"

	@MACBLD.BLD
.;
.; Following setting for MAC.EXE for VAX-11 RSX
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For the VAX-11 RSX product"
	.SETT $AME
	.SETS $GENB "GEN:0:177700"	!FULL Partition
	.SETS $TYP1 "VDK"		!CMD/ODL file ident "","RES","FSL","VMS"
	.SETS $TYP2 "VDK"		!Task/Map suffix "","RES","FSL","VMS"

	@MACBLD.BLD

.;
.; Following setting for MACRES.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For the VAX-11 RSX product"
	.SETT $AME
	.SETF $FCSTK			!Resident-library version if FALSE
	.SETS $GEN "GEN:0:0"		!Resident library parition
	.SETS $LIBOP "RESLIB=LB:[1,1]FCSRES/RO"	!LIBR= line for res versions
	.SETS $MUSW "/MU"		!/MU (MPLUS versions) or /-MU (Others)
	.SETS $TYP1 "VRL"		!CMD/ODL file ident "","RES","FSL","VMS"
	.SETS $TYP2 "VRL"		!Task/Map suffix "","RES","FSL","VMS"
	.SETS $RSLIB "FCSRES"		!Res library "","FCSRES","FCSFSL"

	@MACBLD.BLD

.;
.; Following setting for PMA.TSK for PRO/Toolkit under RSX-11M/PLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For the PRO/Toolkit under RSX-11M/PLUS"
	.SETT $PTK
	.SETS $GENB "GEN:0:177700"	!FULL Partition
	.SETS $NAME "PMA"
	.SETS $OLB "PMA"

	@MACBLD.BLD

.;
.; Following setting for PMA.TSK for PRO/Toolkit under VAX-11 RSX
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For the PRO/Toolkit under VAX-11 RSX"
	.SETT $PTK
	.SETS $GENB "GEN:0:177700"	!FULL Partition
	.SETS $NAME "PMAVMS"
	.SETS $OLB "PMAVMS"

	@MACBLD.BLD

.;
.; Following setting for PMAPRO.TSK for PRO/Toolkit under P/OS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For the PRO/Toolkit under P/OS"
	.SETT $PTK
	.SETT $POS
	.SETS $GENB "GEN:0:177700"	!FULL Partition
	.SETS $NAME "PMAPRO"
	.SETS $OLB "PMAPRO"

	@MACBLD.BLD

.;
.; Following setting for MAC.TSK for IAS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For IAS"
	.SETT $IAS
	.SETF $FCSTK			!Resident-library version if FALSE
	.SETS $GEN "GEN"		!Resident library parition
	.SETS $LIBOP "RESLIB=LB:[1,1]SYSRES/RO"	!LIBR= line for res versions
	.SETS $MMSW ""			!IAS TKB does not support /MM switch
	.SETS $MUSW "/MU"		!/MU (MPLUS versions) or /-MU (Others)
	.SETS $RSLIB "SYSRES"		!Res library "","FCSRES","FCSFSL"
	.SETS $NAME "MACIAS"
	.SETS $OLB "MACIAS"
	.SETS $TK "[11,1]"		!Task image device/directory
	.SETS $OD "[11,10]"		!ODL file device/directory
	.SETS $LI "[11,10]"		!OLB device/directory
	.SETS $MP "[111,10]"		!MAP device/directory

	@MACBLD.BLD

	.SETS $TK "SY:"			!Task image device/directory
	.SETS $OD "SY:"			!ODL file device/directory
	.SETS $LI "SY:"			!OLB device/directory
	.SETS $MP "SY:"			!MAP device/directory
.;
.; Following setting for MAC.TSK for RSTS/E
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For RSTS/E"
	.SETT $RSTS
	.SETS $GENB "GEN:0:177700"	!FULL Partition
	.SETS $NAME "MACRST"
	.SETS $OLB "MACRST"

	@MACBLD.BLD

.;
.; Following setting for MACID.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For an I&D space RSX-11M/PLUS system"
	.SETT $11MPL
	.SETT $ID
	.SETS $MUSW "/MU"		!/MU (MPLUS versions) or /-MU (Others)
	.SETS $NAME "MACID"
	.SETF $FCSTK			!Allow EXTTSK
	.SETS $GEN "GEN:0:0"
	.SETS $LIBOP ";"		!LIBR= line (None for I&D versions)
	@MACBLD.BLD

.;
.; Following setting for MACD.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For an RSX-11M/PLUS system - DEBUG VERSION"
	.SETT $11MPL
	.SETT $DBG
	.SETS $NAME "MACD"
	.SETF $FCSTK			!Allow EXTTSK
	.SETS $GEN "GEN:0:0"
	.SETS $LIBOP ";"		!LIBR= line (None for debug versions)
	@MACBLD.BLD

.;
.; Following setting for MACDID.TSK for MPLUS
.;
	.GOSUB SETDEF			!Set defaults non-res,non-multiuser
	.SETS $COM "For an I&D space RSX-11M/PLUS system - DEBUG VERSION"
	.SETT $11MPL
	.SETT $DBG
	.SETT $ID
	.SETS $NAME "MACDID"
	.SETS $MUSW "/MU/PR:0"		!+ /PR:0 for debugging
	.SETF $FCSTK			!Allow EXTTSK
	.SETS $GEN "GEN:0:0"
	.SETS $LIBOP "SUPLIB=FCSFSL:SV"	!LIBR= line (FCSFSL for debug I&D ver)
	@MACBLD.BLD

	.EXIT 1
.;
.; Setup defaults for next invocation of MACBLD.BLD
.;
.SETDEF:
.;
.; The following group of symbols are mutually exclusive
.;
	.SETF $11M			!RSX-11M if true
	.SETF $11MPL			!RSX-11M-PLUS if true
	.SETF $AME			!VAX-11 RSX if true
	.SETF $RSTS			!RSTS/E if true
	.SETF $IAS			!IAS if true
	.SETF $PTK			!PRO-TOOLKIT
.;
.; End of mutually exclusive symbols
.;
	.SETS $NAME "MAC"		!Default CMD/ODL prefix name to MAC
	.SETS $OLB "MAC"		!Default .OLB file name to MAC
	.SETF $POS			!PRO-TOOLKIT UNDER P/OS VERSION
	.SETS $DFLIB "SYSLIB"		!Use LB:[1,1]SYSLIB.OLB
	.SETT $FCSTK			!Non-resident-library version if TRUE
	.SETS $GEN ""			!Resident library parition (Not used)
	.SETS $LIBOP ""			!LIBR= line for res versions (Not used)
	.SETS $GENB "GEN:0:70000"	!Partition (Non-resident library version
	.SETS $MUSW "/-MU"		!/MU (MPLUS RES vers) or /-MU (Others)
	.SETS $TYP1 ""			!CMD/ODL file ident "","RES","FSL","VMS"
	.SETS $TYP2 ""			!Task/Map suffix "","RES","FSL","VMS"
	.SETS $RSLIB ""			!Res library "","FCSRES","FCSFSL"
	.SETF $ID			!Default to not building I&D space ver
	.SETF $DBG			!Default to not building debug version
	.SETS $DEB "DBG"		!Use DBG as debugger
	.RETURN
;  DEC/CMS REPLACEMENT HISTORY, Element CALMACBLD.CMD
;  *4    29-NOV-1988 11:54:23 STEVENS "FIX BUILD FILES FOR IAS"
;  *3    14-AUG-1987 15:33:09 STEVENS "REMOVED EXTRANEOUS CHARACTERS"
;  *2    30-JUL-1987 08:44:55 STEVENS "Remove /MM for MACIASBLD.BLD - TKB on IAS does not support /MM switch"
;  *1     8-APR-1986 18:13:31 SYSTEM "Add .CMD/.ODL builder driver"
;  DEC/CMS REPLACEMENT HISTORY, Element CALMACBLD.CMD
