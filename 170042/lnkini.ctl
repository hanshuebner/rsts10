$JOB/CCL/NOLIMIT/PRI:-16 [170,42]
$!
$!	LNKINI.CTL --	Link INIT modules with SAVRES
$!			and drivers, and SILUS INIT
$!
$!---------------------------------------------------
$!	Note:
$!	The Misc modules (FIP, INITTM, INIPRO and ODT) must
$!	be linked as the first overlay so they are loaded
$!	by the bootstrap loader (INIBOO doesn't have to be here - it
$!	was just a convenient place to put it).  The MSCP phase,
$!	BBR phase, and disk and tape drivers should be next.
$!	Next is the QNA phase.
$!	Next is the one-shot overlay (which contains the logo, one-shot 
$!	code, and the IDENT function).  Everything else follows in no
$!	particular order.
$!
$ASSIGN  D: DK
$ASSIGN  D: SY
$SWITCH RT11
$SIZE 28
$R SY:LINK
INIT.SAV/Z,INIT,INIT=INIT/X/B:#1026/U:#1000/C
INIFIP/c
#ERR.STB/C
INITTY/c
[170,56]SROOT/C
DWDSK/c
ROOT/c
STACOM/c
SILNDX/C
MSCPFX/o:1/c
INIFIM/c
INITTM/c
INIBOO/C
INIGPK/c
ODTPAD,[170,30]ODT/C
INIMCP/O:1/C
[170,43]UQPORT,[170,43]CPH/C
[170,43]DUDSK/C
INIBBR/O:1/C
[170,43]BBRROT,[170,43]BBRSUB/C
[170,48]MUDINT,[170,48]MUDVR/C
INIDSK/O:1/C
[170,43]ECCDSK/C
[170,48]MSDINT,[170,48]MTDINT,[170,48]MMDINT/C
[170,48]MSDVR,[170,48]MTDVR,[170,48]MMDVR/C
DZDSK/c
[170,43]DFDSK,[170,43]DSDSK,[170,43]DKDSK,[170,43]DLDSK/C
[170,43]DMDSK,[170,43]DPDSK,[170,43]DBDSK/C
INIQNA/O:1/C
LOGO,INIONE,INIXMC/O:1/C
INIIDE,INIIDS/C
INIDSI/O:1/C
INICOP/O:1/C
INILOA/c
BLDBOT/C
[170,56]IMA/O:1/C
[170,56]DIA/O:1/C
[170,56]SAV/O:1/C
[170,56]RES/O:1/C
INIPAT/O:1/C
INIINS/O:1/C
INIBLD/O:1/C
INISTA/O:1/C
INILDI/O:1/C
INILDR/O:1/C
INIDEF/O:1/C
INIHAR/O:1/C
INIREF/O:1/C
INIDST/O:1/C
INICLN/O:1
PATCH
$EOD
$R SY:SILUS
INIT.SYS/S,INIT=INIT
$EOJ
