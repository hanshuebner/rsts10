;
; THIS COMMAND FILE ASSEMBLES TKBUTL MODULES.
;
; In task data structures, overlay runtime system, and supermode support
;
;	REG	25-JUL-87	UPDATE FOR LATEST RSX SOURCES
;
SYSTEM:ALERR	,LST:ALERR/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,UTL:ALERR
SYSTEM:AUTO.DGE,LST:AUTO /-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,UTL:AUTO
SYSTEM:AUTOT.DGE,LST:AUTOT/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,UTL:OVAST,AUTO
;SYSTEM:AUTOX.DGE,LST:AUTOX/-SP/CR=RSX:COMMON,UTL:AUTOX
;SYSTEM:AUTOY.DGE,LST:AUTOY/-SP/CR=RSX:COMMON,UTL:AUTOY
SYSTEM:AUTOA,LST:AUTOA/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:AUTOA
SYSTEM:CMPAL,LST:CMPAL/-SP/CR=RSX:COMMON,UTL:CMPAL
SYSTEM:LOAD.DGE,LST:LOAD /-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:LOAD
SYSTEM:OVCTL.DGE,LST:OVCTL/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:OVCTL
SYSTEM:OVCTR.DGE,LST:OVCTR/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:PLAS,OVCTL
SYSTEM:OVCTC.DGE,LST:OVCTC/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:CLUST,OVCTL
SYSTEM:OVIDL.DGE,LST:OVIDL/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:OVIDL
SYSTEM:OVIDR.DGE,LST:OVIDR/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:PLAS,OVIDL
SYSTEM:OVIDC.DGE,LST:OVIDC/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:CLUST,OVIDL
SYSTEM:FSTMAP.DGE,LST:FSTMAP/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:FSTMAP
SYSTEM:OVFCTL.DGE,LST:OVFCTL/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:OVFCTL
SYSTEM:OVFCTR.DGE,LST:OVFCTR/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:PLAS,OVFCTL
SYSTEM:OVFCTC.DGE,LST:OVFCTC/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:CLUST,OVFCTL
SYSTEM:OVFIDL.DGE,LST:OVFIDL/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:OVFIDL
SYSTEM:OVFIDR.DGE,LST:OVFIDR/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:PLAS,OVFIDL
SYSTEM:OVFIDC.DGE,LST:OVFIDC/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,PLSDF,UTL:CLUST,OVFIDL
SYSTEM:OVDAT	,LST:OVDAT/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,UTL:OVDAT
SYSTEM:OVRES	,LST:OVRES/-SP/CR=RSX:COMMON,TKB:MACFLM,SEGDF,UTL:OVRES,OVDAT
SYSTEM:SUPL	,LST:SUPL /-SP/CR=RSX:COMMON,UTL:SUPL
SYSTEM:VCTDF	,LST:VCTDF/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:VCTDF
;SYSTEM:VCTDF	,LST:VCTDF/-SP/CR=RSX:COMMON,EX:[1,1]EXEMC/ML,TKB:MACFLM,UTL:VCTDF
SYSTEM:VEXT	,LST:VEXT /-SP/CR=RSX:COMMON,UTL:VEXT
SYSTEM:VEXTA	,LST:VEXTA/-SP/CR=RSX:COMMON,UTL:VEXTA,VEXT
;
; Conversion routines
;
SYSTEM:CATB	,LST:CATB/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:CATB
SYSTEM:CAT5	,LST:CAT5/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:CAT5
SYSTEM:CAT5B	,LST:CAT5B/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:CAT5B
SYSTEM:CBTA	,LST:CBTA/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:CBTA
SYSTEM:C5TA	,LST:C5TA/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:C5TA
SYSTEM:CDDMG	,LST:CDDMG/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:CDDMG
SYSTEM:CVTUC	,LST:CVTUC/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:CVTUC
SYSTEM:EDDAT	,LST:EDDAT/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:EDDAT
;SYSTEM:EDDAT.SUP,LST:EDDAT.SUP/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:SUP,EDDAT
SYSTEM:INTDAT.DGE,LST:INTDAT/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:INTDAT
;SYSTEM:INTDAT.SUP,LST:INTDAT.SUP/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:SUP,INTDAT
SYSTEM:EDTMG	,LST:EDTMG/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:EDTMG
;SYSTEM:EDTMG.SUP,LST:EDTMG.SUP/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:SUP,EDTMG
SYSTEM:HLPSUB,LST:HLPSUB/-SP/CR=RSX:COMMON,UTL:HLPSUB
SYSTEM:PGLEN	,LST:PGLEN/-SP/CR=RSX:COMMON,UTL:PGLEN
SYSTEM:RQLCB	,LST:RQLCB/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:RQLCB
SYSTEM:SAVAL	,LST:SAVAL/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:SAVAL
SYSTEM:SAVRG	,LST:SAVRG/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:SAVRG
SYSTEM:SAVVR	,LST:SAVVR/-SP/CR=RSX:COMMON,TKB:MACFLM,UTL:SAVVR
