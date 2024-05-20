10	DIM M%(30%) &
\	PRINT "Adding Histogram Dynamic region" &
\	M$=SYS(CHR$(6%)+CHR$(-18%)+CHR$(24%)+CHR$(0%)+CVT%$(0%)+ &
		MID(SYS(CHR$(6%)+CHR$(-10%)+"HISTOG"),7%,4%)+ &
		CVT%$(0%) + &
		CHR$(68%) +		! Required size of region &
		STRING$(3%,0%)+CHR$(1%)+CHR$(128%)+CHR$(0%)+CHR$(128%)) &
\	END
