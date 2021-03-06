/* List the files on the floppy (with RootDir.EXE) and apply the long names to the files as .LONGNAME EAs */
CALL RXFUNCADD "SYSLOADFUNCS","REXXUTIL","SYSLOADFUNCS"
CALL SYSLOADFUNCS
SAY "Fetching information..."
"@ROOTDIR|RXQUEUE"
SAY "Applying EAs..."
DO WHILE QUEUED()>0
	PARSE PULL BASE 9 10 EXT 13 14 LONGNAME
	SHORTNAME=STRIP(BASE)"."STRIP(EXT)
	IF LONGNAME\="" THEN DO
		SAY SHORTNAME"->"LONGNAME
		CALL SYSPUTEA "A:\"SHORTNAME,".LONGNAME","FDFF"x||REVERSE(D2C(LENGTH(LONGNAME),2))LONGNAME
	END
END
