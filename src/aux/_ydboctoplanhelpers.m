;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; Below helper functions are used by the generated Octo Plans (M files _ydboctoP*.m)
; -------------------------------------------------------------

%ydboctoplanhelpers	;
	QUIT

UNIONALL(inputId1,inputId2,outputId)
	; Helper M function that does UNION ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW %ydboctoz1,%ydboctoz2,%ydboctozmax1,%ydboctozmax2
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 into outputId
	SET %ydboctozmax1=$GET(%ydboctocursor(cursorId,"keys",inputId1,"",""),0)
	FOR %ydboctoz1=1:1:%ydboctozmax1 SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctoz1)=%ydboctocursor(cursorId,"keys",inputId1,"","",%ydboctoz1)
	KILL %ydboctocursor(cursorId,"keys",inputId1,"","")
	; Merge key corresponding to inputId2 into outputId
	SET %ydboctozmax2=$GET(%ydboctocursor(cursorId,"keys",inputId2,"",""),0)
	FOR %ydboctoz2=1:1:%ydboctozmax2 SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozmax1+%ydboctoz2)=%ydboctocursor(cursorId,"keys",inputId2,"","",%ydboctoz2)
	KILL %ydboctocursor(cursorId,"keys",inputId2,"","")
	; Set # of records in output table before returning
	SET %ydboctocursor(cursorId,"keys",outputId,"","")=%ydboctozmax1+%ydboctozmax2
	QUIT

UNION(inputId1,inputId2,outputId)
	; Helper M function that does UNION of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW %ydboctoz,%ydboctoz2,%ydboctozmax,%ydboctozindex,%ydboctozval,%ydboctoid
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozmax=$GET(%ydboctocursor(cursorId,"keys",%ydboctoid,"",""),0)
	. FOR %ydboctoz=1:1:%ydboctozmax DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctoz)
	. . QUIT:$DATA(%ydboctozindex(%ydboctozval))
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(%ydboctoz2))=%ydboctozval
	. . SET %ydboctozindex(%ydboctozval)=""
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	; Set # of records in output table before returning
	SET:$DATA(%ydboctoz2) %ydboctocursor(cursorId,"keys",outputId,"","")=%ydboctoz2
	QUIT

INTERSECTALL(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW %ydboctoz,%ydboctoz2,%ydboctozmax,%ydboctozindex,%ydboctozval,%ydboctoid
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozmax=$GET(%ydboctocursor(cursorId,"keys",%ydboctoid,"",""),0)
	. FOR %ydboctoz=1:1:%ydboctozmax DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctoz)
	. . IF (%ydboctoid=inputId1) DO
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozval))
	. . ELSE  IF +$GET(%ydboctozindex(%ydboctozval)) DO
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozval),-1)
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(%ydboctoz2))=%ydboctozval
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	; Set # of records in output table before returning
	SET:$DATA(%ydboctoz2) %ydboctocursor(cursorId,"keys",outputId,"","")=%ydboctoz2
	QUIT

INTERSECT(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW %ydboctoz,%ydboctoz2,%ydboctozmax,%ydboctozindex,%ydboctozval,%ydboctoid
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozmax=$GET(%ydboctocursor(cursorId,"keys",%ydboctoid,"",""),0)
	. FOR %ydboctoz=1:1:%ydboctozmax DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctoz)
	. . IF (%ydboctoid=inputId1) SET %ydboctozindex(%ydboctozval)=""
	. . ELSE  IF $DATA(%ydboctozindex(%ydboctozval)) DO
	. . . KILL %ydboctozindex(%ydboctozval)
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(%ydboctoz2))=%ydboctozval
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	; Set # of records in output table before returning
	SET:$DATA(%ydboctoz2) %ydboctocursor(cursorId,"keys",outputId,"","")=%ydboctoz2
	QUIT

EXCEPTALL(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW %ydboctoz,%ydboctoz2,%ydboctozmax,%ydboctozindex,%ydboctozval,%ydboctoid,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozmax=$GET(%ydboctocursor(cursorId,"keys",%ydboctoid,"",""),0)
	. FOR %ydboctoz=1:1:%ydboctozmax DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctoz)
	. . IF (%ydboctoid=inputId1) DO
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozval))
	. . ELSE  IF +$GET(%ydboctozindex(%ydboctozval)) DO
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozval),-1)
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	SET %ydboctozsubs="" FOR  SET %ydboctozsubs=$ORDER(%ydboctozindex(%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. FOR %ydboctoz=1:1:%ydboctozindex(%ydboctozsubs) DO
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(%ydboctoz2))=%ydboctozsubs
	; Set # of records in output table before returning
	SET:$DATA(%ydboctoz2) %ydboctocursor(cursorId,"keys",outputId,"","")=%ydboctoz2
	QUIT

EXCEPT(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW %ydboctoz,%ydboctoz2,%ydboctozmax,%ydboctozindex,%ydboctozval,%ydboctoid,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozmax=$GET(%ydboctocursor(cursorId,"keys",%ydboctoid,"",""),0)
	. FOR %ydboctoz=1:1:%ydboctozmax DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctoz)
	. . IF (%ydboctoid=inputId1) SET %ydboctozindex(%ydboctozval)=""
	. . ELSE  KILL %ydboctozindex(%ydboctozval)
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	SET %ydboctozsubs="" FOR  SET %ydboctozsubs=$ORDER(%ydboctozindex(%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(%ydboctoz2))=%ydboctozsubs
	; Set # of records in output table before returning
	SET:$DATA(%ydboctoz2) %ydboctocursor(cursorId,"keys",outputId,"","")=%ydboctoz2
	QUIT

columnkeyUNIONALL(inputId1,inputId2,outputId)
	; Helper M function that does UNION ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as UNIONALL^%ydboctoplanhelpers except that this operates on UNION ALL operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement. The input key structure in
	; this case is of the form
	;	%ydboctocursor(cursorId,"keys",inputId,"","",value1)
	;	%ydboctocursor(cursorId,"keys",inputId,"","",value2)
	;	etc.
	; instead of the usual
	;	%ydboctocursor(cursorId,"keys",inputId,"","",1)
	;	%ydboctocursor(cursorId,"keys",inputId,"","",2)
	;	etc.
	;
	NEW %ydboctoid,%ydboctozsubs,%ydboctozval
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozsubs=""
	. FOR  SET %ydboctozsubs=$ORDER(%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)
	. . IF $INCREMENT(%ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozsubs),%ydboctozval)
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	QUIT

columnkeyUNION(inputId1,inputId2,outputId)
	; Helper M function that does UNION of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as UNION^%ydboctoplanhelpers except that this operates on UNION operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW %ydboctoid,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	FOR %ydboctoid=inputId1,inputId2 DO
	. SET %ydboctozsubs=""
	. FOR  SET %ydboctozsubs=$ORDER(%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozsubs)=1
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	QUIT

columnkeyINTERSECTALL(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as INTERSECTALL^%ydboctoplanhelpers except that this operates on INTERSECT ALL operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW %ydboctozindex,%ydboctozval,%ydboctozval2,%ydboctoid,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. FOR  SET %ydboctozsubs=$ORDER(%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)
	. . IF (%ydboctoid=inputId1) DO
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozsubs),%ydboctozval)
	. . ELSE  IF $GET(%ydboctozindex(%ydboctozsubs)) DO
	. . . SET %ydboctozval2=+$GET(%ydboctozindex(%ydboctozsubs))
	. . . SET:(%ydboctozval>%ydboctozval2) %ydboctozval=%ydboctozval2
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozsubs)=%ydboctozval
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	QUIT

columnkeyINTERSECT(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as INTERSECT^%ydboctoplanhelpers except that this operates on INTERSECT operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW %ydboctozindex,%ydboctoid,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. FOR  SET %ydboctozsubs=$ORDER(%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. . IF (%ydboctoid=inputId1) DO
	. . . SET %ydboctozindex(%ydboctozsubs)=1
	. . ELSE  IF $GET(%ydboctozindex(%ydboctozsubs)) DO
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozsubs)=1
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	QUIT

columnkeyEXCEPTALL(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as EXCEPTALL^%ydboctoplanhelpers except that this operates on EXCEPT ALL operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW %ydboctozindex,%ydboctoid,%ydboctozval,%ydboctozval2,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. FOR  SET %ydboctozsubs=$ORDER(%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. . SET %ydboctozval=%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)
	. . IF (%ydboctoid=inputId1) DO
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozsubs),%ydboctozval)
	. . ELSE  IF $GET(%ydboctozindex(%ydboctozsubs)) DO
	. . . SET %ydboctozval2=+$GET(%ydboctozindex(%ydboctozsubs))
	. . . SET:(%ydboctozval>%ydboctozval2) %ydboctozval=%ydboctozval2
	. . . IF $INCREMENT(%ydboctozindex(%ydboctozsubs),-%ydboctozval)
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	FOR  SET %ydboctozsubs=$ORDER(%ydboctozindex(ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. SET %ydboctozval=+$GET(%ydboctozindex(%ydboctozsubs))
	. QUIT:'%ydboctozval
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozsubs)=%ydboctozval
	QUIT

columnkeyEXCEPT(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	; Same as EXCEPT^%ydboctoplanhelpers except that this operates on EXCEPT operations done in a SELECT
	; statement that is inside the IN operator/clause of a parent SELECT statement.
	; See comment block in "columnkeyUNIONALL" label for input key structure.
	;
	NEW %ydboctozindex,%ydboctoid,%ydboctozsubs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR %ydboctoid=inputId1,inputId2 DO
	. FOR  SET %ydboctozsubs=$ORDER(%ydboctocursor(cursorId,"keys",%ydboctoid,"","",%ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. . IF (%ydboctoid=inputId1) DO
	. . . SET %ydboctozindex(%ydboctozsubs)=1
	. . ELSE  IF $GET(%ydboctozindex(%ydboctozsubs)) DO
	. . . KILL %ydboctozindex(%ydboctozsubs)
	. KILL %ydboctocursor(cursorId,"keys",%ydboctoid,"","")
	FOR  SET %ydboctozsubs=$ORDER(%ydboctozindex(ydboctozsubs)) QUIT:%ydboctozsubs=""  DO
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",%ydboctozsubs)=1
	QUIT
