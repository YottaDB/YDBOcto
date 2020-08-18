;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	;
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

max(isString,a,b)
	; return the greatest of a and b
	; uses lexicographical sorting if `isString` is true; otherwise uses numerical sorting
	; if `a` is NULL, returns `b` (even if b is NULL)
	; invoked by GREATEST
	QUIT $SELECT($ZYISSQLNULL(a):b,$ZYISSQLNULL(b):a,isString:$SELECT(a]b:a,1:b),1:$SELECT(a>b:a,1:b))

min(isString,a,b)
	; return the least of a and b (see `max` for details)
	; invoked by LEAST
	QUIT $SELECT($ZYISSQLNULL(a):b,$ZYISSQLNULL(b):a,isString:$SELECT(a]b:b,1:a),1:$SELECT(a>b:b,1:a))

dollarZTRIGGER(arg1,arg2);
	; Helper M function invoked by generated M code whenever it needs to do a $ZTRIGGER call.
	; $ZTRIGGER invocation can cause output like the following.
	;	Added SET and/or Non-SET trigger on ^names named %ydboctoTOMMMsQ8ks3NI1C42wS8
	; But we do not want this output to confuse the Octo user who is expecting some query results.
	; Therefore redirect this output to a file by opening a file and switching to it as the current device
	; before the $ZTRIGGER. Currently, we don't know what to do with this output so we redirect to "/dev/null".
	; This might need to be changed at a later point.
	NEW file,status
	SET file="/dev/null"
	OPEN file:(newversion)
	USE file
	SET status=$ZTRIGGER(arg1,arg2)
	CLOSE file
	QUIT status

UNIONALL(inputId1,inputId2,outputId)
	; Helper M function that does UNION ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z1,z2,zmax1,zmax2
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 into outputId
	SET zmax1=$GET(%ydboctocursor(cursorId,"keys",inputId1,"",""),0)
	FOR z1=1:1:zmax1 SET %ydboctocursor(cursorId,"keys",outputId,"","",z1)=%ydboctocursor(cursorId,"keys",inputId1,"","",z1)
	KILL %ydboctocursor(cursorId,"keys",inputId1,"","")
	; Merge key corresponding to inputId2 into outputId
	SET zmax2=$GET(%ydboctocursor(cursorId,"keys",inputId2,"",""),0)
	FOR z2=1:1:zmax2 SET %ydboctocursor(cursorId,"keys",outputId,"","",zmax1+z2)=%ydboctocursor(cursorId,"keys",inputId2,"","",z2)
	KILL %ydboctocursor(cursorId,"keys",inputId2,"","")
	; Set # of records in output table before returning
	SET %ydboctocursor(cursorId,"keys",outputId,"","")=zmax1+zmax2
	QUIT

UNION(inputId1,inputId2,outputId)
	; Helper M function that does UNION of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . QUIT:$DATA(index(val))
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=val
	. . SET index(val)=""
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

INTERSECTALL(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(val))
	. . ELSE  IF +$GET(index(val)) DO
	. . . IF $INCREMENT(index(val),-1)
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=val
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

INTERSECT(inputId1,inputId2,outputId)
	; Helper M function that does INTERSECT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) SET index(val)=""
	. . ELSE  IF $DATA(index(val)) DO
	. . . KILL index(val)
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=val
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

EXCEPTALL(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT ALL of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(val))
	. . ELSE  IF +$GET(index(val)) DO
	. . . IF $INCREMENT(index(val),-1)
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	SET subs=""
	FOR  SET subs=$ORDER(index(subs)) QUIT:subs=""  DO
	. FOR z=1:1:index(subs) DO
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=subs
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
	QUIT

EXCEPT(inputId1,inputId2,outputId)
	; Helper M function that does EXCEPT of two queries each with output key# "inputId1" and "inputId2"
	; and creates the result in a key with output key# "outputId". Used by the generated M file/plan _ydboctoP*.m
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW z,z2,zmax,index,val,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET zmax=$GET(%ydboctocursor(cursorId,"keys",id,"",""),0)
	. FOR z=1:1:zmax DO
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",z)
	. . IF (id=inputId1) SET index(val)=""
	. . ELSE  KILL index(val)
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	SET subs=""
	FOR  SET subs=$ORDER(index(subs)) QUIT:subs=""  DO
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",$INCREMENT(z2))=subs
	; Set # of records in output table before returning
	SET:$DATA(z2) %ydboctocursor(cursorId,"keys",outputId,"","")=z2
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
	NEW id,subs,val
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",subs)
	. . IF $INCREMENT(%ydboctocursor(cursorId,"keys",outputId,"","",subs),val)
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
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
	NEW id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
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
	NEW index,val,val2,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",subs)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(subs),val)
	. . ELSE  IF $GET(index(subs)) DO
	. . . SET val2=+$GET(index(subs))
	. . . SET:(val>val2) val=val2
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=val
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
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
	NEW index,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . IF (id=inputId1) DO
	. . . SET index(subs)=1
	. . ELSE  IF $GET(index(subs)) DO
	. . . SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
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
	NEW index,id,val,val2,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . SET val=%ydboctocursor(cursorId,"keys",id,"","",subs)
	. . IF (id=inputId1) DO
	. . . IF $INCREMENT(index(subs),val)
	. . ELSE  IF $GET(index(subs)) DO
	. . . SET val2=+$GET(index(subs))
	. . . SET:(val>val2) val=val2
	. . . IF $INCREMENT(index(subs),-val)
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	SET subs=""
	FOR  DO:$DATA(index(subs))  SET subs=$ORDER(index(subs)) QUIT:subs=""
	. SET val=+$GET(index(subs))
	. QUIT:'val
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=val
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
	NEW index,id,subs
	KILL %ydboctocursor(cursorId,"keys",outputId,"","")
	; Merge key corresponding to inputId1 and inputId2 into outputId
	FOR id=inputId1,inputId2 DO
	. SET subs=""
	. FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",id,"","",subs))  SET subs=$ORDER(%ydboctocursor(cursorId,"keys",id,"","",subs)) QUIT:subs=""
	. . IF (id=inputId1) DO
	. . . SET index(subs)=1
	. . ELSE  IF $GET(index(subs)) DO
	. . . KILL index(subs)
	. KILL %ydboctocursor(cursorId,"keys",id,"","")
	SET subs=""
	FOR  DO:$DATA(index(subs))  SET subs=$ORDER(index(subs)) QUIT:subs=""
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	QUIT

GetScalar(keyId)
	; Helper M function that given an output key # (keyId) checks if the output key at most one row (guaranteed
	; to have only one column at parse time). If so it returns that as the value. If not, it issues an error.
	; Used by generated plans where a sub-query is used in place of a scalar value (e.g. arithmetic expression etc.)
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	NEW firstsub,secondsub,morethanonesub
	; Check if there are no rows in subquery output. If so we should return NULL per SQL standard.
	QUIT:(1>=$DATA(%ydboctocursor(cursorId,"keys",keyId,"",""))) $ZYSQLNULL
	SET firstsub=$SELECT($DATA(%ydboctocursor(cursorId,"keys",keyId,"","","")):"",1:$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","","")))
	; Find out if the output key has more than one row. If so issue an error
	; Note that it is possible the same row gets duplicated more than once. In that case though
	; the node value would be greater than 1. So check that too (in addition to checking $ORDER returns "").
	SET secondsub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",firstsub))
	; It is possible firstsub and secondsub are both "" in which case there is only one sub. Check for that.
	; Note that if firstsub is $ZYSQLNULL, then we are guaranteed there is no second subscript (since
	; $ZYSQLNULL is last subscript in $ORDER sequence).
	SET morethanonesub=$SELECT($ZYISSQLNULL(firstsub):0,$ZYISSQLNULL(secondsub):1,'$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",secondsub)):0,1:(firstsub'=secondsub))
	SET:'morethanonesub morethanonesub=(1<%ydboctocursor(cursorId,"keys",keyId,"","",firstsub))
	ZMESSAGE:morethanonesub %ydboctoerror("SUBQUERYMULTIPLEROWS")
	QUIT firstsub ; Return scalar in only column and only row of keyId

EXISTS(keyId)
	; Helper M function that given an output key # (keyId) checks if the output key has at least one row
	; If so returns 1 and if not returns 0. Implements the EXISTS operator in SQL.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	;
	QUIT (1<$DATA(%ydboctocursor(cursorId,"keys",keyId,"","")))

ANY(inputValue,keyId,compOp,isString)
	; Helper M function that implements the ANY/SOME operator in SQL.
	; Given an output key # (keyId) checks if the output key has at least one row with a value that satisfies the
	; compOp property (which can be any one of "<",">","<=",">=","=","'=") against the input value (inputValue).
	; If so returns 1 and if not returns 0.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; NOTE: The below implementation returns $ZYSQLNULL in case none of the $$Compare calls returns TRUE and at
	; least one of the return is NULL (in accordance with SQL rules for NULL).
	;
	NEW ret,sub
	SET sub="",ret=0
	FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",sub))  SET sub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",sub)) QUIT:ret!(""=sub)
	. SET ret=$$Compare(inputValue,compOp,sub,isString)
	QUIT ret

ALL(inputValue,keyId,compOp,isString)
	; Helper M function that implements the ALL operator in SQL.
	; Given an output key # (keyId) checks if the output key has ALL rows with a value that satisfies the
	; compOp property (which can be any one of "<",">","<=",">=","=","'=") against the input value (inputValue).
	; If so returns 1 and if not returns 0.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; NOTE: The below implementation returns $ZYSQLNULL in case none of the $$Compare calls returns FALSE and at
	; least one of the return is NULL (in accordance with SQL rules for NULL).
	;
	NEW ret,sub
	SET sub="",ret=1
	FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",sub))  SET sub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",sub)) QUIT:'ret!(""=sub)
	. SET ret=$$Compare(inputValue,compOp,sub,isString)
	QUIT ret

Compare(value1,compOp,value2,isString)
	; Helper M function used by $$ANY and $$ALL to perform comparison
	NEW ret
	; If value is a STRING type, then simple M "=" or "'=" operators are okay. Otherwise, numeric comparison needs a
	; coercion of the operand to a number hence the use of "+" on the operands before doing the "=" or "'=" below (YDBOcto#574).
	QUIT:("="=compOp) $SELECT(isString:value1=value2,1:$$ForceNumeric(value1)=$$ForceNumeric(value2))
	QUIT:("'="=compOp) $SELECT(isString:value1'=value2,1:$$ForceNumeric(value1)'=$$ForceNumeric(value2))
	IF 'isString  DO  QUIT ret
	. SET:("<"=compOp) ret=(value1<value2)
	. SET:("<="=compOp) ret=(value1<=value2)
	. SET:(">"=compOp) ret=(value1>value2)
	. SET:(">="=compOp) ret=(value1>=value2)
	; Now that we know it is a string type and we have inequality checks, we need to figure out
	; the right operator to use (M FOLLOWS operator or its complement).
	QUIT:(">"=compOp) value1]value2
	QUIT:("<="=compOp) value1']value2
	QUIT:(">="=compOp) value2']value1
	QUIT:("<"=compOp) value2]value1
	QUIT  ; We do not expect to reach here. Hence the QUIT without any value (will generate a runtime error).

ForceNumeric(value)
	; Check for $ZYSQLNULL first as `+` operator on that does not work in all cases (YDB#629)
	QUIT:$ZYISSQLNULL(value) $ZYSQLNULL	; This line can be removed when YDB#629 is fixed.
	QUIT +value

CountAsterisk(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the COUNT(*) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex` (unused)
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	; Since COUNT(*) does not examine values for NULL, no need to use `curValue` in this case.
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex))
	QUIT

Count(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the COUNT() aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex))
	QUIT

Min(keyId,groupBySubs,aggrIndex,curValue,isString)
	; Helper M function to implement the MIN() and MIN(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	;	isString    : 1 if curValue is of string type, 0 if curValue is of numeric type
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	NEW curMin
	QUIT:$ZYISSQLNULL(curValue)
	IF $DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)) DO
	. ; Values have already been aggregated. Compare current value against stored min value and update if needed.
	. SET curMin=%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)
	. IF (('isString&(curValue<curMin))!(isString&(curMin]curValue))) DO
	. . SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	ELSE  DO
	. ; No values have been aggregated yet. Current value is the MIN.
	. SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	QUIT

Max(keyId,groupBySubs,aggrIndex,curValue,isString)
	; Helper M function to implement the MAX() and MAX(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	;	isString    : 1 if curValue is of string type, 0 if curValue is of numeric type
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	NEW curMax
	QUIT:$ZYISSQLNULL(curValue)
	IF $DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)) DO
	. ; Values have already been aggregated. Compare current value against stored max value and update if needed.
	. SET curMax=%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)
	. IF (('isString&(curValue>curMax))!(isString&(curValue]curMax))) DO
	. . SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	ELSE  DO
	. ; No values have been aggregated yet. Current value is the MAX.
	. SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex)=curValue
	QUIT

Sum(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the SUM() aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex),curValue)
	QUIT

Avg(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the AVG() aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	; Note: We store the cumulative sum and the cumulative count here. Actual average will be taken in generated M code
	;	(code generation happens in `src/m_templates/tmpl_print_expression.ctemplate` in the LP_AGGREGATE_FUNCTION_AVG
	;	and LP_AGGREGATE_FUNCTION_AVG_DISTINCT switch/case blocks).
	; If we use `aggrIndex` (without the minus sign prefix), we could encounter an issue.
	;	This is because in the case of AVG(DISTINCT), the label `AvgDistinct` could encounter curValue as "SUM" or "COUNT"
	;	in which case it is going to set that as a subscript underneath the `aggrIndex` subscript in which case, it
	;	would confuse the "SUM"/"COUNT" maintenance for AVG. Hence the choice of a negative subscript.
	QUIT:$ZYISSQLNULL(curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,-aggrIndex,"SUM"),curValue)
	IF $INCREMENT(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,-aggrIndex,"COUNT"))
	QUIT

CountDistinct(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the COUNT(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	QUIT:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue))
	SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue)=""
	DO Count(keyId,groupBySubs,aggrIndex,curValue)
	QUIT

SumDistinct(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the SUM(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	QUIT:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue))
	SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue)=""
	DO Sum(keyId,groupBySubs,aggrIndex,curValue)
	QUIT

AvgDistinct(keyId,groupBySubs,aggrIndex,curValue)
	; Helper M function to implement the AVG(DISTINCT) aggregate function in SQL.
	; Input:
	;	keyId       : output key #
	;	groupBySubs : the subscript corresponding to the GROUP BY column list specification
	;	aggrIndex   : N if we are processing the Nth aggregate function specified in the query
	;	curValue    : the value that should be aggregated into `aggrIndex`
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; Ideally we should be using PP_GROUP_BY instead of "GroupBy" below but we need some preprocessor in M for that
	; So we use the hardcoded string instead here.
	QUIT:$ZYISSQLNULL(curValue)
	QUIT:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue))
	SET %ydboctocursor(cursorId,"keys",keyId,"","","GroupBy",groupBySubs,aggrIndex,curValue)=""
	DO Avg(keyId,groupBySubs,aggrIndex,curValue)
	QUIT

Integer2Boolean(intvalue)	;
	; Converts an input boolean parameter (`intvalue`) to 1 if it evaluates to a non-zero value and 0 otherwise.
	; This is so we are compatible with Postgres
	QUIT $SELECT($ZYISSQLNULL(intvalue):$ZYSQLNULL,+intvalue:1,1:0)

Boolean2String(boolvalue)	;
	; Converts an input boolean parameter (`boolvalue`) to `true` if it evaluates to a non-zero value and `false` otherwise
	; This is so we are compatible with Postgres
	QUIT $SELECT($ZYISSQLNULL(boolvalue):$ZYSQLNULL,+boolvalue:"true",1:"false")

String2Boolean(boolstr)	;
	; Converts an input boolean string value (`boolstr`) (can be `t` or `f`) to 1 or 0 respectively
	QUIT:$ZYISSQLNULL(boolstr) $ZYSQLNULL
	IF '$DATA(%ydboctoStr2Bool) DO
	.	; Below are list of string literals which are accepted for boolean conversion
	.	; Anything else is treated as the boolean value 0 (false).
	.	SET %ydboctoStr2Bool("true")=1
	.	SET %ydboctoStr2Bool("t")=1
	.	SET %ydboctoStr2Bool("yes")=1
	.	SET %ydboctoStr2Bool("y")=1
	.	SET %ydboctoStr2Bool("1")=1
	.	SET %ydboctoStr2Bool("false")=0
	.	SET %ydboctoStr2Bool("f")=0
	.	SET %ydboctoStr2Bool("no")=0
	.	SET %ydboctoStr2Bool("n")=0
	.	SET %ydboctoStr2Bool("0")=0
	IF '$DATA(%ydboctoStr2Bool(boolstr))  DO
	.	SET %ydboctoerror("INVALIDINPUTSYNTAXBOOL",1)=boolstr	; pass parameter to `src/ydb_error_check.c`
	.	ZMESSAGE %ydboctoerror("INVALIDINPUTSYNTAXBOOL")
	QUIT %ydboctoStr2Bool(boolstr)

str2mval(str)
	; Converts an input string `str` into a `len,str` 2-tuple. Cannot use `str` as is in case input string is `$ZYSQLNULL`
	; (as that cannot then be later used as part of a concatenation operation since the result of the concatenation
	; would be $ZYSQLNULL losing all other operands of the concatenation operator).
	;
	; The length can be 1 or 2 or 3 bytes long.
	;
	;	String	Output
	;	------	-------------
	;	NULL    00000000
	;	1-byte  00000001                    to 01111110                   [ String Byte Lengths of 0 to 126 + 1-byte header length = length of 1 to 127]
	;	2-byte  10000000 10000001           to 10111111 11111111          [ String Byte Lengths of 127 to 16381 + 2-byte header length = length of 129 to 16383]
	;	3-byte  11000000 01000000 00000001  to 11010000 00000000 00000000 [ String Byte Lengths of 16382 to 1048573 + 3-byte header length = length of 16385 + 1048576]
	;
	QUIT:$ZYISSQLNULL(str) $ZCHAR(0)
	NEW len,hdr
	SET len=$ZLENGTH(str)
	IF 127>len DO
	.	; 1-byte header
	. 	SET hdr=$ZCHAR(1+len)
	ELSE  IF 16382>len DO
	.	; 2-byte header
	.	SET len=len+2	; Add 2-byte header length too
	.	SET hdr=$ZCHAR(128+(len\256))_$ZCHAR(len#256)
	ELSE  DO
	.	; 3-byte header
	.	SET len=len+3	; Add 3-byte header length too
	.	SET hdr=$ZCHAR(192+(len\65536))_$ZCHAR((len\256)#256)_$ZCHAR(len#256)
	QUIT hdr_str

mval2str(mval)
	; Converts the input `mval` (`len,str` tuple) back into a string (just `str`) and returns that.
	; This is the inverse of $$str2mval^%ydboctoplanhelpers.
	;
	NEW byte1,hdrlen,datalen
	SET byte1=$ZASCII($ZEXTRACT(mval,1))
	QUIT:0=byte1 $ZYSQLNULL
	IF 128>byte1 DO
	.	; 1-byte header
	.	SET hdrlen=1
	.	SET datalen=byte1-1
	ELSE  IF 192>byte1 DO
	.	; 2-byte header
	.	SET hdrlen=2
	.	SET datalen=$$get2bytedatalen(byte1,mval,0)
	ELSE  DO
	.	; 3-byte header
	.	SET hdrlen=3
	.	SET datalen=$$get3bytedatalen(byte1,mval,0)
	QUIT $ZEXTRACT(mval,hdrlen+1,hdrlen+datalen)

mvalPiece(mval,piecenum)
	; Locates the `piecenum`th piece in `mval` (a concatenated sequence of `mval`s) and returns that `mval (`len,str`)
	;
	; Note: `src/get_mval_len.c` has very similar logic as below. So any changes here need to be reflected there as well.
	;
	NEW byte1,i,hdrlen,datalen,cumullen
	SET cumullen=0
	FOR i=1:1:piecenum DO  QUIT:i=piecenum  IF $INCREMENT(cumullen,hdrlen+datalen)
	.	SET byte1=$ZASCII($ZEXTRACT(mval,cumullen+1))
	.	IF 0=byte1 DO
	.	.	; $ZYSQLNULL
	.	.	SET hdrlen=1
	.	.	SET datalen=0
	.	ELSE  IF 128>byte1 DO
	.	.	; 1-byte header
	.	.	SET hdrlen=1
	.	.	SET datalen=byte1-1
	.	ELSE  IF 192>byte1 DO
	.	.	; 2-byte header
	.	.	SET hdrlen=2
	.	.	SET datalen=$$get2bytedatalen(byte1,mval,cumullen)
	.	ELSE  DO
	.	.	; 3-byte header
	.	.	SET hdrlen=3
	.	.	SET datalen=$$get3bytedatalen(byte1,mval,cumullen)
	QUIT $ZEXTRACT(mval,cumullen+1,cumullen+hdrlen+datalen)

get2bytedatalen(byte1,mval,offset)
	; Computes the length given a 2-byte header (1st byte is already in byte1 and 2nd byte is obtained from mval)
	; `offset` indicates how many bytes to go past before extracting the 2nd byte
	QUIT (byte1-128)*256+$ZASCII($ZEXTRACT(mval,offset+2))-2

get3bytedatalen(byte1,mval,offset)
	; Computes the length given a 3-byte header (1st byte is already in byte1 and 2nd/3rd bytes are obtained from mval)
	; `offset` indicates how many bytes to go past before extracting the 2nd/3rd byte
	QUIT (byte1-192)*65536+($ZASCII($ZEXTRACT(mval,offset+2))*256)+$ZASCII($ZEXTRACT(mval,offset+3))-3

empty2null(isnotnull,type,piece)
	; Conditionally converts an empty string value returned by $PIECE to $ZYSQLNULL if NOT NULL is not specified
	NEW result
	SET result=piece
	IF ""=piece DO
	. IF isnotnull DO
	. . IF ("NUMERIC"=type)!("INTEGER"=type)!("BOOLEAN"=type) DO
	. . . SET result="0"
	. . ; No action needed for VARCHAR case, since ""=piece already
	. ELSE  DO
	. . SET result=$ZYSQLNULL
	QUIT result

trimdotstar(resstr)
	; Removes consequent .*'s present in resstr
	; Example: .*.* -> .*
	NEW trim,result,len
	SET trim=0,result=""
	SET len=$ZLENGTH(resstr)
	FOR i=1:1:len  DO
	. SET ch=$ZEXTRACT(resstr,i)
	. IF "."=ch DO
	. . SET ch=$ZEXTRACT(resstr,i+1)
	. . IF "*"=ch DO
	. . . IF 1'=trim DO
	. . . . SET trim=1
	. . . . SET result=result_".*"
	. . . SET i=i+1
	. . ELSE  DO
	. . . SET trim=0
	. . . SET result=result_"."
	. ELSE  DO
	. . SET trim=0
	. . SET result=result_ch
	QUIT result

regexinit
	; Initialize the regex transformation local variables once per octo process.
	; Comments at the beginning of routine regexmatch explains why and how the transformations
	; arrays below are used.
	IF (0=$GET(%ydboctoregex(0,1,"%"),0)) DO
	. ; LIKE
	. SET %ydboctoregex(0,1,"%")=".*",%ydboctoregex(0,1,"_")=".",%ydboctoregex(0,1,".")="\.",%ydboctoregex(0,1,"*")="\*"
	. SET %ydboctoregex(0,1,"[")="\[",%ydboctoregex(0,1,"]")="\]",%ydboctoregex(0,1,"+")="+",%ydboctoregex(0,1,"?")="?"
	. SET %ydboctoregex(0,1,"{")="{",%ydboctoregex(0,1,"}")="}",%ydboctoregex(0,1,"(")="(",%ydboctoregex(0,1,")")=")"
	. SET %ydboctoregex(0,1,"|")="|"
	. ; SIMILAR TO
	. SET %ydboctoregex(0,2,"%")=".*",%ydboctoregex(0,2,"_")=".",%ydboctoregex(0,2,".")="\.",%ydboctoregex(0,2,"*")="*"
	. SET %ydboctoregex(0,2,"[")="[",%ydboctoregex(0,2,"]")="]",%ydboctoregex(0,2,"+")="\+",%ydboctoregex(0,2,"?")="\?"
	. SET %ydboctoregex(0,2,"{")="\{",%ydboctoregex(0,2,"}")="\}",%ydboctoregex(0,2,"(")="\(",%ydboctoregex(0,2,")")="\)"
	. SET %ydboctoregex(0,2,"|")="$\|^"
	. ; ~
	. SET %ydboctoregex(0,3,"%")="%",%ydboctoregex(0,3,"_")="_",%ydboctoregex(0,3,".")=".",%ydboctoregex(0,3,"*")="*"
	. SET %ydboctoregex(0,3,"[")="[",%ydboctoregex(0,3,"]")="]",%ydboctoregex(0,3,"+")="\+",%ydboctoregex(0,3,"?")="\?"
	. SET %ydboctoregex(0,3,"{")="\{",%ydboctoregex(0,3,"}")="\}",%ydboctoregex(0,3,"(")="\(",%ydboctoregex(0,3,")")="\)"
	. SET %ydboctoregex(0,3,"|")="\|"
	. ; LIKE, SIMILAR TO & ~ operation with an escaped character
	. SET %ydboctoregex(1,"%")="%",%ydboctoregex(1,"_")="_",%ydboctoregex(1,"|")="|",%ydboctoregex(1,"+")="+"
	. SET %ydboctoregex(1,"?")="?",%ydboctoregex(1,"{")="{",%ydboctoregex(1,"}")="}",%ydboctoregex(1,"(")="("
	. SET %ydboctoregex(1,")")=")",%ydboctoregex(1,"\")="\\",%ydboctoregex(1,".")="\.",%ydboctoregex(1,"*")="\*"
	. SET %ydboctoregex(1,"[")="\[",%ydboctoregex(1,"]")="\]"
	QUIT

throwregexerr(regexstr)
	SET %ydboctoerror("INVALIDESCAPEPATTERN",1)=regexstr  ; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("INVALIDESCAPEPATTERN")
	; QUIT will not be reached as ZMESSAGE triggers error handling and returns control out of M code back to caller C code
	QUIT

pattransform(patt,type)
	; transform a pattern with appropriate escapes
	; type=1 transformation for LIKE
	; type=2 transfoprmation for SIMILAR TO
	; type=3 transformation for ~
	NEW len,ch,res,resstr
	SET len=$ZLENGTH(patt),resstr=""
	FOR i=1:1:len  DO
	. SET (res,ch)=$ZEXTRACT(patt,i)
	. IF "\"=ch DO
	. . ; escape character encountered
	. . IF ((i+1)>len) DO
	. . . IF (1=type) DO
	. . . . SET %ydboctoregex("result")="error"
	. . . . DO throwregexerr(patt)
	. . . ELSE  SET res="\"
	. . ELSE  DO
	. . . SET ch=$ZEXTRACT(patt,i+1),i=i+1
	. . . SET res=$GET(%ydboctoregex(1,ch),0)
	. . . SET:0=res res="\"_ch
	. ELSE  DO
	. . SET res=$GET(%ydboctoregex(0,type,ch),0)
	. . SET:0=res res=ch
	. SET resstr=resstr_res
	quit resstr

regexmatch(str,regexstr,plantype,intval)
	; By default, regex engine ($$regmatch^ydbposix) treats some metacharacters as off.
	; Where as SIMILAR TO and LIKE by default are expected to have these metacharacters to be on.
	; In order to use regex engine to process all regex pattern operations, regexstr is processed
	; to enable necessary metacharacters based on its operation.
	; Example:
	; 	Posix regex engine by default doesn't treat + with special meaning.
	;	But, \+ provides the character special meaning.
	;	SQL standard by default treats + with special meaning i.e using literal character +
	;	means match one or more units in case of SIMILAR TO and ~.
	;	To acheive synchronization between SQL standard and Posix regex engine we transform
	;	+ in a SQl query to \+ (SIMILAR TO & ~ usage) to treat it with special meaning.
	; This operation is done here as column values are not available in earlier stages and we
	; need pattern strings passed as column reference also to be processed.
	; Parameter:
	;	intval -  if defined acts as the third argument for regex engine
	; 	plantype -  represents operation : 1->PP_LIKE (LIKE) ,2->PP_SIMILARTO (SIMILAR TO) & 3->PP_TILDE (~)
	; 	str - left operand of regex operation
	;	regexstr - right operand of regex operation (pattern string)
	; regexstr is processed using the following rules:
	; Like			Similar TO			~
	; Input	Output		Input	Output		Input	Output
	; %	.*		%	.*		%	%
	; _	.		_	.		_	_
	; .	\.		.	\.		.	.
	; *	\*		*	*		*	*
	; [	\[		[	[		[	[
	; ]	\]		]	]		]	]
	; +	+		+	\+		+	\+
	; ?	?		?	\?		?	\?
	; {	{		{	\{		{	\{
	; }	}		}	\}		}	\}
	; (	(		(	\(		(	\(
	; )	)		)	\)		)	\)
	; |	|		|	$\|^		|	\|
	; \%	%		\%	%		\%	%
	; \_	_		\_	_		\_	_
	; \|	|		\|	|		\|	|
	; \+	+		\+	+		\+	+
	; \?	?		\?	?		\?	?
	; \{	{		\{	{		\{	{
	; \}	}		\}	}		\}	}
	; \(	(		\(	(		\(	(
	; \)	)		\)	)		\)	)
	; \\	\\		\\	\\		\\	\\
	; \.	\.		\.	\.		\.	\.
	; \*	\*		\*	\*		\*	\*
	; \[	\[		\[	\[		\[	\[
	; \]	\]		\]	\]		\]	\]
	;
	; if any of the input strings are $ZYSQLNULL return without processing
	QUIT:$ZYISSQLNULL(regexstr) $ZYSQLNULL
	QUIT:$ZYISSQLNULL(str) $ZYSQLNULL
	; optimization:
	; 	fetch previous regexstr, plantype and transformed regexstr("result")
	; 	avoid performing transformation again when current and previous regexstr and plantype are same
	NEW pvstr,pvrs,pvpln,ret
	SET pvstr=$GET(%ydboctoregex("regexstr"))
	SET pvrs=$GET(%ydboctoregex("result"))
	SET pvpln=$GET(%ydboctoregex("plan"))
	IF ((regexstr=pvstr)&(plantype=pvpln)) DO  QUIT ret
	. IF ("error"=pvrs) DO throwregexerr(regexstr)
	. ELSE  IF (".*"=pvrs) SET ret=1
	. ELSE  IF (0=$DATA(intval)) SET ret=$$regmatch^%ydbposix(str,pvrs)
	. ELSE  SET ret=$$regmatch^%ydbposix(str,pvrs,intval)
	SET %ydboctoregex("regexstr")=regexstr
	SET %ydboctoregex("plan")=plantype
	DO regexinit
	NEW resstr,result	; result will hold transformed regexstr
	SET resstr=""
	; anchor incase of SIMILAR TO and LIKE operation
	SET:plantype'=3 resstr="^"
	SET resstr=resstr_$$pattransform(regexstr,plantype)
	; anchor in case of LIKE and SIMILAR TO
	SET:3'=plantype resstr=resstr_"$"
	SET ret=0,result=resstr
	IF 3=plantype DO
	. ; .*.* type of regexstr can only occur in ~ operation
	. ; trim .* as its processing is faster here than in regex engine
	. SET result=$$trimdotstar(resstr)
	. IF (".*"=result) SET %ydboctoregex("result")=result,ret=1
	QUIT:1=ret 1
	SET %ydboctoregex("result")=result
	IF (0=$DATA(intval)) SET ret=$$regmatch^%ydbposix(str,result)
	ELSE  SET ret=$$regmatch^%ydbposix(str,result,intval)
	QUIT ret
