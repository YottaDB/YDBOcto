;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	;
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
	SET status=$ZTRIGGER(arg1,arg2)
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
	; Merge key corresponding to inputId2 into outputId
	SET zmax2=$GET(%ydboctocursor(cursorId,"keys",inputId2,"",""),0)
	FOR z2=1:1:zmax2 SET %ydboctocursor(cursorId,"keys",outputId,"","",zmax1+z2)=%ydboctocursor(cursorId,"keys",inputId2,"","",z2)
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
	SET subs=""
	FOR  DO:$DATA(index(subs))  SET subs=$ORDER(index(subs)) QUIT:subs=""
	. SET %ydboctocursor(cursorId,"keys",outputId,"","",subs)=1
	QUIT

GetScalarOrArray(keyId,toArray,planName)
	; Helper M function for processing scalar values and single-row arrays. The toArray parameter indicates whether or not to
	; produce a scalar value or compose an array based on the provided output key # (keyId). In either case, the return is a
	; single value. In the array case, this return value is a string in PostgreSQL array format, i.e. {elem1,elem2,...}. In the
	; scalar case, this routine checks if the output key has at most one row (guaranteed to have only one column at parse time).
	; If so it returns that as the value. If not, it issues an error.
	;
	; Note that the array case permits multiple rows to be returned, per the PostgreSQL specification for ARRAY constructors
	; using subqueries at https://www.postgresql.org/docs/11/sql-expressions.html#SQL-SYNTAX-ARRAY-CONSTRUCTORS.
	;
	; This routine is used by generated plans where a sub-query is used in place of a scalar value (e.g. arithmetic expression
	; etc.). Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	NEW firstsub,secondsub,morethanonesub,curvalue,result
	; The variable planName points to the physical plan entryref that needs to be invoked first (in case it is a deferred plan)
	;   in order to generate the output key rows. Example value is "octoPlan2^%ydboctoPrTrjCuwSxj7urDaUaUSh1G"). The
	;   resulting output key rows are used to obtain the return value of this function call.
	;   It is "" in case the plan is not a deferred plan. And in this case, no physical plan entryref needs to be invoked.
	IF $$InvokeOctoPlan(planName)
	; Check if there are no rows in subquery output. If so we should return NULL per SQL standard.
	QUIT:(1>=$DATA(%ydboctocursor(cursorId,"keys",keyId,"",""))) $ZYSQLNULL
	SET firstsub=$SELECT($DATA(%ydboctocursor(cursorId,"keys",keyId,"","","")):"",1:$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","","")))
	; Find out if the output key has more than one row. If so issue an error
	; Note that it is possible the same row gets duplicated more than once. In that case though
	; the node value would be greater than 1. So check that too (in addition to checking $ORDER returns "").
	; It is possible firstsub and secondsub are both "" in which case there is only one sub. Check for that.
	; Note that if firstsub is $ZYSQLNULL, then we are guaranteed there is no second subscript (since
	IF (toArray) DO
	. SET result="{"
	. SET %ydboctocursor(cursorId,"keys",keyId,"","")=""
	. FOR  SET %ydboctocursor(cursorId,"keys",keyId,"","")=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"",""))) QUIT:(%ydboctocursor(cursorId,"keys",keyId,"","")="")  DO
	. . IF ($ZYISSQLNULL($$mval2str^%ydboctoplanhelpers(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"",""))))) DO
	. . . SET result=result_"NULL,"
	. . ELSE  IF (""=$$mval2str^%ydboctoplanhelpers(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"","")))) DO
	. . . SET result=result_""""","
	. . ELSE  DO
	. . . SET result=result_$$mval2str^%ydboctoplanhelpers(%ydboctocursor(cursorId,"keys",keyId,"","",%ydboctocursor(cursorId,"keys",keyId,"","")))_","
	. SET result=$EXTRACT(result,1,$LENGTH(result)-1)_"}"
	ELSE  DO
	. SET result=firstsub
	. SET secondsub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",firstsub))
	. SET morethanonesub=$SELECT($ZYISSQLNULL(firstsub):0,$ZYISSQLNULL(secondsub):1,'$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",secondsub)):0,1:(firstsub'=secondsub))
	. SET:'morethanonesub morethanonesub=(1<%ydboctocursor(cursorId,"keys",keyId,"","",firstsub))
	. ZMESSAGE:morethanonesub %ydboctoerror("SUBQUERYMULTIPLEROWS")
	QUIT result ; Return scalar in only column and only row of keyId

EXISTS(keyId,planName)
	; Helper M function that given an output key # (keyId) checks if the output key has at least one row
	; If so returns 1 and if not returns 0. Implements the EXISTS operator in SQL.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; See comment about "planName" variable in "GetScalarOrArray" entryref section.
	;
	IF $$InvokeOctoPlan(planName)
	QUIT (1<$DATA(%ydboctocursor(cursorId,"keys",keyId,"","")))

ANY(inputValue,keyId,compOp,isString,planName)
	; Helper M function that implements the ANY/SOME operator in SQL.
	; Given an output key # (keyId) checks if the output key has at least one row with a value that satisfies the
	;   compOp property (which can be any one of "<",">","<=",">=","=","'=") against the input value (inputValue).
	; If so returns 1 and if not returns 0.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; NOTE: The below implementation returns $ZYSQLNULL in case none of the $$Compare calls returns TRUE and at
	;   least one of the return is NULL (in accordance with SQL rules for NULL).
	; See comment about "planName" variable in "GetScalarOrArray" entryref section.
	;
	IF $$InvokeOctoPlan(planName)
	NEW ret,sub
	SET sub="",ret=0
	FOR  DO:$DATA(%ydboctocursor(cursorId,"keys",keyId,"","",sub))  SET sub=$ORDER(%ydboctocursor(cursorId,"keys",keyId,"","",sub)) QUIT:ret!(""=sub)
	. SET ret=$$Compare(inputValue,compOp,sub,isString)
	QUIT ret

ALL(inputValue,keyId,compOp,isString,planName)
	; Helper M function that implements the ALL operator in SQL.
	; Given an output key # (keyId) checks if the output key has ALL rows with a value that satisfies the
	;   compOp property (which can be any one of "<",">","<=",">=","=","'=") against the input value (inputValue).
	; If so returns 1 and if not returns 0.
	; Assumes "%ydboctocursor" and "cursorId" are appropriately set by caller.
	; NOTE: The below implementation returns $ZYSQLNULL in case none of the $$Compare calls returns FALSE and at
	;   least one of the return is NULL (in accordance with SQL rules for NULL).
	; See comment about "planName" variable in "GetScalarOrArray" entryref section.
	;
	IF $$InvokeOctoPlan(planName)
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
	; If mval is the empty string, $ZEXTRACT would return the empty string and $ZASCII would return -1 ("byte1" == -1).
	; In that case, we should treat the mval as the empty string and return $ZYSQLNULL as the corresponding string value.
	; In the case byte1 is 0, we want to return $ZYSQLNULL.
	; Hence the below check for whether "byte1" is less than or equal to 0.
	QUIT:0>=byte1 $ZYSQLNULL
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

piecevalue2colvalue(piecevalue)
	; Check if "piecevalue" is not empty string. If so, return that right away.
	QUIT:(""'=piecevalue) piecevalue
	; Now that we know "piecevalue" is "" (i.e. empty string), return M equivalent of SQL NULL.
	QUIT $ZYSQLNULL

colvalue2piecevalue(colvalue)
	; Inverse of "piecevalue2colvalue()"
	;
	; First handle case where "colvalue" is not $ZYSQLNULL (the M equivalent of SQL NULL).
	QUIT:'$ZYISSQLNULL(colvalue) colvalue
	; Now we know "colvalue" is $ZYSQLNULL. Return empty string.
	QUIT ""

trimdotstar(resstr)
	; Removes consecutive .*'s present in resstr
	; Example: .*.* -> .*
	NEW trim,result,len
	SET trim=0,result=""
	SET len=$ZLENGTH(resstr)
	FOR i=1:1:len  DO
	. SET ch=$ZEXTRACT(resstr,i)
	. IF "."=ch DO
	. . SET ch=$ZEXTRACT(resstr,i+1)
	. . IF "*"=ch DO
	. . . DO:1'=trim
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
	DO:(0=$GET(%ydboctoregex(0,1,"%"),0))
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

regexmatch(str,regexstr,regextype,regexflags)
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
	; 	str - left operand of regex operation
	;	regexstr   - right operand of regex operation (pattern string)
	; 	regextype   -  represents operation : 1->REGEX_LIKE (LIKE) ,2->REGEX_SIMILARTO (SIMILAR TO) & 3->REGEX_TILDE (~)
	;	regexflags -  if defined acts as the third argument for regex engine (see "man regcomp" for details on the below)
	;		1 => REG_EXTENDED	; use extended regular expression syntax (default is basic regex syntax)
	;		2 => REG_ICASE		; ignore case when matching (default is to not ignore case)
	;		3 => REG_EXTENDED and REG_ICASE are both enabled
	;		etc.
	;
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
	; 	fetch previous regexstr, regextype and transformed regexstr("result")
	; 	avoid performing transformation again when current and previous regexstr and regextype are same
	NEW pvstr,pvrs,pvpln,ret
	SET pvstr=$GET(%ydboctoregex("regexstr"))
	SET pvrs=$GET(%ydboctoregex("result"))
	SET pvpln=$GET(%ydboctoregex("plan"))
	IF ((regexstr=pvstr)&(regextype=pvpln)) DO  QUIT ret
	. IF ("error"=pvrs) DO throwregexerr(regexstr)
	. ELSE  IF (".*"=pvrs) SET ret=1
	. ELSE  IF (0=$DATA(regexflags)) SET ret=$$regmatch^%ydbposix(str,pvrs)
	. ELSE  SET ret=$$regmatch^%ydbposix(str,pvrs,regexflags)
	SET %ydboctoregex("regexstr")=regexstr
	SET %ydboctoregex("plan")=regextype
	DO regexinit
	NEW resstr,result	; result will hold transformed regexstr
	SET resstr=""
	; anchor incase of SIMILAR TO and LIKE operation
	SET:regextype'=3 resstr="^"
	SET resstr=resstr_$$pattransform(regexstr,regextype)
	; anchor in case of LIKE and SIMILAR TO
	SET:3'=regextype resstr=resstr_"$"
	SET ret=0,result=resstr
	DO:3=regextype
	. ; .*.* type of regexstr can only occur in ~ operation
	. ; trim .* as its processing is faster here than in regex engine
	. SET result=$$trimdotstar(resstr)
	. IF (".*"=result) SET %ydboctoregex("result")=result,ret=1
	QUIT:1=ret 1
	SET %ydboctoregex("result")=result
	IF (0=$DATA(regexflags)) SET ret=$$regmatch^%ydbposix(str,result)
	ELSE  SET ret=$$regmatch^%ydbposix(str,result,regexflags)
	QUIT ret

Cast2VARCHAR(string,size)
	; This function helps implement the typecast operator where the target type is VARCHAR
	; "string" is the input string that needs to be type cast.
	; "size" is the maximum character length (not byte length) of the target string.
	; e.g. 'abcd'::VARCHAR(2) should return 'ab'
	QUIT $EXTRACT(string,1,size)

Cast2NUMERIC(number,precision,scale)
	; This function helps implement the typecast operator where the target type is NUMERIC
	; "number" is the input number that needs to be type cast.
	; "precision" is the maximum precision (i.e. total count of significant digits on either side of the decimal point)
	;	of the target number.
	; "scale" is the maximum scale (i.e. count of decimal digits to the right sight of the decimal point) of the target number.
	; e.g. 15.54::NUMERIC(3,1) should return '15.5'
	; e.g. 15.54::NUMERIC(3,0) should return '16'
	; e.g. 15.54::NUMERIC(4,1) should return '15.5'
	NEW tmpnumber,tmpprecision
	SET tmpprecision=precision
	SET:'$DATA(scale) scale=0
	SET number=$FNUMBER(number,"",scale)	; If number has more digits after the decimal point than scale, truncate/round it
	SET tmpprecision=tmpprecision-scale
	SET tmpnumber=number\1		; Remove fractional part
	SET:(0>tmpnumber) tmpnumber=-tmpnumber ; Get absolute value (if negative)
	SET:(0>tmpprecision) tmpprecision=0	; If precision is negative, set it to 0
	DO:tmpnumber>=(10**tmpprecision)
	.	SET %ydboctoerror("NUMERICOVERFLOW",1)=precision	; pass parameter to `src/ydb_error_check.c`
	.	SET:'$DATA(scale) scale=0
	.	SET %ydboctoerror("NUMERICOVERFLOW",2)=scale		; pass parameter to `src/ydb_error_check.c`
	.	SET %ydboctoerror("NUMERICOVERFLOW",3)=tmpprecision	; pass parameter to `src/ydb_error_check.c`
	.	ZMESSAGE %ydboctoerror("NUMERICOVERFLOW")
	QUIT number

SizeCheckVARCHAR(string,size)
	; This function is different from "Cast2VARCHAR" in that it issues an error if the "string" parameter does not fit
	; in the "size" parameter. This is invoked in the case of an INSERT INTO and an error is expected by the SQL standard
	; (the only exception is if the portion of the string greater than "size" characters is all spaces in which case the
	; excess spaces have to be removed and expected by the SQL standard).
	; Whereas the Cast2VARCHAR function is invoked by a type cast operator and truncation to fit the "size" (and not an error)
	; is expected by the SQL standard for the case where the "string" parameter does not fit in the "size" parameter.
	NEW charlen
	SET charlen=$LENGTH(string)
	QUIT:charlen<=size string
	; Check if excess portion is all spaces. If so, trim that out and return the trimmed string (no error).
	NEW excess,expected
	SET excess=$EXTRACT(string,size+1,charlen)
	SET expected=$JUSTIFY(" ",charlen-size)
	QUIT:excess=expected $EXTRACT(string,1,size)
	; Excess portion is not all spaces. This is an error scenario. Issue error.
	SET %ydboctoerror("VARCHARTOOLONG",1)=size	; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("VARCHARTOOLONG")
	QUIT

DuplicateKeyValue(name,detail)
	; This function is invoked to signal a UNIQUE constraint violation error.
	; "name" has the constraint name.
	; "detail" has additional detail on the actual values of the affected columns for the user.
	SET %ydboctoerror("DUPLICATEKEYVALUE",1)=name_" : Node "_detail_" already exists" ; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("DUPLICATEKEYVALUE")
	QUIT

NullKeyValue(colname)
	; This function is invoked to signal a NOT NULL constraint violation error.
	; "colname" has the column name.
	SET %ydboctoerror("NULLKEYVALUE",1)=colname	; pass parameter to `src/ydb_error_check.c`
	ZMESSAGE %ydboctoerror("NULLKEYVALUE")
	QUIT

CheckConstraintViolation(tablename,constraintname,numcols)
	; This function is invoked to signal a CHECK constraint violation error.
	; "tablename" has the table name
	; "constraintname" has the constraint name
	; "numcols" is the number of columns in the table (used to derive the column values of the violating row)
	NEW i,parm2
	SET %ydboctoerror("CHECKCONSTRAINTVIOLATION",1)=tablename ; pass parameter to `src/ydb_error_check.c`
	SET parm2=constraintname_" : Failing row contains ("
	FOR i=1:1:numcols SET parm2=parm2_$SELECT($ZYISSQLNULL(col(i)):"NULL",1:col(i))_$SELECT(i=numcols:")",1:", ")
	SET %ydboctoerror("CHECKCONSTRAINTVIOLATION",2)=parm2
	ZMESSAGE %ydboctoerror("CHECKCONSTRAINTVIOLATION")
	QUIT

InvokeOctoPlan(planName)
	; Given a comma-separated list of plan names in "planName" (e.g. "octoPlan2,octoPlan3") this function invokes each of
	;   those plans and finally returns a value of 0. This is needed in cases where we want to invoke the plan (using
	;   "DO octoPlan2" etc.) but cannot do so because we are in the middle of an expression evaluation. Making it a
	;   function that returns a value of 0 allows us to use this function as the first choice of a $SELECT function call
	;   which always gets evaluated before processing the rest of the $SELECT function call (where the real processing
	;   happens based on the results of the execution of the input plan name).
	; Additionally a plan name can also contain a space-separated list of parameters corresponding to a SET operation.
	;   For example, a plan name in "planName" could be the string "SET 1 2 3 UNION". In this case, we are guaranteed
	;   the first word in the space-separated list is the string "SET" (see "tmpl_invoke_deferred_setoper.ctemplate"
	;   for the "InvokeDeferredPlan_EXISTS" case).
	; So the logic is to first extract each plan name using $PIECE() and the delimiter ",".
	;   And in the result check if the first space separated word is "SET". If so, it is a SET operation related invocation.
	;   Do SET related processing. If not, it is a direct octo plan invocation request. So do that instead.
	; Assumes "routine" and "cursorId" are appropriately set by caller.
	; Example values for routine is "%ydboctoP0sGaZQ410YcOGHn8750h9E" and "cursorId" is some integer like "10".
	NEW entryref,i,pieces
	SET pieces=$ZLENGTH(planName,",")
	; Since we will always have a trailing comma at the end, ignore the last empty piece. Hence the use of "pieces-1" below.
	FOR i=1:1:pieces-1 DO
	. SET entryref=$ZPIECE(planName,",",i)
	. IF "SET"=$ZPIECE(entryref," ",1) DO
	. . ; This is a SET operation type of request
	. . NEW inputId1,inputId2,outputId,mlabref
	. . SET inputId1=$ZPIECE(entryref," ",2)
	. . SET inputId2=$ZPIECE(entryref," ",3)
	. . SET outputId=$ZPIECE(entryref," ",4)
	. . SET mlabref=$ZPIECE(entryref," ",5)
	. . DO @mlabref@(inputId1,inputId2,outputId)
	. ELSE  DO
	. . ; This is an octo plan invocation type of request
	. . SET entryref=entryref_"^"_routine	; note: "routine" is a variable set in "src/aux/_ydboctoselect.m"
	. . DO @entryref@(cursorId)
	QUIT 0

InvokeSetOper(inputId1,inputId2,outputId,mlabref)
	; Just like "InvokeOctoPlan" entryref above helps invoke generated plan entryrefs, "InvokeSetOper" does it for SET
	; operations where the outputs of two operands of the SET operation (pointed to by "inputId1" and "inputId2") need
	; to be merged into the output table (pointed to by "outputId"). The entryref that implements the SET operation type
	; is pointed to by "mlabref".
	DO @mlabref@(inputId1,inputId2,outputId)
	QUIT 0

RowIsNull(mval,numCols)
	; Implements the IS NULL check for a row/record (potentially more than 1 column value).
	; Currently this is invoked only for the "t1.* IS NULL" case (where "t1.*" is a TABLENAME.ASTERISK syntax).
	; Input
	;   "mval" is a concatenated list of mvals representing the column values.
	;   "numCols" is the total number of column values in the row.
	; Returns
	;   1 if ALL columns values are NULL
	;   0 otherwise (i.e. if at least one column value is not NULL)
	NEW i,str,isNull
	SET isNull=1
	FOR i=1:1:numCols DO  QUIT:'isNull
	. SET str=$$mval2str($$mvalPiece(mval,i))
	. SET:'$ZYISSQLNULL(str) isNull=0
	QUIT isNull

RowIsNotNull(mval,numCols)
	; Implements the IS NOT NULL check for a row/record (potentially more than 1 column value).
	; Currently this is invoked only for the "t1.* IS NOT NULL" case (where "t1.*" is a TABLENAME.ASTERISK syntax).
	; Input
	;   "mval" is a concatenated list of mvals representing the column values.
	;   "numCols" is the total number of column values in the row.
	; Returns
	;   1 if ALL columns values are not NULL
	;   0 otherwise (i.e. if at least one column value is NULL)
	; Note that it is possible for both IS NULL and IS NOT NULL to return FALSE for a row if a row has
	; a mix of NULL and non-NULL values. So "NOT t1.* IS NULL" is not the same as "t1.* IS NOT NULL".
	; Hence the need for separate RowIsNull and RowIsNotNull entryrefs.
	NEW i,str,isNotNull
	SET isNotNull=1
	FOR i=1:1:numCols DO  QUIT:'isNotNull
	. SET str=$$mval2str($$mvalPiece(mval,i))
	. SET:$ZYISSQLNULL(str) isNotNull=0
	QUIT isNotNull

TableAsteriskCompare(firstOperand,secondOperand,operator,numColumns,colTypeList)
	; Input
	;   "firstOperand" and "secondOperand" is table.* values in the form col1val_col2val_.._colnval or
	;      $ZYSQLNULL in case of a composite NULL row.
	;   "numColumns" represents the number of columns present in table.
	;   "colTypeList" is a list of comma separated values representing column types.
	;      `'t'` represents a NUL_VALUE type or STRING_LITERAL type column. `'f'` represents
	;      all other types of columns. This is required to be able to apply $$ForceNumeric if the type is non-string.
	;   "operator" is a value representing any of the following characters `<`, `>`, `<=`, `>=`, `'=` and `=`.
	;      It is converted appropriately to an m operator by considering the operand type.
	; Returns
	;   1 or 0 based on comparison result
	;   $ZYSQLNULL if any of the operands have a $ZYSQLNULL value
	; Implementation Details
	;   This function performs the comparison as noted below:
	;   1. `=` : Returns on first FALSE comparison. If none exist the return value is TRUE.
	;   2. `'=`: Returns on first TRUE comparison. If none exist the return value is FALSE.
	;   3. `<` : If both operands are equal result is FALSE, continue to the next operand comparison. If first
	;            operand `<` second then exit with the result TRUE. If first operand `>` second then exit with the result FALSE.
	;   4. `<=`: Same as `<` but if both operands are equal result is TRUE.
	;   5. `>` : If both operands are equal result is FALSE, continue to the next operand comparison. If first
	;            operand `>` second then exit with the result TRUE. If first operand `<` second then exit with the result FALSE.
	;   6. `>=`: Same as `>` but if both operands are equal result is TRUE.
	; Note
	;   - Operator is in the form `<`,`>`,`<=`,`>=`,`'=` and `=`. Its converted appropriately to an m operator
	;       based on the operand type. For example `<` is `]` for a string operation where as for a numeric
	;       operation it is `<` itself.
	;   - In case of string operation the following operators `<`, `>`, `<=` and `>=` are handled by ] and ']. The operands
	;       are swapped in case of `<` and `>=` in order to achieve the correct operation. Look at `LP_BOOLEAN_LESS_THAN` case
	;       in `tmpl_print_expressions` for more details.
	;   - Since all `table.*` operands are treated as strings the operands are swapped in case of `<` and `>=` operations
	;       irrespective of whether the column type is a string or not. This function handles non-string type comparison by
	;       swapping the operands back before performing a comparison.
	;   - Additionally, for all types, a comparison involving $ZYSQLNULL values should be compared without changing the
	;       operand position as this results in incorrect output. Hence we swap the operands before performing a ZYSQLNULL
	;       comparison.
	;   - If either one of the operand is a $ZYSQLNULL then that operand is a composite NULL, return without any additional
	;       processing. $ZYSQLNULL result is expected in this case.
	QUIT:(($ZYISSQLNULL(firstOperand))!($ZYISSQLNULL(secondOperand))) $ZYSQLNULL
	NEW result,curResult,limitingCompareReached,limitingCompareResult
	SET (curResult,result)=1
	SET limitingCompareReached=0 ; Used to quit out of the FOR loop below if comparison result is found mid-way
	FOR i=1:1:numColumns DO  QUIT:(1=limitingCompareReached)
	. NEW firstCol,secondCol,firstColVal,secondColVal,type,isEquals
	. ; Get the mval piece which corresponds to the column number i
	. SET firstCol=$$mvalPiece(firstOperand,i)
	. SET secondCol=$$mvalPiece(secondOperand,i)
	. SET isEquals=0
	. ; Get the value of the mval piece i.e. value of the column
	. SET firstColVal=$$mval2str(firstCol)
	. SET secondColVal=$$mval2str(secondCol)
	. ; get i'th colTypeList value
	. SET type=$PIECE(colTypeList,",",i)
	. ; Apply ForceNumeric if the operands are non-string or `NULL`
	. if ("f"=type)  do
	. . ; Get the numeric value of the columns
	. . SET firstColVal=$$ForceNumeric(firstColVal)
	. . SET secondColVal=$$ForceNumeric(secondColVal)
	. ; Save original operand values
	. NEW tmpFirstColVal,tmpSecondColVal
	. SET tmpFirstColVal=firstColVal
	. SET tmpSecondColVal=secondColVal
	. IF ((("<"=operator)!(">="=operator))) DO
	. . ; The below swap is done because table.* are treated as strings by tmpl_print_expression and this results in the
	. . ; operands being swapped at the comparison cases in tmpl_print_expression and this swap results in incorrect comparison
	. . ; when $ZYSQLNULL and numeric data is involved. Because of that we perform a swap below. In case it turns out to be a
	. . ; pure string comparison we swap back at the string comparison code below so that the reasoning behind
	. . ; tmpl_print_expression swap still applies for this operation.
	. . NEW tmpColVal
	. . SET tmpColVal=firstColVal
	. . SET firstColVal=secondColVal
	. . SET secondColVal=tmpColVal
	. IF (($ZYISSQLNULL(firstColVal))&($ZYISSQLNULL(secondColVal))) DO
	. . ; Two NULL values are considered equal
	. . SET curResult=$SELECT(">"=operator:0,"'="=operator:0,"<="=operator:1,">="=operator:1,"<"=operator:0,"="=operator:1,1:1)
	. . ; Unconditionally set `isEquals` as it is needed only for `<`,`<=`,`>` and `>=` than operation and code at the end
	. . ; selectively ignores its value for `=` and `!=` operation.
	. . SET isEquals=1
	. ELSE  IF ($ZYISSQLNULL(firstColVal)) DO
	. . ; NULL is considered > non-NULL
	. . SET curResult=$SELECT(">"=operator:1,"'="=operator:1,"<="=operator:0,">="=operator:1,"<"=operator:0,"="=operator:0,1:1)
	. ELSE  IF $ZYISSQLNULL(secondColVal) DO
	. . ; NULL is considered > non-NULL
	. . SET curResult=$SELECT(">"=operator:0,"'="=operator:1,"<="=operator:1,">="=operator:0,"<"=operator:1,"="=operator:0,1:1)
	. ELSE  IF ((("<"=operator)!(">"=operator)!(">="=operator)!("<="=operator))&(firstColVal=secondColVal))  DO
	. . ; Both operand values are equal
	. . ; Set `curResult` to `0` in case of `<` and `>` operation and allow the loop to compare other column values by setting
	. . ;   `isEquals` to 1.
	. . SET:(("<"=operator)!(">"=operator)) curResult=0,isEquals=1
	. . ; Set `curResult` to `1` in case of `>=` and `<=` operation and allow the loop to compare other column values by
	. . ;   setting `isEquals` to 1.
	. . SET:(("<="=operator)!(">="=operator)) curResult=1,isEquals=1
	. ELSE  DO
	. . ; Non-NULL operands and they are not equal. Compare based on column types.
	. . IF "t"=type DO
	. . . ; STRING_LITERAL or NUL_VALUE TYPE
	. . . ; There is a swap that is done before this IF block chain to handle $ZYSQLNULL case
	. . . ;   re-swap it so that we operate on the original passed operands.
	. . . IF (("<"=operator)!(">="=operator)) DO
	. . . . ; Re-swap only in these two operation cases as these are the only ones which require the reverse positioning of
	. . . . ;   operands.
	. . . . SET firstColVal=tmpFirstColVal
	. . . . SET secondColVal=tmpSecondColVal
	. . . IF (">"=operator) SET curResult=(firstColVal]secondColVal)
	. . . ELSE  IF ("'="=operator) SET curResult=(firstColVal'=secondColVal)
	. . . ELSE  IF ("<="=operator) SET curResult=(firstColVal']secondColVal)
	. . . ELSE  IF (">="=operator) SET curResult=(firstColVal']secondColVal)
	. . . ELSE  IF ("<"=operator) SET curResult=(firstColVal]secondColVal)
	. . . ELSE  SET curResult=(firstColVal=secondColVal) ; "="=operator
	. . ELSE  DO
	. . . ; 'f'= type. Choose operators accordingly.
	. . . IF (">"=operator) SET curResult=(firstColVal>secondColVal)
	. . . ELSE  IF ("'="=operator) SET curResult=(firstColVal'=secondColVal)
	. . . ELSE  IF ("<="=operator) SET curResult=(firstColVal<=secondColVal)
	. . . ELSE  IF (">="=operator) SET curResult=(firstColVal>=secondColVal)
	. . . ELSE  IF ("<"=operator) SET curResult=(firstColVal<secondColVal)
	. . . ELSE  SET curResult=(firstColVal=secondColVal) ; "="=operator
	. IF (("="=operator)&(0=curResult)) DO
	. . set limitingCompareReached=1
	. . QUIT
	. ELSE  IF (("'="=operator)&(1=curResult)) DO
	. . set limitingCompareReached=1
	. . QUIT
	. ELSE  IF (("<"=operator)!(">"=operator)!("<="=operator)!(">="=operator)) DO
	. . QUIT:(1=isEquals)  ; Both values are equal, QUIT right here so that next column values get compared
	. . ; We only reach the following code if the operands are not equal
	. . ;  so exit the loop as we found our comparison result.
	. . set limitingCompareReached=1
	. . QUIT
	set result=curResult
	QUIT result
