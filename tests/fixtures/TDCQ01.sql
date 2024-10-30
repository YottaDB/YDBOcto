#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDCQ01 : OCTO985 : DBeaver : Connect queries

-- The below are various queries that were found to be issued through DBeaver.
-- Some of them issue errors in Octo, but those still let Octo navigate table/column data/metadata and so they are not fixed yet.

-- [ERROR]: ERR_UNKNOWN_FUNCTION: No function pg_catalog.pg_encoding_to_char defined with given parameter types (INTEGER)
SELECT c.contoencoding as encid,pg_catalog.pg_encoding_to_char(c.contoencoding) as encname FROM pg_catalog.pg_conversion c GROUP BY c.contoencoding ORDER BY 2;

-- [ERROR]: ERR_UNKNOWN_TABLE_OR_VIEW: Unknown table or view: pg_catalog.pg_get_keywords
select string_agg(word, ',') from pg_catalog.pg_get_keywords() where word <> ALL ('{a,abs,absolute,action,ada,add,admin,after,all,allocate,alter,always,and,any,are,array,as,asc,asensitive,assertion,assignment,asymmetric,at,atomic,attribute,attributes,authorization,avg,before,begin,bernoulli,between,bigint,binary,blob,boolean,both,breadth,by,c,call,called,cardinality,cascade,cascaded,case,cast,catalog,catalog_name,ceil,ceiling,chain,char,char_length,character,character_length,character_set_catalog,character_set_name,character_set_schema,characteristics,characters,check,checked,class_origin,clob,close,coalesce,cobol,code_units,collate,collation,collation_catalog,collation_name,collation_schema,collect,column,column_name,command_function,command_function_code,commit,committed,condition,condition_number,connect,connection_name,constraint,constraint_catalog,constraint_name,constraint_schema,constraints,constructors,contains,continue,convert,corr,corresponding,count,covar_pop,covar_samp,create,cross,cube,cume_dist,current,current_collation,current_date,current_default_transform_group,current_path,current_role,current_time,current_timestamp,current_transform_group_for_type,current_user,cursor,cursor_name,cycle,data,date,datetime_interval_code,datetime_interval_precision,day,deallocate,dec,decimal,declare,default,defaults,deferrable,deferred,defined,definer,degree,delete,dense_rank,depth,deref,derived,desc,describe,descriptor,deterministic,diagnostics,disconnect,dispatch,distinct,domain,double,drop,dynamic,dynamic_function,dynamic_function_code,each,element,else,end,end-exec,equals,escape,every,except,exception,exclude,excluding,exec,execute,exists,exp,external,extract,false,fetch,filter,final,first,float,floor,following,for,foreign,fortran,found,free,from,full,function,fusion,g,general,get,global,go,goto,grant,granted,group,grouping,having,hierarchy,hold,hour,identity,immediate,implementation,in,including,increment,indicator,initially,inner,inout,input,insensitive,insert,instance,instantiable,int,integer,intersect,intersection,interval,into,invoker,is,isolation,join,k,key,key_member,key_type,language,large,last,lateral,leading,left,length,level,like,ln,local,localtime,localtimestamp,locator,lower,m,map,match,matched,max,maxvalue,member,merge,message_length,message_octet_length,message_text,method,min,minute,minvalue,mod,modifies,module,month,more,multiset,mumps,name,names,national,natural,nchar,nclob,nesting,new,next,no,none,normalize,normalized,not,"null",nullable,nullif,nulls,number,numeric,object,octet_length,octets,of,old,on,only,open,option,options,or,order,ordering,ordinality,others,out,outer,output,over,overlaps,overlay,overriding,pad,parameter,parameter_mode,parameter_name,parameter_ordinal_position,parameter_specific_catalog,parameter_specific_name,parameter_specific_schema,partial,partition,pascal,path,percent_rank,percentile_cont,percentile_disc,placing,pli,position,power,preceding,precision,prepare,preserve,primary,prior,privileges,procedure,public,range,rank,read,reads,real,recursive,ref,references,referencing,regr_avgx,regr_avgy,regr_count,regr_intercept,regr_r2,regr_slope,regr_sxx,regr_sxy,regr_syy,relative,release,repeatable,restart,result,return,returned_cardinality,returned_length,returned_octet_length,returned_sqlstate,returns,revoke,right,role,rollback,rollup,routine,routine_catalog,routine_name,routine_schema,row,row_count,row_number,rows,savepoint,scale,schema,schema_name,scope_catalog,scope_name,scope_schema,scroll,search,second,section,security,select,self,sensitive,sequence,serializable,server_name,session,session_user,set,sets,similar,simple,size,smallint,some,source,space,specific,specific_name,specifictype,sql,sqlexception,sqlstate,sqlwarning,sqrt,start,state,statement,static,stddev_pop,stddev_samp,structure,style,subclass_origin,submultiset,substring,sum,symmetric,system,system_user,table,table_name,tablesample,temporary,then,ties,time,timestamp,timezone_hour,timezone_minute,to,top_level_count,trailing,transaction,transaction_active,transactions_committed,transactions_rolled_back,transform,transforms,translate,translation,treat,trigger,trigger_catalog,trigger_name,trigger_schema,trim,true,type,uescape,unbounded,uncommitted,under,union,unique,unknown,unnamed,unnest,update,upper,usage,user,user_defined_type_catalog,user_defined_type_code,user_defined_type_name,user_defined_type_schema,using,value,values,var_pop,var_samp,varchar,varying,view,when,whenever,where,width_bucket,window,with,within,without,work,write,year,zone}'::text;

-- The below issued a syntax error in Octo.
SHOW TRANSACTION ISOLATION LEVEL;

-- The below query used $1 in DBeaver but has been replaced with 1 for automated test purposes
SELECT i.*,c.relnamespace FROM pg_catalog.pg_inherits i,pg_catalog.pg_class c WHERE i.inhrelid=1 AND c.oid=i.inhparent ORDER BY i.inhseqno;

-- The below query used $1 in DBeaver but has been replaced with 'name' for automated test purposes
select description from pg_shdescription join pg_database on objoid = pg_database.oid where datname = 'name';

-- The below queries issued no errors in Octo
SELECT r.oid,r.*, pg_get_ruledef(r.oid) AS definition FROM pg_rewrite r WHERE r.ev_class=81 AND r.rulename <> '_RETURN'::name;
select * from pg_catalog.pg_policies where schemaname='efgh' and tablename='abcd';
SELECT a.oid,a.*,pd.description FROM pg_catalog.pg_roles a left join pg_catalog.pg_shdescription pd on a.oid = pd.objoid ORDER BY a.rolname;
SELECT c.oid,c.*,d.description,pg_catalog.pg_get_expr(c.relpartbound, c.oid) as partition_expr, pg_catalog.pg_get_partkeydef(c.oid) as partition_key , dep.objid, dep.refobjsubid FROM pg_catalog.pg_class c LEFT OUTER JOIN pg_catalog.pg_description d ON d.objoid=c.oid AND d.objsubid=0 AND d.classoid='pg_class'::regclass LEFT OUTER JOIN pg_depend dep on dep.refobjid = c."oid" AND dep.deptype = 'i' and dep.refobjsubid <> 0 and dep.classid = dep.refclassid WHERE c.relnamespace=1 AND c.relkind not in ('i','I','c');
SELECT c.oid,c.*,t.relname as tabrelname,rt.relnamespace as refnamespace,d.description, case when c.contype='c' then "substring"(pg_get_constraintdef(c.oid), 7) else null end consrc_copy FROM pg_catalog.pg_constraint c INNER JOIN pg_catalog.pg_class t ON t.oid=c.conrelid LEFT OUTER JOIN pg_catalog.pg_class rt ON rt.oid=c.confrelid LEFT OUTER JOIN pg_catalog.pg_description d ON d.objoid=c.oid AND d.objsubid=0 AND d.classoid='pg_constraint'::regclass WHERE c.conrelid=1 ORDER BY c.oid;
select c.oid,pg_catalog.pg_total_relation_size(c.oid) as total_rel_size,pg_catalog.pg_relation_size(c.oid) as rel_size FROM pg_class c WHERE c.relnamespace=1;
SELECT c.relname,a.*,pg_catalog.pg_get_expr(ad.adbin, ad.adrelid, true) as def_value,dsc.description FROM pg_catalog.pg_attribute a INNER JOIN pg_catalog.pg_class c ON (a.attrelid=c.oid) LEFT OUTER JOIN pg_catalog.pg_attrdef ad ON (a.attrelid=ad.adrelid AND a.attnum = ad.adnum) LEFT OUTER JOIN pg_catalog.pg_description dsc ON (c.oid=dsc.objoid AND a.attnum = dsc.objsubid) WHERE NOT a.attisdropped AND c.oid=1 ORDER BY a.attnum;
SELECT db.oid,db.* FROM pg_catalog.pg_database db WHERE datname='names';
SELECT DISTINCT connamespace FROM pg_catalog.pg_constraint WHERE confrelid=1;
SELECT i.*,i.indkey as keys,c.relname,c.relnamespace,c.relam,c.reltablespace,tc.relname as tabrelname,dsc.description,pg_catalog.pg_get_expr(i.indpred, i.indrelid) as pred_expr,pg_catalog.pg_get_expr(i.indexprs, i.indrelid, true) as expr,pg_catalog.pg_relation_size(i.indexrelid) as index_rel_size,pg_catalog.pg_stat_get_numscans(i.indexrelid) as index_num_scans FROM pg_catalog.pg_index i INNER JOIN pg_catalog.pg_class c ON c.oid=i.indexrelid INNER JOIN pg_catalog.pg_class tc ON tc.oid=i.indrelid LEFT OUTER JOIN pg_catalog.pg_description dsc ON i.indexrelid=dsc.objoid WHERE i.indrelid=1 ORDER BY c.relname;
SELECT x.oid,x.*,p.pronamespace as func_schema_id,d.description FROM pg_catalog.pg_trigger x LEFT OUTER JOIN pg_catalog.pg_proc p ON p.oid=x.tgfoid LEFT OUTER JOIN pg_catalog.pg_description d ON d.objoid=x.oid AND d.objsubid=0 WHERE x.tgrelid = 1 AND NOT x.tgisinternal;
SELECT t.oid,t.*,pg_tablespace_location(t.oid) loc FROM pg_catalog.pg_tablespace t ORDER BY t.oid;

select c.relname, attname, format_type(a.atttypid, a.atttypmod) from pg_attribute a, pg_class c where a.attrelid = c.oid and attnum > 0;

SELECT c.oid,c.*,d.description,pg_catalog.pg_get_expr(c.relpartbound, c.oid) as partition_expr,  pg_catalog.pg_get_partkeydef(c.oid) as partition_key , dep.objid, dep.refobjsubid
FROM pg_catalog.pg_class c
LEFT OUTER JOIN pg_catalog.pg_description d ON d.objoid=c.oid AND d.objsubid=0 AND d.classoid='pg_class'::regclass
LEFT OUTER JOIN pg_depend dep on dep.refobjid = c."oid" AND dep.deptype = 'i' and dep.refobjsubid <> 0 and dep.classid = dep.refclassid
WHERE c.relnamespace=2200 AND c.relkind not in ('i','I','c');

SELECT c.oid,c.*,d.description,pg_catalog.pg_get_expr(c.relpartbound, c.oid) as partition_expr,  pg_catalog.pg_get_partkeydef(c.oid) as partition_key
FROM pg_catalog.pg_class c
LEFT OUTER JOIN pg_catalog.pg_description d ON d.objoid=c.oid AND d.objsubid=0 AND d.classoid='pg_class'::regclass
WHERE c.relnamespace=2200 AND c.relkind not in ('i','I','c');

-- 20-way LEFT JOIN DBeaver query with * as select column list and NO WHERE clause
-- Also at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/985#note_1496159032
SELECT count(*)
FROM pg_depend dep
LEFT JOIN pg_class cl ON dep.objid=cl.oid
LEFT JOIN pg_attribute att ON dep.objid=att.attrelid AND dep.objsubid=att.attnum
LEFT JOIN pg_namespace nsc ON cl.relnamespace=nsc.oid
LEFT JOIN pg_proc pr ON dep.objid=pr.oid
LEFT JOIN pg_namespace nsp ON pr.pronamespace=nsp.oid
LEFT JOIN pg_trigger tg ON dep.objid=tg.oid
LEFT JOIN pg_class tgr ON tg.tgrelid=tgr.oid
LEFT JOIN pg_namespace tgrn ON tgr.relnamespace=tgrn.oid
LEFT JOIN pg_type ty ON dep.objid=ty.oid
LEFT JOIN pg_namespace nst ON ty.typnamespace=nst.oid
LEFT JOIN pg_constraint co ON dep.objid=co.oid
LEFT JOIN pg_class coc ON co.conrelid=coc.oid
LEFT JOIN pg_namespace nso ON co.connamespace=nso.oid
LEFT JOIN pg_rewrite rw ON dep.objid=rw.oid
LEFT JOIN pg_class clrw ON clrw.oid=rw.ev_class
LEFT JOIN pg_namespace nsrw ON clrw.relnamespace=nsrw.oid
LEFT JOIN pg_language la ON dep.objid=la.oid
LEFT JOIN pg_namespace ns ON dep.objid=ns.oid
LEFT JOIN pg_attrdef ad ON ad.oid=dep.objid
LEFT JOIN pg_attribute attr ON attr.attrelid=ad.adrelid and attr.attnum=ad.adnum;

-- Actual 20-way LEFT JOIN query when clicking Dependencies in DBeaver client
-- Also at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/985#note_1523644251
SELECT DISTINCT dep.deptype, dep.classid, dep.objid, cl.relkind, attr.attname,pg_get_expr(ad.adbin, ad.adrelid) adefval,
    CASE WHEN cl.relkind IS NOT NULL THEN cl.relkind::text || COALESCE(dep.objsubid::text, '')::text
        WHEN tg.oid IS NOT NULL THEN 'T'::text
        WHEN ty.oid IS NOT NULL THEN 'y'::text
        WHEN ns.oid IS NOT NULL THEN 'n'::text
        WHEN pr.oid IS NOT NULL THEN 'p'::text
        WHEN la.oid IS NOT NULL THEN 'l'::text
        WHEN rw.oid IS NOT NULL THEN 'R'::text
        WHEN co.oid IS NOT NULL THEN 'C'::text || contype::text
        WHEN ad.oid IS NOT NULL THEN 'A'::text
        ELSE ''
    END AS type,
    COALESCE(coc.relname, clrw.relname, tgr.relname) AS ownertable,
    CASE WHEN cl.relname IS NOT NULL AND att.attname IS NOT NULL THEN cl.relname || '.' || att.attname
    ELSE COALESCE(cl.relname, co.conname, pr.proname, tg.tgname, ty.typname, la.lanname, rw.rulename, ns.nspname)
    END AS refname,
    COALESCE(nsc.nspname, nso.nspname, nsp.nspname, nst.nspname, nsrw.nspname, tgrn.nspname) AS nspname
FROM pg_depend dep
LEFT JOIN pg_class cl ON dep.objid=cl.oid
LEFT JOIN pg_attribute att ON dep.objid=att.attrelid AND dep.objsubid=att.attnum
LEFT JOIN pg_namespace nsc ON cl.relnamespace=nsc.oid
LEFT JOIN pg_proc pr ON dep.objid=pr.oid
LEFT JOIN pg_namespace nsp ON pr.pronamespace=nsp.oid
LEFT JOIN pg_trigger tg ON dep.objid=tg.oid
LEFT JOIN pg_class tgr ON tg.tgrelid=tgr.oid
LEFT JOIN pg_namespace tgrn ON tgr.relnamespace=tgrn.oid
LEFT JOIN pg_type ty ON dep.objid=ty.oid
LEFT JOIN pg_namespace nst ON ty.typnamespace=nst.oid
LEFT JOIN pg_constraint co ON dep.objid=co.oid
LEFT JOIN pg_class coc ON co.conrelid=coc.oid
LEFT JOIN pg_namespace nso ON co.connamespace=nso.oid
LEFT JOIN pg_rewrite rw ON dep.objid=rw.oid
LEFT JOIN pg_class clrw ON clrw.oid=rw.ev_class
LEFT JOIN pg_namespace nsrw ON clrw.relnamespace=nsrw.oid
LEFT JOIN pg_language la ON dep.objid=la.oid
LEFT JOIN pg_namespace ns ON dep.objid=ns.oid
LEFT JOIN pg_attrdef ad ON ad.oid=dep.objid
LEFT JOIN pg_attribute attr ON attr.attrelid=ad.adrelid and attr.attnum=ad.adnum
WHERE dep.refobjid=1
ORDER BY type;

