;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2003-2016 Fidelity National Information		;
; Services, Inc. and/or its subsidiaries. All rights reserved.	;
;								;
; Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Note: This file is a copy of YDBTest repo 'com/job.m'
; and hence inherits the FIS copyright from there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input variables that are not passed as parameters
; ---------------------------------------------------
; jmaxwait --> If 0, implies return right after jobbing of children. non-zero implies max absolute time to wait for children to die.
; jprint   --> If defined, then will print more information. By default is not defined as it creates test reference file issues.
; jdetach  --> If defined, then it creates "detached" jobs. By default jobs are "nodetached"
; jmjoname --> If defined, this will cause <jmjoname>.mjo and <jmjoname>.mje to be the output and error log files of the job command
; jnoerrchk--> If defined to 1, this will cause *.mje to NOT be checked after the job is complete.
;		* Use this only if going to run LOTS (hundreds/thousands) of jobs that will otherwise take a long
;		* time to go through the *.mje check since it is done via a zsystem (a fork). In those cases, searching
;		* for errors at the test framework level through a single grep command might be faster.
; jobid    --> If defined, this will allow for concurrent multiple ^job() invocations. To achieve that,
;		* the jobid will be appended to the name of the .mjo and .mje files
;		* ^%jobwait will be subscripted with jobid
; jnomupipstop --> If defined, this implies that the crash is of type KILL -9 else crash is of type MUPIP STOP.
;		* For this to take effect, the logical gtm_test_crash should be defined in the first place.
; jzgbldir --> If defined, this will cause the jobbed off process to start with $zgbldir=<jzgbldir>
; jnolock  --> If defined, this will skip the LOCK commands done by ^job and thread^job.
;		* Note: This will no longer ensure all threads start off simultaneously.
;		* Which means it is possible one thread starts and finishes even before another thread starts.
; jpasscurlvn --> If 0, the parameters are passed using the actual list to the child (jobs off threadparm).
;  	      	  If 1, the paramaters are passed by using the PASSCURLVN mechanism instead of the actual list (jobs off threadnoparm)
;		  If undefined, a random value will be assigned to jpasscurlvn
;
job(routine,njobs,parms);
	new logstr,str,lckstr,unlckstr,$etrap,jroutine,jobindex,jdefwait,jobcomu,jobcomv,i
	set $etrap="goto error"
	if $data(jprint) set jprint=1
	if '$data(jprint) new jprint set jprint=0
	if '$data(jobid) new jobid set jobid=""
	if jprint=1 write $zver,!
	if jprint=1 write "$h = ",$h,!
	if $data(jnolock)=0 new jnolock set jnolock=0
	do:'jnolock
	.	set lckstr="lock +^job"_$piece(routine,"^",1)
	.	set unlckstr="lock -^job"_$piece(routine,"^",1)
	.	if jprint=1 write lckstr,!
	.	xecute lckstr
	; VMS job command does not accept label^routine as output or error file name so replace them with underscores "_"
	set jroutine=$translate(routine,"^","_")
	; If there was no label or the params are being passed to the jobbed off routine
	if "_"=$extract(jroutine,1) s jroutine=$piece($piece(jroutine,"_",2),"(",1)
	else    set jroutine=$piece(jroutine,"(",1)
	;
	set jroutine=jroutine_jobid	; append jobid for uniqueness (in case it exists)
	; In VMS, for a detached job, the LOG qualifier to the job command should have the full path
	;	including the current directory in it or the log file is written in sys$login
	if $data(jdetach)  set detachstr="detached",logstr=$zdir_jroutine
	if '$data(jdetach) new detachstr set detachstr="nodetached",logstr=jroutine
	new gldstr
	if $data(jzgbldir) set gldstr="gbl="""_jzgbldir_""":"
	else  set gldstr=""
	if '$data(jnoerrchk) new jnoerrchk  set jnoerrchk=0
	if '$data(jmjoname) new jmjoname set jmjoname=jroutine
	set jobidparm=$select(jobid'="":jobid,1:"""""")
	set jnolockparm=$select(jnolock:1,1:"""""")
	if $data(jpasscurlvn)=0 set jpasscurlvn=(($tr($piece($zversion," ",2),"V-","")'<6.2001)&(+$ztrnlnm("gtm_test_passcurlvn")))
	do:jpasscurlvn
	.	xecute "set parms="_parms ; Strip off the quotes around the parms since we will be directly using parms
	.	xecute "set jobidparm="_jobidparm
	.	xecute "set jnolockparm="_jnolockparm
	for index=1:1:njobs  do
	.	if jpasscurlvn set jobcom="Job threadnoparm^job:("_gldstr_"output="""_jmjoname_".mjo"_index_""":error="""_jmjoname_".mje"_index_""":PASSCURLVN)"
	.	else  set jobcom="Job threadparm^job("""_routine_""","_index_","_parms_","_jobidparm_","_jnolockparm_"):("_gldstr_"output="""_jmjoname_".mjo"_index_""":error="""_jmjoname_".mje"_index_""")"
	.	if jprint=1 write jobcom,!
	.	do
	.	.	new:jpasscurlvn (jobcom,routine,index,parms,jobidparm,jnolockparm,jnolock)
	.	.	xecute jobcom
	.	set jobindex(index)=$zjob
	if 'jnolock  do
	.	if jobid="" set trylock="lock +^%jobwait(i):0",unlock="lock -^%jobwait(i)"
	.	else  set trylock="lock +^%jobwait("_jobid_",i):0",unlock="lock -^%jobwait("_jobid_",i)"
	.	set all=0
	.	for try=1:1:600 do  quit:all  hang 1
	.	.	set all=1
	.	.	for i=1:1:njobs  do
	.	.	.	xecute trylock
	.	.	.	if  set all=0 xecute unlock    ;if we got the LOCK, a child has not started so release it
	.	if 'all write !,"Failed to start all jobs within 10 minutes. Releasing lock now. Jobs would not have started at the same time.",!
	.	if jprint=1 write unlckstr,!
	.	xecute unlckstr
	set jdefwait=7200	; max. wait of 2 hours (was previously 1 hour but some loaded systems seem to take longer)
	if $data(jmaxwait)=0 new jmaxwait set jmaxwait=jdefwait
	if jmaxwait<0 write "TEST-E-JOBJMAXWAITNEG : jmaxwait = ",jmaxwait,! goto error
	if jmaxwait'=0 do waitchld
	if jmaxwait=0  do
	.	;
	.	; store children pids if going to wait for those much later
	.	;
	.	if jobid=""  set ^%jobwait("njobs")=njobs
	.	else  set ^%jobwait(jobid,"njobs")=njobs
	.	set ^("jmaxwait")=jmaxwait
	.	set ^("jdefwait")=jdefwait
	.	set ^("jprint")=jprint
	.	set ^("jroutine")=jroutine
	.	set ^("jmjoname")=jmjoname
	.	set ^("jnoerrchk")=jnoerrchk
	.	for i=1:1:njobs  set ^(i)=jobindex(i)
	.	do writecrashfileifneeded
	.	do writejobinfofileifneeded
	quit

writecrashfileifneeded
	new crash,file
	set crash=+$ztrnlnm("gtm_test_crash")
	if (crash)  do
	.	if jobid=""  set file="gtm_test_crash_jobs",njobs=^%jobwait("njobs")
	.	else         set file="gtm_test_crash_jobs_"_jobid,njobs=^%jobwait(jobid,"njobs")
	.	set file=file_".csh",killstr="$gtm_dist/mupip stop ",sleepstr="sleep "
	.	set ^("crashfilename")="chmod +x "_file_"; ./"_file
	.	if $data(jnomupipstop)'=0  do
	.	.	set killstr="kill -9 "
	.	;
	.	set nprocs=+$ztrnlnm("gtm_test_crash_nprocs")
	.	if nprocs=0  set nprocs=njobs
	.	set maxdelay=+$ztrnlnm("gtm_test_crash_maxdelay")
	.	;
	.	open file:newversion
	.	use file
	.	for i=1:1:nprocs  do
	.	.	set pid=jobindex(i)
	.	.	write killstr,pid,!
	.	.	if maxdelay'=0  write sleepstr,$random(maxdelay),!
	.	close file
	.	use $p
	quit

writejobinfofileifneeded
	new needinfo,file,lastref
	set needinfo=($ztrnlnm("gtm_test_onlinerollback")="TRUE")
	quit:needinfo=0
	if jobid=""  set file="gtm_test_mjob_info",lastref=^%jobwait("njobs")
	else         set file="gtm_test_mjob_info_"_jobid,lastref=^%jobwait(jobid,"njobs")
	open file:newversion
	use file
	zwrite ^()	; note that above references to ^%jobwait set $REFERENCE for this to work
	close file
	use $p
	quit

loadjobinfofileifneeded
	new file,needinfo,njobs,line,i
	set needinfo=($ztrnlnm("gtm_test_onlinerollback")="TRUE")
	quit:needinfo=0
	if jobid=""  set file="gtm_test_mjob_info",needinfo=$get(^%jobwait($get(^%jobwait("njobs"))))
	else         set file="gtm_test_mjob_info_"_jobid,needinfo=$get(^%jobwait(jobid,$get(^%jobwait(jobid,"njobs"),-1)))
	quit:needinfo'=""
	open file:readonly
	use file
	for i=1:1  read line xecute:$length(line) "set "_line quit:$zeof
	close file
	use $p
	quit


wait	;
	; copies values stored in globals into locals and invokes waitchld to wait for children to die before returning
	;
	new $etrap
	set $etrap="goto error"
	new njobs,jdefwait,jprint,i,jobindex,jroutine,jmjoname,jnoerrchk
	if $data(jobid)=0 new jobid set jobid=""
	do loadjobinfofileifneeded
	if jobid=""  set njobs=^%jobwait("njobs")	; do not know if null subscripts will be allowed or not
	if jobid'="" set njobs=^%jobwait(jobid,"njobs")
	if $data(jmaxwait)=0 new jmaxwait set jmaxwait=^("jdefwait")
	set jdefwait=^("jdefwait")
	set jprint=^("jprint")
	set jroutine=^("jroutine")
	set jmjoname=^("jmjoname")
	set jnoerrchk=^("jnoerrchk")	; set local to 0 if global does not exist
	for i=1:1:njobs set jobindex(i)=^(i)
	do waitchld
	quit

waitchld;
	; wait for all children to die before returning
	; expects njobs,jmaxwait,jprint,jobindex to be defined appropriately
	;
	new j,childalive
	set childalive=njobs
	if jmaxwait'>0 new jmaxwait set jmaxwait=jdefwait
	set waitfile="jwait_ps_"_jmjoname_$select(jobid'="":"_"_jobid,1:"")_"_"_$job_".list"
	set zsywaitchld="ps -ef >& "_waitfile
	for j=1:1:jmaxwait  do  quit:childalive=0
	.	if jprint=1 write "$h = ",$h,!
	.	set childalive=0
	.	for i=1:1:njobs  set childalive=childalive+$$^isprcalv(jobindex(i))
	.	zsystem:childalive zsywaitchld
	.	if j=1,childalive  zsystem "cp "_waitfile_" first_"_waitfile
	.	hang:childalive'=0 1
	if (j'<jmaxwait) write "TEST-E-JOBWAITTOOLONG : Total children = ",njobs," : Still alive = ",childalive,! zshow "*"
	if jprint=1 write "$h = ",$h,!
	if 'jnoerrchk  do
	.	set zsyjnoerrchk="cat "_jmjoname_".mje*"
	.	zsystem zsyjnoerrchk
	quit

error	;
	use $p
        zshow "*"
        if $tlevel trollback
	quit

; This label is meant to be used without PASSCURLVN
threadparm(routine,index,parms,jobidparm,jnolock);
; This label is meant to be used with PASSCURLVN
threadnoparm
	if $data(jnolock)=0 set jnolock=0
	if $data(jobidparm)=0 set jobidparm=""
	if 'jnolock  do
	.	if jobidparm=""  set childlock="lock +^%jobwait("_index_")"
	.	if jobidparm'="" set childlock="lock +^%jobwait("_jobidparm_","_index_")"
	.	write childlock,!
	.	; lock %^jobwait to signal the parent that this child has reached a point and waiting to start execution
	.	xecute childlock
	.	set str="lock +^job"_$piece(routine,"^",1)_"("_index_","_$j_")"
	.	write str,!
	.	; ^job_$piece(routine,"^",1) is held by the parent and will be released once all the children reach this point.
	.	xecute str
	.	lock
	set jobindex=index
	if $piece(routine,"^",2)="" set str="d ^"
	else  set str="d "
	if parms="" set str=str_routine
	else  set str=str_routine_"("_parms_")"
	write str,!
	xecute str
	quit

childcnt();
	; Copies values stored in globals into locals and uses isprcalv to count children still running
	;
	new $etrap,childalive,njobs,i,jobindex
	set $etrap="goto error"
	if $data(jobid)=0 new jobid set jobid=""
	if jobid=""  set njobs=^%jobwait("njobs")	; do not know if null subscripts will be allowed or not
	if jobid'="" set njobs=^%jobwait(jobid,"njobs")
	for i=1:1:njobs set jobindex(i)=^(i)
	set childalive=0
	for i=1:1:njobs set childalive=childalive+$$^isprcalv(jobindex(i))
	quit childalive
