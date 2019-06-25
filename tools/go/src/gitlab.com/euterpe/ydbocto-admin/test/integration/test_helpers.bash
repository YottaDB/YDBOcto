#################################################################
#								                                #
#   Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	    #
#   All rights reserved.						                #
#								                                #
#	This source code contains the intellectual property	        #
#	of its copyright holder(s), and is made available	        #
#	under a license.  If you do not know the terms of	        #
#	the license, please stop and do not read further.	        #
#								                                #
#################################################################

init_test() {
  PROJECTDIR=$GOPATH/src/gitlab.com/euterpe/ydbocto-admin
  cd $PROJECTDIR
  if [ ! -d $PROJECTDIR/bin ]; then
    mkdir $PROJECTDIR/bin
  fi
  go build -o $PROJECTDIR/bin/ydbocto-admin $PROJECTDIR/cmd/ydbocto-admin/main.go
  export PATH="$PROJECTDIR/bin:$PATH"
  if [ ! -d $PROJECTDIR/build ]; then
    mkdir $PROJECTDIR/build
  fi
  export test_temp=$(mktemp -d $PROJECTDIR/build/bats-test.XXXXXX)
  echo "Temporary files in: $test_temp"
  exec >  >(tee -ia $test_temp/stdout.txt)
  exec 2> >(tee -ia $test_temp/stderr.txt >&2)
  date
  cd $test_temp
}

createdb() {
  unset ydb_gbldir gtmgbldir	# needed or else ydb_env_set can issue ZGBLDIRACC error (due to it calling MUPIP DUMPFHEAD)
				# if ydb_gbldir is defined and points to a non-existent gld file.
  source $(pkg-config --variable=prefix yottadb)/ydb_env_set
  export ydb_gbldir="$test_temp/mumps.gld"
  echo "ydb_gbldir: $ydb_gbldir"
  $(pkg-config --variable=prefix yottadb)/mumps -r ^GDE <<FILE
change -r DEFAULT -key_size=1019 -record_size=32768
change -segment DEFAULT -file_name=$test_temp/mumps.dat
change -r DEFAULT -NULL_SUBSCRIPTS=true
exit
FILE
 $(pkg-config --variable=prefix yottadb)/mupip create
}

load_fixture() {
  fixture_name=$1
  if [[ $fixture_name == *.zwr ]]; then
    $(pkg-config --variable=prefix yottadb)/mupip load  $PROJECTDIR/internal/fixtures/$fixture_name
  else
    exit 1
  fi
}

verify_output() {
  echo "Comparing outref/$1 $2"
  copy_test_files outref/$1.ref
  diff outref/$1.ref $2
  return
}
