#!/bin/bash

source /opt/yottadb/current/ydb_env_set

mkdir build
cd build
git clone https://github.com/bats-core/bats-core.git
cd bats-core
./install.sh /usr/local
cd ..
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
make 2> make_warnings.txt
../tools/ci/sort_warnings.sh
echo -n "Checking for unexpected warning(s)... "
new_warnings="$(diff sorted_warnings.txt ../tools/ci/expected_warnings.txt)"
if new_warnings
then
  echo "FAIL: "
  echo $(new_warnings)
  exit 1
fi
echo "OK."
source activate
pushd src
$ydb_dist/mupip set -n=true -reg '*'
# Load the data
./octo -f ../../tests/fixtures/names.sql
$ydb_dist/mupip load ../../tests/fixtures/names.zwr
popd
make test

