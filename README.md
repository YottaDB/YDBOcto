# YDB Octo

[![pipeline status](https://gitlab.com/YottaDB/DBMS/YDBOcto/badges/master/pipeline.svg)](https://gitlab.com/YottaDB/DBMS/YDBOcto/commits/master)

The YottaDB Octo Database Management System is a SQL access layer built on top of the not-only-SQL database YottaDB.
It aims to provide SQL 92 compliance and exceptional performance.

Homepage: https://gitlab.com/YottaDB/DBMS/YDBOcto

Documentation: https://docs.yottadb.com/Octo/index.html

## Setup

YottaDB r1.28 or greater is required for successful installation of Octo. Installing and configuring YottaDB is described on its own [documentation page](https://docs.yottadb.com/AdminOpsGuide/installydb.html).

*NOTE: the environment variable `$ydb_dist` is required to be defined - `$gtm_dist` is not a valid subsitute*

## Contributing

To contribute or help with further development, [fork the repository](https://docs.gitlab.com/ee/gitlab-basics/fork-project.html), clone your fork to a local copy and begin contributing! Please also set up the pre-commit script to automatically enforce some coding conventions. Assuming you are in the top-level directory, the following will work:

```sh
ln -s ../../pre-commit .git/hooks
```

Note that this script will require `tcsh` and `clang-format-9` or a later release.

```sh
# Ubuntu 20.04
sudo apt install clang-format-9
# Any Debian-like distro; see also https://apt.llvm.org/
bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
# CentOS 8
sudo yum install clang-tools-extra
```

### YDBCMake

Octo uses the upstream [YDBCMake] repository for building using YottaDB as the M compiler. Any changes to `ydbcmake/` should first be upstreamed to that repository.
Once the changes are upstreamed, you can merge them into Octo using
```
git pull --no-rebase git@gitlab.com:YottaDB/Tools/YDBCMake.git
```

[YDBCMake]: https://gitlab.com/YottaDB/Tools/YDBCMake

## Quickstart

### Install prerequisites

#### Install YottaDB POSIX plugin

The YottaDB POSIX plugin can be installed easily by adding the `--posix` option when installing YottaDB with the `ydbinstall` script:

```sh
./ydbinstall --posix
```

Alternatively, users can build the POSIX plugin from source:

```sh
# In a temporary directory perform the following commands
curl -fSsLO https://gitlab.com/YottaDB/Util/YDBPosix/-/archive/master/YDBPosix-master.tar.gz
tar xzf YDBPosix-master.tar.gz
cd YDBPosix-master
mkdir build && cd build
# Make sure that you have YottaDB environment variables in your shell before continuing
cmake ..
make -j `grep -c ^processor /proc/cpuinfo` && sudo make install
```

More detailed instructions are on the [YottaDB POSIX plugin page](https://gitlab.com/YottaDB/Util/YDBPosix/blob/master/README.md).

#### (Optional) Install YottaDB encryption plugin

Installing the YottaDB encryption plugin enables TLS support (Recommended for production installations). You will need to make sure TLS/SSL is enabled for the driver in the client software chosen.

The YottaDB encryption plugin can be installed by adding the `--encplugin` option when installing YottaDB with the `ydbinstall` script:

```sh
./ydbinstall --encplugin
```

Alternatively, users can build the encryption plugin from source:

```sh
# In a temporary directory perform the following commands
sudo tar -xf $ydb_dist/plugin/gtmcrypt/source.tar
# Make sure that you have YottaDB environment variables in your shell before continuing
sudo ydb_dist=$ydb_dist make -j `grep -c ^processor /proc/cpuinfo`
sudo ydb_dist=$ydb_dist make install
```

### Install Octo

Note: there are no binary releases during the beta period

#### From Tarball

1. Retrieve a YDBOcto binary package for your platform from GitLab (links will be provided when Octo 1.0 is released).

2. Decompress the Octo binary package, e.g.

```sh
tar xzf yottadb_octo_1.0.0_ubuntu18.04_x8664_pro.tar.gz
```

3. Install Octo

This will install Octo to your `$ydb_dist/plugin` directory. If you are not running as root, you may need to prefix the install script with `sudo -E`

```sh
cd yottadb_octo_1.0.0_ubuntu18.04_x8664_pro
./octoinstall.sh
```

4. Redefine environment variables to include newly installed files:

```sh
$ydb_dist/ydb_env_unset
source $(pkg-config --variable=prefix yottadb)/ydb_env_set
```

#### From source

Note: This is the recommended instructions during the beta period as it provides the easiest upgrade path from each commit.

1. Install prerequisite packages

```sh
# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
sudo apt-get install build-essential cmake bison flex xxd libreadline-dev libconfig-dev libssl-dev

# CentOS Linux OR RedHat Linux
# Note: epel-release has to be installed before cmake3 is installed
sudo yum install epel-release
sudo yum install vim-common cmake3 bison flex readline-devel libconfig-devel openssl-devel
```

2. (Optional) Install Bats Automated Test System (BATS)

Octo uses BATS for automated integration and regression testing. To use BATS to run tests on Octo, BATS version 1.1+ must be installed:

```sh
git clone https://github.com/bats-core/bats-core.git
cd bats-core
sudo ./install.sh /usr
```

This will install BATS to /usr/bin. Note that installing to /usr may require root access or use of `sudo`. To specify an alternative path change the argument to your preferred location, e.g. "/usr/local" to install to /usr/local/bin.

Details available in the [BATS source repo](https://github.com/bats-core/bats-core).

Some bats tests also require go, java and expect.
To run these, the appropriate libraries must installed:

```sh
# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
sudo apt-get install default-jdk expect golang-go

# CentOS Linux OR RedHat Linux
sudo yum install java-latest-openjdk expect golang
```

Additionally, some tests require a JDBC driver. The JDBC driver must be downloaded to the build directory and JDBC_VERSION must be set in the environment. Versions starting with 42.2.6 are tested, but earlier versions may work. For example, 42.2.12 is the latest release at time of writing:

```sh
export JDBC_VERSION=42.2.12
wget https://jdbc.postgresql.org/download/postgresql-$JDBC_VERSION.jar
```

3. (Optional) Install cmocka unit testing framework

Octo uses cmocka for automated unit testing. To build and run Octo's unit tests, cmocka must be installed:

```sh
# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
sudo apt-get install libcmocka-dev

# CentOS Linux OR RedHat Linux
sudo yum install libcmocka-devel
```

4. (Optional) Install PostgreSQL client (psql)

Octo uses the psql PostgreSQL for some integration/regression tests. To build and run these tests, psql must be installed:

```sh
# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
sudo apt-get install postgresql-client

# CentOS Linux OR RedHat Linux
sudo yum install postgresql
```

5. (Optional) Install PostgreSQL server

Octo uses the PostgreSQL server for some integration/regression tests. To build and run these tests, PostgreSQL must be installed:

```sh
# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
sudo apt-get install postgresql

# CentOS Linux OR RedHat Linux
sudo yum install postgresql
```

Additionally, PostgreSQL must be set up for the user who will be running the tests:

```sh
sudo -u postgres createuser [username]
sudo -u postgres psql <<PSQL
alter user [username] createdb;
PSQL
```

6. Download Octo Source Code

```sh
# In a temporary directory perform the following commands
curl -fSsLO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/archive/master/YDBOcto-master.tar.gz
tar xzf YDBOcto-master.tar.gz
cd YDBOcto-master
```

7. Compile Octo

```sh
mkdir build
cd build
# For VistA the String Buffer Length needs to be larger (described below) add `-DSTRING_BUFFER_LENGTH=300000` to the cmake command below
cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. # for CentOS/RedHat use cmake3 instead
make -j `grep -c ^processor /proc/cpuinfo`
```

To generate a Debug build instead of a Release build (the default), add `-DCMAKE_BUILD_TYPE=Debug` to the CMake line above.

To additionally disable the generation of installation rules for `make install`, add `-DDISABLE_INSTALL=ON`. This can be useful when doing testing in a temporary build directory only.

To build the full test suite rather than a subset of it, the `FULL_TEST_SUITE` option needs to be set to `ON`, e.g. `cmake -D FULL_TEST_SUITE=ON ..`.

**NOTE**: Octo uses some CMake parameters to control generation of fixed-size buffer allocations. These are:

* `STRING_BUFFER_LENGTH` -- the maximum length of a string within the system; this supersedes any VARCHAR definitions.
* `INIT_M_ROUTINE_LENGTH` -- the initial length for the buffer of generated M routines. The default is 10MB.
* `MEMORY_CHUNK_SIZE` -- size of memory chunks to allocate; default is 32MB.
* `MEMORY_CHUNK_PROTECT` -- if non-zero, memory following chunks is protected to detect buffer overflows. If 2, data is placed closer to the protected region to increase the chances of detecting an error.

Example usage of above parameters:

```sh
cmake -DSTRING_BUFFER_LENGTH=600000 -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin ..
```

8. Install Octo

Install Octo:

```sh
sudo -E make install
```

Redefine environment variables to include newly installed files:

```sh
$ydb_dist/ydb_env_unset
source $(pkg-config --variable=prefix yottadb)/ydb_env_set
```

Note: New Octo installations include a default `octo.conf` configuration file at `$ydb_dist/plugin/octo/octo.conf`, which may be modified post-install. Re-installing Octo will *not* overwrite an existing `octo.conf` in this location, so modifications to this file will be preserved across installations.

### Configure Octo

#### Setup environment variables

The following environment variables must be set for Octo to operate properly:

* `ydb_dist`
* `ydb_gbldir`
* `ydb_routines`
* `ydb_xc_ydbposix`

The environment variables `ydb_dist`, `ydb_gbldir`, and `ydb_routines` can initially be set by sourcing `ydb_env_set` in your YottaDB installation directory. Additional modifications to ydb_routines may be needed due to configuration in `octo.conf` described later in this manual.

Example setting of the environment variables (assuming default paths):

```sh
source /usr/local/lib/yottadb/r1.28/ydb_env_set
export ydb_xc_ydbposix=$ydb_dist/plugin/ydbposix.xc
```

#### Setup Database

Octo uses several internal global variables to map a SQL schema/DDL to a YottaDB database: %ydboctoschema, %ydboctoxref, and %ydboctoocto. It is best practice to map these to a separate region that is exclusive to Octo, which requires settings that may conflict with those required by other regions. For more information, refer to the Additional Configuration section below.

Please see the following example for creating a database from scratch with the recommended settings. For more information on setting up a database in YottaDB, refer to the [Administration and Operations Guide](https://docs.yottadb.com/AdminOpsGuide/index.html).

The below commands create an Octo database in the `build` directory. The build directory may be empty; all relevant files will be created.

```sh
$ cd build
$ export ydb_gbldir="$(pwd -P)/octo.gld"
$ echo >gde.cmd "\
add -segment OCTO -access_method=bg -file_name=$(pwd -P)/octo.dat
add -region OCTO -dynamic=octo -journal=(before,file=\"$(pwd -P)/octo.mjl\") -null_subscripts=always -key_size=1019 -record_size=300000
add -name %ydboctoschema -region=octo
add -name %ydboctoxref -region=octo
add -name %ydboctoocto -region=octo
verify
exit"
$ $ydb_dist/mumps -r GDE @gde.cmd
$ mupip create
```

#### Install Octo seed data

Note: This step is mandatory for full Octo functionality, as it loads internal system tables and functions required both by clients and Octo's own test system.

```sh
$ydb_dist/mupip load $ydb_dist/plugin/octo/octo-seed.zwr
$ydb_dist/plugin/bin/octo -f $ydb_dist/plugin/octo/octo-seed.sql
```

#### Test with dummy data

The `northwind` dummy data set can be found in the `tests/fixtures` directory of the YDBOcto repository. These are typically used for automated testing, but can also be used for manual testing.

Each dummy data set consists of a `.zwr` file and a `.sql` file. The former contains the actual data to be stored in YottaDB, while the latter contains a schema that maps relational SQL structures (tables and columns) to the NoSQL data contained in YottaDB.

Accordingly, to use this dummy data, both the data and DDL must be loaded. For example, to load these data sets from a temporary build directory within the YDBOcto repo (`YDBOcto/build`):

```sh
$ydb_dist/mupip load ../tests/fixtures/northwind.zwr
$ydb_dist/plugin/bin/octo -f ../tests/fixtures/northwind.sql
```

Once loaded, these data sets may be queried with standard SQL queries.


## Additional Configuration

Octo currently looks for a configuration file in the following directories:

* $ydb_dist/plugin/octo/octo.conf
* ~/octo.conf
* ./octo.conf

If the same setting exists in more than one configuration file the setting in the later file (according to the list above) will prevail. An example config file can be found in `$ydb_dist/plugin/octo/octo.conf`.

### Routines

Octo requires that `$ydb_dist/plugin/o/_ydbocto.so` and `$ydb_dist/plugin/o/_ydbposix.so` be included in `$ydb_routines`. This is necessary not only for running the `octo` and `rocto` excutables, but also for correctly updating and maintaining the YottaDB triggers that are used to maintain cross references for Octo. Accordingly, these paths should be added to `$ydb_routines` in your normal environment setup scripts.

### Globals

All octo related globals are prefixed with `^%ydbocto`. Using normal global mapping procedures for an existing application global directory (where you want to run Octo), map the global variable namespace `^%ydbocto*` to a separate region (and its associated database file) that meets the below requirements (the below example commands assume the separate region is named `OCTO`).

* `NULL_SUBSCRIPTS` must be set to `ALWAYS`.
  * Example: `$ydb_dist/mupip set -null_subscripts=true -region 'OCTO'`
* `KEY_SIZE` must be tuned to your data - this can be set to the maximum allowed by YottaDB - `1019`.
  * Example: `$ydb_dist/mupip set -key_size=1019 -region 'OCTO'`
* `RECORD_SIZE` must be tuned to your data/queries - a reasonable starting value is `300000`.
  * Example: `$ydb_dist/mupip set -record_size=300000 -region 'OCTO'`

### TLS/SSL Configuration

Enabling TLS/SSL requires several additional steps beyond installing the YottaDB encryption plugin - it requires creating a Certificate Authority (CA), generating a TLS/SSL certificate, and making additional changes to `octo.conf`

#### Generate CA key and certificate

```sh
# In a directory in which you want to store all of the certificates for Octo
# Be sure to create a good passphrase for the CA
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out CA.key
# This creates a CA valid for 1 Year and interactively prompts for additional information
openssl req -new -nodes -key CA.key -days 365 -x509 -out CA.crt
```

#### Create server key and certificate request

```sh
# This creates a 2048 bit private key
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out server.key
# This creeates the certificate signing request
openssl req -new -key server.key -out server.csr
```

#### Sign certificate based on request and local CA

```sh
# Asks the CA to sign the certificate with a 1 Year validity time
openssl x509 -req -in server.csr -CA CA.crt -CAkey CA.key -CAcreateserial \
        -out server.crt -days 365
# Mask the password for the certificate in a way YottaDB understands
$ydb_dist/plugin/gtmcrypt/maskpass
# This will need to be added to any startup scripts for octo/rocto
export ydb_tls_passwd_OCTOSERVER=[Masked Password from maskpass]
export ydb_crypt_config=/path/to/octo.conf
```

#### Update Octo configuration file

The $ydb_dist/plugin/octo/octo.conf contains an outline of the minimum configuration options needed to enable TLS/SSL. The key items are:

1. In the "rocto" section, "ssl_on" must be set to "true" (no quotes needed in the conf).
2. A "tls" section must be present and generally conform to the requirements specified for the [TLS plugin itself](https://docs.yottadb.com/AdminOpsGuide/tls.html). Other notes:
      * Octo doesn't use any of the "dh*" settings, so those can be omitted.
      * The "format" specifier can also be omitted, as long as the certs are in PEM format.
      * The "CAfile" and "CApath" fields are mandatory and must point to valid files/locations with a full path.
      * A subsection named "OCTOSERVER" with "key", and "cert" settings specifying the names of the private key and cert files.
3. The ydb_tls_passwd_OCTOSERVER and ydb_crypt_config environment variables must be set correctly.

## Usage

Before running Octo/Rocto make sure that the required YottaDB variables are set either by creating your own script or run `source $ydb_dist/ydb_env_set`.

To use the command-line SQL interpreter run: `$ydb_dist/plugin/bin/octo`.
To use the PostgreSQL protocol compatible server run: `$ydb_dist/plugin/bin/rocto`.

### Docker container

A docker container is available in this repository and on [docker hub](https://hub.docker.com/u/yottadb/octo). This docker container automatically starts `rocto`. The container is built with the following assumptions:

* The `ydb_env_set` script is used to setup the YottaDB environment and creates/expects a specific layout for globals and routines, specifically:
  * a `r1.28_x86_64` directory with the following sub directories:
    * `g` directory which contains at a minimum:
    * `yottadb.gld` global directory
    * `o` directory which contains the compiled M code
    * `r` directory which contains the source M code
  * a `r` directory which contains the source M code
* The octo default configuration is used in `/opt/yottadb/current/plugin/octo/octo.conf`

### Starting the docker container

To start the Docker container and make rocto available on the host's network on the default port of 1337 (unless octo.conf within the container is configured otherwise):

```sh
docker run -it --network=host yottadb/octo:latest-master
```

To login with the default `ydb` user use `psql` and enter `ydbrocks` when prompted for a password:

```sh
psql -U ydb -h localhost -p 1337
```

If you'd like to use YDB data in an existing local directory structure, then issue the `docker run` command from a directory where the above directory structure is defined. This is needed to mount it as a volume within the Docker container.

```sh
docker run -it -v `pwd`:/data yottadb/octo:latest-master
```

This will then display the rocto log file on stdout. If you'd prefer to run the container as a daemon use the `-d` command line parameter to run the container as a daemon. Also, if you'd like to publish the container on specific ports, specify this with the `-p` option. For example:

```sh
docker run -itd -v `pwd`:/data -p 1337:1337 yottadb/octo:latest-master
```

The logs can then be retrieved using the `docker logs` command with the container name or ID as an argument.

### Getting access to the container

#### PostgreSQL wire protocol

The rocto server is listening on port 1337 and all of the directions in the above documentation apply.

#### Command-line access

You can use the `docker exec` command to get access to the container for more troubleshooting. Example:

```sh
docker exec -it {nameOfContainer/IDOfContainer} /bin/bash
```
