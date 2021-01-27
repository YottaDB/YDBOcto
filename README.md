# YDB Octo

[![pipeline status](https://gitlab.com/YottaDB/DBMS/YDBOcto/badges/master/pipeline.svg)](https://gitlab.com/YottaDB/DBMS/YDBOcto/commits/master)

Octo<sup>®</sup> is a SQL database engine whose tables are stored in YottaDB global variables (i.e., YottaDB hierarchical key-value nodes). Octo is installed as a YottaDB plugin.

*NOTE: As Octo at this time supports read-only access to YottaDB databases, you must already have a YottaDB database with global variables to use Octo. It is not yet read-write SQL database.*

Homepage: https://gitlab.com/YottaDB/DBMS/YDBOcto

Documentation: https://docs.yottadb.com/Octo/

Octo requires [YottaDB](https://gitlab.com/YottaDB/DB/YDB) r1.30 or greater. Installing and configuring YottaDB is described on its [documentation page](https://docs.yottadb.com/AdminOpsGuide/installydb.html).

*NOTE: Octo is a YottaDB application, not an application that runs on the upstream GT.M for which YottaDB is a drop-in upward-compatible replacement. Octo requires `ydb*` environment variables to be defined, and does not recognize the `gtm*` environnment variables. Specifically, it requires `ydb_dist` to be defined.*

## Quickstart

Install YottaDB, Octo, and the required POSIX plugin all together.

```sh
mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
cd /tmp/tmp ; chmod +x ydbinstall.sh
sudo ./ydbinstall.sh --utf8 default --verbose --octo --posix
```

`./ydbinstall.sh --help` gives a full list of its numerous options.

The [Quickstart section of the Octo user documentation](https://docs.yottadb.com/Octo/intro.html#quickstart) has more details.

## Installation from Source

### Prerequisites

#### Install YottaDB POSIX plugin

If you do not install the YottaDB POSIX plugin when installing YottaDB using the `--posix` option of `ydbinstall`, build the POSIX plugin from source:

```sh
# In a temporary directory perform the following commands
git clone https://gitlab.com/YottaDB/Util/YDBPosix.git YDBPosix-master
cd YDBPosix-master
mkdir build && cd build
# Make sure that you have the ydb_dist environment variable defined in your shell before continuing
cmake ..
make -j `grep -c ^processor /proc/cpuinfo` && sudo make install
```

More detailed instructions are on the [YottaDB POSIX plugin page](https://gitlab.com/YottaDB/Util/YDBPosix).

#### (Optional) Install YottaDB encryption plugin

Installing the YottaDB encryption plugin enables TLS support, which is recommended for production installations. You will need to make sure TLS/SSL is enabled for the driver in the client software chosen.

If you do not install the YottaDB encryption plugin when installing YottaDB using the `--encplugin` option of `ydbinstall`, build the encryption plugin from source:

```sh
# In a temporary directory perform the following commands
sudo tar -xf $ydb_dist/plugin/gtmcrypt/source.tar
# Make sure that you have the ydb_dist environment variable defined in your shell before continuing
sudo ydb_dist=$ydb_dist make -j `grep -c ^processor /proc/cpuinfo`
sudo ydb_dist=$ydb_dist make install
```

### Install Octo

Octo is a continuously updated YottaDB plugin that is distributed as source code. A CI (Continuous Integration) pipeline runs a considerable number of unit and system tests before allowing any source code to be merged. This ensures that the master branch is always current with the latest production-ready source code.

The Octo plugin can be installed by using the `--octo` option when installing YottaDB with the `ydbinstall` script. Alternatively, you can build the Octo plugin from source:


1. Install prerequisite packages

   ```sh
   # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
   sudo apt-get install --no-install-recommends build-essential cmake bison flex xxd libreadline-dev libconfig-dev libssl-dev

   # CentOS Linux OR RedHat Linux
   # Note: codeready-builder-for-rhel-8-x86_64-eus-rpms repository is needed in order to get the libconfig-devel package on RHEL 8
   # Note: epel-release has to be installed before cmake3 is installed
   sudo yum install epel-release
   sudo yum install vim-common cmake3 bison flex readline-devel libconfig-devel openssl-devel
   ```

1. (Optional) Prerequisites for Automated Regression Testing

   *NOTE: As we run the automated regression tests on every Octo source code update, install and run BATS only if you are an advanced user who wants to contribute to Octo or run on a Linux distribution on which YottaDB is Supportable but not Supported.*

   - Octo uses BATS for automated integration and regression testing. To use BATS to run tests on Octo, BATS version 1.1+ must be installed:

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
	 sudo apt-get install --no-install-recommends default-jdk expect golang-go

	 # CentOS Linux OR RedHat Linux
	 sudo yum install java-latest-openjdk expect golang
	 ```

	 Additionally, some tests require a JDBC driver. The JDBC driver must be downloaded to the build directory and JDBC_VERSION must be set in the environment. Versions starting with 42.2.6 are tested, but earlier versions may work. For example, 42.2.12 is the latest release at time of writing:

	 ```sh
	 export JDBC_VERSION=42.2.12
	 wget https://jdbc.postgresql.org/download/postgresql-$JDBC_VERSION.jar
	 ```

   - Install cmocka unit testing framework

	 Octo uses cmocka for automated unit testing. To build and run Octo's unit tests, cmocka must be installed:

	 ```sh
	 # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	 sudo apt-get install --no-install-recommends libcmocka-dev

	 # CentOS Linux OR RedHat Linux
	 sudo yum install libcmocka-devel
	 ```

   - Install PostgreSQL client (psql)

	 Octo uses the psql PostgreSQL for some integration/regression tests. To build and run these tests, psql must be installed:

	 ```sh
	 # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	 sudo apt-get install --no-install-recommends postgresql-client

	 # CentOS Linux OR RedHat Linux
	 sudo yum install postgresql
	 ```

   - Install PostgreSQL server

	 Octo uses the PostgreSQL server for some integration/regression tests. To build and run these tests, PostgreSQL must be installed:

	 ```sh
	 # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	 sudo apt-get install --no-install-recommends postgresql

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

   - (CentOS/RHEL7 only) Install Perl

	 On CentOS 7 and RHEL7, Octo test queries sometimes produce output with superfluous escape sequences. These escape sequences are removed by a Perl script, making Perl a dependency for Octo testing on these platforms.

	 To install Perl on CentOS 7 or RHEL7:

	 ```sh
	 # CentOS Linux OR RedHat Linux
	 sudo yum install perl
	 ```

1. Clone the Octo source code repository

   ```sh
   # In a temporary directory perform the following commands
   git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git YDBOcto-master
   cd YDBOcto-master
   ```

1. Compile Octo

   ```sh
   mkdir build ; cd build
   cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. # for CentOS/RedHat use cmake3 instead
   make -j `grep -c ^processor /proc/cpuinfo`
   ```

  You can also use `ninja`, which has more friendly progress output and compiles slightly faster:

  ```sh
  cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. -G Ninja # for CentOS/RedHat use cmake3 instead
  ninja && sudo ninja install
  ```


   Most users: proceed to the *Install Octo* step below. The instructions here are for those wishing to contribute to Octo, or building it on Supportable but not Supported platforms.

   To generate a Debug build instead of a Release build (the default), add `-DCMAKE_BUILD_TYPE=Debug` to the CMake line above.

   To additionally disable the generation of installation rules for `make install`, add `-DDISABLE_INSTALL=ON`. This can be useful when doing testing in a temporary build directory only.

   To build the full test suite rather than a subset of it, the `FULL_TEST_SUITE` option needs to be set to `ON`, e.g. `cmake -D FULL_TEST_SUITE=ON ..`.

   To show the output of failed tests, export the environment variable `CTEST_OUTPUT_ON_FAILURE=TRUE`. Alternatively, you can show output for only a single run by passing the argument to make: `make CTEST_OUTPUT_ON_FAILURE=TRUE test`.

1. Install Octo

   Install Octo:

   ```sh
   sudo -E make install
   ```

   Redefine environment variables to include newly installed files:

   ```sh
   source $ydb_dist/ydb_env_unset
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

Octo uses several global variables for its operation, all of which start with `%ydbocto`. Use [GDE](https://docs.yottadb.net/AdminOpsGuide/gde.html) to map `%ydbocto*` global variables to a separate region. Global variables used by Octo must have [NULL_SUBSCRIPTS=ALWAYS](https://docs.yottadb.net/AdminOpsGuide/gde.html#no-n-ull-subscripts-always-never-existing).

The following example creates an OCTO database region with the recommended settings in the `$ydb_dir/$ydb_rel/g` directory and assumes an existing application global directory at `$ydb_dir/$ydb_rel/g/yottadb.gld`. For more information on setting up a database in YottaDB, refer to the [Administration and Operations Guide](https://docs.yottadb.com/AdminOpsGuide/index.html), and the [YottaDB Acculturation Guide](https://docs.yottadb.com/AcculturationGuide/) for self-paced exercises on YottaDB DevOps.

```sh
$ echo $ydb_dir $ydb_rel
/tmp/test r1.30_x86_64
$ yottadb -run GDE
%GDE-I-LOADGD, Loading Global Directory file
        /tmp/test/r1.30_x86_64/g/yottadb.gld
%GDE-I-VERIFY, Verification OK


GDE> add -segment OCTO -access_method=BG -file_name=$ydb_dir/$ydb_rel/g/octo.dat
GDE> add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000 -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl")
GDE> add -name %ydbocto* -region=OCTO
GDE> verify
%GDE-I-VERIFY, Verification OK


GDE> exit
%GDE-I-VERIFY, Verification OK

%GDE-I-GDUPDATE, Updating Global Directory file
        /tmp/test/r1.30_x86_64/g/yottadb.gld
$ mupip create -region=OCTO
%YDB-I-DBFILECREATED, Database file /tmp/test/r1.30_x86_64/g/octo.dat created
$ mupip set -journal=before,enable,on -region OCTO
%YDB-I-JNLCREATE, Journal file /tmp/test/r1.30_x86_64/g/octo.mjl created for region OCTO with BEFORE_IMAGES
%YDB-I-JNLSTATE, Journaling state for region OCTO is now ON
$
```

The commands in the example above are reproduced below, to facilitate copying and pasting.

```
echo $ydb_dir $ydb_rel
yottadb -run GDE
add -segment OCTO -access_method=BG -file_name=$ydb_dir/$ydb_rel/g/octo.dat
add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000 -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl")
add -name %ydbocto* -region=OCTO
verify
exit
mupip create -region=OCTO
mupip set -journal=before,enable,on -region OCTO
```

#### (Optional) Test with dummy data

You can use the [Northwind](https://docs.yottadb.com/Octo/grammar.html#northwind-ddl-example) sample database to get started. The dummy data set can be found in the `tests/fixtures` subdirectory of the YDBOcto repository created by `git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git YDBOcto-master`.

A dummy data set consists of a `.zwr` file and a `.sql` file. The former contains the actual data to be stored in YottaDB, while the latter contains a schema that maps relational SQL structures (tables and columns) to the NoSQL data contained in YottaDB. Assuming that `/tmp/YDBOcto-master` is the directory from the `git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git YDBOcto-master` command:

```sh
$ydb_dist/mupip load /tmp/YDBOcto-master/build/tests/fixtures/northwind.zwr
$ydb_dist/plugin/bin/octo -f /tmp/YDBOcto-master/build/tests/fixtures/northwind.sql
```

Once loaded, you can use [SELECT](https://docs.yottadb.com/Octo/grammar.html#select) queries to access the data.

## Additional Configuration

Octo currently looks for a configuration file in the following directories:

* $ydb_dist/plugin/octo/octo.conf
* ~/octo.conf
* ./octo.conf

If the same setting exists in more than one configuration file the setting in the later file (according to the list above) will prevail. An example config file can be found in `$ydb_dist/plugin/octo/octo.conf`.

### Routines

Octo requires that `$ydb_dist/plugin/o/_ydbocto.so` and `$ydb_dist/plugin/o/_ydbposix.so` (`$ydb_dist/plugin/o/utf8/_ydbocto.so` and `$ydb_dist/plugin/o/utf8/_ydbposix.so` when using Octo in YottaDB's UTF-8 mode) be included in `$ydb_routines`. This is necessary not only for running the `octo` and `rocto` executables, but also for correctly updating and maintaining the YottaDB triggers that are used to maintain cross references for Octo. Accordingly, these paths should exist in the `$ydb_routines` in your normal environment setup scripts.

*NOTE: The `source $(pkg-config --variable=prefix yottadb)/ydb_env_set` command sets these up automatically. For UTF-8, set the environment variable `ydb_chset` to `UTF-8`, e.g, `export ydb_chset=UTF-8` before sourcing `ydb_env_set`.*

### Global Variables

All Octo global variables are prefixed with `^%ydbocto`. Using normal global mapping procedures for an existing application global directory (where you want to run Octo), map the global variable namespace `^%ydbocto*` to a separate region (and its associated database file) that meets the below requirements (the below example commands assume the separate region is named `OCTO`).

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
# This creates a CA valid for 1-Year and interactively prompts for additional information
openssl req -new -nodes -key CA.key -days 365 -x509 -out CA.crt
```

#### Create server key and certificate request

```sh
# This creates a 2048 bit private key
openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out server.key
# This creates the certificate signing request
openssl req -new -key server.key -out server.csr
```

#### Sign certificate based on request and local CA

```sh
# Asks the CA to sign the certificate with a 1-Year validity time limit
openssl x509 -req -in server.csr -CA CA.crt -CAkey CA.key -CAcreateserial -out server.crt -days 365
# Mask the password for the certificate in a way YottaDB understands
$ydb_dist/plugin/gtmcrypt/maskpass
# This will need to be added to any startup scripts for octo/rocto
export ydb_tls_passwd_OCTOSERVER=[Masked Password from maskpass]
export ydb_crypt_config=/path/to/octo.conf
```

#### Update Octo configuration file

`$ydb_dist/plugin/octo/octo.conf` contains an outline of the minimum configuration options.

For TLS/SSL a configuration file is required, based on the changes below.

1. In the "rocto" section, "ssl_on" must be set to "true" (no quotes needed in the configuration file).
2. A "tls" section must be present and generally conform to the requirements specified for the [TLS plugin itself](https://docs.yottadb.com/AdminOpsGuide/tls.html). Other notes:
      * Octo doesn't use any of the "dh*" settings, so those can be omitted.
      * The "format" specifier can also be omitted, as long as the certs are in PEM format.
      * The "CAfile" and "CApath" fields are mandatory and must point to valid files/locations with a full path.
      * A subsection named "OCTOSERVER" with "key", and "cert" settings specifying the names of the private key and cert files.
3. The ydb_tls_passwd_OCTOSERVER and ydb_crypt_config environment variables must be set correctly.

If you source `$(pkg-config --variable=prefix yottadb)/ydb_env_set` it provides reasonable default values of environment variables. Review the `$ydb_dist/plugin/octo/octo.conf` file to configure your own environment.

## Usage

Before running Octo/Rocto make sure that the required YottaDB variables are set either by creating your own script or run `source $(pkg-config --variable=prefix yottadb)/ydb_env_set`.

To use the command-line SQL interpreter run: `$ydb_dist/plugin/bin/octo`.
To use rocto, the PostgreSQL protocol compatible server, run: `$ydb_dist/plugin/bin/rocto -p XXXX` where `-p XXXX` optionally specifies a TCP port at which rocto is to listen for connections. The default port number is 1337.

To exit either an Octo or Rocto instance running in a terminal, you may use `CTRL+C`. Note that in the case of Rocto, the rocto instance may not exit immediately if there are still active client connections. In that case, rocto will gracefully exit once all client connections are closed.

### Docker container

A Docker image is available on [docker hub](https://hub.docker.com/r/yottadb/octo). This image is built with the following assumptions about the host environment and automatically starts `rocto` when run by Docker using the commands below.

* The `ydb_env_set` script is used to setup the YottaDB environment and creates/expects a specific layout for globals and routines, specifically:
  * a `r1.30_x86_64` directory with the following sub directories:
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

If you would like to use YDB data in an existing local directory structure, then issue the `docker run` command from a directory where the above directory structure is defined. This is needed to mount it as a volume within the Docker container.

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

## Contributing

To contribute or help with further development, [fork the repository](https://docs.gitlab.com/ee/gitlab-basics/fork-project.html), clone your fork to a local copy and begin contributing!

Please also set up the pre-commit script to automatically enforce some coding conventions. Creating a symbolic link to YDBOcto/pre-commit will be enough for the setup. Assuming you are in the top-level directory of your local copy, the following will work:

```sh
ln -s ../../pre-commit .git/hooks
```

Note that this script will require `tcsh` and `clang-format-9` or a later release.

```sh
# Ubuntu 20.04
sudo apt install --no-install-recommends clang-format-9
# Any Debian-like distro; see also https://apt.llvm.org/
bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
# CentOS 8
sudo yum install clang-tools-extra
```

### clang-tidy

The CI pipeline will run the [`clang-tidy`] tool to catch common errors. You can replicate its behavior locally as follows:

```
# Ubuntu 20.04
sudo apt install --no-install-recommends clang-tidy
# Any Debian-like distro
bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
# CentOS 8
sudo yum install clang-tools-extra

mkdir build
cd build
cmake -D CMAKE_EXPORT_COMPILE_COMMANDS=ON ..
clang-tidy ../src/octo_init.c  # replace octo_init.c with the file you want to check
```

`clang-tidy-8` and later are supported.

[`clang-tidy`]: https://clang.llvm.org/extra/clang-tidy/

### YDBCMake

Octo uses the upstream [YDBCMake] repository to build using YottaDB as the M compiler. Any changes to `ydbcmake/` should first be upstreamed to that repository.
Once the changes are upstreamed, you can merge them into Octo using
```
git pull --no-rebase git@gitlab.com:YottaDB/Tools/YDBCMake.git
```

[YDBCMake]: https://gitlab.com/YottaDB/Tools/YDBCMake

*NOTE: Octo<sup>®</sup> is a registered trademark of YottaDB LLC.*
