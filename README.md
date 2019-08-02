# YDB Octo

[![pipeline status](https://gitlab.com/YottaDB/DBMS/YDBOcto/badges/master/pipeline.svg)](https://gitlab.com/YottaDB/DBMS/YDBOcto/commits/master)

The YottaDB Octo Database Management System is a SQL access layer built on top of the not-only-SQL database YottaDB.
It aims to provide SQL 92 compliance and exceptional performance.

Homepage https://gitlab.com/YottaDB/DBMS/YDBOcto

## Setup

YottaDB r1.26 or greater is required for successful installation of Octo. Installing and configuring YottaDB is described on its own [documentation page](https://docs.yottadb.com/AdminOpsGuide/installydb.html).

*NOTE: the environment variable `$ydb_dist` is required to be defined - `$gtm_dist` is not a valid subsitute*

## Quickstart - Install from source

* Install YottaDB POSIX plugin

More detailed instructions are on the [YottaDB POSIX plugin page](https://gitlab.com/YottaDB/Util/YDBposix/blob/master/README.md).

```sh
# In a temporary directory perform the following commands
curl -fSsLO https://gitlab.com/YottaDB/Util/YDBposix/-/archive/master/YDBposix-master.tar.gz
tar xzf YDBposix-master.tar.gz
cd YDBposix-master
mkdir build && cd build
# Make sure that you have YottaDB environment variables in your shell before continuing
cmake ..
make -j `grep -c ^processor /proc/cpuinfo` && sudo make install
```

* (Optional) Install YottaDB encryption plugin

Installing the YottaDB encryption plugin enables TLS support (Recommended for production installations). You will need to make sure TLS/SSL is enabled for the driver in the client software chosen.

```sh
# In a temporary directory perform the following commands
sudo tar -xf $ydb_dist/plugin/gtmcrypt/source.tar
# Make sure that you have YottaDB environment variables in your shell before continuing
sudo ydb_dist=$ydb_dist make -j `grep -c ^processor /proc/cpuinfo`
sudo ydb_dist=$ydb_dist make install
```

* Install prerequisite packages

```sh
# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
sudo apt-get install build-essential cmake bison flex xxd libreadline-dev libconfig-dev libssl-dev

# CentOS Linux OR RedHat Linux
# Note: epel-release has to be installed before cmake3 is installed
sudo yum install epel-release
sudo yum install vim-common cmake3 bison flex readline-devel libconfig-devel openssl-devel
```

* Download Octo Source Code

```sh
# In a temporary directory perform the following commands
curl -fSsLO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/archive/master/YDBOcto-master.tar.gz
tar xzf YDBOcto-master.tar.gz
cd YDBOcto-master
```

* Compile Octo

```sh
mkdir build
cd build
# For VistA the String Buffer Length needs to be larger (described below) add `-DSTRING_BUFFER_LENGTH=300000` to the cmake command below
cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. # for CentOS/RedHat use cmake3 instead
make -j `grep -c ^processor /proc/cpuinfo`
```

* Install Octo

```sh
make install
```

### Optional CMake parameters

Octo uses some cmake parameters to control generation of fixed-size buffer allocations. These are:

* STRING_BUFFER_LENGTH -- the maximum length of a string within the system; this supercedes any VARCHAR definitions.
* MAX_ROUTINE_LENGTH -- the maximum length of a generated routine. The default is 10MB.
* MEMORY_CHUNK_SIZE -- size of memory chunks to allocate; default is 32MB.
* MEMORY_CHUNK_PROTECT -- if non-zero, memory following chunks is protected to detect buffer overflows. If 2, data is placed closer to the protected region to increase the chances of detecting an error.

Example usage of above parameters:

```sh
cmake -DSTRING_BUFFER_LENGTH=600000 -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin ..
```

## Configuration

Octo currently looks for a configuration file in the following directories:

* $ydb_dist/plugin/etc/octo.conf
* ~/octo.conf
* ./octo.conf

If the same setting exists in more than one configuration file the setting in the later file (according to the list above) will prevail. An example config file can be found in `$ydb_dist/plugin/etc/octo.conf`.

### Environment variables

The following environment variables must be set:

* ydb_dist
* ydb_gbldir
* ydb_routines
* ydb_ci
* ydb_xc_ydbposix

The environment variables `ydb_dist`, `ydb_gbldir`, and `ydb_routines` can initially be set by souring `ydb_env_set` in your YottaDB installation directory. Additional modifications to ydb_routines may be needed due to configuration in `octo.conf` described later in this manual.

Example setting of the environment variables (assuming default paths):

```sh
source /usr/local/lib/yottadb/r1.26/ydb_env_set
export ydb_routines=". $ydb_routines"
export ydb_ci=$ydb_dist/plugin/ydbocto.ci
export ydb_xc_ydbposix=$ydb_dist/plugin/ydbposix.xc
```

### Routines

Octo requires that `$ydb_dist/plugin/o/_ydbocto.so`, `$ydb_dist/plugin/o/_ydbposix.so`, and the path configured for `routine_cache` setting in `octo.conf` be part of `$ydb_routines` - both for running the `octo` and `rocto` excutables and added to your normal environment setup scripts as YottaDB triggers are used to maintain cross references for Octo.

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

The $ydb_dist/plugin/etc/octo.conf contains an outline of the minimum configuration options needed to enable TLS/SSL. The key items are:

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
