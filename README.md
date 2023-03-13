# YDB Octo

[![pipeline status](https://gitlab.com/YottaDB/DBMS/YDBOcto/badges/master/pipeline.svg)](https://gitlab.com/YottaDB/DBMS/YDBOcto/commits/master)

Octo<sup>®</sup> is a SQL database engine whose tables are stored in YottaDB global variables (i.e., YottaDB hierarchical key-value nodes). Octo is installed as a YottaDB plugin.

Homepage: https://gitlab.com/YottaDB/DBMS/YDBOcto

Documentation: https://docs.yottadb.com/Octo/

Octo requires [YottaDB](https://gitlab.com/YottaDB/DB/YDB) r1.34 or greater. Installing and configuring YottaDB is described on its [documentation page](https://docs.yottadb.com/AdminOpsGuide/installydb.html).

*NOTE: Octo is a YottaDB application, not an application that runs on the upstream GT.M for which YottaDB is a drop-in upward-compatible replacement.*

## Quickstart

Install prerequisite packages:

```sh
# Ubuntu
sudo apt update && sudo apt install -y --no-install-recommends build-essential cmake bison flex libreadline-dev libssl-dev wget ca-certificates file libelf-dev curl git pkg-config libicu-dev libconfig-dev

# RHEL 8/Rocky Linux
yum --enablerepo=powertools install -y gcc make cmake bison flex readline-devel git libconfig-devel pkg-config libicu-devel wget findutils procps file openssl-devel postgresql
```

Install YottaDB, Octo, and the required POSIX plugin all together:

```sh
mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
cd /tmp/tmp ; chmod +x ydbinstall.sh
sudo ./ydbinstall.sh --utf8 --verbose --octo
```

Note that the `--octo` option implicitly installs any dependent plugins, such as the POSIX plugin.

`./ydbinstall.sh --help` gives a full list of its numerous options.

The Octo [Developer Documentation](https://docs.yottadb.com/Octo/developer_doc.html) provides instructions on building and installing Octo from source.

### Test with dummy data using Octo and ROcto

Set the environment variables:

```sh
source $(pkg-config --variable=prefix yottadb)/ydb_env_set
```

Download the dummy data set and load it:

```sh
wget https://gitlab.com/YottaDB/DBMS/YDBOcto/-/raw/master/tests/fixtures/northwind.zwr
wget https://gitlab.com/YottaDB/DBMS/YDBOcto/-/raw/master/tests/fixtures/northwind.sql
mupip load northwind.zwr
octo -f northwind.sql
```

Run a sample query in Octo:

```sh
octo
SELECT * FROM Suppliers;
```

Set up PostgreSQL and create a user:

```sh
sudo apt install postgresql-client
yottadb -r %ydboctoAdmin add user test # Enter password when prompted
```

Run a sample query in ROcto:

```sh
rocto &
psql -h localhost -p 1337 -U test # Enter password when prompted
SELECT * FROM Suppliers;
```

Kill the ROcto process:

```sh
kill %1
```

## Using the Docker Images
We provide two docker images, one containing a copy of a Northwind database, and another containing a copy of VistA.

### Northwind
To run the docker image containing a copy of the Northwind database, with the Rocto Daemon running on port 1337:

```sh
docker run -d --rm -p 1337:1337 --name=rocto download.yottadb.com/yottadb/octo
```

You can pass a directory on your computer to keep the data in, so you won't lose it when you stop the docker container:

```sh
docker run -d -v $HOME/mydata:/data --rm -p 1337:1337 --name=rocto download.yottadb.com/yottadb/octo
```

To access the data, you can use the Postgres client (or any other compatible client) like this (Username `ydb` password `ydbrocks`):

```sh
PGPASSWORD=ydbrocks psql -U ydb -h localhost -p 1337
```

To look at the Rocto log, you can type this:
```sh
docker logs rocto
```

To go to the shell, and access YottaDB or the Octo command line:

```sh
docker exec -it rocto su -
yottadb -dir # YottaDB
octo # Octo
exit
```

To stop the image, simply kill it, as it will autodelete after stopping. Your data will be kept if you mapped a directory on your computer:

```sh
docker kill rocto
```

### VistA
The VistA image's instructions can be found on DockerHub here: https://hub.docker.com/r/yottadb/octo-vehu

## Building the documentation

Octo's documentation is maintained in reStructuredText (RST) format. Assuming the current working directory is the repository root, the documentation can be built as an html site with the following commands:

```sh
cd doc/
make html
```

When the build is complete, the documentation will be accessible as html files in `doc/_build/html`.

To clean up after a documentation build, use:

```sh
cd doc/
make clean
```

*NOTE: Octo<sup>®</sup> is a registered trademark of YottaDB LLC.*
