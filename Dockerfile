#################################################################
#								#
# Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

# Build Octo in octo-builder
FROM yottadb/yottadb-base as octo-builder

ADD COPYING /tmp/octo/COPYING
ADD README.md /tmp/octo/README.md
ADD CMakeLists.txt /tmp/octo/CMakeLists.txt
ADD cmake /tmp/octo/cmake
ADD src /tmp/octo/src
ADD tests /tmp/octo/tests
ADD tools /tmp/octo/tools
ADD .git /tmp/octo/.git

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
        apt-get install -y -qq --no-install-recommends \
        git \
        cmake \
        pkg-config \
        wget \
        ca-certificates \
        gcc \
        curl \
        make \
        flex \
        bison \
        libreadline-dev \
        libconfig-dev \
        libssl-dev \
        libicu-dev

# Install AIM and Posix Plugins
RUN cd /tmp/ && \
    wget https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh && \
    chmod +x ./ydbinstall.sh && \
    ./ydbinstall.sh --plugins-only --posix --aim && \
    rm ./ydbinstall.sh

# Download, build, and install Octo
RUN mkdir /tmp/octo/build && cd /tmp/octo/build && \
    cmake .. && make -j `getconf _NPROCESSORS_ONLN` install

FROM yottadb/yottadb-base as octo-release
# Copy to runtime
# $ydb_dist/plugin/ydbposix.xc
# $ydb_dist/plugin/libydbposix.so
# $ydb_dist/plugin/octo/ydbocto.ci
# $ydb_dist/plugin/octo/octo-seed.{zwr,sql}
# $ydb_dist/plugin/octo/octo.conf
# $ydb_dist/plugin/octo/bin/{,r}octo
# $ydb_dist/plugin/bin/{,r}octo (symlinks)
# $ydb_dist/plugin/o/_ydbposix.so
# $ydb_dist/plugin/o/_ydbocto.so
# $ydb_dist/plugin/o/utf8/_ydbocto.so
# $ydb_dist/plugin/o/utf8/_ydbposix.so
# $ydb_dist/plugin/libydbposix.so
# $ydb_dist/plugin/r/_ydbposix.m
# $ydb_dist/plugin/r/_ydbposixtest.m
# $ydb_dist/plugin/o/_ydbaim.so
# $ydb_dist/plugin/o/utf8/_ydbaim.so
COPY --from=octo-builder /opt/yottadb/current/plugin /opt/yottadb/current/plugin
# Copy northwind files too
COPY --from=octo-builder /tmp/octo/tests/fixtures/northwind.* /opt/yottadb/current/plugin/octo/

# Install required libraries for Octo/Rocto (note: NOT the dev versions)
RUN apt-get update && apt-get install -y libreadline8 libconfig9 libicu70 libssl3
# Change octo.conf to accept connections from everywhere
RUN cp /opt/yottadb/current/plugin/octo/octo.conf /data/octo.conf && \
    sed -i 's/address = "127.0.0.1"/address = "0.0.0.0"/' /data/octo.conf
# Load Northwind and create the Rocto user
RUN . /opt/yottadb/current/ydb_env_set && \
    octo -f $ydb_dist/plugin/octo/northwind.sql && \
    mupip load $ydb_dist/plugin/octo/northwind.zwr && \
    printf "ydbrocks\nydbrocks" | "$ydb_dist/yottadb" -r %ydboctoAdmin add user ydb
# Make .bashrc contain ydb_env_set by default for interactive users
RUN echo "export ydb_dir=/data && . /opt/yottadb/current/ydb_env_set" >> ~root/.bashrc

# Add Entry Point and expose 1337 publiclly
ADD ./tools/entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
EXPOSE 1337
