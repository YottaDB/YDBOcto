#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

FROM yottadb/yottadb-base:latest-master

RUN export DEBIAN_FRONTEND=noninteractive
RUN apt-get install -y -qq \
        git \
        libreadline-dev \
        libconfig-dev

ADD ./build/Octo-*.tar.gz /tmp
ADD ./tools/entrypoint.sh /
RUN cd /tmp/Octo-*-Linux && . /opt/yottadb/current/ydb_env_set && yes | ./install.sh

ENV ydb_ci /opt/yottadb/current/plugin/ydbocto.ci
ENTRYPOINT "/entrypoint.sh"
