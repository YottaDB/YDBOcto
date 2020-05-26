#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
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
# `apt-get update` is included to account for the case when the upstream Ubuntu container
# goes out of sync with the upstream Ubuntu repositories. When this happens, `apt-get install`
# fails, causing `docker build` to fail in turn.
RUN apt-get update && \
	apt-get install -y -qq \
        git \
        libreadline-dev \
        libconfig-dev \
		cmake

ADD ./build /tmp/build
ADD ./tools/entrypoint.sh /
RUN cd /tmp/build/ && . /opt/yottadb/current/ydb_env_set && ./install.sh
RUN cd /tmp && rm -r build

ENTRYPOINT "/entrypoint.sh"
