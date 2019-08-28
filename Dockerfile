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

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update -qq && \
    apt-get install -y -qq \
        build-essential \
        cmake \
        bison \
        flex \
        libcmocka-dev \
        python-pip \
        libreadline-dev \
        git \
        libconfig-dev \
        libssl-dev \
        postgresql-client \
        postgresql \
        xxd \
        wget
RUN pip install \
        sphinxcontrib-fulltoc \
        sphinx \
        sphinx_rtd_theme
ENV PATH=/usr/local/go/bin:$PATH
ENV GOLANG_VERSION=1.11.2
ENV USER=root
RUN wget -O go.tgz -q https://golang.org/dl/go${GOLANG_VERSION}.linux-amd64.tar.gz
RUN tar -C /usr/local -xzf go.tgz
RUN rm go.tgz
RUN go version

ADD . /builds/YDBDBMS/
WORKDIR /builds/YDBDBMS

RUN tools/ci/build.sh
