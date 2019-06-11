#!/bin/sh
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

# We are going to use a forked version of docker-vista until the changes are accepted upstream we need the following features:
#
# * Install YottaDB master from source
# * Install Octo from source or from a mounted directory

cd ..
git clone https://github.com/ChristopherEdwards/docker-vista.git --branch octo-support
cd docker-vista
docker ps -a

# command line args:
#
# -y install YottaDB
# -f install Kernel-GTM fixes
# -b Skip bootstrapping (aka setup for docker container)
# -s Skip testing
# -q Install Octo
# -d Create development directories
# -a Alternate VistA version (VEHU)
docker build --pull -t yottadb/octo-vehu:latest-master --build-arg flags="-y -f -b -s -q -d -a https://github.com/OSEHRA-Sandbox/VistA-VEHU-M/archive/master.zip" --build-arg instance="vehu" -t yottadb/octo-vehu:latest-master .
docker save yottadb/octo-vehu:latest-master > $CI_PROJECT_DIR/docker-images/octo-vehu.tar
