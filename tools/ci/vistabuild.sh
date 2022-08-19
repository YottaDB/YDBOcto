#!/bin/sh -v
#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
set -e

cd ..
git clone https://github.com/WorldVistA/docker-vista.git
cd docker-vista
docker ps -a

# command line args:
#
# -o install YottaDB from source
# -f install Kernel-GTM fixes
# -b Skip bootstrapping (aka setup for docker container)
# -s Skip testing
# -q Install Octo
# -d Create development directories
# -a Alternate VistA version (VEHU)
# -n Install YottaDB GUI
docker build --pull --build-arg flags="-o -f -b -s -q -d -n -a https://github.com/WorldVistA/VistA-VEHU-M/archive/master.zip" --build-arg instance="vehu" -t yottadb/octo-vehu:latest-master .
