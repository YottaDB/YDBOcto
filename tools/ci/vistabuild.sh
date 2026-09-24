#!/bin/sh -v
#################################################################
#								#
# Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	#
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
if [ ! -d docker-vista ]; then
	git clone https://github.com/WorldVistA/docker-vista.git
fi
cd docker-vista
git pull
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
#
# The docker-vista Dockerfile installs its prerequisites with yum on rockylinux:9, which picks a mirror for each
# repository independently. A mirror whose BaseOS lags the AppStream mirror leaves a -devel package from AppStream
# without the exact runtime package version it requires from BaseOS, and the build fails. Another attempt draws other
# mirrors, so retry the build, waiting longer after each attempt.
attempt=1
while ! docker build --pull --provenance=false --build-arg flags="-o -f -b -s -q -d -n -a https://github.com/WorldVistA/VistA-VEHU-M/archive/master.zip" --build-arg instance="vehu" -t yottadb/octo-vehu:latest-master .; do
	if [ "$attempt" -ge 3 ]; then
		echo "docker build failed after 3 attempts"
		exit 1
	fi
	echo "docker build failed on attempt ${attempt} of 3; retrying in $((attempt * 30)) seconds"
	sleep $((attempt * 30))
	attempt=$((attempt + 1))
done
