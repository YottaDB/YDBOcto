#!/bin/bash
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

# Make sure ydb_dist/gtmdist is set
distset=0
if [ ! -z "$ydb_dist" ]; then
        distset=1
fi

if [ ! -z "$gtmdist" ]; then
        distset=$(($distset + 2))
fi

if [ 2 = $distset ]; then
        # This should always be safe to do as YottaDB will prefer ydb_dist to gtmdist
        ydb_dist=$gtmdist
fi

if [ 3 = $distset ]; then
        # make sure both $ydb_dist and $gtmdist are the same
        if [[ $ydb_dist != $gtmdist ]]; then
                echo "The environment variables \$ydb_dist and \$gtmdist do not match, unable to continue. Please check the values of the variables an unset the incorrect one."
                exit 1
        fi
fi

if [ 0 = $distset ]; then
        echo "The environment variable \$ydb_dist or \$gtmdist is not set, unable to continue. Please set one of them and re-run this script to continue."
        exit 1
fi

echo "This will install the Octo by YottaDB plugin to $ydb_dist/plugin"
read -p "Is this OK? (Y/N)" -n 1 install
echo
if [[ $install =~ ^[Yy]$ ]]; then
        install -d $ydb_dist/plugin
        install -D -m 644 ydbocto.ci $ydb_dist/plugin
        install -d $ydb_dist/plugin/bin
        install -D bin/* $ydb_dist/plugin/bin
        install -d $ydb_dist/plugin/r
        install -D -m 644 r/* $ydb_dist/plugin/r
        install -d $ydb_dist/plugin/o
        install -D -m 644 o/* $ydb_dist/plugin/o
        install -d $ydb_dist/plugin/o/utf8
        install -D -m 644 o/* $ydb_dist/plugin/o/utf8
        install -d $ydb_dist/plugin/etc
        if [ -f "$ydb_dist/plugin/etc/octo.conf" ]; then
                install -D -m 644 $(ls -d etc/* | grep -v octo.conf) $ydb_dist/plugin/etc
        else
                install -D -m 644 etc/* $ydb_dist/plugin/etc
        fi
fi

# Import Postgres metatadata to current global directory
read -p "Do you want to install the metadata required for octo? (Y/N)" -n 1 metadata
echo
if [[ $metadata =~ ^[Yy]$ ]]; then
        $ydb_dist/mupip load $ydb_dist/plugin/etc/postgres-seed.zwr
        $ydb_dist/plugin/bin/octo -f $ydb_dist/plugin/etc/postgres-seed.sql
fi
