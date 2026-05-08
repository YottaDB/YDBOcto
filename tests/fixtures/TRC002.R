#################################################################
#								#
# Copyright (c) 2024-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# Install and use Package
# Create user install directory (necessary on Debian)
dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
# Install
install.packages('RPostgres', repos='https://cloud.r-project.org/', lib = Sys.getenv("R_LIBS_USER"), quiet=TRUE)
# Load
.libPaths( c( .libPaths(), Sys.getenv("R_LIBS_USER") ) )
library(DBI)

# Connect to database
args<-commandArgs(TRUE)
con <- dbConnect(RPostgres::Postgres(), dbname = 'helloR', host = 'localhost',
		 port = args[1], user = 'ydb', password = 'ydbrocks')

# Load, summarize, create a pie chart into a pdf
query <- dbSendQuery(con, "SELECT * FROM nwcustomers")
customers <- dbFetch(query)
# Restrict summary() to numeric columns. R 4.6 changed summary.character()
# from "Length / Class / Mode" to "N.unique / N.blank / Min.nchar / Max.nchar",
# so the character block is not portable across R versions.
summary(customers[sapply(customers, is.numeric)])
country_table <- table(customers$country)
pdf('customers.pdf')
pie(country_table)
dev.off()
