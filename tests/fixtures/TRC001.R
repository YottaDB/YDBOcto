#################################################################
#								#
# Copyright (c) 2024-2025 YottaDB LLC and/or its subsidiaries.	#
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
install.packages('RJDBC', repos='https://cloud.r-project.org/', lib = Sys.getenv("R_LIBS_USER"), quiet=TRUE)
# Load
.libPaths( c( .libPaths(), Sys.getenv("R_LIBS_USER") ) )
library(RJDBC)

# Load driver
args<-commandArgs(TRUE)
drv <- JDBC("org.postgresql.Driver", args[1])

# Connect to database
jdbc_connector <- paste0("jdbc:postgresql://localhost:", args[2], "/helloR")
conn <- dbConnect(drv, jdbc_connector, "ydb", "ydbrocks")

# Load, summarize, create a pie chart into a pdf
customers <- dbGetQuery(conn, "select * from nwcustomers")
summary(customers)
country_table <- table(customers$country)
pdf('customers.pdf')
pie(country_table)
dev.off()
