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
# RJDBC is installed in the image that runs the tests. Install it where it is missing, which is
# the case on a machine that has not followed developer_doc.rst.
if (!requireNamespace("RJDBC", quietly = TRUE)) {
	dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
	install.packages('RJDBC', repos='https://cloud.r-project.org/', lib = Sys.getenv("R_LIBS_USER"), quiet=TRUE)
	.libPaths( c( .libPaths(), Sys.getenv("R_LIBS_USER") ) )
}
library(RJDBC)

# Load driver
args<-commandArgs(TRUE)
drv <- JDBC("org.postgresql.Driver", args[1])

# Connect to database
jdbc_connector <- paste0("jdbc:postgresql://localhost:", args[2], "/helloR")
conn <- dbConnect(drv, jdbc_connector, "ydb", "ydbrocks")

# Load, summarize, create a pie chart into a pdf
customers <- dbGetQuery(conn, "select * from nwcustomers")
# Restrict summary() to numeric columns. R 4.6 changed summary.character()
# from "Length / Class / Mode" to "N.unique / N.blank / Min.nchar / Max.nchar",
# so the character block is not portable across R versions.
summary(customers[sapply(customers, is.numeric)])
country_table <- table(customers$country)
pdf('customers.pdf')
pie(country_table)
dev.off()
