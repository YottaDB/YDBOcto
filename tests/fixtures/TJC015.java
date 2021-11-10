/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

import java.sql.*;
import java.util.Properties;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

public class TJC015 {
	public static void main( String args[] ) {
		Properties props = new Properties();
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");

		System.out.printf("protocol: %s%n", args[2]);
		if (args[2].equals("useextended"))
			props.setProperty("preferQueryMode","extended");
		else
			props.setProperty("preferQueryMode","simple");

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		int exitStatus = 0;
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				String			queryString;
				StringBuilder		builder = new StringBuilder();
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount;
				/* Use the maximum number of prepared statement parameters (INT16_MAX), minus 1 for the single
				 * regular column identifier (id) used in the query below.
				 */
				int			numFields = 32765;
				FileWriter writer = new FileWriter("TJC015.sql");
				PrintWriter printWriter = new PrintWriter(writer);

				builder.append("SELECT id");
				printWriter.printf("SELECT id");
				for (int curField = 0; curField < numFields; curField++) {
					builder.append(", ?");
					printWriter.printf(", '%s'", "1234567890123456789012345678901234567890");
				}
				printWriter.printf(", 123456789 FROM names WHERE");
				builder.append(" FROM names WHERE");
				/*
				for (int curField = 0; curField < 10000; curField++) {
					builder.append(" firstname = firstname AND");
					printWriter.printf(" firstname = firstname AND");
				}
				*/
				builder.append(" firstname = firstname;");
				printWriter.printf(" firstname = firstname;");
				printWriter.close();
				preparedStatement = conn.prepareStatement(builder.toString());
				for (int curField = 0; curField < numFields; curField++) {
					/* Populate parameters, incrementing by one for compatibility with 1-indexing expected by
					 * the setInt method.
					 */
					// preparedStatement.setInt(curField+1, curField+1000000001);
					preparedStatement.setString(curField+1, "1234567890123456789012345678901234567890");
				}
				System.out.printf("-------------------------------------------------------------------------%n");
				System.out.printf(" --> Running query : ");
				System.out.printf("SELECT id, 0, 1, 2, ..., %d FROM names WHERE firstname = firstname ...;", numFields);
				System.out.printf(" <--%n");
				System.out.printf("-------------------------------------------------------------------------%n");
				try {
					resultSet = preparedStatement.executeQuery();
					resultSetMetaData = resultSet.getMetaData();
					columnCount = resultSetMetaData.getColumnCount();
					while (resultSet.next()) {
						for (int i = 1; i <= columnCount; i++) {
							System.out.printf("%s", resultSet.getString(i));
							if (i != columnCount) {
								System.out.printf("|");
							}
						}
						System.out.printf("%n");
					}
				} catch (SQLException e) {
					System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
					/* Note: Do not set "exitStatus = 1;" like is done in other *.java files
					 * in this repository as we do expect some errors in this test.
					 */
				}
				preparedStatement.close();
			} else {
				System.out.println("Failed to make connection!");
				exitStatus = 1;
			}
		} catch (SQLException e) {
			System.err.format("SQL State2: %s\n%s", e.getSQLState(), e.getMessage());
			exitStatus = 1;
		} catch (Exception e) {
			e.printStackTrace();
			exitStatus = 1;
		}
		if (0 != exitStatus) {
			/* Some error occurred. Caller (i.e. bats) relies on a non-zero exit status so it can signal
			 * a test failure. So do just that using "exit".
			 */
			System.exit(exitStatus);
		}
	}
}
