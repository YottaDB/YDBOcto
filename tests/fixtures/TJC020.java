/****************************************************************
 *								*
 * Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	*
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

public class TJC020 {
	public static void main( String args[] ) {
		Properties props = new Properties();
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");
		props.setProperty("preferQueryMode","extended");

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		int exitStatus = 0;
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				String			queryString;
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount;

				preparedStatement = conn.prepareStatement("CREATE TABLE extractnames (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER, fullname VARCHAR EXTRACT CONCAT(?, lastname)) GLOBAL \"^names(keys(\"\"ID\"\"))\";");
				preparedStatement.setString(1,"Zero");
				System.out.printf("-------------------------------------------------------------------------%n");
				System.out.printf(" --> Running query : ");
				System.out.printf("CREATE TABLE extractnames (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30),  age INTEGER, fullname VARCHAR EXTRACT CONCAT('Joey', lastname)) GLOBAL \"^names(keys(\"\"ID\"\"))\";");
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
			System.err.format("SQL State: %s\n%s", e.getSQLState(), e.getMessage());
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
