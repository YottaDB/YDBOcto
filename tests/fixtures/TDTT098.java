/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

public class TDTT098 {
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
				String[]		queryStrings = new String[13];
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount, rowLimit=0, numQueries;

				numQueries = 0;
				// Comparison operation resulting in an error when type is not determined for PARAMETER_VALUE
				queryStrings[numQueries++] = "select ? > date'2023-01-01';"; // ERR_TYPE_MISMATCH
				// Addition operation resulting in an error when type is not determined for PARAMETER_VALUE
				queryStrings[numQueries++] = "select ? + date'2023-01-01';"; // ERR_TYPE_NOT_COMPATIBLE
				// type is not known for the following two columns
				queryStrings[numQueries++] = "select ?,?;";
				// type of the date/time value is known for the following query because of LocalDate and LocalDateTime usage
				queryStrings[numQueries++] = "select ?,?;";
				for (int query = 0; query < numQueries; query++) {
					preparedStatement = conn.prepareStatement(queryStrings[query]);
					switch(query) {
						case 0:
							preparedStatement.setTimestamp(1, java.sql.Timestamp.valueOf("2023-12-08 13:30:00")); // type not sent
							break;
						case 1:
							preparedStatement.setTimestamp(1, java.sql.Timestamp.valueOf("2023-12-08 13:30:00")); // type not sent
							break;
						case 2:
							preparedStatement.setObject(1 ,java.sql.Date.valueOf("2016-06-30")); // type not sent
							preparedStatement.setTimestamp(1, java.sql.Timestamp.valueOf("2023-12-08 13:30:00")); // type not sent
							break;
						case 3:
							preparedStatement.setObject(1 ,java.time.LocalDate.parse( "2016-06-30" )); // type is sent
							preparedStatement.setObject(2 ,java.time.LocalDateTime.parse( "2016-06-30T01:01:01" )); // type is sent
							break;
						default:
							break;
					}
					System.out.printf("-------------------------------------------------------------------------%n");
					System.out.printf(" --> Running query (row limit: %d): ", rowLimit);
					System.out.printf(queryStrings[query]);
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
				}
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
