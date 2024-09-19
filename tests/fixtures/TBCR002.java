/****************************************************************
 *								*
 * Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

// See https://docs.oracle.com/javase/tutorial/jdbc/basics/transactions.html
//
// ROLLBACK
// ROLLBACK without a start
// ROLLBACK + turn off transactions + readwrite + readonly

import java.sql.*;
import java.util.Properties;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;

public class TBCR002 {
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
				String[]		queryStrings = new String[4];
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount, rowLimit=0, numQueries;
				String selectStatement = "select count(*) from names;";
				String insertStatement = "INSERT INTO names values (?, ?, ?);"; // 6, 'foo', 'boo'
				String updateStatement = "UPDATE names SET lastname=? WHERE id=?;"; // 'foo', 1
				String deleteStatement = "DELETE FROM names WHERE id=?;"; // 1


				numQueries = 0;
				// Comparison operation resulting in an error when type is not determined for PARAMETER_VALUE
				queryStrings[numQueries++] = selectStatement;
				queryStrings[numQueries++] = insertStatement;
				queryStrings[numQueries++] = updateStatement;
				queryStrings[numQueries++] = deleteStatement;
				conn.setAutoCommit(false);
				for (int query = 0; query < numQueries; query++) {
					preparedStatement = conn.prepareStatement(queryStrings[query]);
					switch(query) {
						case 0:
							break;
						case 1:
							preparedStatement.setInt(1,6);
							preparedStatement.setString(2,"foo");
							preparedStatement.setString(3,"boo");
							break;
						case 2:
							preparedStatement.setString(1,"foo");
							preparedStatement.setInt(2,1);
							break;
						case 3:
							preparedStatement.setInt(1,1);
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
				conn.commit();
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
