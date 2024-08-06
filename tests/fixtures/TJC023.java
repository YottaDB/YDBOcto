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

public class TJC023 {
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
				queryStrings[numQueries++] = "select ?::date;"; // 0. Convert int (with type not passed) to date
				queryStrings[numQueries++] = "select ?::date;"; // 1. Convert int to date
				queryStrings[numQueries++] = "select ?::int;"; //  2. Convert varchar to int
				queryStrings[numQueries++] = "select ?::varchar;"; // 3. Convert int to varchar
				queryStrings[numQueries++] = "select ?::bool;"; // 4. Convert int to varchar
				queryStrings[numQueries++] = "select ?::varchar(3);"; // 5. Convert int to varchar(3)
				queryStrings[numQueries++] = "select ?::numeric(10,1);"; // 6. Convert int to numeric(10,1)
				queryStrings[numQueries++] = "select ?::varchar(3);"; // 7. Convert string 1.1f to varchar(3)
				queryStrings[numQueries++] = "select ?::numeric(10,2);"; // 8. Convert string 12.123 to numeric(10,2)
				queryStrings[numQueries++] = "select ?::numeric;"; // 9. Convert string 12.123 to numeric
				queryStrings[numQueries++] = "select ?::numeric(3);"; // 10. Convert string 123.123 to numeric(3)
				queryStrings[numQueries++] = "select ?::numeric(3);"; // 11. Convert int to numeric(3)
				queryStrings[numQueries++] = "select ?::numeric;"; // 12. Convert int to numeric
				for (int query = 0; query < numQueries; query++) {
					preparedStatement = conn.prepareStatement(queryStrings[query]);
					switch(query) {
						case 0:
							preparedStatement.setObject(1 ,String.valueOf(1)); // type not sent
							break;
						case 1:
							preparedStatement.setInt(1,1);
							break;
						case 2:
							preparedStatement.setString(1,"Zero");
							// preparedStatement.setObject(1 ,String.valueOf("Zero")); // type not sent
							break;
						case 3:
							preparedStatement.setInt(1,1);
							break;
						case 4:
							preparedStatement.setObject(1 ,String.valueOf("Zero")); // type not sent
							break;
						case 5:
							preparedStatement.setInt(1,123456);
							break;
						case 6:
							preparedStatement.setInt(1,123456);
							break;
						case 7:
							preparedStatement.setObject(1 ,String.valueOf(1.1f)); // type not sent
							break;
						case 8:
							preparedStatement.setObject(1 ,String.valueOf(12.123f)); // type not sent
							break;
						case 9:
							preparedStatement.setObject(1 ,String.valueOf(12.123f)); // type not sent
							break;
						case 10:
							preparedStatement.setObject(1 ,String.valueOf(123.123f)); // type not sent
							break;
						case 11:
							preparedStatement.setInt(1,123);
							break;
						case 12:
							preparedStatement.setInt(1,123456);
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
