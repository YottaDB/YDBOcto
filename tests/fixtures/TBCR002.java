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
				String[]		queryStrings = new String[5];
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount, numQueries;
				String selectStatement = "select * from names;";

				String insertStatement = "INSERT INTO names values (?, ?, ?);"; // 6, 'foo', 'boo'
				String updateStatement = "UPDATE names SET lastname=? WHERE id=?;"; // 'foo', 1
				String deleteStatement = "DELETE FROM names WHERE id=?;"; // 1


				numQueries = 0;
				// Comparison operation resulting in an error when type is not determined for PARAMETER_VALUE
				queryStrings[numQueries++] = selectStatement; //0
				queryStrings[numQueries++] = insertStatement; //1
				queryStrings[numQueries++] = updateStatement; //2
				queryStrings[numQueries++] = deleteStatement; //3
                                queryStrings[numQueries++] = selectStatement; //4

                                // Run the queries in an outer loop: 0 commits, 1 rollsback, 2 turns off transactions
				//                                 // --> avoids having to write the same code over and over again.
                                for (int commrollback = 0; commrollback < 3; commrollback++) {
                                        if (commrollback == 0 || commrollback == 1) {
                                                // Don't turn it on twice (for cleaner output)
                                                if (conn.getAutoCommit() != false) {
                                                        System.out.printf("--- TURNING ON TRANSACTIONS (expect to see 25006 errors, data not modified) ---%n");
                                                        // This turns on transactions (sends BEGIN before queries start)
                                                        conn.setAutoCommit(false);
                                                }
                                        }
                                        else {
						assert(commrollback == 2);
                                                System.out.printf("--- TURNING OFF TRANSACTIONS (no errors expected, data will be modified) ---%n");
                                                conn.setAutoCommit(true);
                                        }
                                        if (0 == commrollback) {
                                                System.out.printf("--- PLAN TO COMMIT IN THE END ---%n");
                                        }
                                        if (1 == commrollback) {
                                                System.out.printf("--- PLAN TO ROLLBACK IN THE END ---%n");
                                        }

					for (int query = 0; query < numQueries; query++) {
						preparedStatement = conn.prepareStatement(queryStrings[query]);
						switch(query) {
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
                                                if (4 == query) {
                                                        // Commit or rollback before the final select query that shows the result
                                                        if (0 == commrollback) {
                                                                System.out.printf("--- COMMITTING ---%n");
                                                                conn.commit();
                                                        }
                                                        if (1 == commrollback) {
                                                                System.out.printf("--- ROLLING BACK ---%n");
                                                                conn.rollback();
                                                        }
							// Turn off transactions for the last query; otherwise the next loop will have transactions turned on
							// without a corresponding commit/rollback
							conn.setAutoCommit(true);
                                                }

						System.out.printf("-------------------------------------------------------------------------%n");
						System.out.printf(" --> Running query: ");
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
					System.out.printf("%n");
				}

				System.out.printf("%n--- TURN ON TRANSACTIONS THEN RUN TWO ROLLBACKS (no error expected) ---%n");
				conn.setAutoCommit(false);
				conn.prepareStatement(queryStrings[0]).executeQuery();
				conn.rollback();
				conn.rollback();
				System.out.printf("%n");

				try {
					System.out.printf("%n--- TURN OFF TRANSACTIONS THEN RUN ROLLBACK (error expected) ---%n");
					conn.setAutoCommit(true);
					conn.prepareStatement(queryStrings[0]).executeQuery();
					conn.rollback();
				} catch (SQLException e) {
					System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
					/* Note: Do not set "exitStatus = 1;" like is done in other *.java files
					 * in this repository as we do expect some errors in this test.
					 */
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
			System.err.format("exitStatus: %d\n", exitStatus);
			System.exit(exitStatus);
		}
	}
}
