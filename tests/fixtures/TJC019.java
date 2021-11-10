/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
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

public class TJC019 {
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
				String		queryString, hyphenString;

				System.out.printf("# Construct first query to run for a total of 60 seconds");
				System.out.printf(" (10 second for each of the 6 rows in the names table)%n");
				System.out.printf("# Set max query timeout to 1 second so first query will get canceled.%n");
				System.out.printf("# Then run second query which is same as first query but with a timeout of 0");
				System.out.printf(" seconds so that does not need to be canceled.%n");
				System.out.printf("# And verify it does not get canceled and works fine after a canceled query.%n");
				queryString = "SELECT HANGTIME(firstname,?) FROM names";
				hyphenString = "-------------------------------------------------------------------------";
				for (int queryNum = 0; queryNum < 2; queryNum++) {
					int			parameter;
					String			parameterString;
					PreparedStatement	preparedStatement;

					/* Set timeout parameter in query to 10 seconds for first and 0 seconds for second query */
					parameter = (0 == queryNum) ? 10 : 0;
					System.out.printf("%s%n", hyphenString);
					parameterString = Integer.toString(parameter);
					System.out.printf(" --> Running query : %s%n", queryString.replace("?",parameterString));
					System.out.printf("%s%n", hyphenString);
					preparedStatement = conn.prepareStatement(queryString);
					preparedStatement.setInt(1,parameter);
					/* Set query timeout only for first query as we want second query to run without
					 * being accidentally canceled in case it runs slow in rare cases.
					 */
					if (0 == queryNum) {
						preparedStatement.setQueryTimeout(1);
					}
					try {
						int			columnCount;
						ResultSetMetaData	resultSetMetaData ;
						ResultSet		resultSet;

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
