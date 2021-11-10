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
import java.util.Random;

public class run_query {
	public static void main( String args[] ) {
		String database_name = new String("");
		String psql_default_port = new String("5432");
		Properties props = new Properties();
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");

		boolean use_extended = false;
		if (3 == args.length) {
			// Use the query protocol specified by caller, if any
			if (args[2].equals("useextended")) {
				use_extended = true;
			} else if (args[2].equals("usesimple")) {
				use_extended = false;
			} else {
				database_name = args[2];
			}
		} else {
			// Randomize use of extended query
			Random rand = new Random();
			use_extended = rand.nextBoolean();
		}
		if (4 == args.length) {
			database_name = args[3];
		}
		if (use_extended) {
			props.setProperty("preferQueryMode","extended");
		} else {
			props.setProperty("preferQueryMode","simple");
		}

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/" + database_name;
		int exitStatus = 0;
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				String queryString;
				PreparedStatement preparedStatement;
				ResultSet resultSet;
				ResultSetMetaData resultSetMetaData;
				int columnCount;

				if (2 <= args.length) {
					preparedStatement = conn.prepareStatement(args[1]);
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
				} else {
					System.out.println("Please pass a query to run.");
					exitStatus = 1;
				}
			} else {
				System.out.println("Failed to make connection!");
				exitStatus = 1;
			}
		} catch (SQLException e) {
			System.err.format("SQL State: %s\n%s", e.getSQLState(), e.getMessage());
			/* Note: Do not set "exitStatus = 1;" like is done in other *.java files
			 * as some caller tests (e.g. TC032) do expect errors in some cases.
			 */
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
