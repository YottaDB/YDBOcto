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

public class TDTT062 {
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
				String			populatedString;
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount;

				queryString = "select ?::date,?::date";
				preparedStatement = conn.prepareStatement(queryString);
				preparedStatement.setTimestamp(1, java.sql.Timestamp.valueOf("2023-12-08 13:30:00")); // type not sent
				preparedStatement.setObject(2 ,java.time.LocalDate.parse( "2016-06-30" )); // type sent
				System.out.printf("-------------------------------------------------------------------------%n");
				System.out.printf(" --> Running query : ");
				populatedString = "select '2023-12-08 13:30:00'::timestamp,'2016-06-30'::date";
				System.out.printf("%s", populatedString);
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
					exitStatus = 1;
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
