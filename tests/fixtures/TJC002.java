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

public class TJC002 {
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

				preparedStatement = conn.prepareStatement("select * from names where id = ? OR firstname = ?");
				for (int query = 0; query < 4; query++) {
					preparedStatement.setInt(1,1);
					System.out.printf("-------------------------------------------------------------------------%n");
					System.out.printf(" --> Running query : ");
					switch (query) {
					case 0:
						preparedStatement.setString(2,"Zero");
						System.out.printf("select * from names where id = 1 OR firstname = 'Zero'");
						break;
					case 1:
						preparedStatement.setInt(2,1);
						System.out.printf("select * from names where id = 1 OR firstname = 1");
						break;
					case 2:
						preparedStatement.setString(1,"Acid");
						System.out.printf("select * from names where id = 'Acid' OR firstname = 1");
						break;
					case 3:
						preparedStatement.setInt(1,2);
						preparedStatement.setString(2,"Joey");
						System.out.printf("select * from names where id = 2 OR firstname = 'Joey'");
						break;
					}
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
