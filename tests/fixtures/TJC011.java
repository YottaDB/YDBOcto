/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

public class TJC011 {
	public static void main( String args[] ) {
		Properties props = new Properties();
		props.setProperty("user","ydb");
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");
		props.setProperty("preferQueryMode","extended");

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				String[]		queryStrings = new String[3];
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount, rowLimit;

				queryStrings[0] = "select * from names;";	// Run with limit
				queryStrings[1] = "select * from names;";	// Run without limit to confirm new query cycle
				queryStrings[2] = "select * from names where firstname = 'Zero';";
				for (int query = 0; query < 3; query++) {
					preparedStatement = conn.prepareStatement(queryStrings[query]);
					switch(query) {
						case 0:
							rowLimit = 3;
							break;
						case 2:
							rowLimit = 1;
							break;
						default:
							// This returns all rows
							rowLimit = 0;
							break;
					}
					preparedStatement.setMaxRows(rowLimit);
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
					}
					preparedStatement.close();
				}
			} else {
					System.out.println("Failed to make connection!");
			}
		} catch (SQLException e) {
			System.err.format("SQL State: %s\n%s", e.getSQLState(), e.getMessage());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
