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

public class TJC011 {
	public static void main( String args[] ) {
		Properties props = new Properties();
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");
		props.setProperty("preferQueryMode","extended");

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				String[]		queryStrings = new String[11];
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount, rowLimit, numQueries;

				numQueries = 0;
				// Run with limit
				queryStrings[numQueries++] = "select * from names;";
				// Run without limit to confirm new query cycle
				queryStrings[numQueries++] = "select * from names;";
				queryStrings[numQueries++] = "select * from names where firstname = 'Zero';";
				// Run INSERT INTO query too to confirm max row limit does not affect it
				// since that type of query does not anyways return any rows.
				queryStrings[numQueries++] = "insert into names select id+6,firstname,lastname from names;";
				// Verify though that the INSERT INTO works fine by doing a SELECT afterwards
				queryStrings[numQueries++] = "select * from names where firstname = 'Zero';";
				// Check if SHOW command with default value works
				queryStrings[numQueries++] = "show DateStyle;";
				queryStrings[numQueries++] = "show transaction_isolation;";
				// Check if SET command works
				queryStrings[numQueries++] = "set DateStyle to ISO;";
				queryStrings[numQueries++] = "set transaction_isolation to 'read committed';";
				// Check if repeat of SHOW command after SET reflects updates done by SET
				queryStrings[numQueries++] = "show DateStyle;";
				queryStrings[numQueries++] = "show transaction_isolation;";
				for (int query = 0; query < numQueries; query++) {
					preparedStatement = conn.prepareStatement(queryStrings[query]);
					switch(query) {
						case 0:
						case 3:
							// Run queries 0 and 3 with maximum row-count limit of 3
							rowLimit = 3;
							break;
						case 2:
							// Run query 2 with maximum row-count limit of 1
							rowLimit = 1;
							break;
						default:
							// Run all other queries with unlimited row-count (i.e. returns all rows)
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
