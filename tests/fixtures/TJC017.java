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
import java.io.File;

public class TJC017 {
	public static void main( String args[] ) {
		Properties props = new Properties();
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");

		System.out.printf("protocol: %s%n", args[2]);
		if (args[2].equals("useextended"))
			props.setProperty("preferQueryMode","extended");
		else
			props.setProperty("preferQueryMode","simple");

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				int			num_queries = 2;
				String			queryString;
				StringBuilder[]		builders = new StringBuilder[num_queries];
				PreparedStatement[]	statements = new PreparedStatement[num_queries];
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData ;
				int			columnCount;
				/* Use the maximum number of prepared statement parameters (INT16_MAX), minus 1 for the single
				 * regular column identifier (id) used in the query below.
				 */
				int			numFields = 32500;

				for (int cur_query = 0; cur_query < num_queries; cur_query++) {
					builders[cur_query] = new StringBuilder();
					builders[cur_query].append("SELECT id");
					for (int curField = 0; curField < numFields; curField++) {
						builders[cur_query].append(", ?");
					}
					builders[cur_query].append(", 123456789 FROM names WHERE");
					builders[cur_query].append(" firstname = firstname;");
					statements[cur_query] = conn.prepareStatement(builders[cur_query].toString());
					for (int curField = 0; curField < numFields; curField++) {
						/* Populate parameters, incrementing by one for compatibility with 1-indexing expected by
						 * the setInt method.
						 */
						if (0 == (curField % 2)) {
							/* Reverse order of population to force each target error code path, one
							 * each in copy_text_parameter and copy_binary_parameter. This is works by
							 * causing the breaking parameter (which causes overflow when expanded) to
							 * be a text parameter in one case and a binary (int) parameter in the
							 * other.
							 */
							if (0 == cur_query) {
								statements[cur_query].setInt(curField+1, curField+1000000001);
							} else {
								statements[cur_query].setString(curField+1, "123456789012345678901234567890123456789");
							}
						} else {
							// Same reversal logic as above.
							if (0 == cur_query) {
								statements[cur_query].setString(curField+1, "123456789012345678901234567890123456789");
							} else {
								statements[cur_query].setInt(curField+1, curField+1000000001);
							}
						}
					}
					System.out.printf("-------------------------------------------------------------------------%n");
					System.out.printf(" --> Running query : ");
					if (0 == cur_query) {
						System.out.printf("SELECT id, 1000000001, '123456789012345678901234567890123456789', 1000000002, '123456789012345678901234567890123456789', ..., %d FROM names WHERE firstname = firstname ...;", numFields+1000000001);
					} else {
						System.out.printf("SELECT id, '123456789012345678901234567890123456789', 1000000001, '123456789012345678901234567890123456789', 1000000002, ..., %d FROM names WHERE firstname = firstname ...;", numFields+1000000001);
					}
					System.out.printf(" <--%n");
					System.out.printf("-------------------------------------------------------------------------%n");
					try {
						resultSet = statements[cur_query].executeQuery();
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
					statements[cur_query].close();
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
