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
import java.util.Random;

public class run_query {
	public static void main( String args[] ) {
		Properties props = new Properties();
		props.setProperty("user","ydb");
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
			}
		} else {
			// Randomize use of extended query
			Random rand = new Random();
			use_extended = rand.nextBoolean();
		}
		if (use_extended) {
			props.setProperty("preferQueryMode","extended");
		} else {
			props.setProperty("preferQueryMode","simple");
		}

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
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
