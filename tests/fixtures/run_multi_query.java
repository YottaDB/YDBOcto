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
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class run_multi_query {
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
				String queryString;
				PreparedStatement preparedStatement;
				ResultSet resultSet;
				ResultSetMetaData resultSetMetaData;
				int columnCount;

				if (2 == args.length) {
					BufferedReader reader;
					try {
						reader = new BufferedReader(new FileReader(args[1]));
						String line = reader.readLine();
						while (line != null) {
							preparedStatement = conn.prepareStatement(line);
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
							line = reader.readLine();
						}
					} catch (IOException e) {
						e.printStackTrace();
					}
				} else {
					System.out.println("Please pass a SQL file.");
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
