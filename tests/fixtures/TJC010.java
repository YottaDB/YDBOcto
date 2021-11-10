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

public class TJC010 {
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

				queryString = "select * from names where id = ? OR firstname = ? OR id = ? OR firstname = ? OR"
						+ " id = ? OR firstname = ? OR id = ? OR firstname = ? OR"
						+ " id = ? OR firstname = ? OR id = ? OR firstname = ? OR"
						+ " id = ? OR firstname = ? OR id = ? OR firstname = ? OR"
						+ " id = ? OR firstname = ? OR id = ? OR firstname = ?";
				preparedStatement = conn.prepareStatement(queryString);
				preparedStatement.setInt(1,0);
				preparedStatement.setString(2,"Zero");
				preparedStatement.setInt(3,1);
				preparedStatement.setString(4,"Lord");
				preparedStatement.setInt(5,2);
				preparedStatement.setString(6,"Acid");
				preparedStatement.setInt(7,3);
				preparedStatement.setString(8,"Joey");
				preparedStatement.setInt(9,4);
				preparedStatement.setString(10,"Cereal");
				preparedStatement.setInt(11,5);
				preparedStatement.setString(12,"Zero");
				preparedStatement.setInt(13,6);
				preparedStatement.setString(14,"Lord");
				preparedStatement.setInt(15,0);
				preparedStatement.setString(16,"Acid");
				preparedStatement.setInt(17,1);
				preparedStatement.setString(18,"Joey");
				preparedStatement.setInt(19,2);
				preparedStatement.setString(20,"Cereal");
				System.out.printf("-------------------------------------------------------------------------%n");
				System.out.printf(" --> Running query : ");
				populatedString = "select * from names where"
						+ " id = 0 OR firstname = 'Zero' OR id = 1 OR firstname = 'Lord' OR"
						+ " id = 2 OR firstname = 'Acid' OR id = 3 OR firstname = 'Joey' OR"
						+ " id = 4 OR firstname = 'Cereal' OR id = 5 OR firstname = 'Zero' OR"
						+ " id = 6 OR firstname = 'Lord' OR id = 0 OR firstname = 'Acid' OR"
						+ " id = 1 OR firstname = 'Joey' OR id = 2 OR firstname = 'Cereal'";
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
