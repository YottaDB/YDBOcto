/****************************************************************
 *								*
 * Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	*
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

/* YDBOcto#1167 : See tests/fixtures/TJC027.sql for the purpose of the test.
 * Each query is prepared and run on a connection of its own (its own rocto process), so that each one is the first
 * to need its cross reference in that process. It is then run a second time with the same value.
 */
public class TJC027 {
	static String runQuery(PreparedStatement preparedStatement) {
		StringBuilder rows = new StringBuilder();

		try (ResultSet resultSet = preparedStatement.executeQuery()) {
			while (resultSet.next()) {
				if (0 < rows.length()) {
					rows.append(",");
				}
				rows.append(resultSet.getString(1));
			}
		} catch (SQLException e) {
			return "SQL State: " + e.getSQLState() + " : " + e.getMessage();
		}
		return "[" + rows.toString() + "]";
	}

	public static void main(String args[]) {
		Properties props = new Properties();
		props.setProperty("user", System.getProperty("user.name"));
		props.setProperty("password", "ydbrocks");
		props.setProperty("ssl", "false");
		props.setProperty("sslmode", "disable");
		props.setProperty("preferQueryMode", "extended");

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		int    exitStatus = 0;
		// Each entry: the ORDER BY of the query, then the value bound to "id LIKE ?"
		String[][] cases = {{"a", "ab"}, {"a DESC", "zz"}, {"a", "a%"}, {"id", "ab"}};
		for (String[] oneCase : cases) {
			String sql = "SELECT id FROM TJC027 WHERE id LIKE ? ORDER BY " + oneCase[0];
			try (Connection conn = DriverManager.getConnection(connectionString, props)) {
				PreparedStatement query = conn.prepareStatement(sql);
				query.setString(1, oneCase[1]);
				System.out.printf("[%s] bound to '%s' -> 1st run: %s", sql, oneCase[1], runQuery(query));
				System.out.printf(" : 2nd run: %s%n", runQuery(query));
			} catch (SQLException e) {
				System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
				exitStatus = 1;
			}
		}
		System.exit(exitStatus);
	}
}
