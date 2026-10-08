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

/* YDBOcto#1158 : LIKE / ILIKE with a bind parameter as the pattern ("WHERE v LIKE ?"). See tests/fixtures/TJC025.sql for
 * the purpose of the test.
 * Usage:
 *	TJC025 <port>				 : For each pattern, print the rows from "v LIKE ?" (which can walk the cross
 *						   reference of column v) next to the rows from "COALESCE(v, NULL) LIKE ?"
 *						   (which checks every row). The two lists must be the same.
 *	TJC025 <port> "" <LIKE|ILIKE> <pattern> : Prepare "v LIKE ?" (or ILIKE) and bind it to <pattern> once, on a new
 *						   connection, so the caller can look up the plan that one Bind picked.
 */
public class TJC025 {
	static String runQuery(PreparedStatement preparedStatement) throws SQLException {
		ResultSet     resultSet;
		StringBuilder ids;

		ids = new StringBuilder();
		resultSet = preparedStatement.executeQuery();
		while (resultSet.next()) {
			if (0 < ids.length()) {
				ids.append(",");
			}
			ids.append(resultSet.getString(1));
		}
		return ids.toString();
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
		if (4 <= args.length) {
			// Bind one value once (see usage above)
			String operator = args[2], pattern = args[3];
			try (Connection conn = DriverManager.getConnection(connectionString, props)) {
				PreparedStatement query
				    = conn.prepareStatement("SELECT id FROM TJC025 WHERE v " + operator + " ? ORDER BY id");
				query.setString(1, pattern);
				System.out.printf("[v %s ?] bound to '%s' -> [%s]%n", operator, pattern, runQuery(query));
			} catch (SQLException e) {
				System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
				exitStatus = 1;
			}
			System.exit(exitStatus);
		}
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			String[] patterns = {"PAT%", "PAT%%", "PATIENT%", "ZZZ%", "PAT%ENT", "PAT_ENT", "PAT", "PAT\\%%", "PAT\\_%", "12%",
					     "a\u00f1%", "a%", "%AT", "_AT%", "%", ""};
			PreparedStatement columnQuery, everyRowQuery;

			columnQuery = conn.prepareStatement("SELECT id FROM TJC025 WHERE v LIKE ? ORDER BY id");
			everyRowQuery = conn.prepareStatement("SELECT id FROM TJC025 WHERE COALESCE(v, NULL) LIKE ? ORDER BY id");
			for (String pattern : patterns) {
				columnQuery.setString(1, pattern);
				everyRowQuery.setString(1, pattern);
				System.out.printf("setString('%s') : v LIKE ? -> [%s] : COALESCE(v, NULL) LIKE ? -> [%s]%n", pattern,
						  runQuery(columnQuery), runQuery(everyRowQuery));
			}
			/* A pattern that ends in an escape character is an error. "v LIKE ?" must issue it even when no row
			 * starts with the prefix ('ZZZ'), and even when the escape is after the prefix ('PAT%\').
			 */
			String[] badPatterns = {"PAT\\", "ZZZ\\", "PAT%\\"};
			for (String pattern : badPatterns) {
				columnQuery.setString(1, pattern);
				try {
					runQuery(columnQuery);
					System.out.printf("setString('%s') : v LIKE ? -> no error (NOT expected)%n", pattern);
				} catch (SQLException e) {
					System.out.printf("setString('%s') : v LIKE ? -> SQL State: %s : %s%n", pattern, e.getSQLState(),
							  e.getMessage());
				}
			}
			/* A table queried only with patterns that have no prefix. Each Bind picks the plain LIKE plan (a table scan),
			 * so no cross reference is built for TJC025B.v. TJC025.bats.in checks that.
			 */
			String[] noPrefixPatterns = {"%AT", "_AT%", "%", ""};
			PreparedStatement noPrefixQuery = conn.prepareStatement("SELECT id FROM TJC025B WHERE v LIKE ? ORDER BY id");
			for (String pattern : noPrefixPatterns) {
				noPrefixQuery.setString(1, pattern);
				System.out.printf("TJC025B : setString('%s') : v LIKE ? -> [%s]%n", pattern, runQuery(noPrefixQuery));
			}
			/* ILIKE with a bind parameter. The prefix ends at the first letter: '12' is an "=", '12%' walks over '12' with no
			 * check (it matches what LIKE '12%' does), '12A%' walks over '12' and checks the ILIKE, 'pat%' reads every row.
			 */
			String[] ilikePatterns = {"12%", "12A%", "12", "pat%"};
			PreparedStatement ilikeQuery = conn.prepareStatement("SELECT id FROM TJC025 WHERE v ILIKE ? ORDER BY id");
			PreparedStatement ilikeEveryRowQuery
			    = conn.prepareStatement("SELECT id FROM TJC025 WHERE COALESCE(v, NULL) ILIKE ? ORDER BY id");
			for (String pattern : ilikePatterns) {
				ilikeQuery.setString(1, pattern);
				ilikeEveryRowQuery.setString(1, pattern);
				System.out.printf("setString('%s') : v ILIKE ? -> [%s] : COALESCE(v, NULL) ILIKE ? -> [%s]%n", pattern,
						  runQuery(ilikeQuery), runQuery(ilikeEveryRowQuery));
			}
			// A value bound without a data type (the client leaves the type to the server)
			columnQuery.setObject(1, "PAT%", Types.OTHER);
			everyRowQuery.setObject(1, "PAT%", Types.OTHER);
			System.out.printf("setObject('PAT%%', Types.OTHER) : v LIKE ? -> [%s] : COALESCE(v, NULL) LIKE ? -> [%s]%n",
					  runQuery(columnQuery), runQuery(everyRowQuery));
			/* A query longer than the buffer that Bind first reads it into (1024 bytes, "OCTO_INIT_BUFFER_LEN"), so that
			 * "get_bind_replan_routine()" has to grow the buffer to parse the query again
			 */
			String		  longValue = "Z".repeat(1100);
			PreparedStatement longQuery
			    = conn.prepareStatement("SELECT id FROM TJC025 WHERE v LIKE ? AND v <> '" + longValue + "' ORDER BY id");
			longQuery.setString(1, "PAT%");
			System.out.printf("setString('PAT%%') : v LIKE ? AND v <> '<1100 x Z>' (a query of more than 1024 bytes) -> [%s]%n",
					  runQuery(longQuery));
		} catch (SQLException e) {
			System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
			exitStatus = 1;
		} catch (Exception e) {
			e.printStackTrace();
			exitStatus = 1;
		}
		System.exit(exitStatus);
	}
}
