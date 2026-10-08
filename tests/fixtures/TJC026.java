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

/* YDBOcto#1127 : Bind parameters whose value is NULL (JDBC "setNull()"). See tests/fixtures/TJC026.sql for the purpose
 * of the test and the behavior before the fix. Each query below prints its rows (a NULL column value prints as NULL) or
 * its error.
 */
public class TJC026 {
	static void runQuery(PreparedStatement preparedStatement, String description) {
		System.out.printf("%s%n", description);
		try {
			if (preparedStatement.execute()) {
				ResultSet	  resultSet = preparedStatement.getResultSet();
				ResultSetMetaData metaData = resultSet.getMetaData();
				int		  columnCount = metaData.getColumnCount();
				int		  rowCount = 0;

				while (resultSet.next()) {
					rowCount++;
					StringBuilder row = new StringBuilder();
					for (int i = 1; i <= columnCount; i++) {
						String value = resultSet.getString(i);
						if (1 < i) {
							row.append("|");
						}
						row.append((null == value) ? "NULL" : value);
					}
					System.out.printf("  %s%n", row);
				}
				if (0 == rowCount) {
					System.out.printf("  (0 rows)%n");
				}
			} else {
				System.out.printf("  %d row(s) affected%n", preparedStatement.getUpdateCount());
			}
		} catch (SQLException e) {
			System.out.printf("  SQL State: %s : %s%n", e.getSQLState(), e.getMessage());
		}
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
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			PreparedStatement ps;
			String		  selectAll = "SELECT * FROM TJC026 ORDER BY id";

			// The query from the issue: a NULL parameter followed by nothing
			ps = conn.prepareStatement("INSERT INTO TJC026 (id, val) VALUES (?, ?)");
			ps.setInt(1, 4);
			ps.setNull(2, Types.INTEGER);
			runQuery(ps, "INSERT INTO TJC026 (id, val) VALUES (4, setNull(INTEGER))");

			// A NULL parameter followed by a non-NULL one (the case that misread the following parameter)
			ps = conn.prepareStatement("INSERT INTO TJC026 (id, val, name) VALUES (?, ?, ?)");
			ps.setInt(1, 5);
			ps.setNull(2, Types.INTEGER);
			ps.setString(3, "five");
			runQuery(ps, "INSERT INTO TJC026 (id, val, name) VALUES (5, setNull(INTEGER), 'five')");

			// A text value followed by a NULL. The NULL's length (-1) is the bytes FF FF FF FF right after the text value.
			ps = conn.prepareStatement("INSERT INTO TJC026 (id, name, val) VALUES (?, ?, ?)");
			ps.setInt(1, 6);
			ps.setString(2, "six");
			ps.setNull(3, Types.INTEGER);
			runQuery(ps, "INSERT INTO TJC026 (id, name, val) VALUES (6, 'six', setNull(INTEGER))");
			ps = conn.prepareStatement("SELECT id, '[' || name || ']', name = 'six' FROM TJC026 WHERE id = 6");
			runQuery(ps, "SELECT id, '[' || name || ']', name = 'six' FROM TJC026 WHERE id = 6");

			ps = conn.prepareStatement("UPDATE TJC026 SET name = ? WHERE id = ?");
			ps.setNull(1, Types.VARCHAR);
			ps.setInt(2, 1);
			runQuery(ps, "UPDATE TJC026 SET name = setNull(VARCHAR) WHERE id = 1");

			ps = conn.prepareStatement(selectAll);
			runQuery(ps, selectAll);

			// The parameter is SQL NULL, not an empty string
			ps = conn.prepareStatement("SELECT ? IS NULL");
			ps.setNull(1, Types.VARCHAR);
			runQuery(ps, "SELECT setNull(VARCHAR) IS NULL");
			ps = conn.prepareStatement("SELECT COALESCE(?, 'was NULL')");
			ps.setNull(1, Types.VARCHAR);
			runQuery(ps, "SELECT COALESCE(setNull(VARCHAR), 'was NULL')");

			System.out.printf("# Before YDBOcto#1127, each query below failed at Bind. Now each one runs, and returns no rows, as a%n");
			System.out.printf("# comparison with NULL is never true (on a key column, a non-key column, and in a LIKE).%n");
			ps = conn.prepareStatement("SELECT id FROM TJC026 WHERE id = ?");
			ps.setNull(1, Types.INTEGER);
			runQuery(ps, "SELECT id FROM TJC026 WHERE id = setNull(INTEGER) (key column)");
			ps = conn.prepareStatement("SELECT id FROM TJC026 WHERE val = ?");
			ps.setNull(1, Types.INTEGER);
			runQuery(ps, "SELECT id FROM TJC026 WHERE val = setNull(INTEGER) (non-key column)");
			ps = conn.prepareStatement("SELECT id FROM TJC026 WHERE name LIKE ?");
			ps.setNull(1, Types.VARCHAR);
			runQuery(ps, "SELECT id FROM TJC026 WHERE name LIKE setNull(VARCHAR)");

			System.out.printf("# The same prepared statement bound to NULL, then to 'PAT%%': the NULL of the first Bind does not stick.%n");
			System.out.printf("# Row 1's name was set to NULL by the UPDATE above, so only row 2 matches 'PAT%%'.%n");
			ps = conn.prepareStatement("SELECT id FROM TJC026 WHERE name LIKE ? ORDER BY id");
			ps.setNull(1, Types.VARCHAR);
			runQuery(ps, "SELECT id FROM TJC026 WHERE name LIKE setNull(VARCHAR) ORDER BY id");
			ps.setString(1, "PAT%");
			runQuery(ps, "SELECT id FROM TJC026 WHERE name LIKE 'PAT%' ORDER BY id");
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
