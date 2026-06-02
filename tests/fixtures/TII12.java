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

/* YDBOcto#1119 : INSERT through a JDBC prepared statement using setTimestamp() and setBoolean().
 *
 * The Postgres JDBC driver sends an unspecified parameter type OID (0) for setTimestamp() and the
 * BOOL type OID (16) for setBoolean(). Before the fix, Octo could not resolve either parameter type:
 *   - the unspecified-OID timestamp parameter reached code paths that "assert(FALSE)" and raised
 *     ERR_UNKNOWN_KEYWORD_STATE (get_sqldatatype_from_sqlvaluetype / check_column_lists_for_type_match),
 *   - the BOOL OID was not handled by get_sqlvaluetype_from_psql_type and raised the same error.
 * Either case aborted the INSERT with a FATAL error that closed the connection, which the JDBC driver
 * surfaced as SQLSTATE 08006 ("I/O error ... sending to the backend") followed by 08003 ("connection
 * has been closed"). So no rows were inserted and the prepared statement could not be used at all.
 *
 * This test inserts a timestamp and a boolean through bind parameters and reads them back to confirm
 * the INSERT now succeeds, the connection survives, and both values round-trip correctly (the timestamp
 * text and the boolean "true"/"false" text are stored and returned as the expected values).
 */

import java.sql.*;
import java.util.Properties;
import java.util.TimeZone;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;

public class TII12 {
	public static void main( String args[] ) {
		/* Pin the JVM default time zone so the setTimestamp() round-trip is deterministic
		 * regardless of the host time zone the test happens to run in.
		 */
		TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

		Properties props = new Properties();
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");
		props.setProperty("preferQueryMode","extended");

		/* The rows to insert. Kept in arrays so the values that are printed below are exactly the
		 * values that get bound into the prepared statement (no chance of the narrative drifting
		 * from what is actually inserted).
		 */
		int[]	  ids	     = { 1, 2 };
		String[]  names	     = { "alice", "bob" };
		String[]  timestamps = { "2026-06-01 13:00:00", "2024-12-31 12:00:00" };
		boolean[] flags	     = { true, false };

		String connectionString = "jdbc:postgresql://localhost:" + args[0] + "/";
		int exitStatus = 0;
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (conn != null) {
				Statement		statement;
				PreparedStatement	preparedStatement;
				ResultSet		resultSet;
				ResultSetMetaData	resultSetMetaData;
				int			columnCount, updateCount;

				String createString = "create table tii12 (id integer primary key, name varchar(20), ts timestamp, flag boolean)";
				String insertString = "insert into tii12 (id, name, ts, flag) values (?, ?, ?, ?)";
				String selectString = "select id, name, ts, flag from tii12 order by id";

				System.out.printf("YDBOcto#1119 : INSERT via JDBC prepared statement using setTimestamp() and setBoolean()%n");
				System.out.printf("--------------------------------------------------------------------------------------------%n");
				System.out.printf("Before the fix this INSERT aborted with ERR_UNKNOWN_KEYWORD_STATE and closed the connection%n");
				System.out.printf("(the JDBC driver reported SQLSTATE 08006 then 08003), so no rows were inserted. The driver%n");
				System.out.printf("sends parameter type OID 0 (unspecified) for setTimestamp() and OID 16 (bool) for setBoolean(),%n");
				System.out.printf("neither of which Octo resolved. After the fix the rows insert and read back correctly.%n");
				System.out.printf("%n");
				System.out.printf("Table  : %s%n", createString);
				System.out.printf("Insert : %s%n", insertString);
				System.out.printf("%n");

				/* Create the table to insert into. */
				statement = conn.createStatement();
				statement.executeUpdate(createString);
				statement.close();

				try {
					preparedStatement = conn.prepareStatement(insertString);
					for (int row = 0; row < ids.length; row++) {
						System.out.printf("Binding : id=%d, name='%s', setTimestamp('%s'), setBoolean(%b)%n",
								  ids[row], names[row], timestamps[row], flags[row]);
						preparedStatement.setInt(1, ids[row]);
						preparedStatement.setString(2, names[row]);
						preparedStatement.setTimestamp(3, Timestamp.valueOf(timestamps[row]));
						preparedStatement.setBoolean(4, flags[row]);
						updateCount = preparedStatement.executeUpdate();
						System.out.printf("  --> INSERT %d row%n", updateCount);
					}
					preparedStatement.close();
				} catch (SQLException e) {
					System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
					exitStatus = 1;
				}

				System.out.printf("%n");
				System.out.printf("Reading back : %s%n", selectString);
				try {
					preparedStatement = conn.prepareStatement(selectString);
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
					preparedStatement.close();
				} catch (SQLException e) {
					System.err.format("SQL State: %s\n%s\n", e.getSQLState(), e.getMessage());
					exitStatus = 1;
				}

				/* Probe other JDBC setter / data-type combinations, each on its own connection. Only the
				 * combinations that currently work correctly are run/asserted here. The combinations that
				 * do NOT work are commented out below, with the tracking issue (if any) noted, so this test
				 * documents the current state and is easy to extend once those are fixed.
				 */
				System.out.printf("%n");
				System.out.printf("Probing other JDBC setter / data-type combinations (each in its own connection):%n");
				probe(connectionString, props, "tii12_long",  "integer", "setLong       -> INTEGER", ps -> ps.setLong(2, 42L));
				probe(connectionString, props, "tii12_short", "integer", "setShort      -> INTEGER", ps -> ps.setShort(2, (short)7));
				probe(connectionString, props, "tii12_byte",  "integer", "setByte       -> INTEGER", ps -> ps.setByte(2, (byte)5));
				probe(connectionString, props, "tii12_time",  "time",    "setTime       -> TIME",    ps -> ps.setTime(2, java.sql.Time.valueOf("13:45:00")));
				probe(connectionString, props, "tii12_tsfrc", "timestamp", "setTimestamp(.frac) -> TIMESTAMP", ps -> ps.setTimestamp(2, java.sql.Timestamp.valueOf("2026-06-02 13:45:30.123")));

				/* The driver's "stringtype=unspecified" connection option makes setString() send an
				 * unspecified (0) type OID, like setTimestamp() always does. The probes below therefore
				 * exercise the resolution of an unspecified-type parameter against every non-date/time
				 * target column type too (resolution infers the type from the target column, like
				 * Postgres does).
				 */
				Properties unspecProps = new Properties();
				unspecProps.putAll(props);
				unspecProps.setProperty("stringtype", "unspecified");
				probe(connectionString, unspecProps, "tii12_uint",  "integer",   "setString(unspec) -> INTEGER",   ps -> ps.setString(2, "42"));
				probe(connectionString, unspecProps, "tii12_unum",  "numeric",   "setString(unspec) -> NUMERIC",   ps -> ps.setString(2, "3.14"));
				probe(connectionString, unspecProps, "tii12_ubool", "boolean",   "setString(unspec) -> BOOLEAN",   ps -> ps.setString(2, "true"));
				probe(connectionString, unspecProps, "tii12_udate", "date",      "setString(unspec) -> DATE",      ps -> ps.setString(2, "2026-06-02"));
				probe(connectionString, unspecProps, "tii12_uts",   "timestamp", "setString(unspec) -> TIMESTAMP", ps -> ps.setString(2, "2026-06-02 13:45:30"));

				/* An unspecified-type parameter that is not a VALUES table column cannot be resolved
				 * from a target column (there is no such column to resolve it from). Verify those
				 * cases produce a clean per-statement error -- not the pre-YDBOcto#1119
				 * ERR_UNKNOWN_KEYWORD_STATE FATAL that closed the connection -- and that the very
				 * same connection remains usable afterwards.
				 */
				System.out.printf("%n");
				System.out.printf("Unresolvable unspecified-type parameters (clean error expected; connection must survive):%n");
				expectCleanError(connectionString, props, "tii12_selsrc", "timestamp",
						 "INSERT..SELECT setTimestamp()",
						 "insert into tii12_selsrc (id, val) select 1, ?",
						 ps -> ps.setTimestamp(1, java.sql.Timestamp.valueOf("2026-06-02 13:45:30")));
				expectCleanError(connectionString, unspecProps, null, null,
						 "SELECT ? UNION SELECT 1 (unspec)",
						 "select ? union select 1",
						 ps -> ps.setString(1, "42"));

				/* The following do NOT work yet and are intentionally not run:
				 *
				 * YDBOcto#1126: setBigDecimal() into a NUMERIC column silently stores 0 (binary numeric
				 *               bind parameter is not decoded).
				 * probe(connectionString, props, "tii12_dec", "numeric", "setBigDecimal -> NUMERIC", ps -> ps.setBigDecimal(2, new java.math.BigDecimal("3.14")));
				 *
				 * YDBOcto#1125: setDate() into a DATE column fails -- the driver appends a " +00" timezone
				 *               which Octo's DATE text parser rejects (SQLSTATE 22007).
				 * probe(connectionString, props, "tii12_date", "date", "setDate -> DATE", ps -> ps.setDate(2, java.sql.Date.valueOf("2026-06-02")));
				 *
				 * YDBOcto#1127: setNull() triggers a rocto Bind error (invalid number of result column
				 *               format codes: -256), SQLSTATE 22003.
				 * probe(connectionString, props, "tii12_null", "integer", "setNull(INTEGER) -> INTEGER", ps -> ps.setNull(2, java.sql.Types.INTEGER));
				 *
				 * No issue (Octo has no FLOAT/DOUBLE type): the float8/float4 type OIDs sent by setDouble()/
				 * setFloat() are unhandled, so these crash the connection (ERR_UNKNOWN_KEYWORD_STATE).
				 * probe(connectionString, props, "tii12_dbl", "numeric", "setDouble -> NUMERIC", ps -> ps.setDouble(2, 2.5));
				 * probe(connectionString, props, "tii12_flt", "numeric", "setFloat  -> NUMERIC", ps -> ps.setFloat(2, 1.5f));
				 *
				 * Works, but the read-back value is the current time (non-deterministic), so it is not asserted here:
				 * probe(connectionString, props, "tii12_tsnow", "timestamp", "setTimestamp(now) -> TIMESTAMP", ps -> ps.setTimestamp(2, new java.sql.Timestamp(System.currentTimeMillis())));
				 */
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

	interface ParamBinder {
		void bind(PreparedStatement ps) throws SQLException;
	}

	/* Probe one JDBC setter / column-type combination on its own connection (so a crash that drops the
	 * connection does not mask the following probes). Prints whether the INSERT succeeded and what was read back.
	 */
	static void probe(String connStr, Properties props, String table, String colType, String label, ParamBinder binder) {
		try (Connection conn = DriverManager.getConnection(connStr, props)) {
			try (Statement st = conn.createStatement()) {
				st.executeUpdate("create table " + table + " (id integer primary key, val " + colType + ")");
			}
			int n;
			try (PreparedStatement ps = conn.prepareStatement("insert into " + table + " (id, val) values (?, ?)")) {
				ps.setInt(1, 1);
				binder.bind(ps);
				n = ps.executeUpdate();
			}
			String got;
			try (PreparedStatement sel = conn.prepareStatement("select val from " + table)) {
				ResultSet rs = sel.executeQuery();
				got = rs.next() ? rs.getString(1) : "(no row)";
			}
			System.out.printf("  %-28s OK     : INSERT %d, read back %s%n", label, n, got);
		} catch (SQLException e) {
			System.out.printf("  %-28s FAILED : %s %s%n", label, e.getSQLState(),
					  e.getMessage().replaceAll("[\\r\\n]+", " "));
		}
	}

	/* Run one statement that is expected to fail with a clean error and NOT drop the connection.
	 * Prints the error, then proves the same connection still works by running another query on it.
	 * If "table" is non-null it is created first (with an "id" key column and a "val" column of "colType").
	 */
	static void expectCleanError(String connStr, Properties props, String table, String colType, String label,
				     String sql, ParamBinder binder) {
		try (Connection conn = DriverManager.getConnection(connStr, props)) {
			if (null != table) {
				try (Statement st = conn.createStatement()) {
					st.executeUpdate("create table " + table + " (id integer primary key, val " + colType + ")");
				}
			}
			try {
				PreparedStatement ps = conn.prepareStatement(sql);
				binder.bind(ps);
				ps.execute();
				ps.close();
				System.out.printf("  %-32s UNEXPECTED SUCCESS%n", label);
			} catch (SQLException e) {
				System.out.printf("  %-32s clean error : %s %s%n", label, e.getSQLState(),
						  e.getMessage().replaceAll("[\\r\\n]+", " "));
			}
			try (Statement st = conn.createStatement()) {
				ResultSet rs = st.executeQuery("select 1");
				rs.next();
				System.out.printf("  %-32s still alive : select 1 -> %s%n", label, rs.getString(1));
			}
		} catch (SQLException e) {
			System.out.printf("  %-32s CONNECTION FAILED : %s %s%n", label, e.getSQLState(),
					  e.getMessage().replaceAll("[\\r\\n]+", " "));
		}
	}
}
