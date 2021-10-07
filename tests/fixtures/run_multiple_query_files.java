/****************************************************************
 *								*
 * Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

import java.sql.*;
import java.util.Arrays;
import java.util.Properties;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.io.FileReader;
import java.io.IOException;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Scanner;
import java.util.regex.Pattern;

/*
 * run_multiple_query_files iterates over files matching the specified queryfile pattern and executes the queries
 * in the files. Note that the database connection is established only once. The query results are written to
 * separate files to facilitate result comparison.
 * Parameters:
 * 	args[0] port
 * 	args[1] queryfile pattern
 * 	args[2] jdbc protocol
 * 	args[3] output file extension .octo.out or .psql.out
 * 	args[4] databasename
 */
public class run_multiple_query_files {
	public static void main( String args[] ) {
		String connectionString;
		String databaseName = new String("");
		Properties props = new Properties();
		// Setup connection properties
		props.setProperty("user",System.getProperty("user.name"));
		props.setProperty("password","ydbrocks");
		props.setProperty("ssl","false");
		props.setProperty("sslmode","disable");
		if (args[2].equals("useextended"))
			props.setProperty("preferQueryMode","extended");
		else
			props.setProperty("preferQueryMode","simple");
		if (5 == args.length)
			databaseName = args[4];
		// Establish connection to database
		connectionString = "jdbc:postgresql://localhost:" + args[0] + "/" + databaseName;
		try (Connection conn = DriverManager.getConnection(connectionString, props)) {
			if (null != conn) {
				if (4 <= args.length) {
					// Init
					Scanner reader;
					String line;
					FileWriter writer;
					PrintWriter printWriter;
					PreparedStatement preparedStatement;
					ResultSet resultSet;
					ResultSetMetaData resultSetMetaData;
					int columnCount;

					// Search present directory for query files
					File dir = new File(".");
					File files[] = dir.listFiles(new RegxFileFilter(args[1]));
					// The listFiles method does not guarantee any order.
					// But we want to run the query files in alphabetical order
					// (e.g. the TCT019 subtest will fail if queries are run in a different order
					// as it expects the first few queries to generate plans and later queries
					// to reuse the generated plans). Hence sort the array returned by "listFiles()".
					// This works because File is a comparable class, which by default sorts
					// pathnames lexicographically. See https://stackoverflow.com/a/7199929 for details.
					Arrays.sort(files);
					for (File file : files) {
						// Read query from file
						reader = new Scanner (file);
						reader.useDelimiter(";");
						line = reader.next();
						// Create writer to write query execution results
						writer = new FileWriter(file.getName() + args[3]);
						printWriter = new PrintWriter(writer);
						// Setup prepared statement and execute query on server
						preparedStatement = conn.prepareStatement(line);
						// Note that the query could be a SELECT OR a DML (INSERT/DELETE etc.) OR a
						// DDL (CREATE TABLE, DROP TABLE etc.) command. Therefore we cannot use
						// "executeQuery()" method as it can only be used for "SELECT" whereas we
						// can encounter DML/DDL commands too. Use "execute()" method and based on its
						// return value decide whether a result set was returned or not. See below url
						// https://docs.oracle.com/javase/8/docs/api/java/sql/PreparedStatement.html#execute--
						// for more details.
						if (preparedStatement.execute()) {
							// It is a SELECT query
							resultSet = preparedStatement.getResultSet();
							// Fetch query execution results
							resultSetMetaData = resultSet.getMetaData();
							columnCount = resultSetMetaData.getColumnCount();
							while (resultSet.next()) {
								for (int i = 1; i <= columnCount; i++) {
									printWriter.printf("%s",resultSet.getString(i));
									if (i != columnCount)
										printWriter.printf("|");
								}
								printWriter.printf("%n");
							}
						} else {
							// It is a DML or DDL command. Produce some output even in this case
							// run_query_in_octo_and_postgres_and_crosscheck() in test_helpers.bash.in
							// relies on that. In the case of INSERT INTO and DELETE FROM, the output
							// will also contain the number of rows updated which is good to cross
							// check between Postgres and Octo.
							int numRows = preparedStatement.getUpdateCount();
							printWriter.printf("# Note that the below would be 0 for CREATE/DROP commands\n");
							printWriter.printf("# It would be non-zero only for INSERT INTO, DELETE FROM etc.\n");
							printWriter.printf("Number of rows updated = %d\n", numRows);
						}
						// Close reader and writer objects to get a
						// clean state for next query file execution.
						printWriter.close();
						reader.close();
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

	static class RegxFileFilter implements java.io.FileFilter {

		final Pattern pattern;

		public RegxFileFilter(String regx) {
			pattern = Pattern.compile(regx);
		}

		public boolean accept(File file) {
			if ((file.getName().endsWith(".sql")) && (pattern.matcher(file.getName()).find()))
				return true;
			else
				return false;
		}
	}
}
