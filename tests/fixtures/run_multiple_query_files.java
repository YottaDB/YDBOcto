/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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
		if ("useextended" == args[2])
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
						resultSet = preparedStatement.executeQuery();
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
