/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

/* Load read-only run-time variables. These variables cannot be SET, but are only accessible via SHOW.
 * They also have no entry in `pg_settings`.
 */
LOAD_READ_ONLY_VARIABLE("is_superuser", DEFAULT_IS_SUPERUSER)
LOAD_READ_ONLY_VARIABLE("session_authorization", DEFAULT_SESSION_AUTHORIZATION)

/* A list of macros for storing valid/canonical runtime parameter names for easy lookup during SET/SHOW commands.
 *
 * The first macro parameter is the name of the run-time parameter.
 *
 * The second parameter specifies a default row value for the given runtime parameter for initializing `pg_catalog.pg_settings` at
 * process startup. This table is local to the process and is stored in an LVN, i.e.
 * (%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,PARAMETER_NAME) for this purpose.
 *
 * Each row contains either known, valid defaults or else an empty row. Over time, various empty rows may be replaced with
 * valid defaults as needed by clients.
 *
 * Values for non-empty rows were determined by querying PostgreSQL with the following query:
 *	select * from pg_settings;
 */
LOAD_PG_VARIABLE("allow_system_table_mods", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("application_name", DEFAULT_APPLICATION_NAME DEFAULT_APPLICATION_NAME_ROW)
LOAD_PG_VARIABLE("archive_cleanup_command", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("archive_command", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("archive_mode", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("archive_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("array_nulls", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("authentication_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_analyze_scale_factor", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_analyze_threshold", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_freeze_max_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_max_workers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_multixact_freeze_max_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_naptime", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_vacuum_cost_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_vacuum_cost_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_vacuum_scale_factor", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_vacuum_threshold", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("autovacuum_work_mem", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("backend_flush_after", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("backslash_quote", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bgwriter_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bgwriter_flush_after", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bgwriter_lru_maxpages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bgwriter_lru_multiplier", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("block_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bonjour", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bonjour_name", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("bytea_output", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("check_function_bodies", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("checkpoint_completion_target", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("checkpoint_flush_after", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("checkpoint_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("checkpoint_warning", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("client_encoding", DEFAULT_CLIENT_ENCODING DEFAULT_CLIENT_ENCODING_ROW)
LOAD_PG_VARIABLE("client_min_messages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("cluster_name", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("commit_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("commit_siblings", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("constraint_exclusion", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("cpu_index_tuple_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("cpu_operator_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("cpu_tuple_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("cursor_tuple_fraction", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("data_checksums", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("data_directory_mode", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("data_sync_retry", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("db_user_namespace", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("deadlock_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("debug_assertions", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("debug_pretty_print", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("debug_print_parse", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("debug_print_plan", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("debug_print_rewritten", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_statistics_target", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_table_access_method", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_tablespace", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_text_search_config", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_transaction_deferrable", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_transaction_isolation", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("default_transaction_read_only", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("dynamic_shared_memory_type", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("effective_cache_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("effective_io_concurrency", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_bitmapscan", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_gathermerge", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_hashagg", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_hashjoin", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_indexonlyscan", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_indexscan", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_material", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_mergejoin", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_nestloop", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_parallel_append", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_parallel_hash", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_partition_pruning", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_partitionwise_aggregate", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_partitionwise_join", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_seqscan", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_sort", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("enable_tidscan", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("escape_string_warning", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("event_source", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("exit_on_error", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("extra_float_digits", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("force_parallel_mode", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("from_collapse_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("fsync", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("full_page_writes", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo_effort", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo_generations", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo_pool_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo_seed", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo_selection_bias", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("geqo_threshold", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("gin_fuzzy_search_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("gin_pending_list_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("hot_standby", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("hot_standby_feedback", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("huge_pages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("idle_in_transaction_session_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ignore_checksum_failure", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ignore_system_indexes", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("integer_datetimes", DEFAULT_INTEGER_DATETIMES DEFAULT_INTEGER_DATETIMES_ROW)
LOAD_PG_VARIABLE("intervalstyle", DEFAULT_INTERVALSTYLE DEFAULT_INTERVALSTYLE_ROW)
LOAD_PG_VARIABLE("jit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_above_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_debugging_support", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_dump_bitcode", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_expressions", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_inline_above_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_optimize_above_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_profiling_support", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("jit_tuple_deforming", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("join_collapse_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("krb_caseins_users", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lc_collate", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lc_ctype", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lc_messages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lc_monetary", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lc_numeric", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lc_time", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("listen_addresses", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lo_compat_privileges", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("local_preload_libraries", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("lock_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_autovacuum_min_duration", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_checkpoints", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_connections", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_destination", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_disconnections", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_duration", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_error_verbosity", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_executor_stats", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_file_mode", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_hostname", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_line_prefix", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_lock_waits", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_min_duration_statement", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_min_error_statement", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_min_messages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_parser_stats", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_planner_stats", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_replication_commands", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_rotation_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_rotation_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_statement", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_statement_stats", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_temp_files", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_timezone", DEFAULT_TIMEZONE_ROW)
LOAD_PG_VARIABLE("log_transaction_sample_rate", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("log_truncate_on_rotation", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("logging_collector", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("maintenance_work_mem", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_connections", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_files_per_process", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_function_args", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_identifier_length", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_index_keys", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_locks_per_transaction", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_logical_replication_workers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_parallel_maintenance_workers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_parallel_workers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_parallel_workers_per_gather", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_pred_locks_per_page", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_pred_locks_per_relation", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_pred_locks_per_transaction", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_prepared_transactions", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_replication_slots", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_stack_depth", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_standby_archive_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_standby_streaming_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_sync_workers_per_subscription", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_wal_senders", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_wal_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("max_worker_processes", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("min_parallel_index_scan_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("min_parallel_table_scan_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("min_wal_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("old_snapshot_threshold", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("operator_precedence_warning", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("parallel_leader_participation", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("parallel_setup_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("parallel_tuple_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("password_encryption", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("plan_cache_mode", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("port", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("post_auth_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("pre_auth_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("primary_slot_name", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("promote_trigger_file", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("quote_all_identifiers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("random_page_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_end_command", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_min_apply_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_action", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_inclusive", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_lsn", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_name", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_time", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_timeline", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("recovery_target_xid", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("restart_after_crash", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("restore_command", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("row_security", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("search_path", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("segment_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("seq_page_cost", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("server_encoding", DEFAULT_SERVER_ENCODING DEFAULT_SERVER_ENCODING_ROW)
LOAD_PG_VARIABLE("server_version", DEFAULT_SERVER_VERSION DEFAULT_SERVER_VERSION_ROW)
LOAD_PG_VARIABLE("server_version_num", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("session_replication_role", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("shared_buffers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("shared_memory_type", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_ca_file", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_cert_file", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_crl_file", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_key_file", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_library", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_passphrase_command", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_passphrase_command_supports_reload", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("ssl_prefer_server_ciphers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("standard_conforming_strings", DEFAULT_STANDARD_CONFORMING_STRINGS DEFAULT_STANDARD_CONFORMING_STRINGS_ROW)
LOAD_PG_VARIABLE("statement_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("superuser_reserved_connections", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("synchronize_seqscans", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("synchronous_commit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("synchronous_standby_names", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("syslog_facility", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("syslog_ident", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("syslog_sequence_numbers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("syslog_split_messages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("tcp_keepalives_count", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("tcp_keepalives_idle", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("tcp_keepalives_interval", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("tcp_user_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("temp_buffers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("temp_file_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("temp_tablespaces", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("timezone", DEFAULT_TIMEZONE DEFAULT_TIMEZONE_ROW)
LOAD_PG_VARIABLE("timezone_abbreviations", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("trace_notify", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("trace_recovery_messages", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("trace_sort", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("track_activities", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("track_activity_query_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("track_commit_timestamp", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("track_counts", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("track_functions", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("track_io_timing", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("transaction_deferrable", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("transaction_isolation", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("transaction_read_only", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("transform_null_equals", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("unix_socket_group", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("unix_socket_permissions", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("update_process_title", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_cleanup_index_scale_factor", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_cost_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_cost_limit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_cost_page_dirty", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_cost_page_hit", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_cost_page_miss", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_defer_cleanup_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_freeze_min_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_freeze_table_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_multixact_freeze_min_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("vacuum_multixact_freeze_table_age", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_block_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_buffers", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_compression", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_consistency_checking", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_init_zero", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_keep_segments", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_level", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_log_hints", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_receiver_status_interval", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_receiver_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_recycle", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_retrieve_retry_interval", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_segment_size", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_sender_timeout", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_sync_method", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_writer_delay", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("wal_writer_flush_after", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("work_mem", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("xmlbinary", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("xmloption", DEFAULT_EMPTY_ROW)
LOAD_PG_VARIABLE("zero_damaged_pages", DEFAULT_EMPTY_ROW)
