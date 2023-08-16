/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

// Struct for gathering and maintaining function call information used in function definition lookup.
typedef struct FunctionCallContext {
	SqlFunctionCall *fc;			   // Function call statement as parsed from user query
	SqlValueType	 arg_types[YDB_MAX_PARMS]; // List of argument types for looking up function definition from function call
	boolean_t	 null_args[YDB_MAX_PARMS]; // Array of flags indicating whether the argument at the given index was SQL NULL
	int		 num_args;		   // The total number of arguments for the function call
	int		 num_ambiguous_args;	   /* The total number of actual arguments that could be matched with multiple types
						    * For example, "NULL" literals could match with any other type (YDBOcto#816).
						    * "INTEGER" literals could match with "NUMERIC" type too (YDBOcto#1010).
						    * This field is used to determine whether to use recursion (which could
						    * grow into an exponential algorithm) or use iteration (over existing prototypes).
						    */
} FunctionCallContext;

// Struct for tracking information about possible function definition matches during function definition lookup.
typedef struct FunctionMatchContext {
	SqlFunction *best_match;  // A pointer to the function definition that best matches the function call
	int	     num_matches; // Number of definitions that could match the given function call
} FunctionMatchContext;

int function_definition_lookup(FunctionCallContext *fc_context, FunctionMatchContext *match_context);
