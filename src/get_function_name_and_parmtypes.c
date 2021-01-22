/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <assert.h>

#include "octo.h"

#define ADD_TO_RET_BUFF_AND_RETURN_IF_FULL(RET_BUFF, RET_BUFF_LEN, STR) \
	{                                                               \
		int nbytes;                                             \
                                                                        \
		nbytes = snprintf(RET_BUFF, RET_BUFF_LEN, "%s", STR);   \
		if (nbytes >= RET_BUFF_LEN) {                           \
			return;                                         \
		}                                                       \
		RET_BUFF += nbytes;                                     \
		RET_BUFF_LEN -= nbytes;                                 \
	}

/* Given the parse tree representation of a function in the parameter "function", this function returns
 * a string containing the function name as well as the names of the parameter types all as one string
 * in "ret_buff" up to a maximum of "ret_buff" - 1 bytes (last byte is null terminated).
 */
void get_function_name_and_parmtypes(char *ret_buff, int ret_buff_len, char *function_name, SqlStatement *parm_list_stmt) {
	int		      nbytes;
	SqlParameterTypeList *cur_parm_list, *parm_list;

	assert(ret_buff_len); /* at least 1 byte is available */
	ret_buff[0] = '\0';   /* Null initialize in case we don't have space to write even function name */
	ADD_TO_RET_BUFF_AND_RETURN_IF_FULL(ret_buff, ret_buff_len, function_name);
	ADD_TO_RET_BUFF_AND_RETURN_IF_FULL(ret_buff, ret_buff_len, "(");
	if (NULL != parm_list_stmt) {
		UNPACK_SQL_STATEMENT(parm_list, parm_list_stmt, parameter_type_list);
		cur_parm_list = parm_list;
		do {
			SqlDataType data_type;
			char *typename;

			assert(data_type_struct_STATEMENT == cur_parm_list->data_type_struct->type);
			data_type = cur_parm_list->data_type_struct->v.data_type_struct.data_type;
			typename = get_user_visible_type_string(get_sqlvaluetype_from_sqldatatype(data_type, FALSE));
			nbytes = snprintf(ret_buff, ret_buff_len, "%s", typename);
			if (nbytes >= ret_buff_len) {
				return;
			}
			ret_buff += nbytes;
			ret_buff_len -= nbytes;
			cur_parm_list = cur_parm_list->next;
			if (cur_parm_list != parm_list) {
				ADD_TO_RET_BUFF_AND_RETURN_IF_FULL(ret_buff, ret_buff_len, ", ");
			} else {
				break;
			}
		} while (TRUE);
	}
	ADD_TO_RET_BUFF_AND_RETURN_IF_FULL(ret_buff, ret_buff_len, ")");
	return;
}
