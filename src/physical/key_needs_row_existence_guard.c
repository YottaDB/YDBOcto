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

#include <assert.h>

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"

/* Should the code that "tmpl_tablejoin" generates for this key test that the row node holds a
 * value, "$DATA(<row node>)#10", rejecting a node that holds only descendants? The test is a
 * "QUIT:'(...)" inside the FOR loop of a table scan, and the probe "IF $DATA(<row node>)#10" where
 * the WHERE clause fixes the key to a value, a list or a range and there is no loop; "tmpl_row_exists"
 * emits the test for both.
 *
 * The guard exists to keep the table scan and the cross reference agreeing on what a row is
 * (YDBAIM#96). A table whose columns all take their value from the row node declares exactly one
 * node per row, so a row exists precisely when that node holds a value, and AIM type 0 - which
 * indexes exactly the nodes holding a value - is the right cross reference for it. Bare $ORDER,
 * which is what the FOR loop walks, also yields nodes that have only descendants. Without the
 * guard such a node comes back as a row that the cross reference has no entry for, and
 * "WHERE col IS NULL" disagrees with the unfiltered SELECT. The two definitions of a row are
 * chosen together in "tmpl_physical_plan" and only agree together. A key the WHERE clause fixes
 * probes its node without a loop, and the probe must ask the same question, or "WHERE id = 2"
 * finds a row that "SELECT *" does not.
 *
 * IMPORTANT: call this BEFORE "tmpl_tablejoin" temporarily clears "cross_reference_output_key"
 * for the second half of a right join. Read after that point, the field is NULL because it was
 * stashed, not because the key is unrelated to a cross reference, and the third test below could
 * not tell the two apart. The caller states that clearing up front instead, through
 * "xref_output_key_ignored".
 *
 * Inputs:
 *   "pplan"      - the physical plan being emitted; supplies iterKeys for the lookahead below.
 *   "key"        - the key whose FOR loop is being generated. "tmpl_tablejoin" has already
 *                  established that key->unique_id is the table currently being emitted.
 *   "cur_key"    - index of "key" within pplan->iterKeys.
 *   "total_keys" - pplan->total_iter_keys.
 *   "xref_output_key_ignored" - TRUE when the caller is about to clear "cross_reference_output_key"
 *                  for this emission, i.e. the FOR loop about to be generated is an ordinary bare
 *                  $ORDER over the table's own global even though the key names a cross reference.
 */
boolean_t key_needs_row_existence_guard(PhysicalPlan *pplan, SqlKey *key, unsigned int cur_key, unsigned int total_keys,
					boolean_t xref_output_key_ignored) {
	assert(cur_key < total_keys);
	if (NULL == key->table) {
		/* Not a key of a real table (e.g. the output key of a derived table), so there is no
		 * row node to test and no table definition to consult.
		 */
		return FALSE;
	}
	if (key->is_cross_reference_key || ((NULL != key->cross_reference_output_key) && !xref_output_key_ignored)) {
		/* This key is part of a cross reference, or is advanced by reading one: a non-NULL
		 * "cross_reference_output_key" means the FOR loop for this table key $ORDERs the AIM
		 * global's key dimension under one value instead of the table's own global. Either way
		 * the loop walks the index, and the index holds only nodes that are rows, so a $DATA of
		 * the row node per row would be an extra global access that can never fail.
		 *
		 * The second half of a right join is the exception, and is what
		 * "xref_output_key_ignored" reports: there the caller drops the cross reference and
		 * emits a plain $ORDER over the table's global, to visit the rows the first half did
		 * not match. That loop yields ancestor-only nodes like any other bare $ORDER, so it
		 * needs the guard exactly as an ordinary table scan does.
		 */
		return FALSE;
	}
	if (((cur_key + 1) < total_keys) && (pplan->iterKeys[cur_key + 1]->unique_id == key->unique_id)) {
		/* The next key in iteration order belongs to this same table ("unique_id" identifies the
		 * table), i.e. this is one column of a composite primary key and more of them follow.
		 * The row reference is then not yet fully bound, and $DATA on it would test an ancestor
		 * of the row: with PRIMARY KEY (order_id, line_id) and rows at ^ORD(order_id,line_id),
		 * a guard on the order_id loop would reject an order whose node holds no value of its
		 * own, and every line under it with it. The guard belongs on the last key of the table,
		 * which is also why it is emitted once per table rather than once per key.
		 */
		return FALSE;
	}
	/* Left for last: it walks every column of the table, whereas everything above is a field
	 * test, and this way it runs at most once per table instead of once per key column.
	 */
	return table_columns_all_on_row_node(key->table);
}
