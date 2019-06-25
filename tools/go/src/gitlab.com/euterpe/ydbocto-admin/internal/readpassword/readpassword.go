/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

package readpassword

import (
	"os"
	"fmt"
	"bufio"
	"golang.org/x/crypto/ssh/terminal"
)

// ReadPassword reads a password string from stdin, changing handling
// depending on whether stdin is or isn't a terminal.
func ReadPassword(prompt string) (pw []byte, err error) {
	fd := int(os.Stdin.Fd())
	if terminal.IsTerminal(fd) {
		fmt.Fprint(os.Stderr, prompt)
		pw, err = terminal.ReadPassword(fd)
		fmt.Fprintln(os.Stderr)
	} else {
		// var line string
		reader := bufio.NewReader(os.Stdin)
		pw, err = reader.ReadBytes('\n')
		// pw = []byte(line)
	}
	return pw, err
}
