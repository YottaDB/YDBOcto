#!/bin/bash

# Extract base filename
echo -n "Extracting basename... "
grep "^/" make_warnings.txt | cut -d " " -f 1 | sed 's!.*/!!' | cut -d ":" -f 1 > filenames.txt
echo "OK."

# Extract base warning message
echo -n "Extracting warning message... "
grep "^/" make_warnings.txt | cut -d " " -f 2- > warnings.txt
echo "OK."

# Concatenate filenames with warning messages and sort
echo -n "Combining and sorting filenames with messages... "
paste -d ": " filenames.txt warnings.txt | sort > sorted_warnings.txt
echo "OK."
