match($0, /%include ".*"/) {
  fileName = path substr($0, RSTART+10, RLENGTH-11);
  while(( getline line<fileName) > 0 ) {
     print line
  }
}

! /^%include/ { print $0 }
