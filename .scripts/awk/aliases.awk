 "\"" BEGIN {{ gsub(/ /, "", $1); print "alias" " " $1 "=" $2} } END

