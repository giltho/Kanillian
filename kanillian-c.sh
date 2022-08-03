FILE=$1
cbmc $FILE --show-symbol-table --json-ui | jq '.[] | select(has("symbolTable"))' > $FILE.symtab.json
esy x kanillian wpst $1.symtab.json -o ${FILE%".c"}.gil --json-ui