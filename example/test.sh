#!/bin/bash
indentStdin() {
  indent='    '
  while read line; do
      echo "${indent}${line}"
  done
  echo
}

isBuild=0

compile() {
    stack build
    isBuild=$?
}	
    
testDIR() {
    for FileName in `ls ${1}*.dhl`
    do
	echo "testing \"${FileName}\""
	stack exec dhli -- ${FileName} ${2}
    done
}

compile
if [ $isBuild -eq 0 ]
then
    testDIR "./" ""
    testDIR "nd/" "--nd"
fi
