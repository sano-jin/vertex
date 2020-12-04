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
    for DIR in `ls -d ${1}test*/`
    do
	echo "testing \"${DIR}\""
	input=`cat "${DIR}input.dhl"`
	result=`stack exec dhli -- ${DIR}input.dhl ${2}`
	expected=`cat "${DIR}output.log"`

	if [ "x${result}" = "x${expected}" ]
	then
	    echo "ok"
	else
	    echo "Error (test-failed) :"

	    echo "the input was"
	    echo "${input}" | indentStdin
	    
	    echo "the result was"
	    echo "${result}" | indentStdin

	    echo "which is expected to be"
	    echo "${expected}" | indentStdin

	    break
	fi
    done
}

compile
if [ $isBuild -eq 0 ]
then
    testDIR "normal/" ""
    testDIR "errors/" ""
    testDIR "nd/" "--nd"
fi
