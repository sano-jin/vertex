#!/bin/bash
indentStdin() {
  indent='    '
  while read line; do
      echo "${indent}${line}"
  done
  echo
}

isCompiled=0

compile() {
    cd ../src
    ghc Main.hs -Wall
    isCompiled=$?
    cd ../test
}	
    
testDIR() {
    for DIR in `ls -d ${1}test*/`
    do
	echo "testing \"${DIR}\""

	input=`cat "${DIR}input.txt"`
	input2=`cat "${DIR}input.txt"`
	
	result=`../src/Main "${input}"`

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
if [ $isCompiled -eq 0 ]
then
    testDIR "normal/"
    testDIR "errors/"
fi
