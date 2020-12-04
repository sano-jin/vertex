# Test
*Adopted from the older project.*
*A more sophisticated way is prefered.*

Files/directories for testing

## Usage 

Just execute the `test.sh`.
It will compile the program and test it with the expected result.

```bash
./test.sh
```


## Directory structure

```
test/
 +- README                # This file
 +- errors/               # files for testing error-handlings
 |   +- test0-.../
 |   |   +- input.txt     # test-code
 |   |   +- output.log    # expected output
 |   |   ...
 |   +- testN-.../...
 |
 +- normal/               # files for testing the ordinary execution
 |   +- test0-.../
 |   |   +- input.txt     # test-code
 |   |   +- output.log    # expected output
 |   |   ...
 |   +- testN-.../...
 |
 +- nd/                   # files for testing the non-deterministic execution
 |   +- test0-.../
 |   |   +- input.txt     # test-code
 |   |   +- output.log    # expected output
 |   |   ...
 |   +- testN-.../...
 |
 + ...	
```



