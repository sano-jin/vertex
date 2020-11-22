# Test
Files/directories for testing

## Usage 

Just execute the `test.sh`.
It will compile the program and test it with the expected result.

```bash
./test.sh
```


## Directory structure

```bash
test/
 +- errors/            # files for testing error-handlings
 |   +- test1/
 |   |   +- input.txt  # test-code
 |   |   +- output.log # expected output
 |   |   ...
 |   +- testN/...
 |
 +- normal/            # files for testing normal-cases
 |   +- test1/
 |   |   +- input.txt  # test-code
 |   |   +- output.log # expected output
 |   |   ...
 |   +-- testN/...
 |
 +- old/               # old files
```



```bash
test/
  ├ errors/            # files for testing error-handlings
  │  ├ test1/
  │  │  ├ input.txt    # test-code
  │  │  └ output.log   # expected output
  │  │ ...
  │  └ testN/...
  │
  ├ normal/            # files for testing normal-cases
  │  ├ test1/
  │  │  ├ input.txt    # test-code
  │  │  └ output.log   # expected output
  │  │ ...
  │  └ testN/...
  │
  └ old/               # old files
```

