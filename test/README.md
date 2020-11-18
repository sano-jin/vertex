# Test
Files/directories for testing

## Usage 

At the root directory of this project (the parent direcory),

```bash
ghc src/Main.hs  # compile Main.hs
test.sh
```

## Directory structure


```bash
test/
  ├ errors/              # files for testing error-handlings
  │  ├ test1/
  │　│  ├ input.txt    # test-code
  │　│  └ output.log   # expected output
  │  │ ...
  │　└ testN/...
  │
  ├ normal/              # files for testing normal-cases
  │  ├ test1/
  │  │  ├ input.txt    # test-code
  │  │  └ output.log   # expected output
  │  │ ...
  │  └ testN/...
  │
  └ old/                 # old files
```

