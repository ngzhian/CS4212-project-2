# CS4212 Project 2 [![Build Status](https://travis-ci.org/burnflare/CS4212-project-2.svg?branch=master)](https://travis-ci.org/burnflare/CS4212-project-2)

Compiler for mini-go!

## Compile

Uses make to build the compiler, which will output a binary named `main` in the current directory.

```
make
```

## Running

```
./main testcases/vm/pass/fib-recur
```

## Test cases

Note, the test script isn't updated anymore because our build file has changed.

Some test cases are in the testcases folder.
To run the tests:

```
make # build files
make test # for both parse test and type checks
```

## References

Official project description: http://www.home.hs-karlsruhe.de/~suma0002/CS4212/project.html

Some tips: http://www.home.hs-karlsruhe.de/~suma0002/CS4212/project-tips.html

Helpful external guide: http://www.cse.chalmers.se/edu/year/2015/course/DAT150/lectures/proglang-07.html
