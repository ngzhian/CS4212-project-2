# CS4212-project-2
Compiler for mini-go!

## Notes
How to disambiguate vars and name if we have a single-letter?
- Lexer reads single letter and must decide if it is a vars or name

## Test cases
Some test cases are in the testcases folder.
To run the tests:

```
make # build files
make test # for both parse test and type checks
make test-parser-only # for parser only
make test-typecheck-only # for typecheck only
```

## References
Official project description: http://www.home.hs-karlsruhe.de/~suma0002/CS4212/project.html
Some tips: http://www.home.hs-karlsruhe.de/~suma0002/CS4212/project-tips.html
Helpful external guide: http://www.cse.chalmers.se/edu/year/2015/course/DAT150/lectures/proglang-07.html
