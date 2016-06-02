# Test case generator for pretty combinators

https://github.com/haskell/pretty/issues/32#issuecomment-223213838

[![Build Status](https://travis-ci.org/jwaldmann/pretty-test.svg)](http://travis-ci.org/jwaldmann/pretty-test)

Results:

* pretty does seem to have some quadratic behaviour 
  (as a user, I am shocked; after reading the source, ndm would have been shocked if it was less than quadratic)
* it seems it's not getting worse that that. I enumerated small contexts and found none that shows drastically different behaviour.
* for these tests, it does not matter that we render to plain `Strings` which has inefficient `(++)`. 
  I used a different render function that just computes a number: results are quite exactly the same.

Conclusion:

* use this for testing:
```
length $ render $ iterate ( \ hole ->  sep [text  "l", cat [hole], text  "l"] ) (text  "l") !! 1000
```

NB: the `Series` module might be a nice alternative to `smallcheck`.

