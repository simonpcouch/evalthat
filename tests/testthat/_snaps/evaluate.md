# eval_files fails informatively with nonexistent path

    Code
      eval_files("doesntexist")
    Condition
      Error in `eval_files()`:
      ! `path` does not exist.

---

    Code
      eval_files("./notthere")
    Condition
      Error in `eval_files()`:
      ! `path` does not exist.

# `evaluate()` errors informatively with non-function eval

    Code
      results <- evaluate(test_path("reporters", "no-function-returned.R"), repeats = 2)
    Output
        PASS FAIL | Context
      
      -    1    0 | no-function-returned []                                           
    Condition
      Error in `evaluate()`:
      ! `path` must return a function when sourced.

