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

