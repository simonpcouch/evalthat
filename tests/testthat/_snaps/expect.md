# expect_loads works

    Code
      expect_loads(x, "dplyr")
    Condition
      Error:
      ! Code does not load the dplyr package.

---

    Code
      expect_loads(x, "tibble")
    Condition
      Error:
      ! Code does not load the tibble package.

