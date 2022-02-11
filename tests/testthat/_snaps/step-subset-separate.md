# checks type of `into` and `sep`

    Code
      separate(dt, x, "x", FALSE)
    Condition
      Error in `separate()`:
      ! `sep` must be a character vector.

---

    Code
      separate(dt, x, FALSE)
    Condition
      Error in `separate()`:
      ! `into` must be a character vector.

