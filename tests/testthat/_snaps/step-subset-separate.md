# checks type of `into` and `sep`

    Code
      separate(dt, x, "x", FALSE)
    Condition
      Error in `separate.dtplyr_step()`:
      ! is.character(sep) is not TRUE

---

    Code
      separate(dt, x, FALSE)
    Condition
      Error in `separate.dtplyr_step()`:
      ! is.character(into) is not TRUE

