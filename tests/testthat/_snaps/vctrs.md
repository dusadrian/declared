# tests have the same output

    Code
      x
    Output
      <declared<numeric>[6]>
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      vctrs::vec_ptype_abbr(x)
    Output
      [1] "dbl+lbl"

---

    Code
      vctrs::vec_ptype_full(x)
    Output
      [1] "declared<double>"

---

    Code
      xi
    Output
      <declared<integer>[6]>
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      vctrs::vec_ptype_abbr(xi)
    Output
      [1] "int+lbl"

---

    Code
      vctrs::vec_ptype_full(xi)
    Output
      [1] "declared<integer>"

---

    Code
      xc
    Output
      <declared<numeric>[9]>
      [1]     1     2 NA(a) NA(b) NA(a)     1     2 NA(b) NA(b)
      Missing values: a, b

---

    Code
      vctrs::vec_restore(vctrs::vec_proxy(xc), to = xc)
    Output
      <declared<numeric>[9]>
      [1]     1     2 NA(a) NA(b) NA(a)     1     2 NA(b) NA(b)
      Missing values: a, b

---

    Code
      df
    Output
      # A tibble: 9 x 2
        xc            y
        <chr+lbl> <int>
      1     1         1
      2     2         2
      3 NA(a)         3
      4 NA(b)         4
      5 NA(a)         5
      6     1         6
      7     2         7
      8 NA(b)         8
      9 NA(b)         9

---

    Code
      dplyr::count(df, xc)
    Output
      # A tibble: 4 x 2
        xc            n
        <chr+lbl> <int>
      1     1         2
      2     2         2
      3 NA(a)         2
      4 NA(b)         3

