# tests have the same output

    Code
      x
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
      vctrs::vec_ptype_abbr(x)
    Output
      [1] "int+lbl"

---

    Code
      vctrs::vec_ptype_full(x)
    Output
      [1] "declared<integer>"

