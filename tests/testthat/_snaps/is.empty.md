# tests have the same output

    Code
      x
    Output
      <declared<integer>[3]>
      [1]       1       2 NA(-91)
      Missing values: -91
      
      Labels:
       value   label
           1    Good
           2     Bad
         -91 Missing

---

    Code
      is.empty(x)
    Output
      [1] FALSE FALSE FALSE

---

    Code
      is.empty(c(x, NA))
    Output
      [1] FALSE FALSE FALSE  TRUE

