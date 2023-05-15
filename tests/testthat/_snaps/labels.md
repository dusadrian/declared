# tests have the same output

    Code
      x
    Output
      <declared<integer>[6]> Variable label
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      label(x)
    Output
      [1] "Variable label"

---

    Code
      attr(x, "label")
    Output
      [1] "Variable label"

---

    Code
      labels(x)
    Output
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      labels(x, prefixed = TRUE)
    Output
       value    label
           1 [1] Good
           5  [5] Bad
          -1  [-1] DK

---

    Code
      hx
    Output
      <labelled_spss<double>[6]>: Variable label
      [1]  1  2  3  4  5 -1
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      labels(hx)
    Output
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      dfd
    Output
             x hx
      1      1  1
      2      2  2
      3      3  3
      4      4  4
      5      5  5
      6 NA(-1) -1

---

    Code
      labels(dfd)
    Output
      $x
       value label
           1  Good
           5   Bad
          -1    DK
      
      $hx
       value label
           1  Good
           5   Bad
          -1    DK
      

---

    Code
      labels(dfd, prefixed = TRUE)
    Output
      $x
       value    label
           1 [1] Good
           5  [5] Bad
          -1  [-1] DK
      
      $hx
       value    label
           1 [1] Good
           5  [5] Bad
          -1  [-1] DK
      

