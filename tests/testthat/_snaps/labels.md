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
      Good  Bad   DK 
         1    5   -1 

---

    Code
      labels(x, prefixed = TRUE)
    Output
      [1] Good  [5] Bad  [-1] DK 
             1        5       -1 

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
      Good  Bad   DK 
         1    5   -1 

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
      Good  Bad   DK 
         1    5   -1 
      
      $hx
      Good  Bad   DK 
         1    5   -1 
      

---

    Code
      labels(dfd, prefixed = TRUE)
    Output
      $x
      [1] Good  [5] Bad  [-1] DK 
             1        5       -1 
      
      $hx
      [1] Good  [5] Bad  [-1] DK 
             1        5       -1 
      

