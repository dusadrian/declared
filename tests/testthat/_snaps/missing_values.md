# tests have the same output

    Code
      x
    Output
      <declared<integer>[6]>
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -2, -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      missing_values(x)
    Output
      [1] -2 -1

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
      missing_values(dfd)
    Output
      $x
      [1] -2 -1
      
      $hx
      [1] -1
      

