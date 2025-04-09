# tests have the same output

    Code
      x
    Output
      <declared<numeric>[7]>
      [1]      2      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      wmode(x)
    Output
      [1] 2

---

    Code
      hx
    Output
      <labelled_spss<double>[7]>
      [1]  2  1  2  3  4  5 -1
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      wmode(hx)
    Output
      [1] 2

