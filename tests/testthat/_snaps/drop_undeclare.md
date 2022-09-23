# tests have the same output

    Code
      drop(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      undeclare(x)
    Output
      <declared<integer>[6]> Variable label
      [1]  1  2  3  4  5 -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      undeclare(x, drop = TRUE)
    Output
      [1]  1  2  3  4  5 -1

---

    Code
      drop(undeclare(x))
    Output
      [1]  1  2  3  4  5 -1

---

    Code
      drop_na(x)
    Output
      <declared<integer>[6]> Variable label
      [1]  1  2  3  4  5 NA
      
      Labels:
       value label
           1  Good
           5   Bad

---

    Code
      drop_na(hx)
    Output
      <labelled_spss<double>[6]>: Variable label
      [1]  1  2  3  4  5 NA
      
      Labels:
       value label
           1  Good
           5   Bad

---

    Code
      drop_na(dfd)
    Output
         x hx
      1  1  1
      2  2  2
      3  3  3
      4  4  4
      5  5  5
      6 NA -1

---

    Code
      drop_na(dfd, drop_labels = TRUE)
    Output
         x hx
      1  1  1
      2  2  2
      3  3  3
      4  4  4
      5  5  5
      6 NA -1

