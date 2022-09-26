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
      haven::as_factor(x)
    Output
      [1] Good 2    3    4    Bad  DK  
      Levels: DK Good 2 3 4 Bad

---

    Code
      haven::zap_labels(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      haven::zap_missing(x)
    Output
      <declared<integer>[6]>
      [1]  1  2  3  4  5 NA
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

