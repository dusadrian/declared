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
      as.factor(x)
    Output
      [1] Good 2    3    4    Bad  <NA>
      Levels: Good 2 3 4 Bad

---

    Code
      as.factor(drop_na(x))
    Output
      [1] Good 2    3    4    Bad  <NA>
      Levels: Good 2 3 4 Bad

---

    Code
      names(labels(x))
    Output
      [1] "Good" "Bad"  "DK"  

---

    Code
      levels(as.factor(undeclare(x)))
    Output
      [1] "DK"   "Good" "2"    "3"    "4"    "Bad" 

---

    Code
      levels(as.factor(x, drop_na = FALSE))
    Output
      [1] "Good" "2"    "3"    "4"    "Bad"  "DK"  

