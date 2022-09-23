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
      as.character(x)
    Output
      [1] "Good" "2"    "3"    "4"    "Bad"  NA    

---

    Code
      as.character(x, values = TRUE)
    Output
      [1] "1" "2" "3" "4" "5" NA 

---

    Code
      as.character(x, drop_na = FALSE)
    Output
      [1] "Good" "2"    "3"    "4"    "Bad"  "DK"  

---

    Code
      as.character(x, drop_na = FALSE, nolabels = TRUE)
    Output
      [1] "Good" NA     NA     NA     "Bad"  "DK"  

---

    Code
      as.character(x, values = TRUE, drop_na = FALSE)
    Output
      [1] "1"  "2"  "3"  "4"  "5"  "-1"

---

    Code
      as.character(drop_na(x))
    Output
      [1] "Good" "2"    "3"    "4"    "Bad"  NA    

---

    Code
      as.character(undeclare(x))
    Output
      [1] "Good" "2"    "3"    "4"    "Bad"  "DK"  

---

    Code
      as.character(undeclare(x), values = TRUE)
    Output
      [1] "1"  "2"  "3"  "4"  "5"  "-1"

