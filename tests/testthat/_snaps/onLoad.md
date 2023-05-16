# tests have the same output

    Code
      x
    Output
      <declared<integer>[6]>
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      Missing range:  [-5, -1]
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      fx
    Output
      [1] Good 2    3    4    Bad  <NA>
      Levels: Good 2 3 4 Bad

---

    Code
      as.factor(fx)
    Output
      [1] Good 2    3    4    Bad  <NA>
      Levels: Good 2 3 4 Bad

---

    Code
      as.factor(c(a = 1, b = 2, c = 2, d = 4, e = 5, f = 6))
    Output
      a b c d e f 
      1 2 2 4 5 6 
      Levels: 1 2 4 5 6

---

    Code
      as.factor(letters[1:6])
    Output
      [1] a b c d e f
      Levels: a b c d e f

---

    Code
      as.factor(x)
    Output
      [1] Good 2    3    4    Bad  <NA>
      Levels: Good 2 3 4 Bad

---

    Code
      as.factor(x, drop_na = FALSE)
    Output
      [1] Good 2    3    4    Bad  DK  
      Levels: Good 2 3 4 Bad DK

---

    Code
      as.factor(x, drop_na = FALSE, nolabels = TRUE)
    Output
      [1] Good <NA> <NA> <NA> Bad  DK  
      Levels: Good Bad DK

---

    Code
      sd(x)
    Output
      [1] 1.581139

---

    Code
      var(x)
    Output
      [1] 2.5

---

    Code
      var(as.data.frame(x), na.rm = TRUE)
    Output
          x
      x 2.5

---

    Code
      fivenum(x)
    Output
      [1] 1 2 3 4 5

---

    Code
      fivenum(drop_na(x), na.rm = TRUE)
    Output
      [1] 1 2 3 4 5

---

    Code
      order(x)
    Output
      [1] 1 2 3 4 5 6

---

    Code
      as.factor(x, levels = "values")
    Output
      [1] 1    2    3    4    5    <NA>
      Levels: 1 2 3 4 5

---

    Code
      is.element(x, labels(x))
    Output
      [1]  TRUE FALSE FALSE FALSE  TRUE  TRUE

---

    Code
      x %in% labels(x)
    Output
      [1]  TRUE FALSE FALSE FALSE  TRUE  TRUE

---

    Code
      match(x, labels(x))
    Output
      [1]  1 NA NA NA  2  3

---

    Code
      x2
    Output
      <declared<integer>[7]>
      [1] NA(-1)      1      2      3      4      5 NA(-2)
      Missing range:  [-5, -1]
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK
          -2 Other

---

    Code
      match(x, x2)
    Output
      [1] 2 3 4 5 6 1

---

    Code
      match(x2, x)
    Output
      [1]  6  1  2  3  4  5 NA

