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

---

    Code
      as.haven(x)
    Output
      <labelled_spss<double>[6]>
      [1]  1  2  3  4  5 -1
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      as.haven(1:5, interactive = FALSE)
    Output
      [1] 1 2 3 4 5

---

    Code
      cx
    Output
      <declared<character>[20]>
       [1]    b     c  NA(z)    b  NA(z) NA(z)    b     a     c  NA(z) NA(z)    b 
      [13]    b     c     b     c     b     b     a  NA(z)
      Missing values: z
      
      Labels:
       value    label
           a     Left
           b   Middle
           c    Right
           z Apolitic

---

    Code
      as.haven(cx)
    Output
      <labelled_spss<character>[20]>
       [1] b c z b z z b a c z z b b c b c b b a z
      Missing values: z
      
      Labels:
       value    label
           a     Left
           b   Middle
           c    Right
           z Apolitic

