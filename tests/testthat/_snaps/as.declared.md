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
      fx
    Output
      [1] Good 2    3    4    Bad  DK  
      Levels: Good 2 3 4 Bad DK

---

    Code
      hx
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
      hs
    Output
      <labelled<double>[6]>
      [1]     1     2     3     4     5 NA(a)
      
      Labels:
       value label
           1  Good
           5   Bad
       NA(a)    DK

---

    Code
      as.declared(fx)
    Output
      <declared<integer>[6]>
      [1] 1 2 3 4 5 6
      
      Labels:
       value label
           1  Good
           2     2
           3     3
           4     4
           5   Bad
           6    DK

---

    Code
      as.declared(hx)
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
      as.declared(hs)
    Output
      <declared<integer>[6]>
      [1]     1     2     3     4     5 NA(a)
      Missing values: a
      
      Labels:
       value label
           1  Good
           5   Bad
           a    DK

---

    Code
      dfh
    Output
         A    B
      1  1 Good
      2  2    2
      3  3    3
      4  4    4
      5  5  Bad
      6 -1   DK

---

    Code
      as.declared(dfh)
    Output
             A B
      1      1 1
      2      2 2
      3      3 3
      4      4 4
      5      5 5
      6 NA(-1) 6

---

    Code
      dfd
    Output
             A B C
      1      1 1 1
      2      2 2 2
      3      3 3 3
      4      4 4 4
      5      5 5 5
      6 NA(-1) 6 6

---

    Code
      as.declared(dfd, interactive = TRUE)
    Output
             A B C
      1      1 1 1
      2      2 2 2
      3      3 3 3
      4      4 4 4
      5      5 5 5
      6 NA(-1) 6 6

