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
      labels(x)
    Output
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      attr(x, "na_index")
    Output
      -1 
       6 

---

    Code
      fx
    Output
      [1] Good 2    3    4    Bad  DK  
      Levels: Good 2 3 4 Bad DK

---

    Code
      labels(declared(fx, na_values = 6))
    Output
       value label
           1  Good
           2     2
           3     3
           4     4
           5   Bad
           6    DK

---

    Code
      labels(declared(fx, na_values = "DK"))
    Output
       value label
           1  Good
           2     2
           3     3
           4     4
           5   Bad
           6    DK

---

    Code
      labels(declared(fx, na_values = 6, llevels = TRUE))
    Output
       value label
           1  Good
           5   Bad
           6    DK

---

    Code
      xc
    Output
      <declared<character>[6]>
      [1]     1      2      3      4      5  NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
           a    DK

---

    Code
      is.character(xc)
    Output
      [1] TRUE

