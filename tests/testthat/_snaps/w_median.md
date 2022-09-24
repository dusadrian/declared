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
      w_median(x)
    Output
      [1] 3

---

    Code
      xc
    Output
        [1] 1 3 1 3 2 1 1 1 2 1 1 2 0 4 4 2 3 3 3 3 4 1 4 2 3 4 3 0 4 4 0 2 2 4 0 4 0
       [38] 2 3 4 4 3 0 3 0 2 3 0 2 2 0 4 2 2 2 0 1 4 2 4 3 2 4 3 4 1 0 1 4 4 0 2 1 4
       [75] 4 2 1 0 1 4 0 3 4 0 4 4 2 3 3 2 1 2 2 3 2 3 4 4 3 1 3 3 2 0 0 2 3 4 0 3 2
      [112] 0 2 4 4 0 3 3 0 3 2 2 0 1 0 3 0 4 1 1 3 0 1 3 1 3 1 1 1 1 1 3 0 2 1 2 3 4
      [149] 2 2 2 2 2 4 0 1 1 0 3 1 4 2 3 1 2 4 3 0 1 3 2 2 0 3 3 3 3 2 3 0 2 1 1 3 1
      [186] 1 4 3 4 0 0 0 4 4 4 2 4 2 2 1 4 0 4 1 1 1 3 1 2 0 4 2 4 2 2

---

    Code
      w_median(xc)
    Output
      [1] 2

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
      w_median(hx)
    Output
      [1] 3

---

    Code
      using(DF, w_median(Age))
    Output
      [1] 55

---

    Code
      using(DF, w_median(Age, wt = fweight))
    Output
      [1] 55

---

    Code
      using(DF, w_median(Age), split.by = Gender)
    Output
      
              w_median
        Males    52   
      Females    57   
      

---

    Code
      using(DF, w_median(Age, wt = fweight), split.by = Gender)
    Output
      
              w_median
        Males  52.000 
      Females  57.465 
      

---

    Code
      using(DF, w_median(Age), split.by = Gender & Children)
    Output
      
                 w_median
        Males, 0   59.0  
        Males, 1   43.0  
        Males, 2   59.0  
        Males, 3   50.0  
        Males, 4   52.0  
        Males, 5   55.0  
      Females, 0   65.0  
      Females, 1   56.5  
      Females, 2   50.0  
      Females, 3   65.0  
      Females, 4   58.0  
      Females, 5   48.0  
      

---

    Code
      using(DF, w_median(Age, wt = fweight), split.by = Gender & Children)
    Output
      
                 w_median
        Males, 0  59.921 
        Males, 1  43.508 
        Males, 2  60.100 
        Males, 3  50.592 
        Males, 4  54.888 
        Males, 5  57.031 
      Females, 0  68.585 
      Females, 1  55.310 
      Females, 2  50.000 
      Females, 3  70.483 
      Females, 4  58.262 
      Females, 5  52.525 
      

