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
      w_fivenum(x)
    Output
      
       Min  Q1  Q2  Q3 Max
        1   2   3   4   5 
      

