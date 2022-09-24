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
      w_quantile(x)
    Output
      
        0%   25%  50%  75% 100%
         1    2    3    4    5 
      

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
      w_quantile(hx)
    Output
      
        0%   25%  50%  75% 100%
         1    2    3    4    5 
      

---

    Code
      using(DF, w_quantile(Age))
    Output
      
        0%   25%  50%  75% 100%
        18   38   55  70.5  90 
      

---

    Code
      using(DF, w_quantile(Age, wt = fweight))
    Output
      
        0%   25%  50%  75% 100%
        18   38   55   71   90 
      

---

    Code
      using(DF, w_quantile(Age), split.by = Gender)
    Output
      
               0%   25%  50%  75% 100%
        Males  18   37   52  70.5  90 
      Females  18   38   57  70.5  90 
      

---

    Code
      using(DF, w_quantile(Age, wt = fweight), split.by = Gender)
    Output
      
                0%     25%    50%    75%   100% 
        Males   19   37.517 52.000   71     90  
      Females   20   39.465 57.465   72     90  
      

