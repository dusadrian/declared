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
      w_sd(x)
    Output
      [1] 1.581139

---

    Code
      using(DF, w_sd(Age))
    Output
      [1] 20.19372

---

    Code
      using(DF, w_sd(Age, wt = fweight))
    Output
      [1] 20.09693

---

    Code
      using(DF, w_sd(Age), split.by = Gender)
    Output
      
               w_sd 
        Males 20.235
      Females 20.245
      

---

    Code
      using(DF, w_sd(Age, wt = fweight), split.by = Gender)
    Output
      
               w_sd 
        Males 20.354
      Females 19.925
      

---

    Code
      using(DF, w_sd(Age), split.by = Gender & Children)
    Output
      
                  w_sd 
        Males, 0 23.114
        Males, 1 20.134
        Males, 2 19.628
        Males, 3 21.098
        Males, 4 18.476
        Males, 5 19.588
      Females, 0 26.102
      Females, 1 18.143
      Females, 2 20.469
      Females, 3 23.093
      Females, 4 15.243
      Females, 5 18.426
      

---

    Code
      using(DF, w_sd(Age, wt = fweight), split.by = Gender & Children)
    Output
      
                  w_sd 
        Males, 0 23.673
        Males, 1 19.889
        Males, 2 19.470
        Males, 3 20.457
        Males, 4 19.240
        Males, 5 20.235
      Females, 0 25.925
      Females, 1 17.313
      Females, 2 19.209
      Females, 3 22.332
      Females, 4 15.887
      Females, 5 18.372
      

