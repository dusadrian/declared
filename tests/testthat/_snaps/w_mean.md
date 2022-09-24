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
      w_mean(x)
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
      w_mean(xc)
    Output
      [1] 2.093023

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
      w_mean(hx)
    Output
      [1] 3

---

    Code
      using(DF, w_mean(Age))
    Output
      [1] 54.62791

---

    Code
      using(DF, w_mean(Age, wt = fweight))
    Output
      [1] 55.12584

---

    Code
      using(DF, w_mean(Age), split.by = Gender)
    Output
      
              w_mean
        Males 54.403
      Females 54.906
      

---

    Code
      using(DF, w_mean(Age, wt = fweight), split.by = Gender)
    Output
      
              w_mean
        Males 54.713
      Females 55.535
      

---

    Code
      using(DF, w_mean(Age), split.by = Gender & Children)
    Output
      
                 w_mean
        Males, 0 57.391
        Males, 1 48.062
        Males, 2 56.560
        Males, 3 50.600
        Males, 4 54.190
        Males, 5 56.526
      Females, 0 54.833
      Females, 1 55.600
      Females, 2 51.609
      Females, 3 61.909
      Females, 4 56.316
      Females, 5 52.667
      

---

    Code
      using(DF, w_mean(Age, wt = fweight), split.by = Gender & Children)
    Output
      
                 w_mean
        Males, 0 56.859
        Males, 1 48.358
        Males, 2 57.741
        Males, 3 50.892
        Males, 4 54.719
        Males, 5 56.671
      Females, 0 57.081
      Females, 1 54.653
      Females, 2 51.695
      Females, 3 63.214
      Females, 4 56.302
      Females, 5 53.765
      

