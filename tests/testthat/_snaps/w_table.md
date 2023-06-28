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
      w_table(x)
    Output
      
           fre    rel   per   vld   cpd
           ----------------------------
      Good   1  0.167  16.7  20.0  20.0 
         2   1  0.167  16.7  20.0  40.0 
         3   1  0.167  16.7  20.0  60.0 
         4   1  0.167  16.7  20.0  80.0 
       Bad   1  0.167  16.7  20.0 100.0 
      ----
        DK   1  0.167  16.7 
           ----------------------------
             6  1.000 100.0
      

---

    Code
      w_table(x, values = TRUE)
    Output
      
              fre    rel   per   vld   cpd
              ----------------------------
      Good  1   1  0.167  16.7  20.0  20.0 
         2  2   1  0.167  16.7  20.0  40.0 
         3  3   1  0.167  16.7  20.0  60.0 
         4  4   1  0.167  16.7  20.0  80.0 
       Bad  5   1  0.167  16.7  20.0 100.0 
        -----
        DK -1   1  0.167  16.7 
              ----------------------------
                6  1.000 100.0
      

---

    Code
      fx
    Output
        [1] e a d d e a d d a b b d d c e c b d b a a c d c e b e b c b c b b b c e a
       [38] c e c a e d a e e c e d b a e c a b b c d c b b c e c b d c c e b a c a d
       [75] e b a d c e d c d a c e a c e d b b c e e c e a a e e a e a b b d e d d c
      [112] b a e d b a d d a d d c b a d a b d e d c b b b d b b a e b d c e e d c c
      [149] e d d a c d b e d c e b a e e c a c c b a b c a a d a d e b e b b d d e b
      [186] e d b d c a b a e a a d a c e c b c d e b e c c c d e e e b
      Levels: a b c d e

---

    Code
      w_table(fx)
    Output
      
        fre    rel   per   cpd
        ----------------------
      a  37  0.172  17.2  17.2 
      b  45  0.209  20.9  38.1 
      c  43  0.200  20.0  58.1 
      d  44  0.205  20.5  78.6 
      e  46  0.214  21.4 100.0 
        ----------------------
        215  1.000 100.0
      

---

    Code
      x2
    Output
      <declared<integer>[215]>
        [1] -91   3   6   3   7 -91   5   1   3   2   7 -91   7   4   3   3   7   2
       [19]   6   5   4   1   6   4 -91   3   3   3   6   1   3   5   4   4   3   5
       [37]   7 -91   4   1   4   1   1   7   5   5   1   2   1   7   7   6   1   5
       [55]   1   4   4   7   3   7   5   5   2   4   5   3 -91   7   2   7   3   2
       [73]   5   6   1   4   7   4   1   2   1   3   2   4   2   7   6   5   6   4
       [91]   4   2   6 -91   3   3   3   7   3   3   6   7   5   6   4   1   2   6
      [109]   3   1   1   4   4   6   2   4   5   7 -91   5 -91   3   4   2   4   7
      [127]   5   7 -91   1   3   2   7   4   5   6   6   7   3   7   6   2   1   7
      [145]   6   4 -91   4   7   7   3   4   2   2   4   2   4 -91   1   6   2   4
      [163]   4   3   4   5   1   4   7   7   3   5 -91   6   3   7   2   6   4   4
      [181]   5 -91   7   7   7   5   4   3   6   2   1   4   3   5   2   7   1   2
      [199]   5   2 -91   7   2   3   3   3   1   7   7   4   4   5   1 -91   1
      
      Labels:
       value label
           1  Good
           7   Bad

---

    Code
      w_table(x2)
    Output
      
           fre    rel   per   cpd
           ----------------------
       -91  16  0.074   7.4   7.4 
      Good  25  0.116  11.6  19.1 
         2  25  0.116  11.6  30.7 
         3  32  0.149  14.9  45.6 
         4  37  0.172  17.2  62.8 
         5  24  0.112  11.2  74.0 
         6  21  0.098   9.8  83.7 
       Bad  35  0.163  16.3 100.0 
           ----------------------
           215  1.000 100.0
      

---

    Code
      x4
    Output
      <declared<integer>[215]>
        [1]       5       3       4       7       6      NA       5      NA      NA
       [10]       3       4       7       6       3      NA      NA       1       6
       [19] NA(-91) NA(-91)      NA       7       7       2 NA(-91)       4 NA(-91)
       [28]       2       3       2       1 NA(-91) NA(-91) NA(-91)      NA NA(-91)
       [37] NA(-91)       5       5       7       4       2       1       7      NA
       [46]       6       2      NA       2       2       2      NA NA(-91)       6
       [55]       4      NA       4       1       4       1       7       6 NA(-91)
       [64]       2       5      NA      NA       4       7 NA(-91)       2       5
       [73]       2      NA       1       6      NA       6       2       5       3
       [82] NA(-91)       5       5       6       2       5       2       4       2
       [91]      NA      NA       1      NA NA(-91)       3       3       2 NA(-91)
      [100]       6       1       4       1 NA(-91)       4 NA(-91) NA(-91)       7
      [109]       5       3       2       4       6       2 NA(-91)       6       4
      [118]       6       1       7       2       4       5       5       3       6
      [127]       4       1       6       3       4       6       7       4       3
      [136]       5       2       7       1       5 NA(-91)      NA       2       4
      [145]       2      NA       3       6       1       3       4      NA       3
      [154]       6       3       4       3       3 NA(-91)       4       7       5
      [163]       4      NA       4       3       2 NA(-91)       3       2       3
      [172]       4      NA       4       5       2       7       2       4 NA(-91)
      [181]       1       6       2       1       5 NA(-91)      NA       3       2
      [190]       1       6       1      NA       6       1       3       2       3
      [199]       1       5       3       3       2       6       2       3       2
      [208]       7       2       1       5       5       5       4       3
      Missing values: -91
      
      Labels:
       value      label
           1       Good
           7        Bad
         -91 Don't know

---

    Code
      w_table(x4)
    Output
      
                 fre    rel   per   vld   cpd
                 ----------------------------
            Good  20  0.093   9.3  12.0  12.0 
               2  33  0.153  15.3  19.9  31.9 
               3  27  0.126  12.6  16.3  48.2 
               4  27  0.126  12.6  16.3  64.5 
               5  22  0.102  10.2  13.3  77.7 
               6  22  0.102  10.2  13.3  91.0 
             Bad  15  0.070   7.0   9.0 100.0 
           -----
      Don't know  24  0.112  11.2 
              NA  25  0.116  11.6 
                 ----------------------------
                 215  1.000 100.0
      

---

    Code
      using(DF, w_table(Gender))
    Output
      
              fre    rel   per   cpd
              ----------------------
        Males 119  0.553  55.3  55.3 
      Females  96  0.447  44.7 100.0 
              ----------------------
              215  1.000 100.0
      

---

    Code
      using(DF, w_table(Gender, wt = fweight))
    Output
      
              fre    rel   per   cpd
              ----------------------
        Males 107  0.498  49.8  49.8 
      Females 108  0.502  50.2 100.0 
              ----------------------
              215  1.000 100.0
      

---

    Code
      using(DF, w_table(Gender), split.by = Area)
    Output
      
      Rural 
      -----
              fre    rel   per   cpd
              ----------------------
        Males  50  0.562  56.2  56.2 
      Females  39  0.438  43.8 100.0 
              ----------------------
               89  1.000 100.0
      
      Urban 
      -----
              fre    rel   per   cpd
              ----------------------
        Males  69  0.548  54.8  54.8 
      Females  57  0.452  45.2 100.0 
              ----------------------
              126  1.000 100.0
      

---

    Code
      using(DF, w_table(Gender, wt = fweight), split.by = Area)
    Output
      
      Rural 
      -----
              fre    rel   per   cpd
              ----------------------
        Males  57  0.500  50.0  50.0 
      Females  57  0.500  50.0 100.0 
              ----------------------
              114  1.000 100.0
      
      Urban 
      -----
              fre    rel   per   cpd
              ----------------------
        Males  50  0.495  49.5  49.5 
      Females  51  0.505  50.5 100.0 
              ----------------------
              101  1.000 100.0
      

---

    Code
      using(DF, w_table(Gender, Area))
    Output
      
              Rural Urban Total
        Males   50    69   119 
      Females   39    57    96 
        Total   89   126   215 
      

---

    Code
      using(DF, w_table(Gender, Area, wt = fweight))
    Output
      
              Rural Urban Total
        Males   57    50   107 
      Females   57    51   108 
        Total  114   101   215 
      

---

    Code
      using(DF, w_table(Gender, vlabel = TRUE))
    Output
      Gender: Respodent's gender 
      
              fre    rel   per   cpd
              ----------------------
        Males 119  0.553  55.3  55.3 
      Females  96  0.447  44.7 100.0 
              ----------------------
              215  1.000 100.0
      

---

    Code
      using(DF, w_table(Gender, Area, vlabel = TRUE))
    Output
      Gender: Respodent's gender 
        Area: Respodent's area 
      
              Rural Urban Total
        Males   50    69   119 
      Females   39    57    96 
        Total   89   126   215 
      

