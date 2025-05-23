# tests have the same output

    Code
      print(wtable(xl))
    Output
      
               fre    rel   per   vld   cpd
               ----------------------------
        1 Good  27  0.126  12.6  14.5  14.5 
        2       38  0.177  17.7  20.4  34.9 
        3       34  0.158  15.8  18.3  53.2 
        4       34  0.158  15.8  18.3  71.5 
      500 Bad   53  0.247  24.7  28.5 100.0 
      -----
       -1 DK    29  0.135  13.5 
               ----------------------------
               215  1.000 100.0
      

---

    Code
      print(wtable(c(xl, NA)))
    Output
      
               fre    rel   per   vld   cpd
               ----------------------------
        1 Good  27  0.125  12.5  14.5  14.5 
        2       38  0.176  17.6  20.4  34.9 
        3       34  0.157  15.7  18.3  53.2 
        4       34  0.157  15.7  18.3  71.5 
      500 Bad   53  0.245  24.5  28.5 100.0 
      -----
       -1 DK    29  0.134  13.4 
       NA        1  0.005   0.5 
               ----------------------------
               216  1.000 100.0
      

---

    Code
      print(wtable(xl, values = TRUE))
    Output
      
               fre    rel   per   vld   cpd
               ----------------------------
        1 Good  27  0.126  12.6  14.5  14.5 
        2       38  0.177  17.7  20.4  34.9 
        3       34  0.158  15.8  18.3  53.2 
        4       34  0.158  15.8  18.3  71.5 
      500 Bad   53  0.247  24.7  28.5 100.0 
      -----
       -1 DK    29  0.135  13.5 
               ----------------------------
               215  1.000 100.0
      

---

    Code
      print(proportions(wtable(xl)))
    Output
           Good         2         3         4       Bad        DK 
      0.1255814 0.1767442 0.1581395 0.1581395 0.2465116 0.1348837 

---

    Code
      print(wdf)
    Output
      
                1 Rural 2 Urban  Total 
      1 Males      50      69     119  
      2 Females    39      57      96  
        Total      89     126     215  
      

---

    Code
      print(proportions(wdf))
    Output
                  Rural     Urban
      Males   0.2325581 0.3209302
      Females 0.1813953 0.2651163

---

    Code
      print(proportions(wtable(xl, DF$Area)))
    Output
                Rural      Urban
      Good 0.06451613 0.08064516
      2    0.06989247 0.13440860
      3    0.07526882 0.10752688
      4    0.09677419 0.08602151
      Bad  0.10215054 0.18279570

---

    Code
      print(wtable(xl, DF$Area, values = TRUE))
    Output
      
                1 Rural 2 Urban  Total 
        1 Good     12      15      27  
        2 2        13      25      38  
        3 3        14      20      34  
        4 4        18      16      34  
      500 Bad      19      34      53  
          Total    76     110     186  
      

---

    Code
      print(with(DF, wtable(Gender, Area)))
    Output
      
                1 Rural 2 Urban  Total 
      1 Males      50      69     119  
      2 Females    39      57      96  
        Total      89     126     215  
      

---

    Code
      print(with(DF, wtable(Gender, Area, vlabel = TRUE)))
    Output
      Gender: Respodent's gender 
        Area: Respodent's area 
      
                1 Rural 2 Urban  Total 
      1 Males      50      69     119  
      2 Females    39      57      96  
        Total      89     126     215  
      

---

    Code
      print(with(DF, wtable(Gender, values = TRUE)))
    Output
      
                fre    rel   per   cpd
                ----------------------
      1 Males   119  0.553  55.3  55.3 
      2 Females  96  0.447  44.7 100.0 
                ----------------------
                215  1.000 100.0
      

---

    Code
      print(with(DF, wtable(Gender, values = TRUE)), show_values = FALSE)
    Output
      
              fre    rel   per   cpd
              ----------------------
      Males   119  0.553  55.3  55.3 
      Females  96  0.447  44.7 100.0 
              ----------------------
              215  1.000 100.0
      

---

    Code
      print(wtable(c(1:5, NA)))
    Output
      
         fre    rel   per   vld   cpd
         ----------------------------
      1    1  0.167  16.7  20.0  20.0 
      2    1  0.167  16.7  20.0  40.0 
      3    1  0.167  16.7  20.0  60.0 
      4    1  0.167  16.7  20.0  80.0 
      5    1  0.167  16.7  20.0 100.0 
      --
      NA   1  0.167  16.7 
         ----------------------------
           6  1.000 100.0
      

---

    Code
      cat("wtable(1:101) # for the output below:")
    Output
      wtable(1:101) # for the output below:

---

    It looks like a lot of categories. If you really want to print
           it, use:
           print(x, force = TRUE)
    
    

---

    Code
      print(w_summary(xl))
    Output
      
         Min.  1st Qu.  Median   Mean  3rd Qu.   Max. 
          1       2       3    144.306   500     500  
      

---

    Code
      print(labels(xl))
    Output
       value label
           1  Good
         500   Bad
          -1    DK

---

    Code
      print(labels(xl, print_as_df = FALSE))
    Output
      Good  Bad   DK 
         1  500   -1 

# very many labels are truncated at print

    Code
      print(xlarge)
    Output
      <declared<integer>[25]>
       [1]  1  6  2  9  4  3  3  6  2  9  8  9  5  8  4  5 10  9  5  8  7  9  6  6  7
      
      Labels:
       value label
           1    A1
           2    A2
           3    A3
           4    A4
           5    A5
           6    A6
           7    A7
      # ... plus 3 more labels, use labels() to print them all.

# wtable works with a variable having all values NA

    Code
      print(tstable)
    Output
      
         fre    rel   per   vld   cpd
         ----------------------------
      --
      NA   5  1.000 100.0 
         ----------------------------
           5  1.000 100.0
      

