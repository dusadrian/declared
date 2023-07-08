# tests have the same output

    Code
      dfd
    Output
             x hx
      1      1  1
      2      2  2
      3      3  3
      4      4  4
      5      5  5
      6 NA(-1) -1

---

    Code
      as.haven(dfd)
    Output
      # A data frame: 6 x 2
        x            hx          
        <int+lbl>    <int+lbl>   
      1  1 [Good]     1 [Good]   
      2  2            2          
      3  3            3          
      4  4            4          
      5  5 [Bad]      5 [Bad]    
      6 -1 (NA) [DK] -1 (NA) [DK]

---

    Code
      as.haven(dfd, interactive = TRUE)
    Output
      # A data frame: 6 x 2
        x            hx          
        <int+lbl>    <int+lbl>   
      1  1 [Good]     1 [Good]   
      2  2            2          
      3  3            3          
      4  4            4          
      5  5 [Bad]      5 [Bad]    
      6 -1 (NA) [DK] -1 (NA) [DK]

---

    Code
      as.haven(dfd, only_declared = FALSE)
    Output
      # A data frame: 6 x 2
        x            hx          
        <int+lbl>    <int+lbl>   
      1  1 [Good]     1 [Good]   
      2  2            2          
      3  3            3          
      4  4            4          
      5  5 [Bad]      5 [Bad]    
      6 -1 (NA) [DK] -1 (NA) [DK]

---

    Code
      as.haven(dfd, only_declared = FALSE, interactive = TRUE)
    Output
      # A data frame: 6 x 2
        x            hx          
        <int+lbl>    <int+lbl>   
      1  1 [Good]     1 [Good]   
      2  2            2          
      3  3            3          
      4  4            4          
      5  5 [Bad]      5 [Bad]    
      6 -1 (NA) [DK] -1 (NA) [DK]

