# tests have the same output

    Code
      x
    Output
      <declared<integer>[6]> Label for variable x
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      labelled::na_values(x)
    Output
      [1] -1

---

    Code
      labelled::na_range(x)
    Output
      NULL

---

    Code
      labelled::val_labels(x)
    Output
      Good  Bad   DK 
         1    5   -1 

---

    Code
      labelled::val_labels(x, prefixed = TRUE)
    Output
      [1] Good  [5] Bad  [-1] DK 
             1        5       -1 

---

    Code
      labelled::var_label(x)
    Output
      [1] "Label for variable x"

---

    Code
      labelled::drop_unused_value_labels(x)
    Output
      <declared<integer>[6]> Label for variable x
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      labelled::val_label(x, 1)
    Output
      [1] "Good"

---

    Code
      labelled::val_label(x, 1, prefixed = TRUE)
    Output
      [1] "[1] Good"

---

    Code
      labelled::sort_val_labels(x)
    Output
      <declared<integer>[6]> Label for variable x
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
          -1    DK
           1  Good
           5   Bad

---

    Code
      labelled::sort_val_labels(x, according_to = "labels")
    Output
      <declared<integer>[6]> Label for variable x
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1
      
      Labels:
       value label
           5   Bad
          -1    DK
           1  Good

---

    Code
      is.empty(labelled::nolabel_to_na(x))
    Output
      [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE

---

    Code
      is.empty(labelled::val_labels_to_na(x))
    Output
      [1]  TRUE FALSE FALSE FALSE  TRUE FALSE

---

    Code
      labelled::remove_labels(x)
    Output
      <declared<integer>[6]>
      [1]      1      2      3      4      5 NA(-1)
      Missing values: -1

---

    Code
      labelled::remove_user_na(x)
    Output
      <declared<integer>[6]> Label for variable x
      [1]  1  2  3  4  5 NA
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      labelled::to_factor(x)
    Output
      [1] Good 2    3    4    Bad  DK  
      attr(,"label")
      [1] Label for variable x
      Levels: DK Good 2 3 4 Bad

---

    Code
      labelled::to_character(x)
    Output
      [1] "Good" "2"    "3"    "4"    "Bad"  "DK"  
      attr(,"label")
      [1] "Label for variable x"

