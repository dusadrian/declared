# tests have the same output

    Code
      x
    Output
      <declared<integer>[7]> Test variable
      [1] NA(-2)      1      2      3      4      5 NA(-1)
      Missing values: -1, -2
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      measurement(x)
    Output
      [1] "categorical, ordinal"

---

    Code
      x2
    Output
      <declared<integer>[7]> Test variable
      [1] NA(-2)      1      2      3      4      5 NA(-1)
      Missing values: -1, -2
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      measurement(x2)
    Output
      [1] "Unspecified, but likely categorical"

---

    Code
      numx
    Output
      <declared<integer>[20]> Respondent's age
       [1] 57 56 55 47 18 89 29 20 26 31 30 81 37 33 79 77 49 42 53 55
      Missing values: -91
      
      Labels:
       value     label
         -91 No answer

---

    Code
      measurement(numx)
    Output
      [1] "Unspecified, but likely quantitative"

---

    Code
      numx2
    Output
      <declared<integer>[20]> Respondent's age
       [1] 57 56 55 47 18 89 29 20 26 31 30 81 37 33 79 77 49 42 53 55
      Missing values: -91
      
      Labels:
       value     label
         -91 No answer

---

    Code
      measurement(numx2)
    Output
      [1] "quantitative, discrete"

