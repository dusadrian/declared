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
      format_declared(x)
    Output
      [1] "     1" "     2" "     3" "     4" "     5" "NA(-1)"

---

    Code
      order_declared(x)
    Output
      [1] 1 2 3 4 5

---

    Code
      order_declared(x, na.last = TRUE)
    Output
      [1] 1 2 3 4 5 6

---

    Code
      order_declared(x, na.last = FALSE)
    Output
      [1] 6 1 2 3 4 5

---

    Code
      order_declared(c(x, NA), na.last = FALSE, empty.last = TRUE)
    Output
      [1] 6 7 1 2 3 4 5

---

    Code
      order_declared(c(x, NA), na.last = TRUE, empty.last = FALSE)
    Output
      [1] 1 2 3 4 5 7 6

---

    Code
      likely_type(1:5)
    Output
      [1] "<integer>"

---

    Code
      likely_type(c(1:5, 2))
    Output
      [1] "<numeric>"

---

    Code
      likely_type("a")
    Output
      [1] "<character>"

---

    Code
      likely_type(as.complex(1))
    Output
      NULL

---

    Code
      check_measurement("nominal")
    Output
      [1] "categorical, nominal"

---

    Code
      check_measurement("ordinal")
    Output
      [1] "categorical, ordinal"

---

    Code
      check_measurement("qualitative")
    Output
      [1] "categorical"

---

    Code
      check_measurement("interval")
    Output
      [1] "quantitative, interval"

---

    Code
      check_measurement("ratio")
    Output
      [1] "quantitative, ratio"

---

    Code
      check_measurement("discrete")
    Output
      [1] "quantitative, discrete"

---

    Code
      check_measurement("continuous")
    Output
      [1] "quantitative, continuous"

---

    Code
      check_measurement("metric")
    Output
      [1] "quantitative"

---

    Code
      check_measurement("numeric")
    Output
      [1] "quantitative"

---

    Code
      check_measurement("interval, discrete")
    Output
      [1] "quantitative, interval, discrete"

---

    Code
      likely_measurement(x)
    Output
      [1] "categorical"

---

    Code
      y
    Output
      <declared<integer>[6]>
      [1]  NA(1)      2      3      4  NA(5) NA(-1)
      Missing values: -1, 1, 5
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      likely_measurement(y)
    Output
      [1] "quantitative"

---

    Code
      z
    Output
      <declared<character>[6]>
      [1] NA(1)    2     3     4  NA(5)    a 
      Missing values: -1, 1, 5
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      likely_measurement(z)
    Output
      [1] ""

---

    Code
      cx1
    Output
      <declared<character>[20]>
       [1]       Middle       Right  NA(Apolitic)       Middle NA(Apolitic)
       [6] NA(Apolitic)       Middle       Left         Right  NA(Apolitic)
      [11] NA(Apolitic)       Middle       Middle       Right        Middle
      [16]       Right        Middle       Middle       Left   NA(Apolitic)
      Missing values: Apolitic

---

    Code
      likely_measurement(cx1)
    Output
      [1] ""

---

    Code
      cx2
    Output
      <declared<character>[20]>
       [1] NA(z)    c     b     b     a  NA(z) NA(z) NA(z)    b  NA(z)    c     a 
      [13]    b     a  NA(z) NA(z)    b     c  NA(z) NA(z)
      Missing values: z
      
      Labels:
       value    label
           a     Left
           b   Middle
           c    Right
           z Apolitic

---

    Code
      likely_measurement(cx2)
    Output
      [1] "categorical"

---

    Code
      names_values(x)
    Output
      Good    2    3    4  Bad   DK 
         1    2    3    4    5   -1 
      attr(,"missing")
      [1] -1

---

    Code
      names_values(x, drop_na = TRUE)
    Output
      Good    2    3    4  Bad 
         1    2    3    4    5 
      attr(,"missing")
      numeric(0)

---

    Code
      xr
    Output
      <declared<integer>[6]>
      [1]      1      2      3      4      5 NA(-1)
      Missing range:  [-5, -1]
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      names_values(xr)
    Output
      Good    2    3    4  Bad   DK 
         1    2    3    4    5   -1 
      attr(,"missing")
      [1] -1

---

    Code
      declared(1:5, labels = c(A = 1))
    Output
      <declared<integer>[5]>
      [1] 1 2 3 4 5
      
      Labels:
       value label
           1     A

---

    Code
      declared(1:10, labels = c(A = 1), na_values = 9, na_range = c(8, 10))
    Output
      <declared<integer>[10]>
       [1]      1      2      3      4      5      6      7  NA(8)  NA(9) NA(10)
      Missing values: 9
      Missing range:  [8, 10]
      
      Labels:
       value label
           1     A

---

    Code
      declared(1:5, labels = c(A = 1), na_range = c(8, 10))
    Output
      <declared<integer>[5]>
      [1] 1 2 3 4 5
      Missing range:  [8, 10]
      
      Labels:
       value label
           1     A

---

    Code
      possibleNumeric_(rep(NA, 5))
    Output
      [1] FALSE

---

    Code
      possibleNumeric_(rep(TRUE, 5))
    Output
      [1] FALSE

---

    Code
      possibleNumeric_(xr)
    Output
      [1] TRUE

---

    Code
      possibleNumeric_(1:5, each = TRUE)
    Output
      [1] TRUE TRUE TRUE TRUE TRUE

---

    Code
      wholeNumeric_(xr)
    Output
      [1] TRUE

---

    Code
      wholeNumeric_(1:5, each = TRUE)
    Output
      [1] TRUE TRUE TRUE TRUE TRUE

---

    Code
      tryCatchWEM_(order_declared(list(A = 1)))
    Output
      
      $error
      [1] "`x` has to be a vector of class `declared`.\n\n"
      

---

    Code
      tryCatchWEM_(value_labels(x), capture = TRUE)
    Output
      $warning
      [1] "Function value_labels() is deprecated, use labels()\n"
      
      $output
      [1] "Good  Bad   DK " "   1    5   -1 "
      
      $value
      Good  Bad   DK 
         1    5   -1 
      

---

    Code
      tryCatchWEM_(as.declared(1:5, interactive = TRUE), capture = TRUE)
    Output
      $message
      [1] "There is no automatic class method conversion for this type of object.\n"
      
      $output
      [1] "[1] 1 2 3 4 5"
      
      $value
      [1] 1 2 3 4 5
      

---

    Code
      padLeft_("foo", 5)
    Output
      [1] "     foo"

---

    Code
      padRight_("foo", 5)
    Output
      [1] "foo     "

---

    Code
      padBoth_("foo", 5)
    Output
      [1] "   foo  "

---

    Code
      hasTag_(bigtag, "")
    Output
      [1] FALSE

---

    Code
      hasTag_(c(atag, NA), "a")
    Output
      [1]  TRUE FALSE

---

    Code
      getTag_(onetag)
    Output
      [1] 1

---

    Code
      decx
    Output
      [1] 12.00 12.30 12.34

---

    Code
      numdec_(decx)
    Output
      [1] 2

---

    Code
      numdec_(decx, each = TRUE)
    Output
      [1] 0 1 2

---

    Code
      decx2
    Output
      [1] "-.1"    " 2.75 " "12"     "B"      NA       "2e-05"  "100.00"

---

    Code
      numdec_(decx2)
    Output
      [1] 5

---

    Code
      numdec_(decx2, each = TRUE)
    Output
      [1]  1  2  0 NA NA  5  0

---

    Code
      trimstr_(" foo ", what = " ")
    Output
      [1] "foo"

---

    Code
      trimstr_(" foo ", what = "+")
    Output
      [1] " foo "

---

    Code
      hasTag_(c(minustag, NA, atag))
    Output
      [1]  TRUE FALSE  TRUE

---

    Code
      hasTag_(c(bigminustag, atag))
    Output
      [1] TRUE TRUE

---

    Code
      getTag_(c(atag, NA, minustag))
    Output
      [1] "a"  NA   "-1"

---

    Code
      hasTag_(makeTag_("-a"), "-a")
    Output
      [1] TRUE

---

    Code
      hasTag_(makeTag_("-ab"), "-ab")
    Output
      [1] TRUE

