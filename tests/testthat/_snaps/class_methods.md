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
      head(x)
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
      tail(x)
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
      is.na(x)
    Output
      [1] FALSE FALSE FALSE FALSE FALSE  TRUE

---

    Code
      na.omit(x)
    Output
      <declared<integer>[5]>
      [1] 1 2 3 4 5
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      na.fail(undeclare(x))
    Output
      [1]  1  2  3  4  5 -1
      attr(,"labels")
      Good  Bad   DK 
         1    5   -1 

---

    Code
      na.exclude(x)
    Output
      <declared<integer>[5]>
      [1] 1 2 3 4 5
      Missing values: -1
      
      Labels:
       value label
           1  Good
           5   Bad
          -1    DK

---

    Code
      c(x, 2, -1)
    Output
      <declared<integer>[8]>
      [1]      1      2      3      4      5 NA(-1)      2 NA(-1)
      Missing values: -1
      
      Labels:
       value label
          -1    DK
           1  Good
           5   Bad

---

    Code
      x7
    Output
      <declared<integer>[9]>
      [1] NA(-92)       1       2       3       4       5 NA(-91)      NA NA(-91)
      Missing values: -92, -91

---

    Code
      sort(x7)
    Output
      <declared<integer>[5]>
      [1] 1 2 3 4 5
      Missing values: -92, -91

---

    Code
      sort(x7, na.last = TRUE)
    Output
      <declared<integer>[9]>
      [1]       1       2       3       4       5 NA(-92) NA(-91) NA(-91)      NA
      Missing values: -92, -91

---

    Code
      sort(x7, na.last = FALSE)
    Output
      <declared<integer>[9]>
      [1]      NA NA(-92) NA(-91) NA(-91)       1       2       3       4       5
      Missing values: -92, -91

---

    Code
      x8
    Output
      <declared<integer>[8]>
      [1] NA(-92)       1       2       3       4       5      NA NA(-91)
      Missing values: -92, -91

---

    Code
      duplicated(x8)
    Output
      [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

---

    Code
      is.na(x8)
    Output
      [1]  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE

---

    Code
      unique(x8)
    Output
      <declared<integer>[8]>
      [1] NA(-92)       1       2       3       4       5      NA NA(-91)
      Missing values: -92, -91

---

    Code
      abs(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      sign(x)
    Output
      [1]  1  1  1  1  1 NA

---

    Code
      sqrt(x)
    Output
      [1] 1.000000 1.414214 1.732051 2.000000 2.236068       NA

---

    Code
      floor(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      ceiling(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      trunc(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      round(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      signif(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      exp(x)
    Output
      [1]   2.718282   7.389056  20.085537  54.598150 148.413159         NA

---

    Code
      log(x)
    Output
      [1] 0.0000000 0.6931472 1.0986123 1.3862944 1.6094379        NA

---

    Code
      expm1(x)
    Output
      [1]   1.718282   6.389056  19.085537  53.598150 147.413159         NA

---

    Code
      log1p(x)
    Output
      [1] 0.6931472 1.0986123 1.3862944 1.6094379 1.7917595        NA

---

    Code
      cos(x)
    Output
      [1]  0.5403023 -0.4161468 -0.9899925 -0.6536436  0.2836622         NA

---

    Code
      sin(x)
    Output
      [1]  0.8414710  0.9092974  0.1411200 -0.7568025 -0.9589243         NA

---

    Code
      tan(x)
    Output
      [1]  1.5574077 -2.1850399 -0.1425465  1.1578213 -3.3805150         NA

---

    Code
      cospi(x)
    Output
      [1] -1  1 -1  1 -1 NA

---

    Code
      sinpi(x)
    Output
      [1]  0  0  0  0  0 NA

---

    Code
      tanpi(x)
    Output
      [1]  0  0  0  0  0 NA

---

    Code
      acos(x[1])
    Output
      [1] 0

---

    Code
      asin(x[1])
    Output
      [1] 1.570796

---

    Code
      atan(x[1])
    Output
      [1] 0.7853982

---

    Code
      gamma(x)
    Output
      [1]  1  1  2  6 24 NA

---

    Code
      lgamma(x[1])
    Output
      [1] 0

---

    Code
      digamma(x[1])
    Output
      [1] -0.5772157

---

    Code
      trigamma(x[1])
    Output
      [1] 1.644934

---

    Code
      cumsum(x)
    Output
      [1]  1  3  6 10 15 NA

---

    Code
      cumprod(x)
    Output
      [1]   1   2   6  24 120  NA

---

    Code
      cummax(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      cummin(x)
    Output
      [1]  1  1  1  1  1 NA

---

    Code
      x - x
    Output
      [1]  0  0  0  0  0 NA

---

    Code
      x * x
    Output
      [1]  1  4  9 16 25 NA

---

    Code
      x / x
    Output
      [1]  1  1  1  1  1 NA

---

    Code
      x^x
    Output
      [1]    1    4   27  256 3125   NA

---

    Code
      x %% x
    Output
      [1]  0  0  0  0  0 NA

---

    Code
      x %/% x
    Output
      [1]  1  1  1  1  1 NA

---

    Code
      x & x
    Output
      [1] TRUE TRUE TRUE TRUE TRUE   NA

---

    Code
      x | x
    Output
      [1] TRUE TRUE TRUE TRUE TRUE   NA

---

    Code
      !x
    Output
      [1] FALSE FALSE FALSE FALSE FALSE    NA

---

    Code
      x > 0
    Output
      [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE

---

    Code
      x != -1
    Output
      [1]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE

---

    Code
      x == "DK"
    Output
      [1] FALSE FALSE FALSE FALSE FALSE  TRUE

---

    Code
      Arg(x)
    Output
      [1]  0  0  0  0  0 NA

---

    Code
      Conj(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      Im(x)
    Output
      [1] 0 0 0 0 0 0

---

    Code
      Mod(x)
    Output
      [1]  1  2  3  4  5 NA

---

    Code
      Re(x)
    Output
      [1]  1  2  3  4  5 NA

