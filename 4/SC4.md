
<html>

<h2 style={font-family: "XB Niloofar">
Statistical Computing
</h2>
<h3 style={font-family: "XB Niloofar">
Author <br>
Amirhossein Khadivi <br>
<a href='https://github.com/amirhossein-khadivi/SC/tree/master'title='GitHub'>Source Codes</a>
<br><br>
Supervisor <br>
Dr. Mohammad Kazemi <br> <br>
Department of Statistics, University of Guilan
<br>
</h3>

<p>

1- میخواهیم برنامه ای بنویسیم که با دریافت یک عدد طبیعی مقدار <br>
(n\!)/(sqrt(2*pi*n)\*(n/exp)^n) <br> را برگرداند.

``` r
f <- function(n) {
  if (n <= 0) {
    stop('n bayad intiger bashd.')
  } else{
    fn <- (factorial(n)) / ((sqrt(2 * pi * n)) * ((n / exp(1)) ^ n))
    fn
  }
}

# Example
f(2)
```

    ## [1] 1.042207

<br> <br> <br>

2- میخواهیم برنامه ای بنویسیم که با دریافت یک ماتریس با 8 ستون و تعداد
دلخواه سطر، ضریب گشتاوری چولگی را برای هر ستون محاسبه و این مقدارها را
در قالب یک بردار برگرداند.

``` r
f <- function(x) {
  if (is.matrix(x) == F) {
    print('sakhtare dadeha bayad matrix bashand.')
  }
  if (ncol(x) != 8) {
    print('tedade sotunhaye matrix bayad 8 bashad.')
  } else{
    y <- c()
    for (i in 1:ncol(x)) {
      v <- x[, i]
      d <-
        ((1 / length(v)) * sum((v - mean(v)) ^ 3)) / ((1 / (length(v) - 1)) * sum((v - mean(v)) ^
                                                                                    2)) ^ (3 / 2)
      y[i] <- d
      
    }
    print(y)
  }
  
}
```

<br> <br> <br>

3- میخواهیم برنامه ای بنویسیم که با دریافت یک ماتریس ، شماره ی سطر و
ستون مولفه های با مقدار منفی را به عنوان خروجی برگرداند. بدنه ی تابع
را هم به گونه ای تعریف میکنیم که اگر ماتریس مولفه ی با مقدار منفی نداشت
، این مطلب را به اطلاع کاربر برساند.

``` r
f <- function(x) {
  if (is.matrix(x) == F) {
    stop('sakhtare dadeha bayad matrix bashad.')
  } else{
    for (i in 1:ncol(x)) {
      for (j in 1:nrow(x)) {
        if (x[i, j] < 0) {
          print(c(i, j))
        }
        s <- 0
        s <- s + sign(x[i, j])
      }
    }
  }
  if (s <- length(x)) {
    print('dadeh manfi vojud nadarad.')
  }
}
```

<br> <br>

</p>

</body>

</html>
