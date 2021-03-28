

  .pvalformat <- function(x) {
    options(scipen = 999)
    if (x < 0.001)
      "<0.001"
    else if (x > 0.999)
      ">0.999"
    else
      format(round(x, 3),
             digits = 3,
             justify = "right",
             width = 6)
  }
