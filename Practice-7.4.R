library (tidyverse)

rairuoho <- read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt',header=T, sep="\t", dec='.')

pearson.test <- function(x, y,
                         alternative = c("two.sided", "less", "greater"),
                         conf.level = 0.95){
  alternative <- match.arg(alternative)
  DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  if (!is.numeric(y)) stop("'y' must be a numeric vector")
  if (length(x) != length(y)) stop("'x' and 'y' must have the same length")
  x <- x[complete.cases (x, y)]
  y <- y[complete.cases (x, y)]
  n <- length(x)
  if (n < 3L) stop("not enough finite observations")
  method <- "Pearson's correlation"
  r <- cor(x, y)
  df <- n - 2
  ESTIMATE <- c(cor = r)
  statistic <-  r * sqrt (df) / sqrt (1 - r^2)
  p.value <- switch(
    alternative,
    less = pt (statistic, df),
    greater = pt (statistic, df, lower.tail = FALSE),
    two.sided = 2 * min (pt(statistic, df), pt (statistic, df, lower.tail = FALSE))
  )
  z <- atanh (r)
  sigma <- 1 / sqrt (n - 3)
  critical <- qnorm ((1 + conf.level) / 2)
  conf.int <- tanh (z + c(-1, 1) * sigma * critical)
  attr (conf.int, "conf.level") <- conf.level
  result <- list(
    statistic = c(t = statistic),
    parameter = c(df = df, n = n),
    p.value = p.value,
    estimate = c(correlation = r),
    conf.int = conf.int,
    null.value = 0,
    alternative = alternative,
    method = method,
    data.name = DNAME
  )
  class (result) <- "htest"
  return (result)}

# correlation in "nutrient"
nutrient_day3_4 <- rairuoho %>% filter (treatment == "nutrient") %>% with (pearson.test (day3, day4))
nutrient_day3_4
nutrient_day3_8 <- rairuoho %>% filter (treatment == "nutrient") %>% with (pearson.test (day3, day8))
nutrient_day3_8

# correlation in "water"
water_day3_4 <- rairuoho %>% filter (treatment == "water") %>% with (pearson.test (day3, day4))
water_day3_4
water_day3_8 <- rairuoho %>% filter (treatment == "water") %>% with (pearson.test (day3, day8))
water_day3_8