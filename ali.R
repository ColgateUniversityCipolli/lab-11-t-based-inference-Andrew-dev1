## importing libraries
library(tidyverse)
library(pwr)
library(e1071)
library(effectsize)


power.test <- pwr.t.test(d = 0.65,
           sig.level = 0.05,
           power = 0.80,
           type = "one.sample",
           alternative = "two.sided")
(observations <- power.test[[1]])

zebra.finches.dat <- read_csv("lab10data.csv") |>
  rename("close" = "Closer vals",
         "far" = "Farther vals") |>
  mutate(difference = close-far)


## summarize data 
zebra.finches.dat |>
  reframe( type = c("far", "close", "difference"),
           mean = c(mean(far), mean(close), mean(difference)),
           sd         = c(sd(far), sd(close), sd(difference)),
           median     = c(median(far), median(close), median(difference)),
           IQR        = c(IQR(far), IQR(close), IQR(difference)),
           skewness   = c(skewness(far), skewness(close), skewness(difference)),
           exkurtosis = c(kurtosis(far), kurtosis(close), kurtosis(difference))) 

## challenge plot
finches.dat.long <- zebra.finches.dat |>
  select(-difference) |>
  pivot_longer( everything(), names_to = "Type", values_to = "F.change")

ggplot(finches.dat.long,aes(x = Type, y = F.change, color = Type)) +
  geom_point() +
  # geom_errorbar(aes(ymin = Type, ymax = Type))+ 
  theme_bw() +
  labs(title = "Column-wise Scatter Plot",
       x = "",
       y = "delta F (%)")

##############
## Task 4
##############
mu0 <- 0
x <- zebra.finches.dat$far
(xbar <- mean(x))
(s <- sd(x))
(n <- length(x))
any(is.na(x)) # no missing data
(t.stat <- (xbar - mu0)/(s/sqrt(n)))


hedges_g(x = x, mu = mu0, alternative = "two.sided")
interpret_hedges_g(-1.51)

(p.val <- 2*pt(q=-abs(t.stat), df = n-1))


x <- zebra.finches.dat$close
(xbar <- mean(x))
(s <- sd(x))
(n <- length(x))
any(is.na(x)) # no missing data
(t.stat <- (xbar - mu0)/(s/sqrt(n)))

hedges_g(x = x, mu = mu0, alternative = "two.sided")
interpret_hedges_g(1.61)

(p.val <- 2*pt(q=-abs(t.stat), df = n-1))


t.test(x=x, mu = mu0, alternative = "less")

t.test(x=x, mu = mu0, alternative = "greater")

t.test(x=x, mu = mu0, alternative = "two.sided")
