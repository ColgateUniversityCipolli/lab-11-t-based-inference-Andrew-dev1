## importing libraries
library(tidyverse)
library(pwr)
library(e1071)
library(effectsize)

## power test Part 1
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


## summarize data numerically in Part 3
(summary <- zebra.finches.dat |>
  reframe( type = c("far", "close", "difference"),
           min = c(min(far), min(close), min(difference)),
           max = c(max(far), max(close), max(difference)),
           mean = c(mean(far), mean(close), mean(difference)),
           sd         = c(sd(far), sd(close), sd(difference)),
           median     = c(median(far), median(close), median(difference)),
           IQR        = c(IQR(far), IQR(close), IQR(difference)),
           skewness   = c(skewness(far), skewness(close), skewness(difference)),
           exkurtosis = c(kurtosis(far), kurtosis(close), kurtosis(difference))) )

# graphical interpretation
finches.dat.long1 <- zebra.finches.dat |>
  pivot_longer( everything(), names_to = "Type", values_to = "F.change")

ggplot(finches.dat.long1) +
  geom_boxplot(aes(x = Type, y = F.change, color = Type))+
  labs(
    x= "Dopamine values"
  )+
  theme_bw()+
  theme(legend.position = "none")
  

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

############################
## Task 4 paper inferences
############################
mu0 <- 0
far.set <- zebra.finches.dat$far
close.set <- zebra.finches.dat$close
difference.set <- zebra.finches.dat$difference

closer.test <- t.test(x=close.set, mu = mu0, alternative = "greater")
t.close <- closer.test[[1]][[1]] #finding the t value 
(p.close <- closer.test[[3]]) #finding the p value
g.close <- interpret_hedges_g(hedges_g(x = close.set, mu = mu0, alternative = "greater"))

farther.test <- t.test(x=far.set, mu = mu0, alternative = "less")
t.far <- farther.test[[1]][[1]] #finding the t value 
(p.far <- farther.test[[3]]) #finding the p value
g.far <- interpret_hedges_g(hedges_g(x = far.set, mu = mu0, alternative = "less"))

difference.test <- t.test(x=difference.set, mu = mu0, alternative = "two.sided")
t.difference <- difference.test[[1]][[1]] #finding the t value 
(p.difference <- difference.test[[3]]) #finding the p value
g.diff <- interpret_hedges_g(hedges_g(x = difference.set, mu = mu0, alternative = "two.sided"))


############################
## Task 5 hypothesis plots
############################

# finding the test statistic 
(xbar <- mean(close.set))
(s <- sd(close.set))
(n <- length(close.set))
any(is.na(close.set)) # no missing data
(close.t.stat <- (xbar - mu0)/(s/sqrt(n)))

(xbar <- mean(far.set))
(s <- sd(far.set))
(n <- length(far.set))
any(is.na(far.set)) # no missing data
(far.t.stat <- (xbar - mu0)/(s/sqrt(n)))

(xbar <- mean(difference.set))
(s <- sd(difference.set))
(n <- length(difference.set))
any(is.na(difference.set)) # no missing data
(difference.t.stat <- (xbar - mu0)/(s/sqrt(n)))

ggdat.obs <- tibble(close.t    = close.t.stat, 
                    far.t = far.t.stat,
                    diff.t = difference.t.stat,
                    y    = 0)


# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))

## Taking resamples to approximate sampling distributions
R <- 1000
resample <- tibble(close.t=numeric(R),
                    far.t=numeric(R),
                    diff.t=numeric(R)
                    )

for(i in 1:R){
  close.sample <- sample(x=close.set,
                        size=n,
                        replace=T)
  far.sample <- sample(x=far.set,
                       size=n,
                       replace=T)
  diff.sample <- sample(x=difference.set,
                       size=n,
                       replace=T)
  
  resample$close.t[i] = (mean(close.sample)-mu0)/(sd(close.sample)/sqrt(n))
  resample$far.t[i] = (mean(far.sample)-mu0)/(sd(far.sample)/sqrt(n))
  resample$diff.t[i] = (mean(diff.sample)-mu0)/(sd(diff.sample)/sqrt(n))
  
}

#### calculating the t and bar breaks for far, close and difference 
t.breaks <- c(-5, qt(0.05, df = n-1), # rejection region (left)
              0, 5, close.t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

t.breaks.far <- c(-5,  0, qt(0.95, df = n-1),   # rejection region (right)
                  5,
                  far.t.stat)                  # t-statistic observed
xbar.breaks.far <- t.breaks.far * s/(sqrt(n)) + mu0

t.breaks.diff <- c(-5, qt(0.025, df = n-1), # rejection region (left)
                  0, 
                  qt(0.975, df = n-1), 5,  # rejection region (right)
                  difference.t.stat)                  # t-statistic observed
xbar.breaks.diff <- t.breaks.diff * s/(sqrt(n)) + mu0


## plotting the close data in comparison to the null distribution 
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection region
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t> close.t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=close.t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resample, 
               aes(x=close.t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Dopamine levels from song closeness",
          subtitle=bquote(H[0]==0*";"~H[a]>0))

## plotting the far data in comparison to the null distribution 
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.05, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t <= far.t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point for further data
  geom_point(data=ggdat.obs, aes(x=far.t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resample, 
               aes(x=far.t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks.far,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks.far,
                                         labels = round(xbar.breaks.far,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Dopamine levels from songs farness",
          subtitle=bquote(H[0]==0*";"~H[a] <0))


## plotting the difference data in comparison to the null distribution 
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>= difference.t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point for further data
  geom_point(data=ggdat.obs, aes(x=diff.t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resample, 
               aes(x=diff.t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks.diff,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks.diff,
                                         labels = round(xbar.breaks.diff,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Dopamine levels from differences ",
          subtitle=bquote(H[0]==0*";"~H[a] !=0 ))
