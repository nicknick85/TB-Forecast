tsm  <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
scr  <- c(57.9, 58.2, 63.3, 61.4, 62.5, 63.8, 64.4, 65.7, 65.8, 66.6, 68.1, 69.3, 71.3, 72.7, 73.7, 66.7, 71.0)                  
incd <- c(83.8, 82.6, 83.3, 85.1, 82.6, 77.2, 73.0, 68.1, 63.0, 59.5, 57.7, 53.3, 48.3, 44.4, 41.2, 32.4, 31.1)                                                                                                
mbt  <- c(85.6, 83.0, 80.2, 79.5, 77.2, 72.9, 69.0, 64.7, 60.5, 56.8, 54.0, 50.7, 46.0, 42.8, 37.8, 30.7, 28.5)

#############################################################################

logit <- function(x)
{
	log(x / (1 - x));
}

expit <- function(x)
{
	1 / (1 + exp(-x))
}

#############################################################################

y  <- logit(incd[5:17]/100000)
x1 <- logit(mbt[4:16]/100000)
x2 <- logit(scr[4:16]/100)
x3 <- logit(scr[5:17]/100)

#############################################################################

dy <- y - logit(incd[4:16]/100000)

png(file = "d_logit_incd.png", width=15000, height=5000, res=1200)
boxplot(dy, outpch = 21, outbg="red", outcex=1.5, horizontal = TRUE)
stripchart(dy[c(1:11, 13)], pch = 19, col = 4, add = TRUE)
dev.off()

Box.test(dy[c(1:11, 13)], type = "Ljung-Box")
shapiro.test(dy[c(1:11, 13)])

library(outliers)
grubbs.test(dy, type = 10)

#############################################################################

mdl <- lm(y ~ x1 + x2 + x3)