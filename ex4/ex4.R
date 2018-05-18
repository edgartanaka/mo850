#
# MO 650 - Scientific Methodology (March 2018)
# Author: Edgar Tanaka
# RA 023577
#

# please set the current directory where this R script is located
setwd("~/git/mo850/ex4/")

#install.packages(c("equivalence", "BEST", "MBESS"))
library(equivalence)
set.seed(42);

control = read.csv("control.csv", stringsAsFactors=FALSE, header=FALSE)$V1
exp = read.csv("exp.csv", stringsAsFactors=FALSE, header=FALSE)$V1

# Equivalence Tests
tost.results = tost(control, exp)

# Cohen D
# https://en.wikipedia.org/wiki/Effect_size#Cohen's_d
n1 = length(control)
n2 = length(exp)
s1 = sd(control)
s2 = sd(exp)
s = sqrt(((n1 - 1) * s1 ^ 2 + (n2 - 1) * s2 ^2) / (n1 + n2 + -2))
m1 = mean(control)
m2 = mean(exp)
d = (m1 - m2)/s
print(paste("Cohen D (analytical solution):", d))
cohen.d(control, exp, pooled=T)

# Bayesian Analysis
Bestout <- BESTmcmc(control, exp)
summary(Bestout)
plotAll(Bestout)

