#
# MO 650 - Scientific Methodology (March 2018)
# Author: Edgar Tanaka
# RA 023577
#
setwd("~/git/mo850/ex1/")

# Non Paired Data - histogram, wilcox and t-test
a1 = read.csv("a1.csv", stringsAsFactors=FALSE, header=FALSE)$V1
b1 = read.csv("b1.csv", stringsAsFactors=FALSE, header=FALSE)$V1
wilcox.test(a1, b1, paired=FALSE)
t.test(a1, b1, paired=FALSE)
hist(a1, breaks=10)
hist(b1, breaks=10)


# Paired Data - wilcox and t-test
a2 = read.csv("paired.csv", header=FALSE)
x = a2$V1
y = a2$V2
wilcox.test(x, y, paired=TRUE)
t.test(x, y, paired=TRUE)

# Paired data - Sign test
successes = sum(x > y)
failures = length(x) - successes
binom.test(c(successes, failures))


# Study on the factors that impact the p-value
mean_p_values <- function(num_pairs, n, mean1, mean2, stddev) {
  p_values = c()
  for (i in 1:num_pairs) {
    a = rnorm(n, mean1, stddev)
    b = rnorm(n, mean2, stddev)
    t_test = t.test(a, b, paired=FALSE)
    p_values[i] = t_test$p.value
  }

  return(mean(p_values))
}

mean_p_values(num_pairs=10, n=15, mean1=10, mean2=13, stddev=5)
mean_p_values(num_pairs=10, n=25, mean1=10, mean2=13, stddev=5)
mean_p_values(num_pairs=10, n=15, mean1=10, mean2=17, stddev=5)
mean_p_values(num_pairs=10, n=15, mean1=10, mean2=13, stddev=8)