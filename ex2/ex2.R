#
# MO 650 - Scientific Methodology (March 2018)
# Author: Edgar Tanaka
# RA 023577
#
setwd("~/git/mo850/ex2/")
#install.packages(c("perm", "coin", "mvtnorm", "exactRankTests", "boot"))

run_unpaired_manual = function() {
  # data = c(x, y)
  # ttest_pvalues = c()
  # wilcox_pvalues = c()
  # for (i in 1:5000) {
  #   x_ind = sample(seq_len(length(data)), size = length(x), replace=FALSE)
  #   x = data[x_ind]
  #   y = data[-x_ind]
  #   wilcox_pvalues = c(wilcox.test(x, y, paired=FALSE)$p.value, wilcox_pvalues)
  #   ttest_pvalues = c(t.test(x, y, paired=FALSE)$p.value, ttest_pvalues)
  # }
  # 
  # cat("mean of t-test p-values:", mean(ttest_pvalues))
  # cat("mean of wilcoxon p-values:", mean(wilcox_pvalues))
  # hist(ttest_pvalues)
  # hist(wilcox_pvalues)
}

run_unpaired = function() {
  # Non paired
  # run a Monte-Carlo permutation test (5000 repetitions) and report the p-value.
  x = read.csv("a1.csv", stringsAsFactors=FALSE, header=FALSE)$V1
  y = read.csv("b1.csv", stringsAsFactors=FALSE, header=FALSE)$V1
  
  DV = c(x,y)
  IV <- factor(rep(c("A", "B"), c(length(x), length(y))))
  
  library(coin)
  print("p-value for unpaired data (oneway_test):")
  print(pvalue(oneway_test(DV ~ IV,  distribution=approximate(B=5000))))

  library(perm)  # for permTS()
  print("p-value for unpaired data (permTS):")
  print(permTS(DV ~ IV, method="exact.mc",control=permControl(nmc=5000))$p.value)
}

run_paired = function() {
  # Paired data
  # compute the Monte Carlo permutation p-value.
  paired = read.csv("paired.csv", header=FALSE)
  x = paired$V1
  y = paired$V2
  
  B = 5000 # number of simulations
  d <- x-y
  m0 <- mean(d)
  
  # perform a one-sample randomization test on d
  # for the null hypothesis H0: mu_d = 0   vs H1 mu_d != 0  (i.e. two tailed)
  # here the test statistic is the mean
  rndmdist <- replicate(B, mean((rbinom(length(d),1,.5)*2-1)*d))
  
  hist(rndmdist)
  
  # two tailed p-value:
  cat("Two tailed p-value:", sum(abs(rndmdist) >= abs(m0)) / length(rndmdist))
}

run_categorical = function() {
  # Run the following tests for some categorical data (0/1)
  # McNemar test
  # (non paired) Chi-square test
  # paired t test
  # Wilcoxon signed rank test
  
  # load data
  data = read.csv("two.csv", header=FALSE)
  x = data$V1
  y = data$V2
  
  # print results
  print(paste0("McNemar test p-value: ", mcnemar.test(x, y)$p.value))
  print(paste0("Unpaired Chi Square p-value: ", chisq.test(x, y)$p.value))
  print(paste0("Paired T Test p-value: ", t.test(x, y, paired=TRUE)$p.value))
  print(paste0("Wilcoxon signed rank test p-value: ", wilcox.test(x, y, paired=TRUE)$p.value))
}

run_ci = function() {
  # load data
  data = read.csv("single.csv", header=FALSE)$V1

  # CI - reverse t-test
  print(t.test(data, conf.int = TRUE))
  
  # CI - reverse wilcoxon signed rank test
  print(wilcox.test(data, conf.int = TRUE))
  
  # CI - bootstrap (with bca) and 5000 repetitions
  my_mean = function(data, indices) { return(mean(data[indices]))  }
  results <- boot(data=data, statistic=my_mean, R=5000)
  print(boot.ci(results, type="bca"))
}


run_unpaired()
run_paired()
run_categorical()
run_ci()


