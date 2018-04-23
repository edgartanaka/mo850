setwd("C:/Users/Edgar/Documents/git/mo850/ex3")

a = read.csv("a.csv", stringsAsFactors=FALSE, header=FALSE)$V1
b = read.csv("b.csv", stringsAsFactors=FALSE, header=FALSE)$V1
c = read.csv("c.csv", stringsAsFactors=FALSE, header=FALSE)$V1
d = read.csv("d.csv", stringsAsFactors=FALSE, header=FALSE)$V1
e = read.csv("e.csv", stringsAsFactors=FALSE, header=FALSE)$V1

groups = c(rep("a", length(a)))
groups = c(groups, rep("b", length(b)))
groups = c(groups, rep("c", length(c)))
groups = c(groups, rep("d", length(d)))
groups = c(groups, rep("e", length(e)))
df = data.frame(group=groups, weight=c(a,b,c,d,e))
anova(lm(weight ~ group, df))

# run the ANOVA - report the p-value
# is the ANOVA p-value is low enough, run some post hoc analysis 
# (Tukey HDS or paired t-test with Holms correction) and report which groups 
# are significantly different from each other
# run the Kruskal-Wallis test and report the p-value
# If the Kruskal Wallis is low enough, run all pairwise 
# Wilcoxon rank sum test. Report which groups are significantly
# different from each other for using Holms and Bonferroni corrections 
# on the p-values from the pairwise comparisons.


# Getting to know the data
library("ggpubr")
ggboxplot(df, x = "group", y = "weight", 
          color = "group",
          order = c("a", "b", "c", "d", "e"),
          ylab = "Weight", xlab = "Group")


# Pairwise T Test
pairwise.t.test(df$weight, df$group, p.adjust.method = "holm")

# Tukey
res.aov <- aov(weight ~ group, data = df)
summary(res.aov)
TukeyHSD(res.aov)


# Kruskal
kruskal.test(weight ~ group, data = df)
pairwise.wilcox.test(df$weight, df$group, p.adjust.method = "holm")
pairwise.wilcox.test(df$weight, df$group, p.adjust.method = "bonferroni")


###################################
paired = read.csv("multi.csv", header=FALSE, col.names=c("me", "competitor1", "competitor2", "competitor3", "competitor4"))
paired = as.matrix(paired)
friedman.test(paired)


install.packages("reshape")
library(reshape)
paired = read.csv("multi.csv", header=FALSE, col.names=c("me", "competitor1", "competitor2", "competitor3", "competitor4"))
paired <- melt(paired)
pairwise.wilcox.test(paired$value, paired$X2, paired = TRUE, p.adjust.method = "holm")
pairwise.wilcox.test(paired$value, paired$X2, paired = TRUE, p.adjust.method = "bonferroni")



library("ggpubr")
paired = read.csv("multi.csv", header=FALSE, col.names=c("me", "competitor1", "competitor2", "competitor3", "competitor4"))
paired <- melt(paired)
ggboxplot(paired, x = "variable", y = "value", 
          color = "variable",
          order = c("me", "competitor1", "competitor2", "competitor3", "competitor4"),
          ylab = "Run time", xlab = "Algorithm")


install.packages("scmamp")
library("scmamp")

