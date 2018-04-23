setwd("C:/Users/Edgar/Documents/git/mo850/ex3")

install.packages("scmamp","ggpubr", "reshape")
library("ggpubr")
library(reshape)
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
biocLite("graph")
library("scmamp")

#################################
# Unpaired data
#################################
# Load data
a = read.csv("a.csv", stringsAsFactors=FALSE, header=FALSE)$V1
b = read.csv("b.csv", stringsAsFactors=FALSE, header=FALSE)$V1
c = read.csv("c.csv", stringsAsFactors=FALSE, header=FALSE)$V1
d = read.csv("d.csv", stringsAsFactors=FALSE, header=FALSE)$V1
e = read.csv("e.csv", stringsAsFactors=FALSE, header=FALSE)$V1

# Creating data frame
groups = c(rep("a", length(a)))
groups = c(groups, rep("b", length(b)))
groups = c(groups, rep("c", length(c)))
groups = c(groups, rep("d", length(d)))
groups = c(groups, rep("e", length(e)))
df = data.frame(group=groups, weight=c(a,b,c,d,e))
anova(lm(weight ~ group, df))

# Plotting data
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


#################################
# Paired data
#################################
# Plotting data
paired = read.csv("multi.csv", header=FALSE, col.names=c("me", "competitor1", "competitor2", "competitor3", "competitor4"))
paired_single <- melt(paired)
ggboxplot(paired_single, x = "variable", y = "value", 
          color = "variable",
          order = c("me", "competitor1", "competitor2", "competitor3", "competitor4"),
          ylab = "Run time", xlab = "Algorithm")

# Friedman
paired_matrix = as.matrix(paired)
friedman.test(paired_matrix)

#Wilcoxon
pairwise.wilcox.test(paired_single$value, paired_single$variable, paired = TRUE, p.adjust.method = "holm")
pairwise.wilcox.test(paired_single$value, paired_single$variable, paired = TRUE, p.adjust.method = "bonferroni")

# Nemeyi
test = nemenyiTest(paired, alpha=0.05)
test
test$diff.matrix
abs(test$diff.matrix) > test$statistic
plotCD (paired, alpha=0.05)