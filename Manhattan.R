# Adapted from course files

library(qqman)
library(data.table)
library(tidyverse)

gwas <- as.data.frame(fread("data/SM_EUR_QCed_v2.txt", header = T))
head(gwas)

gwas$BP <- gwas$POS
gwas$SNP <- gwas$rsID
head(gwas)

interest <- gwas %>% filter(P < 5e-08)
snpsOfInterest <- interest$SNP

# make manhattan plot
png("manhattan_final.png", units = "in", width = 15, height = 8, res = 300)
manhattan(gwas, chr = "CHR", bp = "BP", p = "P", snp = "SNP", suggestiveline = F, genomewideline = -log10(5e-08), annotatePval = 5e-08, highlight = snpsOfInterest)
dev.off()

# make QQ plot
png("qqplot_final.png", units = "in", width = 8, height = 8, res = 300)
qq(gwas$P)
dev.off()

# print genomic inflation factor
(lambda <- median((qnorm(gwas$P / 2, 0, 1))^2) / qchisq(0.5, 1))