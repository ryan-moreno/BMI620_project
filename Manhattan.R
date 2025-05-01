library(qqman)
library(data.table)
library(tidyverse)

gwas <- as.data.frame(fread("data/SM_EUR_QCed_v2.txt",header=T))
head(gwas)

# gwas$P <- gwas$`P-value`
# gwas$CHR <- str_split_i(gwas$MarkerName, ":", 1)
# gwas$CHR <- ifelse(gwas$CHR=="X", 23, gwas$CHR)
# gwas$CHR <- ifelse(gwas$CHR=="Y", 24, gwas$CHR)
# gwas$CHR <- ifelse(gwas$CHR=="MT", 25, gwas$CHR)
# gwas$CHR <- as.numeric(gwas$CHR)
# gwas$BP <- as.numeric(str_split_i(gwas$MarkerName, ":", 2))
# gwas$SNP <- gwas$MarkerName
gwas$BP <- gwas$POS
gwas$SNP <- gwas$rsID
head(gwas)

interest <- gwas %>% filter(P < 5e-08)
snpsOfInterest <- interest$SNP

# make manhattan plot
png("manhattan_final.png",units="in",width = 15,height = 8,res=300)
manhattan(gwas,chr="CHR",bp="BP",p="P",snp="SNP",suggestiveline=F,genomewideline=-log10(5e-08), annotatePval=5e-08, highlight = snpsOfInterest)
dev.off()

# make QQ plot
png("qqplot_final.png",units="in",width=8,height=8,res=300)
qq(gwas$P)
dev.off()

# print genomic inflation factor
(lambda=median((qnorm(gwas$P/2,0,1))^2)/qchisq(0.5,1))


# gwas <- as.data.frame(fread("SM_transethnic_sumstats_unfiltered.txt",header=T))
# gwas <- gwas %>% filter(Ncohort>=11)
# head(gwas)
# 
# gwas$P <- gwas$`P-value_association`
# gwas$CHR <- str_split_i(gwas$MarkerName, ":", 1)
# # gwas$CHR <- ifelse(gwas$CHR=="X", 23, gwas$CHR)
# # gwas$CHR <- ifelse(gwas$CHR=="Y", 24, gwas$CHR)
# # gwas$CHR <- ifelse(gwas$CHR=="MT", 25, gwas$CHR)
# gwas$CHR <- as.numeric(gwas$Chromosome)
# gwas$BP <- as.numeric(gwas$Position)
# gwas$SNP <- gwas$MarkerName
# head(gwas)
# 
# gwas %>% filter(P < 5e-08)
# 
# # make manhattan plot
# png("manhattan_trans.png",units="in",width = 15,height = 8,res=300)
# manhattan(gwas,chr="CHR",bp="BP",p="P",snp="SNP",suggestiveline=F,genomewideline=-log10(5e-08))
# dev.off()
# 
# # make QQ plot
# png("qqplot_trans.png",units="in",width=8,height=8,res=300)
# qq(gwas$P)
# dev.off()
# 
# # print genomic inflation factor
# (lambda=median((qnorm(gwas$P/2,0,1))^2)/qchisq(0.5,1))
# 
# 
# 
# gwas <- as.data.frame(fread("RM_sumstats_unfiltered.txt",header=T))
# gwas <- gwas %>% filter(HetDf>=1) %>% filter(Freq1>0.005) %>% filter(MinFreq>0.001)
# head(gwas)
# 
# gwas$P <- gwas$`P-value`
# gwas$CHR <- str_split_i(gwas$MarkerName, ":", 1)
# gwas$CHR <- ifelse(gwas$CHR=="X", 23, gwas$CHR)
# gwas$CHR <- ifelse(gwas$CHR=="Y", 24, gwas$CHR)
# gwas$CHR <- ifelse(gwas$CHR=="MT", 25, gwas$CHR)
# gwas$CHR <- as.numeric(gwas$CHR)
# gwas$BP <- as.numeric(str_split_i(gwas$MarkerName, ":", 2))
# gwas$SNP <- gwas$MarkerName
# head(gwas)
# 
# gwas %>% filter(P < 5e-08)
# 
# # make manhattan plot
# png("manhattan_rm.png",units="in",width = 15,height = 8,res=300)
# manhattan(gwas,chr="CHR",bp="BP",p="P",snp="SNP",suggestiveline=F,genomewideline=-log10(5e-08))
# dev.off()
# 
# # make QQ plot
# png("qqplot_rm.png",units="in",width=8,height=8,res=300)
# qq(gwas$P)
# dev.off()
# 
# # print genomic inflation factor
# (lambda=median((qnorm(gwas$P/2,0,1))^2)/qchisq(0.5,1))
