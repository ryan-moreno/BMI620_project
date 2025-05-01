library(data.table)
library(tidyverse)
library(tidyverse)    # Data wrangling 
library(TwoSampleMR)  # MR 
library(LDlinkR)      # LD and proxy snps

out_raw <- fread("data/SM_EUR_QCed_v2.txt")
out_raw <- as.data.frame(out_raw)

out_dat <- format_data( out_raw,
                        type = "outcome",
                        snp_col = "rsID",
                        beta_col = "BETA",
                        se_col = "SE",
                        effect_allele_col = "A1",
                        other_allele_col = "A2",
                        pval_col = "P",
                        chr_col = "CHR",
                        pos_col = "POS"
)


exp_raw <- fread("data/Age_at_first_birth.tsv")
exp_raw <- as.data.frame(exp_raw)

exp_dat <- format_data( exp_raw,
                        type = "exposure",
                        snp_col = "variant_id",
                        beta_col = "beta",
                        se_col = "standard_error",
                        effect_allele_col = "effect_allele",
                        other_allele_col = "other_allele",
                        pval_col = "p_value"
)

# Perform LD clumping on SNP data, filter SNPs to make it run faster
clumped_exp <- exp_dat %>% 
  filter(pval.exposure < 0.01) %>%
  clump_data(.,
             clump_kb = 10000,
             clump_r2 = 0.001,
             clump_p1 = 1,
             clump_p2 = 1,
             pop = "EUR"
  )

filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()
exposure_dat <- filter(clumped_exp, pval.exposure < 5e-08)
harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res

write.csv(harmonized_data, "outcome/mr_dat_age")
write.csv(mr_res, "outcome/mr_res_age.csv")
write.csv(res_single, "outcome/res_single_age.csv")

exp_raw <- fread("data/Income.txt")
exp_raw <- as.data.frame(exp_raw)
# exp_raw <- subset(exp_raw,exp_raw$P<1e-05)

exp_dat <- format_data( exp_raw,
                        type = "exposure",
                        snp_col = "SNP",
                        beta_col = "Beta",
                        se_col = "Standard_Error_of_Beta",
                        effect_allele_col = "Effect_Allele",
                        other_allele_col = "Non_effect_Allele",
                        pval_col = "P",
                        chr_col = "Chr", 
                        pos_col = "BPos"
)
clumped_exp <- exp_dat %>% 
  filter(pval.exposure < 0.01) %>%
  clump_data(.,
             clump_kb = 10000,
             clump_r2 = 0.001,
             clump_p1 = 1,
             clump_p2 = 1,
             pop = "EUR"
  )
filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()

exposure_dat <- filter(clumped_exp, pval.exposure < 5e-08)

# clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 


harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res

write.csv(harmonized_data, "outcome/mr_dat_income")
write.csv(mr_res, "outcome/mr_res_income.csv")
write.csv(res_single, "outcome/res_single_income.csv")


exp_raw <- fread("data/Post-Traumatic_Stress_Disorder.results")
# exp_raw <- subset(exp_raw,exp_raw$P<1e-05)
exp_raw <- as.data.frame(exp_raw)
exp_raw$BETA <-  (exp(exp_raw$OR)/(1+exp(exp_raw$OR)))

exp_dat <- format_data( exp_raw,
                        type = "exposure",
                        snp_col = "SNP",
                        beta_col = "BETA",
                        se_col = "SE",
                        effect_allele_col = "A1",
                        other_allele_col = "A2",
                        pval_col = "P",
                        chr_col = "CHR",
                        pos_col = "BP"
)
clumped_exp <- exp_dat %>% 
  filter(pval.exposure < 0.01) %>%
  clump_data(.,
             clump_kb = 10000,
             clump_r2 = 0.001,
             clump_p1 = 1,
             clump_p2 = 1,
             pop = "EUR"
  )

filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()
exposure_dat <- filter(clumped_exp, pval.exposure < 1e-05)

# clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 


harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res

write.csv(harmonized_data, "outcome/mr_dat_ptsd")
write.csv(mr_res, "outcome/mr_res_ptsd.csv")
write.csv(res_single, "outcome/res_single_ptsd.csv")


# exp_raw <- fread("data/Insomnia.txt")
# # exp_raw <- subset(exp_raw,exp_raw$P<1e-05)
# exp_raw <- as.data.frame(exp_raw)
# exp_raw$BETA <-  log(exp_raw$OR)
# 
# exp_dat <- format_data( exp_raw,
#                         type = "exposure",
#                         snp_col = "RSID_UKB",
#                         beta_col = "BETA",
#                         se_col = "SE",
#                         effect_allele_col = "A1",
#                         other_allele_col = "A2",
#                         pval_col = "P",
#                         chr_col = "CHR",
#                         pos_col = "BP"
# )
# clumped_exp <- exp_dat %>% 
#   filter(pval.exposure < 0.01) %>%
#   clump_data(.,
#              clump_kb = 10000,
#              clump_r2 = 0.001,
#              clump_p1 = 1,
#              clump_p2 = 1,
#              pop = "EUR"
#   )
# filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()
# exposure_dat <- filter(clumped_exp, pval.exposure < 5e-08)
# 
# # clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 
# 
# 
# harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)
# 
# mr_res <- mr(harmonized_data, method_list = c(
#   "mr_ivw_fe", "mr_ivw_mre"
# ))
# res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()
# 
# mr_res
# 
# write.csv(harmonized_data, "outcome/mr_dat_insomnia")
# write.csv(mr_res, "outcome/mr_res_insomnia.csv")
# write.csv(res_single, "outcome/res_single_insomnia.csv")

# exp_raw <- fread("data/COVID.txt")
# # exp_raw <- subset(exp_raw,exp_raw$all_inv_var_meta_p<1e-05)
# exp_raw <- as.data.frame(exp_raw)
# 
# exp_dat <- format_data( exp_raw,
#                         type = "exposure",
#                         snp_col = "rsid",
#                         beta_col = "all_inv_var_meta_beta",
#                         se_col = "all_inv_var_meta_sebeta",
#                         effect_allele_col = "REF",
#                         other_allele_col = "ALT",
#                         pval_col = "all_inv_var_meta_p",
#                         # chr_col = "#CHR",
#                         pos_col = "POS"
# )
# clumped_exp <- exp_dat %>% 
#   filter(pval.exposure < 0.01) %>%
#   clump_data(.,
#              clump_kb = 10000,
#              clump_r2 = 0.001,
#              clump_p1 = 1,
#              clump_p2 = 1,
#              pop = "EUR"
#   )
# filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()
# exposure_dat <- filter(clumped_exp, pval.exposure < 5e-08)
# 
# # clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 
# 
# 
# harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)
# 
# mr_res <- mr(harmonized_data, method_list = c(
#   "mr_ivw_fe", "mr_ivw_mre"
# ))
# res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()
# 
# mr_res
# 
# write.csv(harmonized_data, "outcome/mr_dat_covid")
# write.csv(mr_res, "outcome/mr_res_covid.csv")
# write.csv(res_single, "outcome/res_single_covid.csv")

exp_raw <- fread("data/Chronotype.txt")
# exp_raw <- subset(exp_raw,exp_raw$P_BOLT_LMM<1e-05)
exp_raw <- as.data.frame(exp_raw)

exp_dat <- format_data( exp_raw,
                        type = "exposure",
                        snp_col = "SNP",
                        beta_col = "BETA",
                        se_col = "SE",
                        effect_allele_col = "ALLELE1",
                        other_allele_col = "ALLELE0",
                        pval_col = "P_BOLT_LMM",
                        chr_col = "CHR",
                        pos_col = "BP"
)
clumped_exp <- exp_dat %>% 
  filter(pval.exposure < 0.01) %>%
  clump_data(.,
             clump_kb = 10000,
             clump_r2 = 0.001,
             clump_p1 = 1,
             clump_p2 = 1,
             pop = "EUR"
  )
filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()
exposure_dat <- filter(clumped_exp, pval.exposure < 5e-08)

# clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 


harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res

write.csv(harmonized_data, "outcome/mr_dat_chrono")
write.csv(mr_res, "outcome/mr_res_chrono.csv")
write.csv(res_single, "outcome/res_single_chrono.csv")

######################################


exp_raw <- fread("data/Number_of_Children_Ever_Born.txt")
# exp_raw <- subset(exp_raw,exp_raw$Pvalue<1e-05)
exp_raw <- as.data.frame(exp_raw)

exp_raw$BETA = exp_raw$Zscore / sqrt(2*exp_raw$Freq_HapMap*(1 - exp_raw$Freq_HapMap)*(318463+ exp_raw$Zscore^2))
exp_raw$SE = 1 / sqrt(2*exp_raw$Freq_HapMap*(1 - exp_raw$Freq_HapMap)*(318463 + exp_raw$Zscore^2))


exp_dat <- format_data( exp_raw,
                        type = "exposure",
                        snp_col = "SNPID",
                        beta_col = "BETA",
                        se_col = "SE",
                        effect_allele_col = "A1",
                        other_allele_col = "A2",
                        pval_col = "Pvalue",
                        chr_col = "CHR",
                        pos_col = "BP"
)

clumped_exp <- exp_dat %>% 
  filter(pval.exposure < 0.01) %>%
  clump_data(.,
             clump_kb = 10000,
             clump_r2 = 0.001,
             clump_p1 = 1,
             clump_p2 = 1,
             pop = "EUR"
  )
filter(clumped_exp, pval.exposure < 5e-08) %>% nrow()
exposure_dat <- filter(clumped_exp, pval.exposure < 1e-05)

# clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 


harmonized_data <- harmonise_data(exposure_dat,out_dat,action=1)
mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res

write.csv(harmonized_data, "outcome/mr_dat_number")
write.csv(mr_res, "outcome/mr_res_number.csv")
write.csv(res_single, "outcome/res_single_number.csv")

