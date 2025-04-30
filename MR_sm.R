library(data.table)
library(tidyverse)
library(tidyverse)    # Data wrangling 
library(TwoSampleMR)  # MR 
library(LDlinkR)      # LD and proxy snps

exp_raw <- fread("sumstats/SM_EUR_QCed_v2.txt")
exp_raw <- subset(exp_raw,exp_raw$P<1e-05)
exp_raw <- as.data.frame(exp_raw)

exp_dat <- format_data( exp_raw,
                        type = "exposure",
                        snp_col = "rsID",
                        beta_col = "BETA",
                        se_col = "SE",
                        effect_allele_col = "A2",
                        other_allele_col = "A1",
                        pval_col = "P",
                        chr_col = "CHR",
                        pos_col = "POS"
)

clumped_exp <- clump_data(exp_dat,clump_r2=0.01,pop="EUR") 


out_raw <- fread("sumstats/Age_at_first_birth.tsv")

out_raw <- as.data.frame(out_raw)

out_dat <- format_data( out_raw,
                  type = "outcome",
                  snp_col = "variant_id",
                  beta_col = "beta",
                  se_col = "standard_error",
                  effect_allele_col = "effect_allele",
                  other_allele_col = "other_allele",
                  pval_col = "p_value"
)


harmonized_data <- harmonise_data(clumped_exp,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res
res_single

write.csv(harmonized_data, "exposure/mr_dat_age")
write.csv(mr_res, "exposure/mr_res_age.csv")
write.csv(res_single, "exposure/res_single_age.csv")

out_raw <- fread("sumstats/Income.txt")
out_raw <- as.data.frame(out_raw)

out_dat <- format_data( out_raw,
                type = "outcome",
                snp_col = "SNP",
                beta_col = "Beta",
                se_col = "Standard_Error_of_Beta",
                effect_allele_col = "Effect_Allele",
                other_allele_col = "Non_effect_Allele",
                pval_col = "P",
                chr_col = "Chr", 
                pos_col = "BPos"
)

harmonized_data <- harmonise_data(clumped_exp,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res
res_single

write.csv(harmonized_data, "exposure/mr_dat_income")
write.csv(mr_res, "exposure/mr_res_income.csv")
write.csv(res_single, "exposure/res_single_income.csv")


out_raw <- fread("sumstats/Post-Traumatic_Stress_Disorder.results")
out_raw <- as.data.frame(out_raw)
out_raw$BETA <-  (exp(out_raw$OR)/(1+exp(out_raw$OR)))

out_dat <- format_data( out_raw,
                  type = "outcome",
                  snp_col = "SNP",
                  beta_col = "BETA",
                  se_col = "SE",
                  effect_allele_col = "A2",
                  other_allele_col = "A1",
                  pval_col = "P",
                  chr_col = "CHR",
                  pos_col = "BP"
)

harmonized_data <- harmonise_data(clumped_exp,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res
res_single

write.csv(harmonized_data, "exposure/mr_dat_ptsd")
write.csv(mr_res, "exposure/mr_res_ptsd.csv")
write.csv(res_single, "exposure/res_single_ptsd.csv")


out_raw <- fread("sumstats/Insomnia.txt")
out_raw <- as.data.frame(out_raw)
out_raw$BETA <-  (exp(out_raw$OR)/(1+exp(out_raw$OR)))

out_dat <- format_data( out_raw,
                type = "outcome",
                snp_col = "RSID_UKB",
                beta_col = "BETA",
                se_col = "SE",
                effect_allele_col = "A2",
                other_allele_col = "A1",
                pval_col = "P",
                chr_col = "CHR",
                pos_col = "BP"
)

harmonized_data <- harmonise_data(clumped_exp,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res
res_single

write.csv(harmonized_data, "exposure/mr_dat_insomnia")
write.csv(mr_res, "exposure/mr_res_insomnia.csv")
write.csv(res_single, "exposure/res_single_insomnia.csv")

out_raw <- fread("sumstats/COVID.txt")
out_raw <- as.data.frame(out_raw)

out_dat <- format_data( out_raw,
            type = "outcome",
            snp_col = "rsid",
            beta_col = "all_inv_var_meta_beta",
            se_col = "all_inv_var_meta_sebeta",
            effect_allele_col = "ALT",
            other_allele_col = "REF",
            pval_col = "all_inv_var_meta_p",
            # chr_col = "#CHR",
            pos_col = "POS"
)

harmonized_data <- harmonise_data(clumped_exp,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res
res_single

write.csv(harmonized_data, "exposure/mr_dat_covid")
write.csv(mr_res, "exposure/mr_res_covid.csv")
write.csv(res_single, "exposure/res_single_covid.csv")

out_raw <- fread("sumstats/Chronotype.txt")
out_raw <- as.data.frame(out_raw)

out_dat <- format_data( out_raw,
          type = "outcome",
          snp_col = "SNP",
          beta_col = "BETA",
          se_col = "SE",
          effect_allele_col = "ALLELE1",
          other_allele_col = "ALLELE0",
          pval_col = "P_BOLT_LMM",
          chr_col = "CHR",
          pos_col = "BP"
)

harmonized_data <- harmonise_data(clumped_exp,out_dat,action=1)

mr_res <- mr(harmonized_data, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(harmonized_data, all_method = c("mr_ivw_fe")) %>% as_tibble()

mr_res
res_single

write.csv(harmonized_data, "exposure/mr_dat_chrono")
write.csv(mr_res, "exposure/mr_res_chrono.csv")
write.csv(res_single, "exposure/res_single_chrono.csv")
