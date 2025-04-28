library(data.table)
library(tidyverse)

# alz <- as.data.frame(fread("Kunkle2019load_stage123.chrall.CPRA_b37.tsv",header=T))
# head(alz)
# 
# tc <- as.data.frame(fread("Willer2013tc.chrall.CPRA_b37.tsv",header=T))
# # head(tc)

library(tidyverse)    # Data wrangling 
library(TwoSampleMR)  # MR 
library(LDlinkR)      # LD and proxy snps


munge_proxies <- function(LDLink_file, outcome, outcome_clump){
  LDLink_file_path <- LDLink_file
  proxy_snps <- read_tsv(LDLink_file_path, skip = 1, col_names = F) %>%
    rename(id = X1, func = X2, proxy_snp = X3, coord = X4, alleles = X5, maf = X6, 
           distance = X7, dprime = X8, rsq = X9, correlated_alleles = X10, FORGEdb = X11, RegulomeDB = X12) %>%
    separate(coord, c('chr', 'pos'), sep = ":") %>%
    mutate(snp = ifelse(id == 1, proxy_snp, NA), 
           chr = str_replace(chr, 'chr', ""), 
           chr = as.numeric(chr), 
           pos = as.numeric(pos)) %>%
    fill(snp, .direction = 'down') %>%
    relocate(snp, .before = proxy_snp) %>%
    dplyr::select(-id, -func, -FORGEdb, -RegulomeDB) %>%
    filter(rsq >= 0.8)
  
  # Munge proxy snp and outcome data
  proxy_outcome <- left_join(
    proxy_snps, outcome, by = c("proxy_snp" = "SNP")
  ) %>%
    separate(correlated_alleles, c("target_a1.outcome", "proxy_a1.outcome", 
                                   "target_a2.outcome", "proxy_a2.outcome"), sep = ",|=") %>%
    filter(!is.na(chr.outcome)) %>%
    arrange(snp, -rsq, abs(distance)) %>%
    group_by(snp) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      proxy.outcome = TRUE,
      target_snp.outcome = snp,
      proxy_snp.outcome = proxy_snp, 
    ) %>% 
    mutate(
      new_effect_allele.outcome = case_when(
        proxy_a1.outcome == effect_allele.outcome & proxy_a2.outcome == other_allele.outcome ~ target_a1.outcome,
        proxy_a2.outcome == effect_allele.outcome & proxy_a1.outcome == other_allele.outcome ~ target_a2.outcome,
        TRUE ~ NA_character_
      ), 
      new_other_allele.outcome = case_when(
        proxy_a1.outcome == effect_allele.outcome & proxy_a2.outcome == other_allele.outcome ~ target_a2.outcome,
        proxy_a2.outcome == effect_allele.outcome & proxy_a1.outcome == other_allele.outcome ~ target_a1.outcome,
        TRUE ~ NA_character_
      ), 
      effect_allele.outcome = new_effect_allele.outcome, 
      other_allele.outcome = new_other_allele.outcome
    ) %>%
    dplyr::select(-proxy_snp, -chr, -pos, -alleles, -maf, -distance, -rsq, -dprime,  
                  -new_effect_allele.outcome, -new_other_allele.outcome) %>%
    relocate(target_a1.outcome, proxy_a1.outcome, target_a2.outcome, proxy_a2.outcome, .after = proxy_snp.outcome) %>%
    rename(SNP = snp) %>%
    relocate(SNP, .after = samplesize.outcome)
  
  # Merge outcome and proxy outcomes
  outcome_dat <- bind_rows(
    outcome_clump, proxy_outcome
  ) %>% 
    arrange(chr.outcome, pos.outcome)
  
  outcome_dat
}


# Define column types for summary statistics
coltypes = cols(
  ID = col_character(),
  CHROM = col_double(),
  POS = col_double(),
  REF = col_character(),
  ALT = col_character(),
  AF = col_double(),
  TRAIT = col_character(),
  BETA = col_double(),
  SE = col_double(),
  Z = col_double(),
  P = col_double(),
  N = col_double(),
  OR = col_double(),
  OR_L95 = col_double(),
  OR_U95 = col_double(),
  DIR = col_character(),
  G1000_ID = col_character(),
  G1000_VARIANT = col_character(),
  DBSNP_ID = col_character(),
  DBSNP_VARIANT = col_character(),
  OLD_ID = col_character(),
  OLD_VARIANT = col_character()
)


exposure_path = "Willer2013tc.chrall.CPRA_b37.tsv"
exposure_ss <- read_tsv(exposure_path, comment = "##", col_types = coltypes, 
                        col_select = c(DBSNP_ID, CHROM, POS, REF, ALT, AF, BETA, SE, Z, P, N, TRAIT))

# Format data to TwoSampleMR format
exposure <- exposure_ss %>%
  format_data(.,
              type = "exposure",
              snps = NULL,
              header = TRUE,
              phenotype_col = "TRAIT",
              snp_col = "DBSNP_ID",
              beta_col = "BETA",
              se_col = "SE",
              eaf_col = "AF",
              effect_allele_col = "ALT",
              other_allele_col = "REF",
              pval_col = "P",
              samplesize_col = "N",
              z_col = "Z",
              chr_col = "CHROM",
              pos_col = "POS",
              log_pval = FALSE
  ) %>%
  as_tibble()


outcome_path = "Kunkle2019load_stage123.chrall.CPRA_b37.tsv"
outcome_ss <- read_tsv(outcome_path, comment = "##",  col_types = coltypes, 
                       col_select = c(DBSNP_ID, CHROM, POS, REF, ALT, AF, BETA, SE, Z, P, N, TRAIT))

# Format outcome
outcome <- outcome_ss %>%
  format_data(.,
              type = "outcome",
              snps = NULL,
              header = TRUE,
              phenotype_col = "TRAIT",
              snp_col = "DBSNP_ID",
              beta_col = "BETA",
              se_col = "SE",
              eaf_col = "AF",
              effect_allele_col = "ALT",
              other_allele_col = "REF",
              pval_col = "P",
              samplesize_col = "N",
              z_col = "Z",
              chr_col = "CHROM",
              pos_col = "POS",
              log_pval = FALSE
  ) %>%
  as_tibble()


# extract exposure SNPs present in outcome
outcome_clump <- semi_join(
  outcome, exposure_dat, by = "SNP"
)

# Exposure SNPs not present in outomce
exp_snps_wo <- anti_join(
  exposure_dat, outcome, by = "SNP"
)

# Use LDLinkR to identify proxy snps
LDproxy_batch(exp_snps_wo$SNP, 
              pop = "CEU",             # Match population ancestries
              r2d = "r2", 
              token = 'a6deee62cc4a', 
              append = TRUE,           # We appended the results of each LDlink query to a single file
              genome_build = "grch37") # Select genome build based on summary stats
system("mv combined_query_snp_list_grch37.txt data/exposure_outcome_proxy_snps.txt")


# Munge proxy snp file
outcome_dat <- munge_proxies("data/exposure_outcome_proxy_snps.txt", outcome, outcome_clump)

mr_dat <- harmonise_data(exposure_dat, outcome_dat, action = 2) %>% as_tibble() %>%
  #  mutate(
  #    apoe_region = case_when(
  #      chr.outcome == 19 & between(pos.outcome, 44912079, 45912079) ~ TRUE,
  #      TRUE ~ FALSE
  #    ), 
  #    gws.outcome = ifelse(pval.outcome < 5e-8, TRUE, FALSE), 
  #   mr_keep = ifelse(mr_keep == FALSE | apoe_region == TRUE | gws.outcome == TRUE, FALSE, TRUE)
  #  )
  filter(pval.exposure < 5e-8)

write.csv(mr_dat, 'data/harmonized_data.csv')


mr_dat <- read_csv('data/harmonized_data.csv')
mr_res <- mr(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))
res_single <- mr_singlesnp(mr_dat, all_method = c("mr_ivw_fe")) %>% as_tibble()

write.csv(mr_res, "data/mr_res.csv")
write.csv(res_single, "data/res_single.csv")

# scatter_p <- mr_scatter_plot(mr_res, mr_dat) 
# scatter_out_p <- scatter_p[[1]] + theme_bw() + 
#   guides(color=guide_legend(ncol =1)) + 
#   theme(
#     text = element_text(size = 8), 
#   )
# 
# scatter_out_p
# forrest_p <- mr_forest_plot(res_single)
# forrest_p[[1]]