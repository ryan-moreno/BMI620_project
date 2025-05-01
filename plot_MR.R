library(data.table)
library(tidyverse)
library(tidyverse)    # Data wrangling 
library(TwoSampleMR)  # MR 
library(LDlinkR)      # LD and proxy snps
library(gt)

mr_dat <- read.csv("exposure/mr_dat_age")
mr_res <- read.csv("exposure/mr_res_age.csv")
res_single <- read.csv("exposure/res_single_age.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("exposure/mr_dat_chrono")
mr_res <- read.csv("exposure/mr_res_chrono.csv")
res_single <- read.csv("exposure/res_single_chrono.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("exposure/mr_dat_covid")
mr_res <- read.csv("exposure/mr_res_covid.csv")
res_single <- read.csv("exposure/res_single_covid.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]

mr_dat <- read.csv("exposure/mr_dat_income")
mr_res <- read.csv("exposure/mr_res_income.csv")
res_single <- read.csv("exposure/res_single_income.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("exposure/mr_dat_insomnia")
mr_res <- read.csv("exposure/mr_res_insomnia.csv")
res_single <- read.csv("exposure/res_single_insomnia.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("exposure/mr_dat_ptsd")
mr_res <- read.csv("exposure/mr_res_ptsd.csv")
res_single <- read.csv("exposure/res_single_ptsd.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]




mr_dat <- read.csv("outcome/mr_dat_age")
mr_res <- read.csv("outcome/mr_res_age.csv")
res_single <- read.csv("outcome/res_single_age.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("outcome/mr_dat_chrono")
mr_res <- read.csv("outcome/mr_res_chrono.csv")
res_single <- read.csv("outcome/res_single_chrono.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("outcome/mr_dat_covid")
mr_res <- read.csv("outcome/mr_res_covid.csv")
res_single <- read.csv("outcome/res_single_covid.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]

mr_dat <- read.csv("outcome/mr_dat_income")
mr_res <- read.csv("outcome/mr_res_income.csv")
res_single <- read.csv("outcome/res_single_income.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("outcome/mr_dat_insomnia")
mr_res <- read.csv("outcome/mr_res_insomnia.csv")
res_single <- read.csv("outcome/res_single_insomnia.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]


mr_dat <- read.csv("outcome/mr_dat_ptsd")
mr_res <- read.csv("outcome/mr_res_ptsd.csv")
res_single <- read.csv("outcome/res_single_ptsd.csv")

mr_res

res_pleio <- mr_pleiotropy_test(mr_dat)

res_pleio %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = c('egger_intercept', 'se')
  ) %>%
  fmt_number(
    columns = pval,
    rows = pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = pval,
    rows = pval <= 0.001,
    decimals = 1
  )

# Heterogeneity statistics 
res_het <- mr_heterogeneity(mr_dat, method_list = c(
  "mr_ivw_fe", "mr_ivw_mre"
))

res_het %>%
  select(-id.exposure, -id.outcome, -outcome, -exposure) %>%
  gt() %>%
  fmt_number(
    columns = Q
  ) %>%
  fmt_number(
    columns = Q_pval,
    rows = Q_pval > 0.001,
    decimals = 3
  ) %>% 
  fmt_scientific(
    columns = Q_pval,
    rows = Q_pval <= 0.001,
    decimals = 1
  )

scatter_p <- mr_scatter_plot(mr_res, mr_dat)
scatter_out_p <- scatter_p[[1]] + theme_bw() +
  guides(color=guide_legend(ncol =1)) +
  theme(
    text = element_text(size = 8),
  )
scatter_out_p

forrest_p <- mr_forest_plot(res_single)
forrest_p[[1]]

