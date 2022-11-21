suppressPackageStartupMessages(library("brms"))
suppressPackageStartupMessages(library("cmdstanr"))
library("scales")
library("ggthemes")
library("emmeans")


# Get final subject pool.

good_subj_names <- rio::import(
  here("data", "processed", "prl", "input_for_hddmrl", "hddm_look_up_table_v3.csv")
)


# Get raw PRL data.
d <- readRDS(
  here::here("data", "processed", "prl", "raw_prl_data", "prl_tot_raw_data.rds")
)

d$stimulus_type <- recode(
  d$stimulus_type, 
  socialshame = 'neutral'
)

# Only subjects used in the hDDMrl analysis.
d1 <- d[d$subj_name %in% good_subj_names$subj_code, ]

# Only 3 diagnostic categories.
dat <- d1 %>% 
  dplyr::filter(diag_cat == "AN" | diag_cat == "BN" | diag_cat == "HC")

dat %>% 
  group_by(diag_cat, stimulus_type) %>% 
  summarise(
    m = mean(is_target_img_chosen, na.rm=TRUE),
    n = n_distinct(subj_name)
  )


m1 <- brm(
  is_target_img_chosen ~ diag_cat * stimulus_type +
    (stimulus_type| subj_name),
  data = dat, 
  family = bernoulli(),
  algorithm = "meanfield",
  # control = list(adapt_delta = 0.95),
  iter = 10000,
  cores = 6,
  backend = "cmdstan"
)

summary(m1)

p <- conditional_effects(
  m1,
  effects = "stimulus_type:diag_cat"
)

p <- plot(p, plot = FALSE)[[1]] +
  xlab("Diagnostic category") +
  ylab("Choice of target stimulus") 
p + scale_colour_colorblind() 



rg <- emmeans::emmeans(m1 , specs = pairwise ~ stimulus_type:diag_cat, adjust = "bonferroni")
# $contrasts
# contrast                estimate lower.HPD upper.HPD
# food AN - neutral AN    -0.18254 -0.225800   -0.1396
# food AN - food BN       -0.17972 -0.231426   -0.1198
# food AN - neutral BN     0.09802 -0.061360    0.2429
# food AN - food HC       -0.14786 -0.185877   -0.1150
# food AN - neutral HC    -0.09816 -0.159036   -0.0342
# neutral AN - food BN     0.00387 -0.069282    0.0719
# neutral AN - neutral BN  0.28114  0.132534    0.4222
# neutral AN - food HC     0.03432 -0.019551    0.0936
# neutral AN - neutral HC  0.08564  0.037120    0.1301
# food BN - neutral BN     0.27763  0.123063    0.4102
# food BN - food HC        0.02990 -0.039617    0.0929
# food BN - neutral HC     0.08155 -0.000903    0.1671
# neutral BN - food HC    -0.24919 -0.398061   -0.0851
# neutral BN - neutral HC -0.19429 -0.351808   -0.0429
# food HC - neutral HC     0.05010  0.001014    0.1044
# 
# Point estimate displayed: median 
# Results are given on the log odds ratio (not the response) scale. 
# HPD interval probability: 0.95 

plot(rg, by = "diag_cat")

