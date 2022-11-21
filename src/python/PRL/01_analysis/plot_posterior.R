library("bayesplot")
library("ggplot2")
library("rstanarm")   
library("tidybayes")
library("ggdist")
library("here")
library("tidyverse")


d <- rio::import(
  here::here("scripts", "python", "PRL", "01_analysis", "traces.csv")
)

glimpse(d)


d$neg_alpha_food_diff <- d$`alpha(AN.food)` - d$`alpha(HC.food)` %>% scale()
d$neg_alpha_neutral_diff <- d$`alpha(AN.neutral)` - d$`alpha(HC.neutral)` %>% scale()

d$pos_alpha_food_diff <- d$`pos_alpha(AN.food)` - d$`pos_alpha(HC.food)` %>% scale()
d$pos_alpha_neutral_diff <- d$`pos_alpha(AN.neutral)` - d$`pos_alpha(HC.neutral)` %>% scale()

d$a_food_diff    <- d$`a(AN.food)`    - d$`a(HC.food)` %>% scale()
d$a_neutral_diff <- d$`a(AN.neutral)` - d$`a(HC.neutral)` %>% scale()


n_gr <- length(d$pos_alpha_neutral_diff)

df1 <- data.frame(
  values = c(
    d$pos_alpha_food_diff, 
    d$pos_alpha_neutral_diff,
    
    d$neg_alpha_food_diff, 
    d$neg_alpha_neutral_diff,
    
    d$a_food_diff, 
    d$a_neutral_diff
  )
)

df2 <- data.frame(
  params = c(
    rep("pos_alpha_food", n_gr), 
    rep("pos_alpha_neutral", n_gr),
    rep("neg_alpha_food", n_gr), 
    rep("neg_alpha_neutral", n_gr),
    rep("a_food", n_gr), 
    rep("a_neutral", n_gr)
  )
)

posts <- bind_cols(df1, df2) 


posts %>% 
  ggplot(aes(x = values, y = params, fill = stat(x < 0))) +
  ggdist::stat_halfeye(.width = c(0.8, 0.95)) +
  geom_vline(xintercept = c(0, 0), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  papaja::theme_apa()






fit <- stan_glm(mpg ~ ., data = mtcars, seed = 1111)
print(fit)
posterior <- as.array(fit)

dimnames(posterior)

n_gr <- 200
df1 <- data.frame(p1 = c(rnorm(n_gr, 10, 2), rnorm(n_gr, 20, 2)))
df2 <- data.frame(
  gr = c(
    rep("a", n_gr), rep("b", n_gr)
  )
)

posts <-
  bind_cols(
    df1,
    df2
  ) 

THR <- 12

posts %>% 
  ggplot(aes(x = p1, y = gr, fill = stat(x < THR))) +
  ggdist::stat_halfeye(.width = c(0.8, 0.95)) +
  geom_vline(xintercept = c(THR, THR), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))
  



