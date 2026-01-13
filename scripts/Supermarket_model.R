#The first research question:

#Is there a significant difference in total product price between the 3 different cities.
#The differences could indicate; different consumer behavior between the cities, 
#maybe consumer income or the supermarket's different locations in city.

library(readr)
library(rstan)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(posterior)
library(bayesplot)
library(bayestestR)
library(ggdist)


#Loading the data set from github.
url <- "https://raw.githubusercontent.com/sersun/supermarket-sales-analysis/main/supermarket_sales.csv"
df <- read_csv(url)

#Preparing the data for Stan
#Giving each city a number. Mandalay(1), Naypyitaw(2), Yangon(3).
city_data <- df %>%
  select(City, Total) %>%
  mutate(city_numeric = as.integer(factor(City, levels = c("Mandalay", "Naypyitaw", "Yangon")))) %>%
  na.omit(Total)
table(city_data$City)

data_list <- list(
  N = nrow(city_data),
  K = length(unique(city_data$city_numeric)),
  city = city_data$city_numeric,
  total = city_data$Total
)

#MCMC
model <- stan_model("supermarket_model.stan")

#Sampling draws for the posterior (applying MCMC)
fit <- sampling(model,
                data = data_list,
                chains = 4,
                iter = 4000,
                warmup = 400,
                thin = 1,
                seed = 444)

as_draws_array(fit) |> dim() #Check out the dimensions
as_draws_array(fit) #Check out the chains

#Extracting estimates
grid <- as_draws_rvars(fit)["log_mu"]
grid$mu <- exp(grid$log_mu)
grid

#Model fit check
#Check out the chains
grid |> mcmc_trace(pars = c("mu[1]", "mu[2]", "mu[3]"))
grid |> mcmc_trace(pars = c("log_mu[1]", "log_mu[2]", "log_mu[3]"))

#Examining the models Rhat and ESS
posterior::rhat(grid$mu) 
(ess_mean(grid$mu)) 
(mcse_mean(grid$mu))

#Autocorrelation of posterior Samples by City
mcmc_acf(as_draws_matrix(grid$mu), lags = 30)
acf(draws_of(grid$mu[1]), main = "ACF for mu 1 (all chains combined)")
acf(draws_of(grid$mu[2]), main = "ACF for mu 2 (all chains combined)")
acf(draws_of(grid$mu[3]), main = "ACF for mu 3 (all chains combined)")

#Describe the posterior
point_estimate(grid["mu"])

grid_4_plot = data.frame(
  mu = grid$mu,
  city = c("Mandalay", "Naypyitaw", "Yangon")
)

ggplot(grid_4_plot, aes(y = city, xdist = mu, fill = city)) +
  stat_slab(alpha = 0.6) +
  scale_fill_brewer(palette = "Pastel1")+
  stat_spike(at = list(
    "mean", "median", "Mode")) +
  labs(
    title = "Posterior Distributions of Average (mu) Purchase by City",
    x = expression(mu),
    y = NULL)+ theme_minimal()

#Credible Intervals
describe_posterior(grid["mu"], ci = 0.95, ci_method = "HDI", test = NULL)


#This plot illustrates the High Density Continuous Interval range
ggplot(grid_4_plot, aes(y = city)) +
  stat_slab(aes(xdist = mu, fill = after_stat(level)),
            point_interval = "median_hdci",
            .width = c(0.5, 0.8, 0.95, 1)) +
  labs(
    title = "Posterior HDI of Average Purchase",
    x = "Average Price ($)",
    y = NULL)+
  theme_minimal()

#Equal Tail Interval
describe_posterior(grid["mu"], ci_method = "eti", test = NULL)

ggplot(grid_4_plot, aes(y = city)) +
  stat_slab(aes(xdist = mu, fill = after_stat(level)),
            point_interval = "median_qi",
            .width = c(0.5, 0.8, 0.95, 1)) +
  labs(
    title = "Posterior Median and Equal Tailed Intervals (ETI)",
    x = "Average Price ($)",
    y = NULL)+
  theme_minimal()


#DGP
#We assumed that the total price $ (Total) is normally distributed.
#Using log scale allows to keep the values of the purchase positive.
#Sigma is a nuisance parameter.

#DIFF
#Remainder -  Mandalay(1), Naypyitaw(2), Yangon(3)
grid$diff_MN <- grid$log_mu[1] - grid$log_mu[2]
grid$diff_MY <- grid$log_mu[1] - grid$log_mu[3]

(rope_result <- rope(grid["diff_MN"], range = c(-5, 5)))
(rope_result <- rope(grid["diff_MY"], range = c(-5, 5)))

(pd_result <- p_direction(grid["diff_MN"]))
(pd_result <- p_direction(grid["diff_MY"]))

(pmap_result <- p_map(grid["diff_MN"]))
(pmap_result <- p_map(grid["diff_MY"]))

ggplot() +
  stat_slab(aes(xdist = grid$diff_MN),
            fill = "Red", alpha = 0.5) +
  geom_rect(aes(xmin = -5, xmax = 5, ymin = 0, ymax = Inf),
            fill = "grey80", alpha = 0.6) +
  labs(title = "Posterior Distribution of Log Mean Difference: Mandalay vs Naypyitaw",
       x = "Difference", y = "Density") +
  theme_minimal()

ggplot() +
  stat_slab(aes(xdist = grid$diff_MY),
            fill = "Blue", alpha = 0.5) +
  geom_rect(aes(xmin = -5, xmax = 5, ymin = 0, ymax = Inf),
            fill = "grey80", alpha = 0.6) +
  labs(title = "Posterior Distribution of Log Mean Difference: Mandalay vs Yangon",
       x = "Difference", y = "Density") +
  theme_minimal()



#Research question with the use of PPD:

#How does gender affect total purchase price within the Food and beverages product line?
#We choose to focus on the "Food and beverages" product line and dividing customers by gender.
#We wanted to examine how this demographic factor influences purchasing behavior.

library(patchwork)          
library(scales)


#Filtering the data to only food and beverages
df_filtered <- df %>%
  filter(`Product line` == "Food and beverages") %>%
  mutate(
    male = if_else(Gender == "Male", 1L, 0L),
    log_total = log(as.numeric(Total))
  ) %>%
  drop_na(male, log_total)

stan_data <- list(
  N = nrow(df_filtered),
  log_total = df_filtered$log_total,
  male = as.integer(df_filtered$male)
)

#MCMC
model <- stan_model("PPD_super.stan")

fit <- sampling(model, 
                data = stan_data, 
                chains = 4, 
                iter = 2000, 
                warmup = 500,
                thin = 1,
                seed = 1)

print(fit, pars = c("alpha", "beta_male", "sigma")) 
#Alpha is the expected value for females (log scale).
#Beta_male is the impact of male - female on the log-total.
#Sigma is the residual Standard Deviation (log scale).


#Model fit check
#Transforming from log scale to regular dollar scale.
grid <- as_draws_rvars(fit)[c("alpha","beta_male","sigma")]
grid$mid_dollars_for_female <- exp(grid$alpha)  #Posterior median of total purchase sum (in Dollars) for Females
grid$mid_dollars_for_male <- exp(grid$alpha+grid$beta_male) #Posterior median of total purchase sum (in Dollars) for Males
grid$sigma <- as_draws_rvars(fit)$sigma

#Examining the models Rhat and ESS
posterior::rhat(grid$alpha)
posterior::rhat(grid$beta_male)
posterior::rhat(grid$sigma)

(ess_mean(grid$alpha)) 
(ess_mean(grid$beta_male)) 
(ess_mean(grid$sigma))

#Examining MCSE
(mcse_mean(grid$alpha))
(mcse_mean(grid$beta_male))
(mcse_mean(grid$sigma))


#Posterior means (log-scale) for females, males, and residual SD.
params <- as_draws_rvars(fit)
mu_female <- params$alpha
mu_male <- params$alpha + params$beta_male
sigma_rv <- params$sigma

#Posterior of the mean (Dollar scale) for each gender.
mean_female_dollars <- exp(mu_female + (sigma_rv^2)/2)
mean_male_dollars <- exp(mu_male   + (sigma_rv^2)/2)

#Plot the two posterior distributions 
plot_df <- tibble(
  gender = factor(c("Female","Male"), levels = c("Female","Male")),
  mean_dollars = c(mean_female_dollars, mean_male_dollars)
)

ggplot(plot_df, aes(y = gender, xdist = mean_dollars, fill = gender)) +
  stat_slabinterval(point_interval = median_qi, .width = c(.5, .95)) +
  labs(
    title = "Posterior of Expected Spending by Gender",
    x = "Expected total ($)", y = NULL
  ) +
  scale_fill_manual(values = c("Male" = "lightgreen", "Female" = "pink")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#HDI
desc_female <- describe_posterior(draws_of(mean_female_dollars), ci = 0.80, ci_method = "HDI")
desc_male <- describe_posterior(draws_of(mean_male_dollars),   ci = 0.80, ci_method = "HDI")

desc_table <- bind_rows(
  mutate(desc_female, gender = "Female"),
  mutate(desc_male,   gender = "Male")
)
desc_table

#Median + HDI at multiple widths (50%, 80%, 95%)
mhdi_f <- median_hdi(draws_of(mean_female_dollars), .width = c(0.5, 0.8, 0.95)) %>%
  mutate(gender = "Female")
mhdi_m <- median_hdi(draws_of(mean_male_dollars),   .width = c(0.5, 0.8, 0.95)) %>%
  mutate(gender = "Male")

mhdi_table <- bind_rows(mhdi_f, mhdi_m)
mhdi_table #It gives us  medians + 50%, 80%, 95% HDI's.


ggplot(plot_df, aes(y = gender)) +
  stat_slab(
    aes(xdist = mean_dollars, fill = after_stat(level)),
    point_interval = "median_hdci",
    .width = c(0.5, 0.8, 0.95, 1)
  ) +
  labs(
    title = "Posterior HDI for Average Spending by Gender",
    x = "Expected Total ($)",
    y = NULL,
    fill = "HDI level"
  ) +
  theme_minimal(base_size = 14)

#The HDI analysis provides a clear summary of the uncertainty around expected spending for each gender. 
#The median expected spending for females is around 362 dollars (80% HDI: [316, 404]), while for males it is about 295 dollars (80% HDI: [257, 334]). 
#The intervals for both groups are wide and overlapping, 
#which indicates that we cannot confidently distinguish  differences in expected spending between male and female customers.
#The plot illustrates this by showing that the HDI bands for the two groups overlap , 
#reinforcing that gender does not have a strong predictive effect in this product line.


#we chose to report the median together with the HDI as a summary of the posterior. 
#The median serves as a robust measure of central tendency and less sensitive to skew or long tails than the mean.


#PPD
#Simulate PPD for ONE new customer per draw, then exponentiate dollars
y_new_female_dollar <- exp(rvar_rng(rnorm, n = 1, mean = params$alpha,
                                    sd = params$sigma))
y_new_male_dollar   <- exp(rvar_rng(rnorm, n = 1, mean = params$alpha + params$beta_male,
                                    sd = params$sigma))


ppd_rvar <- tibble(
  gender = factor(c("Female","Male"), levels = c("Female","Male")),
  spending = c(y_new_female_dollar, y_new_male_dollar)  #rvar columns
)


ggplot(ppd_rvar, aes(y = gender, xdist = spending, fill = gender)) +
  stat_slabinterval(point_interval = median_qi, .width = c(0.5, 0.95)) +
  labs(
    title = "PPD for a New Customer (dollar scale)",
    x = "Predicted Total ($)", y = NULL
  ) +
  scale_fill_manual(values = c(Female = "pink", Male = "lightgreen")) +
  coord_cartesian(xlim = c(0, 500))+ #There is extreme values so we wanted to zoom in on this values. if you want to see the full graph just add # to the start of this line code
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


#Analyzing the results of PPD:
set.seed(1)
draws <- as_draws_df(fit)
new_male <- exp(rnorm(nrow(draws), mean = draws$alpha + draws$beta_male, sd = draws$sigma))
new_female <- exp(rnorm(nrow(draws), mean = draws$alpha,sd = draws$sigma))

prob_male_gt_female <- mean(new_male > new_female)
prob_male_gt_female
#The result is 0.44 which means there is ~56% that women tends to spend more


diff_draws <- new_male - new_female
ci <- quantile(diff_draws, c(0.025, 0.975))
p_gt0 <- mean(diff_draws > 0)

ggplot(tibble(difference = diff_draws), aes(x = difference)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  geom_area(
    data = subset(tibble(difference = diff_draws),
                  difference > ci[1] & difference < ci[2]),
    aes(y = ..density..),
    stat = "density", fill = "blue", alpha = 0.3
  ) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = ci, color = "darkblue", linetype = "dotted", size = 1) +
  labs(
    title = "Predictive Difference in Spending (Male − Female)",
    subtitle = paste0("Pr(Male > Female) ≈ ", round(p_gt0, 3),
                      "   |   95% CI: [", round(ci[1], 1), ", ", round(ci[2], 1), "]"),
    x = "Difference in Predicted Total ($)",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)

hdi_95 <- hdi(diff_draws, ci = 0.95)
hdi_95


#PPC
set.seed(123)

#Subset to 50 draws for plotting using less "computer power",it took my PC long time to calculate and plot.
params50 <- params |> subset_draws(draw = 1:50)

#Design matrix: Intercept + male
X <- cbind(Intercept = 1, Male = df_filtered$male)

beta_rv <- c(params50$alpha, params50$beta_male)

#Mean on log scale (rvar length N)
mu_log_rv <- as.vector(X %**% beta_rv)

#Simulate replicated datasets on Log scale 
yrep_log_rv <- rvar_rng(rnorm, n = nrow(df_filtered), mean = mu_log_rv, sd = params50$sigma)
PPD_matrix <- draws_of(yrep_log_rv)   

#Observed log data
y_log <- as.vector(df_filtered$log_total)


#PPC density overlay (log scale)
bayesplot::ppc_dens_overlay(y = y_log, yrep = PPD_matrix) +
  ggtitle("PPC Density Overlay (log scale)")

#Grouped overlay (log scale)
group_fac <- factor(ifelse(df_filtered$male == 1, "Male", "Female"), levels = c("Female","Male"))
bayesplot::ppc_dens_overlay_grouped(y = y_log, yrep = PPD_matrix, group = group_fac) +
  ggtitle("PPC Density Overlay by Gender (log scale)")

