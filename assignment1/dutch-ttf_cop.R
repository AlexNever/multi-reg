# Exploring the relationship between Dutch TTF price volatility, and $xle (US natural gas exporter)
library(ggplot2)
library(stringr)
library(moments)
setwd("G:/My Drive/2. Stern/1. Academics/2. Semester/3. Regression and Multivariate Data Analysis/Project 1/")

# Loading and transforming data
t_xle_d = read.csv(paste("XLE (Daily, 2022)", ".csv", sep = ""))
t_ttf_d = read.csv(paste("Dutch TTF Natural Gas Futures (Daily, 2022)", ".csv", sep = ""))

t_ = merge(t_xle_d, t_ttf_d, by = "Date")
t_ttf_xle = t_[,c(1, 3, 4, 5, 9)]
colnames(t_ttf_xle) <- c("Date", "price_xle_open", "price_xle_high", "price_xle_low", "price_ttf_open")

# Calculating daily price spread
t_ttf_xle["id_range_perc_xle"] <- (t_ttf_xle$price_xle_high - t_ttf_xle$price_xle_low)/t_ttf_xle$price_xle_open

# Calculating indexed daily change in TTF price
v_ttf_price_ln = log(t_ttf_xle$price_ttf_open)
v_ttf_price_ln_offset = c(0, v_ttf_price_ln)
## I use the absolute value since we are interested in the magnitude of the swings
v_indchange_ttf = abs((v_ttf_price_ln/head(v_ttf_price_ln_offset, -1) - 1))

# Remove first row from data (inf change)

t_ttf_xle["price_ttf_change"] = v_indchange_ttf
# Doing a scatter plot between the two measures
ggplot(data = t_ttf_xle, aes(y = id_range_perc_xle, x = price_ttf_change)) +
                  geom_point() +
                  labs(title = "Scatterplot",
                  x = "Pre-Open Dutch TTF Price Change", y = "Intraday Price Range % of $xle")

ggsave("scatterplot_1.png", width = 8, height = 6, dpi = 300)

# We notice some heteroskedasticity in the plot, but generally we observe a positive relationship
# Lets run the regression now.
t_ttf_xle_f = t_ttf_xle[-1,c(1,6)]
linear_mod <- lm( id_range_perc_xle ~ price_ttf_change, data=t_ttf_xle_f)
summary(linear_mod)

#################################

# Lets try something more interesting
## Is there a relationship between one-day price jumps and the subsequent 5 day volatility in price?
# df_reg2 <- merge(t_, t_ttf_xle, by = "Date")
forward = 4
df_reg2 <- merge(t_, t_ttf_xle, by = "Date")

# Calculating indexed daily change in XLE price
v_xle_price_ln = log(df_reg2[,2])
v_xle_price_ln_offset = c(0, v_xle_price_ln)
## I use the absolute value since we are interested in the magnitude of the swings
v_indchange_xle = (v_xle_price_ln/head(v_xle_price_ln_offset, -1) - 1)

# Remove first row from data (inf change)
# df_reg2 = df_reg2[-1,c(1,6)]

df_reg2["price_xle_change"] = v_indchange_xle
df_reg2["fd_sd_xle"] = 0

for(i in c(2:(length(df_reg2$Date)-forward))) {
  x_sd = sd(df_reg2$price_xle_change[c(i, i+forward)])
  df_reg2$fd_sd_xle[i] = x_sd
}
df_reg2 = df_reg2[-1,]
df_reg2 <- head(df_reg2, - 4) 

ggplot(data = df_reg2, aes(y = fd_sd_xle, x = price_ttf_change)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Pre-Open Dutch TTF Price Change", y = "Intraday Price Range % of $xle")

linear_mod <- lm( log(Price.x) ~ log(Price.y), data=df_reg2)
summary(linear_mod)

df_reg2["volume_xle"] = 0
df_reg2["time"] = 0



for(i in c(1:(nrow(df_reg2)))) {
  df_reg2$volume_xle[i] = as.numeric(str_sub(df_reg2$Vol..x[i], 1, 5))
  df_reg2$time[i] = as.numeric(i)
}

linear_mod <- lm( log(volume_xle) ~ sqrt(price_ttf_change) + time, data=df_reg2)
summary(linear_mod)

ggplot(data = df_reg2, aes(y = log(volume_xle), x = sqrt(price_ttf_change))) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Pre-Open Dutch TTF Price Change", y = "Daily Volume for $XLE")

ggsave("scatterplot_1.png", width = 8, height = 6, dpi = 300)

ggplot(data = df_reg2, aes(y = log(volume_xle), x = time)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Pre-Open Dutch TTF Price Change", y = "Daily Volume for $XLE")

ggplot(data = df_reg2, aes(y = sqrt(price_ttf_change), x = time)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Pre-Open Dutch TTF Price Change", y = "Daily Volume for $XLE")

ggplot(data = df_reg2, aes(y = sqrt(price_xle_change), x = sqrt(price_ttf_change))) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "Pre-Open Dutch TTF Price Change", y = "Daily Volume for $XLE")

linear_mod <- lm( log(volume_xle) ~ sqrt(price_xle_change) + sqrt(price_ttf_change) + time, data=df_reg2)
summary(linear_mod)

residuals = resid(linear_mod)
plot(fitted(linear_mod), residuals)
abline(0,0)
plot(density(residuals))

obs_order <- seq(1, (nrow(df_reg2)-1))
plot(obs_order, residuals, xlab = "Observation Order", ylab = "Residuals")

qqnorm(residuals, xlab = "Theoretical Quantiles", ylab = "Ordered Values")
qqline(residuals) 

skewness(residuals, na.rm = TRUE)



