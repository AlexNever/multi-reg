## Project 1 - Assessing the relationship between US-EU Natural Gas Arbitrage (TTF-Henry Hub) and Energy Select Sector SPDR Fund (XLE) Prices
library(ggplot2)
library(stringr)
library(moments)
library(jtools)
library(dplyr)
library(qqplotr)

# Loading and transforming data
t_xle_d = read.csv("https://raw.githubusercontent.com/AlexNever/multi-reg/main/assignment1/XLE.csv")
t_ttf_d = read.csv("https://raw.githubusercontent.com/AlexNever/multi-reg/main/assignment1/TTF.csv")
t_hhb_d = read.csv("https://raw.githubusercontent.com/AlexNever/multi-reg/main/assignment1/HHB.csv")

t_temp = merge(t_xle_d, t_ttf_d, by = "Date")
t_ = merge(t_temp, t_hhb_d, by = "Date")
colnames(t_) = c("date", "c.xle", "o.xle", "h.xle", "l.xle", "vol.xle", "change.xle", 
                         "c.ttf", "o.ttf", "h.ttf", "l.ttf", "vol.ttf", "change.ttf",
                         "c.hhb", "o.hhb", "h.hhb", "l.hhb", "vol.hhb", "change.hhb")

t_df <- t_

#Order by Date
t_df = t_df[order(as.Date(t_df$date, format="%m/%d/%Y")),]

rm(t_, t_temp, t_xle_d, t_ttf_d, t_hhb_d)

# Calculate Arbitrage. 
mmBTU_MWh = 1/0.293071
USD_EUR = 1.06

# Express the Henry Hub price in terms of TTF (EUR/MWH)
t_df["hbb.price.conv"] = ((mmBTU_MWh)*t_df$c.hhb)/USD_EUR

# Calculate difference between Henry Hub and TTF
t_df["arb.price"] = (t_df$c.ttf - t_df$hbb.price.conv)*USD_EUR

#Converting the data to weekly frequency
t_df_week = t_df

t_df_week["year_week"] = paste(format(as.POSIXct(t_df_week$date, format = "%m/%d/%Y"), format = "%Y"),
                               strftime(as.POSIXlt(t_df_week$date, format = "%m/%d/%Y"), format = "%V"),
                               sep = "")

t_df_week["id"] = c(1:nrow(t_df_week))

temp_list = list()
count = 0
for (i in t_df_week$year_week){
  count = count + 1
  df_temp = t_df_week[which(t_df_week$year_week == i),]
  week_close = df_temp[which(df_temp$id == max(df_temp$id)),]
  temp_list[[count]] <- week_close
}

t_df_week <- unique(as.data.frame(do.call(rbind, temp_list)))

# Calculating weekly change in arbitrage price
arb.offset = c(0, t_df_week$arb.price)
arb.change <- (t_df_week$arb.price/head(arb.offset, -1))-1
t_df_week["arb.change"] = arb.change*100

# Calculating weekly change in XLE price
xle.offset = c(0, t_df_week$c.xle)
xle.change <- (t_df_week$c.xle/head(xle.offset, -1))-1
t_df_week["xle.change"] = xle.change*100

# Remove first row from data (inf change)
t_df_week <- t_df_week[-1,]
t_df_week$id = c(1:nrow(t_df_week))

rm(df_temp, t_df, temp_list, week_close, arb.change, arb.offset, count, i, mmBTU_MWh, USD_EUR, xle.change, xle.offset)

#####################
## END OF DATA ETL ##
#####################

################################################################
## Preparing DF for all three regressions, isolating outliers ##
################################################################

t_df_reg1 = t_df_week

t_df_reg2 = t_df_week[-which(t_df_week$arb.change == max(t_df_week$arb.change)),]
outlier1 = t_df_week[which(t_df_week$arb.change == max(t_df_week$arb.change)),]
outlier2 = t_df_reg2[which(t_df_reg2$arb.change == max(t_df_reg2$arb.change)),]
t_df_reg2 = t_df_reg2[-which(t_df_reg2$arb.change == max(t_df_reg2$arb.change)),]


outlier_weeks <- c(outlier1$date[1], outlier2$date[1])

# After Explosion
t_df_reg3 = t_df_reg2[which(t_df_reg2$date > outlier_weeks[2]),]



rm(t_df_week,outlier1,outlier2)
###################
## Regression 1 ###
###################

# Scatterplot
ggplot(data = t_df_reg1, aes(y = xle.change, x = arb.change)) +
  geom_point() +
  labs(title = "Scatterplot - Weekly Frequency",
       x = "TTF - Henry Hub Arbitrage % Change", y = "XLE Price % Change")
      #+ geom_smooth(method='lm', formula= y~x)

# Regression output
lm_1 <- lm( xle.change ~ arb.change, data=t_df_reg1)
summ(lm_1, digits = 4)

# Residuals
residuals = resid(lm_1)
labels <- t_df_reg1$date

#### START: Fitted vs. Residuals
highlight_o1 <- subset(data.frame(x = fitted(lm_1), y = residuals, labels = labels), labels %in% outlier_weeks[1])
highlight_o2 <- subset(data.frame(x = fitted(lm_1), y = residuals, labels = labels), labels %in% outlier_weeks[2])

p <-  ggplot(data = data.frame(x = fitted(lm_1), y = residuals, labels = labels), aes(x = x, y = y)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 0, color = "blue") +
      labs(x = "Fitted", y = "Residuals") 

p + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o2, aes(x = x, y = y), color = "purple")
#### END: Fitted vs. Residuals


#### START: Density Plot
plot(hist(residuals))
#### END: Density Plot

#### START: Residuals vs. Order
highlight_o1 <- subset(data.frame(x = t_df_reg1$id, y = residuals, labels = labels), labels %in% outlier_weeks[1])
highlight_o2 <- subset(data.frame(x = t_df_reg1$id, y = residuals, labels = labels), labels %in% outlier_weeks[2])

p <-  ggplot(data = data.frame(x = t_df_reg1$id, y = residuals, labels = labels), aes(x = x, y = y)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 0, color = "blue") +
      labs(x = "Order", y = "Residuals") 

p + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o2, aes(x = x, y = y), color = "purple")
#### END: Residuals vs. Order


#### START: Normal Probability Plot
qqnorm(residuals, main = "Normal Probability Plot", xlab = "Normal", ylab = "Residuals")
qqline(residuals, col = "red")
#### END: Normal Probability Plot

rm(highlight_o1, highlight_o2, lm_1, p, labels, residuals, t_df_reg1)

###################
## Regression 2 ###
###################

# Scatterplot
ggplot(data = t_df_reg2, aes(y = xle.change, x = arb.change)) +
  geom_point() +
  labs(title = "Scatterplot - Weekly Frequency",
       x = "TTF - Henry Hub Arbitrage % Change", y = "XLE Price % Change")
#+ geom_smooth(method='lm', formula= y~x)

# Regression output
lm_2 <- lm( xle.change ~ arb.change, data=t_df_reg2)
summ(lm_2, digits = 4)

# Residuals
residuals = resid(lm_2)
labels <- t_df_reg2$date

#### START: Fitted vs. Residuals
highlight_o1 <- subset(data.frame(x = fitted(lm_2), y = residuals, labels = labels), labels %in% outlier_weeks[1])
highlight_o2 <- subset(data.frame(x = fitted(lm_2), y = residuals, labels = labels), labels %in% outlier_weeks[2])

p <-  ggplot(data = data.frame(x = fitted(lm_2), y = residuals, labels = labels), aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Fitted", y = "Residuals") 

p + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o2, aes(x = x, y = y), color = "purple")
#### END: Fitted vs. Residuals


#### START: Density Plot
plot(density(residuals))
#### END: Density Plot

#### START: Residuals vs. Order
highlight_o1 <- subset(data.frame(x = t_df_reg2$id, y = residuals, labels = labels), labels %in% outlier_weeks[1])
highlight_o2 <- subset(data.frame(x = t_df_reg2$id, y = residuals, labels = labels), labels %in% outlier_weeks[2])

p <-  ggplot(data = data.frame(x = t_df_reg2$id, y = residuals, labels = labels), aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Order", y = "Residuals") 

p + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o2, aes(x = x, y = y), color = "purple")
#### END: Residuals vs. Order


#### START: Normal Probability Plot
qqnorm(residuals, xlab = "Theoretical Quantiles", ylab = "Ordered Values")
qqline(residuals)
#### END: Normal Probability Plot

###################
## Regression 3 ###
###################

# Scatterplot
ggplot(data = t_df_reg3, aes(y = xle.change, x = arb.change)) +
  geom_point() +
  labs(title = "Scatterplot - Weekly Frequency",
       x = "TTF - Henry Hub Arbitrage % Change", y = "XLE Price % Change")
#+ geom_smooth(method='lm', formula= y~x)

# Regression output
lm_3 <- lm( xle.change ~ arb.change, data=t_df_reg3)
summ(lm_3, digits = 4)

# Residuals
residuals = resid(lm_3)
labels <- t_df_reg3$date

#### START: Fitted vs. Residuals
highlight_o1 <- subset(data.frame(x = fitted(lm_3), y = residuals, labels = labels), labels %in% outlier_weeks[1])
highlight_o3 <- subset(data.frame(x = fitted(lm_3), y = residuals, labels = labels), labels %in% outlier_weeks[3])

p <-  ggplot(data = data.frame(x = fitted(lm_3), y = residuals, labels = labels), aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Fitted", y = "Residuals") 

p + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o3, aes(x = x, y = y), color = "purple")
#### END: Fitted vs. Residuals


#### START: Density Plot
plot(density(residuals))
#### END: Density Plot

#### START: Residuals vs. Order
highlight_o1 <- subset(data.frame(x = t_df_reg3$id, y = residuals, labels = labels), labels %in% outlier_weeks[1])
highlight_o3 <- subset(data.frame(x = t_df_reg3$id, y = residuals, labels = labels), labels %in% outlier_weeks[3])

p <-  ggplot(data = data.frame(x = t_df_reg3$id, y = residuals, labels = labels), aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Order", y = "Residuals") 

p + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o3, aes(x = x, y = y), color = "purple")
#### END: Residuals vs. Order


#### START: Normal Probability Plot
qqnorm(residuals, main = "Normal Probability Plot", xlab = "Normal", ylab = "Residuals")
qqline(residuals, col = "red")
#### END: Normal Probability Plot
