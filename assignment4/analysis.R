# Assignment 3 - Multivariate Regression
library(stringr)
library(haven)
library(ggplot2)
library(stringr)
library(moments)
library(jtools)
library(dplyr)
library(vtable)
library(gridExtra)
library(pracma)
library(car)
library(olsrr)
library(DescTools)
library(dynlm)
library(leaps)
library(AICcmodavg)

## Read data. All data was pretransformed in excel. Sources are mentioned in the report.
url = "https://raw.githubusercontent.com/AlexNever/multi-reg/main/assignment4/"
files = c("YAL%20Historical%20Data.csv", "Newcastle%20Coal%20Futures%20Historical%20Data.csv",
          "S%26P_ASX%20200%20Historical%20Data.csv", "Baltic%20Dry%20Index%20Historical%20Data.csv")

lagged = function(v){
  rev(c(rev(v),0)[-1])
}

temp = list()

for ( i in files){
  temp[[which(files == i)]] = read.csv(paste(url, i, sep = ""), skip = 1, header = FALSE)
}


for (i in c(1:length(temp))){
  temp[[i]] = temp[[i]][,c(1,2)]
}

colnames(temp[[1]]) <- c("date", "yal_price")
colnames(temp[[2]]) <- c("date", "coal_price")
colnames(temp[[3]]) <- c("date", "asx_price")
colnames(temp[[4]]) <- c("date", "bdi_price")


left_join_df <- function(x, y) {
  dplyr::left_join(x, y, by = "date")
}

joined_df <- Reduce(left_join_df, temp)
df <- joined_df[,]

rm(temp, joined_df, files, i, j, pattern, url)



# Format date
df$date = as.POSIXct(df$date, format = "%m/%d/%Y")
df <- df[order(df$date, decreasing = FALSE),]

# Some cleaning
df = data.frame((lapply(df, function(x) {gsub(",", "", x)})))
df$yal_price = as.numeric(df$yal_price)
df$coal_price = as.numeric(df$coal_price)
df$asx_price = as.numeric(df$asx_price)
df$bdi_price = as.numeric(df$bdi_price)

# If there is no baltic index for a given week (Christmas Weeks) take last weeks value
df$bdi_price[52] = (df$bdi_price[51] + df$bdi_price[53])/2
df$bdi_price[104] = (df$bdi_price[103] + df$bdi_price[105])/2

# Get changes in price for each variable
returns = function(v){
  c(0,(v[-1]/rev(c(rev(v),0)[-1])[-1]) - 1)
}

df["d_yal"] = returns(df$yal_price)
df["d_coal"] = returns(df$coal_price)
df["d_asx"] = returns(df$asx_price)
df["d_bdi"] = returns(df$bdi_price)
df = df[-1,]

df["time"] = c(1:nrow(df))
df["time2"] = df$time^2


##### END DATA TRANSFORMATION #####

##### Data Exploration #####

sumtable(df[,c(6:11)])

#### Histograms
brs <- seq(min(df$d_yal), max(df$d_yal), length.out = 100)

hist(df$d_yal, breaks = 25)

####### Beautifully distributed!

#### Scatterplots

### Yal vs. Coal
p_yal_coal <- ggplot(df, aes_string(y = df$d_yal, x = df$d_coal)) +
  geom_point() + xlab("Change in Coal Price") + ylab("Change in Yancoal Stock Price") 

### Yal vs. ASX
p_yal_asx <- ggplot(df, aes_string(y = df$d_yal, x = df$d_asx)) +
  geom_point()+ xlab("Change in ASX") + ylab("") 

### Yal vs. BDI
p_yal_bdi <- ggplot(df, aes_string(y = df$d_yal, x = df$d_bdi)) +
  geom_point() + xlab("Change in BDI") + ylab("Change in Yancoal Stock Price (weekly)") 

### Yal vs. Time
p_yal_time <- ggplot(df, aes_string(y = df$d_yal, x = df$time)) +
  geom_point() + xlab("Time") + ylab("") 

grid.arrange(p_yal_coal, p_yal_asx, p_yal_bdi, p_yal_time, nrow = 2, ncol = 2)

#### End Scatterplots

#Start models
lm_1 <- lm(d_yal ~ d_coal + d_asx + d_bdi, data=df)
summary(lm_1)

df["res_lm1"] = c(rstandard(lm_1))
df["fit_lm1"] = c(fitted(lm_1))
dd_lm1 = c(density(resid(lm_1)))

#### Regression Diagnostics
p_fit_res1 <-  ggplot(data = df, aes(x = fit_lm1, y = res_lm1)) +
                geom_point() +
                geom_abline(intercept = 0, slope = 0, color = "blue") +
                labs(x = "Fitted", y = "Residuals") 

p_density1 <- ggplot(data = data.frame(x = dd_lm1$x, y = dd_lm1$y), aes(x = x, y = y)) +
                geom_line() +
                xlab("Residuals") +
                ylab("Density")

p_res_order1 <-  ggplot(data = df, aes(x = time, y = res_lm1)) +
                  geom_point() +
                  geom_abline(intercept = 0, slope = 0, color = "blue") +
                  labs(x = "Order", y = "Residuals") 

qq_plot1 <- ggplot(data = data.frame(residuals = df$res_lm1), aes(sample = df$res_lm1)) +
              stat_qq() +
              xlab("Theoretical Quantiles") +
              ylab("Ordered Values") +
              stat_qq_line()

grid.arrange(p_fit_res1, p_density1, p_res_order1, qq_plot1, nrow = 2, ncol = 2)

#### End: Regression Diagnostics

#### Start: Cook's Distance
df["cooksd_lm1"] <- cooks.distance(lm_1)
cooksd = cooks.distance(lm_1)
plot(df$cooksd_lm1, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance

#Store big leverage point for later.

top_x_outlier <- 1
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))
df_inf = df[influential,]

# Collinearity
paste(c(vif(lm_1), 1/(1-summary(lm_1)$r.squared)) )

# Durbin-Watson Test
acf(df$res_lm1)
durbinWatsonTest(lm_1)

# ACF Plot
acf(df$res_lm1)

#Runs Test
RunsTest(df$res_lm1)

# Model Selection Cp, R^2, adjR^2
cp = leaps(cbind(df$d_coal,df$d_asx,df$d_bdi),df$d_yal,nbest=3, method = "Cp")
adjr2 = leaps(cbind(df$d_coal,df$d_asx,df$d_bdi),df$d_yal,nbest=3,method="adjr2")
r2 = leaps(cbind(df$d_coal,df$d_asx,df$d_bdi),df$d_yal,nbest=3,method="r2")

# Order test
r2[["which"]] == cp[["which"]]
r2[["which"]] == adjr2[["which"]]

aic_models = list()
# AIC, AICc
aic_models[[1]] <- lm(d_yal ~ d_coal, data = df)
aic_models[[2]] <- lm(d_yal ~ d_asx, data = df)
aic_models[[3]] <- lm(d_yal ~ d_bdi, data = df)
aic_models[[4]] <- lm(d_yal ~ d_coal + d_asx, data = df)
aic_models[[5]] <- lm(d_yal ~ d_asx + d_bdi, data = df)
aic_models[[6]] <- lm(d_yal ~ d_coal + d_bdi, data = df)
aic_models[[7]] <- lm(d_yal ~ d_coal + d_asx + d_bdi, data = df)

aic_names = c(1:7)

#AIC
aic = aictab(cand.set = aic_models, modnames = aic_names, second.ord = FALSE)

#AICc
aic_c = aictab(cand.set = aic_models, modnames = aic_names, second.ord = TRUE)

aic["aic_c"] = aic_c$AICc
aic["d_coal d_asx d_bdi"] = 0
aic["Cp"] = 0
aic["r2"] = 0
aic["adjr2"] = 0

for (i in c(1:nrow(aic))){
  aic$`d_coal d_asx d_bdi`[i] = paste(cp[["which"]][i,1],cp[["which"]][i,2],cp[["which"]][i,3], sep = " ") #this is the model inclusion for the first model.
  aic$Cp[i] = cp[["Cp"]][i] # this is the Cp value
  aic$r2[i] = r2[["r2"]][i]
  aic$adjr2[i] = adjr2[["adjr2"]][i]
}

ms = aic[,-c(4:8)]

model_selection = data.frame(ms$Modnames, ms$`d_coal d_asx d_bdi`, ms$AIC, ms$aic_c, ms$Cp, ms$r2, ms$adjr2)


########################################################################Model 2
df_red = df[-c((influential-1):nrow(df)),]

##### Data Exploration #####

sumtable(df_red[,c(6:11)])

#### Histograms
brs <- seq(min(df_red$d_yal), max(df_red$d_yal), length.out = 100)

hist(df_red$d_yal, breaks = 25)

####### Beautifully distributed!

#### Scatterplots

### Yal vs. Coal
p_yal_coal <- ggplot(df_red, aes_string(y = df_red$d_yal, x = df_red$d_coal)) +
  geom_point() + xlab("Change in Coal Price") + ylab("Change in Yancoal Stock Price") 

### Yal vs. ASX
p_yal_asx <- ggplot(df_red, aes_string(y = df_red$d_yal, x = df_red$d_asx)) +
  geom_point()+ xlab("Change in ASX") + ylab("") 

### Yal vs. BDI
p_yal_bdi <- ggplot(df_red, aes_string(y = df_red$d_yal, x = df_red$d_bdi)) +
  geom_point() + xlab("Change in BDI") + ylab("Change in Yancoal Stock Price (weekly)") 

### Yal vs. Time
p_yal_time <- ggplot(df_red, aes_string(y = df_red$d_yal, x = df_red$time)) +
  geom_point() + xlab("Time") + ylab("") 

grid.arrange(p_yal_coal, p_yal_asx, p_yal_bdi, p_yal_time, nrow = 2, ncol = 2)

#### End Scatterplots

#Start models
lm_2 <- lm(d_yal ~ d_coal + d_asx + d_bdi, data=df_red)
summary(lm_2)

df_red["res_lm2"] = c(rstandard(lm_2))
df_red["fit_lm2"] = c(fitted(lm_2))
dd_lm2 = c(density(resid(lm_2)))

#### Regression Diagnostics
p_fit_res1 <-  ggplot(data = df_red, aes(x = fit_lm2, y = res_lm2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Fitted", y = "Residuals") 

p_density1 <- ggplot(data = data.frame(x = dd_lm2$x, y = dd_lm2$y), aes(x = x, y = y)) +
  geom_line() +
  xlab("Residuals") +
  ylab("Density")

p_res_order1 <-  ggplot(data = df_red, aes(x = time, y = res_lm2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Order", y = "Residuals") 

qq_plot1 <- ggplot(data = data.frame(residuals = df_red$res_lm2), aes(sample = df_red$res_lm2)) +
  stat_qq() +
  xlab("Theoretical Quantiles") +
  ylab("Ordered Values") +
  stat_qq_line()

grid.arrange(p_fit_res1, p_density1, p_res_order1, qq_plot1, nrow = 2, ncol = 2)

#### End: Regression Diagnostics

#### Start: Cook's Distance
df_red["cooksd_lm2"] <- cooks.distance(lm_2)
cooksd = cooks.distance(lm_2)
plot(df_red$cooksd_lm2, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance

#Store big leverage point for later.

top_x_outlier <- 1
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))
df_red_inf = df_red[influential,]

# Collinearity
paste(c(vif(lm_2), 1/(1-summary(lm_2)$r.squared)) )

# Durbin-Watson Test
acf(df_red$res_lm2)
durbinWatsonTest(lm_2)

# ACF Plot
acf(df_red$res_lm2)

#Runs Test
RunsTest(df_red$res_lm2)

# Model Selection Cp, R^2, adjR^2
cp = leaps(cbind(df_red$d_coal,df_red$d_asx,df_red$d_bdi),df_red$d_yal,nbest=3, method = "Cp")
adjr2 = leaps(cbind(df_red$d_coal,df_red$d_asx,df_red$d_bdi),df_red$d_yal,nbest=3,method="adjr2")
r2 = leaps(cbind(df_red$d_coal,df_red$d_asx,df_red$d_bdi),df_red$d_yal,nbest=3,method="r2")

# Order test
r2[["which"]] == cp[["which"]]
r2[["which"]] == adjr2[["which"]]

aic_models = list()
# AIC, AICc
aic_models[[1]] <- lm(d_yal ~ d_coal, data = df_red)
aic_models[[2]] <- lm(d_yal ~ d_asx, data = df_red)
aic_models[[3]] <- lm(d_yal ~ d_bdi, data = df_red)
aic_models[[4]] <- lm(d_yal ~ d_coal + d_asx, data = df_red)
aic_models[[5]] <- lm(d_yal ~ d_asx + d_bdi, data = df_red)
aic_models[[6]] <- lm(d_yal ~ d_coal + d_bdi, data = df_red)
aic_models[[7]] <- lm(d_yal ~ d_coal + d_asx + d_bdi, data = df_red)

aic_names = c(1:7)

#AIC
aic = aictab(cand.set = aic_models, modnames = aic_names, second.ord = FALSE)

#AICc
aic_c = aictab(cand.set = aic_models, modnames = aic_names, second.ord = TRUE)

aic["aic_c"] = aic_c$AICc
aic["d_coal d_asx d_bdi"] = 0
aic["Cp"] = 0
aic["r2"] = 0
aic["adjr2"] = 0

for (i in c(1:nrow(aic))){
  aic$`d_coal d_asx d_bdi`[i] = paste(cp[["which"]][i,1],cp[["which"]][i,2],cp[["which"]][i,3], sep = " ") #this is the model inclusion for the first model.
  aic$Cp[i] = cp[["Cp"]][i] # this is the Cp value
  aic$r2[i] = r2[["r2"]][i]
  aic$adjr2[i] = adjr2[["adjr2"]][i]
}

ms = aic[,-c(4:8)]

model_selection = data.frame(ms$Modnames, ms$`d_coal d_asx d_bdi`, ms$AIC, ms$aic_c, ms$Cp, ms$r2, ms$adjr2)

