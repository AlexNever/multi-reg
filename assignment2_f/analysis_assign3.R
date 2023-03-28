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
## Read data. All data was pretransformed in excel. Sources are mentioned in the report.
url = "https://raw.githubusercontent.com/AlexNever/multi-reg/main/assignment2_f/"
files = c("coal_2019_btu.csv", "erps_2019.csv", "manufacturing_gdp_capita_pop.csv", "natgas_2019_btu.csv", 
          "nuclear_2019_btu.csv", "petrol_2019_btu.csv", "pro_renew_2019_btu.csv", "area_by_country.csv")

temp = list()

for ( i in files){
  temp[[which(files == i)]] = read.csv(paste(url, i, sep = ""), skip = 1, header = FALSE)
}

pattern <- "(?<=1-)[^-]+(?=-Q)"

for (j in 1:(length(temp))){
  if (temp[[j]][1,1] == "API"){
      temp[[j]][,1] =  str_extract(temp[[j]][,1], pattern)
      colnames(temp[[j]]) <- c("iso", "country", temp[[j]][2,2])
      temp[[j]] = temp[[j]][which(!is.na(temp[[j]][,1])),]
  }
}

colnames(temp[[2]]) <- c("iso", "erp")
colnames(temp[[3]]) <- c("iso", "gdpc", "pop_m")
colnames(temp[[8]]) <- c("iso", "area")


left_join_df <- function(x, y) {
  dplyr::left_join(x, y, by = "iso")
}

joined_df <- Reduce(left_join_df, temp)
df <- joined_df[, c(1,3,4,5,6,8,10,12,14,15)]

rm(temp, joined_df, files, i, j, pattern, url)

# Remove countries w/o ERP
df = df[which(!is.na(df$erp)),]

# Remove countries w/o pop_m
df = df[which(!is.na(df$pop_m)),]

# Replace "--" and NA with 0, define everything as numerical
df <- apply(df, 2, function(x) gsub("--", "0", x))
df <- data.frame(df)
df[is.na(df)] <- 0
colnames(df) <- c("iso,", "coal_pop_m", "erp", "gdpc", "pop_m",  "gas_pop_m", "nuc_pop_m", "oil_pop_m", "ren_pop_m", "area_sqk")
for (i in c(2:10)){
  df[,i] = as.numeric(df[,i])
}

df[,c(2,6:9)] = (df[,c(2,6:9)]*1000000/(df[,5]))

df$erp <- df$erp*100

df["pure_import"] = 0

df[is.na(df)] <- 0

for (i in 1:nrow(df)){
  if (round(sum(df$coal_pop_m[i]+df$gas_pop_m[i]+df$nuc_pop_m[i]+df$oil_pop_m[i]),4) == 0)
  df$pure_import[i] = 1
}

df["id"] = c(1:nrow(df))

df["l_coalcap"] <- log(df$coal_pop_m + 1)
df["l_oilcap"] <- log(df$oil_pop_m + 1)
df["l_gascap"] <- log(df$gas_pop_m + 1)
df["l_nuccap"] <- log(df$nuc_pop_m + 1)
df["l_area"] <- log(df$area_sqk)
df["l_rencap"] <- log(df$ren_pop_m)
df$erp <- log(df$erp)

##### END DATA TRANSFORMATION #####

##### Data Exploration #####

sumtable(df[,c(2,3,6,7,8,9,10)])

#### Histograms
brs <- seq(min(df$ren_pop_m), max(df$ren_pop_m), length.out = 100)

hist(df$ren_pop_m, breaks = brs)

brs <- seq(min(df$l_rencap), max(df$l_rencap), length.out = 20)

hist(df$l_rencap, breaks = brs)
### End Histo


#### Scatterplots

### Renewables vs. erp
p_ren_erp <- ggplot(df, aes_string(y = df$l_rencap, x = df$erp)) +
  geom_point() + xlab("log(ERP)") + ylab("log(Renewables)") 

### Renewables vs. area
p_ren_area <- ggplot(df, aes_string(y = df$l_rencap, x = df$l_area)) +
  geom_point()+ xlab("log(Area in KM^2)") + ylab("") 

### Renewables vs. coal_pop_m
p_ren_coal <- ggplot(df, aes_string(y = df$l_rencap, x = df$l_coalcap)) +
  geom_point() + xlab("log(Coal)") + ylab("") 

### Renewables vs. gas_pop_m
p_ren_gas <- ggplot(df, aes_string(y = df$l_rencap, x = df$l_gascap)) +
  geom_point() + xlab("log(Gas)") + ylab("") 

### Renewables vs. oil_pop_m
p_ren_oil <- ggplot(df, aes_string(y = df$l_rencap, x = df$l_oilcap)) +
  geom_point() + xlab("log(Oil)") + ylab("") 

### Renewables vs.nuc_pop_m
p_ren_nuc <- ggplot(df, aes_string(y = df$l_rencap, x = df$l_nuccap)) +
  geom_point() + xlab("log(Nuclear)") + ylab("log(Renewables)") 

grid.arrange(p_ren_erp, p_ren_area, p_ren_coal, p_ren_nuc, p_ren_gas, p_ren_oil, nrow = 2, ncol = 3)

#### End Scatterplots

#Start models

lm_1 <- lm( l_rencap ~ erp + l_area + l_oilcap + l_gascap + l_coalcap + l_nuccap, data=df)
summ(lm_1, digits = 4)
paste(c(vif(lm_1), 1/(1-summary(lm_1)$r.squared)) )
cooksdlm1 <- ols_plot_cooksd_bar(lm_1)
data
df["res_lm1"] = rstandard(lm_1)
df["fit_lm1"] = fitted(lm_1)
dd_lm1 = density(resid(lm_1))

p_fit_res1 <-  ggplot(data = df, aes(x = fit_lm1, y = res_lm1)) +
                geom_point() +
                geom_abline(intercept = 0, slope = 0, color = "blue") +
                labs(x = "Fitted", y = "Residuals") 
#               + geom_point(data = highlight_o1, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o2, aes(x = x, y = y), color = "purple")

p_density1 <- ggplot(data = data.frame(x = dd_lm1$x, y = dd_lm1$y), aes(x = x, y = y)) +
                  geom_line() +
                  xlab("Residuals") +
                  ylab("Density")

p_res_order1 <-  ggplot(data = df, aes(x = id, y = res_lm1)) +
                geom_point() +
                geom_abline(intercept = 0, slope = 0, color = "blue") +
                labs(x = "Order", y = "Residuals") 

#### START: Normal Probability Plot
qq_plot1 <- ggplot(data = data.frame(residuals = df$res_lm1), aes(sample = df$res_lm1)) +
  stat_qq() +
  xlab("Theoretical Quantiles") +
  ylab("Ordered Values") +
  stat_qq_line()

#### END: Normal Probability Plot

grid.arrange(p_fit_res1, p_density1, p_res_order1, qq_plot1, nrow = 2, ncol = 2)

df["sum_oil_gas"] = log(df$oil_pop_m + df$gas_pop_m + 1)
df["sum_other"] = df$l_coalcap + df$l_nuccap

plot(df$l_gascap, df$l_oilcap)

lm_2 <- lm( l_rencap ~ erp + l_area + sum_oil_gas + l_coalcap + l_nuccap, data=df)
summ(lm_2, digits = 4)
paste(c(vif(lm_2), 1/(1-summary(lm_1)$r.squared)) )
ols_plot_cooksd_bar(lm_2)
df["res_lm2"] = rstandard(lm_2)
df["fit_lm2"] = fitted(lm_2)
dd_lm2 = density(resid(lm_2))

# Outlier investigation
### Highlighting import_only countries

residuals = resid(lm_2)

p_fit_res2 <-  ggplot(data = df, aes(x = fit_lm2, y = res_lm2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Fitted", y = "Residuals") 
#               + geom_point(data = highlight_o2, aes(x = x, y = y), color = "red") + geom_point(data = highlight_o2, aes(x = x, y = y), color = "purple")

p_density2 <- ggplot(data = data.frame(x = dd_lm2$x, y = dd_lm2$y), aes(x = x, y = y)) +
  geom_line() +
  xlab("Residuals") +
  ylab("Density")

p_res_order2 <-  ggplot(data = df, aes(x = id, y = res_lm2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color = "blue") +
  labs(x = "Order", y = "Residuals")

#### START: Normal Probability Plot
qq_plot2 <- ggplot(data = data.frame(residuals = df$res_lm2), aes(sample = df$res_lm2)) +
            stat_qq() +
            xlab("Theoretical Quantiles") +
            ylab("Ordered Values") +
            stat_qq_line()

grid.arrange(p_fit_res2, p_density2, p_res_order2, qq_plot2, nrow = 2, ncol = 2)

anova(lm_1, lm_2)
