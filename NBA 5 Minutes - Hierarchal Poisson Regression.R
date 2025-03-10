# install.packages('rjags')
# install.packages('coda')
# install.packages('corrplot')
# install.packages('dplyr')

library(rjags)
library(coda)
library("corrplot")
library(dplyr)

### Exploratory Data Analysis and Visualizations
# download data
nba5min = read.csv("Data/DifferentialNBAData.csv",header=TRUE)

head(nba5min)
str(nba5min)

## scatterplots
plot.new()
par(mfrow = c(3, 2), ask = TRUE)
x_columns = 4:21
y_columns = 24:25

for (y_col in y_columns) {
  for (x_col in x_columns) {
    plot(
      nba5min[, x_col],
      nba5min[, y_col],
      xlab = colnames(nba5min)[x_col],
      ylab = colnames(nba5min)[y_col],
      main = paste(colnames(nba5min)[x_col], "vs", colnames(nba5min)[y_col]),
      pch = 19, col = "blue"
    )
  }
}

par(mfrow = c(1, 1), ask = FALSE)

# exclude `date`
Cor = cor(nba5min[,-1])
corrplot(Cor, method = "circle")

# create the interaction terms and transform the variables
transformations <- function(data) {
  # Loop through selected columns
  for (col1 in colnames(data)[4:21]) {
    for (col2 in colnames(data)[4:21]) {
      # Skip when the columns are the same
      if (col1 == col2) {
        next
      }
      
      ## Transformations for col1
      data[[paste0(col1, "^2")]] = data[[col1]]^2
      data[[paste0("LogOf", col1)]] = log(abs(data[[col1]]) + 1e-8)
      data[[paste0("Inverse", col1)]] = 1 / (data[[col1]] + 1e-8)
      data[[paste0("SQRT(", col1, ")")]] = sqrt(abs(data[[col1]]) + 1e-8)

      ## Interaction terms
      # 2nd-degree polynomial
      data[[paste0(col1, "^2*", col2, "^2")]] = (data[[col1]]^2) * (data[[col2]]^2)
      # Powers
      data[[paste0(col1, "^2/", col2)]] = (data[[col1]]^2) / (data[[col2]] + 1e-8)
      data[[paste0(col1, "^2*", col2)]] = (data[[col1]]^2) * data[[col2]]
      data[[paste0(col1, "^", col2)]] = data[[col1]]^(data[[col2]] + 1e-8)
      # Log
      data[[paste0("LogOf", col1, "/", col2)]] = log(abs(data[[col1]]) + 1e-8) / (data[[col2]] + 1e-8)
      data[[paste0("LogOf", col1, "/", col2, "^2")]] = log(abs(data[[col1]]) + 1e-8) / ((data[[col2]]^2) + 1e-8)
      data[[paste0(col1, "/LogOf", col2)]] = data[[col1]] / (log(abs(data[[col2]]) + 1e-8))
      data[[paste0(col1, "^2/LogOf", col2)]] = (data[[col1]]^2) / (log(abs(data[[col2]]) + 1e-8))
      data[[paste0("LogOf", col1, "*", col2)]] = log(abs(data[[col1]]) + 1e-8) * data[[col2]]
      data[[paste0("LogOf", col1, "*", col2, "^2")]] = log(abs(data[[col1]]) + 1e-8) * (data[[col2]]^2)
      data[[paste0(col1, "*LogOf", col2)]] = data[[col1]] * log(abs(data[[col2]]) + 1e-8)
      data[[paste0(col1, "^2*LogOf", col2)]] = (data[[col1]]^2) * log(abs(data[[col2]]) + 1e-8)
      # Inverse
      data[[paste0("Inverse", col1, "/LogOf", col2)]] = (1 / (data[[col1]] + 1e-8)) / log(abs(data[[col2]]) + 1e-8)
      data[[paste0("Inverse", col1, "*LogOf", col2)]] = (1 / (data[[col1]] + 1e-8)) * log(abs(data[[col2]]) + 1e-8)
      data[[paste0("Inverse", col1, "*", col2, "^2")]] = (1 / (data[[col1]] + 1e-8)) * (data[[col2]]^2)
      data[[paste0("Inverse", col1, "*", col2)]] = (1 / (data[[col1]] + 1e-8)) * data[[col2]]
      # Sqrt
      data[[paste0("SQRT", col1, "/", col2, "^2")]] = sqrt(abs(data[[col1]]) + 1e-8) / ((data[[col2]]^2) + 1e-8)
      data[[paste0("SQRT", col1, "/", col2)]] = sqrt(abs(data[[col1]]) + 1e-8) / (data[[col2]] + 1e-8)
      data[[paste0(col1, "^2/SQRT", col2)]] = (data[[col1]]^2) / sqrt(abs(data[[col2]]) + 1e-8)
      data[[paste0(col1, "/SQRT", col2)]] = data[[col1]] / sqrt(abs(data[[col2]]) + 1e-8)
      data[[paste0("SQRT", col1, "*LogOf", col2)]] = sqrt(abs(data[[col1]]) + 1e-8) * log(abs(data[[col2]]) + 1e-8)
      data[[paste0("SQRT", col1, "*", col2, "^2")]] = sqrt(abs(data[[col1]]) + 1e-8) * (data[[col2]]^2)
      data[[paste0("SQRT", col1, "*", col2)]] = sqrt(abs(data[[col1]]) + 1e-8) * data[[col2]]
      data[[paste0("LogOf", col1, "*SQRT", col2)]] = log(abs(data[[col1]]) + 1e-8) * sqrt(abs(data[[col2]]) + 1e-8)
      data[[paste0(col1, "^2*SQRT", col2)]] = (data[[col1]]^2) * sqrt(abs(data[[col2]]) + 1e-8)
    }
  }
  return(data)
}
## median thresholds and differences based on lambda windows # ChatGPT come up with the design here
lambdaWindowMedianData = function(data) {
  for (col in colnames(data)[4:21]) {
    # Binary column for whether value is above/below lambda window median
    data[[paste0("LamWin_MedianThreshold_", col)]] = with(data,
      ifelse(
        data[[col]] > ave(data[[col]], data[["LambdaWID"]], FUN = function(x) median(x, na.rm = TRUE)),
        1,
        0
      )
    )
    
    # Difference from median column
    data[[paste0("LamWin_DifferenceFromMedian_", col)]] = with(data,
      data[[col]] - ave(data[[col]], data[["LambdaWID"]], FUN = function(x) median(x, na.rm = TRUE))
    )
  }
  return(data)  # Return the modified dataset
}

robustNBA = transformations(nba5min)
robustNBA = lambdaWindowMedianData(robustNBA)

# check column length
ncol(robustNBA)

# true to its name, robustNBA was a bit too robust for R 
# batch the correlations and then collect the results
# corrplot above threshold to analyze most correlating factors

cor_col = character(0)
bgn_idx = 2
step_size = 20
total_cols = ncol(robustNBA)
key_columns = c("PointsScoredRemaining_Home", "PointsScoredRemaining_Away")
key_indices = match(key_columns, colnames(robustNBA))

while (bgn_idx <= total_cols) {
  end_idx = min(bgn_idx + step_size - 1, total_cols)
  Cor = cor(robustNBA[, c(bgn_idx:end_idx, key_indices)], use = "complete.obs")
  threshold = 0.1
  columns = colnames(robustNBA[, bgn_idx:end_idx])[apply(
    abs(Cor[, (ncol(Cor) - length(key_columns) + 1):ncol(Cor)]) > threshold, 1, any
  )]
  cor_col = unique(c(cor_col, columns))
  bgn_idx = bgn_idx + step_size
}

cor_col = unique(na.omit(c(cor_col, key_columns)))
# cor_col
filtered_data = robustNBA[, cor_col]

# from eda w/ boxplots
filtered_data = filtered_data %>%
  filter(LambdaWID != 15)
filtered_data = filtered_data %>%
  filter(!(LambdaWID == 1 & PointsScoredRemaining_Home > 6))

## Boxplot and overdispersion
# should show a decreasing relationship of points to be scored in later time windows

grouped_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_away = mean(PointsScoredRemaining_Away, na.rm = TRUE),
    var_away = var(PointsScoredRemaining_Away, na.rm = TRUE)
  )
grouped_stats

grouped_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_home = mean(PointsScoredRemaining_Home, na.rm = TRUE),
    var_home = var(PointsScoredRemaining_Home, na.rm = TRUE)
  )
grouped_stats

boxplot(PointsScoredRemaining_Home ~ LambdaWID, data=nba5min)
points(jitter(as.numeric(nba5min$LambdaWID)), nba5min$PointsScoredRemaining_Home,
       col = "red", pch = 16, cex = 0.5)

boxplot(PointsScoredRemaining_Away ~ LambdaWID, data=nba5min)
points(jitter(as.numeric(nba5min$LambdaWID)), nba5min$PointsScoredRemaining_Away,
       col = "blue", pch = 16, cex = 0.5)

nba5min = nba5min %>%
  filter(LambdaWID != 15)

nba5min = nba5min %>%
  filter(!(LambdaWID == 1 & PointsScoredRemaining_Home > 6))

# transformations:
# log
log_grouped_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_away = mean(log(abs(PointsScoredRemaining_Away+1e-8)), na.rm = TRUE),
    var_away = var(log(abs(PointsScoredRemaining_Away+1e-8)), na.rm = TRUE)
  )
log_grouped_stats

log_grouped_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_home = mean(log(abs(PointsScoredRemaining_Home+1e-8)), na.rm = TRUE),
    var_home = var(log(abs(PointsScoredRemaining_Home+1e-8)), na.rm = TRUE)
  )
log_grouped_stats

# square root
sqrt_grouped_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_away = mean(sqrt(abs(PointsScoredRemaining_Away+1e-8)), na.rm = TRUE),
    var_away = var(sqrt(abs(PointsScoredRemaining_Away+1e-8)), na.rm = TRUE)
  )
sqrt_grouped_stats
# square root
sqrt_grouped_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_home = mean(sqrt(abs(PointsScoredRemaining_Home+1e-8)), na.rm = TRUE),
    var_home = var(sqrt(abs(PointsScoredRemaining_Home+1e-8)), na.rm = TRUE)
  )
sqrt_grouped_stats

## LASSO
# filtering the data further
# we will only include the 10 strongest correlation features which include 'GameScore' in their name
# keep all other features

# 'GameScore'
gamescore_cols = grep("GameScore", colnames(filtered_data), value = TRUE)
non_gamescore_cols = setdiff(colnames(filtered_data), gamescore_cols)
Cor = cor(filtered_data[, c(gamescore_cols, "PointsScoredRemaining_Home", "PointsScoredRemaining_Away")], use = "complete.obs")
cor_with_target = apply(abs(Cor[gamescore_cols, c("PointsScoredRemaining_Home", "PointsScoredRemaining_Away")]), 1, max)
top_3_gamescore_features = names(sort(cor_with_target, decreasing = TRUE))[1:3]
retained_columns = unique(c("HomeGameScore","AwayGameScore",non_gamescore_cols, top_3_gamescore_features))
filtered_data = filtered_data[, retained_columns]

# 'PointsScoredLast\\d{3,}Seconds'
points_scored_cols = grep("PointsScoredLast\\d{3,}Seconds", colnames(filtered_data), value = TRUE)
non_points_scored_cols = setdiff(colnames(filtered_data), points_scored_cols)
Cor = cor(filtered_data[, c(points_scored_cols, "PointsScoredRemaining_Home", "PointsScoredRemaining_Away")], use = "complete.obs")
cor_with_target = apply(abs(Cor[points_scored_cols, c("PointsScoredRemaining_Home", "PointsScoredRemaining_Away")]), 1, max)
top_3_points_scored_features = names(sort(cor_with_target, decreasing = TRUE))[1:3]
retained_columns = unique(c("HomePointsScoredLast120Seconds",non_points_scored_cols, top_3_points_scored_features))
filtered_data = filtered_data[, retained_columns]
ncol(filtered_data)
retained_columns

# laplace distribution for feature selection
ddexp = function(x, mu, tau) {
  0.5*tau*exp(-tau*abs(x-mu)) 
}

# select subset
select_subset = function(data, fraction = 0.25, seed = 42) {
  set.seed(seed)
  sample_size = ceiling(fraction * nrow(data))
  sample_indices = sample(1:nrow(data), size = sample_size, replace = FALSE)
  subset_data = data[sample_indices, ]
  return(subset_data)
}

# JAGS was crashing out at using the full data, so lets take a subsample which should be representative of the population descriptive statistics in accordance with the central limit theorem
subset = select_subset(filtered_data, 0.05, seed = 42)

lasso_string = " model {
  for (i in 1:N) { 
    home_y[i] ~ dnegbin(home_p[i], r)
    away_y[i] ~ dnegbin(away_p[i], r)
    home_p[i] = r / (r + home_mu[i])
    away_p[i] = r / (r + away_mu[i])
    log(home_mu[i]) = b_window[meanWindow[i]] + inprod(X[i,], b_h[1:P]) 
    log(away_mu[i]) = b_window[meanWindow[i]] + inprod(X[i,], b_a[1:P])  
  } 

  for (w in 1:max(meanWindow)) {
    b_window[w] ~ dnorm(0, tau)
  }

  for (j in 1:P) {
    b_h[j] ~ ddexp(0.0, 1.0)
    b_a[j] ~ ddexp(0.0, 1.0)
  }

  tau ~ dgamma(1.0, 1.0)
  r ~ dgamma(5.0, 0.1)
}"

predictors = colnames(subset)[!(colnames(subset) %in% c("PointsScoredRemaining_Home","PointsScoredRemaining_Away", "LambdaWID"))]
data_jags = list(
  X = as.matrix(scale(subset[, predictors])),
  home_y = subset[,"PointsScoredRemaining_Home"],
  away_y = subset[,"PointsScoredRemaining_Away"],
  meanWindow = subset$LambdaWID,
  N = length(subset$PointsScoredRemaining_Home),
  P = length(predictors)
)

params = c("b_h","b_a","r","b_window","tau")

lasso_mod = jags.model(textConnection(lasso_string),data=data_jags,n.chains=2)
update(lasso_mod,1e4) # give this some good burnin

mod_sim = coda.samples(lasso_mod,variable.names=params,n.iter=1e5)
mod_csim = as.mcmc(do.call(rbind,mod_sim))

summary_stats = summary(mod_sim)
summary_stats

# convergence diagnostics
plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
effectiveSize(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)

DIC = dic.samples(lasso_mod,n.iter=1e4)
pm_params = colMeans(mod_csim)

# means = summary_stats$statistics[, "Mean"]
# threshold = 0.01
# important_coeffs = means[abs(means) > threshold]
# names(important_coeffs)
# features = names(important_coeffs)
# lasso_indices = as.integer(gsub("\\D", "", features))
# colnames(filtered_data[,lasso_indices])

### Ridge
ridge_string = "
model {
  for (i in 1:N) {
    home_y[i] ~ dnegbin(home_p[i], r)
    away_y[i] ~ dnegbin(away_p[i], r)
    home_p[i] = r / (r + home_mu[i])
    away_p[i] = r / (r + away_mu[i])
    log(home_mu[i]) = b_window[meanWindow[i]] + inprod(X[i,], b_h[1:P])
    log(away_mu[i]) = b_window[meanWindow[i]] + inprod(X[i,], b_a[1:P])
  }

  for (w in 1:max(meanWindow)) {
    b_window[w] ~ dnorm(0,tau_w)
  }

  for (j in 1:P) {
    b_h[j] ~ dnorm(0.0, tau_b)
    b_a[j] ~ dnorm(0.0, tau_b)
  }

  tau_w ~ dgamma(1.0,1.0)
  tau_b = 1 / sigma_b^2
  sigma_b ~ dunif(0, 10)

  r ~ dgamma(5.0, 0.1)
}
"

predictors = colnames(subset)[!(colnames(subset) %in% c("PointsScoredRemaining_Home","PointsScoredRemaining_Away", "LambdaWID"))]
data_jags = list(
  X = as.matrix(scale(subset[, predictors])),
  home_y = subset[,"PointsScoredRemaining_Home"],
  away_y = subset[,"PointsScoredRemaining_Away"],
  meanWindow = subset$LambdaWID,
  N = length(subset$PointsScoredRemaining_Home),
  P = length(predictors)
)

params = c("b_h","b_a","b_window")

ridge_mod = jags.model(textConnection(ridge_string),data=data_jags,n.chains=3)
update(ridge_mod,5e4) # give this some good burnin

mod_sim = coda.samples(ridge_mod,variable.names=params,n.iter=1e5)
mod_csim = as.mcmc(do.call(rbind,mod_sim))

summary_stats = summary(mod_sim)
summary_stats

plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
effectiveSize(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)

DIC = dic.samples(ridge_mod,n.iter=1e4)
pm_params = colMeans(mod_csim)

### Columns reduction
manDis_cols = c("HomePointsScoredLast120Seconds",
                "HomeGameScore","AwayGameScore",
                "SQRTHomeTurnoversDifferential*HomeFGDifferential",
                "SQRTHomeGameScore*AwayGameScore",
                "HomePointsScoredLast120Seconds^2*AwayPointsScoredLast120Seconds")

data_jags = list(
  X = as.matrix(scale(subset[, manDis_cols])),
  home_y = subset[,"PointsScoredRemaining_Home"],
  away_y = subset[,"PointsScoredRemaining_Away"],
  meanWindow = subset$LambdaWID,
  N = length(subset$PointsScoredRemaining_Home),
  P = length(manDis_cols)
)
### second LASSO run
params = c("b_h","b_a","b_window")

lasso_mod = jags.model(textConnection(lasso_string),data=data_jags,n.chains=2)
update(lasso_mod,1e6) # give this some good burnin
# update(lasso_mod,2e6)
mod_sim = coda.samples(lasso_mod,variable.names=params,n.iter=2e4)
mod_csim = as.mcmc(do.call(rbind,mod_sim))

summary_stats = summary(mod_sim)
summary_stats

# plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
effectiveSize(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)

#### second ridge run
# remove column

manDis_cols = c("HomeGameScore","AwayGameScore",
                "SQRTHomeTurnoversDifferential*HomeFGDifferential",
                "SQRTHomeGameScore*AwayGameScore",
                "HomePointsScoredLast120Seconds^2*AwayPointsScoredLast120Seconds")

data_jags = list(
  X = as.matrix(scale(subset[, manDis_cols])),
  home_y = subset[,"PointsScoredRemaining_Home"],
  away_y = subset[,"PointsScoredRemaining_Away"],
  meanWindow = subset$LambdaWID,
  N = length(subset$PointsScoredRemaining_Home),
  P = length(manDis_cols)
)

ridge_mod = jags.model(textConnection(ridge_string),data=data_jags,n.chains=3)
update(ridge_mod,5e6) # give this some good burnin
# update(ridge_mod,7.5e5)
mod_sim = coda.samples(ridge_mod,variable.names=params,n.iter=1e4)
mod_csim = as.mcmc(do.call(rbind,mod_sim))

summary_stats = summary(mod_sim)
summary_stats

# convergence diagnostics
# plot(mod_sim, ask=TRUE)
gelman.diag(mod_sim)
effectiveSize(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)

### Determining Priors
# establish some priors # relatively weak
# analyze the different windows to establish reasonable priors
summary_stats = nba5min %>%
  group_by(LambdaWID) %>%
  summarize(
    mean_home_points = mean(PointsScoredRemaining_Home, na.rm = TRUE),
    mean_away_points = mean(PointsScoredRemaining_Away, na.rm = TRUE),
    sd_home_points = sd(PointsScoredRemaining_Home, na.rm = TRUE),
    sd_away_points = sd(PointsScoredRemaining_Away, na.rm = TRUE)
  )
print(summary_stats) # Inspect the summary stats to set the pri_*

## I want to have a slightly below the actual mean to begin as the prior, with a relatively strong 
pri_a = mean(nba5min$PointsScoredRemaining_Home*0.95)^2/var(nba5min$PointsScoredRemaining_Home*0.95)
pri_b = mean(nba5min$PointsScoredRemaining_Home*0.95)/var(nba5min$PointsScoredRemaining_Home*0.95)
pri_lam = 1 / mean(var(nba5min$PointsScoredRemaining_Home))

################################################################################## 
##*~\-------|||----------------------*<|||>*-----------------------|||-------/~*##
##|############################################################################|##    
##|# 	 	__    __   _____  _____   _____  	__	  		_________________ #|##
##|#	 	||\	`/||  ||   || ||  \\  ||	    ||	    	|===============| #|##
##|#	 	|| \/ ||  ||   || ||   ||  ===	    ||	    	|-|-|-|-|-|-|-|-| #|##
##|# 	 	||	  ||  ||   || ||   || ||	  	||	   __  	 \|/\|/\|/\|/\|/  #|##
##|#	 	|_	  _|  ||___|| ||__//  ||____ 	||_____||     \|/\/\|/\/\|/   #|##
##|# 		____________                      ___________      \/\/\|/\/\/	  #|##
##|#    	|___________\********************/__________|	    \|/\|/\|/	  #|##
##|# 		|___________|NBAyesian Model v1.0|__________|        \/\|/\/      #|##
##|# 		|___________/********************\__________|           	      #|##
##|#												   						  #|##
##|############################################################################|##
##*~/-------|||----------------------*<|||>*-----------------------|||-------\~*##
##################################################################################

## negative binomoial
mod_string = " model {
    for (i in 1:length(homePoints)) {
        homePoints[i] ~ dnegbin(homePhi[lambdaWindow[i]] / (homePhi[lambdaWindow[i]] + homeMu[homeTeam[i]]), homePhi[lambdaWindow[i]])
        log(homeMu[homeTeam[i]]) <- bhome_window[lambdaWindow[i]] + inprod(b_h[1:3], X[i,])
    }
    for (i in 1:length(awayPoints)) {
        awayPoints[i] ~ dnegbin(awayPhi[lambdaWindow[i]] / (awayPhi[lambdaWindow[i]] + awayMu[awayTeam[i]]), awayPhi[lambdaWindow[i]])
        log(awayMu[awayTeam[i]]) <- baway_window[lambdaWindow[i]] + inprod(b_a[1:3], X[i,])
    }
    
    for (j in 1:max(homeTeam)) {
        homeMu[j] ~ dgamma(alpha[lambdaWindow[j]], beta[lambdaWindow[j]])
    }
    for (j in 1:max(awayTeam)) {
        awayMu[j] ~ dgamma(alpha[lambdaWindow[j]], beta[lambdaWindow[j]])
    }

    for (w in 1:max(lambdaWindow)) {
        tau_w[w] ~ dgamma(2.0, 2.0)
        bhome_window[w] ~ dnorm(0, tau_w[w])
        baway_window[w] ~ dnorm(0, tau_w[w])
    }
    
    for (w in 1:max(lambdaWindow)) {
        mu[w] ~ dgamma(pri_a, pri_b)
        sig[w] ~ dexp(pri_lam)
        homePhi[w] ~ dgamma(2.0, 1.0)  
        awayPhi[w] ~ dgamma(2.0, 1.0)  
        
        alpha[w] <- mu[w]^2 / sig[w]^2
        beta[w] <- mu[w] / sig[w]^2
    }
    
    for (k in 1:3) {
        b_h[k] ~ dnorm(0.0, tau)
        b_a[k] ~ dnorm(0.0, tau)
    }
    
    tau ~ dgamma(0.1, 0.1)
} "

set.seed(102)

### Standardize feature data
# scale these values
feature_list = subset[,c("HomeGameScore","AwayGameScore",
                            "SQRTHomeGameScore*AwayGameScore")]

data_jags = list(
			homePoints=subset$PointsScoredRemaining_Home,
			homeTeam=as.numeric(subset$HomeTeam),
			awayPoints=subset$PointsScoredRemaining_Away,
			awayTeam=as.numeric(subset$AwayTeam),
			lambdaWindow=as.numeric(subset$LambdaWID),
			X=as.matrix(scale(feature_list)),
			pri_a=pri_a,
			pri_b=pri_b,
			pri_lam=pri_lam
		)

params = c("homeLam","awayLam","b_window","b_a","b_h","mu","sig")

nba_model = jags.model(textConnection(mod_string),data_jags,n.chains=3)
update(nba_model,5e6)
nba_sim = coda.samples(mod,variable.names=params,n.iter=2e4)
nba_csim = as.mcmc(do.call(rbind,nba_sim))

summary(nba_sim)

nba_DIC = dic.samples(nba_model,n.iter=1e4)
pm_params = colMeans(nba_csim)

# convergence diagnostics
plot(nba_sim, ask=TRUE)
gelman.diag(nba_sim)
autocorr.diag(nba_sim)
autocorr.plot(nba_sim)
effectiveSize(nba_sim)

### If we run this and do not need major mofication move to next:
### inference and prediction

