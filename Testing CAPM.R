# Testing the CAPM for portfolios
# Marcos Diniz
# June-2022
# Brooks, 2019, p. 742


# Simulating data ---------------------------------------------------------
set.seed(59)

# 100 stocks
i <- 100

# 60 months
t <- 60

# Average returns of the firms
mu_ret <- rnorm(i, 0, 0.01)

# Standart deviations of returns
sd_ret <- runif(i, 0.03, 0.06)

# Generating 60 monthly returns based on parameters mu_ret and sd_ret
my_data <- matrix(NA, nrow = t, ncol = i)
for (j in 1:i){
  my_data[,j] <- rnorm(t, mu_ret[j], sd_ret[j])
    
}

# Simulating 60 monthly returns for risk free asset
# Assuming the risk free rate will be constant 
risk_free <- rep(0.001, t)
my_data_excess <- apply(my_data, 2, function(x){x-risk_free})

# Get the lowest the return with the lowest variance
sd_rets <- apply(my_data, 2, sd)
index <- match(sd_rets[which.min(sd_rets)], sd_rets) # column 54 will be the market portfolio

# Step (1): Run individual regressions -----------------------------------
# We'll regress individual returns agains portfolio returns to get betas
# Note1: regression of returns in EXCESS
betas <- apply(my_data_excess, 2, function(x){lm(x ~ my_data_excess[,index])$coefficients[2]})



# Step (2): Run betas against average returns -----------------------------
# Note2: regression of ACTUAL returns, not EXCESS returns
# R_i_mean = λ0 + λ1*beta_i + v_i
mean_ret <- colMeans(my_data)

# Estimating regression
capm_test <- lm(mean_ret ~ betas)
summary(capm_test)

# If the CAPM is a valid model, two key predictions arise which can be tested
# using this second stage regression: λ0 = Rf and λ1 = [Rm − Rf ].
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
