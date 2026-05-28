


# DATA GENERATION AND ORACLES  ----



# simulating discrete and continuous data with no positivity violations
sim_no <- function(n = 1e5, is_discrete = TRUE) {
  if (is_discrete) {
    W = sample(0:4, size = n, replace = TRUE)
  }
  else {
    W = runif(n, 0, 4)
  }
  
  g1 = plogis(-2 +  W)
  A = rbinom(n, size = 1, prob = g1)
  
  QA = 0.3 + 0.3 * A + 0.05 * W
  Q0 = 0.3 + 0.3 * 0 + 0.05 * W
  Q1 = 0.3 + 0.3 * 1 + 0.05 * W
  
  Y = rbinom(n, size = 1, prob = QA)
  
  Y_true = g1 * Q1 + (1 - g1) * Q0
  
  oracle_standard = mean(Q1) - mean(Y_true)
  
  dt = data.table(W, A, Y, g1)
  
  list(
    data = dt,
    oracle_standard = oracle_standard
  )
}



# simulating discrete and continuous data with structural positivity violations
sim_struc <- function(n = 1e5, is_discrete = TRUE) {
  if (is_discrete) {
    W = sample(0:4, size = n, replace = TRUE)
    W_out = (W == 3)
  }
  else {
    W = runif(n, 0, 4)
    W_out <- (2 < W & W < 3)
  }
  
  g1 = plogis(-2 +  W)
  g1[W_out] = 0
  A = rbinom(n, size = 1, prob = g1)
  
  QA = 0.3 + 0.3 * A + 0.05 * W
  Q0 = 0.3 + 0.3 * 0 + 0.05 * W
  Q1 = 0.3 + 0.3 * 1 + 0.05 * W
  
  Y = rbinom(n, size = 1, prob = QA)
  
  Y_true = g1 * Q1 + (1 - g1) * Q0
  
  # standard oracle (not identifiable) and sub-sample oracle (identifiable)
  # oracle_sub <- mean(Q1[!W_out]) - mean(Y_true[!W_out])
  oracle_standard = mean(Q1) - mean(Y_true)
  
  dt = data.table(W, A, Y, g1)
  
  list(
    data = dt,
    oracle_standard = oracle_standard
  )
}



# simulating discrete and continuous data with practical positivity violations
sim_prac <- function(n = 1e5, is_discrete = TRUE) {
  if (is_discrete) {
    W = sample(0:4, size = n, replace = TRUE)
  }
  else {
    W = runif(n, 0, 4)
  }
  
  g1 = plogis(-4 + W)
  A = rbinom(n, size = 1, prob = g1)
  
  QA = 0.3 + 0.3 * A + 0.05 * W
  Q0 = 0.3 + 0.3 * 0 + 0.05 * W
  Q1 = 0.3 + 0.3 * 1 + 0.05 * W
  
  Y = rbinom(n, size = 1, prob = QA)
  
  Y_true = g1 * Q1 + (1 - g1) * Q0
  
  oracle_standard = mean(Q1) - mean(Y_true)
  
  dt = data.table(W, A, Y, g1)
  
  list(
    data = dt,
    oracle_standard = oracle_standard
  )
}



# simulating discrete and continuous data with structural and practical positivity violations
sim_struc_prac <- function(n = 1e5, is_discrete = TRUE) {
  if (is_discrete) {
    W = sample(0:4, size = n, replace = TRUE)
    W_out = (W == 3)
  }
  else {
    W = runif(n, 0, 4)
    W_out <- (2 < W & W < 3)
  }
  
  g1 = plogis(-4 + W)
  g1[W_out] = 0
  A = rbinom(n, size = 1, prob = g1)
  
  QA <- 0.3 + 0.3 * A + 0.05 * W
  Q0 = 0.3 + 0.3 * 0 + 0.05 * W
  Q1 = 0.3 + 0.3 * 1 + 0.05 * W
  
  Y = rbinom(n, size = 1, prob = QA)
  
  Y_true = g1 * Q1 + (1 - g1) * Q0
  
  # standard oracle (not identifiable) and sub-sample oracle (identifiable)
  # oracle_sub = mean(Q1[!W_out]) - mean(Y_true[!W_out])
  oracle_standard = mean(Q1) - mean(Y_true)

  
  dt = data.table(W, A, Y, g1)
  
  list(
    data = dt,
    oracle_standard = oracle_standard
  )
}



get_alpha_oracles <- function(dt, alpha) {
  W <- dt$W
  A <- dt$A
  Y <- dt$Y
  g1 <- dt$g1
  
  QA <- 0.3 + 0.3 * A + 0.05 * W
  Q0 = 0.3 + 0.3 * 0 + 0.05 * W
  Q1 = 0.3 + 0.3 * 1 + 0.05 * W
  
  g1_trunc = pmax(g1, alpha)
  # for truncation as intervention
  # oracle_trunc = mean(Q1) - mean(Ytrunc_true)
  # Ytrunc_true = g1_trunc * Q1 + (1 - g1_trunc) * Q0 
  
  Y_true = g1 * Q1 + (1 - g1) * Q0
  sub = g1 >= alpha
  
  d <- as.integer(g1 >= alpha) # decision boundary
  
  oracle_trunc = mean(g1 / g1_trunc * Q1) - mean(Y_true)
  oracle_trim = mean(Q1[sub]) - mean(Y_true[sub])
  
  oracle_rit = mean(ifelse(d, Q1, Q0)) - mean(Y_true)
  oracle_itt = mean(ifelse(d, Q1, g1 * Q1 + (1 - g1) * Q0)) - mean(Y_true)
  
  
  list(
    alpha = alpha,
    oracle_trunc = oracle_trunc,
    oracle_trim = oracle_trim,
    oracle_rit = oracle_rit,
    oracle_itt = oracle_itt
  ) 
}



get_delta_oracles <- function(dt, delta) {
  W <- dt$W
  A <- dt$A
  Y <- dt$Y
  g1 <- dt$g1
  
  Q0 = 0.3 + 0.3 * 0 + 0.05 * W
  Q1 = 0.3 + 0.3 * 1 + 0.05 * W
  
  Y_true = g1 * Q1 + (1 - g1) * Q0
  
  oracle_ips = mean((exp(delta) * g1 * Q1 + (1 - g1) * Q0) / (exp(delta) * g1 + 1 - g1)) - mean(Y_true)
  
  list(
    delta = delta,
    oracle_ips = oracle_ips
  )
}



# .  ---- 
# NONPARAMETRIC ESTIMATION
# (not used in the simulation studies)



# # g-computation (nonparametric version)
# gcomp_nonpara <- function(dt) {
#   
#   Q1_tab <- dt[, mean(Y[A==1]), by = W]
#   Q1_hat <- Q1_tab$V1[match(dt$W, Q1_tab$W)]
#   
#   psi <- mean(Q1_hat) - mean(dt$Y)
#   
#   return(psi)
# }
# 
# 
# 
# # iptw (nonparametric version)
# iptw_nonpara <- function(dt) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   psi <- mean(dt$A / g1_hat * dt$Y) - mean(dt$Y)
#   # Under structural positivity violations we end up with A/g1_hat = 0/0.
#   return(psi)
# }
# 
# 
# 
# # truncated iptw estimation (nonparametric version)
# iptw_trunc_nonpara <- function(dt, alpha) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   # weights cannot be smaller than a certain level alpha
#   g1_trunc_hat <- pmax(g1_hat, alpha)
#   
#   psi <- mean(dt$A / g1_trunc_hat * dt$Y) - mean(dt$Y)
#   
#   return(psi)
# }
# 
# 
# 
# # trimmed iptw estimation (nonparametric version)
# iptw_trim_nonpara <- function(dt, alpha) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   # subsample
#   sub <- g1_hat >= alpha
#   
#   psi <- mean(dt$A[sub] / g1_hat[sub] * dt$Y[sub]) - mean(dt$Y[sub])
#   
#   return(psi)
# }
# 
# 
# 
# # ips intervention with iptw estimation (nonparametric version)
# iptw_ips_nonpara <- function(dt, delta) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   psi <- mean(dt$Y * (exp(delta) * dt$A + 1 - dt$A) /
#                 (exp(delta) * g1_hat + (1 - g1_hat))
#   ) - mean(dt$Y)
#   
#   return(psi)
# }
# 
# 
# 
# # iptw estimation with hajek stabilization (nonparametric version)
# iptw_hajek_nonpara <- function(dt) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   pA1 <- mean(dt$A)
#   # reweighting outcomes relative to how commen the treatment is overall
#   w <- pA1 * dt$A / g1_hat
#   
#   # Hájek normalization
#   psi <- mean(w * dt$Y) / mean(w) - mean(dt$Y)
#   
#   return(psi)
# }
# 
# 
# 
# # one-step estimation (nonparametric version)
# aiptw_nonpara <- function(dt) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   Q1_tab <- dt[, mean(Y[A==1]), by = W]
#   Q1_hat <- Q1_tab$V1[match(dt$W, Q1_tab$W)]
#   
#   psi <- mean(dt$A / g1_hat * (dt$Y - Q1_hat) + Q1_hat) - mean(dt$Y)
#   
#   return(psi)
# }
# 
# 
# 
# # ips intervention with plug-in estimation (nonparametric version)
# plugin_ips_nonpara <- function(dt, delta) {
#   
#   g1_tab <- dt[, mean(A==1), by = W]
#   g1_hat <- g1_tab$V1[match(dt$W, g1_tab$W)]
#   
#   Q1_tab <- dt[, mean(Y[A==1]), by = W]
#   Q1_hat <- Q1_tab$V1[match(dt$W, Q1_tab$W)]
#   
#   Q0_tab <- dt[, mean(Y[A==0]), by = W]
#   Q0_hat <- Q0_tab$V1[match(dt$W, Q1_tab$W)]
#   
#   psi <- mean((exp(delta) * g1_hat * Q1_hat + (1 - g1_hat) * Q0_hat) /
#                 (exp(delta) * g1_hat + (1 - g1_hat))
#   ) - mean(dt$Y)
#   
#   return(psi)
# }



# ESTIMATORS   ----



# # g-computation (parametric version)
# gcomp_para <- function(dt) {
# 
#   fitQ <- lm(Y ~ A * W, data = dt)
# 
#   dt1 <- copy(dt)[, A := 1]
#   Q1_hat <- predict(fitQ, dt1, type='response')
# 
#   psi <- mean(Q1_hat) - mean(dt$Y)
# 
#   return(psi)
# }
# 
# 
# 
# # plug-in estimation of an ips intervention (parametric version)
# plugin_ips_para <- function(dt, delta) {
#   
#   fitg <- glm(A ~ W, family = binomial, data = dt)
#   g1_hat <- predict(fitg, type = "response")
#   
#   fitQ <- lm(Y ~ A * W, data = dt)
#   
#   dt0 <- copy(dt)[, A := 0]
#   dt1 <- copy(dt)[, A := 1]
#   
#   Q0_hat <- predict(fitQ, dt0, type='response')
#   Q1_hat <- predict(fitQ, dt1, type='response')
#   
#   psi <- mean(
#     (exp(delta) * g1_hat * Q1_hat + (1 - g1_hat) * Q0_hat) /
#       (exp(delta) * g1_hat + (1 - g1_hat))
#   ) - mean(dt$Y)
#   
#   return(psi)
# }



## IPTW ----



# inverse probability of treatment weighting estimation
iptw_standard <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat) {
  
  if_terms <- dt$A / g1_hat * dt$Y - dt$Y

  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
    
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# truncated iptw estimation
iptw_trunc <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  g1_trunc_hat <- pmax(g1_hat, alpha)
  if_terms <- dt$A / g1_trunc_hat * dt$Y - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# trimmed iptw estimation
iptw_trim <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  sub <- g1_hat >= alpha
  if_terms <- dt$A[sub] / g1_hat[sub] * dt$Y[sub] - dt$Y[sub]
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt[sub])) # estimating in a smaller sample!!!
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# iptw estimation of a rit rule
iptw_rit <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  dhat <- as.integer(g1_hat >= alpha)
  gA_hat <- ifelse(dhat == 1, g1_hat, 1 - g1_hat)
  if_terms <- (dt$A == dhat) / gA_hat * dt$Y - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# iptw estimation of a itt rule
iptw_itt <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  dhat <- as.integer(g1_hat >= alpha)
  
  wt <- ifelse(dhat, dt$A / g1_hat, 1)
  if_terms <- wt * dt$Y - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# iptw estimation of an ips intervention
iptw_ips <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, delta) {
  
  if_terms <- dt$Y * (exp(delta) * dt$A + 1 - dt$A) / (exp(delta) * g1_hat + 1 - g1_hat) - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



## IPTW HAJEK ----



# iptw estimation with hajek stabilization
iptw_hajek_standard <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat) {

  wt <- dt$A / g1_hat
  mu_hat <- mean(wt * dt$Y) / mean(wt)
  
  psi_hat <- mu_hat - mean(dt$Y)
  
  # important to use a different influence function structure
  # as the influence of one observation depends on the whole sample through mean(wt)
  # mu_hat is not independent across observations
  D_hat <-  wt / mean(wt) * (dt$Y - mu_hat) - (dt$Y - mean(dt$Y))

  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat

  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# truncated iptw estimation with hajek stabilization
iptw_hajek_trunc <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  g1_trunc_hat <- pmax(g1_hat, alpha)
  wt <- dt$A / g1_trunc_hat
  mu_hat <- mean(wt * dt$Y) / mean(wt)
  
  psi_hat <- mu_hat - mean(dt$Y)
  
  D_hat <-  wt / mean(wt) * (dt$Y - mu_hat) - (dt$Y - mean(dt$Y))
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# trimmed iptw estimation with hajek stabilization
iptw_hajek_trim <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  sub <- g1_hat >= alpha
  wt <- dt$A / g1_hat
  
  mu_sub_hat <- mean(wt[sub] * dt$Y[sub]) / mean(wt[sub])
  
  psi_hat <- mu_sub_hat - mean(dt$Y[sub])
  
  D_hat <-  wt[sub] / mean(wt[sub]) * (dt$Y[sub] - mu_sub_hat) - (dt$Y[sub] - mean(dt$Y[sub]))
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt[sub]))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# iptw estimation of a rit rule with hajek stabilization
iptw_hajek_rit <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  dhat <- as.integer(g1_hat >= alpha)
  gA_hat <- ifelse(dhat == 1, g1_hat, 1 - g1_hat)
  wt <- (dt$A == dhat) / gA_hat
  mu_hat <- mean(wt * dt$Y) / mean(wt)
  
  psi_hat <- mu_hat - mean(dt$Y)
  
  D_hat <-  wt / mean(wt) * (dt$Y - mu_hat) - (dt$Y - mean(dt$Y))
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# iptw estimation of an itt rule with hajek stabilization
iptw_hajek_itt <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  dhat <- as.integer(g1_hat >= alpha)
  wt <- ifelse(dhat, dt$A / g1_hat, 1)
  mu_hat <- mean(wt * dt$Y) / mean(wt)
  
  psi_hat <- mu_hat - mean(dt$Y)
  
  D_hat <-  wt / mean(wt) * (dt$Y - mu_hat) - (dt$Y - mean(dt$Y))
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# iptw estimation of an ips intervention with hajek stabilization
iptw_hajek_ips <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, delta) {
  
  wt <- (exp(delta) * dt$A + 1 - dt$A) / (exp(delta) * g1_hat + 1 - g1_hat)
  mu_hat <- mean(wt * dt$Y) / mean(wt)
  
  psi_hat <- mu_hat - mean(dt$Y)
  
  D_hat <-  wt / mean(wt) * (dt$Y - mu_hat) - (dt$Y - mean(dt$Y))
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



## ONE-STEP ----



# one-step estimation
onestep_standard <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat) {
  
  if_terms <- dt$A / g1_hat * (dt$Y - QA_hat) + Q1_hat - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# truncated one-step estimation
onestep_trunc <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  g1_trunc_hat <- pmax(g1_hat, alpha)
  if_terms <- dt$A / g1_trunc_hat * (dt$Y - QA_hat) + Q1_hat - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# trimmed one-step estimation
onestep_trim <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  # outcome regression was fitted on the whole sample
  
  sub <- g1_hat >= alpha
  if_terms <- dt$A[sub] / g1_hat[sub] * (dt$Y[sub] - QA_hat[sub]) + Q1_hat[sub] - dt$Y[sub]
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt[sub]))  # estimating in a smaller sample!!!
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# one-step estimation of a rit rule
onestep_rit <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  gA_hat <- ifelse(dt$A == 1, g1_hat, 1 - g1_hat)
  
  dhat <- as.integer(g1_hat >= alpha)
  Qd_hat <- ifelse(dhat == 1, Q1_hat, Q0_hat)
  
  if_terms <- (dt$A == dhat) / gA_hat * (dt$Y -  QA_hat) + Qd_hat - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# one-step estimation of an itt rule
onestep_itt <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  dhat <- as.integer(g1_hat >= alpha)
  wt <- ifelse(dhat, dt$A / g1_hat, 1)
  Qd_hat <- ifelse(dhat == 1, Q1_hat, g1_hat * Q1_hat + (1 - g1_hat) * Q0_hat)
  
  if_terms <- wt * (dt$Y -  QA_hat) + Qd_hat - dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# one-step estimation of an ips intervention
onestep_ips <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, delta) {
  
  phi1 <- dt$A / g1_hat * (dt$Y - Q1_hat) + Q1_hat
  phi0 <- (1 - dt$A) / (1 - g1_hat) * (dt$Y - Q0_hat) + Q0_hat
  
  denom <- exp(delta) * g1_hat + 1 - g1_hat
  
  if_terms <- (exp(delta) * g1_hat * phi1 + (1 - g1_hat) * phi0) / denom +
    (exp(delta) * (Q1_hat - Q0_hat) * (dt$A - g1_hat)) / denom^2 -
    dt$Y
  
  psi_hat <- mean(if_terms)
  D_hat <- if_terms - psi_hat
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



## TMLE ----
# https://www.khstats.com/blog/tmle/tutorial-pt2



# targeted maximum likelihood estimation
tmle_standard <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat) {
  
  HQ <- dt$A / g1_hat
  Q_fluc <- glm(Y ~ offset(qlogis(QA_hat)) + HQ - 1,  family = "quasibinomial", data = dt)
  
  QA_hat_star <- predict(Q_fluc, type = "response")
  Q1_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q1_hat, HQ = 1 / g1_hat), type = "response")
  
  # # check the empirical efficient score equation
  # score <- mean(HQ * (dt$Y - QA_hat_star))
  # cat("score:", score, "\n")
  
  if_terms <- dt$A / g1_hat * (dt$Y - QA_hat_star) + Q1_hat_star - dt$Y
  
  psi_hat <- mean(Q1_hat_star) - mean(dt$Y)
  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# truncated tmle
tmle_trunc <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  g1_trunc_hat <- pmax(g1_hat, alpha)
  
  HQ <- dt$A / g1_trunc_hat
  Q_fluc <- glm(Y ~ offset(qlogis(QA_hat)) + HQ - 1,  family = "quasibinomial", data = dt)
  
  QA_hat_star <- predict(Q_fluc, dt, type = "response")
  Q1_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q1_hat, HQ = 1 / g1_hat), type = "response")
  
  # # check the empirical efficient score equation
  # score <- mean(HQ * (dt$Y - QA_hat_star))
  # print(score)
  
  if_terms <- dt$A / g1_trunc_hat * (dt$Y - QA_hat_star) + Q1_hat_star - dt$Y
  
  psi_hat <- mean(Q1_hat_star) - mean(dt$Y)
  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# trimmed tmle
tmle_trim <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  sub <- g1_hat >= alpha
  
  HQ <- (dt$A / g1_hat) * sub # gives weight 0 to some observations
  Q_fluc <- glm(Y ~ offset(qlogis(QA_hat)) + HQ - 1,  family = "quasibinomial", data = dt)
  
  QA_hat_star <- predict(Q_fluc, dt, type = "response")
  Q1_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q1_hat, HQ = 1 / g1_hat), type = "response")
  
  # # check the empirical efficient score equation
  # score <- mean(HQ * (dt$Y - QA_hat_star))
  # print(score)
  
  if_terms <- dt$A[sub] / g1_hat[sub] * (dt$Y[sub] - QA_hat_star[sub]) + Q1_hat_star[sub] - dt$Y[sub]
  
  psi_hat <- mean(Q1_hat_star[sub]) - mean(dt$Y[sub])
  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt[sub])) # estimating in a smaller sample!!!
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# tmle of a rit rule
tmle_rit <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  gA_hat <- ifelse(dt$A == 1, g1_hat, 1 - g1_hat)
  
  dhat <- as.integer(g1_hat >= alpha)
  
  HQ <- (dt$A == dhat) / gA_hat
  Q_fluc <- glm(Y ~ offset(qlogis(QA_hat)) + HQ - 1,  family = "quasibinomial", data = dt)
  
  QA_hat_star <- predict(Q_fluc, dt, type = "response")
  Q0_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q0_hat, HQ = ifelse(dhat, 0, 1 / (1 - g1_hat))), type = "response")
  Q1_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q1_hat, HQ = ifelse(dhat, 1 / g1_hat, 0)), type = "response")
  Qd_hat_star <- ifelse(dhat == 1, Q1_hat_star, Q0_hat_star)
  
  # # check the empirical efficient score equation
  # score <- mean(HQ * (dt$Y - QA_hat_star))
  # print(score)
  
  if_terms <- (dt$A == dhat) / gA_hat * (dt$Y - QA_hat_star) + Qd_hat_star - dt$Y
  
  psi_hat <- mean(Qd_hat_star) - mean(dt$Y)
  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# tmle of an itt rule
tmle_itt <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, alpha) {
  
  dhat <- as.integer(g1_hat >= alpha)
  wt <- ifelse(dhat, dt$A / g1_hat, 1)
  
  HQ <- ifelse(dhat, dt$A / g1_hat, 0)
  Q_fluc <- glm(Y ~ offset(qlogis(QA_hat)) + HQ - 1,  family = "quasibinomial", data = dt)
  
  QA_hat_star <- predict(Q_fluc, type = "response")
  # Q0_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q0_hat, HQ = ifelse(dhat, 0, 0)), type = "response")
  Q0_hat_star <- Q0_hat
  Q1_hat_star <- predict(Q_fluc, data.frame(QA_hat = Q1_hat, HQ = ifelse(dhat, 1 / g1_hat, 0)), type = "response")
  Qd_hat_star <- ifelse(dhat == 1, Q1_hat_star, g1_hat * Q1_hat_star + (1 - g1_hat) * Q0_hat_star)
  
  # # check the empirical efficient score equation
  # score <- mean(HQ * (dt$Y - QA_hat_star))
  # print(score)
  
  if_terms <- wt * (dt$Y - QA_hat_star) + Qd_hat_star - dt$Y
  
  psi_hat <- mean(Qd_hat_star) - mean(dt$Y)
  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# !!! IMPLEMENT MULTIPLE ITERATIONS AND CHECK CONVERGENCE!!!

# tmle of an ips intervention
tmle_ips <- function(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, delta) {
  
  max_iter <- 50
  tol <- 1e-6
  
  
  score = 2 * tol
  k = 0
  
  
  
  while (abs(score) > tol && max_iter > k) {
    
    # denominator also needs to be updated
    denom <- exp(delta) * g1_hat + 1 - g1_hat
    
    # fluctuate the outcome model
    HQ <-  (exp(delta) * dt$A + (1 - dt$A)) / denom
    Q_fluc <- glm(Y ~ offset(qlogis(QA_hat)) + HQ - 1,  family = "quasibinomial", data = dt)
    
    
    QA_hat_k1 <- predict(Q_fluc, dt, type = "response")
    Q0_hat_k1 <- predict(Q_fluc, data.frame(QA_hat = Q0_hat, HQ = 1 / denom), type = "response")
    Q1_hat_k1 <- predict(Q_fluc, data.frame(QA_hat = Q1_hat, HQ = exp(delta) / denom), type = "response")
    
    
    # fluctuate the propensity score model
    Hg <- exp(delta) * (Q1_hat_k1 - Q0_hat_k1) / denom^2
    g1_fluc <- glm(A ~ offset(qlogis(g1_hat)) + Hg - 1, family = "quasibinomial", data = dt)

    g1_hat_k1 <- predict(g1_fluc, type = "response")
    
    # denominator also needs to be updated
    denom_k1 <- exp(delta) * g1_hat_k1 + 1 - g1_hat_k1
    
    
    # empirical efficient score equation
    score <- mean(
      (exp(delta) * g1_hat_k1 * dt$A / g1_hat_k1 * (dt$Y - QA_hat_k1) +
         (1 - g1_hat_k1) * (1 - dt$A) / (1 - g1_hat_k1) * (dt$Y - QA_hat_k1)) / denom_k1 +
      (exp(delta) * (Q1_hat_k1 - Q0_hat_k1) * (dt$A - g1_hat_k1)) / denom_k1^2
    )
    
   
    # set updated values to current for next iteration
    QA_hat <- QA_hat_k1
    Q0_hat <- Q0_hat_k1
    Q1_hat <- Q1_hat_k1
    g1_hat <- g1_hat_k1
    
    k = k + 1
      
  }
  
  # cat("empirical efficient score:", score, "\n")
  # cat("final iteration:", k, "\n\n")
  
  # if loop ends we have the final values (star)
  QA_hat_kstar <- QA_hat
  Q0_hat_kstar <- Q0_hat
  Q1_hat_kstar <- Q1_hat
  g1_hat_kstar <- g1_hat
  
  
  
  D1_kstar <- dt$A / g1_hat_kstar * (dt$Y - QA_hat_kstar) + Q1_hat_kstar
  D0_kstar <- (1 - dt$A) / (1 - g1_hat_kstar) * (dt$Y - QA_hat_kstar) + Q0_hat_kstar
  
  denom_kstar <- exp(delta) * g1_hat_kstar + 1 - g1_hat_kstar
  
  if_terms <- (exp(delta) * g1_hat_kstar * D1_kstar + (1 - g1_hat_kstar) * D0_kstar) / denom_kstar +
    (exp(delta) * (Q1_hat_kstar - Q0_hat_kstar) * (dt$A - g1_hat_kstar)) / denom_kstar^2 -
    dt$Y
  
  
  psi_hat <- mean(
    (exp(delta) * g1_hat_kstar * Q1_hat_kstar + (1 - g1_hat_kstar) * Q0_hat_kstar) / denom_kstar
  ) - mean(dt$Y)
  

  D_hat <- if_terms - mean(if_terms)
  
  sd_hat <- sd(D_hat)
  se_hat <- sd_hat / sqrt(nrow(dt))
  ci95_hat <- psi_hat + c(-1,1) * qnorm(0.975) * se_hat
  
  return(list(
    psi_hat = psi_hat,
    se_hat = se_hat,
    ci95_hat = ci95_hat
  ))
}



# .  ---- 
# SIMULATION SETUP ----



get_target <- function(method) {
  tolower(sapply(strsplit(method, "[()]"), function(x) x[2] ))
}



run_oracles <- function(sim, alpha_vals, delta_vals) {
  
  dt <- sim$data
  res_list <- list()
  
  # constant oracles
  res_list[["standard"]] <- data.table(
    method    = "Oracle (Standard)",
    parameter = NA_real_,
    type      = "none",
    psi       = sim$oracle_standard
  )
  
  # alpha oracles
  alpha_dt <- rbindlist(lapply(alpha_vals, function(a)
    as.data.table(get_alpha_oracles(dt, a))
  ))
  
  res_list[["trunc"]] <- data.table(
    method    = "Oracle (Truncated)",
    parameter = alpha_dt$alpha,
    type      = "alpha",
    psi       = alpha_dt$oracle_trunc
  )
  
  res_list[["trim"]] <- data.table(
    method    = "Oracle (Trimmed)",
    parameter = alpha_dt$alpha,
    type      = "alpha",
    psi       = alpha_dt$oracle_trim
  )
  
  res_list[["rit"]] <- data.table(
    method    = "Oracle (RIT)",
    parameter = alpha_dt$alpha,
    type      = "alpha",
    psi       = alpha_dt$oracle_rit
  )
  
  res_list[["itt"]] <- data.table(
    method    = "Oracle (ITT)",
    parameter = alpha_dt$alpha,
    type      = "alpha",
    psi       = alpha_dt$oracle_itt
  )
  
  # delta oracle
  delta_dt <- rbindlist(lapply(delta_vals, function(d)
    as.data.table(get_delta_oracles(dt, d))
  ))
  
  res_list[["ips"]] <- data.table(
    method    = "Oracle (IPS)",
    parameter = delta_dt$delta,
    type      = "delta",
    psi       = delta_dt$oracle_ips
  )
  
  
  rbindlist(res_list, fill = TRUE)
}



run_estimators <- function(sim, name, estimator, alpha_vals, delta_vals, nuisance) {
  
  dt <- sim$data
  
  # compute nuisance parameters once to reduce runtime
  if (nuisance == "correct") {
    fitg <- glm(A ~ W, family = binomial, data = dt)
    fitQ <- lm(Y ~ A + W, data = dt)
    
  } else if (nuisance == "misspecifiedg") {
    fitg <- glm(A ~ I(W^2), family = binomial, data = dt)
    fitQ <- lm(Y ~ A + W, data = dt)
    
  } else if (nuisance == "gamg") {
    fitg <- gam(A ~ s(W), family = binomial, data = dt)
    fitQ <- lm(Y ~ A + W, data = dt)
    
  } else if (nuisance == "misspecifiedQ") {
    fitg <- glm(A ~ W, family = binomial, data = dt)
    fitQ <- lm(Y ~ A, data = dt)
    
  } else if (nuisance == "misspecified") {
    fitg <- glm(A ~ I(W^2), family = binomial, data = dt)
    fitQ <- lm(Y ~ A, data = dt)
    
  } else {
    stop("unknown nuisance specification")
  }
  
  
  g1_hat <- predict(fitg, type = "response")
  
  dt0 <- copy(dt)[, A := 0]
  dt1 <- copy(dt)[, A := 1]
  
  QA_hat <- predict(fitQ, dt, type = "response")
  Q0_hat <- predict(fitQ, dt0, type = "response")
  Q1_hat <- predict(fitQ, dt1, type = "response")
  
  
  
  
  
  res_list <- list()
  
  name <- estimator$name
  targets <- estimator$targets
  

  # standard target
  
  if ("standard" %in% names(targets)) {
    res_standard <- targets$standard(dt, g1_hat, QA_hat, Q1_hat, Q0_hat)
    
    res_list[["standard"]] <- data.table(
      method     = paste0(name, " (Standard)"),
      estimator  = name, 
      parameter  = NA_real_,
      type       = "none",
      psi        = res_standard$psi_hat,
      se         = res_standard$se_hat,
      ci95_lower = res_standard$ci95_hat[1],
      ci95_upper = res_standard$ci95_hat[2]
    )
  }
  
  
  
  # alpha targets
  
  if ("truncated" %in% names(targets)) {
    res_trunc <- rbindlist(lapply(alpha_vals, function(a) {
      out <- targets$truncated(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, a)
    
      data.table(
        parameter  = a,
        psi        = out$psi_hat,
        se         = out$se_hat,
        ci95_lower = out$ci95_hat[1],
        ci95_upper = out$ci95_hat[2]
      )
    }))
    
    res_list[["trunc"]] <- res_trunc[, `:=`(
      method    = paste0(name, " (Truncated)"),
      estimator = name,
      type      = "alpha"
    )]
  }
  
  
  
  if ("trimmed" %in% names(targets)) {
    res_trim <- rbindlist(lapply(alpha_vals, function(a) {
      out <- targets$trimmed(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, a)
      
      data.table(
        parameter  = a,
        psi        = out$psi_hat,
        se         = out$se_hat,
        ci95_lower = out$ci95_hat[1],
        ci95_upper = out$ci95_hat[2]
      )
    }))
    
    res_list[["trim"]] <- res_trim[, `:=`(
      method    = paste0(name, " (Trimmed)"),
      estimator = name,
      type      = "alpha"
    )]
  }
  
  
  
  if ("rit" %in% names(targets)) {
    res_rit <- rbindlist(lapply(alpha_vals, function(a) {
      out <- targets$rit(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, a)
      
      data.table(
        parameter  = a,
        psi        = out$psi_hat,
        se         = out$se_hat,
        ci95_lower = out$ci95_hat[1],
        ci95_upper = out$ci95_hat[2]
      )
    }))
    
    res_list[["rit"]] <- cbind(
      data.table(
        method    = paste0(name, " (RIT)"),
        estimator = name,
        type      = "alpha"),
      res_rit
    )
  }
  
  
  
  if ("itt" %in% names(targets)) {
    res_itt <- rbindlist(lapply(alpha_vals, function(a) {
      out <- targets$itt(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, a)
      
      data.table(
        parameter  = a,
        psi        = out$psi_hat,
        se         = out$se_hat,
        ci95_lower = out$ci95_hat[1],
        ci95_upper = out$ci95_hat[2]
      )
    }))
    
    res_list[["itt"]] <- cbind(
      data.table(
        method    = paste0(name, " (ITT)"),
        estimator = name,
        type      = "alpha"),
      res_itt
    )
  }
  
  
  
  # delta target
  
  if ("ips" %in% names(targets)) {
    res_ips <- rbindlist(lapply(delta_vals, function(d) {
      out <- targets$ips(dt, g1_hat, QA_hat, Q1_hat, Q0_hat, d)
      
      data.table(
        parameter  = d,
        psi        = out$psi_hat,
        se         = out$se_hat,
        ci95_lower = out$ci95_hat[1],
        ci95_upper = out$ci95_hat[2]
      )
    }))
    
    res_list[["ips"]] <- cbind(
      data.table(
        method    = paste0(name, " (IPS)"),
        estimator = name,
        type      = "delta"),
      res_ips
    )
  }
  
  
  
  rbindlist(res_list, fill = TRUE)
}



make_plot_data <- function(results) {
  
  # get parameters and create grids
  alpha_vals <- sort(unique(results[type == "alpha"]$parameter))
  delta_vals <- sort(unique(results[type == "delta"]$parameter))
  
  # invert delta grid
  delta_vals <- rev(delta_vals)
  
  # get the max grid length
  l <- max(length(alpha_vals), length(delta_vals))
  
  # extend individual grids to the max grid length l with NAs
  param_index <- data.table(
    xidx   = seq_len(l),
    alpha = c(alpha_vals, rep(NA, l - length(alpha_vals))),
    delta = c(delta_vals, rep(NA, l - length(delta_vals)))
  )
  
  # map parameters to a joint xgrid
  results_plot <- copy(results)
  results_plot[type == "alpha",
               xgrid := param_index$xidx[match(parameter, param_index$alpha)]]
  
  results_plot[type == "delta",
               xgrid := param_index$xidx[match(parameter, param_index$delta)]]
  
  
  # extend constant oracles to the max grid length l
  const_methods <- results_plot[type == "none"]
  const_plot <- const_methods[, .(xgrid = seq_len(l), psi = rep(psi, l), type = "none"),
                              by = method]
  
  # remove NA indices and psi values
  results_plot <-results_plot[!is.na(xgrid)]
  
  
  # add extended constant methods to results plot
  results_plot <- rbind(results_plot, const_plot, fill = TRUE)
  
  
  
  # get the rightmost point for each method
  label_data <- results_plot[, .SD[which.max(xgrid)], by = method]
  
  return(list(
    results_plot = results_plot, # full extended data
    label_data = label_data, # rightmost points
    param_index = param_index # map for each parameter (alpha, delta) to xgrid
  ))
}



labels_sci <- function(x) {
  sapply(x, function(val) {
    
    if (val == 0)
      return("0.0e+0")
    
    lab <- sprintf("%.2e", val)
    
    # remove leading zeros but keep one exponent digit
    lab <- gsub("e([+-])0*([0-9]+)", "e\\1\\2", lab)
    lab
  })
}



# https://r-graph-gallery.com/web-line-chart-with-labels-at-end-of-line.html
plot_methods <- function(plot_data,
                         title = "Positivity Violations",
                         method_colors = NULL,
                         y_label,
                         bars = FALSE) {
  
  if (is.null(method_colors)) {
    methods <- unique(plot_data$results_plot$method)
    method_colors <- setNames(
      scales::hue_pal()(length(methods)),
      methods
    )
  }
  
  results_plot <- plot_data$results_plot
  label_data <- plot_data$label_data
  param_index <- plot_data$param_index
  
  results_plot <- results_plot[is.finite(psi)]
  label_data   <- label_data[is.finite(psi)]
  
  results_plot[, method := factor(method)]
  label_data[, method := factor(method)]
  
  # y scale
  k <- length(param_index$xidx)
  
  # adding sd to constant methods
  sd_vals <- ifelse(is.na(results_plot$sd_psi), 0, results_plot$sd_psi)
  
  # include error bars
  if (bars) {
    y_min <- min(results_plot$psi - sd_vals, na.rm=TRUE)
    y_max <- max(results_plot$psi + sd_vals, na.rm=TRUE)
  } else {
    y_min <- min(results_plot$psi, na.rm=TRUE)
    y_max <- max(results_plot$psi, na.rm=TRUE)
  }
  
  # avoid collapsing y-axis
  min_range <- 1e-5
  
  if ((y_max - y_min) < min_range) {
    center <- (y_min + y_max) / 2
    y_min <- center - min_range / 2
    y_max <- center + min_range / 2
  }
  
  # get y step size
  y_breaks_full <- pretty(c(y_min, y_max), n = k)
  step <- diff(y_breaks_full)[1]
  
  y_start <- floor(y_min / step) * step # - step
  y_end   <- ceiling(y_max / step) * step # + step
  
  y_breaks <- seq(y_start, y_end, by = step)
  y_lim <- c(y_start, y_end)
  
  
  # x scale
  pad <- 2.5
  x_data_min <- min(param_index$xidx)
  x_data_max <- max(param_index$xidx)
  x_max <- x_data_max + pad
  
  # used later to adjust axis labels
  h <- (mean(param_index$xidx) - x_data_min) / (x_max - x_data_min)
  
  label_data[, x_label := xgrid + 0.5]
  
  range_data <- x_data_max - x_data_min
  left_ratio <- pad / range_data
  base_margin <- 5
  left_margin <- base_margin + 210 * left_ratio
  
  
# base plot
  p <- ggplot(results_plot, aes(x = xgrid, y = psi, color = method)) +
    
    geom_vline(
      xintercept = param_index$xidx,
      color = "grey91",
      linewidth = 0.5
    ) +
    
    geom_segment(
      data = data.table(
        y = y_breaks,
        x1 = x_data_min,
        x2 = x_data_max
      ),
      aes(x = x1, xend = x2, y = y, yend = y),
      inherit.aes = FALSE,
      color = "grey91",
      linewidth = 0.5
    )
  
  if (y_lim[1] <= 0 && 0 <= y_lim[2]) {
    p <- p +
      geom_segment(
        data = data.table(
          x = x_data_min,
          xend = x_data_max,
          y = 0,
          yend = 0
        ),
        aes(x = x, xend = xend, y = y, yend = yend),
        inherit.aes = FALSE,
        color = "black",
        linetype = "longdash",
        linewidth = 0.5
      )
  }
  
  # adding error bars
  if (bars) {
    p <- p + geom_errorbar(
      aes( 
        ymin = psi - sd_psi,
        ymax = psi + sd_psi
      ),
      width = 0.2,
      alpha = 0.5
    )
  }
  
  # adding further layers
  p <- p +
    geom_line() +
    geom_point(aes(shape = type), size = 3.5, stroke = 1.25) +
    
    scale_shape_manual(
      name = "Parameter type",
      values = c( "none"  = 1, "alpha" = 0, "delta" = 2)) +
    
    scale_color_manual(values = method_colors) +
    
    geom_text_repel(
      data = label_data,
      aes(label = method, color = method),
      fontface = "bold",
      size = 6,
      direction = "y",
      hjust = 0,
      nudge_x = 1,
      
      box.padding = 0.2,
      point.padding = 0,
      
      segment.linetype = "dotted",
      segment.size = 1,
      
      force = 5,
      force_pull = 0,
      show.legend = FALSE
    ) +
    
    coord_cartesian(
      clip = "off",
      ylim = y_lim,
      xlim = c(x_data_min, x_max)
    ) +
    
    scale_x_continuous(
      # limits = c(x_data_min, x_max),
      expand = expansion(mult = 0, add = 0),
      breaks = param_index$xidx,
      labels = param_index$delta,
      name = expression(delta), # bottom axis
      
      sec.axis = sec_axis(
        ~ .,
        breaks = param_index$xidx,
        labels = param_index$alpha,
        name = expression(alpha) # top axis
      )
    ) +
    
    scale_y_continuous(
      expand = expansion(mult = 0, add = 0),
      breaks = y_breaks,
      labels = labels_sci
    ) +
    
    labs(
      x = NULL,
      y = y_label,
      # labs(y = expression(Psi ~ "(mean ± SD)")),
      title = title
    ) +
    
    theme_light(base_size = 18) +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      
      plot.title = element_text(face = "bold", size = 22, hjust = 0),
      plot.title.position = "plot",
      
      axis.text = element_text(size = 18),
      axis.title.y = element_text(size = 20),
      axis.title.x = element_text(size = 20, face = "bold", hjust = h),
      axis.title.x.top = element_text(size = 20, face = "bold", hjust = h),
      
      # legend.position = c(h - 0.105, - 0.35),
      # legend.justification = c(h, 0),
      legend.position = "bottom",
      legend.margin = ggplot2::margin(b = 20),
      legend.justification = 0,
      legend.direction = "horizontal",
      legend.text = element_text(size = 22),
      legend.title = element_text(size = 22),
      
      plot.margin = ggplot2::margin(t = base_margin,
                                    r = base_margin,
                                    b = base_margin,
                                    l = base_margin
                                    # l = left_margin
      )
    ) +
    
    guides(
      color = "none",
      shape = guide_legend(
        title = "Parameter Type:",
        override.aes = list(size = 4),
        nrow = 1)
    ) +
    
    expand_limits(x = max(param_index$xidx) + 2)
}


