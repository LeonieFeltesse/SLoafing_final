###Creating of the base functions to compute the outcome of the simulation later on

#for the reproducibility
set.seed(123)

# load the necessary libraries
library(ggplot2)
library(dplyr)
library(multcomp)

#' p1 Compute the contingency between input and outcome
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' 
#' @return (CON) contingency between input and outcome. Ranges from 0 to 1 (continuous)

get_CON <- function(CT) {
  n <- CT + 1
  CON <- 1/n
  return(CON)
}

# (example) check vectorized form:
CT <- c(0, 1, 5)
CON <- get_CON(CT)
CON

# (example) create data for CT 
plot_data_CON <- expand.grid(
  CT = c(0, 1, 5)
)

# (example) compute CON from the CT data
plot_data_CON$CON <- get_CON(
  CT = plot_data_CON$CT
)

# (example) plot a visual graph for the p1-function
ggplot(plot_data_CON, aes(x = CT, y = CON)) +
  geom_point(shape = 8) +
  labs(title = "contingency between input and outcome", subtitle = "Dependent on Cotargets", y = "Contingency", x = "Number of Cotargets") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  theme_classic() +
  geom_line()



#' p2 compute the social pressure on each given target member of the group
#' 
#' @param CT number of people who are also viewed as targets of social impact. Ranges from 0 to infinite (discrete)
#' @param S number of people viewed as source of social pressure. Ranges from 0 to infinite (discrete)
#' 
#' @return (P) social pressure. Ranges from 0 to 1 (continuous)

get_P <- function (CT, S) {
  P <- S/(S+CT+1)
  return(P)
}

# (example) check vectorized form:
CT <- c(0, 1, 5)
S <- c(1, 4, 7)
P <- get_P(CT, S)
P

# (example) create data for CT and S 
plot_data_P <- expand.grid(
  CT = c(0, 1, 5),
  S = c(1, 4, 7)
)

# (example) compute pressure from CT and S
plot_data_P$P <- get_P(
  CT = plot_data_P$CT,
  S = plot_data_P$S
)

# (example) create an extra column for the plotting
plot_data_P$S_label <- paste("Sources:", plot_data_P$S)

# (example) plot a visual graph for the p2-function
ggplot(plot_data_P, aes(x = CT, y = P)) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data_P$S_label) +
  labs(title = "Pressure", subtitle = "Dependent on Cotargets", y = "Pressue", x = "Number of Cotargets") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  theme_classic() +
  geom_line()



#' t1 compute the incentive to give full effort
#' 
#' @param CON number of people who are also viewed as targets of social impact. Ranges from 0 to infinite (discrete)
#' 
#' @return (INC) incentive to give full effort. Ranges from 0 to 1 (continuous)

get_INC <- function(CON) {
  INC <- CON
  return(INC)
}


# (example) check vectorized form:
INC <- get_INC(CON)
INC

# (example) create data for CT 
plot_data_INC <- expand.grid(
  CON = CON
)

# (example) compute the incentive from CT
plot_data_INC$INC <- get_INC(
  CON = plot_data_INC$CON
)

# (example) plot a visual graph for the t1-function
ggplot(plot_data_CON, aes(x = CON, y = INC)) +
  geom_point(shape = 8) +
  labs(title = "incentive to give full effort", y = "Incentive", x = "Contingency") +
  scale_x_continuous(limits = c(0, 1), breaks = c(round(CON, digits = 2))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(round(INC, digits = 2))) +
  theme_classic() +
  geom_line()



#' p3 compute the individual effort 
#' 
#' @param INC incentive to give full effort. Ranges from 0 to 1 (continuous)
#' @param P social pressure. Ranges from 0 to 1 (continuous)
#' 
#' @return (IE) individual effort. Ranges from 0 to 1 (continuous)

get_IE <- function(INC, P) {
  IE <- 0.5 * INC + 0.5 * P
  return(IE)
}

# (example) check vectorized form:
IE <- get_IE(INC, P)
IE


# (example) create data for INC and P
plot_data_IE <- expand.grid(
  INC = INC,
  P = P
)

# (example) compute the individual effort from INC and P
plot_data_IE$IE <- get_IE(
  INC = plot_data_IE$INC,
  P = plot_data_IE$P
)

# (example) create an extra column for the plotting
plot_data_IE$P_label <- paste("Pressure:", round(plot_data_IE$P, digits = 2))

# (example) plot a visual graph for the p1-function
ggplot(plot_data_IE, aes(x = INC, y = IE)) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data_IE$P_label) +
  labs(title = "Individual effort", subtitle = "Dependent on Pressure + Incentive", y = "Individual effort", x = "Incentive") +
  scale_x_continuous(limits = c(0, 1), breaks = c(round(INC, digits = 2))) +
  scale_y_continuous(limits = c(0, 1), breaks = c(round(IE, digits = 2))) +
  theme_classic() +
  geom_line()



#' Superfunction - compute the individual effort with some noise (through all the previous functions)
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' @param S number of people viewed as source of social pressure. Ranges from 0 to infinite (discrete)
#' 
#' @return (IE) individual effort. Ranges from 0 to 1 (continuous)

get_super_IE <- function(CT, S) {
  CON <- get_CON(CT) 
  P <- get_P(CT, S)
  INC <- get_INC(CON)
  IE <- get_IE(INC, P)
  IE <- IE + rnorm(1, mean = 0, sd = 0.1)
  IE[IE > 1] <- 1
  IE[IE < 0] <- 0
  return(IE)
}

#' p4 Compute the coordination loss
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' 
#' @return (CL) coordination loss. Ranges from 0 to 3 (continuous)

get_CL <- function(CT) {
  n <- CT
  CL <- 4*sqrt(1*CT)/(1+sqrt(1*CT)) + rnorm(1, mean = 0, sd = 0.05)
  CL[CT == 0] <- 0 # theoretical constraint: if CT = 0 there cannot be coordination loss
  return(CL)
}

# (example) check vectorized form:
CT <- c(0, 1, 5, 10)
CL <- get_CL(CT)
CL

# (example) create data for CT 
plot_data_CL <- expand.grid(
  CT = c(0, 1, 5, 10)
)

# (example) compute CL from the CT data
plot_data_CL$CL <- get_CL(
  CT = plot_data_CL$CT
)
CT

# (example) plot a visual graph for the p4-function
ggplot(plot_data_CL, aes(x = CT, y = CL)) +
  geom_point(shape = 8) +
  labs(title = "coordination loss", subtitle = "Dependent on Cotargets", y = "Coordination Loss", x = "Number of Cotargets") +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  theme_classic() +
  geom_line()


#' p5-function (manifest) - compute the individual outcome
#' 
#' @param MC maximum capacity to scream. Ranges from 0 to 15 (continuous)
#' @param IE individual effort Ranges from 0 to 1 (continuous)
#' @param CL coordination loss Ranges from 0 to 1 (continuous)
#' 
#' @return (IO) individual outcome. Ranges from 0 to inf. (continuous)

get_IO <- function(MC, IE, CL) {
  IO <- MC * IE - CL  
  return (IO)
}

# (example) check vectorized form:
MC <- c(9, 10, 13)
CL <- 1
IO <- get_IO(MC, IE, CL)
IO

# (example) create data for MC, IE and CL
plot_data_IO <- expand.grid(
  MC = c(9, 10, 13),
  CL = c(0, 0.5, 1),
  IE = IE
)

# (example) compute the individual effort from MC, IE and CL
plot_data_IO$IO <- get_IO(
  MC = plot_data_IO$MC,
  IE = plot_data_IO$IE,
  CL = plot_data_IO$CL
)

# (example) create an extra column for the plotting
plot_data_IO$IE_label <- paste("Individual Effort:", round(plot_data_IO$IE, digits = 2))

# (example) plot a visual graph for the p1-function
ggplot(plot_data_IO, aes(x = MC, y = IO, linetype = factor(CL))) +
  geom_point(shape = 8) +
  facet_wrap(~IE_label) +
  labs(title = "Individual outcome", subtitle = "Dependent on Maximum + Individual effort + Coordination Loss", y = "Individual outcome", x = "Individual Maximum", linetype = "coordination loss") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_continuous(limits = c(min(MC), max(MC)), breaks = c(round(MC, digits = 2))) +
  scale_y_continuous(limits = c(min(plot_data_IO$IO), max(plot_data_IO$IO)), breaks = pretty(plot_data_IO$IO)) +
  theme_classic() +
  geom_line()


#' Superfunction - compute the individual outcome  (through all the previous functions)
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' @param S number of people viewed as source of social pressure. Ranges from 0 to infinite (discrete)
#' @param MC maximum capacity to scream. Ranges from 0 to 15 (continuous)
#' @param CL coordination loss. Ranges from 0 to 3 (continuous)
#' 
#' @return (IO) individual outcome. Ranges from 0 to inf (continuous)

get_super_IO <- function(CT, S, MC, CL){
  IO <- pmax(0, get_super_IE(CT, S) * MC - CL)
  return (IO)
}

get_super_IO



### Final Simulation

## Replicate the original Study of Latané et al. 

# Simulate virtual experiments from the model.
# ---------------------------------------------------------------------------
# We assume that the model is the true data generating model.
# As it is a deterministic model, any virtual participant
# will have on average the same individual effort based on 
# the number of Co-Targets (CT) and Sources of Social Pressure (S)


# Variability comes into the experiment by:
# (a) Different trait values for the maximum noise each person can produce
# (b) Randomness from the noise we compute in the superfuncion - due to e.g. 
#     faitque, personality, mental state etc.  during the experiment
# (c) Randomness from the noise we compute in the p4-function for coordination loss - due to
#     e. g. room size, differences in sound pressure waves, differences in synchronicity between
#     group members etc. during the experiment

# You can run the script repeatedly to simulate new replication studies,
# and observe the variability in outcomes due to sampling variability.

set.seed(123)

# set sample size of participants (we assume a within-person design)
# derived  from experiment 1 - the pseudo-groups (Latané et al.)
n_per_group <- 6 #participants per group
n_of_groups <- 6 #number of groups
n_total     <- n_per_group*n_of_groups #number of participants in total

# experimental design: Latané et al. (1979)
# create data for CT and S

df <- expand.grid(
  group = 1: n_of_groups,
  id = 1:n_per_group, 
  CT = c(0, 1, 5),
  S = c(1)
)

# determine the variables for a simulation of the exogenous variables
phi <- 5
alpha <- 0.922*phi
beta <- (1-0.922)*phi

scale_factor <- 12.29 / 0.922

# interindividual variability in exogenous variable ("maximum capacity")
max_per_person <- data.frame(
  group = rep(1:n_of_groups, each = n_per_group),
  id = rep(1:n_per_group, times = n_of_groups),
  maximum = scale_factor * rbeta(n_total, alpha, beta)
)

mean(max_per_person$maximum)

# merge into df
df <- merge(df, max_per_person, by = "id")

# compute the psychological outcome variable (IE)
df$IE <- get_super_IE(
  CT = df$CT,
  S = df$S
)

# compute coordination loss from CT
df$CL <- get_CL(
  CT = df$CT
)

# compute the manifest outcome variable (IO)
df$IO <- get_super_IO(
  CT = df$CT,
  S = df$S,
  MC = df$maximum,
  CL = df$CL
)

# look at the simulated data 
# View(df)

#compute the means of IO for each experimental group
m0 <- mean(df$IO[df$CT == 0], na.rm = TRUE)

m1 <- mean(df$IO[df$CT == 1], na.rm = TRUE)

m5 <- mean(df$IO[df$CT == 5], na.rm = TRUE)

data_final <- data.frame(
  groups = c(0, 1, 5),
  means = c(m0, m1, m5)
)

plot_final <- ggplot(data_final, aes(x = groups, y = means)) +
  geom_point(shape = 8) +
  labs(title = "Mean Individual Outcome (dyn/cm^2)", subtitle = "Dependent on experimental group (S = 1)", y = "Sound Pressure in dyn percm^2", x = "conditions (Number of Cotargets)") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  scale_y_continuous(limits = c(0, m0), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) +
  theme_classic() +
  geom_line()
plot_final


### Analysis of the simulated experiment
# Create factor variable for the experimental condition
df$CT_factor <- factor(df$CT,
                       levels = c(0,1,5),
                       labels = c("CT0","CT1","CT5"))


# Scatterplot of simulated raw data
ggplot(df, aes(x = CT, y = IO)) +
  geom_point() +
  theme_classic() +
  labs(
    title = "Individual Outcomes",
    x = "Number of Cotargets",
    y = "Individual Outcome"
  )


# ANOVA WITH coordination loss
fit_aov_CL <- aov(IO ~ CT_factor, data = df)
summary(fit_aov_CL)

# Effect size (eta squared) WITH coordination loss
anova_CL <- summary(fit_aov_CL)

SS_between_CL <- anova_CL[[1]]["CT_factor", "Sum Sq"]
SS_total_CL <- sum(anova_CL[[1]][,"Sum Sq"])

eta_squared_CL <- SS_between_CL / SS_total_CL
eta_squared_CL

# Outcome WITHOUT coordination loss
df$IO_noCL <- df$maximum * df$IE

# ANOVA WITHOUT coordination loss
fit_aov_noCL <- aov(IO_noCL ~ CT_factor, data = df)
summary(fit_aov_noCL)

# Effect size WITHOUT coordination loss
anova_noCL <- summary(fit_aov_noCL)

SS_between_noCL <- anova_noCL[[1]]["CT_factor", "Sum Sq"]
SS_total_noCL <- sum(anova_noCL[[1]][,"Sum Sq"])

eta_squared_noCL <- SS_between_noCL / SS_total_noCL
eta_squared_noCL

# Plot comparing outcomes with and without coordination loss

# compute outcome WITH coordination loss using SAME IE
df$IO <- pmax(0, df$maximum * df$IE - df$CL)

# Compute outcome WITHOUT coordination loss
df$IO_noCL <- df$maximum * df$IE

# Compute mean outcomes per CT condition
# mean outcome WITH coordination loss
m0_with <- mean(df$IO[df$CT == 0], na.rm = TRUE)
m1_with <- mean(df$IO[df$CT == 1], na.rm = TRUE)
m5_with <- mean(df$IO[df$CT == 5], na.rm = TRUE)

# mean outcome WITHOUT coordination loss
m0_without <- mean(df$IO_noCL[df$CT == 0], na.rm = TRUE)
m1_without <- mean(df$IO_noCL[df$CT == 1], na.rm = TRUE)
m5_without <- mean(df$IO_noCL[df$CT == 5], na.rm = TRUE)

# Create dataset for plotting
# create dataset for plot with coordination loss
data_with_CL <- data.frame(
  CT = c(0,1,5),
  mean_IO = c(m0_with, m1_with, m5_with),
  condition = "with coordination loss"
)

# create dataset for plot without coordination loss
data_without_CL <- data.frame(
  CT = c(0,1,5),
  mean_IO = c(m0_without, m1_without, m5_without),
  condition = "without coordination loss"
)

# combine both datasets
data_plot <- rbind(data_with_CL, data_without_CL)

#Plot both effects together
ggplot(data_plot, aes(x = CT, y = mean_IO, linetype = condition)) +
  geom_point(size = 3) +
  geom_line() +
  scale_linetype_manual(
    values = c("solid", "dashed"),
    labels = c("With coordination loss", "Without coordination loss")
  ) +
  labs(
    title = "Social Loafing vs Coordination Loss",
    subtitle = "Comparison of outcomes with and without coordination loss",
    x = "Number of Cotargets",
    y = "Mean Individual Outcome",
    linetype = "Condition"
  ) +
  scale_x_continuous(breaks = c(0,1,5)) +
  theme_classic()
