# script to test whether aggregating mean length at age for two year age groups
# changes von Bertalanffy growth parameters as compared with actual length at age

# define growth function and sse optimizers
vb <- function(linf, k, t0, age) {linf * (1 - exp(-k * (age - t0)))}
vb_optimizer <- function(data, age) {
    linf <- data[1]
    k <- data[2]
    t0 <- data[3]
    return(vb(linf, k, t0, age))
}
vb_sse <- function(data, age, length) {
    y_hat <- vb_optimizer(data, age)
    return(sum((length - y_hat)^2))
}

# make up some data
dat <- data.frame(age = sort(rep(1:10, 100)))
dat$length <- vb(100, 0.2, -0.5, dat$age) + rnorm(nrow(dat), 0, 2)

# find parameters - nlm does so nearly perfectly
dat_vb_params <- nlm(vb_sse, c(110, 0.1, 0), dat$age, dat$length)


# now aggregate the data and test to see if params differ
dat2 <- dat
dat2$age <- dat2$age + (dat2$age %% 2)
age_means <- tapply(dat2$length, dat2$age, mean)
dat2 <- data.frame(length = age_means)
dat2$age <- as.numeric(rownames(dat2))

# find parameters for aggregated length at age
dat2_vb_params <- nlm(vb_sse, c(110, 0.1, 0), dat2$age, dat2$length)
