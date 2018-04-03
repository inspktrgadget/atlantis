# function to parse ages from raw atlantis output to individual yearclasses
parse_ages <- function(atl_output, z_vals) {
    base_col_names <- grep("^count|^age", colnames(atl_output), value = TRUE, invert = TRUE)
    base_cols <- subset(atl_output, select = base_col_names)
    ages <- unique(atl_output$age)
    atl_output$z <- z_vals[match(atl_output$age, ages)]
    init_count <- (atl_output$count / (exp(-atl_output$z) + 1))
    next_yr_count <- atl_output$count - init_count
    first_yr_data <- data.frame(base_cols, age=atl_output$age, count=init_count)
    next_yr_data <- data.frame(base_cols, age=(atl_output$age+1), count=next_yr_count)
    return(rbind(first_yr_data, next_yr_data))
}

# function to smooth weight data by age from raw atlantis output
smooth_atl_weight <- function(data) {
    temp <- 
        data %>% 
        filter(count >= 1) %>%
        group_by(year, month, age) %>%
        summarize(mn_wt = mean(weight)) %>%
        ungroup()
    wt_gr_fun_nll <- function(par, wts, ages) {
        linf <- par[1]
        k <- par[2]
        t0 <- par[3]
        alpha <- par[4]
        beta <- par[5]
        means <- vb_wt(linf, k, t0, alpha, beta, ages)
        return(-sum(dnorm(wts, means, sd = 1e-5, log = TRUE)))
    }
    opt_params <- 
        temp %>%
        filter(month %in% 1:9) %>%
        mutate(age = age + ((month - 1) / 12)) %>%
        group_by(year) %>%
        do(broom::tidy(optim(fn = wt_gr_fun_nll, 
                      par = c(linf = 130, k = 0.15, t0 = 0, alpha = 0.0021, beta = 3.3437),
                      wts = .$mn_wt,
                      ages = .$age))) %>%
        spread(key = parameter, value = value)
    smooth_wts <- 
        data %>%
        left_join(opt_params, by = "year") %>%
        mutate(weight = vb_wt(linf, k, t0, alpha, beta, age = age + ((month - 1) / 12)))
    return(select(smooth_wts, -alpha, -beta, -k, -linf, -t0))
}

# von Bertalanffy weight function
vb_wt <- function(linf, k, t0, alpha, beta, age) {
    return(alpha * (linf ^ beta) * ((1 - exp(-k * (age - t0)))^beta))
}

# convert weight to length
wt2len <- function(alpha, beta, weight) {
    return((weight / alpha) ^ (1 / beta))
}




