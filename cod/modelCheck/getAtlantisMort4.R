# script to find the optimum M (both constant and as a function of age) that 
# smooths the parsed age distribution in atlantis
# I will then try using this as fixed M in gadget under the assumption that
# these values which smooth counts across ages the best must be the correct M
# this is probably a horrible assumption, but I'm not sure what else to try at this point

# NOTE: After messing around with this for quite some time and thinking it over I have
# come to the conclusion that finding the m or m_age_fun parameters that smooth the
# parsed ages most optimally actually tells you nothing about M in atlantis.
# all it tells you is the optimum(s) for smoothing age groups

#------------------------------------------------------------------------------------------
# define smoothing function to predict optimum count at age decay
count_by_age <- function(m, max_count, min_count, age) {
    cba <- ((exp(-(m) * max_count * age)) * (max_count - min_count)) + min_count
    return(cba)
}

m <- function(m, N, age) {
    return(N * exp(-(m * age)))
}

m_sse <- function(m, N, age, count) {
    yhat <- m(m, N, age)
    return(sum((yhat - count)^2))
}

#---------------------------------------------------
# starting with a solution for constant M across age

# using an M of 0.35 for my base to calculate count across age function
m35_count <- 
    is_fg_count %>%
    mutate(m = 0.35) %>%
    parseAges(.) %>%
    arrange(year, month, day, area, depth, age) %>%
    group_by(year, month, age) %>%
    summarize(total = sum(count))

smooth_const_m <- 
    m35_count %>%
    mutate(m = nlm(m_sse, c(0.35),
                   N = total[1],
                   age = age, count = total)$estimate) %>% 
    mutate(pred_count = m(m, total[1], age))

med_m <- median(smooth_const_m$m)

const_m_vals <- data.frame(age = 0:19, m = med_m)


#--------------------------------------------------------------------------
# now trying the same exercise but with m as a decreasing function of age

m_age_fun <- function(max_m, min_m, m_decay, age) {
    return(exp((-1)*m_decay*age)*(max_m - min_m) + min_m)
}

m_by_age_count <- function(data, N, age) {
    max_m <- data[1]
    min_m <- data[2]
    m_decay <- data[3]
    count <- N
    busch_na <- 
        vapply(2:length(age), function(x) {
            count[x] <<- count[x - 1] * exp(-(m_age_fun(max_m, min_m, m_decay, age[x])))
            return(NA)
        }, numeric(1))
    return(count)
}

m_age_fun_sse <- function(data, N, age, count) {
    yhat <- m_by_age_count(data, N, age)
    return(sum((yhat - count)^2))
}

smooth_var_m <- 
    m35_count %>%
    do(m_params = nlm(m_age_fun_sse, c(0.3, 0.3, 0.3), N = .$total[1],
                      age = .$age, count = .$total)$estimate)

smooth_var_m_tidy <- 
    smooth_var_m %>%
    tidy(., m_params) %>%
    ungroup() %>%
    mutate(var = rep(c("max_m", "min_m", "m_decay"), (n() / 3))) %>%
    spread(key = var, value = x)

test <- 
    left_join(m35_count, smooth_var_m_tidy) %>%
    mutate(pred_count = m_by_age_count(data = c(max_m, min_m, m_decay), 
                                       N = total[1], age = age))
