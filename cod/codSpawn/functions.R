init_age <- function(init_max, init_min, init_decay, age) {
    (exp((-1) * init_decay * age) * (init_max - init_min)) + init_min
}

bev_holt <- function(mu, lambda, ssb) {
    return((mu * ssb) / (lambda + ssb))
}