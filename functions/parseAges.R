# the following function is to parse out numbers by cohort and place them in 
# one year age classes - you must have already merged the mortality data

parseAges <- function(data) {
    base.cols <- select(data, depth:length, maturity_stage);
    init.count <- (data$count / (exp(-data$z) + 1));
    next.yr.count <- data$count - init.count;
    first.yr.data <- data.frame(base.cols, age=data$age, count=init.count);
    next.yr.data <- data.frame(base.cols, age=(data$age+1), count=next.yr.count);
    return(rbind(first.yr.data, next.yr.data))
}

parseCatchAges <- function(data) {
    base.cols <- select(data, -count, -z, -age);
    init.count <- (data$count / (exp(-data$z) + 1));
    next.yr.count <- data$count - init.count;
    first.yr.data <- data.frame(base.cols, age=data$age, count=init.count);
    next.yr.data <- data.frame(base.cols, age=(data$age+1), count=next.yr.count);
    return(rbind(first.yr.data, next.yr.data))
}