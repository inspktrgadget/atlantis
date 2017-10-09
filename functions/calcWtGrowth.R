# this is a source code file to compute growth parameters for use in formatting
# atlantis output
source('../functions/exponentialGrowth.R')
source('../functions/quarticGrowth.R')
source('../functions/lengthWeight.R')

# calculate growth parameters
atl.sub <- 
    is_fg_count %>%
    filter(count >= 1)

# using the length weight relationship for even and odd years as ages are parsed
lw.min <- 
    smooth.len %>%
    #mutate(even.odd = age %% 2) %>%
    #group_by(even.odd) %>%
    summarize(a = nlm(lw.sse, c(1e-03, 3), length, weight)$estimate[1],
              b = nlm(lw.sse, c(1e-03, 3), length, weight)$estimate[2])

# # using a quartic growth function
# # THIS IS OFF BY A BIT
# quartic.growth.min <-
#     atl.sub %>%
#     mutate(age = age + 0.5) %>%
#     group_by(year, month) %>%
#     summarize(a = nlm(quartic.growth.sse, c(1,1,1,1,0), weight, age)$estimate[1],
#               b = nlm(quartic.growth.sse, c(1,1,1,1,0), weight, age)$estimate[2],
#               c = nlm(quartic.growth.sse, c(1,1,1,1,0), weight, age)$estimate[3],
#               d = nlm(quartic.growth.sse, c(1,1,1,1,0), weight, age)$estimate[4],
#               e = nlm(quartic.growth.sse, c(1,1,1,1,0), weight, age)$estimate[5])


# # THIS IS A BIT OLDER AND DIDN'T WORK VERY WELL
# # compute growth parameters using nlm on functions in exponentialGrowth.R
# exp.growth.min <- 
#     atl.sub %>%
#     group_by(year, month) %>%
#     summarize(max.wt = nlm(exp.growth.sse, c(10000, 0.3, 7), weight, age)$estimate[1],
#               a = nlm(exp.growth.sse, c(10000, 0.3, 7), weight, age)$estimate[2],
#               c = nlm(exp.growth.sse, c(10000, 0.3, 7), weight, age)$estimate[3])