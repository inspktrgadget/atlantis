# this is a source code file to compute growth parameters for use in formatting
# atlantis output
source('../functions/vbParams.R')
source('../functions/vbSimple.R')

# calculate growth parameters
atl.sub <- 
    is_fg_count %>%
    filter(count >= 1)

# compute growth parameters using nlm on functions in vbParams.R
vbMin <- 
    atl.sub %>%
    mutate(age = age + 0.5) %>%
    group_by(year, month) %>%
    summarize(linf = nlm(vb.sse, c(100, 0.1, -1), length, age)$estimate[1],
              k = nlm(vb.sse, c(100, 0.1, -1), length, age)$estimate[2],
              t0 = nlm(vb.sse, c(100, 0.1, -1), length, age)$estimate[3])

vbSimpleMin <- 
    atl.sub %>%
    mutate(age = age) %>%
    group_by(year, month) %>%
    summarize(simple.linf = nlm(vb.simple.sse, c(164, 0.07, 27), length, age)$estimate[1],
              simple.k = nlm(vb.simple.sse, c(164, 0.07, 27), length, age)$estimate[2],
              simple.recl = nlm(vb.simple.sse, c(164, 0.07, 27), length, age)$estimate[3])