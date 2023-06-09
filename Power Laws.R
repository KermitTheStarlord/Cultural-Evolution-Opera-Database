library(poweRlaw)

# Name of work
play <- data.frame(table(rawData$Name.of.Work))
colnames(play) <- c("play","Freq")

data_pl <- displ$new(play$Freq)
est <- estimate_xmin(data_pl)
data_pl$xmin <- est$xmin
data_pl$pars <- est$pars
bs <- bootstrap_p(data_pl)

# Log-Normal
data_alt <- dislnorm$new(play$Freq)
data_alt$xmin <- est$xmin
data_alt$pars <- estimate_pars(data_alt)
comp <- compare_distributions(data_pl, data_alt)

# Exponential
data_alt <- disexp$new(play$Freq)
data_alt$xmin <- est$xmin
data_alt$pars <- estimate_pars(data_alt)
comp <- compare_distributions(data_pl, data_alt)

# Poisson
data_alt <- dispois$new(play$Freq)
data_alt$xmin <- est$xmin
data_alt$pars <- estimate_pars(data_alt)
comp <- compare_distributions(data_pl, data_alt)

nationalityOfComposer <- data.frame(table(rawData$Nationality.of.Composer))
colnames(natioanlityOfComposer) <- c("nat","Freq")

data_pl2 <- displ$new(nationalityOfComposer$Freq)
est2 <- estimate_xmin(data_pl2)
data_pl2$xmin <- est2$xmin
data_pl2$pars <- est2$pars
bs2 <- bootstrap_p(data_pl2)

nationalityOfPlay <- data.frame(table(rawData$Nationality.of.Work))
colnames(nationalityOfPlay) <- c("nat","Freq")

data_pl3 <- displ$new(nationalityOfComposer$Freq)
est3 <- estimate_xmin(data_pl3)
data_pl3$xmin <- est3$xmin
data_pl3$pars <- est3$pars
bs3 <- bootstrap_p(data_pl3)