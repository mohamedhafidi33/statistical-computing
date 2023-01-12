library(bootstrap)
library(boot)


set.seed(2117)
x <-rnorm(40,100,25)


#definiert eine Funktion, die mit dem Namen CV bezeichnet wird, um den Variationskoeffizienten zu berechnen.
CV <- function(x,i) {sqrt (var(x[i]))/ mean (x[])}
CV(x) #[1] 0.2875216

# Ein Plot des Histogramms dieser Werte folgt mit
hist(boot,col = "orange")

#Jackknife
results <- jackknife (x , CV )
results # std.error = 0.03412704 , bias = -0.002143762
mean(results$jack.values) #[1] 0.2874666


#Bootstrap
boots = boot(x,CV,1000)
boots$t0 #[1] 0.2875216
boots # std.error = 0.03271438 , bias = -0.005703249