library(ggplot2)
library(gridExtra)
library(dplyr)
library(coda)

params <- read.csv('params.csv')

params$CL <- exp(params$logCL)
params$V1 <- exp(params$logV1)

params.mcmc <- mcmc(params %>% filter(X>=100000 & (X%%100 == 0)) %>% select(-X))

#plot(params.mcmc)
effectiveSize(params.mcmc)

summary(params.mcmc)
HPDinterval(params.mcmc)

my.traceplot <- function(pname, .params=params) {
  g1 <- ggplot(.params, aes_string(x="X", y=pname)) +
    geom_line() +
    xlab('Iterations')
  g2 <- ggplot(.params, aes_string(y=pname)) +
    geom_histogram(aes(x=..density..), bins=30, fill="cyan", color="black") +
    geom_density(color="red")
  list(g1, g2)
}

plot <- plot1 <- my.traceplot("CL"  ); grid.arrange(plot[[1]], plot[[2]], nrow=1)
plot <- plot2 <- my.traceplot("V1"  ); grid.arrange(plot[[1]], plot[[2]], nrow=1)
plot <- plot3 <- my.traceplot("sg"  ); grid.arrange(plot[[1]], plot[[2]], nrow=1)
plot <- plot4 <- my.traceplot("omCL"); grid.arrange(plot[[1]], plot[[2]], nrow=1)
plot <- plot5 <- my.traceplot("omV1"); grid.arrange(plot[[1]], plot[[2]], nrow=1)

grid.arrange(plot1[[1]], plot1[[2]], plot2[[1]], plot2[[2]],
             plot3[[1]], plot3[[2]],
             ncol=2)
grid.arrange(plot4[[1]], plot4[[2]], plot5[[1]], plot5[[2]],
             ncol=2)
