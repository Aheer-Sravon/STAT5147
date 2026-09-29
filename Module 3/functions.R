require(ggplot2)

plot.discrete <- function(y, proby) {
bars <- as.data.frame(cbind(y, proby))

ggplot(bars, aes(x=y, y=proby)) +
  geom_bar(stat="identity", width=1, fill="blue", colour="black") +
  theme_bw() +
  theme(axis.title.y=element_text(size=rel(1.4)),
        axis.title.x=element_text(size=rel(1.4)),
        axis.text.x=element_text(size=rel(1.2)),
        axis.text.y=element_text(size=rel(1.2)),
        plot.title=element_text(hjust=0.5, size=rel(1.6))) +
  labs(
    x="y",
    y="p(y)",
    title="Distribution of Discrete R.V."
  )
}

plot.binom <- function(n, p) {
  y <- c(0:n)
  proby <- dbinom(0:n, n, p)
  bars <- as.data.frame(cbind(y, proby))
  
  ggplot(bars, aes(x=y, y=proby)) +
    geom_bar(stat="identity", width=1, fill="blue", colour="black") +
    theme_bw() +
    theme(axis.title.y=element_text(size=rel(1.4)),
          axis.title.x=element_text(size=rel(1.4)),
          axis.text.x=element_text(size=rel(1.2)),
          axis.text.y=element_text(size=rel(1.2)),
          plot.title=element_text(hjust=0.5, size=rel(1.6))) +
    labs(x="Number of Successes (y)",
         y="Probability",
         title="Distribution of Binomial R.V.")
}

plot.pois<- function(lambda){
  maxy<- qpois(0.9999, lambda)
  y<- c(0:maxy)
  proby<- dpois(0:maxy, lambda)
  Bars<- as.data.frame(cbind(y, proby))
  ggplot(Bars, aes(x = y, y = proby))+
    geom_bar(stat="identity", width = 1,
             fill = "blue", colour = "black")+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.6)))+
    labs(x = "Number of Events (y)",
         y = "Probability",
         title = "Distribution of Poisson R.V.")
}