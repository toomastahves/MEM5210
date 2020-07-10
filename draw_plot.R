library(corrplot)

draw_plot <- function(data) {
  data$Loan_Status <- as.numeric(data$Loan_Status)
  M <- cor(data)
  corrplot(M, method='color')
  
  data$Loan_Status <- as.numeric(data$Loan_Status)
  result <- prcomp(data, center=TRUE, scale=TRUE)
  print(summary(result))
  screeplot(result, main="PCA", type="lines", ylim=c(0,3), npcs = 16)
  cumpro <- cumsum(result$sdev^2 / sum(result$sdev^2))
  plot(cumpro[0:16], xaxt="n", yaxt="n", xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
  abline(v = 12, col="blue", lty=5)
  abline(h = 0.88759, col="blue", lty=5)
  xtick<-seq(0, 16, by=1)
  text(x=xtick,  par("usr")[3], labels = xtick, pos = 1, xpd = TRUE)
  ytick<-seq(0.2, 1, by=0.1)
  text(y=ytick,  par("usr")[3], labels = ytick, pos = 2, xpd = TRUE)
  axis(side=1, at=xtick, labels = FALSE)
}