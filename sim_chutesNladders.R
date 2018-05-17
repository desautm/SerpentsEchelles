source("chutes_and_ladders.R")

n.players <- 2:4
n.sim <- 10000

# simulate different numbers of players and summarize results
simSum <- function(junk, n.players)
{
  result <- rep(NA, length(n.players)*2)
  for(i in seq(along=n.players)) {
    out <- chutesNladders(n.players[i])
    result[i] <- nspins(out)
    result[i+length(n.players)] <- whowon(out)
  }
  result
}

# simulation in parallel
result <- mclapply(1:n.sim, simSum, n.players=n.players)
result <- matrix(unlist(result), nrow=n.sim, byrow=TRUE)
result.nspins <- result[,1:3]
result.whowon <- result[,4:6]
colnames(result.nspins) <- colnames(result.whowon) <- n.players

# save result
save(result.nspins, result.whowon, file="chutes_results.RData")

# average no. spins
colMeans(result.nspins)

# 90th percentile
apply(result.nspins, 2, quantile, 0.9)

# chance that the first player wins
colMeans(result.whowon == 1)

# histograms of the no. spins by no. players
library(RColorBrewer)
library(broman) # get it from github: http://github.com/kbroman/broman

breaks <- seq(-0.5, max(result.nspins)+0.5, by=1)
dens <- matrix(ncol=ncol(result.nspins)+1, nrow=length(breaks)*2)
for(i in 1:ncol(result.nspins)) {
  tmp <- histlines(result.nspins[,i], breaks=breaks, use="density")
  if(i==1) dens[,1] <- tmp[,1]
  dens[,i+1] <- tmp[,2]
}

png("chutes_and_ladders_spins.png", height=900, width=1800, res=288, pointsize=12)
par(mar=c(3.1, 4.1, 1.1, 1.1))
col <- brewer.pal(length(n.players), "Dark2")
grayplot(dens[,1], dens[,2], xlab="No. spins", ylab="", col=col[1], lwd=2, type="l",
         yat = seq(0, 0.02, by=0.005), hlines=seq(0, 0.02, by=0.005),
     xlim=c(0, 200), xaxs="i", yaxs="i", ylim=c(0, max(dens[,2])*1.05),
         xat=seq(0, 200, by=50), vlines=seq(0, 200, by=50), mgp=c(1.8, 0.3, 0))
title(ylab="Probability", mgp=c(2.7, 0, 0))
for(i in 2:length(n.players))
  lines(dens[,1], dens[,i+1], lwd=2, col=col[i])
ymx <- apply(dens[,-1], 2, max)
xmx <- apply(dens[,-1], 2, function(a,b) max(b[a==max(a)]), dens[,1])
text(xmx+c(12, 10, 20), ymx*0.95, paste(n.players, "players"), adj=c(0, 0.5), col=col)
dev.off()
