# Regarding Chutes & Ladders:
# numerical calculations (rather than simulations)

# calcTM: calculate transition matrix
#   (101 x 101), first is starting position; rest are board locations 1-100
calcTM <-
function()
{
  transitions <- rbind(
    c(1, 38),
    c(4, 14),
    c(9, 31),
    c(16, 6),
    c(21, 42),
    c(28, 84),
    c(36, 44),
    c(48, 26),
    c(49, 11),
    c(51, 67),
    c(56, 53),
    c(62, 19),
    c(64, 60),
    c(71, 91),
    c(80, 100),
    c(87, 24),
    c(93, 73),
    c(95, 75),
    c(98, 78))

  # simple transition matrix, not accounting for chutes and ladders
  tm <- matrix(0,ncol=107, nrow=107)
  dimnames(tm) <- list(as.character(0:106), as.character(0:106))
  tm[col(tm) > row(tm) & col(tm) < row(tm) + 7] <- 1/6

  # account for the chutes and ladders
  for(i in 1:nrow(transitions)) {
    tm[,transitions[i,2]+1] <- tm[,transitions[i,2]+1] + tm[,transitions[i,1]+1]
    tm[,transitions[i,1]+1] <- 0
  }

  # can't go past 100
  # (not efficient code, but who cares)
  for(i in 1:nrow(tm)) {
    tm[i,i] <- tm[i,i] + sum(tm[i,102:107])
    tm[i,102:107] <- 0
  }

  tm[1:101,1:101]
}

# calculate transition matrix
tm <- calcTM()
# start vector
start <- rbind(c(1, rep(0, 100)))
# vector to pull out end probability
end <- cbind(c(rep(0, 100), 1))

# calculation full distribution at each step
maxsteps <- 400
probs <- matrix(nrow=maxsteps, ncol=ncol(tm))
rownames(probs) <- as.character(1:maxsteps)
probs[1,] <- start %*% tm
for(i in 2:maxsteps)
  probs[i,] <- probs[i-1,] %*% tm

# Pr(game complete by round k, 1 player)
cdf <- probs %*% end

# pr(game complete by round k, n players)
cdf <- cbind(cdf)
for(n in 2:4)
  cdf <- cbind(cdf, 1-(1-cdf[,1])^n)
colnames(cdf) <- as.character(1:4)

# pr(complete game at round k, n players), by differences
stopprob <- apply(cdf, 2, function(a) diff(c(0, a)))

library(broman)
library(RColorBrewer)
col <- brewer.pal(3, "Dark2")
br <- seq(0.5, nrow(stopprob)+0.5, by=1)
png("chutes_and_ladders_rounds.png", height=900, width=1800, res=288, pointsize=12)
par(mar=c(3.1, 4.1, 1.1, 1.1))
grayplot(histlines(br, stopprob[,1]), type="l", lwd=2, xlim=c(0, 150), xaxs="i", yaxs="i",
         ylim=c(0, max(stopprob)*1.05), ylab="", xlab="No. rounds", yat=seq(0, 0.05, by=0.01),
         xat=sort(c(7, seq(0, 150, by=25))), vlines=seq(0, 150, by=25), mgp=c(1.6, 0.2, 0),
         hlines=seq(0, 0.05, by=0.01))
title(ylab="Probability", mgp=c(2.4, 0, 0))
for(i in 2:4)
  lines(histlines(br, stopprob[,i]), col=col[i-1], lwd=2)
for(i in 1:4) {
  dens <- histlines(br, stopprob[,i])
  ymx <- max(dens[,2])
  xmx <- max(dens[dens[,2]==max(dens[,2]),1])
  text(xmx+c(10, 9, 6, 6)[i], ymx*0.95, paste0(i, " player", c("", "s", "s","s")[i]),
       adj=c(0, 0.5), col=c("black", col)[i])
}
dev.off()

# pr(player j wins at round k)
#   cdf = Pr(end by round k for a single player)
prob.player.wins <-
function(cdf, n.players=4)
{
  if(is.na(match(n.players, 2:4)))
    stop("n.players should be in {2,3,4}")

  stopprob <- diff(c(0, cdf))

  result <- cbind(stopprob * (stopprob + 1-cdf)^(n.players-1))

  if(n.players == 2)
    return(cbind(result, (1-cdf)*stopprob))

  result <- cbind(result, (1-cdf)*stopprob*(stopprob + 1-cdf)^(n.players-2))

  if(n.players == 3)
    return(cbind(result, (1-cdf)^2*stopprob))

  cbind(result, (1-cdf)^2*stopprob*(stopprob + 1-cdf),
        (1-cdf)^3*stopprob)
}

p2 <- prob.player.wins(cdf[,1], 2)
p3 <- prob.player.wins(cdf[,1], 3)
p4 <- prob.player.wins(cdf[,1], 4)


png("advantage_to_first_player.png", height=900, width=1800, res=288, pointsize=12)
par(mar=c(3.1, 4.1, 1.1, 1.1))
grayplot(7:101, (p2[,1]/rowSums(p2))[7:101]*2, type="l", lwd=2, xlim=c(6.5, 100.5), xaxs="i", yaxs="i",
         ylim=c(0.998, 1.068), ylab="", xlab="No. rounds to complete game", yat=seq(1, 1.06, by=0.01),
         xat=c(7, seq(25, 100, by=25)), vlines=c(7,seq(25, 100, by=25)), mgp=c(1.6, 0.2, 0),
         hlines=seq(1, 1.06, by=0.01), col=col[1])
title(ylab="Relative advantage for player 1", mgp=c(2.4, 0, 0))
lines(7:101, (p3[,1]/rowSums(p3))[7:101]*3, col=col[2], lwd=2)
lines(7:101, (p4[,1]/rowSums(p4))[7:101]*4, col=col[3], lwd=2)
mx <- c(max(p2[,1]/rowSums(p2)*2, na.rm=TRUE),
        max(p3[,1]/rowSums(p3)*3, na.rm=TRUE),
        max(p4[,1]/rowSums(p4)*4, na.rm=TRUE))
text(rep(99, 3), mx+0.0015, paste(2:4, "players"), adj=c(1, 0), col=col)
dev.off()


# pr(finish after k spins)
#   cdf = Pr(end by round k for a single player)
probBySpins <-
function(cdf, n.players=4)
{
  if(is.na(match(n.players, 2:4)))
    stop("n.players should be in {2,3,4}")

  # prob player k wins at round n
  p <- prob.player.wins(cdf, n.players)
  p <- p/rowSums(p)

  cdf <- 1 - (1-cdf)^n.players
  stopprob <- diff(c(0, cdf))

  p <- as.numeric(t(p * stopprob))
  p[is.na(p)] <- 0
  p
}

stopprobBySpins <-
  cbind(stopprob[,1],
        probBySpins(cdf[,1], 2)[1:nrow(stopprob)],
        probBySpins(cdf[,1], 3)[1:nrow(stopprob)],
        probBySpins(cdf[,1], 4)[1:nrow(stopprob)])

breaks <- seq(0.5, nrow(stopprobBySpins)+0.5, by=1)
dens <- matrix(ncol=ncol(stopprobBySpins)+1, nrow=length(breaks)*2)
for(i in 1:ncol(stopprobBySpins)) {
  tmp <- histlines(breaks, stopprobBySpins[,i], use="density")
  if(i==1) dens[,1] <- tmp[,1]
  dens[,i+1] <- tmp[,2]
}

png("chutes_and_ladders_spins_exact.png", height=900, width=1800, res=288, pointsize=12)
par(mar=c(3.1, 4.1, 1.1, 1.1))
col <- brewer.pal(3, "Dark2")
grayplot(dens[,1], dens[,2], xlab="No. spins", ylab="", col="black", lwd=2, type="l",
         yat = seq(0, 0.025, by=0.005), hlines=seq(0, 0.025, by=0.005),
         xlim=c(0, 200), xaxs="i", yaxs="i", ylim=c(0, max(dens[,2])*1.03),
         xat=seq(0, 200, by=50), vlines=seq(0, 200, by=50), mgp=c(1.8, 0.3, 0))
title(ylab="Probability", mgp=c(2.7, 0, 0))
for(i in 3:ncol(dens))
  lines(dens[,1], dens[,i], lwd=2, col=col[i-2])
ymx <- apply(dens[,-1], 2, max)
xmx <- apply(dens[,-1], 2, function(a,b) max(b[a==max(a)]), dens[,1])
text(xmx+c(8, 12, 10, 20), ymx*0.95, paste0(1:4, " player", c("", "s", "s", "s")),
     adj=c(0, 0.5), col=c("black",col))
dev.off()
