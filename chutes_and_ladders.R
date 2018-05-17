# function to simulate chutes and ladders game
chutesNladders <-
function(nplayers=1)
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

  transmat <- 1:106
  names(transmat) <- as.character(1:106)
  transmat[transitions[,1]] <- transitions[,2]

  lastpos <- 0
  curpos <- history <- NULL
  while(all(curpos < 100)) {
    curpos <- lastpos + sample(1:6, nplayers, repl=TRUE)
    curpos <- transmat[curpos]
    if(any(curpos > 100)) curpos[curpos > 100] <- lastpos[curpos > 100]

    lastpos <- curpos
    history <- rbind(history, curpos)
  }

  history
}


# for output from chutesNladders, which of the players won?
whowon <-
function(chutesOutput)
  min(which(chutesOutput[nrow(chutesOutput),] == 100))

# for output from chutesNladders, how many total spins were there?
nspins <-
function(chutesOutput)
  ncol(chutesOutput)*(nrow(chutesOutput)-1) +
    whowon(chutesOutput)

