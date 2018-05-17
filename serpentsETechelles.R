library(tidyverse)

serpentsETechelles <- function(njoueurs = 1, partie = 1){

  echelles <- tibble(
    debut = c(1,4,9,21,28,36,51,71,80),
    fin = c(38,14,31,42,84,44,67,91,100)
  )
  serpents <- tibble(
    debut = c(98,95,93,87,64,62,56,49,47,16),
    fin = c(78,75,73,24,60,19,53,11,26,6)
  )

  partie <- tibble(
    no = partie,
    joueur = 1:njoueurs,
    njoueurs = njoueurs,
    ncoups = 0,
    gagnant = FALSE,
    e1 = 0,
    e2 = 0,
    e3 = 0,
    e4 = 0,
    e5 = 0,
    e6 = 0,
    e7 = 0,
    e8 = 0,
    e9 = 0,
    s1 = 0,
    s2 = 0,
    s3 = 0,
    s4 = 0,
    s5 = 0,
    s6 = 0,
    s7 = 0,
    s8 = 0,
    s9 = 0,
    s10 = 0
  )

  lastpos <- 0
  ncoups <- 0
  curpos <- NULL

  while(all(curpos < 100)) {

    ncoups <- ncoups + 1
    curpos <- lastpos + sample(1:6, njoueurs, replace = TRUE)

    for (i in seq(njoueurs)){
      # Echelles
      id <- which(echelles$debut %in% curpos[i])
      if (length(id) > 0){
        curpos[i] <- echelles$fin[id]
        partie[i, id + 4] <- partie[i, id + 4] + 1
      }
      # Serpents
      id <- which(serpents$debut %in% curpos[i])
      if (length(id) > 0){
        curpos[i] <- serpents$fin[id]
        partie[i, id + 13] <- partie[i, id + 13] + 1
      }
    }

    #if(any(curpos > 100)) curpos[curpos > 100] <- lastpos[curpos > 100]

    lastpos <- curpos

  }

  partie$ncoups <- ncoups
  partie$gagnant[which(lastpos >= 100)] <- TRUE

  return(partie)

}

iter <- 1000
partie <- NULL

start_time <- Sys.time()

for (i in seq(iter)){
  game <- serpentsETechelles(4, i)
  partie <- rbind(partie, game)
}

end_time <- Sys.time()

print(end_time - start_time)

partie %>%
  group_by(no) %>%
  summarise(e1 = sum(e1), e2 =  sum(e2))
