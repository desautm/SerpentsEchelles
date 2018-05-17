SerpentsEchelles <- function(njoueurs = 1, nparties = 1, pile = TRUE){

  echelles <- tibble(
    debut = c(1,4,9,21,28,36,51,71,80),
    fin = c(38,14,31,42,84,44,67,91,100)
  )
  serpents <- tibble(
    debut = c(98,95,93,87,64,62,56,49,47,16),
    fin = c(78,75,73,24,60,19,53,11,26,6)
  )

  plateau <- 1:106
  plateau[echelles$debut] <- echelles$fin
  plateau[serpents$debut] <- serpents$fin

  precedent <- 0
  ncoups <- 0
  maintenant <- trajet <- NULL

  while(all(maintenant < 100)) {

    ncoups <- ncoups + 1
    maintenant <- precedent + sample(1:6, njoueurs, replace = TRUE)
    maintenant <- plateau[maintenant]

    # On doit arriver exactement sur la case 100
    if (pile) {
      if(any(maintenant > 100)) maintenant[maintenant > 100] <- precedent[maintenant > 100]
    }
    else {
      if(any(maintenant > 100)) maintenant[maintenant > 100] <- 100
    }

    precedent <- maintenant
    trajet <- rbind(trajet, maintenant)

  }

  trajet <- as.tibble(trajet)
  colnames(trajet) <- paste0(rep("Joueur",njoueurs),1:njoueurs)

  return(as.tibble(trajet))

}

NCoups <- function(partie){

  return((nrow(partie)-1)*ncol(partie)+Gagnant(partie))

}

Gagnant <- function(partie){

  id <- min(which(partie[nrow(partie),] == 100))
  return(id)

}

# Simulations

nparties <- 10000

# On doit tomber pile sur la case finale

partiespile <- vector("list", 4)

for (i in seq(4)){
  partiespile[[i]] <- map(1:nparties, ~SerpentsEchelles(njoueurs = i, nparties = .x, pile = TRUE))
}

ncoupspile1 <- tibble(
  ncoups = map_dbl(partiespile[[1]], ~ NCoups(.x)),
  njoueurs = as.factor(1),
  pile = TRUE
  )
ncoupspile2 <- tibble(
  ncoups = map_dbl(partiespile[[2]], ~ NCoups(.x)),
  njoueurs = as.factor(2),
  pile = TRUE
)
ncoupspile3 <- tibble(
  ncoups = map_dbl(partiespile[[3]], ~ NCoups(.x)),
  njoueurs = as.factor(3),
  pile = TRUE
)
ncoupspile4 <- tibble(
  ncoups = map_dbl(partiespile[[4]], ~ NCoups(.x)),
  njoueurs = as.factor(4),
  pile = TRUE
)

# On ne doit pas nécessairement tomber pile sur la case finale

partiesface <- vector("list", 4)

for (i in seq(4)){
  partiesface[[i]] <- map(1:nparties, ~SerpentsEchelles(njoueurs = i, nparties = .x, pile = FALSE))
}
ncoupsface1 <- tibble(
  ncoups = map_dbl(partiesface[[1]], ~ NCoups(.x)),
  njoueurs = as.factor(1),
  pile = FALSE
)
ncoupsface2 <- tibble(
  ncoups = map_dbl(partiesface[[2]], ~ NCoups(.x)),
  njoueurs = as.factor(2),
  pile = FALSE
)
ncoupsface3 <- tibble(
  ncoups = map_dbl(partiesface[[3]], ~ NCoups(.x)),
  njoueurs = as.factor(3),
  pile = FALSE
)
ncoupsface4 <- tibble(
  ncoups = map_dbl(partiesface[[4]], ~ NCoups(.x)),
  njoueurs = as.factor(4),
  pile = FALSE
)

ncoups <- rbind(ncoupspile1, ncoupspile2, ncoupspile3, ncoupspile4,
                ncoupsface1, ncoupsface2, ncoupsface3, ncoupsface4)

ggplot(data = ncoups)+
  geom_freqpoly(mapping = aes(x = ncoups,
                              y = ..count../nparties,
                              color = pile),
                binwidth = 1,
                size = 1)+
  facet_wrap(~ njoueurs)+
  labs(
    x = "Nombre de coups",
    y = "Probabilité"
  )

ncoups %>%
  group_by(njoueurs, pile) %>%
  summarise(mean(ncoups))
