
library(spatstat)

clm_fires <- spatstat.data::clmfires
clm_extra <- spatstat.data::clmfires.extra

nb_fires <- spatstat.data::nbfires
nb_extra <- View(spatstat.data::nbfires.extra)

plot(clm_extra)

lambdahat <- density(clm_fires)

plot(lambdahat, main = "Default bandwidth")
plot(density(clm_fires, sigma = 0.05))
plot(density(clm_fires, sigma = 0.1))
plot(density(clm_fires, sigma = 1))
plot(density(clm_fires, sigma = 5))
plot(density(clm_fires, sigma = 10))
plot(density(clm_fires, sigma = 25))


plot( clm_extra$clmcov100$elevation )
lines( clm_fires$window )


plot(Kest(clm_fires))
plot(Kest(nb_fires))

plot(envelope(clm_fires,Kest))
plot(envelope(nbfires,Kest))


bei_data  <- spatstat.data::bei

bei_extra <- spatstat.data::bei.extra


par( mfrow = c(2,2))

plot( density( bei, sigma = 1 ))
plot( density( bei, sigma = 5 ))
plot( density( bei, sigma = 10 ))
plot( density( bei, sigma = 25 ))


par( mfrow = c(1,2))
plot( density( bei, sigma = 25 ))
plot( bei_extra$grad )


par( mfrow = c(1,2))

plot( Kest( bei ), main = "K - Function - Bei")

plot( envelope( bei, Kest ), main = "K - Function - Intervalos")


model  <- kppm(bei ~ elev + grad, "LGCP", model="exp", data=bei.extra)

model_2 <- kppm( clmfires ~ 1, "LGCP", model="exp", data=clmfires )

plot( bei.extra )

simulacoes <-
  simulate( model, nsim = 4 )

simulacoes_2 <-
  simulate( mode)

par( mfrow = c(2,2))
for( i in 1:4 ){
  plot( density( simulacoes[[i]], sigma = 25 ),
        main = paste0("Simulação ",i))
}

# Real x efective

plot( density( bei, 20 ))
plot( predict( model ))






