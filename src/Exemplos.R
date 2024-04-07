

library( tidyverse )

library( spatstat )
library( plotrix )

set.seed(119)

# Univariate Plot

rhosGen <- function(lambda, maxTime, inomo  ) {
  rhos <- NULL
  i <- 1
  while (sum(rhos) < maxTime) {
    samp <- rexp(n = 1, rate = lambda/if_else( inomo,  i /50, 1 ) )
    rhos[i] <- samp
    i <- i + 1
  }
  return(head(rhos, -1))  # Remove the last sample
}


taos <- function(lambda, max, inomo = FALSE ) {
  rho_vec <- rhosGen(lambda, max, inomo)
  vec <- vector()
  vec[1] <- 0
  sum <- 0
  for (i in 2:max) {
    sum <- sum + rho_vec[i]
    vec[i] <- sum
  }
  return(vec)
}

par(oma=c(0,0,0,0))
par(mar=c(2,2,2,2))

## Relizações Homogenea e Não Homogeneas

realizations <-
  taos( lambda = 5, max = 100 )
realizations_inomo <-
  taos( lambda = 5, max = 100, inomo = TRUE )

## Intervalos

intervals <- seq( from = min( realizations ), to = abs( max( realizations) +1 ), by = 1 )


# points( x = intervals,y = rep( 1, length(intervals)), pch = "I", col = "red")

interval_realizations <-
  data.frame( grupo = cut( realizations, breaks = seq( from = 0, to = 60, by = 1 ) ) ) %>%
  group_by( grupo ) %>%
  summarise( n = n() )

hist( interval_realizations$n )

summary( interval_realizations$n )

# Inomogenous Poisson Process

par( mfrow = c(1,2))

plot( realizations ,rep(1, 100), main = expression( lambda * " = 5"),
      ylim = c(0.99,1.01), xlab = "Time", ylab = "", pch = 3,
      yaxt = "n" )
lines( x = realizations, y = rep(1,length(realizations))  )

plot( realizations_inomo ,rep(1, 100), main = expression( lambda*"(t) = 5"*t/50 ),
      ylim = c(0.99,1.01), xlab = "Time", ylab = "", pch = 3,
      yaxt = "n" )
lines( x = realizations_inomo, y = rep(1,length(realizations_inomo))  )





plot( realizations, 1:length(realizations), type = "s" )
lines( realizations_inomo, 1:length(realizations_inomo), col = "red", lwd = 1.5, type = "s"  )
lines( 1:20, 5*(1:20), lty = 2, col = "grey" )
legend( 1,100, legend =c( "Homogenea", "Não Homogenea" ),
         col = c("black", "red"), lty = c(1,1) )
#plot( spatstat.random::runifpoint( 100 ))


# Slide - Intensidade Homogenea



sim_poss_1 <- spatstat.random::runifpoispp( lambda = 5, win = owin(c(0,10),c(0,10)) )

#plot(density(sim_poss_1, sigma = 100 ))

pos_x <- list( "0-2" = 0.75, "2-4" = 2.75, "4-6" = 4.75, "6-8" = 6.75, "8-10" = 8.75 )

pos_y <- list( "0-2" = 1.75, "2-4" = 3.75, "4-6" = 5.75, "6-8" = 7.75, "8-10" = 9.75 )

avg_cases <-
  data.frame( x = sim_poss_1$x , y = sim_poss_1$y ) %>%
    mutate( int_x = 
              case_when( x >= 0 & x < 2 ~ "0-2", 
                         x >= 2 & x < 4 ~ "2-4",
                         x >= 4 & x < 6 ~ "4-6",
                         x >= 6 & x < 8 ~ "6-8",
                         x >= 8 & x <= 10 ~ "8-10" ),
            int_y = 
              case_when( y >= 0 & y < 2 ~ "0-2", 
                         y >= 2 & y < 4 ~ "2-4",
                         y >= 4 & y < 6 ~ "4-6",
                         y >= 6 & y < 8 ~ "6-8",
                         y >= 8 & y <= 10 ~ "8-10") ) %>%
    group_by( int_x, int_y )%>%
    summarise( n = n() ) %>%
    mutate( pos_x = map_dbl( int_x, ~{ pos_x[[.x]] }),
            pos_y = map_dbl( int_y, ~{ pos_y[[.x]] }) )


par( mfrow = c(1,3))

plot( sim_poss_1$x, sim_poss_1$y, xlab = "x", ylab = "y" )

plot( sim_poss_1$x, sim_poss_1$y, xlab = "x", ylab = "y" )
abline( v=c(2,4,6,8), h = c(2,4,6,8), col = "red" )

plot( sim_poss_1$x, sim_poss_1$y, xlab = "x", ylab = "y" )
abline( v=c(2,4,6,8), h = c(2,4,6,8), col = "red" )
for( i in 1:nrow(avg_cases) ){
  text( x = avg_cases[i,"pos_x"][[1]], 
        y = avg_cases[i,"pos_y"][[1]],
        paste0( avg_cases[i,"n"][[1]] ),
        col = "red", cex = 1.5 )
}

quadrat.test( sim_poss_1 )


# Slide - Não Homogeneo


fnintensity <- function(x, y){return( 7 * x + 1.1 * y)}

pinhom <- spatstat.random::rpoispp( lambda = fnintensity,
                   win = owin( c(0, 10), yrange = c(0, 10)))


par(oma=c(0,0,0,0))
par(mar=c(2,2,2,2))
par( mfrow = c(1,4) )
plot(pinhom, main = expression( lambda*"(x,y) = 7x + 1.1y " ) )
plot(density(pinhom, sigma = 0.1), main = expression( sigma * "=0.1" ) )
plot(density(pinhom, sigma = 0.5), main = expression( sigma * "=0.5" ) )
plot(density(pinhom, sigma = 1.0), main = expression( sigma * "=1" ) )
# Contage

# Slide - Intensidade Não Homogenea




# Slide - K function

sim_poiss<-spatstat.random::runifpoispp( 1 , win = owin( c(0,10), c(0,10 )))

plot( sim_poiss$x, sim_poiss$y, xlab = "", ylab = "",
      main = expression("processo de poisson com " * lambda * "=1, em área 10 x 10") )
points( c(5.97), c(5.60), pch = 19, lwd = 4 )
draw.circle( 5.97, 5.60, radius = 1, border = "blue", lty = 2, lwd = 1.7 )
draw.circle( 5.97, 5.60, radius = 2, border = "red", lty = 2, lwd = 1.7 )
draw.circle( 5.97, 5.60, radius = 3, border = "green", lty = 2, lwd = 1.7 )
legend( 0.5,9.5, legend =c( "Area 1", "Area 2", "Area 3"), lty = c(2,2,2), cex = 0.8,
        col = c("blue", "red", "green"), lwd = c(1.7,1.7,1.7) )


# Kest

plot( Kest(sim_poiss) )
plot( envelope( sim_poiss, Kest ))

# Slide Random Fields


sim_exp_lgcp    <- rLGCP( model = "exp", mu = 2, var=0.5,win = owin( c(0,10), c(0,10)) )
sim_gauss_lgcp  <- rLGCP( model = "gauss", mu = 2, var=0.5,win = owin( c(0,10), c(0,10)) )
sim_matern_lgcp <- rLGCP( model = "matern", nu = 2, var = 0.5,win = owin( c(0,10), c(0,10)))


par( mfrow = c(2,3))
plot( sim_exp_lgcp, main = expression( "Exponential "*mu*"=2,"*sigma^2*"=0.5" ) )
plot( sim_gauss_lgcp, main = expression( "Gaussian "*mu*"=2,"*sigma^2*"=0.5" ) )
plot( sim_matern_lgcp, main = expression( "Exponential "*nu*"=2,"*sigma^2*"=0.5" ) )
plot( density( sim_exp_lgcp, sigma = 1 ),    main =  expression(sigma*"=1") )
plot( density( sim_gauss_lgcp, sigma = 1 ),  main =  expression(sigma*"=1") )
plot( density( sim_matern_lgcp, sigma = 1 ), main =  expression(sigma*"=1") )
  


