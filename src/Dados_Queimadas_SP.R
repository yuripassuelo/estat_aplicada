

library( tidyverse )
library( spatstat )
library( sf )
library( tigris )

path <-
  "C:/Users/yurim/Desktop/Mestrado/1S2024/Estat Aplicada/Trabalho/data/"

base <- 
  readRDS( paste0( path, "/inter/inter_data.rds" ) )


base_sp <- 
  filter( base, year == 2020, Estado == "SÃO PAULO", RiscoFogo == 1 )

map_sp <- 
  geobr::read_state( code_state = "SP")


# Conversão do Mapa em sf para janela do pacote spatstat

sp_mod <- st_union( map_sp )

sp_flat <- st_transform( sp_mod, crs = 3857 )

sp_owin <- as.owin( sp_flat )

# Conversão dos dados para padrao Spatstat

mod <- base_sp %>%
  mutate( x = Longitude, 
          y = Latitude )%>%
  select( x,y,everything() )


points_sp <- mod %>% 
  select( x,y ) %>%
  st_as_sf( coords = c("x","y"), crs = 4674 ) %>%
  #st_union() %>%
  st_transform( crs = 3857 )%>%
  st_coordinates() %>% 
  as.data.frame()

mod_sp <-
  mod %>%
  mutate( x = points_sp$X,
          y = points_sp$Y)

# Criação  de dados em formato ppp

sp_ppp_dta <-
  as.ppp( mod_sp %>% select( x, y ), W = sp_owin  ) %>%
  unique.ppp( ) #biomes_owin )

sp_ppp_dta_extra <-
  as.ppp( mod_sp , W = sp_owin  ) %>%
  unique.ppp( )


# Plot Dados

par(oma=c(0,0,0,0))
par(mar=c(2,2,2,2))
par( mfrow = c(1,2) )

plot( sp_ppp_dta, main = "Ocorrências de Incendio - SP" )

plot( density( sp_ppp_dta ), main = "Intensidade" )

# Plot Funcao K

par(oma=c(0,0,0,0))
par(mar=c(2,2,2,2))

plot( Kest( sp_ppp_dta ) )

# Modelo

model_sp <- kppm( sp_ppp_dta, trend=~1, 
                  "LGCP", model = "exp" )




