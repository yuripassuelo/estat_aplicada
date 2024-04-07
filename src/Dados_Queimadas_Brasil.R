
library( tidyverse )
library( data.table )
library( geobr )

library( lubridate )

library(raster)

library( spatstat )

path <-
  "./data/"

files <- 
  list.files( paste0( path, "raw/" ) )

read_raw_files <- function( .path ){
  
  folders_n_files <- list.files( .path )
  
  folders <- folders_n_files[ !grepl( ".zip", folders_n_files ) ]
  
  # Leitura dos arquivos csv's :
  
  print(  paste0( .path, folders, "/" ) )
  
  bind_rows(
    map( list.files( paste0( .path, folders, "/" ), full.names = TRUE ),
         function( .file ){
           fread( .file, encoding = "UTF-8" )
       } ) )

}

data2 <-
  read_raw_files( paste0( path, "raw/" )) %>%
  mutate( date_str = str_sub( DataHora, start = 1, end = 10 ),
          date     = as.Date( date_str, format = "%Y/%m/%d"),
          
          month    = lubridate::month( date ),
          year     = lubridate::year( date ))

# Save Compacted data

saveRDS( data2, paste0( path, "/inter/inter_data.rds" ) )

# Leitura de Mapas do GEOBR

br_map <-
  geobr::read_state( year = 2010 )

biomes <- 
  geobr::read_biomes( year = 2019 )

data2_cerrado <-
  filter( data2, Bioma == "Cerrado" )#, RiscoFogo == 1 )

ggplot( )+
  geom_sf( data = br_map )+
  geom_sf( data = filter( biomes, name_biome == "Cerrado"), fill = "green", alpha = 0.3 )+
  geom_point( data = filter( data2_cerrado, year == 2020), 
              aes( x = Longitude, y = Latitude), size = 0.1 )


# Densidade 2d usando bind2d

ggplot( )+
  geom_sf( data = br_map )+
  geom_sf( data = filter( biomes, name_biome == "Cerrado"), fill = "green", alpha = 0.3 )+
  geom_point( data = filter( data2_cerrado, year == 2020), 
              aes( x = Longitude, y = Latitude), size = 0.1 )+
  geom_bin2d( data = filter( data2_cerrado, year == 2020), 
              aes( x = Longitude, y = Latitude), bins = 70 )+
  scale_fill_continuous( type = "viridis" )

# Densidade 2d usando geom_density_2d

ggplot( )+
  geom_sf( data = br_map )+
  geom_sf( data = filter( biomes, name_biome == "Cerrado"), fill = "green", alpha = 0.3 )+
  geom_point( data = filter( data2_cerrado, year == 2020), 
              aes( x = Longitude, y = Latitude), size = 0.1 )+
  stat_density_2d(data = filter( data2_cerrado, year == 2020),
                  aes( x = Longitude, y = Latitude, fill = after_stat(level) ), geom = "polygon")+
  scale_fill_continuous( type = "viridis" )
  

data_agg <-
  data2_cerrado %>%
  mutate( date_year_month = as.Date( paste( year, month, "01", sep = "-") ) )%>%
  group_by( date ) %>%
  summarise( n = n() )

data_risco1 <-
  data2_cerrado %>%
  filter( RiscoFogo == 1 )%>%
  mutate( date_year_month = as.Date( paste( year, month, "01", sep = "-") ) )%>%
  group_by( date ) %>%
  summarise( n = n() )

ggplot()+
  geom_line( data = data_agg, aes( x = date, y = n ))+
  geom_line( data = data_risco1, aes( x = date, y = n ), col = "red")  

data2_cerrado %>%
  mutate( d_01 = if_else( RiscoFogo == 1, "1", "0" ))%>%
  group_by( date, d_01 )%>%
  summarise( n = n() )%>%
  ungroup()%>%
  group_by( date )%>%
  mutate( prop = n/sum(n))%>%
  ggplot()+
  geom_line( aes( x = date, y = prop, color = d_01 ))

#ggplot( data = br_map )+
#  geom_sf()+
#  geom_point( data = filter( data2, RiscoFogo == 1 ), 
#              aes( x = Longitude, y = Latitude ), size = 0.1 )

#data_incend <-
#  filter( data2, RiscoFogo == 1 ) %>%
#  mutate( DiaSemChuva = if_else( DiaSemChuva == -999, NA_real_, DiaSemChuva ) )

week_dta <-
  data_agg %>%
  mutate( week = lubridate::week( date ),
          year = lubridate::year( date ),
          year_week = paste0( week,"-",year )) %>%
  group_by( year, week, year_week )%>%
  summarise( n = sum( n )) %>%
  ungroup()%>%
  mutate( i = row_number() )

month_dta <-
  data_agg %>%
  mutate( month = lubridate::month( date ),
          year  = lubridate::year( date ),
          month_date = as.Date(paste0( year,"-",month,"-01" )) ) %>%
  group_by( year, month, month_date )%>%
  summarise( n = sum( n )) %>%
  ungroup()%>%
  mutate( i = row_number() )

# Serie Temporal Mensal

ts_data <-
  ts( month_dta$n,
    frequency = 12, 
    start =  c(2014, 01))

# AR(1) -

model_ar1 <- arima( ts_data, order = c( 2, 1, 0 ) )

plot( ts_data, type = "l" )
lines( ts_data - model_ar1$residuals, col = "red" )



#ts_data_day <-
#  ts( data_agg$n,
#      frequency = 365.25, 
#      start = as.Date( "2014-01-01" ) )

plot( ts_data )

library( auto.arima )

ts_decomp <- 
  decompose( ts_data )

install.packages( "auto.arima")

library( auto.arima )

plot( ts_decomp_mult )

lubridate::week( data_agg$date )

wday( data_agg$date )

plot( ts_data )

ts_data[[1]]

View( ts_data )

plot( week_dta$n, type = "l" )

library( zoo )

