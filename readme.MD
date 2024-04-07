
# Processos Espaciais de Poisson e Cox

Diretório que contém códigos e apresentação com objetivos de introduzir os conceitos principais relacionados aos processos de poisson e de cox, para além disso aplicações no R.

O diretório `src` contém códigos de exemplo e análises além disso um código para processamento de dados.

- Dados_Queimadas_Brasil.R : Processamento dos dados relacionados
- Dados_Queimadas_SP.R : Utilização dos dados a nível Brasil com restrição ao estado de SP
- Exemplos.R : Exemplos de códigos
- Simulações_spatstat.R : Simulações do pacote spatstat

o diretório `data` contém duas pastas 

- `raw` Dados baixados de forma manual do distema de monitoramento de queimadas do INPE
- `inter` Dados processados de queimadas do Brasil que são utilizados para contextualizar aplicações *spatstat* com dados reais

O diretório `rmds` contém uma breve apresentação com conceitos iniciais e exemplos, e um html que contém os exemplos códigos.

# Observações




Para caso queira se replicar os scripts acima algumas informações sobre pacotes e versionamentos:

- `tidyverse`
- `spatstats`
- `sf`
- `RandomFields`*
- `plotrix`

* Pacote `RandomFields` estava fora do CRAN até a data de 07/04/2024 sendo assim necessário a instalação de uma de suas versões no Archive do CRAN, para esse exercício usamos em especifico a versão 3.3.14 disponível em [https://cran.r-project.org/src/contrib/Archive/RandomFields/](https://cran.r-project.org/src/contrib/Archive/RandomFields/) 

Informações Extras :

```{r}
R version 4.1.1 (2021-08-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22631)

Matrix products: default

locale:
[1] LC_COLLATE=Portuguese_Brazil.1252  LC_CTYPE=Portuguese_Brazil.1252    LC_MONETARY=Portuguese_Brazil.1252
[4] LC_NUMERIC=C                       LC_TIME=Portuguese_Brazil.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] plotrix_3.8-4           RandomFields_3.3.14     RandomFieldsUtils_1.2.5 sp_1.6-0               
 [5] spatstat_3.0-3          spatstat.linnet_3.1-0   spatstat.model_3.2-1    rpart_4.1-15           
 [9] spatstat.explore_3.1-0  nlme_3.1-152            spatstat.random_3.1-4   spatstat.geom_3.1-0    
[13] spatstat.data_3.0-4     forcats_0.5.1           stringr_1.5.0           dplyr_1.1.2            
[17] purrr_1.0.1             readr_2.1.4             tidyr_1.3.0             tibble_3.2.1           
[21] ggplot2_3.4.4           tidyverse_1.3.1         sf_1.0-9               

loaded via a namespace (and not attached):
 [1] fs_1.5.2              spatstat.sparse_3.0-1 lubridate_1.8.0       httr_1.4.2            tools_4.1.1          
 [6] backports_1.4.1       utf8_1.2.2            R6_2.5.1              KernSmooth_2.23-20    DBI_1.1.2            
[11] mgcv_1.8-36           colorspace_2.0-2      withr_2.5.1           tidyselect_1.2.0      curl_4.3.2           
[16] compiler_4.1.1        cli_3.6.1             rvest_1.0.2           xml2_1.3.3            scales_1.2.1         
[21] classInt_0.4-8        proxy_0.4-27          goftest_1.2-3         digest_0.6.31         spatstat.utils_3.0-2 
[26] rmarkdown_2.11        pkgconfig_2.0.3       htmltools_0.5.5       dbplyr_2.1.1          fastmap_1.1.1        
[31] rlang_1.1.0           readxl_1.3.1          rstudioapi_0.15.0     generics_0.1.3        jsonlite_1.7.3       
[36] magrittr_2.0.1        Matrix_1.5-4          Rcpp_1.0.10           munsell_0.5.0         fansi_1.0.2          
[41] abind_1.4-5           lifecycle_1.0.3       stringi_1.7.6         yaml_2.2.2            grid_4.1.1           
[46] parallel_4.1.1        crayon_1.4.2          deldir_1.0-6          lattice_0.20-44       haven_2.4.3          
[51] splines_4.1.1         tensor_1.5            hms_1.1.1             knitr_1.37            pillar_1.9.0         
[56] reprex_2.0.1          glue_1.6.2            evaluate_0.22         data.table_1.14.2     modelr_0.1.8         
[61] vctrs_0.6.1           tzdb_0.3.0            cellranger_1.1.0      gtable_0.3.0          polyclip_1.10-4      
[66] assertthat_0.2.1      xfun_0.29             broom_1.0.5           e1071_1.7-12          class_7.3-19         
[71] units_0.8-1           geobr_1.7.0           ellipsis_0.3.2   
```







