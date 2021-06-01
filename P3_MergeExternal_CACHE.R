# USED files:
#            full_geomerged_df_3.csv (from part 2: from derived)
#            data (3).xlsx (from Atlas.bi)
#            atlas_data (udh level input + full municip)
#            udh_queried_data
#            dffort.xlsx
# 
# WRITE files:
#            udh_queried_data
#            full_geomerged_df_4.csv (for part 5)

library(readxl)
library(tidyr)
library(dplyr)
library(corrplot)
library(FNN)
library(tmaptools)
library(geosphere)
library(sp)
library(ggplot2)
library(stringi)

# ---------- ---------- #
# 1. Defining Functions # -----------------------------------------------------
# --------------------- #


# Purpose: put data-level indicator for column name (state, municip, udh) 
# Function is called from the column_fixer function
locality_names <- function(input_data, data_level) {
  
  if (data_level == "state") {
    level_indicator <- "st."
  } else if (data_level == "municip") {
    level_indicator <- "mc."
  } else {
    level_indicator <- "udh."
  }
  
  old_colnames <- colnames(input_data)
  new_colnames <- c()
  countertje <- 1
  
  for (i in old_colnames) {
    new_colnames[countertje] <- paste(level_indicator, i, sep = "")
    countertje <- countertje + 1
  }
  
  input_data <- setNames(input_data, new_colnames)
  
  return(input_data)
}


# Purpose: cleans atlas Brasil external data
# works for all data levels
column_fixer <- function(new_data, data_level){
  if (grepl("Brasil", head(new_data[, "Territorialidades"], n = 1)) &
      grepl("Fontes:", tail(new_data[, "Territorialidades"], n = 1))) {
    print("Ready for action.")
    
    new_data <- new_data %>%
      mutate(Territorialidades = as.character(Territorialidades),
             Territorialidades = stri_trans_general(Territorialidades, "Latin-ASCII"),
             Territorialidades = tolower(Territorialidades)
      ) %>%
      filter(row_number() <= n()-3
      ) %>%
      filter(!row_number() == 1
      ) %>%
      drop_na(Territorialidades
      ) %>%
      mutate(
        urbanity = `População urbana 2010` / `População total 2010`,
        rurality = `População rural 2010` / `População total 2010`,
        rurality = ifelse(is.na(`População rural 2010`) == TRUE, 0, rurality)
      ) %>% 
      mutate(cumul_age_24 = 
               `População masculina de 0 a 4 anos de idade 2010` +
               `População masculina de 5 a 9 anos de idade 2010` +
               `População masculina de 10 a 14 anos de idade 2010` + 
               `População masculina de 15 a 19 anos de idade 2010` + 
               `População masculina de 20 a 24 anos de idade 2010`
      ) %>%
      select(
        - `População masculina de 0 a 4 anos de idade 2010`,
        - `População masculina de 5 a 9 anos de idade 2010`,
        - `População masculina de 10 a 14 anos de idade 2010`,
        - `População masculina de 15 a 19 anos de idade 2010`,
        - `População masculina de 20 a 24 anos de idade 2010`,
        - `População masculina de 25 a 29 anos de idade 2010`,
        - `População masculina de 30 a 34 anos de idade 2010`
      ) %>%
      mutate(young_ratio = cumul_age_24 / `População total 2010`)
    
    new_data <- locality_names(new_data, data_level)
    
    return(new_data)
    
  } else {
    print("Not ready for action")
    return("Something is wrong")
    
  }
}


# Purpose: Two functions for one purpose
# (1) get_cors: Called from "coordinate_retriever" (below), uses names to query 
# coordinate data and returns this
# (2) coordinate_retriever: Called from the main script. Cleans inputs before 
# query and calls query function
get_cors <- function(datasetje){
  counter <- 1 
  subbie_df <- datasetje
  for (i in subbie_df$udh.Territorialidades){
    tryCatch({
      retrieved_cors <- geocode_OSM(i)
      subbie_df$udh.long[counter] <- retrieved_cors[["coords"]][["x"]]
      subbie_df$udh.lat[counter] <- retrieved_cors[["coords"]][["y"]]
    },
    error = function(e){
      subbie_df$udh.long[counter] <- 0
      subbie_df$udh.lat[counter] <- 0
    })
    print(i)
    counter <- counter + 1
  }
  subbie_df <- subbie_df %>%
    mutate(udh.lat = ifelse(udh.lat == 0, NA, udh.lat),
           udh.long = ifelse(udh.long == 0, NA, udh.long))
  return(subbie_df)
}
coordinate_retriever <- function(input_udh_data, city_name) {
  # Clean text to avoid trouble in OSM queries
  input_udh_data <- input_udh_data %>%
    mutate(
      udh.Territorialidades = gsub(":.*", "", udh.Territorialidades),
      udh.Territorialidades = ifelse(
        grepl("/", udh.Territorialidades), 
        gsub("/.*", "", udh.Territorialidades), 
        udh.Territorialidades),
      udh.Territorialidades = gsub("\\s*\\([^\\)]+\\)","", udh.Territorialidades),
      udh.Territorialidades = paste(udh.Territorialidades, city_name, sep = " "),
      udh.Territorialidades = gsub("\\s+", " ", udh.Territorialidades) # whitespaces
    )
  # drop non uniques 
  input_udh_data <- input_udh_data[!duplicated(input_udh_data$udh.Territorialidades), ]
  # add longitude and latitude columns (empty)
  input_udh_data$udh.lat <- 0
  input_udh_data$udh.long <- 0
  # Call the query function
  input_udh_data_retrieved_cors <- get_cors(input_udh_data)
  # Return dataset
  return(input_udh_data_retrieved_cors)
}


# Purpose: combines udh-level dataset with data from internal
# Uses euclidean distance to find nearest coordinate neighbors of both sets.
udh_merge_ex_with_in <- function(internal_data, external_data, metro_munics) {
  
  external_data <- external_data[
    !is.na(external_data$udh.lat) | !is.na(external_data$udh.long),]
  
  internal_data <- internal_data[
    internal_data$customer_city %in% metro_munics & !is.na(internal_data$centroid_lat),]
  
  nn2 <- get.knnx(
    external_data[,c("udh.lat", "udh.long")], 
    internal_data[,c("centroid_lat", "centroid_long")], 
    2)
  
  internal_data$local_index <- c(1:nrow(internal_data)) 
  external_data$local_index <- c(1:nrow(external_data))
  
  internal_data$index_other_data <- nn2$nn.index[,1]
  internal_data$dist <- nn2$nn.dist[,1]
  
  married <- merge(internal_data,
                   external_data,
                   by.x = "index_other_data",
                   by.y = "local_index")
  return(married)
}


# Purpose: Haversine variant of udh_merge_ex_within
haversine_function <- function(brazil_df, udh_df, metros_list){
  # udh clean
  udh_df <- udh_df[!is.na(udh_df$udh.lat),]
  # Select right municipal subset
  internal_data <- brazil_df[
    brazil_df$customer_city %in% metros_list & !is.na(brazil_df$centroid_lat),]
  # identifier of coordinate combinations
  internal_data$coordinate_id <- paste(as.character(internal_data$centroid_long), 
                                       as.character(internal_data$centroid_lat), sep = " ")
  # Select only relevant columns 
  coordinates_uniques <- internal_data %>% 
    select(centroid_lat, 
           centroid_long, 
           coordinate_id)
  # Get rid of duplicates to speed up the whole process
  coordinates_uniques <- coordinates_uniques[!duplicated(coordinates_uniques), ]
  coordinates_uniques$nn_index <- 0
  coordinates_uniques$nn_dist <- 0
  # wrap the whol thing
  for (i in 1:nrow(coordinates_uniques)) {
    emptyvec <- rep(0, nrow(udh_df))
    for (b in 1:nrow(udh_df)){
      emptyvec[b] <- distHaversine(
        coordinates_uniques[i,c("centroid_long", "centroid_lat")],
        udh_df[b,c("udh.long", "udh.lat")], )
    }
    # index of minimal distance 
    coordinates_uniques[i,]$nn_index <- which.min(emptyvec)
    # the minimal distance itself
    coordinates_uniques[i,]$nn_dist <- min(emptyvec)
  }
  # Get reid of centroid_lat, -long, to avoid duiplicates in merge back 
  coordinates_uniques <- coordinates_uniques %>%
    select(-centroid_lat,
           -centroid_long)
  # Merge duplicate coordiantes back to data
  merged_back <- merge(internal_data,
                       coordinates_uniques,
                       by.x = "coordinate_id",
                       by.y = "coordinate_id",
                       all.x = TRUE)
  # Merge census data based on inices
  merged_2 <- merge(merged_back, 
                    udh_df,
                    by.x = "nn_index",
                    by.y = "X",
                    all.x = TRUE)
  # Coordinate ID no longer needed
  merged_2 <- merged_2 %>%
    select(-coordinate_id)
  
  return(merged_2)
}
# -------------------------------------- #
# 2.Define municipalities per metro area # -------------------------------------
# -------------------------------------- #

# Region: North
# State: Para (pa)
belem_metro_municips <- c(
  "ANANINDEUA",
  "BELÉM",
  "BENEVIDES",
  "CASTANHAL",
  "MARITUBA",
  "SANTA BÁRBARA DO PARÁ",
  "SANTA IZABEL DO PARÁ")
belem_metro_municips <- stri_trans_general(belem_metro_municips, "Latin-ASCII")
belem_metro_municips <- tolower(belem_metro_municips)
belem_metro_municips <- paste(belem_metro_municips, "(pa)")

# Region: North 
# State: Amazonas (am)
manaus_metro_municips <- c(
  "AUTAZES",
  "CAREIRO",
  "CAREIRO DA VÁRZEA",
  "IRANDUBA",
  "ITACOATIARA",
  "ITAPIRANGA",
  "MANACAPURU",
  "MANAQUIRI",
  "MANAUS",
  "NOVO AIRÃO",
  "PRESIDENTE FIGUEIREDO",
  "RIO PRETO DA EVA",
  "SILVES")
manaus_metro_municips <- stri_trans_general(manaus_metro_municips, "Latin-ASCII")
manaus_metro_municips <- tolower(manaus_metro_municips)
manaus_metro_municips <- paste(manaus_metro_municips, "(am)")

# Region: Northeast
# State: Ceara (ce) 
fortaleza_metro_municips <- c(
  "Aquiraz", 
  "Cascavel",
  "Caucaia",
  "Chorozinho",
  "Eusébio",
  "Fortaleza",
  "Guaiuba", 
  "Horizonte",
  "Itaitinga",
  "Maracanaú",
  "Maranguape", 
  "Pacajus",
  "Pacatuba",
  "Paracuru",
  "Paraipaba",
  "Pindoretama",
  "São Gonçalo do Amarante",
  "São Luís do Curu e Trairi")
fortaleza_metro_municips <- stri_trans_general(fortaleza_metro_municips, "Latin-ASCII")
fortaleza_metro_municips <- tolower(fortaleza_metro_municips)
fortaleza_metro_municips <- paste(fortaleza_metro_municips, "(ce)")

# Region: Northeast
# State: Pernambuco (pe)
recife_metro_municips <- c(
  "ABREU E LIMA",
  "ARAÇOIABA",
  "CABO DE SANTO AGOSTINHO",
  "CAMARAGIBE",
  "GOIANA",
  "IGARASSU",
  "ILHA DE ITAMARACÁ",
  "IPOJUCA",
  "ITAPISSUMA",
  "JABOATÃO DOS GUARARAPES",
  "MORENO",
  "OLINDA",
  "PAULISTA",
  "RECIFE",
  "SÃO LOURENÇO DA MATA")
recife_metro_municips <- stri_trans_general(recife_metro_municips, "Latin-ASCII")
recife_metro_municips <- tolower(recife_metro_municips)
recife_metro_municips <- paste(recife_metro_municips, "(pe)")

# Region: Northeast
# State: Bahia (ba)
salvador_metro_municips <- c(
  "CAMAÇARI",
  "CANDEIAS",
  "DIAS D’ÁVILA",
  "ITAPARICA",
  "LAURO DE FREITAS",
  "MADRE DE DEUS",
  "MATA DE SÃO JOÃO",
  "POJUCA",
  "SALVADOR",
  "SÃO FRANCISCO DO CONDE",
  "SÃO SEBASTIÃO DO PASSÉ",
  "SIMÕES FILHO",
  "VERA CRUZ")
salvador_metro_municips <- stri_trans_general(salvador_metro_municips, "Latin-ASCII")
salvador_metro_municips <- tolower(salvador_metro_municips)
salvador_metro_municips <- paste(salvador_metro_municips, "(ba)")

# Region: Northeast
# State: Rio Grande do Norte (rn)
natal_metro_municips <- c(
  "ARÊS",
  "CEARÁ-MIRIM",
  "EXTREMOZ",
  "GOIANINHA",
  "IELMO MARINHO",
  "MACAÍBA",
  "MAXARANGUAPE",
  "MONTE ALEGRE",
  "NATAL",
  "NÍSIA FLORESTA",
  "PARNAMIRIM",
  "SÃO GONÇALO DO AMARANTE",
  "SÃO JOSÉ DE MIPIBU",
  "VERA CRUZ")
natal_metro_municips <- stri_trans_general(natal_metro_municips, "Latin-ASCII")
natal_metro_municips <- tolower(natal_metro_municips)
natal_metro_municips <- paste(natal_metro_municips, "(rn)")


# Region: Northeast
# State: Alagoas (al)
maceio_metro_municips <- c(
  "ATALAIA",
  "BARRA DE SANTO ANTÔNIO",
  "BARRA DE SÃO MIGUEL",
  "COQUEIRO SECO",
  "MACEIÓ",
  "MARECHAL DEODORO",
  "MESSIAS",
  "MURICI",
  "PARIPUEIRA",
  "PILAR",
  "RIO LARGO",
  "SANTA LUZIA DO NORTE",
  "SATUBA")
maceio_metro_municips <- stri_trans_general(maceio_metro_municips, "Latin-ASCII")
maceio_metro_municips <- tolower(maceio_metro_municips)
maceio_metro_municips <- paste(maceio_metro_municips, "(al)")

# Region: Northeast
# State: Maranhao (ma)
sao_luis_metro_municips <- c(
  "ALCÂNTARA",
  "BACABEIRA",
  "ICATU",
  "PAÇO DO LUMIAR",
  "RAPOSA",
  "ROSÁRIO",
  "SANTA RITA",
  "SÃO JOSÉ DE RIBAMAR",
  "SÃO LUÍS",
  "AXIXÁ",
  "CACHOEIRA GRANDE",
  "MORROS",
  "PRESIDENTE JUSCELINO")
sao_luis_metro_municips <- stri_trans_general(sao_luis_metro_municips, "Latin-ASCII")
sao_luis_metro_municips <- tolower(sao_luis_metro_municips)
sao_luis_metro_municips <- paste(sao_luis_metro_municips, "(ma)")

# Region: Northeast
# State: Piaui (pi) e Maranhao (ma) 
teresina_metro_municips <- c(
  "altos (pi)",
  "Beneditinos (pi)",
  "Coivaras (pi)",
  "Curralinhos (pi)",
  "Demerval Lobão (pi)",
  "José de Freitas (pi)",
  "Lagoa Alegre (pi)",
  "Lagoa do Piauí (pi)",
  "Miguel Leão (pi)",
  "Monsenhor Gil (pi)",
  "Nazária (pi)",
  "Pau D'Arco do Piauí (pi)",
  "Teresina (pi)",
  "União (pi)",
  "Timon")
teresina_metro_municips <- stri_trans_general(teresina_metro_municips, "Latin-ASCII")
teresina_metro_municips <- tolower(teresina_metro_municips)
# No additional one due to inter-state 

# Region: Northeast
# State: Bahia (ba) e Pernambuco (pe)
petrolina_juazeiro_metro_municips <- c(
  "CASA NOVA (ba)",
  "CURAÇÁ (ba)",
  "JUAZEIRO (ba)",
  "SOBRADINHO (ba)",
  "LAGOA GRANDE (pe)",
  "OROCÓ (pe)",
  "PETROLINA (pe)",
  "SANTA MARIA DA BOA VISTA (pe)")
petrolina_juazeiro_metro_municips <- stri_trans_general(petrolina_juazeiro_metro_municips, "Latin-ASCII")
petrolina_juazeiro_metro_municips <- tolower(petrolina_juazeiro_metro_municips)

# Region: Central-west
# State: Goias (go)
goiania_metro_municips <- c(
  "ABADIA DE GOIÁS",
  "APARECIDA DE GOIÂNIA",
  "ARAGOIÂNIA",
  "BELA VISTA DE GOIÁS",
  "BONFINÓPOLIS",
  "BRAZABRANTES",
  "CALDAZINHA",
  "CATURAÍ",
  "GOIANÁPOLIS",
  "GOIANIRA",
  "GOIÂNIA",
  "GUAPÓ",
  "HIDROLÂNDIA",
  "INHUMAS",
  "NERÓPOLIS",
  "NOVA VENEZA",
  "SANTO ANTÔNIO DE GOIÁS",
  "SENADOR CANEDO",
  "TEREZÓPOLIS DE GOIÁS",
  "TRINDADE")
goiania_metro_municips <- stri_trans_general(goiania_metro_municips, "Latin-ASCII")
goiania_metro_municips <- tolower(goiania_metro_municips)
goiania_metro_municips <- paste(goiania_metro_municips, "(go)")

# Region: Central-west
# State: Distrito Federal (df)
brasilia_metro_municips <- c(
  "brasilia",
  "ceilandia",
  "guara",
  "santa maria",
  "sobradinho",
  "taguatinga")
brasilia_metro_municips <- stri_trans_general(brasilia_metro_municips, "Latin-ASCII")
brasilia_metro_municips <- tolower(brasilia_metro_municips)
brasilia_metro_municips <- paste(brasilia_metro_municips, "(df)")

# Region: Central-west 
# State: Mato Grosso (mt)
vale_do_rio_cuiaba_metro_municips <- c(
  "ACORIZAL",
  "CHAPADA DOS GUIMARÃES",
  "CUIABÁ",
  "NOSSA SENHORA DO LIVRAMENTO",
  "SANTO ANTÔNIO DE LEVERGER",
  "VÁRZEA GRANDE" # Last one from metro region 
)
vale_do_rio_cuiaba_metro_municips <- stri_trans_general(vale_do_rio_cuiaba_metro_municips, "Latin-ASCII")
vale_do_rio_cuiaba_metro_municips <- tolower(vale_do_rio_cuiaba_metro_municips)
vale_do_rio_cuiaba_metro_municips <- paste(vale_do_rio_cuiaba_metro_municips, "(mt)")

# Region: Southeast 
# State: Sao Paulo (sp)
sao_paulo_metro_municips <- c(
  "São Paulo",
  "Arujá",
  "Barueri",
  "Biritiba Mirim",
  "Caieiras",
  "Cajamar",
  "Carapicuíba",
  "Cotia",
  "Diadema",
  "Embu",
  "Embu-Guaçu",
  "Ferraz de Vasconcelos",
  "Francisco Morato",
  "Franco da Rocha",
  "Guararema",
  "Guarulhos",
  "Itapecerica da Serra",
  "Itapevi",
  "Itaquaquecetuba",
  "Jandira",
  "Juquitiba",
  "Mairiporã",
  "Mauá",
  "Mogi das Cruzes",
  "Osasco",
  "Pirapora do Bom Jesus",
  "Poá",
  "Ribeirão Pires",
  "Rio Grande da Serra",
  "Salesópolis",
  "Santa Isabel",
  "Santana do Parnaíba",
  "Santo André",
  "São Bernardo do Campo",
  "São Caetano do Sul",
  "São Lourenço da Serra Suzano",
  "Suzano",
  "Taboão da Serra",
  "Vargem Grande Paulista")
sao_paulo_metro_municips <- stri_trans_general(sao_paulo_metro_municips, "Latin-ASCII")
sao_paulo_metro_municips <- tolower(sao_paulo_metro_municips)
sao_paulo_metro_municips <- paste(sao_paulo_metro_municips, "(sp)")

# Region: Southeast
# State: Rio de Janeiro (rj)
rio_dj_metro_municips <- c(
  "CACHOEIRAS DE MACACU",
  "BELFORD ROXO",
  "DUQUE DE CAXIAS",
  "GUAPIMIRIM",
  "ITABORAÍ",
  "ITAGUAÍ",
  "JAPERI",
  "MAGÉ",
  "MARICÁ",
  "MESQUITA",
  "NILÓPOLIS",
  "NITERÓI",
  "NOVA IGUAÇU",
  "PARACAMBI",
  "QUEIMADOS",
  "RIO BONITO",
  "RIO DE JANEIRO",
  "SÃO GONÇALO",
  "SÃO JOÃO DE MERITI",
  "SEROPÉDICA",
  "TANGUÁ")
rio_dj_metro_municips <- stri_trans_general(rio_dj_metro_municips, "Latin-ASCII")
rio_dj_metro_municips <- tolower(rio_dj_metro_municips)
rio_dj_metro_municips <- paste(rio_dj_metro_municips, "(rj)")

# Region: Southeast
# State: Minas Gerais (mg)
belo_horizonte_metro_municips <- c(
  "BALDIM",
  "BELO HORIZONTE",
  "BETIM",
  "BRUMADINHO",
  "CAETÉ",
  "CAPIM BRANCO",
  "CONFINS",
  "CONTAGEM",
  "ESMERALDAS",
  "FLORESTAL",
  "IBIRITÉ",
  "IGARAPÉ",
  "ITAGUARA",
  "ITATIAIUÇU",
  "JABOTICATUBAS",
  "JUATUBA",
  "LAGOA SANTA",
  "MÁRIO CAMPOS",
  "MATEUS LEME",
  "MATOZINHOS",
  "NOVA LIMA",
  "NOVA UNIÃO",
  "PEDRO LEOPOLDO",
  "RAPOSOS",
  "RIBEIRÃO DAS NEVES",
  "RIO ACIMA",
  "RIO MANSO",
  "SABARÁ",
  "SANTA LUZIA",
  "SÃO JOAQUIM DE BICAS",
  "SÃO JOSÉ DA LAPA",
  "SARZEDO",
  "TAQUARAÇU DE MINAS",
  "VESPASIANO")
belo_horizonte_metro_municips <- stri_trans_general(belo_horizonte_metro_municips, "Latin-ASCII")
belo_horizonte_metro_municips <- tolower(belo_horizonte_metro_municips)
belo_horizonte_metro_municips <- paste(belo_horizonte_metro_municips, "(mg)")

# Region: Southeast
# State: Espirito Santo (es)
grande_vitoria_metro_municips <- c(
  "CARIACICA",
  "FUNDÃO",
  "GUARAPARI",
  "SERRA",
  "VIANA",
  "VILA VELHA",
  "VITÓRIA")
grande_vitoria_metro_municips <- stri_trans_general(grande_vitoria_metro_municips, "Latin-ASCII")
grande_vitoria_metro_municips <- tolower(grande_vitoria_metro_municips)
grande_vitoria_metro_municips <- paste(grande_vitoria_metro_municips, "(es)")

# Region: Southeast
# State: Sao Paulo (sp)
sorocaba_metro_municips <- c(
  "ALAMBARI",
  "ALUMÍNIO",
  "ARAÇARIGUAMA",
  "ARAÇOIABA DA SERRA",
  "BOITUVA",
  "CAPELA DO ALTO",
  "CERQUILHO",
  "CESÁRIO LANGE",
  "IBIÚNA",
  "IPERÓ",
  "ITAPETININGA",
  "ITU",
  "JUMIRIM",
  "MAIRINQUE",
  "PIEDADE",
  "PILAR DO SUL",
  "PORTO FELIZ",
  "SALTO",
  "SALTO DE PIRAPORA",
  "SÃO MIGUEL ARCANJO",
  "SÃO ROQUE",
  "SARAPUÍ",
  "SOROCABA",
  "TAPIRAÍ",
  "TATUÍ",
  "TIETÊ",
  "VOTORANTIM")
sorocaba_metro_municips <- stri_trans_general(sorocaba_metro_municips, "Latin-ASCII")
sorocaba_metro_municips <- tolower(sorocaba_metro_municips)
sorocaba_metro_municips <- paste(sorocaba_metro_municips, "(sp)")


# Region: Southeast
# State: Sao Paulo
baixada_santista_metro_municips <- c(
  "Bertioga",
  "Cubatão",
  "Guarujá",
  "Itanhaém",
  "Mongaguá",
  "Peruíbe",
  "Praia Grande",
  "Santos",
  "São Vicente")
baixada_santista_metro_municips <- stri_trans_general(baixada_santista_metro_municips, "Latin-ASCII")
baixada_santista_metro_municips <- tolower(baixada_santista_metro_municips)
baixada_santista_metro_municips <- paste(baixada_santista_metro_municips, "(sp)")

# Region: Southeast
# State: Sao Paulo
campinas_metro_municips <- c(
  "Americana",
  "Artur Nogueira",
  "Campinas",
  "Cosmópolis",
  "Engenheiro Coelho",
  "Holambra",
  "Hortolândia",
  "Indaiatuba",
  "Itatiba",
  "Jaguariúna",
  "Monte Mor",
  "Morungaba",
  "Nova Odessa",
  "Paulínia",
  "Pedreira",
  "Santa Bárbara d'Oeste",
  "Santo Antônio de Posse",
  "Sumaré",
  "Valinhos",
  "Vinhedo")
campinas_metro_municips <- stri_trans_general(campinas_metro_municips, "Latin-ASCII")
campinas_metro_municips <- tolower(campinas_metro_municips)
campinas_metro_municips <- paste(campinas_metro_municips, "(sp)")

# Region: Southeast
# State: Sao Paulo
litoral_norte_metro_municips <- c(
  "APARECIDA",
  "ARAPEÍ",
  "AREIAS",
  "BANANAL",
  "CAÇAPAVA",
  "CACHOEIRA PAULISTA",
  "CAMPOS DO JORDÃO",
  "CANAS",
  "CARAGUATATUBA",
  "CRUZEIRO",
  "CUNHA",
  "GUARATINGUETÁ",
  "IGARATÁ",
  "ILHABELA",
  "JACAREÍ",
  "JAMBEIRO",
  "LAGOINHA",
  "LAVRINHAS",
  "LORENA",
  "MONTEIRO LOBATO",
  "NATIVIDADE DA SERRA",
  "PARAIBUNA",
  "PINDAMONHANGABA",
  "PIQUETE",
  "POTIM",
  "QUELUZ",
  "REDENÇÃO DA SERRA",
  "ROSEIRA",
  "SANTA BRANCA",
  "SANTO ANTÔNIO DO PINHAL",
  "SÃO BENTO DO SAPUCAÍ",
  "SÃO JOSÉ DO BARREIRO",
  "SÃO JOSÉ DOS CAMPOS",
  "SÃO LUIZ DO PARAITINGA",
  "SÃO SEBASTIÃO",
  "SILVEIRAS",
  "TAUBATÉ",
  "TREMEMBÉ",
  "UBATUBA")
litoral_norte_metro_municips <- stri_trans_general(litoral_norte_metro_municips, "Latin-ASCII")
litoral_norte_metro_municips <- tolower(litoral_norte_metro_municips)
litoral_norte_metro_municips <- paste(litoral_norte_metro_municips, "(sp)")

# Region: South
# State: Rio Grande do Sul (rs)
porto_alegre_metro_municips <- c(
  "ALVORADA",
  "ARARICÁ",
  "ARROIO DOS RATOS",
  "CACHOEIRINHA",
  "CAMPO BOM",
  "CANOAS",
  "CAPELA DE SANTANA",
  "CHARQUEADAS",
  "DOIS IRMÃOS",
  "ELDORADO DO SUL",
  "ESTÂNCIA VELHA",
  "ESTEIO",
  "GLORINHA",
  "GRAVATAÍ",
  "GUAÍBA",
  "IGREJINHA",
  "IVOTI",
  "MONTENEGRO",
  "NOVA HARTZ",
  "NOVA SANTA RITA",
  "NOVO HAMBURGO",
  "PAROBÉ",
  "PORTÃO",
  "PORTO ALEGRE",
  "ROLANTE",
  "SANTO ANTÔNIO DA PATRULHA",
  "SÃO JERÔNIMO",
  "SÃO LEOPOLDO",
  "SÃO SEBASTIÃO DO CAÍ",
  "SAPIRANGA",
  "SAPUCAIA DO SUL",
  "TAQUARA",
  "TRIUNFO",
  "VIAMÃO")
porto_alegre_metro_municips <- stri_trans_general(porto_alegre_metro_municips, "Latin-ASCII")
porto_alegre_metro_municips <- tolower(porto_alegre_metro_municips)
porto_alegre_metro_municips <- paste(porto_alegre_metro_municips, "(rs)")

# Region: South
# State: Parana (pr)
curitiba_metro_municips <- c(
  "ADRIANÓPOLIS",
  "AGUDOS DO SUL",
  "ALMIRANTE TAMANDARÉ",
  "ARAUCÁRIA",
  "BALSA NOVA",
  "BOCAIÚVA DO SUL",
  "CAMPINA GRANDE DO SUL",
  "CAMPO DO TENENTE",
  "CAMPO LARGO",
  "CAMPO MAGRO",
  "CERRO AZUL",
  "COLOMBO",
  "CONTENDA",
  "CURITIBA",
  "DOUTOR ULYSSES",
  "FAZENDA RIO GRANDE",
  "ITAPERUÇU",
  "LAPA",
  "MANDIRITUBA",
  "PIÊN",
  "PINHAIS",
  "PIRAQUARA",
  "QUATRO BARRAS",
  "QUITANDINHA",
  "RIO BRANCO DO SUL",
  "RIO NEGRO",
  "SÃO JOSÉ DOS PINHAIS",
  "TIJUCAS DO SUL",
  "TUNAS DO PARANÁ")
curitiba_metro_municips <- stri_trans_general(curitiba_metro_municips, "Latin-ASCII")
curitiba_metro_municips <- tolower(curitiba_metro_municips)
curitiba_metro_municips <- paste(curitiba_metro_municips, "(pr)")

# Region: South
# State: Santa Catarina (sc)
florianopolis_metro_municips <- c(
  "ÁGUAS MORNAS",
  "ANTÔNIO CARLOS",
  "BIGUAÇU",
  "FLORIANÓPOLIS",
  "GOVERNADOR CELSO RAMOS",
  "PALHOÇA",
  "SANTO AMARO DA IMPERATRIZ",
  "SÃO JOSÉ",
  "SÃO PEDRO DE ALCÂNTARA")
florianopolis_metro_municips <- stri_trans_general(florianopolis_metro_municips, "Latin-ASCII")
florianopolis_metro_municips <- tolower(florianopolis_metro_municips)
florianopolis_metro_municips <- paste(florianopolis_metro_municips, "(sc)")


# all together
all_udh_municips <- baixada_santista_metro_municips
all_udh_municips <- append(all_udh_municips, belem_metro_municips)
all_udh_municips <- append(all_udh_municips, belo_horizonte_metro_municips)
all_udh_municips <- append(all_udh_municips, brasilia_metro_municips)
all_udh_municips <- append(all_udh_municips, campinas_metro_municips)
all_udh_municips <- append(all_udh_municips, curitiba_metro_municips)
all_udh_municips <- append(all_udh_municips, florianopolis_metro_municips)
all_udh_municips <- append(all_udh_municips, fortaleza_metro_municips)
all_udh_municips <- append(all_udh_municips, goiania_metro_municips)
all_udh_municips <- append(all_udh_municips, grande_vitoria_metro_municips)
all_udh_municips <- append(all_udh_municips, litoral_norte_metro_municips)
all_udh_municips <- append(all_udh_municips, maceio_metro_municips)
all_udh_municips <- append(all_udh_municips, manaus_metro_municips)
all_udh_municips <- append(all_udh_municips, natal_metro_municips)
all_udh_municips <- append(all_udh_municips, petrolina_juazeiro_metro_municips)
all_udh_municips <- append(all_udh_municips, porto_alegre_metro_municips)
all_udh_municips <- append(all_udh_municips, recife_metro_municips)
all_udh_municips <- append(all_udh_municips, rio_dj_metro_municips)
all_udh_municips <- append(all_udh_municips, salvador_metro_municips)
all_udh_municips <- append(all_udh_municips, sao_luis_metro_municips)
all_udh_municips <- append(all_udh_municips, sao_paulo_metro_municips)
all_udh_municips <- append(all_udh_municips, sorocaba_metro_municips)
all_udh_municips <- append(all_udh_municips, teresina_metro_municips)
all_udh_municips <- append(all_udh_municips, vale_do_rio_cuiaba_metro_municips)

# --------------------------------------------------------------------------- #
# 3. Join internal with Census data based on nearest neighbors of coordinates #
# PART 1: Municipalities                                                     #
# --------------------------------------------------------------------------- #

# internal data
input <- read.csv("full_geomerged_df_2.csv")

brazil_df <- input
brazil_df <- brazil_df %>%
  mutate(customer_city = paste(customer_city, "(", sep = " "),
         customer_city = paste(customer_city, customer_state, sep = ""),
         customer_city = paste(customer_city, ")", sep=""),
         
         # already done beforehand but do it again just to be certain
         customer_city = tolower(customer_city),
         customer_city = stri_trans_general(customer_city, "Latin-ASCII")
  )

# Some (few) in "customer_city" are actual customer cities instead of municipalities
# So I searched the corresponding municipality to these cities
# Code below goes through the whole thing.
keys <- read_excel("dffort.xlsx")
keys <- keys %>%
  drop_na() %>%
  mutate(new_names = as.character(new_names),
         new_names = stri_trans_general(new_names, "Latin-ASCII"),
         new_names = tolower(new_names)) %>%
  select(- nummie)
keys <- keys[!duplicated(keys$new_names),]
brazil_df <- merge(brazil_df,
                   keys,
                   by.x = "customer_city",
                   by.y = "old_names",
                   all.x = TRUE)
brazil_df <- brazil_df %>%
  mutate(customer_city = ifelse(is.na(new_names) == TRUE, customer_city, new_names)) %>%
  select(- new_names)

# Get all cases that are NOT in the list of metro-municipalities
non_metros_1 <- brazil_df %>%
  filter(!customer_city %in% all_udh_municips)

# Read municipal level 2010 census data
brazil_municip <- read_excel("./atlas_data/brazil_municipal.xlsx")
brazil_municip <- column_fixer(brazil_municip, "municip")

# Add columns to ensure coumns are the same for rbind
non_metros_1 <- non_metros_1 %>%
  select(- index) %>%
  mutate(
    dist = NA,
    udh.Territorialidades = NA,
    udh.População.total.2010 = NA,
    udh.População.rural.2010 = NA,
    udh.População.urbana.2010 = NA,
    udh.IDHM.2010 = NA,
    udh.IDHM.Educação.2010 = NA,
    udh.Taxa.de.analfabetismo...25.anos.ou.mais.de.idade.2010 = NA,
    udh.Taxa.de.analfabetismo...18.anos.ou.mais.de.idade.2010 = NA,
    udh.urbanity = NA,
    udh.rurality = NA,
    udh.cumul_age_24 = NA,
    udh.young_ratio = NA,
    udh.lat = NA,
    udh.long = NA,
    metro = NA
  )
# Finally, merge
non_metros_1 <- merge(non_metros_1,
                      brazil_municip,
                      by.x = "customer_city",
                      by.y = "mc.Territorialidades",
                      all.x = TRUE)

colSums(is.na(non_metros_1)) # only 80 NA whereas it was 876

# --------------------------------------------------------------------------- #
# 3. Join internal with Census data based on nearest neighbors of coordinates #
# PART 2: Metros                                                              #
# --------------------------------------------------------------------------- #

# Load data on census 2010 + retrieved coordinates
sp_udh <- read.csv("./udh_queried_data/sao_paulo_udh_queried.csv")
fortaleza_udh <- read.csv("./udh_queried_data/fortaleza_udh_queried.csv")
recife_udh <- read.csv("./udh_queried_data/recife_udh_queried.csv")
rio_dj_udh <- read.csv("./udh_queried_data/rio_dj_udh_queried.csv")
salvador_udh <- read.csv("./udh_queried_data/salvador_udh_queried.csv")
porto_alegre_udh <- read.csv("./udh_queried_data/porto_alegre_udh_queried.csv")
natal_udh <- read.csv("./udh_queried_data/natal_udh_queried.csv")
maceio_udh <- read.csv("./udh_queried_data/maceio_udh_queried.csv")
belo_horizonte_udh <- read.csv("./udh_queried_data/belo_horizonte_udh_queried.csv")
campinas_udh <- read.csv("./udh_queried_data/campinas_udh_queried.csv")
curitiba_udh <- read.csv("./udh_queried_data/curtiba_udh_queried.csv")
belem_udh <- read.csv("./udh_queried_data/belem_udh_queried.csv")
goiania_udh <- read.csv("./udh_queried_data/goiania_udh_queried.csv")
grande_vitoria_udh <- read.csv("./udh_queried_data/grande_vitoria_udh_queried.csv")
florianopolis_udh <- read.csv("./udh_queried_data/florianopolis_udh_queried.csv")
manaus_udh <- read.csv("./udh_queried_data/manaus_udh_queried.csv")
sao_luis_udh <- read.csv("./udh_queried_data/sao_luis_udh_queried.csv")
sorocaba_udh <-read.csv("./udh_queried_data/sorocaba_udh_queried.csv")
baixada_santista_udh <- read.csv("./udh_queried_data/baixada_santista_udh_queried.csv")
brasilia_udh <- read.csv("./udh_queried_data/brasilia_udh_queried.csv")
teresina_udh <- read.csv("./udh_queried_data/teresina_udh_queried.csv")
petrolina_juazeiro_udh <- read.csv("./udh_queried_data/petrolina_juazeiro_udh_queried.csv")
vale_do_rio_cuiaba_udh <- read.csv("./udh_queried_data/vale_do_rio_cuiaba_udh_queried.csv")
vale_do_paraiba_e_litoral_norte_udh <- read.csv("./udh_queried_data/vale_do_paraiba_e_litoral_norte_udh_queried.csv")

# Euclidean method to find nearest neighbors
sao_paulo_udh_merged <- udh_merge_ex_with_in(brazil_df, sp_udh, sao_paulo_metro_municips)
sao_paulo_udh_merged$metro <- "sao_paulo"
fortaleza_udh_merged <- udh_merge_ex_with_in(brazil_df, fortaleza_udh, fortaleza_metro_municips)
fortaleza_udh_merged$metro <- "fortaleza"
recife_udh_merged <- udh_merge_ex_with_in(brazil_df, recife_udh, recife_metro_municips)
recife_udh_merged$metro <- "recife"
rio_dj_udh_merged <- udh_merge_ex_with_in(brazil_df, rio_dj_udh, rio_dj_metro_municips)
rio_dj_udh_merged$metro <- "rio_de_janeiro"
salvador_udh_merged <- udh_merge_ex_with_in(brazil_df, salvador_udh, salvador_metro_municips)
salvador_udh_merged$metro <- "salvador"
porto_alegre_udh_merged <- udh_merge_ex_with_in(brazil_df, porto_alegre_udh, porto_alegre_metro_municips)
porto_alegre_udh_merged$metro <- "porto_alegre"
natal_udh_merged <- udh_merge_ex_with_in(brazil_df, natal_udh, natal_metro_municips)
natal_udh_merged$metro <- "natal"
maceio_udh_merged <- udh_merge_ex_with_in(brazil_df, maceio_udh, maceio_metro_municips)
maceio_udh_merged$metro <- "maceio"
belo_horizonte_udh_merged <- udh_merge_ex_with_in(brazil_df, belo_horizonte_udh, belo_horizonte_metro_municips)
belo_horizonte_udh_merged$metro <- "belo_horizonte"
campinas_udh_merged <- udh_merge_ex_with_in(brazil_df, campinas_udh, campinas_metro_municips)
campinas_udh_merged$metro <- "campinas"
curitiba_udh_merged <- udh_merge_ex_with_in(brazil_df, curitiba_udh, curitiba_metro_municips)
curitiba_udh_merged$metro <- "curitiba"
belem_udh_merged <- udh_merge_ex_with_in(brazil_df, belem_udh, belem_metro_municips)
belem_udh_merged$metro <- "belem"
goiania_udh_merged <- udh_merge_ex_with_in(brazil_df, goiania_udh, goiania_metro_municips)
goiania_udh_merged$metro <- "goiania"
grande_vitoria_udh_merged <- udh_merge_ex_with_in(brazil_df, grande_vitoria_udh, grande_vitoria_metro_municips)
grande_vitoria_udh_merged$metro <- "grande_vitoria"
florianopolis_udh_merged <- udh_merge_ex_with_in(brazil_df, florianopolis_udh, florianopolis_metro_municips)
florianopolis_udh_merged$metro <- "florianopolis"
manaus_udh_merged <- udh_merge_ex_with_in(brazil_df, manaus_udh, manaus_metro_municips)
manaus_udh_merged$metro <- "manaus"
sao_luis_udh_merged <- udh_merge_ex_with_in(brazil_df, sao_luis_udh, sao_luis_metro_municips)
sao_luis_udh_merged$metro <- "sao_luis"
sorocaba_udh_merged <- udh_merge_ex_with_in(brazil_df, sorocaba_udh, sorocaba_metro_municips)
sorocaba_udh_merged$metro <- "sorocaba"
baixada_santista_udh_merged <- udh_merge_ex_with_in(brazil_df, baixada_santista_udh, baixada_santista_metro_municips)
baixada_santista_udh_merged$metro <- "baixada_santista"
brasilia_udh_merged <- udh_merge_ex_with_in(brazil_df, brasilia_udh, brasilia_metro_municips) 
brasilia_udh_merged$metro <- "brasilia"
teresina_udh_merged <- udh_merge_ex_with_in(brazil_df, teresina_udh, teresina_metro_municips)
teresina_udh_merged$metro <- "teresina"
petrolina_juazeiro_udh_merged <- udh_merge_ex_with_in(brazil_df, petrolina_juazeiro_udh, petrolina_juazeiro_metro_municips )
petrolina_juazeiro_udh_merged$metro <- "petrolina_juazeiro"
vale_do_rio_cuiaba_udh_merged <- udh_merge_ex_with_in(brazil_df, vale_do_rio_cuiaba_udh, vale_do_rio_cuiaba_metro_municips)
vale_do_rio_cuiaba_udh_merged$metro <- "vale_do_rio_cuiaba"
vale_do_paraiba_e_litoral_norte_udh_merged <- udh_merge_ex_with_in(brazil_df, vale_do_paraiba_e_litoral_norte_udh, litoral_norte_metro_municips)
vale_do_paraiba_e_litoral_norte_udh_merged$metro <- "litoral_norte"

# In the last few census 2010 data imports, I accidentally had more columns included
# in the query. The code below fixes that. 
to_get_rid_func <- function(rid_dataset) {
  rid_dataset <- rid_dataset %>%
    select(
      - udh.Taxa.de.analfabetismo...11.a.14.anos.de.idade.2010,
      - udh.Taxa.de.analfabetismo...15.a.17.anos.de.idade.2010,
      - udh.Taxa.de.analfabetismo...18.a.24.anos.de.idade.2010,
      - udh.Taxa.de.analfabetismo...15.anos.ou.mais.de.idade.2010)
}
brasilia_udh_merged <- to_get_rid_func(brasilia_udh_merged)
manaus_udh_merged <- to_get_rid_func(manaus_udh_merged)
petrolina_juazeiro_udh_merged <- to_get_rid_func(petrolina_juazeiro_udh_merged)
sao_luis_udh_merged <- to_get_rid_func(sao_luis_udh_merged)
sorocaba_udh_merged <- to_get_rid_func(sorocaba_udh_merged)
teresina_udh_merged <- to_get_rid_func(teresina_udh_merged)
vale_do_paraiba_e_litoral_norte_udh_merged <- to_get_rid_func(vale_do_paraiba_e_litoral_norte_udh_merged)
vale_do_rio_cuiaba_udh_merged <- to_get_rid_func(vale_do_rio_cuiaba_udh_merged)

# Turn all seperate UDH data into 1 big metro datset
metros_1_euclidean <- rbind(sao_paulo_udh_merged, fortaleza_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, recife_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, rio_dj_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, salvador_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, porto_alegre_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, natal_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, maceio_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, belo_horizonte_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, curitiba_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, belem_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, goiania_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, grande_vitoria_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, florianopolis_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, manaus_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, sao_luis_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, sorocaba_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, baixada_santista_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, brasilia_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, teresina_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, petrolina_juazeiro_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, vale_do_rio_cuiaba_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, vale_do_paraiba_e_litoral_norte_udh_merged)
metros_1_euclidean <- rbind(metros_1_euclidean, campinas_udh_merged)

# ---------------------------------------------------------------- #
# 4. Bind together the municipality-level data and the metro data  # ----------                                                           #
# ---------------------------------------------------------------- #

colnames(non_metros_1[! colnames(metros_1) %in% colnames(non_metros_1)])


metros_1_euclidean <- metros_1_euclidean %>%
  rename(udh.População.total.2010  = udh.PopulaÃ.Ã.o.total.2010,
         udh.População.rural.2010  = udh.PopulaÃ.Ã.o.rural.2010,
         udh.População.urbana.2010 = udh.PopulaÃ.Ã.o.urbana.2010,
         udh.IDHM.Educação.2010    = udh.IDHM.EducaÃ.Ã.o.2010)


# Sort out the precise columns to ensure a smooth rbind
metros_1 <- metros_1_euclidean %>%
  select(
    - index_other_data, 
    - X,               
    - index,
    - local_index
  ) %>%
  mutate(
    `mc.População total 2010` = NA,
    `mc.População rural 2010` = NA,
    `mc.População urbana 2010` = NA,
    `mc.IDHM 2010` = NA,
    `mc.IDHM Educação 2010` = NA,
    `mc.Taxa de analfabetismo - 25 anos ou mais de idade 2010` = NA,
    `mc.Taxa de analfabetismo - 18 anos ou mais de idade 2010` = NA,
    mc.urbanity = NA,
    mc.rurality = NA,
    mc.cumul_age_24 = NA,
    mc.young_ratio = NA
  ) %>%
  filter(
    dist < 2 # to deal with extreme distances
  )


# Put together both sides of the equation
binded_df <- rbind(metros_1, non_metros_1)
nrow(binded_df) # 98973
saved_binded <- binded_df



# Instances with overlap? NOPE (should be zero)
nrow(binded_df[is.na(binded_df$mc.urbanity) == FALSE & 
               is.na(binded_df$udh.cumul_age_24) == FALSE,])

colSums(is.na(binded_df))


# hhhhh
both_nas <- binded_df[
  is.na(binded_df$mc.young_ratio) == TRUE &
    is.na(binded_df$udh.long) == TRUE,] # check


# Take care of double NA rows.
binded_df <- subset(binded_df, # xor works because there are no overlaps on 1
                    xor(is.na(mc.young_ratio), 
                        is.na(udh.long))) # implement 




# --------------------- #
# 5. Clean, tidy, etc.  # -----------------------------------------------------
# --------------------- #

binded_df <- binded_df %>%
  # First fill with mc data
  mutate(new_total_pop = `mc.População total 2010`,
         new_total_rural = `mc.População rural 2010`,
         new_total_urban = `mc.População urbana 2010`,
         new_idhm = `mc.IDHM 2010`,
         new_idhm_edu = `mc.IDHM Educação 2010`,
         new_anafal_25_oumais = `mc.Taxa de analfabetismo - 25 anos ou mais de idade 2010`,
         new_anafal_18_oumais = `mc.Taxa de analfabetismo - 18 anos ou mais de idade 2010`,
         new_urbanity = mc.urbanity,
         new_rurality = mc.rurality,
         new_cumul_age_24 = mc.cumul_age_24,
         new_young_ratio = mc.young_ratio
  ) %>%
  # If na, fill up with the udh data
  mutate(
    new_total_pop = ifelse(is.na(new_total_pop) == TRUE, udh.População.total.2010, new_total_pop),
    new_total_rural = ifelse(is.na(new_total_rural) == TRUE, udh.População.rural.2010, new_total_rural),
    new_total_urban = ifelse(is.na(new_total_urban) == TRUE, udh.População.urbana.2010, new_total_urban),
    new_idhm = ifelse(is.na(new_idhm) == TRUE, udh.IDHM.2010, new_idhm),
    new_idhm_edu = ifelse(is.na(new_idhm_edu) == TRUE, udh.IDHM.Educação.2010, new_idhm_edu),
    new_anafal_25_oumais = ifelse(is.na(new_anafal_25_oumais) == TRUE, udh.Taxa.de.analfabetismo...25.anos.ou.mais.de.idade.2010, new_anafal_25_oumais),
    new_anafal_18_oumais = ifelse(is.na(new_anafal_18_oumais) == TRUE, udh.Taxa.de.analfabetismo...18.anos.ou.mais.de.idade.2010, new_anafal_18_oumais),
    new_urbanity = ifelse(is.na(new_urbanity) == TRUE, udh.urbanity, new_urbanity),
    new_rurality = ifelse(is.na(new_rurality) == TRUE, udh.rurality, new_rurality),
    new_cumul_age_24 = ifelse(is.na(new_cumul_age_24) == TRUE, udh.cumul_age_24, new_cumul_age_24),
    new_young_ratio = ifelse(is.na(new_young_ratio) == TRUE, udh.young_ratio, new_young_ratio)
  ) %>%
  mutate(
    udh_indicator = ifelse(is.na(udh.urbanity) == FALSE, 1, 0)
  ) %>%
  select(
    - udh.População.total.2010,
    - udh.População.rural.2010,
    - udh.População.urbana.2010,
    - udh.IDHM.2010,
    - udh.IDHM.Educação.2010,
    - udh.Taxa.de.analfabetismo...25.anos.ou.mais.de.idade.2010,
    - udh.Taxa.de.analfabetismo...18.anos.ou.mais.de.idade.2010,
    - udh.urbanity,
    - udh.rurality,
    - udh.cumul_age_24,
    - udh.young_ratio,
    - `mc.População total 2010`,
    - `mc.População rural 2010`,
    - `mc.População urbana 2010`,
    - `mc.IDHM 2010`,
    - `mc.IDHM Educação 2010`,
    - `mc.Taxa de analfabetismo - 25 anos ou mais de idade 2010`,
    - `mc.Taxa de analfabetismo - 18 anos ou mais de idade 2010`,
    - mc.urbanity,
    - mc.rurality,
    - mc.cumul_age_24,
    - mc.young_ratio
  )

colSums(is.na(binded_df))


binded_df <- binded_df %>%
  mutate(udh.Territorialidades = as.character(udh.Territorialidades),
         customer_city = as.character(customer_city),
         customer_city = ifelse(!is.na(udh.Territorialidades) & !is.na(customer_city), udh.Territorialidades, customer_city),
         customer_city = as.factor(customer_city))


# Cache full dataset!
write.csv(binded_df, "full_geomerged_df_3.csv")


# ----------- #
# 6. Visuals  # ---------------------------------------------------------------
# ----------- #
library(geobr)
library(crul)
library(extrafont)
library(ggplot2)

# PART 1: Show neighbors for each metro!
# --------------------------------------

metros <- read_metro_area(2010)

# Get the fonts right
windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM São Paulo",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Sao Paulo (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "sao_paulo"  & binded_df$udh.lat < -23.2,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "sao_paulo"  & binded_df$udh.lat < -23.2,]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "sao_paulo" & binded_df$udh.lat < -23.2,]$udh.lat, 
    x =  binded_df[binded_df$metro == "sao_paulo"  & binded_df$udh.lat < -23.2,]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Baixada Santista",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Baixada Santista (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "baixada_santista" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "baixada_santista",]$centroid_long), col = "black", size = 1) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "baixada_santista",]$udh.lat, 
    x =  binded_df[binded_df$metro == "baixada_santista",]$udh.long), col = "red", size = 1, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Belém",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Belém (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "belem",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "belem",]$centroid_long), col = "black", size = 1) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "belem",]$udh.lat, 
    x =  binded_df[binded_df$metro == "belem",]$udh.long), col = "red", size = 1, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Belo Horizonte",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Belo Horizonte (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "belo_horizonte" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "belo_horizonte",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "belo_horizonte",]$udh.lat, 
    x =  binded_df[binded_df$metro == "belo_horizonte",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RIDE Distrito Federal (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "brasilia" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "brasilia",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "brasilia",]$udh.lat, 
    x =  binded_df[binded_df$metro == "brasilia",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Campinas",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Campinas (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "campinas" & binded_df$udh.lat < -22 ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "campinas"  & binded_df$udh.lat < -22,]$centroid_long), col = "black", size = 0.8) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "campinas"  & binded_df$udh.lat < -22,]$udh.lat, 
    x =  binded_df[binded_df$metro == "campinas" & binded_df$udh.lat < -22,]$udh.long), col = "red", size = 0.8, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Curitiba",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Curitiba (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "curitiba" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "curitiba",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "curitiba",]$udh.lat, 
    x =  binded_df[binded_df$metro == "curitiba",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Florianópolis",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Florianópolis (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "florianopolis" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "florianopolis",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "florianopolis",]$udh.lat, 
    x =  binded_df[binded_df$metro == "florianopolis",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Fortaleza",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Fortaleza (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "fortaleza" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "fortaleza",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "fortaleza",]$udh.lat, 
    x =  binded_df[binded_df$metro == "fortaleza",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Goiânia",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Goiânia (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "goiania" ,]$centroid_lat, 
    x =  binded_df[binded_df$metro == "goiania",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "goiania",]$udh.lat, 
    x =  binded_df[binded_df$metro == "goiania",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Grande Vitória",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Grande Vitória (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "grande_vitoria",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "grande_vitoria",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "grande_vitoria" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "grande_vitoria",]$udh.long), col = "red", size = 0.2, shape = 23) + 
  xlim(-40.9, -40)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Maceió",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Maceió (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "maceio",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "maceio",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "maceio" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "maceio",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Manaus",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Manaus (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "manaus",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "manaus",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "manaus" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "manaus",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RIDE Petrolina/Juazeiro Região Administrativa Integrada de Desenvolvimento do Pólo Petrolina/PE e Juazeiro/BA",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RIDE Petrolina Juazeiro (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "petrolina_juazeiro",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "petrolina_juazeiro",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "petrolina_juazeiro" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "petrolina_juazeiro",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Porto Alegre",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Porto Alegre (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "porto_alegre",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "porto_alegre",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "porto_alegre" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "porto_alegre",]$udh.long), col = "red", size = 0.2, shape = 23) + 
  xlim(-52, -50) + 
  ylim(-30.8, -29.25)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Recife",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Recife (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "recife",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "recife",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "recife" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "recife",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Rio de Janeiro",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Rio de Janeiro (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "rio_de_janeiro",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "rio_de_janeiro",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "rio_de_janeiro" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "rio_de_janeiro",]$udh.long), col = "red", size = 0.2, shape = 23) +
  xlim(-44,-42.5) +
  ylim(-23.2, -22.3)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Salvador",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Salvador (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "salvador",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "salvador",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "salvador" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "salvador",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM Sorocaba",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RM Sorocaba (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "sorocaba",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "sorocaba",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "sorocaba" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "sorocaba",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RIDE TERESINA - Região Integrada de Desenvolvimento da Grande Teresina",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in RIDE Teresina (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "teresina",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "teresina",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "teresina" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "teresina",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "?",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in Litoral Norte (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "litoral_norte",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "litoral_norte",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "litoral_norte" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "litoral_norte",]$udh.long), col = "red", size = 0.2, shape = 23)

ggplot() +
  geom_sf(data=metros[metros$name_metro == "RM  Vale do Rio Cuiabá",], fill="steelblue", color="black", size=.2, show.legend = FALSE) +
  labs(title = expression(bold("Figure 2")),
       subtitle = expression(italic("Locations of Olist data cases in Litoral Norte (black) overlaid with 2010 census data (red)"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + 
  ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = binded_df[binded_df$metro == "vale_do_rio_cuiaba",]$centroid_lat, 
    x =  binded_df[binded_df$metro == "vale_do_rio_cuiaba",]$centroid_long), col = "black", size = 0.01) +
  geom_point(aes(
    y = binded_df[binded_df$metro == "vale_do_rio_cuiaba" ,]$udh.lat, 
    x =  binded_df[binded_df$metro == "vale_do_rio_cuiaba",]$udh.long), col = "red", size = 0.2, shape = 23)


# PART 2: Full Brazil plot
# ------------------------
state <- read_state(year=2010)
# The one 
note = expression(paste(italic("Note. "), "Variation in longitude along latitude is not adjusted for in this graph."))

ggplot() + geom_sf(data=state, 
                   fill=(ifelse(state$name_region == "Norte", "deepskyblue", 
                                ifelse(state$name_region == "Nordeste", "deepskyblue4",
                                       ifelse(state$name_region == "Sul", "coral", 
                                              ifelse(state$name_region == "Sudeste", "deeppink3", "chartreuse"))))), color="white", size=.15, show.legend = FALSE) +
  labs(title = expression(bold("Figure 5")),
       subtitle = expression(italic("Geographic locations of Olist dataset cases in Brazil's five administritave regions"))) +
  theme_minimal() +
  labs(caption = note) + 
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  annotate("text", x = -48, y = -31, label = "South", col = "coral1", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -63, y = -18, label = "Centerwest", col = "chartreuse3", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -47, y = 2.5, label = "North", col = "deepskyblue1", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -32, y = -10, label = "Northeast", col = "deepskyblue4", family = "Times New Roman", fontface = "bold") +
  annotate("text", x = -37, y = -22, label = "Southeast", col = "deeppink3", family = "Times New Roman", fontface = "bold") +
  
  xlab("Longitude in degrees using WGS84 (EPSG:4326)") + ylab("Latitude in degrees using WGS84 (EPSG:4326)") +
  geom_point(aes(
    y = brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_lat, 
    x =  brazil_df[brazil_df$centroid_lat < 10 & brazil_df$centroid_long < -30 ,]$centroid_long), size = 0.07) + theme(legend.position="bottom")

# PART 3:Histograms of some continuous variables
# ----------------------------------------------

ggplot(binded_df, aes(x= new_idhm)) + 
  geom_histogram(color="black", fill="coral", size = 0.1, bins = 40) + 
  facet_wrap(~udh_indicator) +
  xlim(0, 1.00) + 
  labs(title = expression(bold("Figure 3")),
       subtitle = expression(italic("Difference in frequencies of Human Development Indices between UDH (1) and municipal data (0) cases"))) +
  xlab("Human Development Index associated with the location of the related to case") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

ggplot(binded_df, aes(x= new_urbanity)) + 
  geom_histogram(color="black", fill="coral", size = 0.1) + 
  facet_wrap(~udh_indicator) +
  labs(title = expression(bold("Figure 4")),
       subtitle = expression(italic("Difference in frequencies of urbanicity cases between UDH (1) and municipal data (0) cases"))) +
  xlab("Ratio of urban population to total population of a single spatial unit associated with a case") + ylab("Count") +
  theme(text = element_text(family = "Times New Roman", size = 14),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14))

# ------------------------------------------------- #
# 7. Haversine &Equilateral Approximation (extra)   # --------------------------
# ------------------------------------------------- #

# Basically the same as before but we call the Haversine distance function
sao_paulo_udh_haversine <- haversine_function(brazil_df, sp_udh, sao_paulo_metro_municips)
fortaleza_udh_haversine <- haversine_function(brazil_df, fortaleza_udh, fortaleza_metro_municips)
recife_udh_haversine <- haversine_function(brazil_df, recife_udh, recife_metro_municips)
rio_dj_udh_haversine <- haversine_function(brazil_df, rio_dj_udh, rio_dj_metro_municips)
salvador_udh_haversine <- haversine_function(brazil_df, salvador_udh, salvador_metro_municips)
porto_alegre_udh_haversine <- haversine_function(brazil_df, porto_alegre_udh, porto_alegre_metro_municips)
natal_udh_haversine <- haversine_function(brazil_df, natal_udh, natal_metro_municips)
maceio_udh_haversine <- haversine_function(brazil_df, maceio_udh, maceio_metro_municips)
belo_horizonte_udh_haversine <- haversine_function(brazil_df, belo_horizonte_udh, belo_horizonte_metro_municips)
campinas_udh_haversine <- haversine_function(brazil_df, campinas_udh, campinas_metro_municips)
curitiba_udh_haversine <- haversine_function(brazil_df, curitiba_udh, curitiba_metro_municips)
belem_udh_haversine <- haversine_function(brazil_df, belem_udh, belem_metro_municips)
goiania_udh_haversine <- haversine_function(brazil_df, goiania_udh, goiania_metro_municips)
grande_vitoria_udh_haversine <- haversine_function(brazil_df, grande_vitoria_udh, grande_vitoria_metro_municips)
florianopolis_udh_haversine <- haversine_function(brazil_df, florianopolis_udh, florianopolis_metro_municips)
manaus_udh_haversine <- haversine_function(brazil_df, manaus_udh, manaus_metro_municips)
sao_luis_udh_haversine <- haversine_function(brazil_df, sao_luis_udh, sao_luis_metro_municips)
sorocaba_udh_haversine <- haversine_function(brazil_df, sorocaba_udh, sorocaba_metro_municips)
baixada_santista_udh_haversine <- haversine_function(brazil_df, baixada_santista_udh, baixada_santista_metro_municips)
brasilia_udh_haversine <- haversine_function(brazil_df, brasilia_udh, brasilia_metro_municips) 
teresina_udh_haversine <- haversine_function(brazil_df, teresina_udh, teresina_metro_municips)
petrolina_juazeiro_udh_haversine <- haversine_function(brazil_df, petrolina_juazeiro_udh, petrolina_juazeiro_metro_municips )
vale_do_rio_cuiaba_udh_haversine <- haversine_function(brazil_df, vale_do_rio_cuiaba_udh, vale_do_rio_cuiaba_metro_municips)
vale_do_paraiba_e_litoral_norte_udh_haversine <- haversine_function(brazil_df, vale_do_paraiba_e_litoral_norte_udh, litoral_norte_metro_municips)

# Fix column discrepancies
brasilia_udh_haversine <- to_get_rid_func(brasilia_udh_haversine)
manaus_udh_haversine <- to_get_rid_func(manaus_udh_haversine)
petrolina_juazeiro_udh_haversine <- to_get_rid_func(petrolina_juazeiro_udh_haversine)
sao_luis_udh_haversine <- to_get_rid_func(sao_luis_udh_haversine)
sorocaba_udh_haversine <- to_get_rid_func(sorocaba_udh_haversine)
teresina_udh_haversine <- to_get_rid_func(teresina_udh_haversine)
vale_do_paraiba_e_litoral_norte_udh_haversine <- to_get_rid_func(vale_do_paraiba_e_litoral_norte_udh_haversine)
vale_do_rio_cuiaba_udh_haversine <- to_get_rid_func(vale_do_rio_cuiaba_udh_haversine)

# one subset for merged haversine
metros_1 <- rbind(sao_paulo_udh_haversine, fortaleza_udh_haversine)
metros_1 <- rbind(metros_1, recife_udh_haversine)
metros_1 <- rbind(metros_1, rio_dj_udh_haversine)
metros_1 <- rbind(metros_1, salvador_udh_haversine)
metros_1 <- rbind(metros_1, porto_alegre_udh_haversine)
metros_1 <- rbind(metros_1, natal_udh_haversine)
metros_1 <- rbind(metros_1, maceio_udh_haversine)
metros_1 <- rbind(metros_1, belo_horizonte_udh_haversine)
metros_1 <- rbind(metros_1, curitiba_udh_haversine)
metros_1 <- rbind(metros_1, belem_udh_haversine)
metros_1 <- rbind(metros_1, goiania_udh_haversine)
metros_1 <- rbind(metros_1, grande_vitoria_udh_haversine)
metros_1 <- rbind(metros_1, florianopolis_udh_haversine)
metros_1 <- rbind(metros_1, manaus_udh_haversine)
metros_1 <- rbind(metros_1, sao_luis_udh_haversine)
metros_1 <- rbind(metros_1, sorocaba_udh_haversine)
metros_1 <- rbind(metros_1, baixada_santista_udh_haversine)
metros_1 <- rbind(metros_1, brasilia_udh_haversine)
metros_1 <- rbind(metros_1, teresina_udh_haversine)
metros_1 <- rbind(metros_1, petrolina_juazeiro_udh_haversine)
metros_1 <- rbind(metros_1, vale_do_rio_cuiaba_udh_haversine)
metros_1 <- rbind(metros_1, vale_do_paraiba_e_litoral_norte_udh_haversine)

# Equilateral Approximation method
equ_formu_1 <- function(df_with_coordinates) {
  df_with_coordinates <- df_with_coordinates %>%
    mutate(udh.long = udh.long * cos(udh.lat))
}








