install.packages('tidyverse')
library(tidyverse)

install.packages('rgbif')
library(rgbif)

dados <- rgbif::occ_data(scientificName = "Acanthurus chirurgus", hasCoordinate = TRUE)

# Exiba as primeiras linhas dos dados
head(dados$data)


# checar funcoes
??occ_data

dados <- occ_data(scientificName = "Acanthurus chirurgus", hasCoordinate = TRUE)


# baixar ocorrencias
cirurgiao_gbif <- occ_data(scientificName = "Acanthurus chirurgus", 
                           hasCoordinate = TRUE,
                           hasGeospatialIssue=FALSE)

# dimensoes
dim(cirurgiao_gbif)

dim(cirurgiao_gbif$data)

# checar campos
cirurgiao_gbif$data %>% names

library(dplyr)
library(rgbif)

# Obter a lista de problemas do GBIF
issues <- gbif_issues()

# Verificar a estrutura do objeto retornado
str(issues)

# Converter para data.frame
issues_df <- data.frame(issues)

# Verificar os nomes das colunas
print(names(issues_df))

# checar problemas reportados
issues_gbif <- cirurgiao_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

cirurgiao_gbif1 <- cirurgiao_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 

cirurgiao_gbif1 <- cirurgiao_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(cirurgiao_gbif1, unique)


# Carregar o pacote bdc
library(bdc)
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = cirurgiao_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- cirurgiao_gbif1 %>%
  select(acceptedScientificName, decimalLatitude, decimalLongitude) %>%
  rename(decimallongitude = decimalLongitude,
         decimallatitude = decimalLatitude,
         scientificName = acceptedScientificName) %>% 
  as_tibble() %>% 
  mutate(val = cc_val(., value = "flagged"),
         sea = cc_sea(., value = "flagged"),
         capital = cc_cap(., value = "flagged"))

# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "capital")  

cl %>% 
  rename(decimalLongitude = decimallongitude,
         decimalLatitude = decimallatitude) %>% 
  bdc::bdc_quickmap(., col_to_map = "sea")  

# investigar niveis suspeitos
cirurgiao_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()

cirurgiao_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 

########Não precisou corrigir pro cirurgiao#######################################################
# fonte das regioes erradas###
cirurgiao_gbif1 %>% 
  filter(waterBody %in% c("")) %>% 
  distinct(datasetName)


cirurgiao_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science"))

# filtrar todas do dataset suspeito
cirurgiao_gbif_ok <- cirurgiao_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))
###########################################################################

library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = cirurgiao_gbif1, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Acanthurus chirurgus")))

# checar profundidade
cirurgiao_gbif1 %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

## OBIS
install.packages('robis')
library(robis)

cirurgiao_obis <- robis::occurrence("Acanthurus chirurgus")

# checar dados
names(cirurgiao_obis)

cirurgiao_obis1 <- cirurgiao_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
cirurgiao_obis1 %>% 
  distinct(flags)

# check NA em datasetName
cirurgiao_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
cirurgiao_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("Mar Caribe", "atlantique", "South Atlantic", "Atlantic Ocean", "Caribe", "Bahia de La Habana", "WESTERN ATLANTIC", "Gulf of Mexico", "CARIB")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 

# checar niveis
cirurgiao_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("Mar Caribe", "atlantique", "South Atlantic", "Atlantic Ocean", "Caribe", "Bahia de La Habana", "WESTERN ATLANTIC", "Gulf of Mexico", "CARIB")) %>% 
  lapply(., unique)

# ok
cirurgiao_obis_ok <- cirurgiao_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("Mar Caribe", "atlantique", "South Atlantic", "Atlantic Ocean", "Caribe", "Bahia de La Habana", "WESTERN ATLANTIC", "Gulf of Mexico", "CARIB", NA)) 

# check
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = cirurgiao_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Acanthurus chirurgus")))

# unir GBIF e OBIS
# ver diferencas

setdiff(names(cirurgiao_gbif1), names(cirurgiao_obis_ok))

setdiff(names(cirurgiao_obis1), names(cirurgiao_gbif_ok))

library(tidyr)
library(tibble)

all_data <- bind_rows(cirurgiao_gbif1 %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      cirurgiao_obis_ok %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Acanthurus chirurgus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Achanthurus chirurgus")))

dir.create("data")
write.csv(all_data, "data/occ_GBIF-OBIS_aca_chir.csv", row.names = FALSE)


