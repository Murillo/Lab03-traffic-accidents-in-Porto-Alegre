####################################################
############ Murillo da Silveira Grubler ###########
####################################################

#####################################################
## Aplicação Lab 3 - Acidentes de trânsitos em POA ##
#####################################################

######################################################
## Função para inserir um char em uma especifica posição
## https://stackoverflow.com/questions/13863599/insert-a-character-at-a-specific-location-in-a-string
split_str_by_index <- function(target, index) {
  index <- sort(index)
  substr(
    rep(target, length(index) + 1),
    start = c(1, index),
    stop = c(index - 1, nchar(target))
  )
}

interleave <- function(v1, v2)
{
  ord1 <- 2 * (1:length(v1)) - 1
  ord2 <- 2 * (1:length(v2))
  c(v1, v2)[order(c(ord1, ord2))]
}

insert_str <- function(target, insert, index) {
  insert <- insert[order(index)]
  index <- sort(index)
  paste(interleave(split_str_by_index(target, index), insert), collapse =
          "")
}
#######################################################

#######################################################
# Função para inserir uma linha em um específico índice
# https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
insertRow <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF, newrow)
  existingDF <-
    existingDF[order(c(1:(nrow(existingDF) - 1), r - 0.5)), ]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)
}
#######################################################

######################################################
# Função criada para tratar algumas colunas do dataset
primeiroTratamento <- function(dataset) {
  dataset$LATITUDE <- gsub(",", "\\.", dataset$LATITUDE)
  fac <- factor(dataset$LATITUDE)
  dataset$LATITUDE <- as.numeric(as.character(fac))
  
  # Convertendo , em . e transformando em número - Longitude
  dataset$LONGITUDE <- gsub(",", "\\.", dataset$LONGITUDE)
  fac <- factor(dataset$LONGITUDE)
  dataset$LONGITUDE <- as.numeric(as.character(fac))
  
  # Ajustando a data do dataset de 2013
  dataset$DATA_OLD <- dataset$DATA_HORA
  dataset$DATA <- NULL
  for (i in 1:dim(dataset)[1]) {
    dataset$DATA[i] <-
      insert_str(as.character(dataset$DATA_OLD[i]), c('-', '-'), c(5, 7))
  }
  return(dataset)
}
#####################################################

######################################################
# Função criada para tratar algumas colunas do dataset
segundoTratamento <- function(dataset) {
  dataset <- dataset %>%
    na.omit() %>%
    mutate(LATITUDE = if_else(
      LATITUDE < -35 |
        LATITUDE > 35,
      LATITUDE / 1000000 - 0.0009,
      LATITUDE - 0.0009
    )) %>%
    filter(LATITUDE < -20) %>%
    mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
    mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
    mutate(FCT_MORTES = as.factor(MORTES))  %>%
    mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
    mutate(FCT_FATAIS = as.factor(FATAIS))
  
  return(dataset)
}

# Função criada para tratar algumas colunas do dataset sem alterar posicionamento
segundoTratamentoSemAlterarPosicionamento <- function(dataset) {
  dataset <- dataset %>%
    na.omit() %>%
    filter(LATITUDE < -20) %>%
    mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
    mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
    mutate(FCT_MORTES = as.factor(MORTES))  %>%
    mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
    mutate(FCT_FATAIS = as.factor(FATAIS))
  
  return(dataset)
}
#####################################################

library("RCurl")
library("tidyverse")
library("shiny")
library("leaflet")
library("leaflet.extras")
library("RColorBrewer")
library("ggplot2")
library("rsconnect")

rsconnect::setAccountInfo(name = '<NAME>',
                          token = '<TOKEN>',
                          secret = '<KEY>')

# Configuração de cores no histograma
acid_types = c(
  "ABALROAMENTO",
  "ATROPELAMENTO",
  "CAPOTAGEM",
  "CHOQUE",
  "COLISAO",
  "EVENTUAL",
  "INCENDIO",
  "QUEDA",
  "TOMBAMENTO"
)

acid_color_scale <- c(
  'salmon',
  'darkorange',
  'olivedrab',
  'limegreen',
  'lightseagreen',
  'deepskyblue',
  'cornflowerblue',
  'orchid',
  'hotpink'
)

scale_color_quant <- c(
  "#ffb3b3",
  "#ff6666",
  "#ff1a1a",
  "#cc0000",
  "#800000",
  "#4d0000",
  "#330000",
  "#000000"
)

informacoes_com_fer_gr <- c(
  "Tipos de acidentes" = "TIPO_ACID",
  "Feridos" = "FERIDOS",
  "Feridos gravimente" = "FERIDOS_GR",
  "Número de mortes no local" = "MORTES",
  "Número de mortes posteriores ao acidente" = "MORTE_POST",
  "Número de mortes no total" = "FATAIS"
)

informacoes_sem_fer_gr <- c(
  "Tipos de acidentes" = "TIPO_ACID",
  "Feridos" = "FERIDOS",
  "Número de mortes no local" = "MORTES",
  "Número de mortes posteriores ao acidente" = "MORTE_POST",
  "Número de mortes no total" = "FATAIS"
)

# URLs dos datasets
URL <- c(
  "2010" = "dataset/acidentes-2010.csv",
  "2011" = "dataset/acidentes-2011.csv",
  "2012" = "dataset/acidentes-2012.csv",
  "2013" = "dataset/acidentes-2013.csv",
  "2014" = "dataset/acidentes-2014.csv",
  "2015" = "dataset/acidentes-2015.csv",
  "2016" = "dataset/acidentes-2016.csv"
)

poa_2010 <- read.csv2(URL["2010"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

poa_2011 <- read.csv2(URL["2011"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

poa_2012 <- read.csv2(URL["2012"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

poa_2013 <- read.csv2(URL["2013"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

poa_2014 <- read.csv2(URL["2014"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

poa_2015 <- read.csv2(URL["2015"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

poa_2016 <- read.csv2(URL["2016"],
                      header = TRUE,
                      sep = ";",
                      dec = ".")

# Total dos dados
names(poa_2013)
names(poa_2014)
names(poa_2015)
names(poa_2016)

######################## 2010 ###########################
# Convertendo , em . e transformando em número - Latitude
poa_2010 <- primeiroTratamento(poa_2010)

######################## 2011 ###########################
# Convertendo , em . e transformando em número - Latitude
poa_2011 <- primeiroTratamento(poa_2011)

######################## 2012 ###########################
# Convertendo , em . e transformando em número - Latitude
poa_2012 <- primeiroTratamento(poa_2012)

######################## 2013 ###########################
# Convertendo , em . e transformando em número - Latitude
poa_2013 <- primeiroTratamento(poa_2013)

######################## 2014 ###########################
# Convertendo , em . e transformando em número - Latitude
poa_2014$LATITUDE <- gsub(",", "\\.", poa_2014$LATITUDE)
fac <- factor(poa_2014$LATITUDE)
poa_2014$LATITUDE <- as.numeric(as.character(fac))

# Convertendo , em . e transformando em número - Longitude
poa_2014$LONGITUDE <- gsub(",", "\\.", poa_2014$LONGITUDE)
fac <- factor(poa_2014$LONGITUDE)
poa_2014$LONGITUDE <- as.numeric(as.character(fac))

# Limpando os dados
poa_2010 <- poa_2010 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))

# Limpando os dados
poa_2011 <- poa_2011 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))

# Limpando os dados
poa_2012 <- poa_2012 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))

# Limpando os dados
poa_2013 <- poa_2013 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_FERIDOS_GR = as.factor(FERIDOS_GR))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))

# Limpando os dados
poa_2014 <- poa_2014 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA_HORA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_FERIDOS_GR = as.factor(FERIDOS_GR))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))

# Limpando os dados
poa_2015 <- poa_2015 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA_HORA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_FERIDOS_GR = as.factor(FERIDOS_GR))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))

# Limpando os dados
poa_2016 <- poa_2016 %>%
  na.omit() %>%
  mutate(LATITUDE = if_else(
    LATITUDE < -35 |
      LATITUDE > 35,
    LATITUDE / 1000000 - 0.0009,
    LATITUDE - 0.0009
  )) %>%
  filter(LATITUDE < -20) %>%
  mutate(DATA = as.Date(DATA, format = "%Y-%m-%d")) %>%
  mutate(FCT_FERIDOS = as.factor(FERIDOS))  %>%
  mutate(FCT_FERIDOS_GR = as.factor(FERIDOS_GR))  %>%
  mutate(FCT_MORTES = as.factor(MORTES))  %>%
  mutate(FCT_MORTE_POST = as.factor(MORTE_POST))  %>%
  mutate(FCT_FATAIS = as.factor(FATAIS))


# Ano inicial
poa <- poa_2016

##################################################
################## Análise #######################
ano = c("2013", "2014", "2015", "2016")

#############################################################################
# O número total de acidentes aumentou de 2013 até 2016?
#############################################################################
total_acidentes = c(dim(poa_2013)[1],
                    dim(poa_2014)[1],
                    dim(poa_2015)[1],
                    dim(poa_2016)[1])
df_total = data.frame(ano, total_acidentes)
colnames(df_total) <- c("Ano", "Total")

ggplot(data = df_total, aes(x = Ano, y = Total)) +
  geom_bar(stat = "identity", fill = "#ff9900") +
  geom_text(aes(label = Total),
            vjust = 1.6,
            color = "white",
            size = 5) +
  ggtitle("O número total de acidentes nos últimos 4 anos em Porto Alegre") +
  theme_minimal()

#############################################################################
# O número total de acidentes com vítimas fatais aumentou de 2013 até 2016?
#############################################################################
f_2013 <- poa_2013 %>% filter(FATAIS > 0)
f_2014 <- poa_2014 %>% filter(FATAIS > 0)
f_2015 <- poa_2015 %>% filter(FATAIS > 0)
f_2016 <- poa_2016 %>% filter(FATAIS > 0)

total_acidentes_fatais = c(dim(f_2013)[1], dim(f_2014)[1], dim(f_2015)[1], dim(f_2016)[1])
df = data.frame(ano, total_acidentes_fatais)
colnames(df) <- c("Ano", "Fatais")

ggplot(data = df, aes(x = Ano, y = Fatais)) +
  geom_bar(stat = "identity", fill = "#FF6666") +
  geom_text(aes(label = Fatais),
            vjust = 1.6,
            color = "white",
            size = 5) +
  ggtitle("O número de mortes fatais em acidentes de trânsito nos últimos 4 anos em Porto Alegre") +
  theme_minimal()


#############################################################################
# Qual o tipo de acidente com vítima fatal ocorre com mais frequencia durante o dia?
#############################################################################
tipos_acidentes_2013 <- poa_2013 %>%
  filter(TIPO_ACID != 'NAO CADASTRADO' & NOITE_DIA == 'DIA') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

tipos_acidentes_2014 <- poa_2014 %>%
  filter(NOITE_DIA == 'DIA') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

tipos_acidentes_2015 <- poa_2015 %>%
  filter(NOITE_DIA == 'DIA') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

tipos_acidentes_2016 <- poa_2016 %>%
  filter(NOITE_DIA == 'DIA') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

p1 <-
  append(tipos_acidentes_2013$Fatais, tipos_acidentes_2014$Fatais)
p2 <- append(p1, tipos_acidentes_2015$Fatais)
p3 <- append(p2, tipos_acidentes_2016$Fatais)

anos = c(rep("2013" , 9), rep("2014" , 9), rep("2015" , 9) , rep("2016" , 9))
tipos = rep(tipos_acidentes_2016$TIPO_ACID, 4)
valores = c(p3)
data = data.frame(anos, tipos, valores)
ggplot(data, aes(fill = tipos, y = valores, x = anos)) +
  ggtitle("Número de mortes por tipo de acidente durante o período do dia nos últimos 4 anos") +
  labs(x = "Anos") +
  labs(y = "Total de vítimas fatais por tipo de acidente") +
  labs(fill = "Tipos de acidentes") +
  geom_bar(position = "dodge", stat = "identity")

#############################################################################
# Qual o tipo de acidente com vítima fatal ocorre com mais frequencia durante o período da noite?
#############################################################################
tipos_acidentes_noite_2013 <- poa_2013 %>%
  filter(TIPO_ACID != 'NAO CADASTRADO' & NOITE_DIA == 'NOITE') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))
tipos_acidentes_noite_2013 <-
  insertRow(tipos_acidentes_noite_2013, c('INCENDIO', 0, 0), 7)
tipos_acidentes_noite_2013$Fatais <-
  as.numeric(tipos_acidentes_noite_2013$Fatais)

tipos_acidentes_noite_2014 <- poa_2014 %>%
  filter(NOITE_DIA == 'NOITE') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

tipos_acidentes_noite_2015 <- poa_2015 %>%
  filter(NOITE_DIA == 'NOITE') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

tipos_acidentes_noite_2016 <- poa_2016 %>%
  filter(NOITE_DIA == 'NOITE') %>%
  group_by(TIPO_ACID) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))
tipos_acidentes_noite_2016 <-
  insertRow(tipos_acidentes_noite_2016, c('INCENDIO', 0, 0), 7)
tipos_acidentes_noite_2016$Fatais <-
  as.numeric(tipos_acidentes_noite_2016$Fatais)

pn1 <-
  append(tipos_acidentes_noite_2013$Fatais,
         tipos_acidentes_noite_2014$Fatais)
pn2 <- append(pn1, tipos_acidentes_noite_2015$Fatais)
pn3 <- append(pn2, tipos_acidentes_noite_2016$Fatais)

anos_noite = c(rep("2013" , 9), rep("2014" , 9), rep("2015" , 9) , rep("2016" , 9))
tipos_noite = rep(tipos_acidentes_noite_2015$TIPO_ACID, 4)
valores_noite = c(pn3)
data_noite = data.frame(anos_noite, tipos_noite, valores_noite)
data_noite <- data_noite[order(anos_noite, tipos_noite), ]
ggplot(data_noite,
       aes(fill = tipos_noite, y = valores_noite, x = anos_noite)) +
  ggtitle("Número de mortes por tipo de acidente durante o período da noite nos últimos 4 anos") +
  labs(x = "Anos") +
  labs(y = "Total de vítimas fatais por tipo de acidente") +
  labs(fill = "Tipos de acidentes") +
  geom_bar(position = "dodge", stat = "identity")

#############################################################################
# Há mais vítimas fatais durante o dia ou a noite?
#############################################################################
fatais_2013 <- poa_2013 %>%
  group_by(NOITE_DIA) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

fatais_2014 <- poa_2014 %>%
  group_by(NOITE_DIA) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

fatais_2015 <- poa_2015 %>%
  group_by(NOITE_DIA) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

fatais_2016 <- poa_2016 %>%
  group_by(NOITE_DIA) %>%
  summarise(Total = n(),
            Fatais = sum(FATAIS))

vn1 <- append(fatais_2013$Fatais, fatais_2014$Fatais)
vn2 <- append(vn1, fatais_2015$Fatais)
vn3 <- append(vn2, fatais_2016$Fatais)

anos_v_fatais = c(rep("2013" , 2), rep("2014" , 2), rep("2015" , 2) , rep("2016" , 2))
tipos_v_fatais = rep(fatais_2016$NOITE_DIA, 4)
valores_v_fatais = c(vn3)
data_v_fatais = data.frame(anos_v_fatais, tipos_v_fatais, valores_v_fatais)
ggplot(data_v_fatais,
       aes(fill = tipos_v_fatais, y = valores_v_fatais, x = anos_v_fatais)) +
  ggtitle("Número de mortes por acidente vínculados com o período do dia.") +
  labs(x = "Anos") +
  labs(y = "Total de vítimas fatais por tipo de acidente") +
  labs(fill = "Período do dia") +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = valores_v_fatais),
    vjust = 1.6,
    color = "white",
    size = 6
  ) +
  facet_wrap( ~ tipos_v_fatais)


#############################################################################
# Há mais acidentes durante o dia ou a noite?
#############################################################################
acn1 <- append(fatais_2013$Total, fatais_2014$Total)
acn2 <- append(acn1, fatais_2015$Total)
acn3 <- append(acn2, fatais_2016$Total)

anos_ac_fatais = c(rep("2013" , 2), rep("2014" , 2), rep("2015" , 2) , rep("2016" , 2))
tipos_ac_fatais = rep(fatais_2016$NOITE_DIA, 4)
valores_ac_fatais = c(acn3)
data_ac_fatais = data.frame(anos_ac_fatais, tipos_ac_fatais, valores_ac_fatais)
ggplot(data_ac_fatais,
       aes(fill = tipos_ac_fatais, y = valores_ac_fatais, x = anos_ac_fatais)) +
  ggtitle("Número de acidentes vínculados com o período do dia.") +
  labs(x = "Anos") +
  labs(y = "Total de acidentes por tipo de acidente") +
  labs(fill = "Período do dia") +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = valores_ac_fatais),
    vjust = 1.6,
    color = "white",
    size = 6
  ) +
  facet_wrap( ~ tipos_ac_fatais)




###################################################
ui <-
  navbarPage(
    "Lab 03",
    id = 'lab03',
    
    tabPanel(
      "Mapa interativo",
      div(
        class = "outer",
        tags$head(includeCSS("www/custom.css")),
        tags$head(includeScript("www/start.js")),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          label = "principal",
          id = "controls",
          class = "panel panel-default over",
          fixed = TRUE,
          draggable = TRUE,
          top = 60,
          left = 30,
          right = "auto",
          bottom = "auto",
          width = 330,
          height = "auto",
          selectInput(
            "periodos",
            label = "Anos",
            choices = c(
              "2010" = "2010",
              "2011" = "2011",
              "2012" = "2012",
              "2013" = "2013",
              "2014" = "2014",
              "2015" = "2015",
              "2016" = "2016"
            ),
            selected = "2016"
          ),
          selectInput(
            "informacoes",
            label = "Informações adicionais",
            choices = informacoes_com_fer_gr,
            selected = "TIPO_ACID"
          ),
          selectInput(
            "tipos_acidentes",
            label = "Tipos de acidentes",
            choices = c("Selecione", as.character(poa$TIPO_ACID))
          ),
          sliderInput(
            "filtro_tipos",
            label = "Filtrar",
            min = 0,
            max = 8,
            value = c(0, 2)
          ),
          dateRangeInput(
            "date_range",
            "Período do ano:",
            format = "dd/mm/yyyy",
            start = min(poa$DATA),
            end = max(poa$DATA)
          ),
          selectInput(
            "periodo_dia",
            label = "Período do dia",
            choices = c("Selecione", as.character(poa$NOITE_DIA))
          ),
          sliderInput(
            inputId = "blur",
            "Heatmap Blur",
            1,
            25,
            value = 15,
            step = 0.1
          ),
          sliderInput(
            inputId = "radius",
            "Heatmap Radius",
            1,
            50,
            value = 8,
            step = 0.1
          ),
          checkboxInput(inputId = "show_points", "Exibir Pontos:", value = FALSE),
          checkboxInput("filter_bounds", "Filtrar BoundingBox", value = TRUE)
        ),
        absolutePanel(
          label = "auxiliar",
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE,
          top = "auto",
          left = 0,
          right = 10,
          bottom = -20,
          width = "100%",
          height = 150,
          plotOutput("linechart", height = 180)
        )
      )
    ),
    tabPanel("Informações complementares",
             dataTableOutput("table_poa")),
    
    navbarMenu(
      "Histogramas",
      tabPanel("Feridos",
               plotOutput("plot_feridos")),
      tabPanel("Feridos Gravemente",
               plotOutput("plot_feridos_gr")),
      tabPanel("Mortes",
               plotOutput("plot_mortes")),
      tabPanel("Mortes posteriores",
               plotOutput("plot_morte_post")),
      tabPanel("Fatais",
               plotOutput("plot_fatais"))
    ),
    
    navbarMenu(
      "Análises para o relatório",
      tabPanel("Total de acidentes nos últimos anos", plotOutput("analise_1")),
      tabPanel(
        "Total de vítimas fatais nos últimos anos",
        plotOutput("analise_2")
      ),
      tabPanel(
        "Tipos de acidentes e sua frequência durante o dia",
        plotOutput("analise_4")
      ),
      tabPanel(
        "Tipos de acidentes e sua frequência durante a noite",
        plotOutput("analise_5")
      ),
      tabPanel("Acidentes fatais por período do dia", plotOutput("analise_3")),
      tabPanel("Vítimas fatais por período do dia", plotOutput("analise_6"))
    )
  )


server <- function(input, output, session) {
  # Alterar valores dinâmicamentes da coluna informações
  observe({
    if (input$periodos <= "2012") {
      updateSelectInput(session,
                        "informacoes",
                        choices = informacoes_sem_fer_gr)
      
      date_start <- as.Date(paste0(input$periodos, "-01-01"))
      date_end <- as.Date(paste0(input$periodos, "-02-01"))
      updateDateRangeInput(session,
                           "date_range",
                           start = date_start,
                           end = date_end)
    } else{
      updateSelectInput(session,
                        "informacoes",
                        choices = informacoes_com_fer_gr)
      
      date_start <- as.Date(paste0(input$periodos, "-01-01"))
      date_end <- as.Date(paste0(input$periodos, "-02-01"))
      updateDateRangeInput(session,
                           "date_range",
                           start = date_start,
                           end = date_end)
    }
  })
  
  # Filtra o dataset de acordo com os parâmetros selecionados
  filteredDate <- reactive({
    switch (
      input$periodos,
      "2010" = poa <- poa_2010,
      "2011" = poa <- poa_2011,
      "2012" = poa <- poa_2012,
      "2013" = poa <- poa_2013,
      "2014" = poa <- poa_2014,
      "2015" = poa <- poa_2015,
      "2016" = poa <- poa_2016
    )
    
    # Selecionar período
    poa <- poa %>% filter(DATA >= input$date_range[1] &
                            DATA <= input$date_range[2])
    
    # Selecionar Tipos de acidentes
    if (input$tipos_acidentes != "Selecione") {
      poa <- poa %>% filter(TIPO_ACID == input$tipos_acidentes)
    } else {
      poa <- poa %>% filter(TIPO_ACID != input$tipos_acidentes)
    }
    
    if (input$periodo_dia != "Selecione") {
      poa <- poa %>% filter(NOITE_DIA == input$periodo_dia)
    }
    
    switch (
      input$informacoes,
      "TIPO_ACID" = poa,
      "FERIDOS" = poa <- poa %>% filter(
        FERIDOS >= input$filtro_tipos[1] &
          FERIDOS <= input$filtro_tipos[2]
      ),
      "FERIDOS_GR" = poa <- poa %>% filter(
        FERIDOS_GR >= input$filtro_tipos[1] &
          FERIDOS_GR <= input$filtro_tipos[2]
      ),
      "MORTES" = poa <- poa %>% filter(
        MORTES >= input$filtro_tipos[1] &
          MORTES <= input$filtro_tipos[2]
      ),
      "MORTE_POST" = poa <- poa %>% filter(
        MORTE_POST >= input$filtro_tipos[1] &
          MORTE_POST <= input$filtro_tipos[2]
      ),
      "FATAIS" = poa <- poa %>% filter(
        FATAIS >= input$filtro_tipos[1] &
          FATAIS <= input$filtro_tipos[2]
      )
    )
    
  })
  
  filteredDateAndBounds <- reactive({
    data <- filteredDate()
    if (input$filter_bounds) {
      if (is.null(input$map_bounds))
        return(data)
      
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      data <-
        subset(
          data,
          LATITUDE >= latRng[1] & LATITUDE <= latRng[2] &
            LONGITUDE >= lngRng[1] &
            LONGITUDE <= lngRng[2]
        )
    }
    return(data)
  })
  
  # Tabela com os dados
  output$table_poa <- renderDataTable({
    filteredDateAndBounds()
  })
  
  # Histogramas
  output$plot_feridos <- renderPlot({
    hist(
      filteredDateAndBounds()$FERIDOS,
      col = "#75AADB",
      breaks = 10,
      main = "Frequência de feridos por acidente",
      xlab = "Feridos",
      ylab = "Frequência",
      border = "white"
    )
  })
  
  output$plot_feridos_gr <- renderPlot({
    if (input$periodos > "2012") {
      hist(
        filteredDateAndBounds()$FERIDOS_GR,
        col = "#75AADB",
        breaks = 10,
        main = "Frequência de feridos gravemente por acidente",
        xlab = "Feridos gravemente",
        ylab = "Frequência",
        border = "white"
      )
    }
  })
  
  output$plot_mortes <- renderPlot({
    hist(
      filteredDateAndBounds()$MORTES,
      col = "#75AADB",
      breaks = 10,
      main = "Frequência de mortes que ocorreram na hora do acidente",
      xlab = "Feridos gravemente",
      ylab = "Frequência",
      border = "white"
    )
  })
  
  output$plot_morte_post <- renderPlot({
    hist(
      filteredDateAndBounds()$MORTE_POST,
      col = "#75AADB",
      breaks = 10,
      main = "Frequência de mortes que ocorreram após internação",
      xlab = "Feridos gravemente",
      ylab = "Frequência",
      border = "white"
    )
  })
  
  output$plot_fatais <- renderPlot({
    hist(
      filteredDateAndBounds()$FATAIS,
      col = "#75AADB",
      breaks = 10,
      main = "Frequência de mortes totais por acidente",
      xlab = "Feridos gravemente",
      ylab = "Frequência",
      border = "white"
    )
  })
  
  
  ############################################################################
  # Análises
  #############################################################################
  # O número total de acidentes aumentou de 2013 até 2016?
  #############################################################################
  output$analise_1 <- renderPlot({
    ano = c("2013", "2014", "2015", "2016")
    total_acidentes = c(dim(poa_2013)[1],
                        dim(poa_2014)[1],
                        dim(poa_2015)[1],
                        dim(poa_2016)[1])
    df_total = data.frame(ano, total_acidentes)
    colnames(df_total) <- c("Ano", "Total")
    
    ggplot(data = df_total, aes(x = Ano, y = Total)) +
      geom_bar(stat = "identity", fill = "#ff9900") +
      geom_text(
        aes(label = Total),
        vjust = 1.6,
        color = "white",
        size = 5
      ) +
      ggtitle("O número total de acidentes nos últimos 4 anos em Porto Alegre") +
      theme_minimal()
  })
  
  #############################################################################
  # O número total de acidentes com vítimas fatais aumentou de 2013 até 2016?
  #############################################################################
  output$analise_2 <- renderPlot({
    ano = c("2013", "2014", "2015", "2016")
    f_2013 <- poa_2013 %>% filter(FATAIS > 0)
    f_2014 <- poa_2014 %>% filter(FATAIS > 0)
    f_2015 <- poa_2015 %>% filter(FATAIS > 0)
    f_2016 <- poa_2016 %>% filter(FATAIS > 0)
    
    total_acidentes_fatais = c(dim(f_2013)[1], dim(f_2014)[1], dim(f_2015)[1], dim(f_2016)[1])
    df = data.frame(ano, total_acidentes_fatais)
    colnames(df) <- c("Ano", "Fatais")
    
    ggplot(data = df, aes(x = Ano, y = Fatais)) +
      geom_bar(stat = "identity", fill = "#FF6666") +
      geom_text(
        aes(label = Fatais),
        vjust = 1.6,
        color = "white",
        size = 5
      ) +
      ggtitle("O número de mortes fatais em acidentes de trânsito nos últimos 4 anos em Porto Alegre") +
      theme_minimal()
  })
  
  
  #############################################################################
  # Há mais acidentes durante o dia ou a noite?
  #############################################################################
  output$analise_3 <- renderPlot({
    ac_2013 <- poa_2013 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    ac_2014 <- poa_2014 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    ac_2015 <- poa_2015 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    ac_2016 <- poa_2016 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    acn1 <- append(ac_2013$Total, ac_2014$Total)
    acn2 <- append(acn1, ac_2015$Total)
    acn3 <- append(acn2, ac_2016$Total)
    
    anos_ac_fatais = c(rep("2013" , 2),
                       rep("2014" , 2),
                       rep("2015" , 2) ,
                       rep("2016" , 2))
    tipos_ac_fatais = rep(ac_2016$NOITE_DIA, 4)
    valores_ac_fatais = c(acn3)
    data_ac_fatais = data.frame(anos_ac_fatais, tipos_ac_fatais, valores_ac_fatais)
    ggplot(
      data_ac_fatais,
      aes(fill = tipos_ac_fatais, y = valores_ac_fatais, x = anos_ac_fatais)
    ) +
      ggtitle("Número de acidentes vínculados com o período do dia.") +
      labs(x = "Anos") +
      labs(y = "Total de acidentes por tipo de acidente") +
      labs(fill = "Período do dia") +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(
        aes(label = valores_ac_fatais),
        vjust = 1.6,
        color = "white",
        size = 6
      ) +
      facet_wrap( ~ tipos_ac_fatais)
    
  })
  
  #############################################################################
  # Qual o tipo de acidente com vítima fatal ocorre com mais frequencia durante o dia?
  #############################################################################
  output$analise_4 <- renderPlot({
    tipos_acidentes_2013 <- poa_2013 %>%
      filter(TIPO_ACID != 'NAO CADASTRADO' &
               NOITE_DIA == 'DIA') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    tipos_acidentes_2014 <- poa_2014 %>%
      filter(NOITE_DIA == 'DIA') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    tipos_acidentes_2015 <- poa_2015 %>%
      filter(NOITE_DIA == 'DIA') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    tipos_acidentes_2016 <- poa_2016 %>%
      filter(NOITE_DIA == 'DIA') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    p1 <-
      append(tipos_acidentes_2013$Fatais,
             tipos_acidentes_2014$Fatais)
    p2 <- append(p1, tipos_acidentes_2015$Fatais)
    p3 <- append(p2, tipos_acidentes_2016$Fatais)
    
    anos = c(rep("2013" , 9),
             rep("2014" , 9),
             rep("2015" , 9) ,
             rep("2016" , 9))
    tipos = rep(tipos_acidentes_2016$TIPO_ACID, 4)
    valores = c(p3)
    data = data.frame(anos, tipos, valores)
    ggplot(data, aes(fill = tipos, y = valores, x = anos)) +
      ggtitle("Número de mortes por tipo de acidente durante o período do dia nos últimos 4 anos") +
      labs(x = "Anos") +
      labs(y = "Total de vítimas fatais por tipo de acidente") +
      labs(fill = "Tipos de acidentes") +
      geom_bar(position = "dodge", stat = "identity")
  })
  
  #############################################################################
  # Qual o tipo de acidente com vítima fatal ocorre com mais frequencia durante
  # o período da noite?
  #############################################################################
  output$analise_5 <- renderPlot({
    tipos_acidentes_noite_2013 <- poa_2013 %>%
      filter(TIPO_ACID != 'NAO CADASTRADO' &
               NOITE_DIA == 'NOITE') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    tipos_acidentes_noite_2013 <-
      insertRow(tipos_acidentes_noite_2013, c('INCENDIO', 0, 0), 7)
    tipos_acidentes_noite_2013$Fatais <-
      as.numeric(tipos_acidentes_noite_2013$Fatais)
    
    tipos_acidentes_noite_2014 <- poa_2014 %>%
      filter(NOITE_DIA == 'NOITE') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    tipos_acidentes_noite_2015 <- poa_2015 %>%
      filter(NOITE_DIA == 'NOITE') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    tipos_acidentes_noite_2016 <- poa_2016 %>%
      filter(NOITE_DIA == 'NOITE') %>%
      group_by(TIPO_ACID) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    tipos_acidentes_noite_2016 <-
      insertRow(tipos_acidentes_noite_2016, c('INCENDIO', 0, 0), 7)
    tipos_acidentes_noite_2016$Fatais <-
      as.numeric(tipos_acidentes_noite_2016$Fatais)
    
    pn1 <-
      append(tipos_acidentes_noite_2013$Fatais,
             tipos_acidentes_noite_2014$Fatais)
    pn2 <- append(pn1, tipos_acidentes_noite_2015$Fatais)
    pn3 <- append(pn2, tipos_acidentes_noite_2016$Fatais)
    
    anos_noite = c(rep("2013" , 9),
                   rep("2014" , 9),
                   rep("2015" , 9) ,
                   rep("2016" , 9))
    tipos_noite = rep(tipos_acidentes_noite_2015$TIPO_ACID, 4)
    valores_noite = c(pn3)
    data_noite = data.frame(anos_noite, tipos_noite, valores_noite)
    data_noite <- data_noite[order(anos_noite, tipos_noite), ]
    ggplot(data_noite,
           aes(fill = tipos_noite, y = valores_noite, x = anos_noite)) +
      ggtitle("Número de mortes por tipo de acidente durante o período da noite nos últimos 4 anos") +
      labs(x = "Anos") +
      labs(y = "Total de vítimas fatais por tipo de acidente") +
      labs(fill = "Tipos de acidentes") +
      geom_bar(position = "dodge", stat = "identity")
  })
  
  #############################################################################
  # Há mais vítimas fatais durante o dia ou a noite?
  #############################################################################
  output$analise_6 <- renderPlot({
    fatais_2013 <- poa_2013 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    fatais_2014 <- poa_2014 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    fatais_2015 <- poa_2015 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    fatais_2016 <- poa_2016 %>%
      group_by(NOITE_DIA) %>%
      summarise(Total = n(),
                Fatais = sum(FATAIS))
    
    vn1 <- append(fatais_2013$Fatais, fatais_2014$Fatais)
    vn2 <- append(vn1, fatais_2015$Fatais)
    vn3 <- append(vn2, fatais_2016$Fatais)
    
    anos_v_fatais = c(rep("2013" , 2),
                      rep("2014" , 2),
                      rep("2015" , 2) ,
                      rep("2016" , 2))
    tipos_v_fatais = rep(fatais_2016$NOITE_DIA, 4)
    valores_v_fatais = c(vn3)
    data_v_fatais = data.frame(anos_v_fatais, tipos_v_fatais, valores_v_fatais)
    ggplot(data_v_fatais,
           aes(fill = tipos_v_fatais, y = valores_v_fatais, x = anos_v_fatais)) +
      ggtitle("Número de mortes por acidente vínculados com o período do dia.") +
      labs(x = "Anos") +
      labs(y = "Total de vítimas fatais por tipo de acidente") +
      labs(fill = "Período do dia") +
      geom_bar(position = "dodge", stat = "identity") +
      geom_text(
        aes(label = valores_v_fatais),
        vjust = 1.6,
        color = "white",
        size = 6
      ) +
      facet_wrap( ~ tipos_v_fatais)
  })
  
  #############################################################################
  # Mapa de Porto Alegre
  #############################################################################
  output$map <- renderLeaflet({
    leaflet(data = poa) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
      fitBounds(~ min(LONGITUDE),
                ~ min(LATITUDE),
                ~ max(LONGITUDE),
                ~ max(LATITUDE)) %>%
      addLayersControl(baseGroups = c("Light", "Dark"),
                       overlayGroups = c("Heatmap"))
  })
  
  
  #############################################################################
  # Histograma auxliar
  #############################################################################
  output$linechart <- renderPlot({
    plot <- filteredDateAndBounds() %>%
      ggplot(aes(x = DATA)) +
      scale_y_log10() +
      labs(x = "Data do Acidente", y = "Nº de Acidentes") +
      theme(legend.text = element_text(size = 8),
            legend.title = element_blank())
    
    if (input$informacoes == "FERIDOS") {
      plot <- plot +
        theme_dark() +
        geom_histogram(aes(fill = FCT_FERIDOS)) +
        scale_fill_manual(values = scale_color_quant)
    }
    else if (input$informacoes == "FERIDOS_GR") {
      plot <- plot +
        theme_dark() +
        geom_histogram(aes(fill = FCT_FERIDOS_GR)) +
        scale_fill_manual(values = scale_color_quant)
    }
    else if (input$informacoes == "MORTES") {
      plot <- plot +
        theme_dark() +
        geom_histogram(aes(fill = FCT_MORTES)) +
        scale_fill_manual(values = scale_color_quant)
    }
    else if (input$informacoes == "MORTE_POST") {
      plot <- plot +
        theme_dark() +
        geom_histogram(aes(fill = FCT_MORTE_POST)) +
        scale_fill_manual(values = scale_color_quant)
    }
    else if (input$informacoes == "FATAIS") {
      plot <- plot +
        theme_dark() +
        geom_histogram(aes(fill = FCT_FATAIS)) +
        scale_fill_manual(values = scale_color_quant)
    } else{
      plot <- plot +
        geom_histogram(aes(fill = TIPO_ACID)) +
        scale_fill_manual(values = acid_color_scale, limits = acid_types)
    }
    
    plot
  })
  
  observe({
    proxy <- leafletProxy("map") %>%
      clearHeatmap() %>%
      addHeatmap(
        data = filteredDate(),
        lng = ~ LONGITUDE,
        lat = ~ LATITUDE,
        blur = input$blur,
        radius = input$radius,
        group = "Heatmap"
      )
  })
  
  observe({
    if (input$informacoes == "TIPO_ACID") {
      pal <- colorFactor(acid_color_scale,
                         domain = filteredDate()$TIPO_ACID)
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      if (input$show_points) {
        proxy %>%
          addTiles() %>%
          addCircles(
            color = ~ pal(TIPO_ACID),
            data = filteredDate(),
            lng = ~ LONGITUDE,
            lat = ~ LATITUDE,
            weight = 10,
            opacity = 1,
            stroke = TRUE,
            group = "Points"
          ) %>%
          addLegend(
            "topright",
            pal = pal,
            values = filteredDate()$TIPO_ACID,
            title = "Tipos de acidentes",
            opacity = 1
          )
      }
    } else if (input$informacoes == "FERIDOS") {
      pal <- colorFactor(scale_color_quant,
                         domain = filteredDate()$FCT_FERIDOS)
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      if (input$show_points) {
        proxy %>%
          addTiles() %>%
          addCircles(
            color = ~ pal(FCT_FERIDOS),
            data = filteredDate(),
            lng = ~ LONGITUDE,
            lat = ~ LATITUDE,
            weight = 10,
            opacity = 1,
            stroke = TRUE,
            group = "Points"
          ) %>%
          addLegend(
            "topright",
            pal = pal,
            values = filteredDate()$FCT_FERIDOS,
            title = "Total de feridos no acidente",
            opacity = 1
          )
      }
    } else if (input$informacoes == "FERIDOS_GR") {
      pal <- colorFactor(scale_color_quant,
                         domain = filteredDate()$FCT_FERIDOS_GR)
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      if (input$show_points) {
        proxy %>%
          addTiles() %>%
          addCircles(
            color = ~ pal(FCT_FERIDOS_GR),
            data = filteredDate(),
            lng = ~ LONGITUDE,
            lat = ~ LATITUDE,
            weight = 10,
            opacity = 1,
            stroke = TRUE,
            group = "Points"
          ) %>%
          addLegend(
            "topright",
            pal = pal,
            values = filteredDate()$FCT_FERIDOS_GR,
            title = "Total de feridos gravimente no acidente",
            opacity = 1
          )
      }
    } else if (input$informacoes == "MORTES") {
      pal <- colorFactor(scale_color_quant,
                         domain = filteredDate()$FCT_MORTES)
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      if (input$show_points) {
        proxy %>%
          addTiles() %>%
          addCircles(
            color = ~ pal(FCT_MORTES),
            data = filteredDate(),
            lng = ~ LONGITUDE,
            lat = ~ LATITUDE,
            weight = 10,
            opacity = 1,
            stroke = TRUE,
            group = "Points"
          ) %>%
          addLegend(
            "topright",
            pal = pal,
            values = filteredDate()$FCT_MORTES,
            title = "Total de mortes ocorridos no momento do acidente",
            opacity = 1
          )
      }
    } else if (input$informacoes == "MORTE_POST") {
      pal <- colorFactor(scale_color_quant,
                         domain = filteredDate()$FCT_MORTE_POST)
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      if (input$show_points) {
        proxy %>%
          addTiles() %>%
          addCircles(
            color = ~ pal(FCT_MORTE_POST),
            data = filteredDate(),
            lng = ~ LONGITUDE,
            lat = ~ LATITUDE,
            weight = 10,
            opacity = 1,
            stroke = TRUE,
            group = "Points"
          ) %>%
          addLegend(
            "topright",
            pal = pal,
            values = filteredDate()$FCT_MORTE_POST,
            title = "Total de mortes que ocorreram após internação",
            opacity = 1
          )
      }
    } else if (input$informacoes == "FATAIS") {
      pal <- colorFactor(scale_color_quant,
                         domain = filteredDate()$FCT_FATAIS)
      proxy <- leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      if (input$show_points) {
        proxy %>%
          addTiles() %>%
          addCircles(
            color = ~ pal(FCT_FATAIS),
            data = filteredDate(),
            lng = ~ LONGITUDE,
            lat = ~ LATITUDE,
            weight = 10,
            opacity = 1,
            stroke = TRUE,
            group = "Points"
          ) %>%
          addLegend(
            "topright",
            pal = pal,
            values = filteredDate()$FCT_FATAIS,
            title = "Total de vítimas fatais",
            opacity = 1
          )
      }
    }
  })
}
shinyApp(ui = ui, server = server)
