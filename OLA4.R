###############################################
# OLA4 – Bilbasen + SQL
# Land Rover Range Rover Sport, diesel
###############################################

library(DBI)
library(RMariaDB)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(httr)

################################################
# Opgave 1.1 – Hente data fra Bilbasen
################################################

startlink <- paste0(
  "https://www.bilbasen.dk/brugt/bil/land_rover/range_rover_sport?",
  "fuel=2&includeengroscvr=true&includeleasing=false&pagesize=200"
)

rawres     <- GET(url = startlink)
rawcontent <- content(rawres, as = "text")
page       <- read_html(rawcontent)

carlist <- page %>% html_elements("article")
length(carlist)  # antal annoncer på siden

# Felter vi skal bruge
ptag      <- "[class^='Listing_price']"
proptag   <- "[class^='Listing_properties']"
maketag   <- "[class^='Listing_makeModel']"
detailtag <- "[class^='Listing_details']"
desctag   <- "[class^='Listing_description']"
loctag    <- "[class^='Listing_location']"

# Her samler vi alle annoncerne
colldf <- tibble()

for (car in carlist) {
  tryCatch({
    # Info fra listesiden
    price       <- car %>% html_element(ptag)       %>% html_text(trim = TRUE)
    props       <- car %>% html_element(proptag)    %>% html_text(trim = TRUE)
    makemodel   <- car %>% html_element(maketag)    %>% html_text(trim = TRUE)
    details     <- car %>% html_elements(detailtag) %>% html_text(trim = TRUE) %>% paste0(collapse = "-")
    description <- car %>% html_elements(desctag)   %>% html_text(trim = TRUE) %>% paste0(collapse = " ")
    location    <- car %>% html_elements(loctag)    %>% html_text(trim = TRUE) %>% paste0(collapse = " ")
    
    # Link til bil
    link_raw <- car %>% html_element("a") %>% html_attr("href")
    if (is.na(link_raw)) next
    
    if (grepl("^https?://", link_raw)) {
      link <- link_raw
    } else {
      link <- paste0("https://www.bilbasen.dk", link_raw)
    }
    
    # Bilbasen-id fra URL
    carid <- str_extract(link, "[0-9]{6,7}")
    
    # Hent detaljer
    car_res <- GET(
      url = link,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124 Safari/537.36")
    )
    if (status_code(car_res) != 200) stop("HTTP-fejl på detaljeside")
    
    car_html <- content(car_res, "text", encoding = "UTF-8")
    car_page <- read_html(car_html)
    
    # Forhandlernavn: første h2 der ikke er tekniske overskrifter
    h2_texts <- car_page %>%
      html_elements("h2.bas-MuiTypography-h3") %>%
      html_text(trim = TRUE)
    
    seller_name <- h2_texts[!h2_texts %in% c(
      "Detaljer", "Beskrivelse", "Generelle modeloplysninger*",
      "Udstyr og tilbehør", "Finansiering"
    )][1]
    
    # Adresse
    seller_address <- car_page %>%
      html_element("[data-e2e='seller-address'], .bas-MuiSellerInfoComponent-address") %>%
      html_text(trim = TRUE) %>%
      str_squish()
    
    # CVR
    cvr_text <- car_page %>%
      html_element("p.bas-MuiSellerInfoComponent-cvr") %>%
      html_text(trim = TRUE)
    seller_cvr <- str_extract(cvr_text, "\\d+")
    
    # Saml alt i 1 række
    tmpdf <- tibble(
      price, details, makemodel, props, description, location,
      link, carid,
      seller_name, seller_address, seller_cvr
    )
    
    colldf <- bind_rows(colldf, tmpdf)
    
    # Lille pause så vi ikke spammer Bilbasen
    Sys.sleep(0.4)
  },
  error = function(e) {
    message("Fejl på én bil: ", e$message)
  })
}

nrow(colldf)

################################################
# Opgave 1.2 – Rense data
################################################

colldf <- colldf %>%
  mutate(
    # Rens beskrivelsen
    description = description %>%
      str_replace_all("[\r\n]+", ". ") %>%                # newline → ". "
      str_replace_all("[^0-9A-Za-zÆØÅæøå., ]+", " ") %>%  # kun bogstaver, tal, punktum, komma, mellemrum
      str_squish(),                                       # fjern dobbelte mellemrum
    scrapedate = Sys.Date()                               # første kørsel: dagens dato
  )

################################################
# Opgave 1.3 – Hente nye data (simuleret kørsel)
################################################

# Kørsel 1 (dag 1) er colldf
old_df <- colldf

# Kørsel 2 (dag 2) – vi lægger én dag til
new_df_full <- old_df %>%
  mutate(scrapedate = scrapedate + 1)

# Tilføj 2 nye biler til kørsel 2
new_cars_full <- tibble(
  price          = c("599.900 kr.", "649.900 kr."),
  details        = c("Ny bil 1", "Ny bil 2"),
  makemodel      = c("RR Sport 3.0 SDV6", "RR Sport 4.4 SDV8"),
  props          = c("2018", "2019"),
  description    = c("Ny bil tilføjet", "Ny bil tilføjet"),
  location       = c("København", "Aarhus"),
  link           = c("https://www.bilbasen.dk/ny1", "https://www.bilbasen.dk/ny2"),
  carid          = c("900001", "900002"),
  seller_name    = c("Ny Forhandler 1", "Ny Forhandler 2"),
  seller_address = c("Testvej 1, 2100 KBH", "Testvej 2, 8000 Aarhus"),
  seller_cvr     = c("11111111", "22222222"),
  scrapedate     = max(new_df_full$scrapedate)           # samme dato som kørsel 2
)

# Vælg 3 tilfældige biler til prisændring
set.seed(1)
change_ids <- sample(old_df$carid, 3)

new_df_full <- new_df_full %>%
  mutate(
    price = if_else(
      carid %in% change_ids,
      paste0(as.integer(str_remove_all(price, "\\D")) + 10000, " kr."),  # +10.000 kr.
      as.character(price)
    )
  )

# Fjern 5 biler (simuleret som solgt – de er væk i kørsel 2)
remove_ids <- sample(old_df$carid, 5)

new_df_full <- new_df_full %>%
  filter(!carid %in% remove_ids)

# Fuldt datasæt for kørsel 2 (inkl. nye biler)
new_full <- bind_rows(new_df_full, new_cars_full)

# Slim version til car_observation (matcher SQL-tabellen)
new_scrape <- new_full %>%
  transmute(
    carid,
    price,
    description_clean = description,
    location,
    scrapedate,
    sold = 0L
  )

################################################
# OPGAVE 2.1 – SQL
################################################

#Her opretter vi via SQL
#Bilbasen_scheme.sql

################################################
# Opgave 2.2 – Gemme første kørsel i databasen
################################################
# (2.1 – selve skemaet/DDL ligger i .sql-filen, se længere nede)

# Forbindelse til MariaDB (lokal)
con <- dbConnect(
  MariaDB(),
  host     = "localhost",
  port     = 3306,
  user     = "root",
  password = "Timmtimm",
  dbname   = "Bilbasen"
)

# Split kørsel 1 (dag 1) op i de tre tabeller:
# dealer (invariant), car (invariant), car_observation (tidsserie)

dealer_table <- colldf %>%
  transmute(
    dealer_id     = carid,  
    seller_name,
    seller_address,
    seller_cvr
  ) %>%
  distinct()

car_table <- colldf %>%
  transmute(
    carid,
    makemodel,
    props,
    link,
    dealer_id = carid
  ) %>%
  distinct()

obs_table <- colldf %>%
  transmute(
    carid,
    price,
    description_clean = description,
    location,
    scrapedate,
    sold = 0L
  )

# Indsæt første kørsel i databasen
dbWriteTable(con, "dealer",          dealer_table, append = TRUE, row.names = FALSE)
dbWriteTable(con, "car",             car_table,    append = TRUE, row.names = FALSE)
dbWriteTable(con, "car_observation", obs_table,    append = TRUE, row.names = FALSE)

################################################
# Opgave 2.3 – Opdatere databasen ud fra kørsel 2
################################################

# Hent alle observationer fra databasen
old_obs <- dbReadTable(con, "car_observation")

# Seneste dato i databasen = kørsel 1
last_date <- max(as.Date(old_obs$scrapedate))

old_obs_last <- old_obs %>%
  filter(as.Date(scrapedate) == last_date)

# Kørsel 2 fra R
new_obs <- new_scrape   

# Nye biler = findes i new_obs men ikke i old_obs_last
new_ids <- setdiff(new_obs$carid, old_obs_last$carid)

# Solgte biler = fandtes i old_obs_last, men er væk i new_obs
sold_ids <- setdiff(old_obs_last$carid, new_obs$carid)

# Prisændringer: samme carid i begge, men forskellig pris
price_changes <- old_obs_last %>%
  inner_join(new_obs, by = "carid", suffix = c("_old", "_new")) %>%
  filter(price_old != price_new)

### A) Nye biler → dealer, car, car_observation

if (length(new_ids) > 0) {
  new_cars_full2 <- new_full %>%
    filter(carid %in% new_ids)
  
  # Tjek hvilke dealer_id der allerede findes
  existing_dealers <- dbGetQuery(con, "SELECT dealer_id FROM dealer")$dealer_id
  
  dealer_new <- new_cars_full2 %>%
    transmute(
      dealer_id     = carid,
      seller_name,
      seller_address,
      seller_cvr
    ) %>%
    distinct() %>%
    filter(!(dealer_id %in% existing_dealers))
  
  if (nrow(dealer_new) > 0) {
    dbWriteTable(con, "dealer", dealer_new, append = TRUE, row.names = FALSE)
  }
  
  # Nye biler til car
  car_new <- new_cars_full2 %>%
    transmute(
      carid,
      makemodel,
      props,
      link,
      dealer_id = carid
    ) %>%
    distinct()
  
  dbWriteTable(con, "car", car_new, append = TRUE, row.names = FALSE)
  
  # Første observation for de nye biler
  obs_new <- new_cars_full2 %>%
    transmute(
      carid,
      price,
      description_clean = description,
      location,
      scrapedate,
      sold = 0L
    )
  
  dbWriteTable(con, "car_observation", obs_new, append = TRUE, row.names = FALSE)
}

### B) Prisændringer og ny række i car_observation

if (nrow(price_changes) > 0) {
  price_ids <- price_changes$carid
  
  price_cars_full <- new_full %>%
    filter(carid %in% price_ids)
  
  obs_price <- price_cars_full %>%
    transmute(
      carid,
      price,
      description_clean = description,
      location,
      scrapedate,
      sold = 0L
    )
  
  dbWriteTable(con, "car_observation", obs_price, append = TRUE, row.names = FALSE)
}

### C) Solgte biler

if (length(sold_ids) > 0) {
  obs_all <- dbReadTable(con, "car_observation")
  
  for (cid in sold_ids) {
    # Find seneste scrapedate for den bil i R
    last_scrapedate <- obs_all %>%
      filter(carid == cid) %>%
      summarise(last_date = max(as.Date(scrapedate))) %>%
      pull(last_date)
    
    # Opdater
    dbExecute(
      con,
      "
      UPDATE car_observation
      SET sold = 1
      WHERE carid = ?
        AND scrapedate = ?
      ",
      params = list(cid, last_scrapedate)
    )
  }
}


dbDisconnect(con)



##################################################
# Opgave 3 – OpenSky
##################################################

library(tidyverse)
library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
library(leaflet)

# Hjælpefunktioner 
source("~/Documents/Dataanalyse/Uge 45/util.r", encoding = "UTF-8")

baseurl   <- "https://opensky-network.org/api"
endpointS <- "/states/all"
endpointT <- "/tracks/all"

queryS <- paste0(baseurl, endpointS)
queryT <- paste0(baseurl, endpointT)


##################################################
# Opgave 3.1 – Hent fly over Nordsøen + barplot
##################################################


# NORDSØEN – bounding box
lamin <- 51
lamax <- 61
lomin <- -4.42
lomax <- 8.36

# URL til states/all med bounding box
fullurl <- paste0(
  baseurl, endpointS,
  "?lamin=", lamin,
  "&lomin=", lomin,
  "&lamax=", lamax,
  "&lomax=", lomax
)

# Hjælpeplots (bruges senere i 3.2 + 3.3)
myMapPlot <- function(df) {
  leaflet(df) %>%
    addTiles() %>%
    addPolylines(lat = ~lat, lng = ~lng)
}

myEDAPlots <- function(df) {
  par(mfcol = c(1, 3), ann = FALSE)
  plot(df$lat, df$lng)
  plot(df$crs, df$time)
  plot(df$alt, df$time)
}

# Hent states over Nordsøen
token <- getToken()
res   <- httr::GET(fullurl, add_headers(Authorization = paste("Bearer", token)))
rescontent <- httr::content(res, as = "text")
resretval  <- jsonlite::fromJSON(rescontent)
statedfpr  <- as.data.frame(resretval$states)

# Find alle icao24 og læg dem i en vektor (bruges i 3.2 + 3.3)
icaov <- statedfpr$V1

# Gør kolonnenavne mere læsbare til barplottet
colnames(statedfpr)[1:3] <- c("icao24", "callsign", "origin_country")

# Tæl antal fly pr. land
country_counts <- statedfpr %>%
  count(origin_country) %>%
  arrange(desc(n))

# Land med flest fly
top_country <- country_counts$origin_country[1]

# Titel til barplot
plot_title <- paste("Flest fly over Nordsøen kommer fra", top_country)

# Barplot: antal fly pr. oprindelsesland
ggplot(country_counts, aes(x = n, y = reorder(origin_country, n))) +
  geom_col() +
  labs(
    x = "Antal fly",
    y = "Oprindelsesland",
    title = plot_title
  ) +
  theme_minimal()


##################################################
# Opgave 3.2 – Ét frisk fly + ét træningsfly
##################################################

## 3.2.1 – Track for ét frisk fly over Nordsøen

ictest <- icaov[[1]]  # vælg første fly som test
token  <- getToken()
turl   <- paste0(baseurl, endpointT, "?icao24=", ictest, "&time=0")

res <- httr::GET(turl, add_headers(Authorization = paste("Bearer", token)))
res$status_code

resretval <- fromJSON(content(res, as = "text"))
statedf   <- as.data.frame(resretval)

colnv <- c(
  "icao24", "callsign", "startTime", "endTime",
  "time", "lat", "lng", "alt", "crs", "grd"
)
colnames(statedf) <- colnv

# Udforsk track for frisk fly
myEDAPlots(statedf)
myMapPlot(statedf)

## 3.2.2 – Track for ét cirklende træningsfly (fra rds-fil)

# Indlæs træningsdata (cirkulerende fly)
fl      <- list.files(path = "~/Desktop/train", pattern = "*rds", full.names = TRUE)
collist <- lapply(fl, readRDS)

# Eksempel: circMT.rds
testdf <- readRDS(file.path("~/Desktop/train", "circMT.rds"))

# Udforsk træningsfly
myEDAPlots(testdf)
myMapPlot(testdf)

par(mfrow = c(1, 1))
plot(testdf$lng, testdf$lat)
plot(testdf$time, testdf$crs)

# Beregn simple differencer på lat/lng (bruges til at se bevægelse)
dlatv        <- c(0, diff(testdf$lat))
dlnv         <- c(0, diff(testdf$lng))
testdf$dlat  <- dlatv
testdf$dlng  <- dlnv

# Tidsserie-plot af lat over tid
ggplot(testdf, aes(x = time, y = lat)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 35))

# Simple lineære modeller (til senere sammenligning)
cmodel <- lm(lat ~ lng + crs + alt, data = testdf)   # cirkulerende fly
nmodel <- lm(lat ~ lng + crs + alt, data = statedf)  # "normalt" fly
summary(cmodel)
summary(nmodel)

csd <- sd(testdf$crs)    # SD i kurs for træningsfly
nsd <- sd(statedf$crs)   # SD i kurs for frisk fly


##################################################
# Opgave 3.3 – Loop over alle fly + beregn metrics
##################################################

## 3.3.1 – Loop over friske fly fra Nordsøen

res_ny <- list()

for (icao in icaov) {
  cat("Henter track for", icao, "\n")
  
  turl <- paste0(baseurl, endpointT, "?icao24=", icao, "&time=0")
  res  <- httr::GET(turl, add_headers(Authorization = paste("Bearer", token)))
  
  if (res$status_code != 200) next  # spring over fejl
  
  txt <- httr::content(res, as = "text")
  df  <- try(fromJSON(txt), silent = TRUE)
  if (inherits(df, "try-error") || length(df) == 0) next
  
  df <- as.data.frame(df)
  if (nrow(df) < 10) next  # skip meget korte tracks
  
  colnames(df) <- c(
    "icao24", "callsign", "startTime", "endTime",
    "time", "lat", "lng", "alt", "crs", "grd"
  )
  
  df <- df %>% filter(!is.na(lat), !is.na(lng))
  
  crs_sd <- sd(df$crs, na.rm = TRUE)
  m      <- lm(lat ~ lng, data = df)
  r2     <- summary(m)$r.squared
  
  res_ny[[icao]] <- data.frame(
    icao24 = icao,
    crs_sd = crs_sd,
    r2     = r2
  )
}

fresh_metrics <- bind_rows(res_ny)

## 3.3.2 – Loop over alle træningsfly (rds-filer)

res_train <- list()

for (f in fl) {
  df <- readRDS(f)
  
  crs_sd <- sd(df$crs, na.rm = TRUE)
  m      <- lm(lat ~ lng, data = df)
  r2     <- summary(m)$r.squared
  
  fname <- basename(f)
  
  res_train[[fname]] <- data.frame(
    file   = fname,
    crs_sd = crs_sd,
    r2     = r2
  )
}

train_metrics <- bind_rows(res_train)

## 3.3.3 – Plot: SD(crs) vs R² for friske fly
# (du kan evt. tilføje træningsfly som ekstra lag/farve)

ggplot(fresh_metrics, aes(x = r2, y = crs_sd)) +
  geom_point() +
  labs(
    x = "R² for lm(lat ~ lng)",
    y = "SD(crs)",
    title = "Cirkulære fly kontra lineære fly"
  ) +
  theme_minimal()



## 3.4 Aim high - jeres egen algoritme
########################################
# Opgave 3.4 – Aim high: egen algoritme
########################################


#Vælg simple tærskler ud fra træningsflyene (som vi ved cirkler)

sd_thr <- quantile(train_metrics$crs_sd, 0.75, na.rm = TRUE)  # top 25 % mest svingende
r2_thr <- quantile(train_metrics$r2,     0.25, na.rm = TRUE)  # bund 25 % laveste R²


sd_thr
r2_thr

# 2) Anvend algoritmen på de friske Nordsø-fly
#    isOff = 1 hvis:
#    - kursen svinger mere end et typisk cirkelfly (crs_sd >= sd_thr)
#    - og ruten er mindre lineær end et typisk cirkelfly (r2 <= r2_thr)
fresh_metrics_flagged <- fresh_metrics %>%
  mutate(
    isOff = if_else(
      crs_sd >= sd_thr+20 & r2 <= r2_thr,
      1L,         # klassificeret som "cirkelfly"
      0L          # klassificeret som "normalt" fly
    )
  )

# 3) Træningsflyene markeres altid som cirklende (isOff = 1)
train_metrics_flagged <- train_metrics %>%
  mutate(
    isOff = 1L     # disse rds-fly ER vores cirkelfly-sandhed
  )

# 4) Saml alle resultater i én dataframe "a la nedenstående"
all_metrics <- bind_rows(
  fresh_metrics_flagged %>%
    mutate(source = "fresh") %>%       # friske fly fra Nordsøen
    rename(id = icao24),
  
  train_metrics_flagged %>%
    mutate(source = "train") %>%       # cirkel-træningsfly
    rename(id = file)
)

View(all_metrics)

# (valgfrit) Lille plot for at se fordelingen
ggplot(all_metrics, aes(x = r2, y = crs_sd, color = factor(isOff))) +
  geom_point() +
  labs(
    x = "R² (lm(lat ~ lng))",
    y = "SD(crs)",
    color = "isOff",
    title = "Cirkelfly (isOff = 1) vs. øvrige fly"
  ) +
  theme_minimal()
