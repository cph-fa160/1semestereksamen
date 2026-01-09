#Opgave 1.

DATALOG <- read.csv("~/Desktop/DATALOG.txt", header=FALSE)

Ny_Data <- DATALOG [3773:nrow(DATALOG),]

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


# Parametre ---------------------------------------------------------
baseline      <- 115
low_threshold <- 100  # person foran sensoren
high_threshold <- 110 # sensoren "fri" igen

# 1) Rens data + state-machine --------------------------------------
Ny_Data_state <- Ny_Data %>%
  mutate(
    Tid     = as.POSIXct(V1, format = "%Y.%m.%d %H:%M:%S"),
    Afstand = as.numeric(V2)
  ) %>%
  filter(Afstand <= 120) %>%          # fjern outliers > 120 cm
  arrange(Tid) %>%
  mutate(
    state_raw = case_when(
      Afstand < low_threshold    ~ "blocked",  # person foran
      Afstand >= high_threshold  ~ "clear",    # ingen person
      TRUE                       ~ NA_character_
    )
  ) %>%
  fill(state_raw, .direction = "downup") %>%   # udfyld huller
  mutate(
    state      = if_else(is.na(state_raw), "clear", state_raw),
    prev_state = lag(state, default = "clear"),
    event      = state == "clear" & prev_state == "blocked"  # PERSON REGISTRERET HER
  )

# Hvor mange personer er registreret?
antal_personer <- sum(Ny_Data_state$event)
antal_personer

events_df <- Ny_Data_state %>% filter(event)

# HERFRA bygger vi kun visuelt ovenpå Ny_Data_state -----------------

# 1) Aggreger pr. sekund (til flottere plot) -----------------------
Ny_Data_sec <- Ny_Data_state %>%
  mutate(Tid_sec = floor_date(Tid, unit = "second")) %>%
  group_by(Tid_sec) %>%
  summarise(
    mean_afstand = mean(Afstand),
    any_blocked  = any(state == "blocked"),  # der stod en person foran i dette sekund
    .groups      = "drop"
  )

# 2) Data til baseline-linje og spikes -----------------------------
plot_df <- Ny_Data_sec %>%
  mutate(
    y_line = baseline  # flad baseline-linje
  )

spike_df <- Ny_Data_sec %>%
  filter(any_blocked) %>%          # kun sekunder med person foran sensoren
  mutate(
    y_start = baseline,
    y_end   = mean_afstand         # gennemsnitlig afstand det sekund (lav = person tæt på)
  )

# 3) Flot spike-plot ------------------------------------------------
ggplot() +
  # diskret bånd omkring baseline (normalområde)
  annotate(
    "rect",
    xmin = min(plot_df$Tid_sec),
    xmax = max(plot_df$Tid_sec),
    ymin = baseline - 5,
    ymax = baseline + 5,
    fill = "#f3f4f6",
    alpha = 0.6
  ) +
  # flad baseline-linje
  geom_line(
    data = plot_df,
    aes(x = Tid_sec, y = y_line),
    linewidth = 0.7,
    colour = "#4b5563"
  ) +
  # lodrette spikes for sekunder med person
  geom_segment(
    data = spike_df,
    aes(x = Tid_sec, xend = Tid_sec,
        y = y_start,  yend = y_end),
    linewidth = 1.6,
    colour = "#b91c1c",
    lineend = "round"
  ) +
  geom_point(
    data = spike_df,
    aes(x = Tid_sec, y = y_end),
    size   = 2.8,
    colour = "#b91c1c"
  ) +
  # baseline som stiplet linje
  geom_hline(
    yintercept = baseline,
    linetype   = "dashed",
    colour     = "#9ca3af"
  ) +
  # vend y-aksen så lave afstande (person tæt på) bliver høje spikes
  scale_y_reverse() +
  labs(
    title    = "Sensorregistreringer over tid",
    subtitle = paste0("Outliers > 120 fjernet · ", antal_personer,
                      " registrerede personer (blocked → clear)"),
    x        = "Tid (sekunder)",
    y        = "Gennemsnitlig afstand (cm)",
    caption = "Kilde: Egen data indsamlet med HC-SR04 ultralydsafstandssensor (SD-kort-logning)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background    = element_rect(fill = "#f9fafb", colour = NA),
    panel.background   = element_rect(fill = "#f9fafb", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(colour = "#4b5563")
  )

#Opgave 2
###############################################
### FOODWASTE PIPELINE – ENDVERSION          ###
### Robust til SQL + API fejlhåndtering      ###
###############################################

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(DBI)
library(RMariaDB)

api_key <- Sys.getenv("SALLING_API_KEY")
run_timestamp <- Sys.time()

### 1) POSTNUMRE ###
postnumre <- c("3520", "7080", "1264", "1560")

########################################################
### 2) ROBUST API-FUNKTION (HÅNDTERER 400 / 500)      ###
########################################################

hent_foodwaste <- function(postnummer) {
  url <- "https://api.sallinggroup.com/v1/food-waste"
  
  res <- GET(
    url,
    query = list(zip = postnummer),
    add_headers(Authorization = paste("Bearer", api_key))
  )
  
  sc <- status_code(res)
  
  # 200 OK → Parse
  if (sc == 200) {
    txt <- content(res, as = "text", encoding = "UTF-8")
    
    if (txt == "" || txt == "null") {
      cat("Tom response ved ZIP:", postnummer, "\n")
      return(NULL)
    }
    
    parsed <- fromJSON(txt, flatten = TRUE)
    df <- as.data.frame(parsed)
    df$postnummer <- postnummer
    return(df)
  }
  
  # 4xx fejl → stop (critcal)
  if (sc >= 400 && sc < 500) {
    stop(paste("Fatal API fejl ved ZIP", postnummer, "status", sc,
               "- sandsynligvis ugyldig API-nøgle eller request."))
  }
  
  # 5xx fejl → skip
  if (sc >= 500) {
    cat("Serverfejl 500 ved ZIP", postnummer, "– springer over.\n")
    return(NULL)
  }
}

########################################################
### 3) HENT BUTIKKER                                   ###
########################################################

food_list <- lapply(postnumre, hent_foodwaste)
food_list <- Filter(Negate(is.null), food_list)   # fjern NULL
food_all  <- do.call(rbind, food_list)

########################################################
### 4) UNNEST + FILTER CLEARANCES                     ###
########################################################

valid <- sapply(food_all$clearances, function(x) {
  is.data.frame(x) && nrow(x) > 0
})

food_all_nonempty <- food_all[valid, ]

offers_all <- food_all_nonempty %>%
  tidyr::unnest(clearances) %>%
  mutate(timestamp_pipeline = run_timestamp)

########################################################
### 5) STAMDATA                                        ###
########################################################

stamdata <- food_all %>%
  select(
    store.id,
    store.brand,
    store.name,
    store.address.street,
    postnummer
  ) %>%
  distinct() %>%
  rename(
    store_id              = store.id,
    store_brand           = store.brand,
    store_name            = store.name,
    store_address_street  = store.address.street
  )


########################################################
### 6) VARIABLE (TILBUD) + DATO-KONVERTERING           ###
########################################################

variable <- offers_all %>%
  select(
    store.id,
    offer.ean,
    offer.startTime,
    offer.endTime,
    offer.newPrice,
    offer.originalPrice,
    offer.percentDiscount,
    offer.discount,
    offer.lastUpdate,
    offer.stock,
    product.ean,
    product.description,
    product.categories.da,
    timestamp_pipeline
  ) %>%
  mutate(
    offer.startTime = ymd_hms(offer.startTime, tz = "UTC"),
    offer.endTime   = ymd_hms(offer.endTime,   tz = "UTC"),
    offer.lastUpdate = ymd_hms(offer.lastUpdate, tz = "UTC"),
    snapshot_date = as.Date(timestamp_pipeline)
  ) %>%
  rename(
    store_id              = store.id,
    offer_ean             = offer.ean,
    offer_startTime       = offer.startTime,
    offer_endTime         = offer.endTime,
    offer_newPrice        = offer.newPrice,
    offer_originalPrice   = offer.originalPrice,
    offer_percentDiscount = offer.percentDiscount,
    offer_discount        = offer.discount,
    offer_lastUpdate      = offer.lastUpdate,
    offer_stock           = offer.stock,
    product_ean           = product.ean,
    product_description   = product.description,
    product_categories_da = product.categories.da
  )

########################################################
### 7) SQL FORBINDELSE                                ###
########################################################

con <- dbConnect(
  MariaDB(),
  user = "root",
  password = "Ucg95fpr!",
  dbname = "foodwaste",
  host = "localhost"
)

########################################################
###  UPLOAD STAMDATA (KUN FØRSTE GANG)              ###
########################################################

rows <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM store_static")$n

if (rows == 0) {
  cat("Uploader stores første gang...\n")
  dbWriteTable(con, "store_static", stamdata, append = TRUE, row.names = FALSE)
} else {
  cat("Springer upload af stores over – findes allerede.\n")
}

########################################################
### 9) UPLOAD OFFERS (HVER GANG)                      ###
########################################################

dbWriteTable(con, "offer_variable", variable, append = TRUE, row.names = FALSE)

dbDisconnect(con)

cat("\n--- FOODWASTE PIPELINE KØRT FÆRDIGT ---\n")
cat("Timestamp:", run_timestamp, "\n")
api.sallinggroup.com

#SQL
CREATE TABLE store_static (
  store_id             INT PRIMARY KEY,
  store_brand          VARCHAR(50),
  store_name           VARCHAR(255),
  store_address_street VARCHAR(255),
  postnummer           VARCHAR(10)
);

CREATE TABLE offer_variable (
  store_id              INT,
  offer_ean             VARCHAR(50),
  offer_startTime       DATETIME,
  offer_endTime         DATETIME,
  offer_newPrice        DECIMAL(10,2),
  offer_originalPrice   DECIMAL(10,2),
  offer_percentDiscount DECIMAL(5,2),
  offer_discount        DECIMAL(10,2),
  offer_lastUpdate      DATETIME,
  offer_stock           INT,
  product_ean           VARCHAR(50),
  product_description   TEXT,
  product_categories_da TEXT,
  timestamp_pipeline    DATETIME,
  snapshot_date         DATE,
  status                VARCHAR(20),
  PRIMARY KEY (store_id, offer_ean, offer_startTime, snapshot_date)
);


#Opgave 3
library(tidyverse)
library(ordinal)
library(MASS)
library(scales)

# Hent CSV fil
regnskaber <- read.csv2("~/Desktop/regnskaber_industri_transport_byg_5_25000_ansatte_anonym(in).csv")



# Sørg for at første kolonne hedder Laanemuligheder
names(regnskaber)[1] <- "Laanemuligheder"

## Lav branchekoderne til factors i stedet for numeric
regnskaber$Branchekode.primær <- factor(regnskaber$Branchekode.primær)
regnskaber$Branchekode.sekundær.1 <- factor(regnskaber$Branchekode.sekundær.1)
regnskaber$Branchekode.sekundær.2 <- factor(regnskaber$Branchekode.sekundær.2)
regnskaber$Branchekode.sekundær.3 <- factor(regnskaber$Branchekode.sekundær.3)


# 2) Ryd op i original-kategorier, og lav til ordnet factor
regnskaber <- regnskaber %>%
  mutate(Laanemuligheder = recode(Laanemuligheder, "Dårlige" = "Dårlig"),
         Laanemuligheder = factor(Laanemuligheder,
                                  levels = c("Meget dårlige", "Dårlig", "Neutrale", "Gode", "Meget gode", "Ved ikke"),
                                  ordered = TRUE
         )
  )

## Lav df'er med hver kategori
daarlig <- regnskaber[grep("Dårlig", regnskaber[[1]], ignore.case = T), ]
neutral <- regnskaber[grep("Neutrale", regnskaber[[1]], ignore.case = T), ]
god <- regnskaber[grep("Gode", regnskaber[[1]], ignore.case = T), ]
vedikke <- regnskaber[grep("Ved ikke", regnskaber[[1]], ignore.case = T), ]


## Omdøb alle entries, så der kun er dårlig, neutral og god
daarlig$Laanemuligheder <- "daarlige"
neutral$Laanemuligheder <- "neutrale"
god$Laanemuligheder <- "gode"
vedikke$Laanemuligheder <- "vedikke"

## Læg de tre df'er sammen til én
regnskaberdf <- rbind(daarlig, neutral, god, vedikke)

regnskaberdf$Laanemuligheder = factor(regnskaberdf$Laanemuligheder,
                                      levels = c("daarlige", "neutrale", "gode", "vedikke"),
                                      ordered = TRUE
)

##ggplot over alle kategorier
ggplot(data = regnskaber, aes(x = Laanemuligheder)) +
  geom_bar(fill = "steelblue") +
  stat_count(aes(label = paste0(..count.., " (", round(..count../sum(..count..)*100), "%)")),
             geom = "text",
             vjust = -0.4,
             size = 4.5) +
  labs(title = "Langt de fleste virksomheder har gode lånemuligheder",
       x = "Kategori",
       y = "Antal virksomheder",
       caption = "Kilde: Dansk Industri") +
  theme_minimal()

## ggplot over dårlig, neutral, god
ggplot(data = regnskaberdf, aes(x = Laanemuligheder)) +
  scale_x_discrete(labels = c("daarlige" = "Dårlige",
                              "neutrale" = "Neutrale",
                              "gode" = "Gode",
                              "vedikke" = "Ved ikke")) +
  geom_bar(fill = "steelblue") +
  stat_count(aes(label = paste0(..count.., " (", round(..count../sum(..count..)*100), "%)")),
             geom = "text",
             vjust = -0.4,
             size = 4.5) +
  labs(title = "Langt de fleste virksomheder har gode lånemuligheder",
       x = "Kategori",
       y = "Antal virksomheder",
       caption = "Kilde: Dansk Industri") +
  theme_minimal()

##Opgave 3.2
## Træk kolonner med 2020 tal ud
daarlig2020 <- daarlig[, c(1, 2, seq(14, ncol(daarlig), by = 6))]
neutral2020 <- neutral[, c(1, 2, seq(14, ncol(daarlig), by = 6))]
god2020 <- god[, c(1, 2, seq(14, ncol(daarlig), by = 6))]

model2020 <- rbind(daarlig2020, neutral2020, god2020)

model2020$Laanemuligheder = factor(model2020$Laanemuligheder,
                                   levels = c("daarlige", "neutrale", "gode"),
                                   ordered = TRUE
)

#Tag log af store tal, så clm bedre kan regne på tallene?
model2020$log_Bruttofortjeneste.2020..1.000.kr. <- if_else(model2020$Bruttofortjeneste.2020..1.000.kr. >= 0,
                                                           log10(model2020$Bruttofortjeneste.2020..1.000.kr. + 0.0001),
                                                           -log10(-model2020$Bruttofortjeneste.2020..1.000.kr.))

model2020$log_EBITDA.2020..1.000.kr. <- if_else(model2020$EBITDA.2020..1.000.kr. >= 0,
                                                log10(model2020$EBITDA.2020..1.000.kr. + 0.0001),
                                                -log10(-model2020$EBITDA.2020..1.000.kr.))

model2020$log_Primært.Resultat.2020..1.000.kr. <- if_else(model2020$Primært.Resultat.2020..1.000.kr. >= 0,
                                                          log10(model2020$Primært.Resultat.2020..1.000.kr. + 0.0001),
                                                          -log10(-model2020$Primært.Resultat.2020..1.000.kr.))

model2020$log_Afskrivninger.2020..1.000.kr. <- if_else(model2020$Afskrivninger.2020..1.000.kr. >= 0,
                                                       log10(model2020$Afskrivninger.2020..1.000.kr. + 0.0001),
                                                       -log10(-model2020$Afskrivninger.2020..1.000.kr.))

model2020$log_Kapacitetsomkostninger.2020..1.000.kr. <- if_else(model2020$Kapacitetsomkostninger.2020..1.000.kr. >= 0,
                                                                log10(model2020$Kapacitetsomkostninger.2020..1.000.kr. + 0.0001),
                                                                -log10(-model2020$Kapacitetsomkostninger.2020..1.000.kr.))

model2020$log_Skat.af.årets.resultat.2020..1.000.kr. <- if_else(model2020$Skat.af.årets.resultat.2020..1.000.kr. >= 0,
                                                                log10(model2020$Skat.af.årets.resultat.2020..1.000.kr. + 0.0001),
                                                                -log10(-model2020$Skat.af.årets.resultat.2020..1.000.kr.))

model2020$log_Finans.indtægter.2020..1.000.kr. <- if_else(model2020$Finans.indtægter.2020..1.000.kr. >= 0,
                                                          log10(model2020$Finans.indtægter.2020..1.000.kr. + 0.0001),
                                                          -log10(-model2020$Finans.indtægter.2020..1.000.kr.))

model2020$log_Finans.udgifter.2020..1.000.kr. <- if_else(model2020$Finans.udgifter.2020..1.000.kr. >= 0,
                                                         log10(model2020$Finans.udgifter.2020..1.000.kr. + 0.0001),
                                                         -log10(-model2020$Finans.udgifter.2020..1.000.kr.))

model2020$log_Overført.resultat.2020..1.000.kr. <- if_else(model2020$Overført.resultat.2020..1.000.kr. >= 0,
                                                           log10(model2020$Overført.resultat.2020..1.000.kr. + 0.0001),
                                                           -log10(-model2020$Overført.resultat.2020..1.000.kr.))

model2020$log_Anlægsaktiver.2020..1.000.kr. <- if_else(model2020$Anlægsaktiver.2020..1.000.kr. >= 0,
                                                       log10(model2020$Anlægsaktiver.2020..1.000.kr. + 0.0001),
                                                       -log10(-model2020$Anlægsaktiver.2020..1.000.kr.))

model2020$log_Omsætningsaktiver.2020..1.000.kr. <- if_else(model2020$Omsætningsaktiver.2020..1.000.kr. >= 0,
                                                           log10(model2020$Omsætningsaktiver.2020..1.000.kr. + 0.0001),
                                                           -log10(-model2020$Omsætningsaktiver.2020..1.000.kr.))

model2020$log_Varebeholdning.2020..1.000.kr. <- if_else(model2020$Varebeholdning.2020..1.000.kr. >= 0,
                                                        log10(model2020$Varebeholdning.2020..1.000.kr. + 0.0001),
                                                        -log10(-model2020$Varebeholdning.2020..1.000.kr.))

model2020$log_Varedebitorer.2020..1.000.kr. <- if_else(model2020$Varedebitorer.2020..1.000.kr. >= 0,
                                                       log10(model2020$Varedebitorer.2020..1.000.kr. + 0.0001),
                                                       -log10(-model2020$Varedebitorer.2020..1.000.kr.))

model2020$log_Likvider.2020..1.000.kr. <- if_else(model2020$Likvider.2020..1.000.kr. >= 0,
                                                  log10(model2020$Likvider.2020..1.000.kr. + 0.0001),
                                                  -log10(-model2020$Likvider.2020..1.000.kr.))

model2020$log_Materielle.anlægsaktiver.2020..1.000.kr. <- if_else(model2020$Materielle.anlægsaktiver.2020..1.000.kr. >= 0,
                                                                  log10(model2020$Materielle.anlægsaktiver.2020..1.000.kr. + 0.0001),
                                                                  -log10(-model2020$Materielle.anlægsaktiver.2020..1.000.kr.))

model2020$log_Driftsmateriel.og.inventar.2020..1.000.kr. <- if_else(model2020$Driftsmateriel.og.inventar.2020..1.000.kr. >= 0,
                                                                    log10(model2020$Driftsmateriel.og.inventar.2020..1.000.kr. + 0.0001),
                                                                    -log10(-model2020$Driftsmateriel.og.inventar.2020..1.000.kr.))

model2020$log_Egenkapital.2020..1.000.kr. <- if_else(model2020$Egenkapital.2020..1.000.kr. >= 0,
                                                     log10(model2020$Egenkapital.2020..1.000.kr. + 0.0001),
                                                     -log10(-model2020$Egenkapital.2020..1.000.kr.))

model2020$log_Langfristet.gæld.2020..1.000.kr. <- if_else(model2020$Langfristet.gæld.2020..1.000.kr. >= 0,
                                                          log10(model2020$Langfristet.gæld.2020..1.000.kr. + 0.0001),
                                                          -log10(-model2020$Langfristet.gæld.2020..1.000.kr.))

model2020$log_Kortfristet.gæld.2020..1.000.kr. <- if_else(model2020$Kortfristet.gæld.2020..1.000.kr. >= 0,
                                                          log10(model2020$Kortfristet.gæld.2020..1.000.kr. + 0.0001),
                                                          -log10(-model2020$Kortfristet.gæld.2020..1.000.kr.))

model2020$log_Balance.2020..1.000.kr. <- if_else(model2020$Balance.2020..1.000.kr. >= 0,
                                                 log10(model2020$Balance.2020..1.000.kr. + 0.0001),
                                                 -log10(-model2020$Balance.2020..1.000.kr.))

model2020$log_Varekreditorer.2020..1.000.kr. <- if_else(model2020$Varekreditorer.2020..1.000.kr. >= 0,
                                                        log10(model2020$Varekreditorer.2020..1.000.kr. + 0.0001),
                                                        -log10(-model2020$Varekreditorer.2020..1.000.kr.))

model2020$log_Årets.bruttoresultat.2020..1.000.kr. <- if_else(model2020$Årets.bruttoresultat.2020..1.000.kr. >= 0,
                                                              log10(model2020$Årets.bruttoresultat.2020..1.000.kr. + 0.0001),
                                                              -log10(-model2020$Årets.bruttoresultat.2020..1.000.kr.))

model2020$log_Årets.nettoresultat.2020..1.000.kr. <- if_else(model2020$Årets.nettoresultat.2020..1.000.kr. >= 0,
                                                             log10(model2020$Årets.nettoresultat.2020..1.000.kr. + 0.0001),
                                                             -log10(-model2020$Årets.nettoresultat.2020..1.000.kr.))

#Disse er udeladt grundet mangel på data
#Omsætning.2020..1.000.kr.
#Vareforbrug.2020..1.000.kr.
#Minoritetsinteresser.2020..1.000.kr.
#Grunde.og.bygninger.2020..1.000.kr.
#Goodwill.2020..1.000.kr.
#Immaterielle.anlægsaktiver.2020..1.000.kr.
#Finansielle.anlægsaktiver.2020..1.000.kr.
#Periode.afgrænsninger.2020..1.000.kr.
#Årets.resultat.efter.minoritetsinteresser.2020..1.000.kr. - Singularitetsfejl

clm_model1 <- clm(Laanemuligheder ~ 
                    Antal.ansatte.Cvr.nr. +
                    log_Finans.udgifter.2020..1.000.kr. +
                    log_Anlægsaktiver.2020..1.000.kr. +
                    log_Varedebitorer.2020..1.000.kr. +
                    log_Balance.2020..1.000.kr.,
                  data = model2020)

summary(clm_model1)

#Fjern de ikke signifikante
#1 log_Driftsmateriel.og.inventar.2020..1.000.kr.
#2 log_Omsætningsaktiver.2020..1.000.kr.
#3 Likviditetsgrad.2020....
#4 Soliditetsgrad.2020....
#5 log_Langfristet.gæld.2020..1.000.kr.
#6 log_Kortfristet.gæld.2020..1.000.kr.
#7 log_Varekreditorer.2020..1.000.kr.
#8 Afkastningsgrad.2020....
#9 Egenkapital.forrentning.2020....
#10 log_Overført.resultat.2020..1.000.kr.
#11 log_Kapacitetsomkostninger.2020..1.000.kr.
#12 log_Finans.indtægter.2020..1.000.kr.
#13 log_Afskrivninger.2020..1.000.kr.
#14 log_EBITDA.2020..1.000.kr.
#15 log_Primært.Resultat.2020..1.000.kr.
#16 log_Egenkapital.2020..1.000.kr.
#17 log_Bruttofortjeneste.2020..1.000.kr.
#18 log_Varebeholdning.2020..1.000.kr.
#19 log_Materielle.anlægsaktiver.2020..1.000.kr.
#20 log_Likvider.2020..1.000.kr.
#21 log_Skat.af.årets.resultat.2020..1.000.kr.
#22 log_Årets.nettoresultat.2020..1.000.kr.
#23 log_Årets.bruttoresultat.2020..1.000.kr.


s <- summary(clm_model1)

# Konverter coefficienttabellen til dataframe
coefdf <- as.data.frame(s$coefficients)

# Tilføj variabelnavn som kolonne
coefdf$variable <- rownames(coefdf)

# Find koefficienten med den højeste p-værdi
highestp <- coefdf[which.max(coefdf$`Pr(>|z|)`), ]
highestp


## Opgave 3.3
daarlig2020_mean <- mean(daarlig2020$Balance.2020..1.000.kr., na.rm = T)
neutral2020_mean <- mean(neutral$Balance.2020..1.000.kr., na.rm = T)
god2020_mean <- mean(god$Balance.2020..1.000.kr., na.rm = T)

dfmeans <- data.frame(
  kategori = factor(c("Dårlige", "Neutrale", "Gode"),
                    levels = c("Dårlige", "Neutrale", "Gode")),
  balance  = c(daarlig2020_mean, neutral2020_mean, god2020_mean)
)


ggplot(dfmeans, aes(x = kategori, y = balance)) +
  geom_col(fill = "steelblue") +
  scale_y_log10(
    limits = c(1, 1000000),
    breaks = c(10, 100, 1000, 10000, 100000, 1000000),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Stor balance giver gode lånemuligheder",
       x = "Lånemuligheder",
       y = "Gennemsnitlig balance (1.000 kr.)") +
  theme_minimal()

