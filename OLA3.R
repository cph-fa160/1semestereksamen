#Opgave 1.1

library(dkstat)
library(tidyverse)

#Først henter vi forbrugerforventninger

FORV1 <- dst_meta("FORV1", lang = "da")
FORV1$variables

FORV1query <- list(INDIKATOR = "*",
                   Tid = "*")

FORV <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")

FORV <- FORV[as.Date(FORV$TID) > as.Date("1999-12-01"), ]
FORV <- FORV[as.Date(FORV$TID) < as.Date("2025-09-01"), ]
FORV <- FORV %>%
  filter(INDIKATOR != "F1 Forbrugertillidsindikatoren")

FORVQ <- data.frame()                   # Tom dataframe til resultater

for (k in unique(FORV$INDIKATOR)) {
  sub <- FORV[FORV$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   # hver 3. måned udgør et kvartal
      mean_score <- mean(sub$value[(i-2):i])  # gennemsnit af de tre måneder
      
      # gem resultatet i et midlertidigt datasæt
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],  # sidste måned i kvartalet
        MeanScore = mean_score
      )
      FORVQ <- rbind(FORVQ, tmp)
    }
  }
}

FORVQ <- pivot_wider(FORVQ, names_from = Kvartal, values_from = MeanScore)

FORVQ[1,1] <- "Spg1"
FORVQ[2,1] <- "Spg2"
FORVQ[3,1] <- "Spg3"
FORVQ[4,1] <- "Spg4"
FORVQ[5,1] <- "Spg5"
FORVQ[6,1] <- "Spg6"
FORVQ[7,1] <- "Spg7"
FORVQ[8,1] <- "Spg8"
FORVQ[9,1] <- "Spg9"
FORVQ[10,1] <- "Spg10"
FORVQ[11,1] <- "Spg11"
FORVQ[12,1] <- "Spg12"

FORVQ_byKvartal <- FORVQ %>%
  # 1) Long: dato-kolonner -> rækker
  pivot_longer(
    cols = -Kategori,            # alt undtagen 'Kategori' (Spg1..Spg12)
    names_to  = "Kvartal",       # kolonnenavne (datoer) bliver til variablen 'Kvartal'
    values_to = "værdi"          # cellernes tal
  ) %>%
  
  # 3) Wide: rækker = Kvartal, kolonner = Spg1..Spg12
  pivot_wider(
    names_from  = Kategori,      # kolonnenavne = spørgsmål
    values_from = værdi          # værdier = tal
  )


#så henter vi forbrug, man kan bruge flere men vi tager NKN1
NKN1 <- dst_meta("NKN1", lang = "da")

NKN1$variables
NKN1$values

NKN1query <- list(TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
                  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
                  SÆSON = "Sæsonkorrigeret",
                  Tid = "*")

NKN1df <- dst_get_data(table = "NKN1", query = NKN1query, lang = "da")

NKN1df <- NKN1df[as.Date(NKN1df$TID) >= as.Date("1998-10-01"), ]

NKN1dfv <- as.data.frame((NKN1df$value[5:nrow(NKN1df)] /
                            NKN1df$value[1:(nrow(NKN1df)-4)] - 1) * 100)

colnames(NKN1dfv)[1] <- "Vækst"

BIND <- cbind(NKN1dfv, FORVQ_byKvartal)

BIND <- BIND[ , -2]

#vend spg5 og 7 da de påvirker omvendt
BIND$Spg7 <- -BIND$Spg7
BIND$Spg8 <- -BIND$Spg8


#laver alle kombinationer
# (uændret kode ovenfor)

comb <- rep(list(rep(list(rep(list(), 1)), nrow(BIND))), (ncol(BIND) - 1))

for (j in 1:(ncol(BIND) - 1)) {
  for (i in 1:length(comb[[1]])) {
    comb[[j]][[i]] <- colMeans(
      combn(as.numeric(unlist(BIND[i, 2:ncol(BIND)])), j)
    )
  }
}

list.indikatorer <- rep(list(rep(list(), 1)), length(comb))
for (i in 1:length(comb)) {
  list.indikatorer[[i]] <- t(matrix(unlist(comb[[i]]), ncol = length(comb[[1]])))
}


indikatorer <- matrix(t(unlist(list.indikatorer)), nrow = length(comb[[1]]))


#opgave 1.2
# --- NY: lav kolonnenavne der matcher kombinationerne (i samme rækkefølge) ---
colnames(indikatorer) <- unlist(lapply(seq_len(ncol(BIND)-1), \(j) combn(names(BIND)[-1], j, paste, collapse = " + ")), use.names = FALSE)




for (i in 1:length(comb)) {
  list.indikatorer[[i]] <- t(matrix(unlist(comb[[i]]), ncol = length(comb[[1]])))
}

indikatorer <- matrix(t(unlist(list.indikatorer)), nrow = length(comb[[1]]))


# indikator r squared
years <- (nrow(indikatorer)):nrow(indikatorer)

#cor.forbrug.ftillid.all <- matrix(0, nrow = length(years), ncol = ncol(indikatorer))
# years <- (nrow(indikatorer)-32):nrow(indikatorer)
years <- seq_len(nrow(indikatorer))   
lm.forbrug.ftillid.all <- rep(list(rep(list(rep(list())), length(years))), ncol(indikatorer))
lm.forbrug.ftillid.all.r2 <- matrix(0, nrow = length(years), ncol = ncol(indikatorer))

for (j in years) {
  for (i in 1:ncol(indikatorer)) {
    lm.forbrug.ftillid.all[[i]][[(1 + j - years[1])]] <- lm(BIND$Vækst[1:j] ~ indikatorer[1:j, i])
    lm.forbrug.ftillid.all.r2[(1 + j - years[1]), i] <- summary(lm.forbrug.ftillid.all[[i]][[(1 + j - years[1])]])$r.squared
  }
}

#kolonne navne
colnames(lm.forbrug.ftillid.all.r2) <- unlist(lapply(seq_len(ncol(BIND)-1), \(j) combn(names(BIND)[-1], j, paste, collapse = " + ")), use.names = FALSE)

lm.forbrug.ftillid.all.r2 <- round(lm.forbrug.ftillid.all.r2, 5)


# --- Top 5 R² for række 102 (med kolonnenavne) ---
top5r <- as.data.frame(head(sort(lm.forbrug.ftillid.all.r2[102, ], decreasing = TRUE, na.last = NA), 100))



model_eksempel <- lm(Vækst ~ I((Spg1 + Spg7)/2), data = BIND) # OLS hvor indikatoren dannes i formel som gennemsnit af 4 spørgsmål
summary(model_eksempel)        # fuld modelsammendrag inkl. R², justeret R², koefficienter m.m

#Opgave 1.3

#opgave 1.4
library(dkstat)
library(tidyverse)
lm_1235691112 <- lm(Vækst ~ I((Spg1 + Spg2 + Spg3 + Spg5 + Spg6 + Spg9 + Spg11 + Spg12)/8), data = BIND)
summary(lm_1235691112)

#Beregn vores FTI for 2025K4
FORV_wider <- FORV %>%
  pivot_wider(
    names_from = TID,
    values_from = value
  )

voresFTI2025K4 <- mean(as.matrix(FORV_wider[c(1, 2, 3, 5, 6, 9, 11, 12), (ncol(FORV_wider)-1):ncol(FORV_wider)]))

vækst2025K4 <- coef(lm_1235691112)[1] + coef(lm_1235691112)[2] * voresFTI2025K4
#Opgave 1.5


#Opgave 2
#Opgave 2.1
library(dkstat)
library(tidyverse)

#Først henter vi forbrugerforventninger

FORV1 <- dst_meta("FORV1", lang = "da")
FORV1$variables

FORV1query <- list(INDIKATOR = "*",
                   Tid = "*")

FORV <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")

FORV <- FORV[as.Date(FORV$TID) > as.Date("1999-12-01"), ]
FORV <- FORV[as.Date(FORV$TID) < as.Date("2025-09-01"), ]
FORV <- FORV %>%
  filter(INDIKATOR != "F1 Forbrugertillidsindikatoren")

FORVQ <- data.frame()                   # Tom dataframe til resultater

for (k in unique(FORV$INDIKATOR)) {
  sub <- FORV[FORV$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   # hver 3. måned udgør et kvartal
      mean_score <- mean(sub$value[(i-2):i])  # gennemsnit af de tre måneder
      
      # gem resultatet i et midlertidigt datasæt
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],  # sidste måned i kvartalet
        MeanScore = mean_score
      )
      FORVQ <- rbind(FORVQ, tmp)
    }
  }
}

FORVQ <- pivot_wider(FORVQ, names_from = Kvartal, values_from = MeanScore)

FORVQ[1,1] <- "Spg1"
FORVQ[2,1] <- "Spg2"
FORVQ[3,1] <- "Spg3"
FORVQ[4,1] <- "Spg4"
FORVQ[5,1] <- "Spg5"
FORVQ[6,1] <- "Spg6"
FORVQ[7,1] <- "Spg7"
FORVQ[8,1] <- "Spg8"
FORVQ[9,1] <- "Spg9"
FORVQ[10,1] <- "Spg10"
FORVQ[11,1] <- "Spg11"
FORVQ[12,1] <- "Spg12"

FORVQ_byKvartal <- FORVQ %>%
  # 1) Long: dato-kolonner -> rækker
  pivot_longer(
    cols = -Kategori,            # alt undtagen 'Kategori' (Spg1..Spg12)
    names_to  = "Kvartal",       # kolonnenavne (datoer) bliver til variablen 'Kvartal'
    values_to = "værdi"          # cellernes tal
  ) %>%
  
  # 3) Wide: rækker = Kvartal, kolonner = Spg1..Spg12
  pivot_wider(
    names_from  = Kategori,      # kolonnenavne = spørgsmål
    values_from = værdi          # værdier = tal
  )


#så henter vi forbrug, man kan bruge flere men vi taer NKHC021
NKN1 <- dst_meta("NKN1", lang = "da")

NKN1$variables
NKN1$values

NKN1query <- list(TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
                  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
                  SÆSON = "Sæsonkorrigeret",
                  Tid = "*")

NKN1df <- dst_get_data(table = "NKN1", query = NKN1query, lang = "da")

NKN1df <- NKN1df[as.Date(NKN1df$TID) >= as.Date("1998-10-01"), ]

NKN1dfv <- as.data.frame((NKN1df$value[5:nrow(NKN1df)] /
                            NKN1df$value[1:(nrow(NKN1df)-4)] - 1) * 100)

colnames(NKN1dfv)[1] <- "Vækst"

BIND <- cbind(NKN1dfv, FORVQ_byKvartal)

BIND <- BIND[ , -2]

BIND$Spg7 <- -BIND$Spg7
BIND$Spg8 <- -BIND$Spg8

library(pls)

pcr.fit <- pcr(Vækst ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg5 + Spg6 + Spg7 + Spg8 + Spg9 + Spg10 + Spg11 + Spg12, 
               data = BIND, scale = TRUE , 
               validation = "CV" ,
               segments = 10, segments.type = "consecutive")

summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
w.indicators.1 <- loadings.pcr.fit[1:12]^2
sum(w.indicators.1)
Data.w.indicators.1 <- as.data.frame(w.indicators.1)

validationplot(pcr.fit)
mtext("Kilde: Danmarks Statistik (DST), FORV1 og NKN1", side = 1, line = 4, adj = 0, cex = 0.8)


loadings.pcr.fit
#Opgave 2.3

FORV1_ <- dst_meta("FORV1", lang = "da")
FORV1_$variables

FORV1_query <- list(INDIKATOR = "*",
                    Tid = "*")

FORV_ <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")

FORV_ <- FORV_[as.Date(FORV_$TID) > as.Date("2025-06-01"), ]
FORV_ <- FORV_[as.Date(FORV_$TID) < as.Date("2025-09-01"), ]
FORV_ <- FORV_ %>%
  filter(INDIKATOR != "F1 Forbrugertillidsindikatoren")

# Hent forbrug 2025Q3

FORV1_ <- dst_meta("FORV1", lang = "da")
FORV1_$variables

FORV1_query <- list(INDIKATOR = "*",
                    Tid = "*")

FORV_ <- dst_get_data(table = "FORV1", query = FORV1_query, lang = "da")

FORV_ <- FORV_[as.Date(FORV_$TID) > as.Date("2025-06-01"), ]
FORV_ <- FORV_[as.Date(FORV_$TID) < as.Date("2025-09-01"), ]
FORV_ <- FORV_ %>%
  filter(INDIKATOR != "F1 Forbrugertillidsindikatoren")

FORV_Q <- data.frame()                   # Tom dataframe til resultater

for (k in unique(FORV_$INDIKATOR)) {
  sub <- FORV_[FORV_$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   # hver 3. måned udgør et kvartal
      mean_score <- mean(sub$value[(i-2):i])  # gennemsnit af de tre måneder
      
      # gem resultatet i et midlertidigt datasæt
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],  # sidste måned i kvartalet
        MeanScore = mean_score
      )
      FORV_Q <- rbind(FORV_Q, tmp)
    }
  }
}

FORV_Q <- pivot_wider(FORV_Q, names_from = Kvartal, values_from = MeanScore)

FORV_Q[1,1] <- "Spg1"
FORV_Q[2,1] <- "Spg2"
FORV_Q[3,1] <- "Spg3"
FORV_Q[4,1] <- "Spg4"
FORV_Q[5,1] <- "Spg5"
FORV_Q[6,1] <- "Spg6"
FORV_Q[7,1] <- "Spg7"
FORV_Q[8,1] <- "Spg8"
FORV_Q[9,1] <- "Spg9"
FORV_Q[10,1] <- "Spg10"
FORV_Q[11,1] <- "Spg11"
FORV_Q[12,1] <- "Spg12"

FORV_Q_byKvartal <- FORV_Q %>%
  # 1) Long: dato-kolonner -> rækker
  pivot_longer(
    cols = -Kategori,            # alt undtagen 'Kategori' (Spg1..Spg12)
    names_to  = "Kvartal",       # kolonnenavne (datoer) bliver til variablen 'Kvartal'
    values_to = "værdi"          # cellernes tal
  ) %>%
  
  # 3) Wide: rækker = Kvartal, kolonner = Spg1..Spg12
  pivot_wider(
    names_from  = Kategori,      # kolonnenavne = spørgsmål
    values_from = værdi          # værdier = tal
  )


FORV_Q_byKvartal$Spg7 <- -FORV_Q_byKvartal$Spg7
FORV_Q_byKvartal$Spg8 <- -FORV_Q_byKvartal$Spg8
FORV_Q_byKvartal$Spg10[1] <- "65.03333"
FORV_Q_byKvartal <- FORV_Q_byKvartal[,-1]
FORV_Q_byKvartal$Spg10 <- as.numeric(gsub("[^0-9]", ".",FORV_Q_byKvartal$Spg10 ))

predict(pcr.fit, ncomp = 1, newdata = FORV_Q_byKvartal )


### OPGAVE 3.1 – Forudsigelse af julehandel 2025 ###

# Hent metadata og data for forbrugertillid (FORV1)
Tillid <- dst_meta("FORV1", lang = "da")
Tillid$variables

Tillidquery <- list(INDIKATOR = "Forbrugertillidsindikatoren",
                    Tid = "*")

Tillid <- dst_get_data(table = "FORV1", query = Tillidquery, lang = "da")

# Afgræns periode for forbrugertillid
Tillid <- Tillid[as.Date(Tillid$TID) > as.Date("1999-12-01"), ]
Tillid <- Tillid[as.Date(Tillid$TID) < as.Date("2025-11-01"), ]

# Lav kvartalsgennemsnit for forbrugertillid (FTI)
TillidQ <- data.frame()   # Tom dataframe til resultater

for (k in unique(Tillid$INDIKATOR)) {
  sub <- Tillid[Tillid$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   
      mean_score <- mean(sub$value[(i-2):i])
      
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],
        MeanScore = mean_score
      )
      TillidQ <- rbind(TillidQ, tmp)
    }
  }
}

TillidQ <- pivot_wider(TillidQ, names_from = Kvartal, values_from = MeanScore)
TillidQ[1,1] <- "FTI"

# Omform FTI til én række pr. kvartal
TillidQ_byKvartal <- TillidQ %>%
  pivot_longer(
    cols = -Kategori,
    names_to  = "Kvartal",
    values_to = "værdi"
  ) %>%
  pivot_wider(
    names_from  = Kategori,
    values_from = værdi
  )

# Hent metadata og data for forbrug (NKN1)
Forbrug <- dst_meta("NKN1", lang = "da")

Forbrugquery <- list(
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*"
)

Forbrugdf <- dst_get_data(table = "NKN1", query = Forbrugquery, lang = "da")

# Afgræns periode for forbrug
Forbrugdf <- Forbrugdf[as.Date(Forbrugdf$TID) > as.Date("1998-12-01"), ]

# Beregn årlig vækst i forbrug (Q_t vs. Q_{t-4})
Forbrugdfv <- as.data.frame((Forbrugdf$value[5:nrow(Forbrugdf)] /
                               Forbrugdf$value[1:(nrow(Forbrugdf)-4)] - 1) * 100)

colnames(Forbrugdfv)[1] <- "Vækst"

# Kombinér kvartalsvis FTI og forbrugsvækst
ForbrugFTI <- cbind(TillidQ_byKvartal, Forbrugdfv)

# Konstruer datasæt til model for december-FTI
FTI_måneder <- Tillid %>%
  mutate(month = month(TID),
         year  = year(TID)) %>%
  filter(month %in% c(10, 11, 12)) %>%
  select(year, month, value) %>%
  pivot_wider(
    names_from = month,
    values_from = value,
    names_prefix = "M"
  ) %>%
  filter(!is.na(M12))

# Estimér december-FTI (M12 ~ M10 + M11)
model_dec <- lm(M12 ~ M10 + M11, data = FTI_måneder)
summary(model_dec)

# Udregn FTI for oktober og november 2025
FTI_2025 <- Tillid %>%
  filter(year(TID) == 2025, month(TID) %in% c(10, 11)) %>%
  mutate(month = month(TID)) %>%
  select(month, value) %>%
  pivot_wider(names_from = month, values_from = value, names_prefix = "M")

# Forudsig december 2025 og beregn FTI_Q4_2025
pred_dec_2025 <- predict(model_dec, newdata = FTI_2025)
pred_dec_2025

FTI_Q4_2025 <- mean(c(FTI_2025$M10, FTI_2025$M11, pred_dec_2025))
FTI_Q4_2025

# Tilføj estimeret Q4 2025 til datasættet
ForbrugFTI <- ForbrugFTI %>%
  add_row(Kvartal = "2025-12-01",
          FTI = FTI_Q4_2025)

# Opret dummy-variabel for positiv vækst
ForbrugFTI <- ForbrugFTI %>%
  mutate(dummy = ifelse(Vækst > 0, 1, 0))

# Udtræk Q4-observationer
JuleQ <- ForbrugFTI %>% filter(str_detect(Kvartal, "-12-01"))

# Træn logistisk julemodel
julemodel <- glm(dummy ~ FTI, 
                 data = JuleQ, 
                 family = binomial())
summary(julemodel)

# Forudsige sandsynlighed for stigning i julehandel 2025
p_2025 <- predict(julemodel,
                  newdata = JuleQ[JuleQ$Kvartal == "2025-12-01",],
                  type = "response")
p_2025   # 0.1235399

### OPGAVE 3.2 – Validering af julemodellen ###

# Beregn sandsynligheder og klassifikation for alle Q4-år
JuleQ$Sandsynlighed_for_stigning <- predict(julemodel, newdata = JuleQ, type = "response")

# Anvend cut-off = 0.45
JuleQ$Forudsiglese <- ifelse(JuleQ$Sandsynlighed_for_stigning >= 0.45, 1, 0)

# Forbered data til confusion matrix (uden 2025)
confmatrixDF <- JuleQ[-nrow(JuleQ), ]

confmatrixDF$Forudsigelse <- factor(confmatrixDF$Forudsiglese, levels = c(0,1))
confmatrixDF$dummy        <- factor(confmatrixDF$dummy,      levels = c(0,1))

# Beregn confusion matrix
confmatrixVækst <- confusionMatrix(confmatrixDF$Forudsigelse,
                                   confmatrixDF$dummy)

confmatrixVækst

#Opgave 4
#top 10 år
seneste10 <- lm.forbrug.ftillid.all.r2[63:102, , drop = FALSE]

seneste10gns <- colMeans(seneste10)

seneste5df <- as.data.frame(head(sort(seneste10gns, decreasing = TRUE, na.last = NA), 5))

#bund 10 år
sidste10 <- lm.forbrug.ftillid.all.r2[1:40, , drop = FALSE]

sidste10gns <- colMeans(sidste10)

sidste5df <- as.data.frame(head(sort(sidste10gns, decreasing = TRUE, na.last = NA), 5))

#samlet gennemsnit
Samlet <- lm.forbrug.ftillid.all.r2[c(1:40, 63:102), , drop = FALSE]

samletgns <- colMeans(Samlet)

samletdf <- as.data.frame(head(sort(samletgns, decreasing = TRUE, na.last = NA), 5))

#gennemsnit på alle år
alleår <- colMeans(lm.forbrug.ftillid.all.r2)

alleårdf <- as.data.frame(head(sort(alleår, decreasing = TRUE, na.last = NA), 5))

#fra 2016 - baum
baum <- lm.forbrug.ftillid.all.r2[1:68, , drop = FALSE]

baumgns <- colMeans(baum)

baumdf <- as.data.frame(head(sort(baumgns, decreasing = TRUE, na.last = NA), 5))




#Opgave 5.4
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyverse)

# API-nøgle
API_Key <- "de466b8a-9dd2-4232-84fc-1110d327ee9d"

# URL
url <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?api-key=", API_Key)

# Storm interval
start <- "2023-10-20T00:00:00Z"
slut  <- "2023-10-21T23:59:59Z"

# --- AARHUS ---
speed <- GET(url, query=list(
  stationId="06074", parameterId="wind_speed",
  datetime=paste0(start,"/",slut), limit=1000))

dir   <- GET(url, query=list(
  stationId="06074", parameterId="wind_dir",
  datetime=paste0(start,"/",slut), limit=1000))

dataspeed <- as.data.frame(fromJSON(content(speed,"text"))$features$properties)[,-c(1,2)]
datadir   <- as.data.frame(fromJSON(content(dir,"text"))$features$properties)[,-c(1,2)]

aarhusjoined <- inner_join(dataspeed, datadir, by=c("stationId","observed"))
names(aarhusjoined) <- c("Aarhus_Speed","Tid","Aarhus_Dir")


# --- ANHOLT ---
speed2 <- GET(url, query=list(
  stationId="06079", parameterId="wind_speed",
  datetime=paste0(start,"/",slut), limit=1000))

dir2   <- GET(url, query=list(
  stationId="06079", parameterId="wind_dir",
  datetime=paste0(start,"/",slut), limit=1000))

dataspeed2 <- as.data.frame(fromJSON(content(speed2,"text"))$features$properties)[,-c(1,2)]
datadir2   <- as.data.frame(fromJSON(content(dir2,"text"))$features$properties)[,-c(1,2)]

anholtjoined <- inner_join(dataspeed2, datadir2, by=c("stationId","observed"))
names(anholtjoined) <- c("Anholt_Speed","Tid","Anholt_Dir")


# --- SAMLE DATA ---
begge <- inner_join(aarhusjoined, anholtjoined, by="Tid")
begge$Tid <- as.POSIXct(begge$Tid, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")

df <- bind_rows(
  begge %>% transmute(Tid, station="Aarhus",
                      speed_ms=as.numeric(Aarhus_Speed),
                      dir_from=as.numeric(Aarhus_Dir)),
  begge %>% transmute(Tid, station="Anholt",
                      speed_ms=as.numeric(Anholt_Speed),
                      dir_from=as.numeric(Anholt_Dir))
) %>%
  mutate(
    dir_to = (dir_from + 180) %% 360,
    card = c("N","NØ","Ø","SØ","S","SV","V","NV")[
      (floor(((dir_from %% 360)+22.5)/45) %% 8) + 1
    ]
  ) %>%
  arrange(station, Tid)


# --- PILE ---
ar <- df %>% group_by(station) %>% slice(seq(1, n(), by=3)) %>% ungroup()

theta <- ar$dir_to * pi/180
L <- 0.03

ar$xend <- ar$Tid + L * sin(theta)
ar$yend <- ar$speed_ms + 0.2 * cos(theta)


# --- PLOT ---
ggplot(df, aes(Tid, speed_ms)) +
  geom_line(color="grey40", linewidth=0.7) +
  geom_segment(data=ar,
               aes(xend=xend, yend=yend, color=card),
               arrow=arrow(length=unit(2.5,"mm")),
               linewidth=0.8) +
  facet_wrap(~station, ncol=1, scales="free_y") +
  scale_color_manual(
    values=c(N="#1f77b4", NØ="#2ca02c", Ø="#17becf", SØ="#bcbd22",
             S="#8c564b", SV="#d62728", V="#9467bd", NV="#ff7f0e"),
    name="Vindretning"
  ) +
  labs(title="Vinden kom fra øst under stormen i oktober 2023",
       x="Tid (UTC)", y="Vindhastighed (m/s)") +
  theme_minimal()

                   
