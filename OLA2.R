### OPGAVE 1 ###

#1.1
BY3 <- dst_meta("BY3", lang = "da")
BY3$variables
BY3$values$BYER
BY3$values$FOLKARTAET
BY3$values$Tid


by3query <- list(
  Byer = "*",
  FOLKARTAET = "Folketal",
  Tid = "2025"
)

#laver en dataframe
dfBY <- dst_get_data(table = "BY3", query = by3query, lang = "da")

#fjerner tal i byer kolonne
dfBY$BYER <- gsub("[0-9]", "", dfBY$BYER)

#cleaning
# fjerner uden fast bopæl
BY3_clean <- dfBY[dfBY$BYER != " - Uden fast bopæl", ]

#fjerner landdistrikter
BY3_clean <- BY3_clean[BY3_clean$BYER != " - Landdistrikter", ]

BY3_clean$BYER <- gsub(" \\(del af flere kommuner\\)", "", BY3_clean$BYER)
BY3_clean$BYER <- gsub(" \\(del af Hovedstadsområdet\\)", "", BY3_clean$BYER)

# lægger dublettter sammen
BY3_clean <- BY3_clean %>%
  group_by(BYER) %>%
  summarise(
    value = sum(value),
    .groups = "drop"
  )

#fjerner alle byer under 200
BY3_clean <- BY3_clean[BY3_clean$value >=1, ]

BY3_clean <- BY3_clean %>%
  mutate(
    BYER = sub(" - ", "", BYER),                   # fjerner første kode før mellemrum
    BYER = sub("\\s*\\(.*\\)", "", BYER)         # fjerner alt i parentes
  )


#OPGAVE 1.2


BY3_clean <- BY3_clean %>%
  mutate(
    bykategori = cut(
      value,
      breaks = c(199, 1000, 2500, 10000, 50000, Inf),
      labels = c("Landsby",
                 "Lille by",
                 "Almindelig by",
                 "Større by",
                 "Storby")
    )
  )


plot(BY3_clean$bykategori)

#hent boligcl12
boligcl2 <- readRDS("~/Desktop/boligcl2.rds")


boligcl2$pris <- as.numeric(gsub("[^0-9]", "", boligcl2$pris))
boligcl2$kvm <- as.numeric(gsub("[^0-9]", "", boligcl2$kvm))
boligcl2$kvmpris <- boligcl2$pris/boligcl2$kvm


boligcl2 <- boligcl2 %>%
  select(zipmunic, kvmpris)






#laver lowercase
BY3_clean$BYER <- tolower(BY3_clean$BYER)

#laver æøå om
BY3_clean$BYER <- gsub("å", "aa", BY3_clean$BYER)
BY3_clean$BYER <- gsub("ø", "oe", BY3_clean$BYER)
BY3_clean$BYER <- gsub("æ", "ae", BY3_clean$BYER)

#navngiver kolonne det samme som i andet dataset
colnames(boligcl2)[1] <- "BYER"




#1.3
# Tilføj indbyggere til bolig-rækkerne (many-to-one)

bolig_joined <- boligcl2 %>%
  inner_join(BY3_clean, by = "BYER")



#1.4
bolig_joined <- boligcl2 %>%
  inner_join(BY3_clean, by = "BYER")




#1.4
plotdata <- bolig_joined %>%
  group_by(bykategori) %>%
  summarise(mean_kvmpris = mean(kvmpris, na.rm = TRUE),
            .groups = "drop")



#Plot gennemsnitlig kvmpris pr. bykategori
ggplot(plotdata, aes(x = bykategori, y = mean_kvmpris, fill = bykategori)) +
  geom_col() +
  labs(
    title = "Gennems nitlig kvadratmeterpris fordelt på bykategori",
    x = "Bykategori",
    y = "Gennemsnitlig kvm-pris (DKK)",
    caption = "Kilde: Boligsiden.dk og Danmarks Statistik"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.caption = element_text(hjust = 0)  # venstrestillet kilde (valgfrit)
  )


#Opgave 2
library(tidyverse)
library(dkstat)

#OPG 2.1
#Hent headers
FORV1 <- dst_meta(table = "FORV1", lang = "da")

#Find de rigtige querys
FORV1$variables
FORV1$values$INDIKATOR
FORV1$values$Tid

#Lav query liste
FORV1_query <- list(
  INDIKATOR = "*",
  Tid = "*"
)

#Hent tabel
FTI1974_2025 <- dst_get_data(table = "FORV1", query = FORV1_query, lang = "da")

#Filtrer år fra
FTI2000_2025_long <- FTI1974_2025 %>% 
  mutate(TID = as.Date(TID)+1) %>% 
  filter(TID >= as.Date("2000-01-01"))

#pivot_wider
FTI2000_2025 <- FTI2000_2025_long %>% 
  select(INDIKATOR, TID, value) %>% 
  pivot_wider(
    names_from = TID,
    values_from = value
  )

#Numerér spørgsmålene
FTI2000_2025[1, 1] <- "FTI"
FTI2000_2025[2, 1] <- "Spg1"
FTI2000_2025[3, 1] <- "Spg2"
FTI2000_2025[4, 1] <- "Spg3"
FTI2000_2025[5, 1] <- "Spg4"
FTI2000_2025[6, 1] <- "Spg5"
FTI2000_2025[7, 1] <- "Spg6"
FTI2000_2025[8, 1] <- "Spg7"
FTI2000_2025[9, 1] <- "Spg8"
FTI2000_2025[10, 1] <- "Spg9"
FTI2000_2025[11, 1] <- "Spg10"
FTI2000_2025[12, 1] <- "Spg11"
FTI2000_2025[13, 1] <- "Spg12"

# Vælg spørgsmål 1, 3, 5 og 6
DI <- FTI2000_2025[c(2,4,6,7), ]

# Beregn gennemsnit 
# Fjern første kolonne (teksten) og beregn gennemsnit pr. måned
DI_gennemsnit <- as.data.frame(colMeans(DI[ , -1]))

colnames(DI_gennemsnit)[1] = "DI FTI"

#Hent forbrug
#Hent headers
NKN1 <- dst_meta(table = "NKN1", lang = "da")

#Find de rigtige querys
NKN1$variables
NKN1$values$TRANSAKT
NKN1$values$PRISENHED
NKN1$values$SÆSON
NKN1$values$Tid

#Lav query liste
NKN1_query <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

#Hent tabel
Forbrug1990_2025 <- dst_get_data(table = "NKN1", query = NKN1_query, lang = "da")

#Filtrer år fra
Forbrug1999_2025_long <- Forbrug1990_2025 %>% 
  mutate(TID = as.Date(TID)+1)%>%               #Den trækker en dag fra
  filter(TID >= as.Date("1999-01-01"))

#pivot_wider
Forbrug1999_2025 <- Forbrug1999_2025_long %>% 
  select(TID, value) %>% 
  pivot_wider(
    names_from = TID,
    values_from = value
  )

# Lav forbug til procent
Forbrug_long <- as.data.frame(t(Forbrug1999_2025))

Forbrug <- as.data.frame(exp(diff(log(as.numeric(Forbrug_long$V1)), lag = 4)) - 1) * 100

colnames(Forbrug)[1] = "Procent forbrug"

Forbrug$TID <- rownames(Forbrug_long)[5:nrow(Forbrug_long)]


# Lav DI FTI til kvartal
DITillidKvartal <- data.frame()  # tomt df til resultater

for (i in seq_along(DI_gennemsnit$`DI FTI`)) {
  if (i %% 3 == 0) {   # hver 3. observation (kvartal)
    
    mean_score <- mean(DI_gennemsnit$`DI FTI`[(i-2):i])
    
    # gem resultatet
    DItmp <- data.frame(
      Kvartal = rownames(DI_gennemsnit)[i],  # sidste måned i kvartalet
      `DI FTI`  = mean_score
    )
    
    DITillidKvartal <- rbind(DITillidKvartal, DItmp)
  }
}
Forbrugvsdifti <- cbind(DITillidKvartal, Forbrug)

#Centrér DI FTI om 0
Forbrugvsdifti$normDIfti <- DITillidKvartal$DI.FTI-mean(DITillidKvartal$DI.FTI)

#Lav plot
ggplot(Forbrugvsdifti, aes(x = 1:nrow(DITillidKvartal))) +
  geom_col(aes(y = `Procent forbrug`), fill = "deepskyblue3") +    # søjler (privatforbrug)
  geom_line(aes(y = normDIfti), color = "black", size = 1) + # linje (tillid)
  scale_x_continuous(
    breaks = seq(1, nrow(DITillidKvartal), by = 4),     # hver fjerde værdi
    labels = seq(2000, 2025, by = 1)
  ) +
  scale_y_continuous(
    name = "Nettotal",   
    sec.axis = sec_axis(~ ., name = "Årlig realvækst i privatforbrug (%)")
  ) +
  labs(
    x = "År",
    title = "Forbrugertillid og årlig realvækst i privatforbrug"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)
  )

#Lav lm på DI's FTI indikator
LM_DIFTI <- lm(Forbrugvsdifti$`Procent forbrug` ~ Forbrugvsdifti$normDIfti)

summary(LM_DIFTI )

#DST FTI indikator
DST <- FTI2000_2025[c(2,3,4,5,6), ]

# Beregn gennemsnit
# Fjern første kolonne (teksten) og beregn gennemsnit pr. måned
DSTgennemsnit <- as.data.frame(colMeans(DST[ , -1]))

colnames(DSTgennemsnit)[1] = "DST FTI"

# Lav DST FTI til kvartal
DSTTillidKvartal <- data.frame()  # tomt df til resultater

for (i in seq_along(DSTgennemsnit$`DST FTI`)) {
  if (i %% 3 == 0) {   # hver 3. observation (kvartal)
    
    mean_score <- mean(DSTgennemsnit$`DST FTI`[(i-2):i])
    
    # gem resultatet
    DSTtmp <- data.frame(
      Kvartal = rownames(DSTgennemsnit)[i],  # sidste måned i kvartalet
      `DST FTI`  = mean_score
    )
    
    DSTTillidKvartal <- rbind(DSTTillidKvartal, DSTtmp)
  }
}

Forbrugvsdstfti <- cbind(DSTTillidKvartal, Forbrug)

#Centrér DST FTI om 0
Forbrugvsdstfti$normDSTfti <- DSTTillidKvartal$DST.FTI-mean(DSTTillidKvartal$DST.FTI)

#Lav ny df med DSTFTI og DIFTI
ForbrugFTI <- cbind(Forbrugvsdifti, Forbrugvsdstfti$normDSTfti)
colnames(ForbrugFTI) [6] = "normDSTfti"

#Samlet ggplot
ggplot(ForbrugFTI, aes(x = 1:nrow(DITillidKvartal))) +
  geom_col(aes(y = `Procent forbrug`), fill = "deepskyblue3") +    # søjler (privatforbrug)
  geom_line(aes(y = normDIfti, color = "DI’s indikator"), size = 1.2) +
  geom_line(aes(y = normDSTfti, color = "DST’s indikator"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +  # Nul-linje
  scale_color_manual(values = c("DI’s indikator" = "black", "DST’s indikator" = "red")) +
  scale_x_continuous(
    breaks = seq(1, nrow(DITillidKvartal), by = 4),     # hver fjerde værdi
    labels = seq(2000, 2025, by = 1)
  ) +
  scale_y_continuous(
    name = "Nettotal",   
    sec.axis = sec_axis(~ ., name = "Årlig realvækst i privatforbrug (%)")
  ) +
  labs(
    x = "År",
    title = "Forbrugertillid og årlig realvækst i privatforbrug",
    subtitle = "DI’s og DST’s indikatorer sammenholdt med kvartalsdata for forbrug"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)
  )

#Lav lm over DST FTI
LM_DSTFTI <- lm(ForbrugFTI$`Procent forbrug` ~ ForbrugFTI$normDSTfti)

summary(LM_DSTFTI)
summary(LM_DIFTI)

#Opg2.2
#Beregn de to FTI'er for 2025K4
DSTFTI2025K4 <- mean(tail(DSTgennemsnit$`DST FTI`, 2))
DIFTI2025K4 <- mean(tail(DI_gennemsnit$`DI FTI`, 2))

#Indsæt i model og beregn forventet årlig realvækst i 2025K4
DSTvækst2025K4 <- coef(LM_DSTFTI)[1] + coef(LM_DSTFTI)[2] * DSTFTI2025K4
DIvækst2025K4 <- coef(LM_DIFTI)[1] + coef(LM_DIFTI)[2] * DIFTI2025K4

#OPG2.3
DSTforbrug2025K4 <- Forbrug1999_2025[ncol(Forbrug1999_2025)-3] * (1 + DSTvækst2025K4 / 100)
DIforbrug2025K4 <- Forbrug1999_2025[ncol(Forbrug1999_2025)-3] * (1 + DIvækst2025K4 / 100)

#OPG2.4
DSTforbrug2025 <- sum(sum(Forbrug1999_2025[1, (ncol(Forbrug1999_2025)-2):ncol(Forbrug1999_2025)])) + DSTforbrug2025K4
DIforbrug2025 <- sum(sum(Forbrug1999_2025[1, (ncol(Forbrug1999_2025)-2):ncol(Forbrug1999_2025)])) + DIforbrug2025K4

Forbrug2024 <- sum(Forbrug1999_2025[1, (ncol(Forbrug1999_2025)-6):(ncol(Forbrug1999_2025)-3)])

DSTvækst24_25 <- (DSTforbrug2025 - Forbrug2024) / Forbrug2024
DIvækst24_25 <- (DIforbrug2025 - Forbrug2024) / Forbrug2024

#Opgave 3
library(dkstat)
library(tidyverse)
################################################################################
#OPG 3.1
################################################################################
#1. Data Retreival

#Først henter vi forbrugerforventninger

FORV1 <- dst_meta("NKN1", lang = "da")
FORV1$variables

FORV1query <- list(TRANSAKT = "*",
                   PRISENHED = "*",
                   SÆSON = "*",
                   Tid = "*")

FORV <- dst_get_data(table = "NKN1", query = FORV1query, lang = "da")

FORV <- FORV[as.Date(FORV$TID) > as.Date("1996-12-01"), ]
FORV <- FORV[as.Date(FORV$TID) < as.Date("2025-09-01"), ]
FORV <- FORV %>%
  filter(TRANSAKT != "F1 Forbrugertillidsindikatoren")

#2. Data Cleaning
Forv1 <- FORV[grep("P31S14D", FORV$TRANSAKT ), ]
Forv2 <- Forv1[grep("LKV_M", Forv1$PRISENHED ), ]
Forv3 <- Forv2[grep("Y", Forv2$SÆSON ), ]


# Gør så vi kun har Tid og value kolonnerne tilbage
Forv3 <- Forv3[,-1]
Forv3 <- Forv3[,-1]
Forv3 <- Forv3[,-1]

#Gør kvartalerne rigtige
library(dplyr)
library(lubridate)

Forv3 <- Forv3 %>% 
  mutate(TID = ceiling_date(TID, unit = "quarter") - days(0))
Forv3 <- Forv3 %>%
  mutate(TID = ceiling_date(TID, "quarter") - months(1))
Forv3 <- Forv3 %>%
  mutate(TID = TID + months(3))


#Gør til Wide format
Forv3Wide <- Forv3 %>%
  pivot_wider(
    names_from = TID,
    values_from = value
  )

#3.  Beregn kvartalsvis årlig vækst (year-on-year)
#Altså: (værdi_t / værdi_(t-4) - 1) * 100
growth2 <- (Forv3Wide[5:length(Forv3Wide)] /
              Forv3Wide[1:(length(Forv3Wide)-4)] - 1) * 100

#5.  Lav dummyvariabel
#Hvis væksten > 0 → 1 (stigning)
#Hvis væksten <= 0 → 0 (fald)
dummy <- ifelse(growth2 < 0, 1, 0)

#6.  Optæl antal stigninger og fald
antal_stigninger <- sum(dummy == 0, na.rm = TRUE)
antal_fald <- sum(dummy == 1, na.rm = TRUE)

Resultat11 <- cbind(antal_fald, antal_stigninger)

################################################################################
#OPG 3.2
################################################################################

#Hent Forbrugertillid fra API
FTILLID <- dst_meta("FORV1", lang = "da")
FTILLID$variables

FORV1query <- list(INDIKATOR = "*",
                   Tid = "*")

FTILLID <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")

FTILLID <- FTILLID[as.Date(FTILLID$TID) > as.Date("1997-12-01"), ]
FTILLID <- FTILLID[as.Date(FTILLID$TID) < as.Date("2025-09-01"), ]
FTILLID <- FTILLID %>%
  filter(INDIKATOR != "F1 Forbrugertillidsindikatoren")

#Data Cleaning 
FTILLID1 <- FTILLID[grep("Familiens økonomiske situation i dag|Danmarks økonomiske situation i dag|Anskaffelse af større forbrugsgoder", FTILLID$INDIKATOR ), ]
any(is.na(FTILLID1))

DIFTI_KVARTAL2 <- data.frame()  # tomt datasæt til resultater

for (k in unique(FTILLID1$INDIKATOR)) {
  sub <- FTILLID1[FTILLID1$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   # hver 3. måned udgør et kvartal
      mean_score <- mean(sub$value[(i-2):i])  # gennemsnit af de tre måneder
      
      # gem resultatet i et midlertidigt datasæt
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],  # sidste måned i kvartalet
        MeanScore = mean_score
      )
      DIFTI_KVARTAL2 <- rbind(DIFTI_KVARTAL2, tmp)
    }
  }
}

#Omdøb så de hedder spg. 1 osv
DIFTI_KVARTAL2$Kategori[DIFTI_KVARTAL2$Kategori == "F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden"] <- "Spg1"
DIFTI_KVARTAL2$Kategori[DIFTI_KVARTAL2$Kategori == "F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden"] <- "Spg2"
DIFTI_KVARTAL2$Kategori[DIFTI_KVARTAL2$Kategori == "F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"] <- "Spg3"
DIFTI_KVARTAL2$Kategori[DIFTI_KVARTAL2$Kategori == "F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."] <- "Spg4"

df_dummy <- as_data_frame(dummy)
rownames(df_dummy)[1] <- "Dummy"

df_dummyf <- df_dummy %>%
  pivot_longer(
    cols = everything(),
    names_to = "Kvartal",
    values_to = "Dummy"
  )

DUMMY_DF <- DUMMY_DF %>%
  filter(!is.na(Dummy))

DIFTI_WIDE <- DIFTI_KVARTAL2 %>%
  pivot_wider(names_from = Kategori, values_from = MeanScore)

DATA <- cbind(df_dummyf, DIFTI_WIDE[ , -1])  # -1 fjerner den gamle Kvartal-kolonne fra DIFTI_WIDE

model <- glm(Dummy ~ Spg1 + Spg2 + Spg3 + Spg4, data = DATA, family = binomial)
summary(model)

model2 <- glm(Dummy ~ Spg1 + Spg2 + Spg4, data = DATA, family = binomial)
summary(model2)

model3 <- glm(Dummy ~ Spg2 + Spg4, data = DATA, family = binomial)
summary(model3)

model4 <- glm(Dummy ~ Spg4, data = DATA, family = binomial)
summary(model4)

plot(model4)

#Finder gennemsnittet for 4. kvartal af spg 4
Spg4DF <- DIFTI_KVARTAL2[grep("Spg4", DIFTI_KVARTAL2$Kategori), ]
Spg4m12 <- Spg4DF[grep("12-01", Spg4DF$Kvartal), ]
Spg4GNM <- mean(Spg4m12$MeanScore)
as_data_frame(Spg4GNM)

DIFTI2025K4 <- data.frame(
  Spg4 = Spg4GNM
)

pred <- predict(model4, newdata = DIFTI2025K4, type = "response")

if (pred < 0.5) {
  print("Modellen forudsiger: OP (forbruget stiger i 2025K4)")
} else {
  print("Modellen forudsiger: NED (forbruget falder i 2025K4)")
}
is.na

################################################################################
#OPG 3.3
################################################################################

library(tidyverse)
library(caret)

Spg4Pred <- DIFTI_KVARTAL2 %>% 
  filter(Kategori == "Spg4")

colnames(Spg4Pred)[3] <- "Spg4"


Spg4Pred$modelpred <- predict(model4, newdata = Spg4Pred, type = "response")

Spg4Pred$Prediction <- ifelse(Spg4Pred$modelpred > 0.5, 1, 0)

Spg4Pred$dummy <- df_dummyf$Dummy

PredictionSpg4 <- factor(Spg4Pred$Prediction, levels = c(0,1))
ActualSpg4     <- factor(Spg4Pred$dummy,      levels = c(0,1))

Confmatrix <- confusionMatrix(PredictionSpg4, ActualSpg4)

Confmatrix

################################################################################
#OPG 3.4
################################################################################
#Hent datasættet ned via dkstat
ForbrugerforventningerOpg3_4 <- dst_meta("FORV1", lang = "da")
ForbrugerforventningerOpg3_4$variables

FORV1query <- list(INDIKATOR = "*",
                   Tid = "*")

ForbrugerforventningerOpg3_4 <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")

ForbrugerforventningerOpg3_4 <- ForbrugerforventningerOpg3_4[as.Date(ForbrugerforventningerOpg3_4$TID) > as.Date("1997-12-01"), ]
ForbrugerforventningerOpg3_4 <- ForbrugerforventningerOpg3_4[as.Date(ForbrugerforventningerOpg3_4$TID) < as.Date("2025-03-01"), ]
ForbrugerforventningerOpg3_4 <- ForbrugerforventningerOpg3_4 %>%
  filter(INDIKATOR != "F1 Forbrugertillidsindikatoren")

# Omdøb spørgsmålene til Spg1 osv.
ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR == 
    "F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden"] <- "Spg1"
ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F3 Familiens økonomiske  situation om et år, sammenlignet med i dag"] <- "Spg2"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden"] <- "Spg3"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F5 Danmarks økonomiske situation om et år, sammenlignet med i dag"] <- "Spg4"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"] <- "Spg5"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F6 Priser i dag, sammenlignet med for et år siden"] <- "Spg6"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F7 Priser om et år, sammenlignet med i dag"] <- "Spg7"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F8 Arbejdsløsheden om et år, sammenlignet med i dag"] <- "Spg8"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."] <- "Spg9"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F11 Anser det som fornuftigt at spare op i den nuværende økonomiske situation"] <- "Spg10"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F12 Regner med at kunne spare op i de kommende 12 måneder"] <- "Spg11"

ForbrugerforventningerOpg3_4$INDIKATOR[
  ForbrugerforventningerOpg3_4$INDIKATOR ==
    "F13 Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"] <- "Spg12"

ForbrugOpg3.4_KVARTAL <- data.frame()  # tomt datasæt til resultater

for (k in unique(ForbrugerforventningerOpg3_4$INDIKATOR)) {
  sub <- ForbrugerforventningerOpg3_4[ForbrugerforventningerOpg3_4$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   # hver 3. måned udgør et kvartal
      mean_score <- mean(sub$value[(i-2):i])  # gennemsnit af de tre måneder
      
      # gem resultatet i et midlertidigt datasæt
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],  # sidste måned i kvartalet
        MeanScore = mean_score
      )
      ForbrugOpg3.4_KVARTAL <- rbind(ForbrugOpg3.4_KVARTAL, tmp)
    }
  }
}

ForbrugOpg3.4Wide <- ForbrugOpg3.4_KVARTAL %>%
  pivot_wider(names_from = Kategori, values_from = MeanScore)

#Laver en ny Dummy
dummy3.4 <- dummy[1:109]
ForbrugOpg3.4_dummy <- cbind(dummy3.4, ForbrugOpg3.4Wide)

model3.4_1 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg5 + Spg6 + Spg7 + Spg8 + Spg9 + Spg10 + Spg11 + Spg12, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_1)

model3.4_2 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg5 + Spg6 + Spg7 + Spg8 + Spg9 + Spg10 + Spg12, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_2)

model3.4_3 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg5 + Spg6 + Spg7 + Spg9 + Spg10 + Spg12, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_3)

model3.4_4 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg6 + Spg7 + Spg9 + Spg10 + Spg12, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_4)

model3.4_5 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg7 + Spg9 + Spg10 + Spg12, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_5)

model3.4_6 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg7 + Spg9 + Spg10, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_6)

model3.4_7 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg4 + Spg9 + Spg10, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_7)

model3.4_8 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg3 + Spg9 + Spg10, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_8)

model3.4_9 <- glm(dummy3.4 ~ Spg1 + Spg2 + Spg9 + Spg10, data = ForbrugOpg3.4_dummy, family = binomial)
summary(model3.4_9)

ForbrugOpg3.4SIG <- ForbrugOpg3.4Wide [,c(2,3,10,11)]

ForbrugOpg3.4SIG$dummy <- ForbrugOpg3.4_dummy$dummy3.4

ForbrugOpg3.4SIG$modelPred <- predict.glm(model3.4_9, newdata = ForbrugOpg3.4SIG, type = "response")

ForbrugOpg3.4SIG$Prediction <- ifelse(ForbrugOpg3.4SIG$modelPred > 0.2, 1, 0)
ForbrugOpg3.4SIG$Prediction03 <- ifelse(ForbrugOpg3.4SIG$modelPred > 0.3, 1, 0)
ForbrugOpg3.4SIG$Prediction04 <- ifelse(ForbrugOpg3.4SIG$modelPred > 0.4, 1, 0)
ForbrugOpg3.4SIG$Prediction05 <- ifelse(ForbrugOpg3.4SIG$modelPred > 0.5, 1, 0)
colnames(ForbrugOpg3.4SIG) [8] <- "Prediction02"

#Gør dummy til faktor for Confmatrix
ForbrugOpg3.4SIG$dummy <- factor(ForbrugOpg3.4SIG$dummy, levels = c(0,1))

#Gør thresholds til faktorer
Prediction02 <- factor(ForbrugOpg3.4SIG$Prediction02, levels = c(0,1))
Prediction03 <- factor(ForbrugOpg3.4SIG$Prediction03, levels = c(0,1))
Prediction04 <- factor(ForbrugOpg3.4SIG$Prediction04, levels = c(0,1))
Prediction05 <- factor(ForbrugOpg3.4SIG$Prediction05, levels = c(0,1))

library(caret)

Conf02 <- confusionMatrix(Prediction02, ForbrugOpg3.4SIG$dummy)
Conf03 <- confusionMatrix(Prediction03, ForbrugOpg3.4SIG$dummy)
Conf04 <- confusionMatrix(Prediction04, ForbrugOpg3.4SIG$dummy)
Conf05 <- confusionMatrix(Prediction05, ForbrugOpg3.4SIG$dummy)

#Bedste Accuracy osv.
Conf03


#Samme men med DST's Tillidsindikator
DSTindikator <- dst_meta("FORV1", lang = "da")
DSTindikator$variables

FORV1query <- list(INDIKATOR = "*",
                   Tid = "*")

DSTindikator <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")

DSTindikator <- DSTindikator[as.Date(DSTindikator$TID) > as.Date("1997-12-01"), ]
DSTindikator <- DSTindikator[as.Date(DSTindikator$TID) < as.Date("2025-03-01"), ]

#Data cleaning
DSTindikator3.4_long <- DSTindikator[grep("F1 Forbrugertillidsindikatoren", DSTindikator$INDIKATOR ), ]

DSTindikator3.4_KVARTAL <- data.frame()  # tomt datasæt til resultater

for (k in unique(DSTindikator3.4_long$INDIKATOR)) {
  sub <- DSTindikator3.4_long[DSTindikator3.4_long$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) {   # hver 3. måned udgør et kvartal
      mean_score <- mean(sub$value[(i-2):i])  # gennemsnit af de tre måneder
      
      # gem resultatet i et midlertidigt datasæt
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],  # sidste måned i kvartalet
        MeanScore = mean_score
      )
      DSTindikator3.4_KVARTAL <- rbind(DSTindikator3.4_KVARTAL, tmp)
    }
  }
}

DSTindikator3.4_Wide <- DSTindikator3.4_KVARTAL %>%
  pivot_wider(names_from = Kategori, values_from = MeanScore)

DSTindikator3.4_dummy <- cbind(dummy3.4, DSTindikator3.4_Wide)

modelFTI <- glm(dummy3.4 ~ `F1 Forbrugertillidsindikatoren`, data = DSTindikator3.4_dummy, family = binomial)
summary(modelFTI)

DSTindikator3.4_Wide$dummy <- DSTindikator3.4_dummy$dummy3.4

DSTindikator3.4_Wide$modelPred <- predict.glm(modelFTI, newdata = DSTindikator3.4_Wide, type = "response")

DSTindikator3.4_Wide$Prediction <- predict(modelFTI, newdata = DSTindikator3.4_Wide, type = "response")

DSTindikator3.4_Wide$Prediction04 <- ifelse(DSTindikator3.4_Wide$modelPred > 0.4, 1, 0)

Prediction04 <- factor(DSTindikator3.4_Wide$Prediction04, levels = c(0,1))
Actual <- factor(DSTindikator3.4_Wide$dummy, levels = c(0,1))

conf04 <- confusionMatrix(Prediction04, Actual)
conf04


DSTindikator3.4_Wide$Prediction6 <- ifelse(DSTindikator3.4_Wide$modelPred > 0.6, 1, 0)

DSTindikator3.4_Wide$Prediction6 <- as.factor(DSTindikator3.4_Wide$Prediction6)

DSTindikator3.4_Wide$dummy <- as.factor(DSTindikator3.4_Wide$dummy)

confmatrixFTI4 <- confusionMatrix(DSTindikator3.4_Wide$Prediction4, DSTindikator3.4_Wide$dummy)
confmatrixFTI6 <- confusionMatrix(DSTindikator3.4_Wide$Prediction6, DSTindikator3.4_Wide$dummy)
confmatrixFTI5 <- confusionMatrix(DSTindikator3.4_Wide$Prediction5, DSTindikator3.4_Wide$dummy)
confmatrixFTI3 <- confusionMatrix(DSTindikator3.4_Wide$Prediction3, DSTindikator3.4_Wide$dummy)
confmatrixFTI2 <- confusionMatrix(DSTindikator3.4_Wide$Prediction2, DSTindikator3.4_Wide$dummy)
confmatrixFTI5
confmatrixFTI3
confmatrixFTI2



#Opgave 4
#OPGAVE 4.1 

# Henter DST’s forbrugertillidsindikator (FTI)
FORV1 <- dst_meta("FORV1", lang = "da")

FORV1query <- list(INDIKATOR = "Forbrugertillidsindikatoren",
                   Tid = "*")

FORV <- dst_get_data(table = "FORV1", query = FORV1query, lang = "da")
FORV <- FORV[as.Date(FORV$TID) > as.Date("1995-12-01"), ]


# Omregner månedlige værdier til kvartaler
FORVQ <- data.frame()

for (k in unique(FORV$INDIKATOR)) {
  sub <- FORV[FORV$INDIKATOR == k, ]
  
  for (i in seq_along(sub$value)) {
    if (i %% 3 == 0) { 
      mean_score <- mean(sub$value[(i-2):i])
      tmp <- data.frame(
        Kategori = k,
        Kvartal = sub$TID[i],
        MeanScore = mean_score
      )
      FORVQ <- rbind(FORVQ, tmp)
    }
  }
}


# Gør klar til plot
FORVQ_plot <- FORVQ %>% mutate(Dato = as.Date(Kvartal))

# Plotter udviklingen
ggplot(FORVQ_plot, aes(x = Dato, y = MeanScore)) +
  geom_area(fill = "#d7301f", alpha = 0.10, color = NA) +
  geom_line(color = "#d7301f", linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(data = tail(FORVQ_plot, 1), size = 3, color = "#d7301f") +
  geom_label(
    data = tail(FORVQ_plot, 1),
    aes(label = round(MeanScore, 1)),
    nudge_x = 60, vjust = 0.5,
    label.size = 0, fill = "white", color = "#333333"
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Forbrugertillidsindikator",
    subtitle = "Kvartalsvise nettotalsværdier",
    x = "År", y = "Nettotal",
    caption = "Kilde: Danmarks Statistik, FORV1"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey30"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )



#############################################
###############  OPGAVE 4.2  ################
#############################################

# Henter enkelt-spørgsmålet om større forbrugsgoder
FORV2 <- dst_meta("FORV1", lang = "da")

FORV2query <- list(
  INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
  Tid = "*"
)

FORV2 <- dst_get_data(table = "FORV1", query = FORV2query, lang = "da")
FORV2 <- FORV2[as.Date(FORV2$TID) > as.Date("1999-12-01"), ]
FORV2 <- FORV2[as.Date(FORV2$TID) < as.Date("2025-09-01"), ]

# Finder gennemsnittet
mean(FORV2$value)



#############################################
###############  OPGAVE 4.3  ################
#############################################

# Henter husholdningernes forbrug (15 grupper)
NKHC021 <- dst_meta("NKHC021", lang = "da")

forbrugquery <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  TID = "*"
)

Forbrugdf <- dst_get_data(table = "NKHC021", query = forbrugquery, lang = "da")
Forbrugdf <- Forbrugdf[as.Date(Forbrugdf$TID) >= as.Date("2019-10-01"), ]
Forbrugdf <- Forbrugdf %>% filter(FORMAAAL != "CPT I alt")

# Finder hvad der bruges mest på i 2025
Forbrug_2025 <- Forbrugdf %>% filter(TID == "2025-07-01")

# Alternativt gennemsnit
Gennemsnit_2025 <- Forbrugdf %>%
  filter(format(TID, "%Y") == "2025") %>%
  group_by(FORMAAAL) %>%
  summarise(Forbrugdf = mean(value))

# Finder ændring fra 2020 til 2025
F2020 <- Forbrugdf[Forbrugdf$TID == as.Date("2020-01-01"), c("FORMAAAL", "value")]
F2025 <- Forbrugdf[Forbrugdf$TID == as.Date("2025-01-01"), c("FORMAAAL", "value")]
Ændringer <- merge(F2020, F2025, by = "FORMAAAL")
Ændringer$pct_change <- 100 * (Ændringer$value.y / Ændringer$value.x - 1)



#############################################
###############  OPGAVE 4.4  ################
#############################################

# Konstruerer DI's forbrugertillidsindikator (gennemsnit af 4 spørgsmål)
DIquery <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."
  ),
  Tid = "*"
)

DIFTI <- dst_get_data(table = "FORV1", query = DIquery, lang = "da")
DIFTI <- DIFTI[as.Date(DIFTI$TID) > as.Date("1999-12-01"), ]
DIFTI <- DIFTI[as.Date(DIFTI$TID) < as.Date("2025-09-01"), ]
# Finder gennemsnit så vi har indikatoren
DIFTI <- DIFTI %>%
  group_by(TID) %>%
  summarise(Indikator = mean(value))


# Henter DST’s forbrugertillidsindikator
DSTquery <- list(INDIKATOR = "Forbrugertillidsindikatoren", Tid = "*")
DSTFTI <- dst_get_data(table = "FORV1", query = DSTquery, lang = "da")
DSTFTI <- DSTFTI[as.Date(DSTFTI$TID) > as.Date("1999-12-01"), ]
DSTFTI <- DSTFTI[as.Date(DSTFTI$TID) < as.Date("2025-09-01"), ]


# Omregner begge indikatorer til kvartaler
DSTkvartal <- data.frame(Kvartal = character(), Værdi = double())
for (i in seq_len(nrow(DSTFTI))) {
  if (i %% 3 == 0) {
    mean_score <- mean(DSTFTI$value[(i-2):i])
    tmp <- data.frame(Kvartal = DSTFTI$TID[i], Værdi = mean_score)
    DSTkvartal <- rbind(DSTkvartal, tmp)
  }
}

DIkvartal <- data.frame(Kvartal = character(), Værdi = double())
for (i in seq_len(nrow(DIFTI))) {
  if (i %% 3 == 0) {
    mean_score <- mean(DIFTI$Indikator[(i-2):i])
    tmp <- data.frame(Kvartal = DIFTI$TID[i], Værdi = mean_score)
    DIkvartal <- rbind(DIkvartal, tmp)
  }
}


# Henter forbrug igen til regressionerne
forbrugquery2 <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  TID = "*"
)
Forbrugdf2 <- dst_get_data(table = "NKHC021", query = forbrugquery2, lang = "da")
Forbrugdf2 <- Forbrugdf2[as.Date(Forbrugdf2$TID) >= as.Date("1999-10-01"), ]
Forbrugdf2 <- Forbrugdf2 %>% filter(FORMAAAL != "CPT I alt")

# Fjerner unødvendige kolonner
Forbrugdf2 <- Forbrugdf2[, -c(2, 3)]


# Laver wide-format af forbrugsgrupperne
Forbrug_wide <- pivot_wider(
  Forbrugdf2,
  id_cols = TID,
  names_from = FORMAAAL,
  values_from = value
)


# Joiner alt i én dataframe
join <- cbind(Forbrug_wide, DIkvartal)
join <- cbind(join, DSTkvartal)

colnames(join)[20] <- "DST"
colnames(join)[18] <- "DI"



# Nu til simple linæere regressioner

# Definerer 15 forbrugsgrupper
forbrugs_grupper <- c(
  "CPA Fødevarer mv.",
  "CPB Drikkevarer og tobak mv.",
  "CPC Beklædning og fodtøj",
  "CPD Boligbenyttelse",
  "CPE Elektricitet, fjernvarme og andet brændsel",
  "CPF Boligudstyr, husholdningstjenester mv.",
  "CPG Medicin, lægeudgifter o.l.",
  "CPH Køb af køretøjer",
  "CPI Drift af køretøjer og transporttjenester",
  "CPJ Information og kommunikation",
  "CPK Fritid, sport og kultur",
  "CPL Undervisning",
  "CPM Restauranter og hoteller",
  "CPN Forsikring og finansielle tjenester",
  "CPO Andre varer og tjenester"
)

# Laver lister til regressioner
resultater_dst <- vector("list", length(forbrugs_grupper))
names(resultater_dst) <- forbrugs_grupper

resultater_di <- vector("list", length(forbrugs_grupper))
names(resultater_di) <- forbrugs_grupper


# Kører regressioner for DST og DI
for (grp in forbrugs_grupper) {
  
  form_dst <- reformulate("DST", response = grp)
  resultater_dst[[grp]] <- summary(lm(form_dst, data = join))
  
  form_di <- reformulate("DI", response = grp)
  resultater_di[[grp]] <- summary(lm(form_di, data = join))
}

# Eksempel på adgang
resultater_dst[["CPA Fødevarer mv."]]
resultater_di [["CPA Fødevarer mv."]]


# Test
summary(lm(CPA Fødevarer mv. ~ DST, data = join))

# Finder p-værdien for DST-koefficienten i hver model
dst_p_values <- sapply(resultater_dst, function(x) {
  if (inherits(x, "summary.lm")) {
    return(coef(x)[2, 4])   # p-værdi
  } else {
    return(NA)              # marker fejlende modeller
  }
})


di_p_values <- sapply(resultater_di, function(x) {
  if (inherits(x, "summary.lm")) {
    return(coef(x)[2, 4])
  } else {
    return(NA)
  }
})

sort(dst_p_values)
sort(di_p_values)


#Opgave 5
#Opgave 5.1


library(tidyverse)
library(eurostat)

# ---------------------------------------
# 1. Hent data fra Eurostat
# ---------------------------------------
dfForbrugclean <- get_eurostat(
  id = "namq_10_gdp",
  filters = list(
    unit    = "CLV_PCH_SM",
    s_adj   = "SCA",
    na_item = "P31_S14"
  )
)

# ---------------------------------------
# 2. Filtrer til 9 lande + perioden 2000Q1–2025Q3
# Tidsfiltrering sker direkte på datoformatet
# ---------------------------------------
lande <- c("DK","BE","NL","SE","AT","DE","FR","IT","ES")

dfForbrugclean <- dfForbrugclean %>%
  filter(
    geo %in% lande,
    time >= as.Date("2000-01-01"),  # 2000Q1
    time <= as.Date("2025-07-01")   # 2025Q3 = juli
  )

# ---------------------------------------
# 3. Plot 
# ---------------------------------------
ggplot(dfForbrugclean, aes(x = time, y = values, color = geo)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +   # baseline ved 0 %
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(-25, 25, by = 2)
  ) +
  labs(
    title = "Kvartalsvis årlig realvækst i husholdningernes forbrugsudgifter (2000Q1–2025Q3)",
    x = "Tid",
    y = "Vækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.caption = element_text(hjust = 0, face = "italic")
  )

#Opgave 5.2

# Gennemsnitlig kvartalsvis årlig realvækst pr. land (2000Q1–2025Q3)
gennemsnit_vækst <- dfForbrugclean %>% 
  filter(geo %in% c("DK","BE","NL","SE","AT","DE","FR","IT","ES")) %>%
  group_by(geo) %>%
  summarise(mean_growth = mean(values, na.rm = TRUE)) %>%
  arrange(desc(mean_growth))
# Barplot af gennemsnitlig vækst (2000Q1–2025Q3)

ggplot(gennemsnit_vækst, aes(x = reorder(geo, mean_growth), 
                             y = mean_growth, 
                             fill = geo)) +
  geom_col() +
  geom_text(aes(label = round(mean_growth, 2)),   # tal på søjler
            hjust = -0.1, 
            size = 3) +
  coord_flip() +
  labs(
    title = "Gennemsnitlig kvartalsvis realvækst i husholdningernes forbrug (2000Q1–2025Q3)",
    x = "Land",
    y = "Gennemsnitlig vækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.position = "none"
  ) +
  expand_limits(y = max(gennemsnit_vækst$mean_growth) + 0.3)   # giver plads til teksten

#Opgave 5.3

# Coronaperiode som Eurostat-datoer (kvartaler)
corona_dates <- as.Date(c(
  "2020-01-01","2020-04-01","2020-07-01","2020-10-01",
  "2021-01-01","2021-04-01","2021-07-01","2021-10-01",
  "2022-01-01","2022-04-01"))

# Gennemsnit med corona (fra eksisterende dfForbrugclean)
gennemsnit_med <- dfForbrugclean %>%
  filter(geo %in% lande) %>%
  group_by(geo) %>%
  summarise(med_corona = mean(values, na.rm = TRUE))

# Fjern coronaperioden
df_no_corona <- dfForbrugclean %>%
  filter(!(time %in% corona_dates))

# Gennemsnit uden corona
gennemsnit_uden <- df_no_corona %>%
  filter(geo %in% lande) %>%
  group_by(geo) %>%
  summarise(uden_corona = mean(values, na.rm = TRUE))

# Sammenlign med og uden corona
df_sammenligning <- gennemsnit_med %>%
  left_join(gennemsnit_uden, by = "geo") %>%
  mutate(
    absforskel = uden_corona - med_corona,
  ) %>%
  arrange(desc(absforskel))

#Plot

df_plot <- df_sammenligning

ggplot(df_plot, aes(y = reorder(geo, absforskel))) +
  geom_segment(aes(x = med_corona, 
                   xend = uden_corona, 
                   yend = geo), 
               color = "grey70", linewidth = 1.2) +
  geom_point(aes(x = med_corona), color = "tomato", size = 3) +
  geom_point(aes(x = uden_corona), color = "steelblue", size = 3) +
  
  # --- Forklaring på plottet ---
  annotate("point", x = max(df_plot$uden_corona) + 0.15, y = 1, 
           color = "tomato", size = 3) +
  annotate("text", x = max(df_plot$uden_corona) + 0.20, y = 1, 
           label = "Med corona", hjust = 0, size = 3.5) +
  
  annotate("point", x = max(df_plot$uden_corona) + 0.15, y = 0.5, 
           color = "steelblue", size = 3) +
  annotate("text", x = max(df_plot$uden_corona) + 0.20, y = 0.5, 
           label = "Uden corona", hjust = 0, size = 3.5) +
  # --------------------------------

labs(
  title = "Effekt af coronaperioden på forbrugsvæksten",
  x = "Gennemsnitlig vækst (%)",
  y = "Land",
  caption = "Kilde: Eurostat"
) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  ) +
  expand_limits(x = max(df_plot$uden_corona) + 0.5)

#Opgave 5.4

library(eurostat)
library(tidyverse)

# -----------------------------
# HENT NYE DATA (hele datasættet)
# -----------------------------
dfForbrug_ny <- get_eurostat(
  id = "namq_10_gdp",
  filters = list(
    unit    = "CLV_PCH_SM",
    s_adj   = "SCA",
    na_item = "P31_S14"
  )
)

# Konverter tid til Date
dfForbrug_ny$time <- as.Date(dfForbrug_ny$time)


# -----------------------------
# EU27 lanekoder
# -----------------------------
alle_eurostat_lande <- c(
  "BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY",
  "LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE",  # EU27
  
  "NO","IS","CH","LI",   # EFTA
  
  "UK",                  # Tidligere EU-medlem
  
  "AL","MK","ME","RS","BA","TR","XK"  # Balkan + kandidater + Kosovo
)



# -----------------------------
# FILTRER coronaperioden 2020Q1–2025Q3 for EU27
# -----------------------------
df_5_4 <- dfForbrug_ny %>%
  filter(
    geo %in% alle_eurostat_lande,
    time >= as.Date("2020-01-01"),
    time <= as.Date("2025-07-01")    # 2025 Q3
  ) %>%
  arrange(time)


# -----------------------------
# PLOT af coronaperioden for EU27
# -----------------------------
df_5_4 %>% 
  ggplot(aes(x = time, y = values, color = geo, group = geo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  scale_y_continuous(
    breaks = seq(-25, 25, by = 1)     # flere procenter
  ) +
  scale_x_continuous(
    breaks = unique(df_5_4$time),
    labels = unique(df_5_4$time)
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  labs(
    title = "Kvartalsvis årlig realvækst i husholdningernes forbrug – EU27 (2020Q1–2025Q3)",
    x = "Tid",
    y = "Vækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 7),
    axis.text.y = element_text(size = 9),
    plot.caption = element_text(hjust = 0, face = "italic")
  )


# -----------------------------
# GENNEMSNITLIG VÆKST per EU-land 2020Q1–2025Q3
# -----------------------------
OPG5_4_gennemsnit <- df_5_4 %>%
  group_by(geo) %>%
  summarise(mean_growth = mean(values, na.rm = TRUE)) %>%
  arrange(desc(mean_growth))

OPG5_4_gennemsnit

# 1. Udvælg top 5 lande efter gennemsnitlig vækst
top5_lande <- OPG5_4_gennemsnit %>%
  top_n(5, mean_growth) %>%
  pull(geo)

top5_lande

# 2. Filtrer datasæt til kun top 5 lande
df_top5 <- df_5_4 %>%
  filter(geo %in% top5_lande)

# 3. Plot for de 5 lande
df_top5 %>% 
  ggplot(aes(x = time, y = values, color = geo, group = geo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  
  scale_y_continuous(
    breaks = seq(-25, 25, by = 1)
  ) +
  
  scale_x_continuous(
    breaks = unique(df_top5$time),
    labels = unique(df_top5$time)
  ) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  
  labs(
    title = "Top 5 EU-lande med højeste gennemsnitlige forbrugsvækst\n(2020Q1–2025Q3)",
    x = "Tid",
    y = "Vækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, face = "italic")
  )

# 1. Find de 5 lande med laveste gennemsnitlige vækst
bottom5_lande <- OPG5_4_gennemsnit %>%
  arrange(mean_growth) %>%
  slice(1:5) %>%
  pull(geo)

bottom5_lande

# 2. Plot udviklingen for disse 5 lande
df_5_4 %>%
  filter(geo %in% bottom5_lande) %>%
  ggplot(aes(x = time, y = values, color = geo, group = geo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  
  scale_y_continuous(
    breaks = seq(-25, 25, by = 1)
  ) +
  scale_x_continuous(
    breaks = unique(df_5_4$time),
    labels = unique(df_5_4$time)
  ) +
  
  labs(
    title = "Bund 5 europæiske lande – gennemsnitlig forbrugsvækst (2020Q1–2025Q3)",
    x = "Tid",
    y = "Vækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(hjust = 0, face = "italic")
  )



