#Opgave 1.1
`newhomes.(1)` <- read.csv("~/Downloads/newhomes (1).csv")
newhomes <- `newhomes.(1)`

hus1 <- newhomes[grep("Frigrunden 9", newhomes$vej), ]

hus2 <- newhomes[grep("Hostrupvej 13A", newhomes$vej), ]

#Opgave 1.2
furesø1 <- newhomes[grep("Kastanie Alle 163520 Farum", newhomes$vej), ]

furesø2 <- newhomes[grep("Åvej 15Hareskov, 3500 Værløse", newhomes$vej), ]

#Opgave 1.3
newhomes <- newhomes[-1, ]

colSums(is.na(newhomes))

#Opgave 1.4
#Check Docs


#Opgave 2.1
#Datapreparation
huseFuresø <- newhomes[grep("3500|3520|3540", newhomes$zip), ]
huseTønder <- newhomes[grep("6270|6261|6280|6270|6780|6340", newhomes$zip), ]
huseFuresø <- huseFuresø[grep("Villa", huseFuresø$type), ]
huseTønder <- huseTønder[grep("Villa", huseTønder$type), ]

#numeric
huseFuresø$pris    <- as.numeric(gsub("[^0-9]", "", huseFuresø$pris))
huseFuresø$kvm     <- as.numeric(gsub("[^0-9]", "", huseFuresø$kvm))
huseFuresø$ejerudg <- as.numeric(gsub("[^0-9]", "", huseFuresø$ejerudg))
huseFuresø$grund   <- as.numeric(gsub("[^0-9]", "", huseFuresø$grund))
huseFuresø$liggetid <- as.numeric(gsub("[^0-9]", "", huseFuresø$liggetid))
huseFuresø$alder    <- as.numeric(gsub("[^0-9]", "", huseFuresø$alder))


huseTønder$pris    <- as.numeric(gsub("[^0-9]", "", huseTønder$pris))
huseTønder$kvm     <- as.numeric(gsub("[^0-9]", "", huseTønder$kvm))
huseTønder$ejerudg <- as.numeric(gsub("[^0-9]", "", huseTønder$ejerudg))
huseTønder$grund   <- as.numeric(gsub("[^0-9]", "", huseTønder$grund))
huseTønder$liggetid <- as.numeric(gsub("[^0-9]", "", huseTønder$liggetid))
huseTønder$alder    <- as.numeric(gsub("[^0-9]", "", huseTønder$alder))


#Indsætter en ekstra kolonne med kvmpris
huseFuresø$kvmpris <- huseFuresø$pris/huseFuresø$kvm
huseTønder$kvmpris <- huseTønder$pris/huseTønder$kvm
#Indsætter en ekstra kolonne med alder
huseFuresø$alder <- 2025 - huseFuresø$alder
huseTønder$alder <- 2025 - huseTønder$alder

#Beskrivende statistik
Furesømean <- mean(huseFuresø$kvmpris)
Tøndermean <- mean(huseTønder$kvmpris)

Furesømin <- min(huseFuresø$kvmpris)
Tøndermin <- min(huseTønder$kvmpris)

Furesømaks <- max(huseFuresø$kvmpris)
Tøndermaks <- max(huseTønder$kvmpris)

Furesømedian <- median(huseFuresø$kvmpris)
Tøndermedian <- median(huseTønder$kvmpris)

Furesøsd <- sd(huseFuresø$kvmpris)
Tøndersd <- sd(huseTønder$kvmpris)

#Samlet df
stats_furesø <- data.frame(
  kommune = "Furesø",
  stat    = c("Mean", "Min", "Max", "Median", "SD"),
  value   = c(Furesømean, Furesømin, Furesømaks, Furesømedian, Furesøsd)
)

stats_tønder <- data.frame(
  kommune = "Tønder",
  stat    = c("Mean", "Min", "Max", "Median", "SD"),
  value   = c(Tøndermean, Tøndermin, Tøndermaks, Tøndermedian, Tøndersd)
)

stats_df <- rbind(stats_furesø, stats_tønder)

#Plot

library(ggplot2)

ggplot(stats_df, aes(x = stat, y = value, fill = kommune)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
             geom_text(
               aes(label = round(value, 0)),
               position = position_dodge(width = 0.8),
                                         vjust = -0.3,
                                         size = 7   # før: 3.5
               ) +
                 scale_fill_manual(
                   values = c("Furesø" = "#1f77b4",   # blå
                              "Tønder" = "#ff7f0e")   # orange
                 ) +
                 labs(
                   title = "Villapriser i Furesø og Tønder (kr. pr. m²)",
                   subtitle = "Mean, median, standardafvigelse, minimum og maksimum for kvm-priser",
                   x = "Statistik",
                   y = "Pris pr. m²",
                   fill = "Kommune"
                 ) +
                 expand_limits(y = 0) +
                 theme_minimal(base_size = 26) +      # før: 13
                 theme(
                   plot.title    = element_text(face = "bold", size = 32),         # før: 16
                   plot.subtitle = element_text(size = 22, margin = margin(b = 10)), # før: 11
                   axis.title.x  = element_text(margin = margin(t = 10)),
                   axis.title.y  = element_text(margin = margin(r = 10)),
                   legend.position = "top",
                   legend.title = element_text(size = 24),   # større legend titel
                   legend.text  = element_text(size = 22)    # større legend tekst
                 )
               





#Opgave 2.2
cor(huseFuresø$kvm,huseFuresø$pris)
cor(huseTønder$kvm,huseTønder$pris)

#Opgave 2.3
furesømodel1 <- lm(kvmpris ~ alder, data = huseFuresø)
summary(furesømodel1)

furesømodel2 <- lm(kvmpris ~ ejerudg, data = huseFuresø)
summary(furesømodel2)

furesømodel3 <- lm(kvmpris ~ liggetid, data = huseFuresø)
summary(furesømodel3)

furesømodel4 <- lm(kvmpris ~ grund, data = huseFuresø)
summary(furesømodel4)


# 1. Variabler vi vil lave simple regressioner for
vars <- c("alder", "ejerudg", "liggetid", "grund")

# 2. Data til plot – long format + pæne labels på variabler
plotdata <- huseFuresø %>% 
  select(kvmpris, all_of(vars)) %>% 
  pivot_longer(
    cols = all_of(vars),
    names_to = "variabel",
    values_to = "x"
  ) %>%
  mutate(
    variabel = recode(
      variabel,
      grund    = "Grund - Multiple R2 = 0.000309",
      alder    = "Alder - Multiple R2 = 0.0928",
      ejerudg  = "Ejerudgift - Multiple R2 = 0.3093",
      liggetid = "Liggetid - Multiple R2 = 1.48e-05"
    )
  )

# 3. Plot alle simple regressioner i ét samlet plot
ggplot(plotdata, aes(x = x, y = kvmpris)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "blue") +
  facet_wrap(~ variabel, scales = "free_x") +
  labs(
    title = "Simple regressioner for Furesø",
    subtitle = "Pris pr. m² som funktion af udvalgte forklarende variabler",
    x = "Forklarende variabel",
    y = "Pris pr. m²"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(face = "bold", size = 18),
    axis.title.x  = element_text(face = "bold", size = 18),
    axis.title.y  = element_text(face = "bold", size = 18),
    axis.text.x   = element_text(face = "bold", size = 14),
    axis.text.y   = element_text(face = "bold", size = 14),
    strip.text    = element_text(face = "bold", size = 20)
  )




#TØNDER
tøndermodel1 <- lm(kvmpris ~ alder, data = huseTønder)
summary(tøndermodel1)

tøndermodel2 <- lm(kvmpris ~ ejerudg, data = huseTønder)
summary(tøndermodel2)

tøndermodel3 <- lm(kvmpris ~ liggetid, data = huseTønder)
summary(tøndermodel3)

tøndermodel4 <- lm(kvmpris ~ grund, data = huseTønder)
summary(tøndermodel4)


plotdata_t <- huseTønder %>% 
  select(kvmpris, all_of(vars)) %>% 
  pivot_longer(
    cols = all_of(vars),
    names_to  = "variabel",
    values_to = "x"
  ) %>% 
  mutate(
    variabel = recode(
      variabel,
      grund    = "Grund - Multiple R2 = 0.009534",
      alder    = "Alder - Multiple R2 = 0.03241",
      ejerudg  = "Ejerudgift - Multiple R2 = 0.1332",
      liggetid = "Liggetid - Multiple R2 = 1.919e-07"
    )
  )

# 3. Plot alle simple regressioner i ét samlet plot for Tønder
ggplot(plotdata_t, aes(x = x, y = kvmpris)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2, color = "blue") +
  facet_wrap(~ variabel, scales = "free_x") +
  labs(
    title    = "Simple regressioner for Tønder",
    subtitle = "Pris pr. m² som funktion af udvalgte forklarende variabler",
    x        = "Forklarende variabel",
    y        = "Pris pr. m²"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(face = "bold", size = 18),
    axis.title.x  = element_text(face = "bold", size = 18),
    axis.title.y  = element_text(face = "bold", size = 18),
    axis.text.x   = element_text(face = "bold", size = 14),
    axis.text.y   = element_text(face = "bold", size = 14),
    strip.text    = element_text(face = "bold", size = 20)
  )

ggplot(huseTønder, aes(x = alder, y = kvmpris)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Corrplot
library(corrplot)
cor_f <- cor(huseFuresø[, c("kvmpris", "alder", "ejerudg", "liggetid", "grund")],
             use = "complete.obs")[1, -1]

cor_t <- cor(huseTønder[, c("kvmpris", "alder", "ejerudg", "liggetid", "grund")],
             use = "complete.obs")[1, -1]

# 2. Saml i ét data frame til heatmap
cordf <- data.frame(
  variable = names(cor_f),
  Furesø   = cor_f,
  Tønder   = cor_t
) |>
  pivot_longer(cols = c("Furesø", "Tønder"),
               names_to = "kommune",
               values_to = "correlation")

# 3. Plot som samlet heatmap
ggplot(cordf, aes(x = kommune, y = variable, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 6) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  scale_x_discrete(
    labels = c(
      "Furesø" = "Furesø – pris pr. m²",
      "Tønder" = "Tønder – pris pr. m²"
    )
  ) +
  labs(
    title = "Korrelationsplot for Furesø og Tønder",
    subtitle = "Sammenhæng mellem pris pr. m² og udvalgte variable",
    x = "",
    y = "Variabel"
  ) +
  theme_minimal(base_size = 16)

#Opgave 3.1
roll_dice <- function(n) {
  rolls <- sample(1:6, size = 25000, replace = TRUE)
}
fives <- sum(rolls == 5) 
table(rolls)

fives / 25000 = 0.16904

#Opgave 3.2
roll_six <- function(n) {
  sum(sample(1:6, size = 6, replace = TRUE))
}
results <- replicate(10000, roll_six())

ggplot(data = data.frame(sum = results), aes(x = sum)) +
  geom_bar()

#opgave 3.3
resultsMil <- replicate(1000000, roll_six())

ggplot(data = data.frame(sum = resultsMil), aes(x = sum)) +
  geom_bar()

#Opgave 3.4

random <- function() {
  sample(c(1, 2, 3, 5, 6))  
}
kol1 <- 2:6 
kol2 <- random()
matrix <- cbind(kol1, kol2)
view(matrix)

#Opgave 4.1
library(dkstat)
library(tidyverse)
library(corrplot)

#Hent headers
FU02 <- dst_meta(table = "FU02", lang = "da")

#Find de rigtige querys
FU02$variables
FU02$values$KONSUMGRP
FU02$values$PRISENHED
FU02$values$Tid

#Lav query liste
my_query <- list(
  KONSUMGRP = c("02.1.1.1 Spiritus og likør",
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
                "02.1.2.3 Hedvin",
                "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                "02.1.3.1 Pilsnerøl, guldøl",
                "02.1.3.2 Andre alkoholholdige øl",
                "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
                "02.1.3.4 Øl-baserede drikkevarer"
  ),
  PRISENHED = "Faste priser",
  Tid = seq(2000, 2022, by = 1)
)

#Hent tabel
Alkoholforbrug <- dst_get_data(table = "FU02", query = my_query, lang = "da")

#Renset for tal og punktum

Alkoholforbrug$KONSUMGRP <- gsub("[0-9\\.]", "", Alkoholforbrug$KONSUMGRP)

#Lav ggplot

ggplot(Alkoholforbrug, aes(x = TID, y = value, color = KONSUMGRP)) +
  geom_line() +
  scale_y_log10() +
  labs(
    title = "Udvikling af alkoholforbrug i Danmark (2000–2022)",
    x = "År",
    y = "Forbrug (log-skala)",
    color = "Forbrugsgruppe",
    caption = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal()



##Opgave 4.2##
df_wide <- Alkoholforbrug %>%
  select(TID, KONSUMGRP, value) %>%
  pivot_wider(names_from = KONSUMGRP, values_from = value)

names(df_wide) <- gsub("[0-9\\.]", "", names(df_wide))

library(corrplot)

# Beregn korrelationsmatrix
cor_matrix <- cor(df_wide[, -1], use = "pairwise.complete.obs")

# Lav pænt corrplot
corrplot(
  cor_matrix,
  method = "color",              # flot farveskala
  type = "upper",                # kun øverste trekant
  addCoef.col = "black",         # viser korrelationstal
  tl.col = "black",              # tekstfarve på labels
  tl.cex = 0.8,                  # label-størrelse
  number.cex = 0.7,              # talstørrelse
  col = colorRampPalette(c("red", "white", "blue"))(200), 
  mar = c(1, 1, 4, 1)            # margin til titel
)

# Titel
title("Korrelationsmatrix for alkoholforbrug i Danmark (2000–2022)", 
      line = 2.5, cex.main = 1.4)

# Kilde
mtext("Kilde: Danmarks Statistik", side = 1, line = 3, cex = 0.8)



##5.1##
#Lav kolonner og saml i df

Klasse <- rep(c("A", "B", "C", "D"), each = 9)
Uge <- rep(1:9, times = 4)
Score <- runif(36, min = 0, max = 10)

df <- data.frame(Klasse, Uge, Score)

##5.2##
#Hvis det er hver tredje, der skal loopes bør det så ikke være en 12x3 df?
n <- nrow(df)

# tom 0x3 dataframe til resultatet
df_loop <- data.frame(
  Klasse = character(),
  Uge    = integer(),
  Score  = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:n) {
  # hver tredje observation
  if (i %% 3 == 0) {
    ny_klasse <- df$Klasse[i]
    ny_uge    <- df$Uge[i]
    ny_score  <- mean(df$Score[(i-2):i])
    
    df_loop <- rbind(
      df_loop,
      data.frame(Klasse = ny_klasse,
                 Uge    = ny_uge,
                 Score  = ny_score)
    )
  }
}

##5.3##
#pivot wider
df_wider <- pivot_wider(df_loop,
                        names_from = Klasse,
                        values_from = Score
)
