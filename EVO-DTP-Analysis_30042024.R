# EVO-DTP-Analysis
#
# copyright (c) 2024 - MHB
# written by: Jonas Janik Ralf Koberschinski
#
# last modified Apr, 2024
# first written Sep, 2023


# Data available: 
# Koberschinski J. EVO_Study_DTP_300424. figshare. Dataset. 2024. https://doi.org/10.6084/m9.figshare.25723758.v1

## First things first ##
library(tidyverse)
library(effectsize)
library(vcd)
library(ggstatsplot)


## Import Data ##
library(readxl)
Wipra_Daten_aufgearbeitet <- read_excel("Wipra_Daten_aufgearbeitet.xls", 
                                        col_types = c("text", "date", "numeric", "text", 
                                                      "text", "text", "text", "text", 
                                                      "date", "numeric", "text", "numeric", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "text", "text", "text", "text", 
                                                      "text", "text", "date", "text", "date", 
                                                      "text", "date"), na = "1899-12-31")
View(Wipra_Daten_aufgearbeitet)
´

## Analysis ##


# Überblick: Alter
ggplot(daten) + 
  geom_histogram(mapping =  aes(x = Alter))

summary(daten$Alter)


# Überblick: Geschlecht
daten %>%
  count(Geschlecht) %>%
  mutate(Prop = n / sum(n))

ggplot(daten) + 
  geom_bar(mapping =  aes(x = Geschlecht))

# Geschlecht x Impfstatus
# Kombiniert in einem Schritt
assocstats(table(daten$Geschlecht, daten$`Impfstatus Diphterie`))
assocstats(table(daten$Geschlecht, daten$`Impfstatus Tetanus`))
assocstats(table(daten$Geschlecht, daten$`Impfstatus Pertussis`))
# Diphterie und Tetanus signifikant

# Spaßigshalber mit Fisher's Exact Test:
fisher.test(table(daten$Geschlecht, daten$`Impfstatus Diphterie`), alternative = "two.sided") 
fisher.test(table(daten$Geschlecht, daten$`Impfstatus Tetanus`), alternative = "two.sided") 
# Ebenfalls signifikant.


# Überblick: Vorlage Impfausweis
daten %>%
  count(`vorlage Impfausweis`) %>%
  mutate(Prop = n / sum(n))

ggplot(daten) + 
  geom_bar(mapping =  aes(x = `vorlage Impfausweis`))


# Überblick: MVZ
daten %>%
  count(MVZ) %>%
  mutate(Prop = n / sum(n))


ggplot(daten) + 
  geom_bar(mapping =  aes(x = MVZ))

# MVZ x Impfstatus
assocstats(table(daten$MVZ, daten$`Impfstatus Diphterie`))
assocstats(table(daten$MVZ, daten$`Impfstatus Tetanus`))
assocstats(table(daten$MVZ, daten$`Impfstatus Pertussis`))
# Insignifikant

#Plot for fun 
ggbarstats(
  data = daten,
  x = `MVZ`,
  y = `Impfstatus Diphterie`,
  label = "both"
)
ggbarstats(
  data = daten,
  x = `MVZ`,
  y = `Impfstatus Tetanus`,
  label = "both"
)
ggbarstats(
  data = daten,
  x = `MVZ`,
  y = `Impfstatus Pertussis`,
  label = "both"
)

# Codierte Diagnose x Impfstatus
assocstats(table(daten$`Codierte Diagnose`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Codierte Diagnose`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Codierte Diagnose`, daten$`Impfstatus Pertussis`))
# Pearson bei Pertussis signifikant


# Versuch der Gruppierung. Scheint nicht zu funktionieren.
daten |> 
  select(`Codierte Diagnose`, `Impfstatus Tetanus`) |> 
  group_by(`Codierte Diagnose` == "Multiples Myelom") |> 
  table()


# Diagnose Subgruppe x Impfstatus
assocstats(table(daten$`Diagnose Subgruppe`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Diagnose Subgruppe`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Diagnose Subgruppe`, daten$`Impfstatus Pertussis`))
#Likelihood bei Pertussis signifikant?





# Stadium x Impfstatus
assocstats(table(daten$Stadium, daten$`Impfstatus Diphterie`))
assocstats(table(daten$Stadium, daten$`Impfstatus Tetanus`))
assocstats(table(daten$Stadium, daten$`Impfstatus Pertussis`))
# nicht signifikant

# Codierte Therapie x Impfstatus
assocstats(table(daten$`Codierte Therapie`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Codierte Therapie`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Codierte Therapie`, daten$`Impfstatus Pertussis`))
# nicht signifikant

# Stadium x Impfstatus
assocstats(table(daten$Stadium, daten$`Impfstatus Diphterie`))
assocstats(table(daten$Stadium, daten$`Impfstatus Tetanus`))
assocstats(table(daten$Stadium, daten$`Impfstatus Pertussis`))
# nicht signifikant

#Plot for fun 
ggbarstats(
  data = daten,
  x = `Stadium`,
  y = `Impfstatus Diphterie`,
  label = "both"
)
ggbarstats(
  data = daten,
  x = `Stadium`,
  y = `Impfstatus Tetanus`,
  label = "both"
)
ggbarstats(
  data = daten,
  x = `Stadium`,
  y = `Impfstatus Pertussis`,
  label = "both"
)


# Codierte Vorherapie x Impfstatus
assocstats(table(daten$`Codierte Vortherapie`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Codierte Vortherapie`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Codierte Vortherapie`, daten$`Impfstatus Pertussis`))
# nicht signifikant

#Diagnose onko2 x Impfstatus
assocstats(table(daten$`Diagnose onko2`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Diagnose onko2`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Diagnose onko2`, daten$`Impfstatus Pertussis`))
# nicht signifikant





# Kard. VE x Impfstatus
assocstats(table(daten$`Kard. VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Kard. VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Kard. VE`, daten$`Impfstatus Pertussis`))
# Diphterie und Tetanus signifikant

# Pulm VE x Impfstatus
assocstats(table(daten$`Pulm VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Pulm VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Pulm VE`, daten$`Impfstatus Pertussis`))
# nicht signifikant

# Neph VE x Impfstatus
assocstats(table(daten$`Neph VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Neph VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Neph VE`, daten$`Impfstatus Pertussis`))
# nicht signifikant

# Hep. VE x Impfstatus
assocstats(table(daten$`Hep. VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Hep. VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Hep. VE`, daten$`Impfstatus Pertussis`))
# nicht signifikant

# GI VE x Impfstatus


# Vask. VE x Impfstatus
assocstats(table(daten$`Vask. VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Vask. VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Vask. VE`, daten$`Impfstatus Pertussis`))
# nicht signifikant

#Chron. Infekt VE x Impfstatus
assocstats(table(daten$`Chron. Infekt VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Chron. Infekt VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Chron. Infekt VE`, daten$`Impfstatus Pertussis`))
# nicht signifikant

#Autoimmune VE x Impfstatus
assocstats(table(daten$`Autoimmune VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Autoimmune VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Autoimmune VE`, daten$`Impfstatus Pertussis`))
# DTP signifikant


#Plot for fun 
ggbarstats(
  data = daten,
  x = `Autoimmune VE`,
  y = `Impfstatus Diphterie`,
  label = "both"
)
ggbarstats(
  data = daten,
  x = `Autoimmune VE`,
  y = `Impfstatus Tetanus`,
  label = "both"
)
ggbarstats(
  data = daten,
  x = `Autoimmune VE`,
  y = `Impfstatus Pertussis`,
  label = "both"
)


# Spaßigshalber mit Fisher's Exact Test:
fisher.test(table(daten$`Autoimmune VE`, daten$`Impfstatus Diphterie`), alternative = "two.sided") 
fisher.test(table(daten$`Autoimmune VE`, daten$`Impfstatus Tetanus`), alternative = "two.sided") 
fisher.test(table(daten$`Autoimmune VE`, daten$`Impfstatus Tetanus`), alternative = "two.sided")
# Nur Diphterie signifikant... Rel?
# Allerdings ist die Stichprobengröße natürlich >20.


# Noch mal mit Yates' continuity correction:
chisq.test(daten$`Autoimmune VE`, daten$`Impfstatus Diphterie`)
chisq.test(daten$`Autoimmune VE`, daten$`Impfstatus Tetanus`)
chisq.test(daten$`Autoimmune VE`, daten$`Impfstatus Pertussis`)
# Mit Yates' continuity correction nicht signikfant.
# Allerdings ist die Stichprobengröße natürlich >50.


#Endokri VE x Impfstatus
assocstats(table(daten$`Endokri VE`, daten$`Impfstatus Diphterie`))
assocstats(table(daten$`Endokri VE`, daten$`Impfstatus Tetanus`))
assocstats(table(daten$`Endokri VE`, daten$`Impfstatus Pertussis`))
# nicht signifikant





#citation()
citation("tidyverse")
citation("rio") #not good for this study because the results are inconsistent with SPSS-data.
citation("effectsize")
citation("vcd") # most helpful
citation("ggstatsplot") #only for fun, but helpful for visulization

# "Dieses war der erste Streich, Doch der zweite folgt sogleich."
# Gratias tibi ago, domine. SDG!




##Appendix (allerlei extra Analysen zur letzten Überarbeitung)




# Diphteria x Cancer Type
# Load the tibble package
library(tibble)
library(vcd)

# Create the 2x2 tibble
D_tibble <- tibble(
  "Cancer Type" = c("Haematological cancers", "Solid tumours"),
  "D vaccinated" = c(43, 123),
  "D not vaccinated" = c(35, 169)
)

# Print the tibble
print(D_tibble) 

# Convert the tibble into a data frame
D_data_frame <- as.data.frame(D_tibble)

# Perform the Pearson's chi-squared test without Yates' correction
D_result <- chisq.test(D_data_frame[, c("D vaccinated", "D not vaccinated")], correct = FALSE)

# Print the result
print(D_result)



# Tetanus x Cancer Type
# Load the tibble package
library(tibble)
library(vcd)

# Create the 2x2 tibble
T_tibble <- tibble(
  "Cancer Type" = c("Haematological cancers", "Solid tumours"),
  "T vaccinated" = c(44, 131),
  "T not vaccinated" = c(34, 161)
)

# Print the tibble
print(T_tibble) 

# Convert the tibble into a data frame
T_data_frame <- as.data.frame(T_tibble)

# Perform the Pearson's chi-squared test without Yates' correction
T_result <- chisq.test(T_data_frame[, c("T vaccinated", "T not vaccinated")], correct = FALSE)

# Print the result
print(T_result)




# Pertussis x Cancer Type
# Load the tibble package
library(tibble)
library(vcd)

# Create the 2x2 tibble
P_tibble <- tibble(
  "Cancer Type" = c("Haematological cancers", "Solid tumours"),
  "P vaccinated" = c(35, 112),
  "P not vaccinated" = c(43, 180)
)

# Print the tibble
print(P_tibble) 

# Convert the tibble into a data frame
P_data_frame <- as.data.frame(P_tibble)

# Perform the Pearson's chi-squared test without Yates' correction
result <- chisq.test( P_data_frame[, c("P vaccinated", "P not vaccinated")], correct = FALSE)

# Print the result
print(result)


# Create a sample dataset (replace this with your actual data)
data <- data.frame(
  Variable1 = c("A", "B", "A", "C", "B", "C", "A", "A"),
  Variable2 = c("X", "Y", "X", "Z", "Y", "X", "Z", "Y")
)

# Create a contingency table
contingency_table <- table(data$Variable1, data$Variable2)

# Print the contingency table
print(contingency_table)


#Count Geschlecht
gender_counts <- table(Wipra_Daten_aufgearbeitet$Geschlecht)
print(gender_counts)

# Create a contingency table
D_contingency_table <- table(Wipra_Daten_aufgearbeitet$Geschlecht, Wipra_Daten_aufgearbeitet$`Impfstatus Diphterie`)
print(D_contingency_table)

T_contingency_table <- table(Wipra_Daten_aufgearbeitet$Geschlecht, Wipra_Daten_aufgearbeitet$`Impfstatus Tetanus`)
print(T_contingency_table)

P_contingency_table <- table(Wipra_Daten_aufgearbeitet$Geschlecht, Wipra_Daten_aufgearbeitet$`Impfstatus Pertussis`)
print(P_contingency_table)


daten %>%
  count(`Impfstatus Diphterie`) %>%
  mutate(Prop = n / sum(n))

daten %>%
  count(`Impfstatus Tetanus`) %>%
  mutate(Prop = n / sum(n))

daten %>%
  count(`Impfstatus Pertussis`) %>%
  mutate(Prop = n / sum(n))
