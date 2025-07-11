# Bibliothèques de traitement
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(labelled)

setwd("C:\\Users\\Julien\\Desktop\\A Suivi de la collecte de M SIDY\\Traitement_de_donnees\\Traitement\\")
# Base de sondage

sondage <- read_excel("Sondage.xlsx", sheet = "données")

# Base insertion

insertion <- read_excel("Questionnaire_Enquête_Insertion_Jeunes_Sénégal_2025_-_all_versions_-_labels_-_2025-06-12-20-04-16.xlsx")

# Traitement de la base insertion

### I. Traitement de la base de données Insertion Jeune P ##########################

dim(insertion)

# Nous avons 11696 observations et 77 colonnes

# Suppression des colonnes inutiles (créer par défaut par kobo)

names(insertion) # Nom des colonnes

# Suppression des 10 dernières variables (reste 67 variables)

insertion <- insertion |> subset(select = - c(68:77))

# Suppression des colonnes vides : il reste 48 colonnes

insertion <- insertion[, colSums(!is.na(insertion)) > 0]

dim(insertion)

# Création d'un dictionnaire de données pour avoir des noms cours

names(insertion) # Nom des colonnes

# Formatage des colonnes

# Noms des colonnes (originaux)

noms_originaux <- c(
  "Prénom et nom de l'enquêteur",
  "Numéro d'identification du dîplome du répondant",
  "Prénom",
  "Nom",
  "Etablissements",
  "Niveau diplôme",
  "Option",
  "TEL",
  "Annee",
  "Selection",
  "Joignabilité du répondant",
  "Veuillez préciser autre.",
  "Q11 : Dans quelle région résidez-vous actuellement ?",
  "Q12 : Avez-vous suivi des modules sur la création d’entreprise ?",
  "Q12bis : Avez-vous fait des séjours en entreprises ?",
  "Q13 : Avez-vous suivi des modules sur la recherche d’emploi ?",
  "Q14 : Avez-vous été accompagné pour votre insertion ?",
  "Q15 : Précisez les modalités",
  "Q15 : Précisez les modalités/Formation",
  "Q15 : Précisez les modalités/Incubateur",
  "Q15 : Précisez les modalités/Rédaction du CV",
  "Q15 : Précisez les modalités/Rédaction de la lettre de motivation",
  "Q15 : Précisez les modalités/Formation en entretiens d’embauche",
  "Q15 : Précisez les modalités/Formalisation administrative de la gestion des projets",
  "Q15 : Précisez les modalités/Formation juridique de la gestion des projets",
  "Q15 : Précisez les modalités/Formation sur la recherche de financement",
  "Q15 : Précisez les modalités/Élaboration du business plan",
  "Q15 : Précisez les modalités/Financement du projet",
  "Q15 : Précisez les modalités/Accompagnement pour formaliser l’entreprise",
  "Q15 : Précisez les modalités/Autre",
  "Veuillez préciser les autres modalités.",
  "Q16 : Statut d’emploi à 6 mois ?",
  "Q17 : Est-ce que le poste occupé est en adéquation avec le diplôme obtenu ?",
  "Q18 : Statut d’emploi à 12 mois ?",
  "Q19 : Est-ce que le poste occupé est en adéquation avec le diplôme obtenu ?",
  "Q20 : Statut actuel d’emploi ?",
  "Q21 :  Quel est le principal canal que vous avez utilisé pour obtenir votre emploi actuel ?",
  "Veuillez préciser l'autre canal.",
  "Q22 : Est-ce que le poste occupé est en adéquation avec le diplôme obtenu ?",
  "Q23 : Quel est le secteur de l’entreprise ou l’établissement au sein de laquelle ou duquel vous travaillez ?",
  "Q24 : L'entreprise ou l’établissement est dans quel secteur d’activités ?",
  "Veuillez préciser l'autre secteur d'activité.",
  "Q25 : Bénéficiez-vous d’une protection sociale (assurance maladie, Sécurité sociale/ IPRES/Inspection du travail) ?",
  "Q26 : Possédez-vous un registre de commerce (enregistrement juridique) ?",
  "Q27 : Possédez-vous un NINEA ?",
  "Q28 : Disposez-vous d’un système de comptabilité ?",
  "Veuillez préciser l'autre système de comptabilité.",
  "Q29 : L'entreprise ou l’établissement est dans quel secteur d’activités ?",
  "Q29.1 : Veuillez préciser le(s) secteur(s) d'activité."
)

# Noms courts correspondants
noms_courts <- c(
  "enq_nom",
  "id_diplome",
  "prenom",
  "nom",
  "etablissement",
  "niv_diplome",
  "option",
  "tel",
  "annee",
  "selection",
  "joignabilite",
  "autre_preciser",
  "region_resid",
  "module_creation",
  "sejour_entreprise",
  "module_recherche",
  "accompagnement",
  "moda_autre",
  "moda_formation",
  "moda_incubateur",
  "moda_cv",
  "moda_lettre",
  "moda_entretien",
  "moda_formalisation",
  "moda_juridique",
  "moda_financement",
  "moda_bizplan",
  "moda_finproj",
  "moda_formaliser",
  "moda_autre2",
  "moda_autre_precise",
  "statut_6mois",
  "adequation_6mois",
  "statut_12mois",
  "adequation_12mois",
  "statut_actuel",
  "canal_emploi",
  "canal_autre",
  "adequation_actuel",
  "secteur_entreprise",
  "secteur_activite",
  "autre_secteur",
  "protection_sociale",
  "registre_commerce",
  "ninea",
  "comptabilite",
  "autre_compta",
  "secteur_29",
  "secteur_29_prec"
)

# Application des noms courts au dataframe

colnames(insertion) <- noms_courts

# Création d'un dictionnaire de données

dictionnaire <- data.frame(
  nom_court = noms_courts,
  label = noms_originaux,
  stringsAsFactors = FALSE
)


# Nature des variables

glimpse(insertion)

# Nous allons supprimer les individus qu'on a pas pu joindre et conserver
# uniquement ceux qui ont répondu aux questionnaires

# Modalités de la variable  joignabilité
table(insertion$joignabilite)

# On filtre les lignes où joignabilite est exactement == "Ok pour débuter l’entretien"

insertion <- insertion[insertion$joignabilite == "Ok pour débuter l’entretien", ]

# La base de données contient ainsi 6686 personnes jointes sur 9490 (67% de réalisation)

dim(insertion)

# Nombre de doublons

sum(duplicated(insertion))

# Numéro des lignes dupliquées

which(duplicated(insertion))

# Identification de toutes les lignes dupliquées (premières incluses)

dups_all <- duplicated(insertion) | duplicated(insertion, fromLast = TRUE)

# Affichage

view(insertion[dups_all, ])

# Suppression des doublons

# On quitte de 6686 lignes à 6686 - 3 = 6683 lignes

insertion <- insertion[!duplicated(insertion), ]


########### Fusion des bases de données

# Clé de fusion = id_diplome
# On harmonise la clé de fusion

sondage <- sondage |> rename(id_diplome = ID_diplome)

# Fusion

fusion <- left_join(insertion, sondage, by = "id_diplome")


# Fusion avec la base contenant les poids de redressement

Poids <- read_excel("Poids.xlsx")


fusion <- left_join(fusion, Poids, by = "ID_Strate_2")


# Exportation de la base de données au format Excel

library(writexl)
write_xlsx(fusion, "fusion.xlsx")

# Le dictionnaire

write_xlsx(dictionnaire, "dictionnaire.xlsx")

###### Analyses descriptives ##############################

# Nombre d'observations et de variables
dim(fusion)

# Proportion de candidats admis joints

table(fusion$Approche)

100*table(fusion$Approche)/nrow(fusion)

# Croisement

library(questionr)

annee_approche <- table(fusion$annee , fusion$Approche)

# Proportions
cprop(annee_approche)

# Proportions avant fusion
annee_source <- table(sondage$Année, sondage$Approche)
lprop(annee_source)


cprop(table(fusion$statut_actuel , fusion$Approche))

