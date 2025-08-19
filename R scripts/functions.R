library(lubridate)
library(googlesheets4)
library(data.table)

substr_right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

slope_deg_to_percent <- function(degrees) {
  return(tan(degrees * pi / 180) * 100)
}

convert_letters_to_numbers <- function(input_string) {
  letter_map <- setNames(1:26, LETTERS) 
  letters_vec <- strsplit(toupper(gsub(" ", "", input_string)), "")[[1]]
  num_vec <- as.character(letter_map[letters_vec])
  paste0(num_vec, collapse = "")
}

# Splitting the data frame into different CSVs every 10000 elements so that the binding process can run faster
rbind_with_temporary_CSVs <- function(current_df, current_element, current_iteration, total_iterations, current_sub_iteration = 0, total_sub_iterations = 0, max_elements = 10000, folder_path) {
  current_df <- rbindlist(list(current_df, current_element))
  
  if (current_iteration == total_iterations & current_sub_iteration == total_sub_iterations) {
    temp_files <- list.files(folder_path, pattern = "\\.csv$", recursive = FALSE, include.dirs = FALSE)
    temp_current_df <- data.frame()
    
    for (a in 1:length(temp_files)) {
      temp_current_df <- rbindlist(list(temp_current_df, read.csv(paste0(folder_path, temp_files[a]))))
      file.remove(paste0(folder_path, temp_files[a]))
    }
    current_df <- rbindlist(list(temp_current_df, current_df))
    
  } else if (nrow(current_df) >= max_elements) {
    n_temp_files <- length(list.files(folder_path, pattern = "\\.csv$", recursive = FALSE, include.dirs = FALSE))
    
    temp_df_file_name <- file.path(paste0(folder_path, "temp_df_", n_temp_files, ".csv"))
    write.csv(x=current_df, file=temp_df_file_name, row.names = FALSE)
    
    current_df <- data.frame()
  }
  
  return(current_df)
}

read_PsiBiom_recordings_on_GD <- function(sheet_tabs_to_read = "all") {
  googlesheets4::gs4_deauth()
  sheet_link <- 'https://docs.google.com/spreadsheets/d/1v6yEwCx4lsCoA2Cgnxrz_548vHZcOHfBl-Sc5TXw8fY'
  if (sheet_tabs_to_read == "all") {
    sheet_tabs <- c('espece', 'soundscape', 'enregistrements_annotes_par_BirdNET', 'enregistrements_cedes_par_les_auteurs', 'soundscapes_cedes_par_les_auteurs', 'enregistrements_des_librairies_en_ligne')
  } else {
    sheet_tabs <- c(sheet_tabs_to_read)
  }
  
  # Getting metadata from Google Drive
  recordings_on_gd <- vector(mode = 'list', length = length(sheet_tabs))
  
  for (i in 1:length(sheet_tabs)) {
    if (i == 4) {
      recordings_on_gd[[i]] <- read_sheet(sheet_link, 
                                          sheet = sheet_tabs[i],
                                          col_types = c("?????????????????nnc?????????????????ccc")) # Otherwise it doesn't read these 3 columns properly for unknown reasons
    } else {
      recordings_on_gd[[i]] <- read_sheet(sheet_link, 
                                          sheet = sheet_tabs[i])
    }
    
    recordings_on_gd[[i]] <- recordings_on_gd[[i]] %>%
      rename("code_unique" = "Code unique") %>%
      mutate(id = row_number(),
             code_unique = as.character(code_unique))
  }
  
  if (sheet_tabs_to_read == "all") {
    return(recordings_on_gd)
  } else {
    return(recordings_on_gd[[1]])
  }
}

get_homogenized_PsiBiom_GD_metadata <- function(recordings_on_gd) {
  
  especes_cible_opportunistes_gd <- recordings_on_gd[[1]]
  soundscapes_opportunistes_gd <- recordings_on_gd[[2]]
  BirdNET_recordings_gd <- recordings_on_gd[[3]]
  especes_cible_auteurs_gd <- recordings_on_gd[[4]]
  soundscapes_auteurs_gd <- recordings_on_gd[[5]]
  enregistrements_telecharges_gd <- recordings_on_gd[[6]]
  
  # Homogenizing the data sets obtained through the automatic generation of metadata and from Google Drive
  
  BirdNET_recordings_gd <- BirdNET_recordings_gd %>%
    rename("ordre" = "Ordre cible",
           "espece" = "Espèce majoritaire",
           "operateur" = "Opérateur",
           "avis_operateur" = "Avis opérateur",
           "validateur" = "Validateur",
           "espece_validee" = "Espèce validée",
           "date" = "Date début",
           "heure_debut" = "Heure début",
           "periode" = "Période",
           "departement" = "Departement",
           "commune" = "Commune",
           "hammeau" = "Hameau",
           "nb_chanteurs" = "Nb chanteurs_espèce cible",
           "temperature" = "Temperature",
           "meteo" = "Météo",
           "vent" = "Vent",
           "autres_especes_nb" = "Autres espèces_nb",
           "autres_especes_annotees" = "Autres espèces_nom",
           "bruits_anthropiques" = "Bruits anthropiques",
           "bruits_geophoniques" = "Bruits géophoniques",
           "bruit_de_fond" = "Bruit de fond",
           "duree_min" = "Durée (min)",
           "duree_sec" = "Durée (sec)",
           "enregistreur" = "Enregistreur",
           "micro" = "Micro",
           "bonnette" = "Bonnette",
           "sampling_rate" = "Sampling rate",
           "notes" = "Notes",
           "stereo" = "Stéréo",
           "expansion_de_temps" = "Expansion de temps (X10)",
           "annotation_exhaustive" = "annotation exhaustive") %>%
    rowwise() %>%
    mutate(auteur = strsplit(localisation, "\\\\")[[1]][6],
           permis_utilisation = "Usage commercial",
           temperature_substrat = "",
           categorie = "BirdNET",
           sampling_rate = gsub(".1kHz", "100", gsub(" kHz", "000", gsub(" KHz", "000", gsub(".1 KHz", "100", sampling_rate)))),
           date = format(date, "%d/%m/%Y"),
           autres_especes = "",
           across(everything(), as.character)) %>%
    select(code_unique, categorie, ordre, espece, auteur, operateur, avis_operateur, validateur, espece_validee, permis_utilisation, date, heure_debut, periode, departement, commune, hammeau, GPS, nb_chanteurs, temperature, temperature_substrat, meteo, vent, autres_especes_nb, autres_especes_annotees, bruits_anthropiques, bruits_geophoniques, bruit_de_fond, duree_min, duree_sec, enregistreur, micro, bonnette, sampling_rate, BPS, expansion_de_temps, localisation, annotation, annotation_exhaustive, photo, stereo, notes, autres_especes)
  
  soundscapes_opportunistes_gd <- soundscapes_opportunistes_gd %>%
    rename("ordre" = "Ordre cible",
           "espece" = "Espèce majoritaire",
           "operateur" = "Opérateur",
           "avis_operateur" = "Avis opérateur",
           "validateur" = "Validateur",
           "espece_validee" = "Espèce validée",
           "date" = "Date début",
           "heure_debut" = "Heure début",
           "periode" = "Période",
           "departement" = "Departement",
           "commune" = "Commune",
           "hammeau" = "Hameau",
           "nb_chanteurs" = "Nb chanteurs_espèce cible",
           "temperature" = "Temperature",
           "meteo" = "Météo",
           "vent" = "Vent",
           "autres_especes_nb" = "Autres espèces_nb",
           "autres_especes_annotees" = "Autres espèces_nom",
           "bruits_anthropiques" = "Bruits anthropiques",
           "bruits_geophoniques" = "Bruits géophoniques",
           "bruit_de_fond" = "Bruit de fond",
           "duree_min" = "Durée (min)",
           "duree_sec" = "Durée (sec)",
           "enregistreur" = "Enregistreur",
           "micro" = "Micro",
           "bonnette" = "Bonnette",
           "sampling_rate" = "Sampling rate",
           "notes" = "Notes",
           "stereo" = "Stéréo",
           "expansion_de_temps" = "Expansion de temps (X10)",
           "annotation_exhaustive" = "annotation exhaustive") %>%
    mutate(auteur = strsplit(localisation, "\\\\")[[1]][7],
           permis_utilisation = "Usage commercial",
           temperature_substrat = "",
           categorie = "Soundscape",
           sampling_rate = gsub(".1kHz", "100", gsub(" kHz", "000", gsub(" KHz", "000", gsub(".1 KHz", "100", sampling_rate)))),
           date = format(date, "%d/%m/%Y"),
           autres_especes = "",
           across(everything(), as.character)) %>%
    select(code_unique, categorie, ordre, espece, auteur, operateur, avis_operateur, validateur, espece_validee, permis_utilisation, date, heure_debut, periode, departement, commune, hammeau, GPS, nb_chanteurs, temperature, temperature_substrat, meteo, vent, autres_especes_nb, autres_especes_annotees, bruits_anthropiques, bruits_geophoniques, bruit_de_fond, duree_min, duree_sec, enregistreur, micro, bonnette, sampling_rate, BPS, expansion_de_temps, localisation, annotation, annotation_exhaustive, photo, stereo, notes, autres_especes)
  
  especes_cible_opportunistes_gd <- especes_cible_opportunistes_gd %>%
    rename("ordre" = "Ordre",
           "espece" = "Espèce cible",
           "operateur" = "Opérateur",
           "avis_operateur" = "Avis opérateur",
           "validateur" = "Validateur",
           "espece_validee" = "Espèce validée",
           "date" = "Date",
           "heure_debut" = "Heure début",
           "periode" = "Période",
           "departement" = "Departement",
           "hammeau" = "Hammeau",
           "commune" = "Commune",
           "nb_chanteurs" = "Nb chanteurs",
           "temperature" = "Temperature",
           "temperature_substrat" = "Temperature substrat",
           "meteo" = "Météo",
           "vent" = "Vent",
           "autres_especes_nb" = "Autres espèces_nb",
           "autres_especes_annotees" = "Autres espèces_annotées",
           "bruits_anthropiques" = "Bruits anthropiques",
           "bruits_geophoniques" = "Bruits géophoniques",
           "bruit_de_fond" = "Bruit de fond",
           "duree_min" = "Durée (min)",
           "duree_sec" = "Durée (sec)",
           "enregistreur" = "Enregistreur",
           "micro" = "Micro",
           "bonnette" = "Bonnette",
           "sampling_rate" = "Sampling rate",
           "stereo" = "Stereo",
           "notes" = "Notes",
           "expansion_de_temps" = "Expansion de temps (X10)",
           "annotation_exhaustive" = "annotation exhaustive",
           "autres_especes" = "Autres_especes_enregistrées") %>%
    mutate(auteur = operateur,
           permis_utilisation = "Usage commercial",
           categorie = "Enregistrement focal",
           sampling_rate = gsub(" kHz", "000", gsub(" KHz", "000", gsub(".1 KHz", "00", sampling_rate))),
           date = format(date, "%d/%m/%Y"),
           across(everything(), as.character)) %>%
    select(code_unique, categorie, ordre, espece, auteur, operateur, avis_operateur, validateur, espece_validee, permis_utilisation, date, heure_debut, periode, departement, commune, hammeau, GPS, nb_chanteurs, temperature, temperature_substrat, meteo, vent, autres_especes_nb, autres_especes_annotees, bruits_anthropiques, bruits_geophoniques, bruit_de_fond, duree_min, duree_sec, enregistreur, micro, bonnette, sampling_rate, BPS, expansion_de_temps, localisation, annotation, annotation_exhaustive, photo, stereo, notes, autres_especes)
  
  especes_cible_auteurs_gd <- especes_cible_auteurs_gd %>%
    rename("ordre" = "Ordre",
           "espece" = "Espèce cible",
           "auteur" = "Auteur",
           "operateur" = "Opérateur",
           "avis_operateur" = "Avis opérateur",
           "validateur" = "Validateur",
           "espece_validee" = "Espèce validée",
           "permis_utilisation" = "Permis d'utilisation",
           "date" = "Date",
           "heure_debut" = "Heure début",
           "periode" = "Période",
           "departement" = "Departement",
           "hammeau" = "Hammeau",
           "commune" = "Commune",
           "nb_chanteurs" = "Nb chanteurs",
           "temperature" = "Temperature",
           "temperature_substrat" = "Temperature substrat",
           "autres_especes_nb" = "Autres espèces_nb",
           "autres_especes_annotees" = "Autres espèces_annotées",
           "bruits_anthropiques" = "Bruits anthropiques",
           "bruits_geophoniques" = "Bruits géophoniques",
           "bruit_de_fond" = "Bruit de fond",
           "duree_min" = "Durée (min)",
           "duree_sec" = "Durée (sec)",
           "enregistreur" = "Enregistreur",
           "sampling_rate" = "Sampling rate",
           "stereo" = "Stereo",
           "notes" = "Notes",
           "expansion_de_temps" = "Expansion de temps (X10)",
           "annotation_exhaustive" = "annotation exhaustive",
           "autres_especes" = "Autres espèces") %>%
    rowwise() %>%
    mutate(meteo = "",
           vent = "",
           bonnette = "",
           micro = case_when(grepl(" \\+ ", enregistreur) || grepl(", ", enregistreur) ~ strsplit(gsub(" \\+ ", ", ", enregistreur), ", ")[[1]][2],
                             TRUE ~ ""),
           enregistreur = strsplit(gsub(" \\+ ", ", ", enregistreur), ", ")[[1]][1],
           categorie = "Enregistrement focal cédé par un auteur",
           date = format(date, "%d/%m/%Y"),
           across(everything(), as.character)) %>%
    ungroup() %>%
    select(code_unique, categorie, ordre, espece, auteur, operateur, avis_operateur, validateur, espece_validee, permis_utilisation, date, heure_debut, periode, departement, commune, hammeau, GPS, nb_chanteurs, temperature, temperature_substrat, meteo, vent, autres_especes_nb, autres_especes_annotees, bruits_anthropiques, bruits_geophoniques, bruit_de_fond, duree_min, duree_sec, enregistreur, micro, bonnette, sampling_rate, BPS, expansion_de_temps, localisation, annotation, annotation_exhaustive, photo, stereo, notes, autres_especes)
  
  soundscapes_auteurs_gd <- soundscapes_auteurs_gd %>%
    rename("ordre" = "Ordre cible",
           "espece" = "Espèce majoritaire",
           "auteur" = "Auteur",
           "operateur" = "Operateur",
           "avis_operateur" = "Avis operateur",
           "validateur" = "Validateur",
           "espece_validee" = "Espèce validée",
           "permis_utilisation" = "Permis d'utilisation",
           "date" = "Date",
           "heure_debut" = "Heure début",
           "periode" = "Période",
           "departement" = "Département",
           "hammeau" = "Hammeau",
           "commune" = "Commune",
           "nb_chanteurs" = "Nb_chanteurs",
           "temperature" = "Temperature",
           "autres_especes_nb" = "Autres_especes_nb",
           "autres_especes_annotees" = "Autres_especes_annotées",
           "bruits_anthropiques" = "Bruits anthropiques",
           "bruits_geophoniques" = "Bruits géophoniques",
           "bruit_de_fond" = "Bruit de fond",
           "duree_min" = "Durée (min)",
           "duree_sec" = "Durée (sec)",
           "enregistreur" = "Enregistreur",
           "micro" = "Micro",
           "sampling_rate" = "Sampling rate",
           "stereo" = "Stéréo",
           "localisation" = "Localisation",
           "annotation" = "Annotation",
           "annotation_exhaustive" = "Annotation exhaustive",
           "photo" = "Photo",
           "notes" = "Notes",
           "expansion_de_temps" = "Expansion de temps (X10)") %>%
    mutate(meteo = "",
           vent = "",
           bonnette = "",
           temperature_substrat = "",
           categorie = "Soundscape cédé par un auteur",
           autres_especes = "",
           date = format(date, "%d/%m/%Y"),
           across(everything(), as.character)) %>%
    select(code_unique, categorie, ordre, espece, auteur, operateur, avis_operateur, validateur, espece_validee, permis_utilisation, date, heure_debut, periode, departement, commune, hammeau, GPS, nb_chanteurs, temperature, temperature_substrat, meteo, vent, autres_especes_nb, autres_especes_annotees, bruits_anthropiques, bruits_geophoniques, bruit_de_fond, duree_min, duree_sec, enregistreur, micro, bonnette, sampling_rate, BPS, expansion_de_temps, localisation, annotation, annotation_exhaustive, photo, stereo, notes, autres_especes)
  
  enregistrements_telecharges_gd <- enregistrements_telecharges_gd %>%
    rename("ordre" = "Ordre",
           "espece" = "Espèce cible",
           "auteur" = "Auteur",
           "operateur" = "Opérateur",
           "avis_operateur" = "Avis opérateur",
           "validateur" = "Validateur",
           "espece_validee" = "Espèce validée",
           "permis_utilisation" = "Permis d'utilisation",
           "date" = "Date",
           "heure_debut" = "Heure début",
           "periode" = "Période",
           "departement" = "Departement",
           "hammeau" = "Hammeau",
           "commune" = "Commune",
           "nb_chanteurs" = "Nb chanteurs",
           "temperature" = "Temperature",
           "temperature_substrat" = "Temperature substrat",
           "autres_especes_nb" = "Autres espèces_nb",
           "autres_especes_annotees" = "Autres espèces_annotées",
           "bruits_anthropiques" = "Bruits anthropiques",
           "bruits_geophoniques" = "Bruits géophoniques",
           "bruit_de_fond" = "Bruit de fond",
           "duree_min" = "Durée (min)",
           "duree_sec" = "Durée (sec)",
           "sampling_rate" = "Sampling rate",
           "stereo" = "Stereo",
           "notes" = "Notes",
           "expansion_de_temps" = "Expansion de temps (X10)",
           "annotation_exhaustive" = "annotation exhaustive",
           "enregistreur" = "Enregistreur",
           "autres_especes" = "Autres espèces") %>%
    mutate(meteo = "",
           vent = "",
           micro = "",
           bonnette = "",
           temperature_substrat = "",
           categorie = "Enregistrement téléchargé",
           date = format(date, "%d/%m/%Y"),
           across(everything(), as.character)) %>%
    select(code_unique, categorie, ordre, espece, auteur, operateur, avis_operateur, validateur, espece_validee, permis_utilisation, date, heure_debut, periode, departement, commune, hammeau, GPS, nb_chanteurs, temperature, temperature_substrat, meteo, vent, autres_especes_nb, autres_especes_annotees, bruits_anthropiques, bruits_geophoniques, bruit_de_fond, duree_min, duree_sec, enregistreur, micro, bonnette, sampling_rate, BPS, expansion_de_temps, localisation, annotation, annotation_exhaustive, photo, stereo, notes, autres_especes)
  
  
  gd_metadata <- rbind(BirdNET_recordings_gd,
                       soundscapes_opportunistes_gd,
                       soundscapes_auteurs_gd, 
                       especes_cible_opportunistes_gd,
                       especes_cible_auteurs_gd,
                       enregistrements_telecharges_gd)
  
  gd_metadata[is.na(gd_metadata)] <- ""
  gd_metadata[gd_metadata == "NULL"] <- ""
  
  return(gd_metadata)
}

read_all_merged_metadata <- function(local_path) {
  BirdNET_recordings_metadata <- read.csv(paste0(local_path, "CSVs/Metadata/Merged with Google Drive metadata/enregistrements_annotes_par_BirdNET_metadata.csv"))
  soundscapes_metadata <- read.csv(paste0(local_path, "CSVs/Metadata/Merged with Google Drive metadata/soundscapes_opportunistes_metadata.csv"))
  soundscapes_by_authors_metadata <- read.csv(paste0(local_path, "CSVs/Metadata/Merged with Google Drive metadata/soundscapes_auteurs_metadata.csv"))
  focal_recordings_metadata <- read.csv(paste0(local_path, "CSVs/Metadata/Merged with Google Drive metadata/especes_cible_opportunistes_metadata.csv"))
  focal_recordings_by_authors_metadata <- read.csv(paste0(local_path, "CSVs/Metadata/Merged with Google Drive metadata/especes_cible_auteus_metadata.csv"))
  downloaded_recordings_metadata <- read.csv(paste0(local_path, "CSVs/Metadata/Merged with Google Drive metadata/enregistrements_telecharges_metadata.csv"))
  
  metadata <- rbind(BirdNET_recordings_metadata,
                    soundscapes_metadata,
                    soundscapes_by_authors_metadata,
                    focal_recordings_metadata,
                    focal_recordings_by_authors_metadata,
                    downloaded_recordings_metadata)
  
  return(metadata)
}

find_singing_ort_hem_anu_species <- function(local_path) {
  insect_subspecies_by_default <- read.csv(paste0(local_path, "CSVs/Species lists/Orthopteran_and_hemipteran_subspecies_by_default.csv"))
  
  ort_hem_species_in_Northwest_and_Central_Europe <- read.csv(paste0(local_path, "CSVs/Species lists/Orthopteran_and_cicada_species_list_+_stridulation_capacity_in_Northwest_and_Central_Europe.csv"))
  
  ort_hem_anu_sp <- rbind(ort_hem_species_in_Northwest_and_Central_Europe %>%
                            rename("Scientific.name" = "Espèce",
                                   "Order" = "Ordre") %>%
                            select(Scientific.name, Order),
                          read.csv(paste0(local_path, "CSVs/Species lists/Batrachos_anuran_species_in_temperate_Western_Europe.csv")) %>%
                            mutate(Order = "Anura") %>%
                            select(Scientific.name, Order)) %>%
    rowwise() %>%
    mutate(Scientific.name = case_when(substr_right(Scientific.name, 1) == " " ~ substr(Scientific.name, 1, nchar(Scientific.name) - 1),
                                       TRUE ~ Scientific.name)) %>% 
    unique() %>%
    left_join(insect_subspecies_by_default,
              by = c("Scientific.name" = "Species")) %>%
    mutate(Scientific.name = case_when(is.na(Subspecies) ~ Scientific.name,
                                       TRUE ~ Subspecies)) %>%
    rename("ordre" = "Order") %>%
    select(-Subspecies)
  
  non_singing_orthopterans_in_Northwest_and_Central_Europe <- ort_hem_species_in_Northwest_and_Central_Europe %>%
    filter(Ordre == "Orthoptera",
           Son %in% c("No", "Unknown")) %>%
    rename("species" = "Espèce") %>%
    rowwise() %>%
    select(species)
  
  non_singing_orthopterans_list_by_David_Sannier <- read.csv(paste0(local_path, "CSVs/Species lists/non_singing_orthopterans_list_by_David_Sannier.csv"))
  
  non_singing_orthopterans <- unique(rbind(non_singing_orthopterans_in_Northwest_and_Central_Europe, non_singing_orthopterans_list_by_David_Sannier))
  
  singing_ort_hem_anu_sp <- ort_hem_anu_sp %>%
    filter(!(Scientific.name %in% c(non_singing_orthopterans$species,
                                    "Chorthippus brunneus x jacobsi", # Excluding "non-official" species
                                    "Sphingonotus non caerulans"))) %>%
    rename(species = Scientific.name)
  
  return(singing_ort_hem_anu_sp)
}

find_all_species_of_interest <- function(local_path) {
  CSV_path <- paste0(local_path, "CSVs/")
  
  singing_ort_hem_anu_sp <- find_singing_ort_hem_anu_species(local_path) %>%
    rename("tax_group" = "ordre")
  
  hymenopteran_species <- read.csv(paste0(CSV_path, "Species lists/GBIF_non_ant_hymenopteran_species_in_temperate_Western_Europe.csv")) %>%
    mutate(tax_group = "Hymenoptera")
  
  dipteran_species <- read.csv(paste0(CSV_path, "Species lists/GBIF_dipteran_species_in_temperate_Western_Europe.csv")) %>%
    mutate(tax_group = "Diptera")
  
  coleopteran_species <- read.csv(paste0(CSV_path, "Species lists/GBIF_coleopteran_species_in_temperate_Western_Europe.csv")) %>%
    mutate(tax_group = "Coleoptera")
  
  lepidopteran_species <- read.csv(paste0(CSV_path, "Species lists/GBIF_lepidopteran_species_in_temperate_Western_Europe.csv")) %>%
    mutate(tax_group = "Lepidoptera")
  
  # dip_col_lep_species <- read.csv(paste0(CSV_path, "Species lists/All_sound_categories.csv")) %>%
  #   rename("tax_group" = "ORDRE",
  #          "Scientific.name" = "LB_NOM") %>%
  #   filter(tax_group %in% c("Diptera", "Coleoptera", "Lepidoptera"),
  #          RANG %in% c("ES", "SSES")) %>%
  #   select(Scientific.name, tax_group)
  
  insect_species <- rbind(singing_ort_hem_anu_sp %>%
                            filter(tax_group %in% c("Hemiptera", "Orthoptera")),
                          hymenopteran_species,
                          dipteran_species,
                          coleopteran_species,
                          lepidopteran_species) %>%
    rename("Scientific.name" = "species")
  
  anuran_species <- singing_ort_hem_anu_sp %>%
    filter(tax_group == "Anura") %>%
    rename("Scientific.name" = "species")
  
  bat_species <- read.csv(paste0(CSV_path, "Species lists/Plan_National_dActions_Chiroptères_bat_species_in_France.csv")) %>%
    select(Scientific.name) %>%
    mutate(tax_group = "Chiroptera")
  
  mammal_species <- read.csv(paste0(CSV_path, "Species lists/Faune_France_Mammalia_species_other_than_bats_in_France.csv")) %>%
    select(Scientific.name) %>%
    mutate(tax_group = "Mammalia")
  
  # squamata_species <- read.csv(paste0(CSV_path, "Species lists/All_sound_categories.csv")) %>%
  #   rename("tax_group" = "ORDRE",
  #          "Scientific.name" = "LB_NOM") %>%
  #   filter(tax_group == "Squamata",
  #          RANG %in% c("ES", "SSES")) %>%
  #   select(Scientific.name, tax_group)
  
  squamata_species <- read.csv(paste0(CSV_path, "Species lists/GBIF_squamata_species_in_temperate_Western_Europe.csv")) %>%
    mutate(tax_group = "Squamata") %>%
    rename("Scientific.name" = "species")
  
  bird_species <- read.csv(paste0(local_path, "CSVs/Species lists/Avibase_non_accidental_bird_species_in_France.csv")) %>%
    mutate(tax_group = "Aves") %>%
    select(Scientific.name, tax_group) %>%
    unique()
  
  all_species <- rbindlist(list(insect_species,
                                anuran_species,
                                bat_species,
                                mammal_species,
                                squamata_species,
                                bird_species)) %>%
    rowwise() %>%
    rename("species" = "Scientific.name") %>%
    select(species, tax_group) %>%
    unique()
  
  return(all_species)
}

numbers_only <- function(x) {
  return(!grepl("\\D", x))
}

transform_date <- function(date_in_code_unique_format) {
  if (date_in_code_unique_format %in% c("", "XXXXXXXX")) {
    return("")
  } else {
    string_length <- nchar(date_in_code_unique_format)
    day <- substr(date_in_code_unique_format, 1, 2) 
    month <- as.character(as.numeric(as.roman(substr(date_in_code_unique_format, 3, string_length - 4))))
    
    if (nchar(month) == 1) {
      month <- paste0("0", month)
    }
    
    year <- substr(date_in_code_unique_format, string_length - 3, string_length)
    
    return(paste(day, month, year, sep = "/"))
  }
}

reverse_transform_date <- function(date_in_regular_format) {
  day <- substr(date_in_regular_format, 1, 2) 
  month <- as.character(as.roman(substr(date_in_regular_format, 4, 5)))
  year <- substr(date_in_regular_format, 7, 10)
  
  return(paste0(day, month, year))
}

dms_to_dd <- function(degrees, minutes, seconds, direction) {
  dd <- degrees + (minutes / 60) + (seconds / 3600)
  if (direction %in% c("S", "W")) {
    dd <- -dd  # South and West are negative in WGS84
  }
  return(dd)
}

getTimeZone <- function(d, country) {
  if (country %in% c("AL",
                     "AD",
                     "AT",
                     "BE",
                     "BA",
                     "HR",
                     "CZ",
                     "DK",
                     "FR",
                     "DE",
                     "HU",
                     "IT",
                     "LI",
                     "LU",
                     "MT",
                     "MC",
                     "ME",
                     "NL",
                     "MK",
                     "PL",
                     "SM",
                     "RS",
                     "SK",
                     "SI",
                     "ES",
                     "CH")) {
    month <- month(d)
    day <- day(d)
    
    if ((month >= 4 || (month == 3 && day > 26)) &&
        (month <= 9 || (month == 10 && day < 30))) {
      
      return("CEST")
      
    } else {
      return("CET")
    }
  } else {
    return("")
  }
}

normalize <- function(y) {
  
  x<-y[!is.na(y)]
  
  if (length(x) == 0) {
    return(y)
  } else {
    x<-(x - min(x)) / (max(x) - min(x))
    
    y[!is.na(y)]<-x
    
    return(y)
  }
}

normalize_for_beta_gamma_dist <- function(y) {
  
  x<-y[!is.na(y)]
  
  value_span <- max(x) - min(x)
  small_addition <- value_span * 1e-6
  
  x<-(x - min(x) + small_addition) / (value_span + 2*small_addition)
  
  y[!is.na(y)]<-x
  
  return(y)
}

is_date <- function(date_string) {
  return(substr(date_string, 1, 2) %in% c("20", "19") && nchar(date_string) == 8)
}

is_short_date <- function(date_string) {
  return(numbers_only(date_string) && nchar(date_string) == 6)
}

is_temperature <- function(temperature_string) {
  return(grepl("º", temperature_string, fixed = TRUE))
}

is_time <- function(time_string) {
  time_format <- (substr(time_string, 3, 3) == "-" && nchar(time_string) %in% c(5, 8)) ||
    (substr(time_string, 3, 3) == "h" && nchar(time_string) %in% c(5, 6, 8, 9)) ||
    (numbers_only(time_string) && nchar(time_string) == 6)
  
  if (time_format) {
    plausible_hours <- as.numeric(substr(time_string, 1, 2)) <= 24
    plausible_minutes <- (nchar(time_string) %in% c(5, 8) && as.numeric(substr(time_string, 4, 5)) <= 60 ||
                            as.numeric(substr(time_string, 3, 4)) <= 60)
    plausible_seconds <- (nchar(time_string) == 5 ||
                            nchar(time_string) == 8 && as.numeric(substr(time_string, 7, 8)) <= 60 ||
                            as.numeric(substr(time_string, 5, 6)) <= 60)
    
    return(time_format && plausible_hours && plausible_minutes && plausible_seconds)
  } else {
    return(FALSE)
  }
}

transform_date_into_regular_format <- function(date_in_yyyymmdd_format) {
  day <- substr(date_in_yyyymmdd_format, 7, 8) 
  month <- substr(date_in_yyyymmdd_format, 5, 6) 
  year <- substr(date_in_yyyymmdd_format, 1, 4)
  
  return(paste(day, month, year, sep = "/"))
}

transform_temperature_into_regular_format <- function(temperature) {
  return(strsplit(temperature, "º")[[1]][1])
}

transform_time_into_regular_format <- function(time_string) {
  if (grepl("-", time_string)) {
    time_split_by_hyphen <- strsplit(time_string, "-")[[1]]
    time <- paste(time_split_by_hyphen[1], time_split_by_hyphen[2], sep = "h")
  } else if (grepl("h", time_string)) {
    time_split_by_h <- strsplit(time_string, "h")[[1]]
    hours <- time_split_by_h[1]
    if (grepl("m", time_split_by_h[2])) {
      minutes <- strsplit(time_split_by_h[2], "m")[[1]][1]
    } else {
      minutes <- strsplit(time_split_by_h[2], "'")[[1]][1]
    }
    time <- paste(hours, minutes, sep = "h")
  } else {
    time <- paste(substr(time_string, 1, 2), substr(time_string, 3, 4), sep = "h")
  }
  
  return(time)
}

hour2dec <- function(hour) {
  hour <- as.character(hour)
  x <- do.call(rbind, strsplit(hour, split=':'))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

save_descdist_plot <- function (data, discrete = FALSE, boot = NULL, method = "unbiased", 
          graph = TRUE, print = TRUE, obs.col = "darkblue", obs.pch = 16, 
          boot.col = "orange", file_path = NULL, width = 1000, height = 1000) 
{
  if (!is.null(file_path)) {
    jpeg(file_path, width, height, res = 300, units = "px")
  }
  if (missing(data) || !is.vector(data, mode = "numeric")) 
    stop("data must be a numeric vector")
  if (length(data) < 4) 
    stop("data must be a numeric vector containing at least four values")
  moment <- function(data, k) {
    m1 <- mean(data)
    return(sum((data - m1)^k)/length(data))
  }
  if (method == "unbiased") {
    skewness <- function(data) {
      sd <- sqrt(moment(data, 2))
      n <- length(data)
      gamma1 <- moment(data, 3)/sd^3
      unbiased.skewness <- sqrt(n * (n - 1)) * gamma1/(n - 
                                                         2)
      return(unbiased.skewness)
    }
    kurtosis <- function(data) {
      n <- length(data)
      var <- moment(data, 2)
      gamma2 <- moment(data, 4)/var^2
      unbiased.kurtosis <- (n - 1)/((n - 2) * (n - 3)) * 
        ((n + 1) * gamma2 - 3 * (n - 1)) + 3
      return(unbiased.kurtosis)
    }
    standdev <- function(data) {
      sd(data)
    }
  }
  else if (method == "sample") {
    skewness <- function(data) {
      sd <- sqrt(moment(data, 2))
      return(moment(data, 3)/sd^3)
    }
    kurtosis <- function(data) {
      var <- moment(data, 2)
      return(moment(data, 4)/var^2)
    }
    standdev <- function(data) {
      sqrt(moment(data, 2))
    }
  }
  else stop("The only possible value for the argument method are 'unbiased' or 'sample'")
  res <- list(min = min(data), max = max(data), median = median(data), 
              mean = mean(data), sd = standdev(data), skewness = skewness(data), 
              kurtosis = kurtosis(data), method = method)
  skewdata <- res$skewness
  kurtdata <- res$kurtosis
  if (graph) {
    if (!is.null(boot)) {
      if (!is.numeric(boot) || boot < 10) {
        stop("boot must be NULL or a integer above 10")
      }
      n <- length(data)
      databoot <- matrix(sample(data, size = n * boot, 
                                replace = TRUE), nrow = n, ncol = boot)
      s2boot <- sapply(1:boot, function(iter) skewness(databoot[, 
                                                                iter])^2)
      kurtboot <- sapply(1:boot, function(iter) kurtosis(databoot[, 
                                                                  iter]))
      kurtmax <- max(10, ceiling(max(kurtboot)))
      xmax <- max(4, ceiling(max(s2boot)))
    }
    else {
      kurtmax <- max(10, ceiling(kurtdata))
      xmax <- max(4, ceiling(skewdata^2))
    }
    ymax <- kurtmax - 1
    plot(skewdata^2, kurtmax - kurtdata, pch = obs.pch, 
         xlim = c(0, xmax), ylim = c(0, ymax), yaxt = "n", 
         xlab = "square of skewness", ylab = "kurtosis", 
         main = "Cullen and Frey graph")
    yax <- as.character(kurtmax - 0:ymax)
    axis(side = 2, at = 0:ymax, labels = yax)
    if (!discrete) {
      p <- exp(-100)
      lq <- seq(-100, 100, 0.1)
      q <- exp(lq)
      s2a <- (4 * (q - p)^2 * (p + q + 1))/((p + q + 2)^2 * 
                                              p * q)
      ya <- kurtmax - (3 * (p + q + 1) * (p * q * (p + 
                                                     q - 6) + 2 * (p + q)^2)/(p * q * (p + q + 2) * 
                                                                                (p + q + 3)))
      p <- exp(100)
      lq <- seq(-100, 100, 0.1)
      q <- exp(lq)
      s2b <- (4 * (q - p)^2 * (p + q + 1))/((p + q + 2)^2 * 
                                              p * q)
      yb <- kurtmax - (3 * (p + q + 1) * (p * q * (p + 
                                                     q - 6) + 2 * (p + q)^2)/(p * q * (p + q + 2) * 
                                                                                (p + q + 3)))
      s2 <- c(s2a, s2b)
      y <- c(ya, yb)
      polygon(s2, y, col = "lightgrey", border = "lightgrey")
      lshape <- seq(-100, 100, 0.1)
      shape <- exp(lshape)
      s2 <- 4/shape
      y <- kurtmax - (3 + 6/shape)
      lines(s2, y, lty = 2)
      lshape <- seq(-100, 100, 0.1)
      shape <- exp(lshape)
      es2 <- exp(shape^2)
      s2 <- (es2 + 2)^2 * (es2 - 1)
      y <- kurtmax - (es2^4 + 2 * es2^3 + 3 * es2^2 - 
                        3)
      lines(s2, y, lty = 3)
      legend(xmax * 0.2, ymax * 1.03, pch = obs.pch, legend = "Observation", 
             bty = "n", cex = 0.8, pt.cex = 1.2, col = obs.col)
      if (!is.null(boot)) {
        legend(xmax * 0.2, ymax * 0.98, pch = 1, legend = "bootstrapped values", 
               bty = "n", cex = 0.8, col = boot.col)
      }
      legend(xmax * 0.55, ymax * 1.03, legend = "Theoretical distributions", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.98 * ymax, pch = 8, legend = "normal", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.94 * ymax, pch = 2, legend = "uniform", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.9 * ymax, pch = 7, legend = "exponential", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.86 * ymax, pch = 3, legend = "logistic", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.82 * ymax, fill = "grey80", 
             legend = "beta", bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.78 * ymax, lty = 3, legend = "lognormal", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.74 * ymax, lty = 2, legend = "gamma", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.58, 0.69 * ymax, legend = c("(Weibull is close to gamma and lognormal)"), 
             bty = "n", cex = 0.6)
    }
    else {
      p <- exp(-10)
      lr <- seq(-100, 100, 0.1)
      r <- exp(lr)
      s2a <- (2 - p)^2/(r * (1 - p))
      ya <- kurtmax - (3 + 6/r + p^2/(r * (1 - p)))
      p <- 1 - exp(-10)
      lr <- seq(100, -100, -0.1)
      r <- exp(lr)
      s2b <- (2 - p)^2/(r * (1 - p))
      yb <- kurtmax - (3 + 6/r + p^2/(r * (1 - p)))
      s2 <- c(s2a, s2b)
      y <- c(ya, yb)
      polygon(s2, y, col = "grey80", border = "grey80")
      legend(xmax * 0.2, ymax * 1.03, pch = obs.pch, legend = "Observation", 
             bty = "n", cex = 0.8, pt.cex = 1.2, col = obs.col)
      if (!is.null(boot)) {
        legend(xmax * 0.2, ymax * 0.98, pch = 1, legend = "bootstrapped values", 
               bty = "n", cex = 0.8, col = boot.col)
      }
      legend(xmax * 0.55, ymax * 1.03, legend = "Theoretical distributions", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.98 * ymax, pch = 8, legend = "normal", 
             bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.94 * ymax, fill = "grey80", 
             legend = "negative binomial", bty = "n", cex = 0.8)
      legend(xmax * 0.6, 0.9 * ymax, lty = 2, legend = "Poisson", 
             bty = "n", cex = 0.8)
      llambda <- seq(-100, 100, 0.1)
      lambda <- exp(llambda)
      s2 <- 1/lambda
      y <- kurtmax - (3 + 1/lambda)
      lines(s2, y, lty = 2)
    }
    if (!is.null(boot)) {
      points(s2boot, kurtmax - kurtboot, pch = 1, col = boot.col, 
             cex = 0.5)
    }
    points(skewness(data)^2, kurtmax - kurtosis(data), pch = obs.pch, 
           cex = 2, col = obs.col)
    points(0, kurtmax - 3, pch = 8, cex = 1.5, lwd = 2)
    if (!discrete) {
      points(0, kurtmax - 9/5, pch = 2, cex = 1.5, lwd = 2)
      points(2^2, kurtmax - 9, pch = 7, cex = 1.5, lwd = 2)
      points(0, kurtmax - 4.2, pch = 3, cex = 1.5, lwd = 2)
    }
  }
  if (!is.null(file_path)) {
    dev.off()
  }
  if (!print) 
    invisible(structure(res, class = "descdist"))
  else structure(res, class = "descdist")
}

save_test_residuals_plot <- function (simulationOutput, plot = T, file_path = NULL, width = 1500, height = 1000) 
{
  if (!is.null(file_path)) {
    jpeg(file_path, width, height, res = 300, units = "px")
  }
  opar = par(mfrow = c(1, 3))
  on.exit(par(opar))
  out = list()
  out$uniformity = testUniformity(simulationOutput, plot = plot)
  out$dispersion = testDispersion(simulationOutput, plot = plot)
  out$outliers = testOutliers(simulationOutput, plot = plot)
  print(out)
  
  if (!is.null(file_path)) {
    dev.off()
  }
  return(out)
}

normalize_without_NA <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

max_without_NA <- function(x) {
  ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
}

as_count_data <- function(x) {
  values_gt_zero <- x[x > 0]
  
  min_value <- min(values_gt_zero)
  return(round(x / min_value))
}

can_convert_to_numeric <- function(x) {
  all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', x, perl = TRUE))  
}

euc.dist <- function(x1, x2) {
  return(sqrt(sum((x1 - x2) ^ 2)))
}

find_closest_point <- function(reference_lon, reference_lat, possible_points) {
  reference_point_df <- data.frame(x = reference_lon, y = reference_lat)
  possible_points <- possible_points %>%
    rowwise() %>%
    mutate(distance = euc.dist(reference_point_df, data.frame(x = longitude, y = latitude))) %>%
    arrange(distance)
  
  return(possible_points[1,])
}