library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)

# Function returning the n last characters of string x
substr_right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Declaring initial variables determining the data to select and the file paths
taxonomic_level <- "subspecies"
selectable_orders <- c("Hemiptera", "Orthoptera")
sets_to_include <- c("train", "val", "test")
output_folder <- "../Output/"
recording_chunks_file_path <- "../Input/Annotations by recording chunk/"


#------------------------- READING DATA --------------------------
# Basic recording metadata
metadata <- read.csv("../Input/Recording metadata/recording_metadata.csv") %>%
  mutate(code_unique = gsub("\\.wav", "", recording_file_name))

# Annotations in each 4-second recording chunk
annotation_chunks <- read.csv(paste0(recording_chunks_file_path, "selected_annotation_chunks.csv"))

# License associated with each recordist having contributed to the acoustic dataset
permissions_by_authors <- read.csv("../Input/Acoustic database publication/Permissions by authors.csv")

# Basic metadata of all publishable recordings in our dataset
recordings_with_permission_to_publish <- read.csv("../Input/Acoustic database publication/recordings_with_permission_to_publish.csv")

# List of soniferous orthopteran and cicada species in North, Central and temperate Western Europe
singing_ort_hem_sp <- read.csv("../Input/Species lists/soniferous_ort_hem_species.csv")

# Different taxonomic ranks of all species present in France, as well as non-French species included in the dataset
original_all_sound_categories <- read.csv("../Input/Species lists/All_sound_categories.csv")
all_sound_categories <- original_all_sound_categories %>%
  rename("species" = "LB_NOM") %>%
  mutate(Taxonomic_group = case_when(CLASSE == "Aves" ~ "Aves",
                                     CLASSE == "Mammalia" & ORDRE != "Chiroptera" ~ "Mammalia",
                                     REGNE == "Anthropophony" ~ "Anthropophony",
                                     REGNE == "Geophony" ~ "Geophony",
                                     species == "Insecta sp." ~ "Insecta",
                                     species == "Animalia sp." ~ "Animalia",
                                     TRUE ~ ORDRE)) %>%
  rename("taxonomic_group" = "Taxonomic_group") %>%
  select(species, taxonomic_group)

# Type of sound produced and frequency range of the different orthopteran and cicada species present in North, Central and temperate Western Europe
ort_hem_species_by_stridulation_capacity <- read.csv("../Input/Species lists/Orthopteran_and_cicada_species_list_+_stridulation_capacity_in_Northwest_and_Central_Europe.csv")

# Summary of the number of annotations per label (including geophony and anthropophony)
total_animals_annotated <- read.csv("../Input/Annotations by recording chunk/total_animals_annotated_at_the_subspecies_level.csv")

# Ad hoc sound categories for anthropophony and geophony
anthropophony_geophony_labels <- read.csv("../Input/Annotation labels/abiotic_sound_labels.csv")

# More detailed metadata of all publishable recordings, including the recording category (pre-existing vs. expressly collected, focal recording vs. soundscape)
recordings_with_permission_to_publish_metadata <- read.csv("../Input/Recording metadata/all_recordings_with_permission_to_publish.csv")

# Basic metadata of all recordings available in the online libraries Xeno-canto, iNaturalist, observation.org, SNSB, MKB, MinIO and BioAcoustica
online_ort_hem_recording_metadata <- read.csv("../Input/Recording metadata/online_recordings_metadata.csv")


#---------------------------- TRANSFORMING AND ADAPTING METADATA -----------------------------
# Splitting the metadata data frame into one row for each species detected in each recording
split_metadata <- metadata %>%
  separate_rows(recorded_species, sep = ", ") %>%
  mutate(espece = recorded_species) %>%
  select(-recorded_species)

if (taxonomic_level == "species") {
  # Replacing subspecies names for species names
  singing_ort_hem_sp <- singing_ort_hem_sp %>%
    rowwise() %>%
    mutate(species = paste(head(strsplit(species, " ")[[1]], 2), collapse = " ")) %>%
    select(species, ordre) %>%
    unique()
  
  annotation_chunks <- annotation_chunks %>%
    rowwise() %>%
    mutate(label = case_when(label_category %in% selectable_orders ~ paste(head(strsplit(label, " ")[[1]], 2), collapse = " "),
                             TRUE ~ label)) %>%
    ungroup()
  
  split_metadata <- split_metadata %>%
    rowwise() %>%
    mutate(espece = paste(head(strsplit(espece, " ")[[1]], 2), collapse = " ")) %>%
    ungroup()
}

# Filtering out potential duplicates
split_metadata <- split_metadata %>%
  unique()

# Only selecting soniferous species
selectable_species <- intersect(all_sound_categories$species, singing_ort_hem_sp$species)

# Filtering out annotation chunks without any orthopteran or cicada sound
n_ort_hem_annotations_per_chunk <- annotation_chunks %>%
  group_by(recording_chunk_id) %>%
  summarize(n_ort_hem = sum(as.numeric(confidence_level[label_category %in% selectable_orders & 
                                                          substr_right(label, 4) != " sp." &
                                                          label %in% selectable_species])))

ort_hem_annotated_chunks <- annotation_chunks %>%
  left_join(n_ort_hem_annotations_per_chunk,
            by = "recording_chunk_id") %>%
  filter(n_ort_hem > 0) %>%
  select(-n_ort_hem)

# Finding all species-recording combinations (a recording can contain multiple species)
code_unique_espece_combinations <- rbind(split_metadata %>%
                                           select(code_unique, espece),
                                           ort_hem_annotated_chunks %>%
                                             select(code_unique, label) %>%
                                             rename("espece" = "label")) %>%
                                           filter(espece %in% selectable_species) %>%  # Either unannotated recordings where we know orthoperans/cicadas are present or recordings with annotations of these taxonomic groups
                                           unique()

# Only selecting recordings with permission to publish and neither being recorded in heterodyne nor having been time-expanded
ort_hem_recordings <- code_unique_espece_combinations %>%
  left_join(metadata %>%
              select(-recorded_species),
            by = "code_unique") %>%
  inner_join(permissions_by_authors %>%
              filter(Permission == "Yes") %>%
              select(Author),
            by = c("author_name" = "Author")) %>%
  inner_join(recordings_with_permission_to_publish %>%
               select(unique_code),
             by = c("code_unique" = "unique_code"))

# Filtering out recordings whose orthopteran and cicada sounds have not been identified at least at the species level
recordings_without_annotations_at_the_species_level <- setdiff(unique((ort_hem_recordings %>%
                                                                         filter(annotated != "Unannotated"))$recording_id),
                                                               unique((ort_hem_annotated_chunks %>%
  filter(!grepl(" sp\\.", label),
         label_order %in% c("Orthoptera", "Hemiptera")))$recording_id))
  
ort_hem_recording_metadata <- ort_hem_recordings %>%
  mutate(longitude = as.character(longitude),
         latitue = as.character(latitude))

# Transforming NAs into empty strings
ort_hem_recording_metadata[is.na(ort_hem_recording_metadata)] <- ""

#---------------------------- JOINING, FILTERING AND REFORMATTING ALL ANNOTATED AUDIO SEGMENTS ----------------------------

# Joining all annotations from the selected sets (train, test, val)
split_results <- data.frame()

for (i in 1:length(sets_to_include)) {
  split_results <- rbind(split_results, read.csv(paste0(recording_chunks_file_path, sets_to_include[i], "_recording_chunks.csv")) %>%
                           mutate(subset = sets_to_include[i]) %>%
                           select(recording_chunk_id, subset))
}

split_results <- split_results %>%
  unique()

# Only selecting annotation chunks with orthopteran or cicada annotations
recording_chunks_with_selected_species <- unique((annotation_chunks %>%
  filter(label %in% selectable_species))$recording_chunk_id)

selected_annotation_chunks <- annotation_chunks %>%
  filter(recording_chunk_id %in% recording_chunks_with_selected_species)

# Recategorizing and renaming metadata variables
annotated_audio_segments <- selected_annotation_chunks %>%
  left_join(split_results,
            by = "recording_chunk_id") %>%
  left_join(anthropophony_geophony_labels %>%
              rename("label_category" = "category",
                     "label" = "original_French_label"),
            by = c("label", "label_category")) %>%
  mutate(audio_segment_file_name = paste0(code_unique, "_split_", chunk_initial_time, "_", chunk_final_time, ".wav"),
         label = case_when(!is.na(English_label) ~ English_label,
                           label == "Mouche" ~ "Diptera sp.",
                           label == "Déplacement animal" ~ "Mammalia sp.",
                           label %in% c("Envol", "Plongeon") ~ "Aves sp.",
                           TRUE ~ label),
         label_category = case_when(label == "Passeriformes sp." ~ "Passeriformes",
                                    label_category == "Aves" & label != "Aves sp." ~ label_order,
                                    TRUE ~ label_category)) %>%
  rename("audio_segment_initial_time" = "chunk_initial_time",
         "audio_segment_final_time" = "chunk_final_time",
         "annotation_min_freq" = "min_frequency",
         "annotation_max_freq" = "max_frequency") %>%
  select(recording_id, audio_segment_initial_time, audio_segment_final_time, annotation_initial_time, annotation_final_time, annotation_min_freq, annotation_max_freq, label, label_category, subset, audio_segment_file_name) %>%
  filter(!is.na(subset),
         subset != "")

annotated_audio_segments[is.na(annotated_audio_segments)] <- ""


#---------------------------- ANNOTATED AUDIO SEGMENTS: NUMERIC SUMMARY BY LABEL ----------------------------

annotated_audio_segments_by_label_summary <- annotated_audio_segments %>%
  group_by(label, label_category) %>%
  summarize(n_audio_segments_in_train = n_distinct(audio_segment_file_name[subset == "train"]),
            n_audio_segments_in_val = n_distinct(audio_segment_file_name[subset == "val"]),
            n_audio_segments_in_test = n_distinct(audio_segment_file_name[subset == "test"])) %>%
  arrange(label_category, label)


#---------------------------- GENERATING TABLE 1 -------------------------------
n_chunks_by_category <- selected_annotation_chunks %>%
  group_by(categorie) %>%
  summarize(n_audio_segments_annotated = n_distinct(recording_chunk_id[confidence_level == 1]))

table1 <- ort_hem_recording_metadata %>%
  left_join(recordings_with_permission_to_publish_metadata %>%
              select(code_unique, categorie),
            by = "code_unique") %>%
  mutate(categorie = case_when(categorie == "Soundscape cédé par un auteur" & author_name %in% c("Dominik Arend", "David Sannier") ~ "Soundscape",
                                categorie == "Enregistrement focal cédé par un auteur" & author_name %in% c("Dominik Arend", "David Sannier") ~ "Enregistrement focal",
                                TRUE ~ categorie)) %>%
  group_by(categorie) %>%
  summarize(n_recordings = n_distinct(code_unique),
            n_recordings_exhaustively_annotated = n_distinct(code_unique[annotated == "Exhaustively annotated"]),
            n_recordings_partially_annotated = n_distinct(code_unique[annotated == "Partially annotated"]),
            total_minutes_recorded = round(sum(duration_min + duration_sec / 60))) %>%
  mutate(categorie = case_when(is.na(categorie) ~ "Soundscape cédé par un auteur", # These recordings are missing from the metadata file
                               TRUE ~ categorie)) %>%
  left_join(n_chunks_by_category,
            by = "categorie") %>%
  mutate(categorie = case_when(categorie == "Enregistrement focal" ~ "Expressly collected focal recording",
                               categorie == "Enregistrement focal cédé par un auteur" ~ "Pre-existing focal recording",
                               categorie == "Soundscape" ~ "Expressly collected soundscape",
                               categorie == "Soundscape cédé par un auteur" ~ "Pre-existing soundscape")) %>%
  rename("recording_category" = "categorie")

table1_file_name <- file.path(paste0(output_folder, "Tables/Table1.csv"))
write.csv(x=table1, file=table1_file_name, row.names = FALSE)


#---------------------------- GENERATING TABLE 2 -------------------------------
results_by_sound_producing_species <- singing_ort_hem_sp %>%
  rename("taxonomic_group" = "ordre") %>%
  full_join(total_animals_annotated %>%
              rowwise() %>%
              mutate(species_without_subspecies = paste(strsplit(species, " ")[[1]][1], strsplit(species, " ")[[1]][2]),
                     species = case_when(species_without_subspecies %in% singing_ort_hem_sp$species & !(species %in% singing_ort_hem_sp$species) ~ species_without_subspecies,
                                         TRUE ~ species)) %>%
              select(-species_without_subspecies),
            by = c("species", "taxonomic_group"))

results_by_sound_producing_species[is.na(results_by_sound_producing_species)] <- 0

table2 <- results_by_sound_producing_species %>%
  left_join(original_all_sound_categories %>%
              filter(ORDRE %in% selectable_orders) %>%
              select(LB_NOM, FAMILLE) %>%
              rename("species" = "LB_NOM",
                     "family" = "FAMILLE"),
            by = "species") %>%
  rename("category" = "taxonomic_group",
         "subspecies" = "species") %>%
  rowwise() %>%
  mutate(species = paste(strsplit(subspecies, " ")[[1]][1], strsplit(subspecies, " ")[[1]][2]),
         taxonomic_level = case_when(species != subspecies ~ subspecies,
                                     TRUE ~ species)) %>%
  ungroup() %>%
  mutate(category = case_when(!(category %in% c(selectable_orders, "Anthropophony", "Geophony")) ~ "Other biophony",
                              TRUE ~ category),
         family = case_when(category %in% selectable_orders ~ family,
                            TRUE ~ "-")) %>%
  group_by(category, family) %>%
  summarize(n_soniferous_species = n_distinct(species),
            n_soniferous_subspecies = n_distinct(subspecies),
            n_species_recorded = n_distinct(species[n_recordings > 0]),
            n_subspecies_recorded = n_distinct(subspecies[n_recordings > 0]),
            n_species_annotated = n_distinct(species[n_annotations > 0]),
            n_subspecies_annotated = n_distinct(subspecies[n_annotations > 0]),
            n_recordings = sum(n_recordings),
            n_recordings_annotated = sum(n_recordings_annotated),
            n_audio_segments_annotated = sum(n_annotated_chunks)) %>%
  mutate(n_soniferous_species = case_when(category %in% c("Anthropophony", "Geophony", "Other biophony") ~ "-",
                                          TRUE ~ paste0(as.character(n_soniferous_species), " (", n_soniferous_subspecies, ")")),
         n_species_recorded = case_when(category %in% c("Anthropophony", "Geophony") ~ "-",
                                        category == "Other biophony" ~ as.character(n_species_recorded),
                                        TRUE ~ paste0(as.character(n_species_recorded), " (", n_subspecies_recorded, ")")),
         n_species_annotated = case_when(category %in% c("Anthropophony", "Geophony") ~ "-",
                                         category == "Other biophony" ~ as.character(n_species_annotated),
                                         TRUE ~ paste0(as.character(n_species_annotated), " (", n_subspecies_annotated, ")"))) %>%
  select(-n_soniferous_subspecies, -n_subspecies_recorded, -n_subspecies_annotated)

table2$category <- factor(table2$category, levels = c("Hemiptera", "Orthoptera", "Other biophony", "Anthropophony", "Geophony"))

table2 <- table2 %>%
  arrange(category, family)

table2_file_name <- file.path(paste0(output_folder, "Tables/Table2.csv"))
write.csv(x=table2, file=table2_file_name, row.names = FALSE)


#------------------------------- GENERATING TABLE S1 --------------------------

n_online_recordings_by_species <- online_ort_hem_recording_metadata %>%
  separate_rows(recorded_species, sep = ", ") %>%
  group_by(recorded_species) %>%
  summarize(n_recordings_on_online_libraries = n_distinct(download_link, original_file_name, author_name, recording_date, recording_time))

sound_type_by_species <- ort_hem_species_by_stridulation_capacity %>%
  filter(!(Stridulation %in% c("Non", "Inconnue"))) %>%
  mutate(sound_type = case_when(Ordre == "Hemiptera" ~ "Timbalization",
                                Stridulation == "Crissement" ~ "Crepitation",
                                Stridulation == "Tambourinage" ~ "Drumming",
                                Stridulation == "Sons mandibulaires" ~ "Mandibular sounds",
                                Stridulation == "Oui" ~ "Stridulation",
                                TRUE ~ "Unknown"),
         frequency_range = case_when(`Fréquences.dominantes..kHz.` == "Audible" ~ "Audible",
                                     substr(`Fréquences.dominantes..kHz.`, 1, 1) == ">" ~ "Ultrasounds",
                                     TRUE ~ "Partially audible")) %>%
  rename("species" = "Espèce",
         "order" = "Ordre") %>%
  select(order, species, sound_type, frequency_range) %>%
  arrange(order, species)

tableS1 <- singing_ort_hem_sp %>%
  rename("taxonomic_group" = "ordre") %>%
  left_join(total_animals_annotated,
            by = c("species", "taxonomic_group")) %>%
  select(species, taxonomic_group, n_recordings, n_recordings_annotated) %>%
  rename("order" = "taxonomic_group") %>%
  left_join(annotated_audio_segments_by_label_summary %>%
              rename("species" = "label"),
            by = "species") %>%
  rename("n_annotated_audio_segments_in_train" = "n_audio_segments_in_train",
         "n_annotated_audio_segments_in_val" = "n_audio_segments_in_val",
         "n_annotated_audio_segments_in_test" = "n_audio_segments_in_test",
         "n_recordings_in_our_dataset" = "n_recordings") %>%
  left_join(n_online_recordings_by_species %>%
              rename("species" = "recorded_species") %>%
              rowwise() %>%
              mutate(species_without_subspecies = paste(strsplit(species, " ")[[1]][1], strsplit(species, " ")[[1]][2]),
                     species = case_when(!(species %in% singing_ort_hem_sp$species) & species_without_subspecies %in% singing_ort_hem_sp$species ~ species_without_subspecies,
                                         TRUE ~ species)) %>%
              ungroup() %>%
              group_by(species) %>%
              summarize(n_recordings_on_online_libraries = sum(n_recordings_on_online_libraries)),
            by = "species") %>%
  left_join(sound_type_by_species,
            by = c("species", "order")) %>%
  select(species, order, sound_type, frequency_range, n_recordings_on_online_libraries, n_recordings_in_our_dataset, n_recordings_annotated, n_annotated_audio_segments_in_train, n_annotated_audio_segments_in_val, n_annotated_audio_segments_in_test) %>%
  arrange(order, species)

tableS1[is.na(tableS1)] <- 0

tableS1_file_name <- file.path(paste0(output_folder, "Tables/TableS1.csv"))
write.csv(x=tableS1, file=tableS1_file_name, row.names = FALSE)


#---------------------------- GENERATING TABLE S2 -------------------------------
tableS2 <- ort_hem_recordings %>%
  rename("species" = "espece") %>%
  left_join(singing_ort_hem_sp,
            by = "species") %>%
  group_by(author_name, ordre) %>%
  summarize(n_recordings = n_distinct(code_unique),
            n_species = n_distinct(species)) %>%
  group_by(author_name) %>%
  summarize(n_hemipteran_recordings = sum(n_recordings[ordre == "Hemiptera"]),
            n_hemipteran_species = sum(n_species[ordre == "Hemiptera"]),
            n_orthopteran_recordings = sum(n_recordings[ordre == "Orthoptera"]),
            n_orthopteran_species = sum(n_species[ordre == "Orthoptera"])) %>%
  rename("recordist" = "author_name") %>%
  mutate(recordist = case_when(recordist == "DRIMM" ~ "Anonymous",
                               TRUE ~ gsub("Ł", "L", recordist))) %>%
  arrange(recordist)

tableS2_file_name <- file.path(paste0(output_folder, "Tables/TableS2.csv"))
write.csv(x=tableS2, file=tableS2_file_name, row.names = FALSE)


#---------------------------- GENERATING FIGURE 4 -------------------------------
adapted_tableS1_data <- tableS1 %>%
  filter(n_recordings_in_our_dataset > 0) %>%
  left_join(original_all_sound_categories %>%
              select(LB_NOM, SOUS_FAMILLE, FAMILLE, TRIBU) %>%
              rename("species" = "LB_NOM"),
            by = "species") %>%
  mutate(taxonomic_category = case_when(FAMILLE == "Cicadidae" ~ "Cicadoidea",
                                        FAMILLE %in% c("Gryllotalpidae", "Gryllidae", "Mogoplistidae", "Trigonidiidae", "Oecanthidae") ~ "Gryllotalpoidea and Grylloidea",
                                        SOUS_FAMILLE == "Tettigoniinae" ~ "Tettigoniinae",
                                        FAMILLE == "Tettigoniidae" ~ "Other katydid",
                                        TRIBU == "Gomphocerini" ~ "Gomphocerini",
                                        FAMILLE == "Acrididae" ~ "Other grasshopper",
                                        TRUE ~ "")) %>%
  select(-FAMILLE)

figure4_data <- data.frame(rbind(adapted_tableS1_data %>%
                                   mutate(labeling_type = " Weakly labeled",
                                          n_recordings = n_recordings_in_our_dataset - n_recordings_annotated) %>%
                                   rename("n_total_recordings" = "n_recordings_in_our_dataset") %>%
                                   select(species, taxonomic_category, labeling_type, n_recordings, n_total_recordings),
                                 adapted_tableS1_data %>%
                                   mutate(labeling_type = "Strongly labeled") %>%
                                   rename("n_recordings" = "n_recordings_annotated",
                                          "n_total_recordings" = "n_recordings_in_our_dataset") %>%
                                   select(species, taxonomic_category, labeling_type, n_recordings, n_total_recordings))) %>%
  unique()

x_axis_labels <- c("Cicadoidea", 
                   "Gryllotalpoidea and Grylloidea", 
                   "Tettigoniinae", 
                   "Other katydid", 
                   "Gomphocerini", 
                   "Other grasshopper")
figure_letters <- c("a", "b", "c", "d", "e", "f")
title_left_margins <- c(-220, -300, -250, -220, -300, -250)
title_text_size <- 30
x_axis_text_size <- 16
x_axis_label_text_size <- 16
plot_width <- 4000
plot_heights <- c(3000, 2000, 4500, 4300, 4600, 3000)
plot_left_margin <- 2.5
plot_text_size <- 16

for(i in 1:length(x_axis_labels)) {  
  pl <- ggplot(figure4_data %>%
                 filter(taxonomic_category == x_axis_labels[i]), aes(x = reorder(species, n_total_recordings), y = n_recordings, fill = labeling_type)) +
    geom_bar(position = "stack", 
             stat = "identity",
             colour = 'black') +
    coord_flip() +
    # xlab(paste0(x_axis_labels[i], " species")) +
    xlab("") +
    ylab("Number of recordings") +
    labs(fill = "Labeling type") +
    ggtitle(paste0(figure_letters[i], ")")) +
    scale_x_discrete(guide = guide_axis(angle = 0)) +
    scale_fill_manual(values = c("#A0DDFF", "#4E92DA")) +
    theme_bw() +
    theme(legend.position = case_when(x_axis_labels[i] == "Cicadoidea" ~ "top",
                                      TRUE ~ "none"),
          text = element_text(size = plot_text_size),
          axis.text.x = element_text(size = x_axis_text_size),
          axis.text.y = element_text(face = "italic"),
          axis.title.x = element_text(size = x_axis_label_text_size),
          plot.margin = unit(c(0.15, 0.15, 0.15, plot_left_margin), "cm"),
          plot.title = element_textbox(margin = margin(l = title_left_margins[i]), size = title_text_size))
  ggsave(paste0(output_folder, "Figures/Figure4", figure_letters[i], ".jpeg"), width = plot_width, height = plot_heights[i], dpi = 300, units = "px", limitsize = FALSE, plot = pl)
}
