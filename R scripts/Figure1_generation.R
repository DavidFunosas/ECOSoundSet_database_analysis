library(sf)
library(ggplot2)
library(ggtext)
library(rnaturalearth)
library(rnaturalearthhires)
library(dplyr)
library(viridis)
library(ggthemes)
library(giscoR)
library(paletteer)
library(tidyr)

output_folder <- "../Output/"
ort_hem_recording_metadata <- read.csv("../Input/Recording metadata/recording_metadata.csv")

# European countries bigger than 300,000 km² for which we have recordings
countries_to_split_in_regions <- c("Spain", "France", "Germany", "Poland", "Italy")
countries_to_split_in_regions_codes <- c("ES", "FR", "DE", "PL", "IT")

# Making sure the region names coincide with the ones specified in the recording metadata
region_scale_link <- ne_states(country = countries_to_split_in_regions, returnclass = "sf") %>%
  select(iso_a2, region, gn_name) %>%
  unique() %>%
  mutate(gn_name = gsub("Departement d'", "", 
                        gsub("Departement de ", "", 
                             gsub("Departement de la ", "", 
                                  gsub("Departement du ", "", 
                                       gsub("Departement des ", "", 
                                            gsub("Departement de l'", "", 
                                                 gsub("Provincia de ", "", 
                                                      gsub("Canton de ", "", 
                                                           gsub("Canton du ", "", 
                                                                gsub("Cantone ", "",  
                                                                     gsub("Land ", "",  
                                                                          gsub("Freistaat ", "",  
                                                                               gsub("Wojewodztwo ", "", gn_name))))))))))))),
         region = case_when(region == "Valenciana" ~ "Comunitat Valenciana",
                            region == "Asturias" ~ "Principado de Asturias",
                            region == "Foral de Navarra" ~ "Comunidad Foral de Navarra",
                            region == "Madrid" ~ "Comunidad de Madrid",
                            region == "Islas Baleares" ~ "Illes Balears",
                            region == "Murcia" ~ "Región de Murcia",
                            region == "Provence-Alpes-Côte-d'Azur" ~ "Provence-Alpes-Côte d’Azur",
                            region == "Île-de-France" ~ "Ile-de-France",
                            region == "Centre-Val de Loire" ~ "Centre — Val de Loire",
                            region == "Sicily" ~ "Sicilia",
                            iso_a2 %in% c("DE", "PL") ~ gn_name,
                            TRUE ~ region))

split_metadata <- ort_hem_recording_metadata %>%
  separate_rows(recorded_species, sep = ", ")

# Calculating the number of recordings for each country, or region if appropriate
region_data <- rbind(rbind(split_metadata %>%
                             filter(country_code %in% c("DE", "PL", "FR", "ES")) %>%
                             mutate(region = gsub("è", "e", 
                                                  gsub("ô", "o", 
                                                       gsub("é", "e", region)))) %>%
                             right_join(region_scale_link,
                                        by = c("country_code" = "iso_a2", "region" = "gn_name")) %>%
                             mutate(region = region.y) %>%
                             select(-region.y) %>%
                             mutate(region = case_when(region %in% c("Lodzkie", "Swietokrzyskie") ~ "Makroregion centralny",
                                                       region %in% c("Malopolskie", "Slaskie") ~ "Makroregion południowy",
                                                       region %in% c("Lubelskie", "Podkarpackie", "Podlaskie") ~ "Makroregion wschodni",
                                                       region %in% c("Wielkopolskie", "Zachodniopomorskie", "Lubuskie") ~ "Makroregion północno-zachodni",
                                                       region %in% c("Dolnoslaskie", "Opolskie") ~ "Makroregion południowo-zachodni",
                                                       region %in% c("Kujawsko-Pomorskie", "Warminsko-Mazurskie", "Pomorskie") ~ "Makroregion północny",
                                                       region %in% c("Warszawski stoleczny", "Mazowiecki regionalny") ~ "Makroregion województwo mazowieckie",
                                                       TRUE ~ region)),
                           split_metadata %>%
                             filter(country_code == "IT") %>%
                             right_join(region_scale_link,
                                        by = c("country_code" = "iso_a2", "region" = "region")) %>%
                             mutate(region = case_when(region %in% c("Piemonte", "Valle d'Aosta", "Liguria", "Lombardia") ~ "Nord-Ovest",
                                                       region %in% c("Abruzzo", "Molise", "Campania", "Puglia", "Basilicata", "Calabria") ~ "Sud",
                                                       region %in% c("Sicilia", "Sardegna") ~ "Isole",
                                                       region %in% c("Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Emilia-Romagna") ~ "Nord-Est",
                                                       region %in% c("Toscana", "Umbria", "Marche", "Lazio") ~ "Centro",
                                                       TRUE ~ region)) %>%
                             select(-gn_name)
                           ) %>%
                       filter(!is.na(recording_id)) %>%
                       group_by(country_code, region) %>%
                       summarize(n_recordings = n_distinct(recording_id),
                                 n_species = n_distinct(recorded_species)) %>%
                       filter(region != ""), # Grouping at the regional level
                     split_metadata %>%
                       filter(!(country_code %in% countries_to_split_in_regions_codes)) %>%
                       group_by(country_code) %>%
                       summarize(n_recordings = n_distinct(recording_id),
                                 n_species = n_distinct(recorded_species)) %>%
                       mutate(region = "") # Grouping at the national level
                     ) %>%
  filter(country_code != "") %>%
  arrange(country_code, region)

# Loading European countries
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>% 
  filter(continent == "Europe")

# Loading Spain Autonomous Communities
spain_regions <- gisco_get_nuts(year = 2021, nuts_level = 2, country = "ES") %>%
  rename(gn_name = NUTS_NAME)

# Loading France Regions
france_regions <- gisco_get_nuts(year = 2021, nuts_level = 1, country = "FR") %>%
  rename(gn_name = NUTS_NAME)

# Loading German States
germany_regions <- gisco_get_nuts(year = 2021, nuts_level = 1, country = "DE") %>%
  rename(gn_name = NUTS_NAME) %>%
  mutate(gn_name = case_when(gn_name == "Thüringen" ~ "Thuringen",
                             gn_name == "Baden-Württemberg" ~ "Baden-Wuerttemberg",
                             TRUE ~ gn_name))

# Loading Polish Makroregions
poland_regions <- gisco_get_nuts(year = 2021, nuts_level = 1, country = "PL") %>%
  rename(gn_name = NUTS_NAME)

# Loading Italian Statistical Regions
italy_regions <- gisco_get_nuts(year = 2021, nuts_level = 1, country = "IT") %>%
  rename(gn_name = NUTS_NAME) %>%
  mutate(gn_name = case_when(gn_name == "Centro (IT)" ~ "Centro",
                             TRUE ~ gn_name))

subdivisions <- bind_rows(spain_regions,
                          france_regions,
                          germany_regions,
                          poland_regions,
                          italy_regions)

# Joining the summary by region with the corresponding polygons
grouped_region_data <- region_data %>%
  mutate(n_recordings = case_when(is.na(n_recordings) ~ 0,
                                  TRUE ~ n_recordings),
         n_species = case_when(is.na(n_species) ~ 0,
                                  TRUE ~ n_species)) %>%
  right_join(subdivisions %>%
               select(CNTR_CODE, gn_name, geometry),
             by = c("country_code" = "CNTR_CODE", "region" = "gn_name")) %>%
  ungroup()

current_crs <- st_crs(grouped_region_data)

processed_geometries <- vector("list", length = length(grouped_region_data$geometry))
for (i in 1:length(grouped_region_data$geometry)) {
  processed_geometries[[i]] <- grouped_region_data$geometry[[i]] 
}

# Transforming the geometry objects into the right format
processed_sfc <- st_sfc(processed_geometries, crs = current_crs)

grouped_region_data$geometry <- processed_sfc
grouped_region_data <- st_sf(grouped_region_data)

grouped_region_data <- st_set_crs(grouped_region_data, 4326)

grouped_region_data$geometry <- st_cast(grouped_region_data$geometry, "GEOMETRY")
grouped_region_data$geometry <- st_cast(grouped_region_data$geometry, "POLYGON")
grouped_region_data$geometry <- st_transform(grouped_region_data$geometry, crs = st_crs(europe))

grouped_region_data$geometry <- st_make_valid(grouped_region_data$geometry)

countries <- ne_countries(scale = "medium", returnclass = "sf")

# Assigning country-wide values to the European countries that we will not split into regions
country_values <- europe %>% 
  group_by(sovereignt) %>%
  filter(sovereignt != "Russia") %>%
  left_join(region_data %>%
              filter(!(country_code %in% countries_to_split_in_regions_codes)), 
            by = c("iso_a2" = "country_code")) %>%
  summarise(n_recordings = sum(n_recordings, na.rm = TRUE),
            n_species = sum(n_species, na.rm = TRUE))

# Joining regional and national polygons
all_polygons <- rbind(grouped_region_data %>%
                        select(n_recordings, n_species, geometry),
                      country_values %>%
                        filter(!(sovereignt %in% countries_to_split_in_regions)) %>%
                        select(n_recordings, n_species, geometry))

# Transforming the number of recordings and species into categorical values
all_polygons <- all_polygons %>%
  mutate(discrete_n_recordings  = as.factor(case_when(n_recordings == 0 | is.na(n_recordings) ~ "0",
                                                      n_recordings < 10 ~ "1-9",
                                                      n_recordings < 50 ~ "10-49",
                                                      n_recordings < 100 ~ "50-99",
                                                      n_recordings < 250 ~ "100-249",
                                                      n_recordings < 500 ~ "250-499",
                                                      n_recordings < 1000 ~ "500-999",
                                                      n_recordings < 2000 ~ "1000-2499",
                                                      TRUE ~ "2500-4999")),
         discrete_n_species  = as.factor(case_when(n_species == 0 | is.na(n_species) ~ "0",
                                                   n_species < 10 ~ "1-9",
                                                   n_species < 25 ~ "10-24",
                                                   n_species < 50 ~ "25-49",
                                                   n_species < 74 ~ "50-74",
                                                   n_species < 100 ~ "75-99",
                                                   n_species < 125 ~ "100-124",
                                                   TRUE ~ "125-149")))

all_polygons$discrete_n_recordings <- factor(all_polygons$discrete_n_recordings, levels = c("0", "1-9", "10-49", "50-99", "100-249", "250-499", "500-999", "1000-2499", "2500-4999"))
all_polygons$discrete_n_species <- factor(all_polygons$discrete_n_species, levels = c("0", "1-9", "10-24", "25-49", "50-74", "75-99", "100-124", "125-149"))

discrete_green_palette <- paletteer_c("grDevices::Greens 3", 9, direction = -1)
discrete_orange_palette <- c("white", paletteer_d("ggsci::orange_material", 7, direction = 1))

# Plotting the number of recordings by country/region
pl <- ggplot() +
  geom_sf(data = all_polygons, aes(fill = discrete_n_recordings), color = "grey60", size = 0.025) +  # Region borders
  geom_sf(data = country_values, fill = NA, color = "black", size = 1.5) +  # Country borders
  scale_fill_manual(values = setNames(discrete_green_palette, levels(all_polygons$discrete_n_recordings))) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_textbox(size = 26, margin = margin(l = -30))) +
  coord_sf(xlim = c(-12, 36), ylim = c(35, 70)) +
  labs(fill = "Number of recordings", title = "a)")
ggsave(paste0(output_folder, "Figures/Figure1a.jpeg"), width = 2000, height = 2000, dpi = 300, units = "px", limitsize = FALSE, plot = pl)

# Plotting the number of species by country/region
pl <- ggplot() +
  geom_sf(data = all_polygons, aes(fill = discrete_n_species), color = "grey60", size = 0.025) +  # Region borders
  geom_sf(data = country_values, fill = NA, color = "black", size = 1.5) +  # Country borders
  scale_fill_manual(values = setNames(discrete_orange_palette, levels(all_polygons$discrete_n_species))) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_textbox(size = 26, margin = margin(l = -30))) +
  coord_sf(xlim = c(-12, 36), ylim = c(35, 70)) +
  labs(fill = "Number of species", title = "b)")
ggsave(paste0(output_folder, "Figures/Figure1b.jpeg"), width = 2000, height = 2000, dpi = 300, units = "px", limitsize = FALSE, plot = pl)
