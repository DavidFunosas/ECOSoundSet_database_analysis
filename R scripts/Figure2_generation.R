library(terra)
library(ggplot2)
library(paletteer)
library(ggthemes)
library(dplyr)
library(scales)

# Function to load and rasterize shapefiles
rasterize_shapefile <- function(shapefile_path, raster_template) {
  # Loading the shapefile
  vector_layer <- sf::st_read(shapefile_path)
  
  # Converting to raster using a raster template for resolution and extent
  raster_layer <- rast(raster_template)
  
  # Rasterizing the vector layer (converting to binary raster: 1 for presence, 0 for absence)
  # Setting 'field = NULL' to rasterize based on geometry (no specific attribute field)
  rasterized_layer <- rasterize(vector_layer, raster_layer, field = NULL, fun = "count", background = 0)
  
  return(rasterized_layer)
}

# Function to count overlaps between multiple rasters
count_overlaps <- function(rasters) {
  # Stacking the rasters together to create a single object
  raster_stack <- rast(rasters)
  
  # Summing the rasters to count overlaps (cell values will represent overlap count)
  overlap_raster <- sum(raster_stack)
  
  return(overlap_raster)
}

# Function to count overlaps and calculate the proportion of raster layers fulfilling a condition
count_overlaps_proportion <- function(present_species_rasters, absent_species_rasters) {
  # Stacking the rasters together to create a single object
  raster_stack <- rast(c(present_species_rasters, absent_species_rasters))  # Stack the rasters
  
  # Creating a condition mask: each cell is 1 if the condition is met, 0 otherwise
  present_species_condition_mask <- lapply(present_species_rasters, function(r) {
    r_condition <- r > 0  # Roundabout way of returning 1
    return(r_condition)
  })
  
  absent_species_condition_mask <- lapply(absent_species_rasters, function(r) {
    r_condition <- as.numeric(is.na(r))  # Roundabout way of returning 0
    return(r_condition)
  })
  
  # Stacking the condition masks (binary rasters indicating where the condition is met)
  condition_stack <- rast(c(present_species_condition_mask, absent_species_condition_mask))
  
  # Summing the rasters to count total overlaps (cell values will represent overlap count)
  total_overlap_raster <- sum(raster_stack)
  
  # Summing the condition masks to count the number of rasters fulfilling the condition
  condition_overlap_raster <- sum(condition_stack)
  
  # Calculating the proportion: condition overlaps / total overlaps
  proportion_raster <- condition_overlap_raster / total_overlap_raster
  
  return(proportion_raster)
}

format_longitude <- function(x) {
  ifelse(x >= 0, paste0(x, "ºE"), paste0(abs(x), "ºW"))
}

# Function to visualize the overlap density
visualize_overlap_density <- function(overlap_raster, output_folder) {
  # Converting the input raster to a data frame for ggplot2
  overlap_df <- as.data.frame(overlap_raster, xy = TRUE, na.rm = TRUE) %>%
    filter(sum != 0) %>% # Filtering out non-European regions
    mutate(discrete_sum = as.factor(case_when(sum == 1 ~ "100",
                     sum > 0.95 ~ "95-99",
                     sum > 0.9 ~ "90-94",
                     sum > 0.85 ~ "85-89",
                     sum > 0.8 ~ "80-84",
                     sum > 0.75 ~ "75-79",
                     sum > 0.7 ~ "70-74",
                     sum > 0.65 ~ "65-69",
                     sum > 0.6 ~ "60-64",
                     sum > 0.55 ~ "55-59",
                     sum > 0.5 ~ "50-54",
                     TRUE ~ "0-49")))
  
  overlap_df$discrete_sum <- factor(overlap_df$discrete_sum, levels = c("100", 
                                                                        "95-99", 
                                                                        "90-94", 
                                                                        "85-89", 
                                                                        "80-84", 
                                                                        "75-79", 
                                                                        "70-74", 
                                                                        "65-69", 
                                                                        "60-64", 
                                                                        "55-59", 
                                                                        "50-54", 
                                                                        "45-49",
                                                                        "0-49"))
  
  discrete_palette <- paletteer_c("grDevices::Blues 3", 13, direction = 1)
  
  # Plotting the overlap density map using ggplot2
  pl <- ggplot(overlap_df, aes(x = x, y = y, fill = discrete_sum)) +
    geom_tile() +
    scale_fill_manual(values = discrete_palette) +
    labs(fill = "Estimated percentage of\nsoniferous orthopteran\nspecies present in\nour dataset") +
    theme_minimal() +
    scale_x_continuous(labels = format_longitude) + 
    scale_y_continuous(labels = label_number(suffix = "ºN")) +
    theme(axis.title = element_blank())
  ggsave(paste0(output_folder, "Figures/Figure2.jpeg"), width = 2300, height = 1800, dpi = 300, units = "px", limitsize = FALSE, plot = pl)
  pl
}

output_folder <- "../Output/"
species_distribution_shapefiles_folder <- "../Input/Species distribution shapefiles/"

# Defining the paths to the shapefiles
species_in_our_dataset_directory <- paste0(species_distribution_shapefiles_folder, "North Central and temperate Western Europe - species in our dataset")
species_not_in_our_dataset_directory <- paste0(species_distribution_shapefiles_folder, "North Central and temperate Western Europe - species absent from our dataset")
species_not_in_temperate_Western_Europe_directory <- paste0(species_distribution_shapefiles_folder, "Other parts of Europe")

present_species_shapefile_paths <- list.files(species_in_our_dataset_directory, pattern = ".shp$", full.names = TRUE, recursive = TRUE)
temperate_Western_Europe_absent_species_shapefile_paths <- list.files(species_not_in_our_dataset_directory, pattern = ".shp$", full.names = TRUE, recursive = TRUE)
other_parts_of_Europe_absent_species_shapefile_paths <- list.files(species_not_in_temperate_Western_Europe_directory, pattern = ".shp$", full.names = TRUE, recursive = TRUE)
absent_species_shapefile_paths <- c(temperate_Western_Europe_absent_species_shapefile_paths, other_parts_of_Europe_absent_species_shapefile_paths)

# Defining a raster template
raster_template <- rast(nrows = 1000, ncols = 1000, xmin = -11, xmax = 42, ymin = 33, ymax = 71)

# Lists to store rasterized layers
present_species_raster_layers <- list()
absent_species_raster_layers <- list()

# Rasterizing each shapefile and storing it in the list
for (shapefile in present_species_shapefile_paths) {
  present_species_raster_layers[[shapefile]] <- rasterize_shapefile(shapefile, raster_template)
}

for (shapefile in absent_species_shapefile_paths) {
  absent_species_raster_layers[[shapefile]] <- rasterize_shapefile(shapefile, raster_template)
}

proportion_raster <- count_overlaps_proportion(present_species_raster_layers, absent_species_raster_layers)

visualize_overlap_density(proportion_raster, output_folder)
