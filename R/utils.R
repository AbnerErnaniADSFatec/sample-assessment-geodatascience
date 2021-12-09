# The libraries to extract time series metada
# for Land Use and Land Cover Samples.
library(sits)
library(sitsdata)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(plotly)
library(stars)
library(leaflet)
library(stats)
library(dendextend)
library(caret)
library(dplyr)
library(magrittr)
library(readr)
library(keras)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(fitdistrplus)
library(httr)
library("rjson")
library(TSdist)
library(dtw)
library(jsonlite)
library(activelearning)
library(caret)
library(sp)

# Reading different extensions of files using the same
# method with a key ext to extension.
read_file.tb <- function(file, ext) {
  data.tb <- NULL
  if (ext == "csv") {
    data.tb <- read.csv(file)
  }
  if (ext == "rds") {
    data.tb <- readRDS(file)
  }
  if (ext == "rda") {
    data.tb <- get(load(file))
  }
  return(data.tb)
}

# Save different file extensions using the same
# method with a key to extension.
save_file.tb <- function(data.tb, name, ext) {
    if (ext == "csv") {
        write.csv(data.tb, paste(name, ".csv", sep=""), row.names = FALSE)
    }
    if (ext == "rda") {
        save(data.tb, file = paste(name, ".rda", sep=""))
    }
}

# Get colors for LULC classes and the status for quality control
color.label <- function(label = "") {
    colors <- list(
        "Pasture" = "#ff8828",
        "Forest" = "#005500",
        "NonForest" = "#0fc80f",
        "Deforestation" = "#ff5f4c",
        "clean" = "green",
        "analyze" = "yellow",
        "remove" = "red"
    )
    return(colors[[label]])
}

# Colorize the som map with class colors saved above
to.color <- function(clustering.lst) {
    clustering <- clustering.lst
    neurons <- c(1:length(clustering.lst$som_properties$neuron_label))
    for (neuron in neurons) {
        label <- clustering.lst$som_properties$neuron_label[[neuron]]
        new_color <- color.label(label)
        if (!is.null(new_color)) {
            clustering$som_properties$paint_map[neuron] <- as.character(new_color)
        }
    }
    return(clustering)
}

# Get a data tibble file and converts to a readable shapefile.
# This shape file can be read by leaflet function to display maps.
point_to_shape_sp <- function (data.tb, date, class_label) {
    try(
        data.tb <- dplyr::filter(
            data.tb,
            (as.Date(start_date) <= as.Date(date)) &
            (as.Date(date) <= as.Date(end_date))
        )
    )
    group_shape <- dplyr::select(data.tb,
        longitude, latitude,
        start_date, end_date,
        label
    )
    try(
        group_shape <- dplyr::select(data.tb,
            longitude, latitude,
            start_date, end_date,
            label, cube
        )
    )
    try(
        group_shape <- dplyr::select(data.tb,
            longitude, latitude,
            start_date, end_date,
            label, cube, sample_id
        )
    )
    try(
        group_shape <- dplyr::select(data.tb,
            longitude, latitude,
            start_date, end_date,
            label, cube,
            id_neuron, eval, post_prob
        )
    )
    colors <- c()
    for (i in 1:length(group_shape$label)) {
        colors <- append(colors, color.label(group_shape[i, ]$label))
    }
    group_shape <- group_shape %>% dplyr::mutate(color = colors)
    sp_data.tb.df <- as.data.frame(group_shape)
    try(
        sp_data.tb.df <- sp_data.tb.df %>% filter(label == class_label)
    )
    points_SF <- as.data.frame(sp_data.tb.df)
    xy <- points_SF[, c(1, 2)]
    sp_data.df <- sp::SpatialPointsDataFrame(
        coords = xy,
        data = points_SF,
        proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    return(sp_data.df)
}

# Get a data tibble file and save this data in a shape file
save_shapefile <- function(data.tb, filename) {
    group_shape <- dplyr::select(data.tb,
        longitude, latitude,
        start_date, end_date,
        label
    )
    group_shape <- dplyr::select(data.tb,
        longitude, latitude,
        start_date, end_date,
        label
    )
    try(
        group_shape <- dplyr::select(data.tb,
            longitude, latitude,
            start_date, end_date,
            label, cube, -time_series
        )
    )
    try(
        group_shape <- dplyr::select(data.tb,
            longitude, latitude,
            start_date, end_date,
            label, cube,
            id_neuron, eval, post_prob
        )
    )
    sp_data.tb.df <- as.data.frame(group_shape)
    points_SF <- as.data.frame(sp_data.tb.df)
    xy <- points_SF[, c(1, 2)]
    sp_data.df <- sp::SpatialPointsDataFrame(
        coords = xy,
        data = points_SF,
        proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    writeOGR(sp_data.df, ".", filename, driver = "ESRI Shapefile")
}

# remove the time_series and cube.
# This method provides an abstraction to view clean data samples.
as_sample <- function(data.tb) {
    data.tb <- as.data.frame(data.tb)
    try(data.tb <- dplyr::select(data.tb, -cube))
    try(data.tb <- dplyr::mutate(data.tb, cube = coverage, .after = 5))
    try(data.tb <- dplyr::select(data.tb, -coverage, -time_series))
    try(data.tb <- dplyr::select(data.tb, -time_series))
    return(data.tb)
}

# Select random samples from number per class
random_samples <- function(input_data.tb, n_per_class, classes = c()) {
    if (length(classes) == 0) {
        labelled_samples <- input_data.tb %>%
            dplyr::group_by(label) %>% 
            dplyr::sample_n(size = n_labelled) %>%
            dplyr::ungroup()
        return(labelled_samples)
    } else {
        aux_ = as.data.frame(list())
        for (label_ in classes) {
            data_ <- dplyr::filter(input_data.tb, label == label_)
            sel <- sample(nrow(data_), n_per_class, replace = FALSE)
            random_samples <- data_[sel, ]
            aux_ <- rbind(aux_, random_samples)
        }
        return(aux_)
    }
}

get_accuracy_metrics <- function(cm) {
    overall_accuracy  <- cm$overall["Accuracy"]
    by_class <- conf_mat[["byClass"]]
    f1_score <- by_class[, "F1"]
    prod_acc <- by_class[, "Pos Pred Value"]
    user_acc <- by_class[, "Sensitivity"]
    class_names <- stringr::str_sub(rownames(by_class), 8)
    return(
        tibble::tibble(class = class_names, metric = "f1_score", accuracy = f1_score) %>%
            dplyr::bind_rows(
                tibble::tibble(class = class_names, metric = "prod_acc", accuracy = prod_acc)
            ) %>%
                dplyr::bind_rows(
                    tibble::tibble(class = class_names, metric = "user_acc", accuracy = user_acc)
                ) %>%
                    dplyr::add_row(class = "overall", metric = "accuracy", accuracy = overall_accuracy)
    )
}

# An implementation for viewing the confusion matrix of a certain algorithm.
cm_plot <- function(cm, x_ac = 1, model_type = "Default"){
  p <- ggplot(data = as.data.frame(cm$table), aes(x = Prediction,y = Reference)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
        geom_text(aes(x = Prediction, y = Reference, label = Freq), size = 8) +
            labs(x = "Referência", y = "Predição", title= paste("Matriz de confusão - ", model_type)) +
                theme_classic() +
                    theme(
                        axis.text.x = element_text(angle = 0, color = "black", size  = 17),
                        axis.title.x = element_text(size=17),
                        plot.title = element_text(size = 14, hjust= 0.5),
                        axis.text.y = element_text(size = 14, color="black"),
                        axis.title.y = element_text(size = 14)
                    )
    return(p)
}

# Get the mode based on statistical moments.
# Get the value that appears the most.
getMode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Get range based on Maximum - Minumum
# value from time series.
getRange <- function(v) {
    return(max(v) - min(v))
}

# Get distribution for time series.
# Count each values from time series and count the values.
getDistribution <- function(vector){
    distribution <- data.frame()
    vector <- sort(vector)
    for (i in 1:length(vector)) {
        values <- c(
            vector[i],
            length(which(vector == vector[i]))
        )
        distribution <- rbind(
            distribution,
            values
        )
    }
    colnames(distribution) <- c("value", "count")
    return(distribution)
}
