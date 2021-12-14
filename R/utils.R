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
    for (i in 1:nrow(group_shape)) {
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
    sp_data.df <- point_to_shape_sp(data.tb)
    writeOGR(sp_data.df, ".", filename, driver = "ESRI Shapefile")
}

# Convert BBOX EPSG 4326
convert_bbox_epsg_4326 <- function(coords) {
    # Setting coordinates for bbox
    # coords = list(
    #     lon_min = -64.9583215,
    #     lon_max = -64.7443645,
    #     lat_min = -10.7323301,
    #     lat_max = -10.5699696
    # )
    # Creating polygon with coordinates
    poly <- sf::st_polygon(
        list(
            rbind(
                c(coords$lon_min, coords$lat_min), 
                c(coords$lon_max, coords$lat_min), 
                c(coords$lon_max, coords$lat_max), 
                c(coords$lon_min, coords$lat_max), 
                c(coords$lon_min, coords$lat_min)
            )
        )
    )
    # Adding the projection
    poly_sf <- sf::st_sfc(poly, crs = 4326)
    # Setting the polygon projection
    poly_sf <- sf::st_transform(
        poly_sf,
        crs = "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs "
    )
    return(sf::st_bbox(poly_sf))
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
random_samples_per_class <- function(input_data.tb, n_per_class, classes = c()) {
    if (length(classes) == 0) {
        labelled_samples <- input_data.tb %>%
            dplyr::group_by(label) %>% 
            dplyr::sample_n(size = n_per_class) %>%
            dplyr::ungroup()
        class(labelled_samples) <- c("sits", class(labelled_samples))
        return(labelled_samples)
    } else {
        aux_ = as.data.frame(list())
        for (label_ in classes) {
            data_ <- dplyr::filter(input_data.tb, label == label_)
            sel <- sample(nrow(data_), n_per_class, replace = FALSE)
            random_samples <- data_[sel, ]
            aux_ <- rbind(aux_, random_samples)
        }
        class(aux_) <- c("sits", class(aux_))
        return(aux_)
    }
}

# Select random samples from number per class
random_samples <- function(input_data.tb, n_samples, class = "") {
    if (class %in% input_data.tb$label) {
        data.tb <- dplyr::filter(input_data.tb, label == class)
        sel <- sample(nrow(data.tb), n_samples, replace = FALSE)
        return(data.tb[sel, ])
    } else {
        sel <- sample(nrow(input_data.tb), n_samples, replace = FALSE)
        return(input_data.tb[sel, ])
    }
    
}

# Get the training and testing from data set
get_train_test_set <- function(input_data.tb, prop = 0.7) {
    rows <- sample(nrow(input_data.tb), replace = FALSE)
    samples <- input_data.tb[rows, ]
    dt <- sample(nrow(samples), nrow(samples) * prop, replace = FALSE)
    return(list(train = samples[dt,], test = samples[-dt,]))
}

# Get accuracy metrics from confusion matrix
get_accuracy_metrics <- function(cm) {
    overall_accuracy  <- cm$overall["Accuracy"]
    by_class <- cm[["byClass"]]
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

# Get Active Learning Metrics for Result Classification
get_metrics <- function(points_tb) {
    metrics <- as.data.frame(list())
    for (i in 1:nrow(points_tb)) {
        true_class <- points_tb[i, ]["label"]
        pred_df <- points_tb[i, ][["predicted"]][[1]]
        probs <- unlist(pred_df[["probs"]][[1]])
        least_conf <- (1 - max(probs)) * length(probs) / (length(probs) - 1)
        best_two <- sort(probs, decreasing = TRUE)[1:2]
        names(best_two) <- NULL
        margin_conf <- 1 - (best_two[1] - best_two[2])
        ratio_conf <- best_two[1] / best_two[2]
        entropy <- -1 * sum(probs * log(probs))
        metrics <- rbind(
            metrics,
            data.frame(
                entropy = entropy,
                least_conf = least_conf,
                margin_conf = margin_conf,
                ratio_conf = ratio_conf,
                prediction = pred_df[["class"]],
                true_class = true_class
            )
        )
    }
    return(metrics)
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
