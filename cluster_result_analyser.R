# read input argument
options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)
print(args)

# read the first argument as the input file
cluster_results <- read.csv2(args[1], sep="\t", na.strings="NA", stringsAsFactors=FALSE, fill=TRUE)

# remove input arguments
rm(args)

# formatting the columns
cluster_results$AVG_PMZ <- as.numeric(as.character(cluster_results$AVG_PMZ))
cluster_results$AVG_PMZ_HIGHEST <- as.numeric(as.character(cluster_results$AVG_PMZ_HIGHEST))
cluster_results$MAX_PMZ <- as.numeric(as.character(cluster_results$MAX_PMZ))
cluster_results$MAX_PMZ <- as.numeric(as.character(cluster_results$MAX_PMZ))
cluster_results$MIN_PMZ <- as.numeric(as.character(cluster_results$MIN_PMZ))
cluster_results$PMZ_RANGE <- as.numeric(as.character(cluster_results$PMZ_RANGE))
cluster_results$MAX_PMZ_HIGHEST <- as.numeric(as.character(cluster_results$MAX_PMZ_HIGHEST))
cluster_results$MIN_PMZ_HIGHEST <- as.numeric(as.character(cluster_results$MIN_PMZ_HIGHEST))
cluster_results$PMZ_RANGE_HIGHEST <- as.numeric(as.character(cluster_results$PMZ_RANGE_HIGHEST))
cluster_results$MAX_RATIO <- as.numeric(as.character(cluster_results$MAX_RATIO))

# Create a high quality subset
high_quality_cluster_results <- cluster_results[cluster_results$MAX_RATIO>=0.7 & cluster_results$NUM_PROJECTS>=2 & cluster_results$NUM_SPECTRA >= 10,]

# plotting
library(ggplot2)
library(gridExtra)

# plot the ratio vs size (min 10 spectra)
ggplot(cluster_results, aes(x=NUM_SPECTRA, y=MAX_RATIO)) +
    geom_point(shape=1) +
    ggtitle("Ratio vs Size (min 10 spectra)") +
    xlab("Number of Spectra") +
    ylab("PSM Ratio")

# plot the ratio distribution in density histogram
ggplot(cluster_results, aes(x=MAX_RATIO)) +
    geom_histogram(aes(y=..density..), binwidth=.01, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("PSM Ratio Distribution") +
    xlab("PSM Ratio") +
    ylab("Density")

# plot the ratio distribution for high quality clusters
ggplot(high_quality_cluster_results, aes(x=MAX_RATIO)) +
    geom_histogram(aes(y=..density..), binwidth=.01, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("PSM Ratio Distrition for High Quality Clusters") +
    xlab("PSM Ratio") +
    ylab("Density")


# plot the size distribution
ggplot(cluster_results, aes(x=NUM_SPECTRA)) +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("Cluster Size Distribution") +
    xlab("Number Of Spectra") +
    ylab("Density")

ggplot(cluster_results, aes(x=factor(0), y = NUM_SPECTRA)) +
    geom_boxplot() +
    ggtitle("Cluster Size Distribution") +
    xlab("") +
    ylab("Number Of Spectra")

# plot the size distribution for high quality cluster
ggplot(high_quality_cluster_results, aes(x=NUM_SPECTRA)) +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("Cluster Size Distribution For High Quality Cluster") +
    xlab("Number Of Spectra") +
    ylab("Density")

ggplot(high_quality_cluster_results, aes(x=factor(0), y = NUM_SPECTRA)) +
    geom_boxplot() +
    ggtitle("Cluster Size Distribution") +
    xlab("") +
    ylab("Number Of Spectra")

# plot the precursor m/z range
ggplot(cluster_results, aes(x=NUM_SPECTRA, y=PMZ_RANGE)) +
    geom_point(shape=1) +
    ggtitle("Precursor m/z Range vs Size (min 10 spectra)") +
    xlab("Number of Spectra") +
    ylab("Precursor m/z Range")

# plot the precursor m/z range for high quality cluster
ggplot(high_quality_cluster_results, aes(x=NUM_SPECTRA, y=PMZ_RANGE)) +
    geom_point(shape=1) +
    ggtitle("Precursor m/z Range vs Size For High Quality Cluster") +
    xlab("Number of Spectra") +
    ylab("Precursor m/z Range")


# plot the number of projects
spectra_projects <- ggplot(cluster_results, aes(x=NUM_SPECTRA, y=NUM_PROJECTS)) +
    geom_point(shape=1) +
    ggtitle("All Clusters") +
    xlab("Number of Spectra") +
    ylab("NUmber of Projects")

spectra_projects_high <- ggplot(high_quality_cluster_results, aes(x=NUM_SPECTRA, y=NUM_PROJECTS)) +
    geom_point(shape=1) +
    ggtitle("High Quality Clusters") +
    xlab("Number of Spectra") +
    ylab("NUmber of Projects")

ratio_projects <- ggplot(cluster_results, aes(x=MAX_RATIO, y=NUM_PROJECTS)) +
    geom_point(shape=1) +
    ggtitle("All Clusters") +
    xlab("Ratio") +
    ylab("NUmber of Projects")

ratio_projects_high <- ggplot(high_quality_cluster_results, aes(x=MAX_RATIO, y=NUM_PROJECTS)) +
    geom_point(shape=1) +
    ggtitle("High Quality Clusters") +
    xlab("Ratio") +
    ylab("NUmber of Projects")

grid.arrange(spectra_projects, spectra_projects_high, ratio_projects, ratio_projects_high, ncol=2)

# plot the species
species <- ggplot(cluster_results, aes(x=NUM_SPECIES)) +
    geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("All Clusters") +
    xlab("Number Of Species") +
    ylab("Density")

species_high <- ggplot(high_quality_cluster_results, aes(x=NUM_SPECIES)) +
    geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("High Quality Clusters") +
    xlab("Number Of Species") +
    ylab("Density")

grid.arrange(species_plot, species_plot_high, ncol=2)




