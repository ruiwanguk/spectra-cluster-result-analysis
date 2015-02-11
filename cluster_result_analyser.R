# read input argument
options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)
print(args)

# read the first argument as the input file
cluster_results <- read.csv2(args[1], sep="\t", na.strings="NA", stringsAsFactors=FALSE, fill=TRUE)

# read the output directory
output_file <- args[2]

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

# define output 
pdf(output_file)

# plot the ratio vs size (min 10 spectra)
ratio_size <- ggplot(cluster_results, aes(x=NUM_SPECTRA, y=MAX_RATIO)) +
    geom_point(shape=1) +
    ggtitle("All Clusters") +
    xlab("Number of Spectra") +
    ylab("Max Ratio")

# plot the ratio distribution in density histogram
ratio_size_density <- ggplot(cluster_results, aes(x=MAX_RATIO)) +
    geom_histogram(aes(y=..density..), binwidth=.01, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("All Clusters") +
    xlab("Max Ratio") +
    ylab("Density")

ratio_size_high <- ggplot(high_quality_cluster_results, aes(x=NUM_SPECTRA, y=MAX_RATIO)) +
    geom_point(shape=1) +
    ggtitle("High Quality Clusters") +
    xlab("Number of Spectra") +
    ylab("Max Ratio")

# plot the ratio distribution in density histogram
ratio_size_density_high <- ggplot(high_quality_cluster_results, aes(x=MAX_RATIO)) +
    geom_histogram(aes(y=..density..), binwidth=.01, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("High Quality Clusters") +
    xlab("Max Ratio") +
    ylab("Density")

grid.arrange(ratio_size, ratio_size_high, ratio_size_density, ratio_size_density_high, ncol=2)


# plot the size distribution
cluster_size <- ggplot(cluster_results, aes(x=NUM_SPECTRA)) +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("All Clusters") +
    xlab("Number Of Spectra") +
    ylab("Density")

cluster_size_box <- ggplot(cluster_results, aes(x=factor(0), y = NUM_SPECTRA)) +
    geom_boxplot() +
    ggtitle("All Clusters") +
    xlab("") +
    ylab("Number Of Spectra")

# plot the size distribution for high quality cluster
cluster_size_high <- ggplot(high_quality_cluster_results, aes(x=NUM_SPECTRA)) +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("High Quality Clusters") +
    xlab("Number Of Spectra") +
    ylab("Density")

cluster_size_box_high <- ggplot(high_quality_cluster_results, aes(x=factor(0), y = NUM_SPECTRA)) +
    geom_boxplot() +
    ggtitle("High Quality Clusters") +
    xlab("") +
    ylab("Number Of Spectra")

grid.arrange(cluster_size, cluster_size_high, cluster_size_box, cluster_size_box_high, ncol=2)

# plot the precursor m/z range
mz_range <- ggplot(cluster_results, aes(x=NUM_SPECTRA, y=PMZ_RANGE)) +
    geom_point(shape=1) +
    ggtitle("All Clusters") +
    xlab("Number of Spectra") +
    ylab("Precursor m/z Range")

# plot the precursor m/z range for high quality cluster
mz_range_high <- ggplot(high_quality_cluster_results, aes(x=NUM_SPECTRA, y=PMZ_RANGE)) +
    geom_point(shape=1) +
    ggtitle("High Quality Clusters") +
    xlab("Number of Spectra") +
    ylab("Precursor m/z Range")

grid.arrange(mz_range, mz_range_high, ncol=2)

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

grid.arrange(spectra_projects, spectra_projects_high, ncol=2)

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

grid.arrange(species, species_high, ncol=2)

# plot peptide and psms
peptide <- ggplot(cluster_results, aes(x=NUM_PEPTIDES)) +
    geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("All Clusters") +
    xlab("Number Of Peptides") +
    ylab("Density")

peptide_high <- ggplot(high_quality_cluster_results, aes(x=NUM_PEPTIDES)) +
    geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
    geom_density(fill="#FF6666", alpha=.3) +
    ggtitle("High Quality Clusters") +
    xlab("Number Of Peptides") +
    ylab("Density")

grid.arrange(peptide, peptide_high, ncol=2)

dev.off()




