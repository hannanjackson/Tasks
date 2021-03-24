setwd("C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Project")
my_data <- read.csv(file = "data_complete.csv")
write.csv (my_data, 'hypothesis.csv')

# Data collected from https://datadryad.org/stash/dataset/doi:10.5061/dryad.6q750
# Data obtained from host_x_symbiont_sporulation tab in the datasheet.
# Used clade number of "Host.Genotype" and "Symbiont.Genotype" for average rate of "Sporulated" per day.