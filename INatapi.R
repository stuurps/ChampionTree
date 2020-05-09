I#nstallation
install.packages("remotes")
library(remotes)
install_github("pjhanly/iNatTools")
library(iNatTools)

df <- iNat(lat = 51.450390, lng = -2.606870, radius = 1, quality_grade = "research")
df$species_guess


head(df)
df$species_guess
table(df$quality_grade)


