##### Land Use Distances and Density Graphs #########

GIS_Info <- read.csv(file = "Data/SiteDistances_Edited.csv", stringsAsFactors = FALSE)
GIS_Info_NoOutliers <- read.csv(file = "Data/LandUse_NoOutliers.csv", stringsAsFactors = FALSE)

plot(GIS_Info$Pasture, GIS_Info$CPIC_Density)
abline(lm(GIS_Info$CPIC_Density ~ GIS_Info$Pasture, col = "red"))

plot(GIS_Info_NoOutliers$Pasture, GIS_Info_NoOutliers$CPIC_Density)
abline(lm(GIS_Info_NoOutliers$CPIC_Density ~ GIS_Info_NoOutliers$Pasture, col = "red"))

plot(GIS_Info$Development, GIS_Info$CPIC_Density)
abline(lm(GIS_Info$CPIC_Density ~ GIS_Info$Development, col = "red"))

plot(GIS_Info_NoOutliers$Development, GIS_Info_NoOutliers$CPIC_Density)
abline(lm(GIS_Info_NoOutliers$CPIC_Density ~ GIS_Info_NoOutliers$Development, col = "red"))


plot(GIS_Info$Cultivated.Crops, GIS_Info$CPIC_Density)
abline(lm(GIS_Info$CPIC_Density ~ GIS_Info$Cultivated.Crops, col = "red"))


plot(GIS_Info$Pasture, GIS_Info$CPIC_Density)
abline(lm(GIS_Info$CPIC_Density ~ GIS_Info$Pasture, col = "red"))


plot(GIS_Info$Potomac, GIS_Info$CPIC_Density)
abline(lm(GIS_Info$CPIC_Density ~ GIS_Info$Potomac, col = "red"))
remove.outliers
