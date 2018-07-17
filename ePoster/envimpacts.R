

# datasource: http://www.lcafood2014.org/papers/132.pdf
type=c("Beef","Sheep","Pork","Poultry","Fish","Eggs","Milk","Meat substitutes","Pulses","Cultured meat")
# Global warming potential (kg CO2-eq/kg protein)
ghg.emiss= c(6.85,6.2,1.7,1.05,2.45,1.4,1.35,1.1,0.35,0.75)*200/6.5
# Land use (m²/kg protein)
land.use=c(4,4,1.8,0.975,0.725,1.25,0.35,0.6,1.025,0.35)*180/4.375


library(rhtmlLabeledScatter)
df <- data.frame(ghg.emiss, land.use)
rownames(df) <- c("Beef","Sheep","Pork","Poultry","Fish","Eggs","Milk","Meat substitutes","Pulses","Cultured meat")
scatt.col <- c(rgb(0, 176, 80, max = 255), rgb(137, 113, 13, max = 255), rgb(237, 125, 49, max = 255), rgb(255, 0, 0, max = 255))
LabeledScatter(X = df$ghg.emiss,
               Y = df$land.use,
               #colors = scatt.col,
               label = rownames(df),
               #group = bigmac.df[, 3],
               point.radius = 4,
               #title = "Global warming potential -vs- Land use",
               title = "GHG emissions vs. land use of meat and plant based protein sources",
               labels.font.color = rgb(0, 0, 255, max = 255),
               y.title = "Land use (m²/kg protein)",
               x.title = "Global warming potential (kg CO2-eq/kg protein)",
               #x.prefix = "$",
               #y.prefix = "$",
               x.decimals = 0,
               y.decimals = 0,
               origin = FALSE,
               legend.show = FALSE,
               y.bounds.minimum = 0,
               y.bounds.maximum = 200,
               y.bounds.units.major = 50,
               x.bounds.minimum = 0,
               x.bounds.maximum = 225,
               x.bounds.units.major = 50)

#plot(df$ghg.emiss,df$land.use)

# ---------------------

# Agriculture, forestry and other land use
# sector contributions to climate change
# datasource: FAO a-i6340e.pdf

#DISTRIBUTION OF GLOBAL GREENHOUSE GAS (GHG) EMISSIONS BY SECTOR


