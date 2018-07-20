

# datasource: http://www.lcafood2014.org/papers/132.pdf
type=c("Beef","Sheep","Pork","Poultry","Fish","Eggs","Milk","Meat substitutes","Pulses","Cultured meat")
# Global warming potential (kg CO2-eq/kg protein)
ghg.emiss=c(210.76923, 190.76923, 52.30769, 32.30769, 75.38462, 43.07692, 41.53846, 33.84615, 10.76923, 23.07692)
# Land use (m²/kg protein)
land.use=c(164.57143, 164.57143, 74.05714, 40.11429, 29.82857, 51.42857, 14.40000, 24.68571, 42.17143, 14.40000)
df <- data.frame(ghg.emiss, land.use)
rownames(df) <- c("Beef","Sheep","Pork","Poultry","Fish","Eggs","Milk","Meat substitutes","Pulses","Cultured meat")

library(ggplot2)
ggplot(df, aes(x=df$ghg.emiss, y=df$land.use)) +
  geom_point() + 
  geom_text(label=rownames(df), nudge_x = 0, nudge_y = 5, check_overlap = F)

library(rhtmlLabeledScatter)
scatt.col <- c(rgb(0, 176, 80, max = 255), rgb(137, 113, 13, max = 255), rgb(237, 125, 49, max = 255), rgb(255, 0, 0, max = 255))
LabeledScatter(X = df$ghg.emiss,
               Y = df$land.use,
               #colors = scatt.col,
               label = rownames(df),
               #group = bigmac.df[, 3],
               point.radius = 4,
               #title = "Global warming potential -vs- Land use",
               title = "GHG Emissions vs. Land Use of different Protein Sources",
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
               x.bounds.units.major = 50,
               title.font.size = 20,
               labels.font.size = 14,
               x.title.font.size = 18,
               y.title.font.size = 18,
               axis.font.size = 14)