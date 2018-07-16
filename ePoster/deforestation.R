library(data.table)
library(dplyr)
library(tidyr)
library(countrycode)
library(treemap)
library(RColorBrewer)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data = fread("./data/Emissions_Land_Use_Forest_Land_E_All_Data.csv", encoding="Latin-1")

# only forest land area
data = data[Item=="Forest land" & Element=="Area"]

world = data[`Country Code`==5000]
# only countries, not continents/regions
data = data[`Country Code`<5000]

# remove the YxxxxF columns, which contain just "A"
data = data[,-paste("Y",1990:2015,"F",sep="")]

# remove other columns we don't need
data = data[,-c("Country Code", "Item Code", "Element Code", "Item", "Element", "Unit")]

# ----------------------

# join countrycodes
data$ISOCode = countrycode(data$Country,"country.name","iso2c")
# put ISO Code at the beginning
#data = setcolorder(data, c("ISOCode",colnames(data)[colnames(data)!="ISOCode"]))
# Move ISO Code to the front
data = select(data, ISOCode, everything())
# remove NAs
data = data[!is.na(ISOCode)]

# -------------------------

# reshape to long format
longdata = data.table(gather(data, key=Year, value=Value, paste("Y",1990:2015,sep="")))
longdata$Year=as.numeric(substr(longdata$Year,2,5))

# -------------------------

# find rows with NA values
#data[rowSums(is.na(data)) > 0,]

# 1990 to be used as reference
deforest = data.table(inner_join(longdata, longdata[Year==1990], by=c("ISOCode","Country")))
# select / rename columns
deforest = data.table(select(deforest, ISOCode, Country, Year=Year.x, Value=Value.x, Value1990=Value.y))
# calcualte the (de)forestation since 1990
deforest$Ratio1990 = deforest$Value/deforest$Value1990-1
deforest$Change1990 = deforest$Value-deforest$Value1990

# plot deforestisation ratio by country
deforest$Ratio1990 = deforest$Ratio1990*-1 #temp change to plot
deforest$Change1990 = deforest$Change1990*-1 #temp change to plot
treemap(as.data.frame(deforest[Year==2015 & Ratio1990>0]),
        index=c("Country"),
        vSize="Change1990",
        vColor="Ratio1990",type="value", #add legend
        title.legend="Deforestation Rate",
        palette="Reds",mapping=c(0, 0.35, 0.7),#range=c(0,0.8)
        title="Deforestation 1990-2015")
deforest$Ratio1990 = deforest$Ratio1990*-1 #temp change to plot
deforest$Change1990 = deforest$Change1990*-1 #temp change to plot

# plot forestisation ratio by country
treemap(as.data.frame(deforest[Year==2015 & Ratio1990>0]),
        index=c("Country"),
        vSize="Ratio1990",
        palette="Greens",
        title="Forestation Ratio 1990 vs. 2015")
treemap(as.data.frame(deforest[Year==2015 & Ratio1990>0]),
        index=c("Country"),
        vSize="Change1990",
        palette="Greens",
        title="Forestation since 1990")

# ---
# Comparison of the world-wide deforestation to the land area of Germany and it's neighbouring countries

# magnitudes forestisation vs. deforestisation
sum(deforest[Year==2015 & Change1990>0]$Change1990, na.rm=T)*10
sum(deforest[Year==2015 & Change1990<0]$Change1990, na.rm=T)*10
# total sum
sum(deforest[Year==2015]$Change1990, na.rm=T)*10
# world-wide sum differs
(world$Y2015-world$Y1990)*10
# due to missing values?!
deforest[Year==2015 & is.na(Change1990)]
deforest[Year==1992 & is.na(Change1990)]

landarea = fread("./data/world-bank_land-area/API_AG.LND.TOTL.K2_DS2_en_csv_v2_9985640.csv",header=T)
landarea = landarea %>% filter(`Country Name` %in% c("Germany","Netherlands","Poland","Austria","Denmark","Belgium","Switzerland","Czech Republic","France")) %>% select(c("Country Name", "2015"))
landarea %>% summarise(val=sum(`2015`))

comp = bind_rows(
  data.table(Group="Land Area",
             Name=landarea$`Country Name`,
             sq.km=landarea$`2015`),
  data.table(Group="Deforested Area",
             Name=c("Worldwide"),
             sq.km=(world$Y2015-world$Y1990)*-10)
  )
comp=comp[order(rank(sq.km))]

ggplot(data = comp, aes(x=Group, y=sq.km, group = Group)) + theme_bw() +
  geom_col(aes(fill = Name), colour="black", size=.3) +
  #scale_fill_brewer(palette="Set3") +
  scale_fill_grey(start=1,end=1) +
  geom_text(aes(label = Name), size=3.5, position = position_stack(vjust = 0.5))+ 
  theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10)) +
  ggtitle("Deforested vs. Land Area (km²)") +
  scale_y_continuous(breaks=c(500000,1000000,1500000), labels=c("500.000","1.000.000","1.500.000"))
  

