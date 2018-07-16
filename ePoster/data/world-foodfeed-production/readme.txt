Datasource: https://www.kaggle.com/dorbicycle/world-foodfeed-production

The Food Balance sheet's data was relatively complete. A few countries that do not exist anymore, such as Czechoslovakia, were deleted from the database. Countries which were formed lately such as South Sudan were kept, even though they do not have all full data going back to 1961. In addition, data aggregation for the 7 different continents was available as well, but was not added to the dataset.

Food and feed production by country and food item from 1961 to 2013, including geocoding.

Y1961 - Y2011 are production years that show the amount of food item produced in 1000 tonnes

Note: The CSV file is Latin1-encoded. Read in the file using, e.g., fao = pd.read_csv("../input/fao.csv", encoding='latin1')