## read worldcities
geodat <- as.data.frame(scan(what=list(city="a",dirlat="a", latdeg=0,latmin=0,latsec=0,
                                       dirlon="a",londeg=0,lonmin=0,lonsec=0)))
Munich N 48 8 14	E 11 34 31
Hamburg N 53 34 31	E 10 0 55
Cologne N 50 56 0	E 6 57 0
Vienna N 48 12 30	E 16 22 19
Lyons N 45 44 54	E 4 50 48
Paris N 48 51 12	E 2 20 55
Marseilles N 43 17 49	E 5 22 51
Calais N 50 57 7	E 1 51 22
Cherbourg N 49 38 23	W 1 36 58
Athens N 37 58 46	E 23 42 58
Barcelona N 41 23 19	E 2 9 32
Gibraltar N 36 8 41	W 5 21 9
Madrid N 40 24 59	W 3 42 9
Lisbon N 38 43 0	W 9 8 0
Rome N 41 53 30	E 12 30 40
Milan N 45 27 51	E 9 11 22
Geneva N 46 12 8	E 6 8 44
"Hook of Holland" N 51 58 39	E 4 8 0
Brussels N 50 51 1	E 4 20 55
Stockholm N 59 19 57	E 18 3 53
Copenhagen N 55 40 33	E 12 33 55


## data obtained from http://www.geonames.org (copy/paste)

geodat$lat <- geodat$latdeg+(60*geodat$latmin+geodat$latsec)/3600
geodat$lon <- geodat$londeg+(60*geodat$lonmin+geodat$lonsec)/3600
geodat$lon[geodat$dirlon=="W"] <- -geodat$lon[geodat$dirlon=="W"]
geodat <- geodat[order(as.character(geodat$city)),]
plot(geodat$lon, geodat$lat, asp=1, cex=0)
text(geodat$lon, geodat$lat, label=geodat$city, cex=0.8)

save(geodat, file="GeoEurocities.RData")
