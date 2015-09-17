"""########################################################################

	 	Script to generate a map of top crime in each SF neighborhood between 2003 - 2014

Shapefile for neigbhorhood taken from SF Open Data

https://data.sfgov.org/Geographic-Locations-and-Boundaries/SFFind-Neighborhoods/ejmn-jyk6

I prefer this over using Police Districts, zip codes or supervisor districts because it's more
'local'.

 The SFFind Neighborhoods lists 118 SF neighborhoods... can anyone even name all 118?

shoutouts to

###########################################################################"""


#---------------------------------------------------------------------------------
#																				  
#	 Libraries, etc									  
#																				  
#---------------------------------------------------------------------------------


# load tools
library(sp)
library(gpclib)
library(maptools)
library(ggplot2)
library(ggmap)
library(rgdal)
library(dplyr)
library(grid)

gpclibPermit()

#---------------------------------------------------------------------------------
#																				  
#	LOAD MAPS, CRIME DATA SET, SHAPEFILE
#																				 
#---------------------------------------------------------------------------------

# 1) CRIME DATA FRAME 
train <-read.csv("train.csv", stringsAsFactors = FALSE)

alldata <- data.frame(train)

# Get rid of 2015 data  & 67  points where Y == 90 
alldata <- subset(alldata, year != 2015 & Y < 40)


# 2) Get Map 
SFsourcemap <- get_map("san francisco", zoom = 12, source = "google", maptype = "road")

# 3) SHAPE FILE, local folder
sffindneighborhoods <- readOGR("/SF Raw Open Data/sffind_neighborhoods", "SFFind_Neighborhoods")

sp_findneighbor <- spTransform(sffindneighborhoods, CRS("+proj=longlat +datum=WGS84"))



#---------------------------------------------------------------------------------
#																				  
#		APPEND CRIME DATAFRAME TO LIST THE NEIGHBORHOODS FOR EACH CRIME
#									 											  
#---------------------------------------------------------------------------------

coordinates(alldata) <- c("X", "Y")

proj4string(alldata) <- CRS(proj4string(sp_findneighbor))

# Heart of this section, overlays all crime coordinates over the corresponding neighborhood
joinerlist <- over(alldata, sp_findneighbor)  

# Convert alldata back to data frame so that zip code can be merged
alldata <- as.data.frame(alldata)

# Create new column where each crime lists the neighborhood
alldata$findneighborhood <- joinerlist$name



#---------------------------------------------------------------------------------
#																				  
#		DETERMINE TOP CRIME/NEIGHBORHOOD 								  
#																				 
#---------------------------------------------------------------------------------

topcrime_neighborhood <- alldata %>% 
					group_by(findneighborhood, Category) %>%
					tally %>% 
					top_n(n = 1) %>% 
					arrange(n) %>% 
					as.data.frame 

topcrime_neighborhood


#---------------------------------------------------------------------------------
#																				  
#		PROCESS SHAPEFILE & TOP CRIME INTO A DATA FRAME SO THAT IT CAN BE MAPPED											  
#																				 
#---------------------------------------------------------------------------------

sfn <- spTransform(sp_findneighbor, CRS("+proj=longlat +datum=WGS84"))

sfn.f <- sfn %>% fortify(region = "name")

dfsfneighborhoods <- merge(sfn.f, sfn@data, by.x = 'id', by.y = "name")

dfcrime_sfneighborhood <- merge(dfsfneighborhoods, topcrime_neighborhood, 
								by.x = 'id', by.y = 'findneighborhood')



#---------------------------------------------------------------------------------
#																				  
#			MAP - Top Crime by neighborhood											  
#																				 
#---------------------------------------------------------------------------------

mapbaselayer <- ggmap(SFsourcemap, darken = 0, extent = "device")

title <- ggtitle("Top Crime by SF Neighborhood\n2003 - 2014") 

maptopcrime_neigbhorhood <- mapbaselayer +
							geom_polygon(aes(fill = Category, x = long, y = lat, group = group),
								data = dfcrime_sfneighborhood, alpha = I(1), color = "black", size = 0.2) +
							title 

maptopcrime_neigbhorhood

ggsave("maptopcrime_neigbhorhood.png")
