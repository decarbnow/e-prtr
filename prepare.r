# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(rgdal)
library(leaflet)


folders = list()
folders$tmp = "./tmp"
# ----------------------------------------------

# get GRHGAS Pollutant quantity
p = fread(file.path(folders$tmp, 
                    "E-PRTR_database_v18_csv", 
                    "dbo.PUBLISH_POLLUTANTRELEASE.csv"),
          encoding = "Latin-1")

p = p[ReleaseMediumName == "Air" & PollutantGroupCode == "GRHGAS"]

p = p[, c("FacilityReportID", 
          "PollutantCode", 
          #"PollutantName", 
          #"MethodBasisName", 
          "TotalQuantity")]

# get column "ReportingYear"
r = fread(file.path(folders$tmp, 
                    "E-PRTR_database_v18_csv", 
                    "dbo.PUBLISH_POLLUTANTRELEASEANDTRANSFERREPORT.csv"),
          encoding = "Latin-1")

r = r[, c("PollutantReleaseAndTransferReportID", 
          "ReportingYear", 
          "CoordinateSystemCode")]

# get long/lat & other information
f = fread(file.path(folders$tmp, 
                    "E-PRTR_database_v18_csv", 
                    "dbo.PUBLISH_FACILITYREPORT.csv"),
          encoding = "Latin-1")

f = f[, c("FacilityReportID", 
          "PollutantReleaseAndTransferReportID", 
          "FacilityID",
          "ParentCompanyName",
          "FacilityName",
          # "StreetName",
          # "BuildingNumber",
          # "City",
          # "PostalCode",
          "Lat",
          "Long",
          # "NACEMainEconomicActivityCode",
          # "NACEMainEconomicActivityName",
          # "TotalEmployeeQuantity",
          "WebsiteCommunication")]

# Encode to UTF-8
convRows = c("ParentCompanyName", "FacilityName")
for(convRow in convRows){
  f[[convRow]] = iconv(f[[convRow]], "latin1", "UTF-8")
  
}

# Merge
e = merge(r, f, by = "PollutantReleaseAndTransferReportID")
e = merge(e, p, by = "FacilityReportID")



# Transform to WGS 84
tmp = e[CoordinateSystemCode == "EPSG:4258"]
coordinates(tmp) = c(9,8)
proj4string(tmp) = CRS("+init=epsg:4258") # WGS 84
CRS.new = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
tmp.wgs84 = as.data.table(spTransform(tmp, CRS.new))
tmp.wgs84[, CoordinateSystemCode := "EPSG:4326"]

# rbind
e = rbind(e[CoordinateSystemCode != "EPSG:4258"], tmp.wgs84)

e[PollutantCode == "CO2", TotalQuantityCO2 := TotalQuantity]
e[PollutantCode == "CH4", TotalQuantityCO2 := TotalQuantity*25]
e[PollutantCode == "N2O", TotalQuantityCO2 := TotalQuantity*298]
e[PollutantCode == "SF6", TotalQuantityCO2 := TotalQuantity*22800]
e = e[!is.na(TotalQuantityCO2)][, `:=`(TotalQuantity = NULL,
                                       PollutantCode = NULL,
                                       PollutantReleaseAndTransferReportID = NULL)]

e = e[, .(TotalQuantityCO2 = sum(TotalQuantityCO2)), .(FacilityReportID,
                                                   ReportingYear,
                                                   CoordinateSystemCode,
                                                   FacilityID,
                                                   ParentCompanyName,
                                                   FacilityName,
                                                   Lat,
                                                   Long,
                                                   WebsiteCommunication)]

e = e[ReportingYear >= 2015]
e[, maxReportingYear := max(ReportingYear), by = FacilityID]
e = e[maxReportingYear == ReportingYear]
e[, maxReportingYear := NULL]

e = e[order(-TotalQuantityCO2)][1:500]

# show biggest 100 in leaflet
leaflet_map = leaflet(data = e[order(-TotalQuantityCO2)]) %>% addProviderTiles("CartoDB.Positron")

leaflet_map = leaflet_map %>%
  addMarkers(~Long, ~Lat, popup = ~as.character(FacilityName), label = ~as.character(FacilityName))

leaflet_map



# finalize
coordinates(e) = c(8,7)



# Check encoding
head(Encoding(e@data$ParentCompanyName), 15)
head(e@data$ParentCompanyName, 15)




# Write out
writeOGR(e, file.path(folders$tmp, "e-prtr.geojson"), layer = "ghg", driver = "GeoJSON", check_exists = FALSE)
