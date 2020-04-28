# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(rgdal)

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
          "PollutantName", 
          "MethodBasisName", 
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
          "StreetName",
          "BuildingNumber",
          "City",
          "PostalCode",
          "Lat",
          "Long",
          "NACEMainEconomicActivityCode",
          "NACEMainEconomicActivityName",
          "TotalEmployeeQuantity",
          "WebsiteCommunication")]

# Encode to UTF-8
convRows = c("ParentCompanyName", "FacilityName", "StreetName", "City")
for(convRow in convRows){
  f[[convRow]] = iconv(f[[convRow]], "latin1", "UTF-8")
  
}

# Merge
e = merge(r, f, by = "PollutantReleaseAndTransferReportID")
e = merge(e, p, by = "FacilityReportID")
e = e[ReportingYear >= 2015]

# Transform to WGS 84
tmp = e[CoordinateSystemCode == "EPSG:4258"]
coordinates(tmp) = 12:13
proj4string(tmp) = CRS("+init=epsg:4258") # WGS 84
CRS.new = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
tmp.wgs84 = as.data.table(spTransform(tmp, CRS.new))
tmp.wgs84[, CoordinateSystemCode := "EPSG:4326"]

# Finalize
e = rbind(e[CoordinateSystemCode != "EPSG:4258"], tmp.wgs84)
coordinates(e) = 12:13

# Check encoding
head(Encoding(e@data$ParentCompanyName), 15)
head(e@data$ParentCompanyName, 15)

# Write out
writeOGR(e, file.path(folders$tmp, "e-prtr.geojson"), layer = "ghg", driver = "GeoJSON", check_exists = FALSE)
