# read in the PIKE data, MIKE centroid data, carcass summary table, etc.
# make list by MIKEsiteID of which site is in which data source.

#----------------------------------------------
# Get the MIKE centroids and other details.
mike.centers <- readxl::read_excel(file.path("..","Data","2021-10-20_mike_sites_list_UpdatedTo2021.xlsx"),
                                   sheet="mike_sites_lis")

mike.centers <- plyr::rename(mike.centers, c("siteid"="MIKEsiteID",
                                             "name"="MIKEsiteName",
                                             "un_region"="UNRegion",
                                             "subregion"="SubregionName"))
data.source <- data.frame(MIKEsiteID=unique(mike.centers$MIKEsiteID[ mike.centers$UNRegion=="Africa"]), GIS=TRUE, stringsAsFactors=FALSE)

# 1. Set end year (start year is always 2003)
startyear <- 2003 
endyear   <- 2025


# 2. Change the name of "carcassummarytable_YYYY-mm-dd.csv" file name below and 
#    save the updated version of this R file.

inputfilename <- file.path("..","Data","carcasssummarytable_2026-06-02.csv")
pike <- read.csv(inputfilename, header=TRUE, as.is=TRUE, strip.white=TRUE)


cat("*** input file name: ", inputfilename, " ****\n")
# analysis only from startyear to endyear
pike <- pike[ pike$year >= startyear,]
pike <- pike[ pike$year <= endyear,]


# select Africa data only
UNRegion.select <- "Africa"
cat("\n\nRestricting PIKE to those countries in ", UNRegion.select, "\n")
pike<- pike[ pike$UNRegion == UNRegion.select,]
cat("\n Here is the number sites reported (including zero carcs.) by year in Africa. \n")
xtabs(~year, data=pike)


pike.original <- pike
# exclude site-years with 0 carcasses reported as not useful for the analysis
select <- pike$TotalNumberOfCarcasses == 0
sum(select)
N.pike.site.years.0.carcasses <- sum(select)  # stat used in the main report
N.pike.site.years.with.carcasses <- sum(select == FALSE) # stat used in the main report
pike <- pike[ !select,]


# find out total number of carcasses reported on
temp <- plyr::ddply(pike, "MIKEsiteID", plyr::summarize,
                    TC =sum(TotalNumberOfCarcasses))
cat("Total number of carcasses reported by site:\n")
temp

# MK -- new code 
cat("Analysis from to:",range(pike$year), "\n")


# << MK - create  mike.pop.est for all possible mike sites with PIKE data across 2003 - endyear

## << MK - create a pop est equal to one for all combination of sites and years
## << MK: head(mike.pop.est)
## MIKEsiteID    year population  SubregionName
## 1        AKG 2003         34 Eastern Africa
## 2        AKG 2006         34 Eastern Africa
# << MK - get all unique site-subregion combo
SSCombo <- unite(pike, col = "keyid", "MIKEsiteID", "SubregionName", sep = "_") %>% 
           select(keyid) %>% unique()
# MK make all SSCombo with all possible years and set population to 1
mike.pop.est <- expand.grid(keyid = SSCombo$keyid, year=2003:endyear, population = 1) %>%
                separate(col="keyid", into = c("MIKEsiteID","SubregionName"), sep="_")

cat("All population estimates set to 1:", all(mike.pop.est$population == 1), "\n" )

# get the base map of africa
# Define the bounding box for Africa

bbox_africa <- data.frame( xmin = -15, xmax = 45, ymin = -33, ymax = 20)
filtered_df <- filter(mike.centers, UNRegion == "Africa")

#get world map added 24-Aug-2024
world <- map_data("world")

base.map2 <- ggplot() + geom_point(data = filtered_df, aes(lon, lat), size = 1.0, shape=3, show.legend = FALSE) +
             geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
             coord_quickmap(xlim = c(bbox_africa$xmin, bbox_africa$xmax),
                            ylim = c(bbox_africa$ymin, bbox_africa$ymax)) + theme_minimal()



