## N. Chardon
## Start: 6 Jun 2016
## Aim: preliminary analyses of PALS cushion data

setwd("~/Desktop/Research/pals_cushion/")
library(ggplot2)
library(maptools)
library(maps)

# load xlsx sheets
library(XLConnect)
importWorksheets <- function(filename) { #filename: name of Excel file
    workbook <- loadWorkbook(filename)
    sheet_names <- getSheets(workbook)
    names(sheet_names) <- sheet_names
    sheet_list <- lapply(sheet_names, function(.sheet){
        readWorksheet(object=workbook, .sheet)})
}
pals_data <- importWorksheets('all_sites.xlsx') #55 sheets
site_meta <- readWorksheetFromFile('site_metadata.xlsx', sheet=1)

# combine pals_data: IN PROGRESS
pals <- as.data.frame(pals_data[1:55])
for(i in 1:length(pals_data)) {
    
}

test <- as.data.frame(unlist(pals_data)) #appends split to colnames...

# silene sites (n=23)
silene <- site_meta[which(site_meta$cushion.spp..Genus_species.=='Silene_acaulis'),]

mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
sil_map <- ggplot(silene) +
    mapWorld +
    geom_point(aes(long, lat, color=site.name), size=1) +
    ggtitle('PALS data for Silene acaulis: USA, Canada, Spain, Italy, Switzerland, Slovakia, Norway, Sweden')
ggsave(sil_map, file='world_map.pdf')
