library(rgbif)
library(dplyr)
library(readr)

# List of species (without authors)
species_list <- c(
  "Anemia glareosa", "Anemia hirta", "Anemia warmingii", "Asplenium hostmannii", 
  "Diplazium mattogrossense", "Austroblechnum divergens", "Blechnum asplenioides", 
  "Blechnum lanceola", "Blechnum polypodioides", "Parablechnum regnellianum", 
  "Salpichlaena papyrus", "Hiya nigrescens", "Arachniodes denticulata", 
  "Bolbitis semipinnatifida", "Cyclodium meniscioides", "Elaphoglossum actinotrichum", 
  "Elaphoglossum alpestre", "Elaphoglossum badinii", "Elaphoglossum liaisianum", 
  "Elaphoglossum macahense", "Elaphoglossum mollissimum", "Elaphoglossum pachydermum", 
  "Elaphoglossum paulistanum", "Elaphoglossum pteropus", "Rumohra glandulosissima", 
  "Sticherus holttumii", "Abrodictyum filiforme", "Didymoglossum angustifrons", 
  "Didymoglossum kapplerianum", "Didymoglossum krausii", "Hymenophyllum apiculatum", 
  "Hymenophyllum elegantulum", "Hymenophyllum fucoides", "Hymenophyllum plumosum", 
  "Polyphlebium diaphanum", "Trichomanes arbuscula", "Lindsaea elatior", 
  "Lindsaea guianensis subsp. lanceastrum", "Lindsaea mesarum", 
  "Lindsaea mesoamericana", "Lindsaea parvula", "Lindsaea tenuis", 
  "Phlegmariurus taxifolius", "Danaea excurrens", "Danaea nodosa", 
  "Ophioglossum nudicaule", "Pecluma robusta", "Pleopeltis burchellii", 
  "Pleopeltis furcata", "Stenogrammitis pumila", "Adiantopsis orbignyana", 
  "Adiantum cinnamomeum", "Adiantum decoratum", "Adiantum diogonaum", 
  "Adiantum dolosum", "Adiantum latifolium", "Adiantum lucidum", 
  "Adiantum macrocladum", "Adiantum petiolatum", "Adiantum poeppigianum", 
  "Adiantum scalare", "Adiantum tenerum", "Adiantum tuomistoanum", 
  "Adiantum villosum", "Adiantum windischii", "Adiantum x", 
  "Ananthacorus angustifolius", "Pteris denticulata", "Pterozonium cyclophyllum", 
  "Schizaea elegans", "Schizaea poeppigiana", "Selaginella producta", 
  "Triplophyllum crassifolium", "Goniopteris amazonica", "Goniopteris poiteana", 
  "Pelazoneuron schizotis"
)

# Create folder to save CSVs (if it does not exist)
if(!dir.exists("newrecords_gbif")) dir.create("newrecords_gbif")

# Desired columns (standardization)
desired_cols <- c(
  "species", "decimalLatitude", "decimalLongitude", "year", "basisOfRecord", 
  "institutionCode", "datasetName", "collectionCode", "catalogNumber", 
  "recordedBy", "recordNumber"
)

# Loop to search, clean, and save data
for(sp in species_list){
  cat("Searching occurrences for:", sp, "...\n")
  
  res <- tryCatch({
    occ_search(scientificName = sp,
               country = "BR",
               hasCoordinate = TRUE,
               limit = 10000)$data
  }, error = function(e) {
    message("Error searching ", sp, ": ", e$message)
    return(NULL)
  })
  
  if(is.null(res) || nrow(res) == 0){
    cat("No occurrences with coordinates found for", sp, "\n")
    next
  }
  
  # Ensure all desired columns exist (if not, create with NA)
  for(col in desired_cols){
    if(!col %in% names(res)){
      res[[col]] <- NA
    }
  }
  
  # Select and rename columns
  res_clean <- res %>%
    dplyr::select(
      species,
      Latitude = decimalLatitude,
      Longitude = decimalLongitude,
      year,
      basisOfRecord,
      institutionCode,
      datasetName,
      collectionCode,
      catalogNumber,
      recordedBy,
      recordNumber
    ) %>%
    distinct()
  
  # Save CSV
  file_name <- paste0("newrecords_gbif/", gsub(" ", "_", sp), "_BR.csv")
  write_csv(res_clean, file_name)
  
  cat("Saved:", file_name, "\n")
}

cat("Process finished.\n")
