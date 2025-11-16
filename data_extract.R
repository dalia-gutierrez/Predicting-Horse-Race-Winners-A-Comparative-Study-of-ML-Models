# Install necessary packages (if not already installed)
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("lubridate")

# Load libraries
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

# Define the main URL for the calendar page
base_url <- "https://www.palermo.com.ar/es/turf/calendario-de-carreras"

# Step 1: Extract Links for Each Race Day
webpage <- tryCatch(read_html(base_url),
                    error = function(e) stop("Error loading the main page:", e$message))

# Extract links for race days
race_day_links <- tryCatch({
  webpage %>%
    html_nodes(".resultado_carrera a") %>%  # Adjust selector as needed
    html_attr("href") #%>%
    #paste0("https://www.palermo.com.ar", .)
}, error = function(e) {
  warning("Error extracting race day links:", e$message)
  return(character(0))
})

# Initialize an empty data frame to store all race data
all_race_data <- data.frame()

# Step 2: Loop Through Each Race Day Link
for (day_url in race_day_links) {
  day_page <- tryCatch(read_html(day_url), 
                       error = function(e) {
                         warning("Error loading race day page:", e$message)
                         return(NULL)
                       })
  
  if (is.null(day_page)) next
  
  # Extract links to individual races within each day
  race_links <- tryCatch({
    day_page %>%
      html_nodes(".tabla_premio a") %>%  # Adjust selector as needed
      html_attr("href") #%>%
      #paste0("https://www.palermo.com.ar", .)
  }, error = function(e) {
    warning("Error extracting race links:", e$message)
    return(character(0))
  })
  
  # Step 3: Loop Through Each Race and Extract Details
  for (race_url in race_links) {
    race_page <- tryCatch(read_html(race_url), 
                          error = function(e) {
                            warning("Error loading race page:", e$message)
                            return(NULL)
                          })
    
    if (is.null(race_page)) next
    
    # Extract race metadata from the first table
    metadata_table <- race_page %>%
      html_nodes(".tabla_ver_caballo") %>%
      .[[1]]
    
    race_date <- metadata_table %>%
      html_node("tr:nth-child(2) .tabla_hora:nth-child(2)") %>%
      html_text() %>%
      str_trim()
    
    race_hour <- metadata_table %>%
      html_node("tr:nth-child(2) .tabla_hora:nth-child(3)") %>%
      html_text() %>%
      str_trim()
    
    distances <- metadata_table %>%
      html_node("tr:nth-child(2) .tabla_hora:nth-child(4)") %>%
      html_text() %>%
      str_trim() %>%
      as.numeric()
    
    pista_conditions <- metadata_table %>%
      html_node("tr:nth-child(2) .tabla_hora:nth-child(5)") %>%
      html_text() %>%
      str_trim()
    
    # Extract race results from the second table
    race_table <- race_page %>%
      html_nodes(".tabla_ver_caballo") %>%
      .[[2]]
    
    positions <- race_table %>%
      html_nodes("tr td:nth-child(1)") %>%
      html_text() %>%
      str_trim() %>%
      str_replace("^POS\\..\\s*", "") %>%
      .[. != ""] %>%
      as.numeric()
    
    horse_numbers <- race_table %>%
      html_nodes("tr td:nth-child(2)") %>%
      html_text() %>%
      str_trim() %>%
      str_replace("^NRO\\.\\s*", "") %>%
      .[. != ""] %>%
      as.numeric()
    
    horse_names <- race_table %>%
      html_nodes("tr td:nth-child(3)") %>%
      html_text() %>%
      str_trim() %>%
      str_replace("^COMPETIDOR\\s*", "") %>%
      .[. != ""]
    
    jockeys <- race_table %>%
      html_nodes("tr td:nth-child(6)") %>%
      html_text() %>%
      str_trim() %>%
      str_replace("^JOCKEY\\s*", "") %>%
      .[. != ""]
    
    cuidador <- race_table %>%
      html_nodes("tr td:nth-child(7)") %>%
      html_text() %>%
      str_trim() %>%
      str_replace("^CUIDADOR\\s*", "") %>%
      .[. != ""]
    
    caballeriza <- race_table %>%
      html_nodes("tr td:nth-child(8)") %>%
      html_text() %>%
      str_trim() %>%
      str_replace("^CABALLERIZA\\.\\s*", "") %>%
      .[. != ""]
    
    combined_weight <- race_table %>%
      html_nodes("tr td:nth-child(9)") %>%  # Select the second <td> in each <tr> (table row)
      html_text() %>%  # Extract the text content of each cell
      str_trim()
    combined_weight <- str_replace(combined_weight, "^PESO JOCKEY / CABALLO\\s*", "")
    combined_weight <- combined_weight[combined_weight != ""]
    # Split the combined weights into horse and jockey weights
    weight_data <- str_split(combined_weight, "/", simplify = TRUE)
    # Convert the split data into separate vectors
    horse_weights <- weight_data[, 2]  # Second column is horse weights
    jockey_weights <- weight_data[, 1]  # First column is jockey weights
    horse_weights <- as.numeric(horse_weights)
    jockey_weights <- as.numeric(jockey_weights)
    
    # Loop through horse links to gather additional horse details
    horse_links <- race_table %>%
      html_nodes(".tabla_premio a") %>%
      html_attr("href") #%>%
      #paste0("https://www.palermo.com.ar", .)
    
    sex_of_horses <- vector("character", length(horse_links))
    nacimiento <- vector("character", length(horse_links))
    pelaje <- vector("character", length(horse_links))
    father <- vector("character", length(horse_links))
    mother <- vector("character", length(horse_links))
    grandfather_mother <- vector("character", length(horse_links))
    criador <- vector("character", length(horse_links))
    
    for (i in seq_along(horse_links)) {
      horse_page <- tryCatch(read_html(horse_links[i]),
                             error = function(e) {
                               warning("Error loading horse page:", e$message)
                               return(NULL)
                             })
      
      if (is.null(horse_page)) next
      
      target_table <- horse_page %>%
        html_nodes(".tabla_ver_caballo") %>%
        .[[1]]
      
      horse_nacimiento <- target_table %>%
        html_node("tr:nth-child(2) .tabla_hora:nth-child(2)") %>%  
        html_text() %>%
        str_trim() 
      
      horse_sex <- target_table %>%
        html_node("tr:nth-child(2) .tabla_hora:nth-child(3)") %>%
        html_text() %>%
        str_trim()
      
      horse_pelaje <- target_table %>%
        html_node("tr:nth-child(2) .tabla_hora:nth-child(4)") %>%  
        html_text() %>%
        str_trim()       
      
      horse_father <-  target_table %>%
        html_node("tr:nth-child(2) .tabla_hora:nth-child(5)") %>%  
        html_text() %>%
        str_trim()   
      
      horse_mother <-  target_table %>%
        html_node("tr:nth-child(4) .tabla_hora:nth-child(1)") %>%  
        html_text() %>%
        str_trim()
      
      horse_grandfather_mother <-  target_table %>%
        html_node("tr:nth-child(4) .tabla_hora:nth-child(2)") %>%  
        html_text() %>%
        str_trim()
      
      horse_criador <-  target_table %>%
        html_node("tr:nth-child(4) .tabla_hora:nth-child(3)") %>%  
        html_text() %>%
        str_trim()
      
      # Store the retrieved info in the vectors
      nacimiento[i] <- horse_nacimiento
      sex_of_horses[i] <- horse_sex
      pelaje[i] <- horse_pelaje
      father[i] <- horse_father
      mother[i] <- horse_mother
      grandfather_mother[i] <- horse_grandfather_mother
      criador[i] <- horse_criador
    }
    
    # Filter out rows with missing jockeys
    valid_rows <- length(jockeys)#!sapply(jockeys, function(x) is.null(x) || x == "")
    
    # Combine valid rows into a data frame
    race_data <- data.frame(
      Date = rep(race_date, sum(valid_rows)),
      Hour = rep(race_hour, sum(valid_rows)),
      Horse = horse_names[1:valid_rows],
      Jockey = jockeys[1:valid_rows],
      Position = positions[1:valid_rows],
      Distance = rep(distances, sum(valid_rows)),
      Cuidador = cuidador[1:valid_rows],
      Caballeriza = caballeriza[1:valid_rows],
      HorseNumber = horse_numbers[1:valid_rows],
      PistaCondition = rep(pista_conditions, sum(valid_rows)),
      BornDate = nacimiento[1:valid_rows],
      Sex = sex_of_horses[1:valid_rows],
      Pelaje = pelaje[1:valid_rows],
      Father = father[1:valid_rows],
      Mother = mother[1:valid_rows],
      Grandfather = grandfather_mother[1:valid_rows],
      Breeder = criador[1:valid_rows],
      HorseWeight = horse_weights[1:valid_rows],
      JockeyWeight = jockey_weights[1:valid_rows]
    )
    
    # Append this race data to the all_race_data data frame
    all_race_data <- bind_rows(all_race_data, race_data)
  }
}

data2 <- data.frame(lapply(all_race_data, function(x) {
  if (is.character(x)) {
    gsub(",", "", x)
  } else {
    x
  }
}))

# Step 4: Save the data to a CSV file
write.csv(data2, "race_results_final.csv", row.names = FALSE, quote = FALSE)
