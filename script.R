library(tidyverse)
library(XML)
library(bitops)
library(RCurl)
library(stringr)
library(ggrepel)
library(scales)

#https://nearplace.com/blog/how-to-generate-google-map-api-key-for-free/

#download.file("https://www.verwaltung.steiermark.at/cms/dokumente/12228622_118256397/c174dcf0/vorl%C3%A4ufiges%20Befragungsergebnis_mitProzent.pdf", "./leitspital.pdf")

spital <- read_csv("./leitspital.csv")

hospital_new <- "Stainach-Pürgg"
# There are three cities with hospitals in them
hospital_old <- spital %>%
  filter(num_hospitals > 0) %>%
  select(municipality)


# GOOGLE API KEY
key = "YOUR_KEY_HERE"


# Functions ---------------------------------------------------------------



#Page where I got the function from
#https://stackoverflow.com/questions/16863018/getting-driving-distance-between-two-points-lat-lon-using-r-and-google-map-ap


# The function calls google maps for the driving duration between two places. 

drive_duration <- function(origin,destination = hospital_new){
  origin <- paste0(str_replace_all(origin," ",""),",Styria,Austria")
  destination <- paste0(str_replace_all(destination," ",""),",Styria,Austria")
  xml.url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false&key=',key)
  xmlfile <- xmlParse(getURL(xml.url))
  dur <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[1]])$value)
  duration <- as.numeric(dur)/60
  return(duration)
}



# Here we call the above function for all three of the old hospitals and return the one with the least driving duration
duration_to_closest_hospital <- function(origin){
  durations <- vector(mode = "numeric", length = nrow(hospital_old))
  for (i in 1:nrow(hospital_old)) {
    durations[i] <- drive_duration(origin, hospital_old[i,1])
  }
  return(min(durations))
}




# Distance Calculation ----------------------------------------------------


spital$duration_new_hosp <- do.call(rbind, lapply(spital$municipality, drive_duration))
spital$duration_old_hosp <- do.call(rbind, lapply(spital$municipality, duration_to_closest_hospital))

spital <- spital %>%
  mutate(duration_diff = duration_new_hosp - duration_old_hosp,
         percent_yes = (yes/valid))



# So artsy ----------------------------------------------------------------



text_formatter <- function(x){
  paste(x,"min")
}


ggplot(data = spital, aes(x = duration_diff, y = percent_yes))+
  geom_point(aes(size = population), fill = "lightblue", colour = "blue", shape = 21, show.legend = F)+
  geom_text_repel(aes(label = municipality)) +
  geom_smooth(method = "lm", linetype = 0, fill = "lightblue") +
  xlab("Differenz der Fahrzeit") +
  ylab("Anteil der Ja-Stimmen") +
  scale_x_continuous(limits = c(-25,40), labels = text_formatter) +
  scale_y_continuous(labels = percent) +
  ggtitle("Wahlverhalten der Gemeinden in Abhängigkeit \n der Fahrzeitänderung zum neuen Leitspital",
          subtitle = "Volksbefragung Leitspital vom 7. April 2019")

hospital_distance


