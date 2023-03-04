library(plotly)
library(dplyr)
library(readr)
library(zoo)


# Set directory

setwd("D:\\Personal\\LinkedinPosts\\Vacunacion\\Data")

# Populate dataset map

states = read_csv("Vacunacion_covid_copia11.csv") %>% 
  select(location, iso_code, date, people_fully_vaccinated, year)

class(states$year)

states$people_fully_vaccinated[]
states$people_fully_vaccinated <- na.locf(states$people_fully_vaccinated, na.rm = FALSE)

states <- states[states$year == 2021,]

# obtain total population and prepare data for the graphs

population_df <- read_csv("Poblacion_por_pais_7.csv") %>%
  select(Country_Code, "2020")
     
Vaccine_df <- left_join(states, population_df, by = c("iso_code" = "Country_Code"))
  
colnames(Vaccine_df)[6] <- "total_population"

Vaccine_df$Porcentaje_Vacunado <- (Vaccine_df$people_fully_vaccinated*100/Vaccine_df$total_population) 

Review <- Vaccine_df[is.na(Vaccine_df$Porcentaje_Vacunado) ,] %>%
  select(location, Porcentaje_Vacunado)

Vaccine_df$Date<- as.character(Vaccine_df$date)

class(Vaccine_df$Date)

colnames(Vaccine_df)[7] <- "% Fully Vaccinated"

# Plot graph

minwage_graph = plot_geo(Vaccine_df, 
                         locationmode = "ISO-3",
                         frame = ~Date) %>%
  add_trace(locations = ~iso_code,
            z = ~`% Fully Vaccinated`,
            zmin = 0,
            zmax = 100,
            color = ~`% Fully Vaccinated`,
            colorscale = list(list(0,"#450D54"),list(0.33,"#A0CBE2"), list(0.66,"#7CD250"), list(1,"green"))) %>%
  layout(title = list(text = "Evolution by country of people fully vaccinated in 2021", 
                      font = list(size=30),
                      pad = list(t = 50)),
         font = "Times New Roman",
         margin = 100) %>%
  colorbar(len=1,
           ticksuffix = "%") %>%
  animation_opts(frame = 0.0001, 
                 transition = 0) %>% 
  animation_slider(currentvalue = list(font = list(size = 20, 
                                                   color="black"), 
                                       xanchor = "center",
                                       offset =15))
minwage_graph