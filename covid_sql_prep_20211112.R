library(tidyverse)
library(queryparser)
library(tidyquery)

# My goal here is to split the data into more manageable chunks
# One being deaths and the other being vaccinations

colnames(covid_raw)
# population is col 47, want it as 5 between date and total_cases
covid_raw_pop <- covid_raw %>% 
  relocate(population, .before = total_cases)
colnames(covid_raw_pop)

# Splitting at 27 new_tests, 
covid_deaths <- covid_raw_pop[-c(27:65)]
# Using the other half, keeping 1:4
covid_vaccinations <- covid_raw_pop[-c(5:26)]

rm(covid_vaccination)

# Moving them over to SQL to work on them there
write.csv(covid_deaths, 
          "C:\\Users\\15037\\Desktop\\DA\\covid_project\\covid_deaths.csv",
          row.names = FALSE)

write.csv(covid_vaccinations, 
          "C:\\Users\\15037\\Desktop\\DA\\covid_project\\covid_vaccinations.csv",
          row.names = FALSE)
# Both exported without issue, still having trouble importing them to MySQL

# Still having issues with importing with the new format
# Might see if I can use MicrosoftSQL

# learning that dplyr ~ SQL

show_dplyr('SELECT total_deaths, location FROM covid_deaths WHERE date = "2021-10-22"')

covid_deaths %>%
  filter(date == "2021-10-22") %>%
  select(total_deaths, location) %>% 
  arrange(desc(total_deaths))




# Starting to follow along with the video in R instead of SQL, fingers crossed

# ** Data Exploration

output0 <- query('SELECT * FROM covid_deaths ORDER BY location, date')
view(output0)

output1 <- query('SELECT * FROM covid_vaccinations ORDER BY location, date')
view(output1)

# Select Data that we are going to be using

output2 <- query('SELECT location, date, total_cases, new_cases, total_deaths, population
                  FROM covid_deaths 
                  ORDER BY location, date')
view(output2)

# Looking at total cases vs total deaths
# Shows the likelihood of dying if you contract Covid in your country
output3 <- query('SELECT location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as death_percentage
                  FROM covid_deaths  
                  WHERE location = "United States"
                  ORDER BY location, date')
view(output3)

## Looking at the total cases vs population
## Shows what percentage of the population got Covid
##    Will comment out US in future for visualizations
output4 <- query('SELECT location, date, total_cases, population, (total_cases/population)*100 as infection_percentage
                  FROM covid_deaths  
                  WHERE location = "United States"
                  ORDER BY location, date')
view(output4)

## Looking at countries with highest infection rate compated to population
output5 <- query('SELECT location, MAX(total_cases) as highest_infection_count, population, MAX((total_cases/population))*100 as percent_pop_infected
                  FROM covid_deaths 
                  GROUP BY population, location
                  ORDER BY percent_pop_infected desc')
view(output5)

## Showing countries with highest death count per population
## Issue with data type in vid,   'MAX(cast(total_deaths as int)) as...'
output6 <- query('SELECT location, MAX(total_deaths) as total_death_count
                  FROM covid_deaths
                  WHERE iso_code not like "%OWID_%"
                  GROUP BY location
                  ORDER BY total_death_count desc')
view(output6)

## Dropping continent grouped data
## can add 'WHERE iso_code not like "%OWID_%"' 
output7 <- query('SELECT *
                  FROM covid_deaths 
                  WHERE iso_code not like "%OWID_%"')
view(output7)

## BREAKING THINGS DOWN BY CONTRINENT
  ## note that AND in WHERE needs to be on its own line
output8 <- query('SELECT continent, MAX(total_deaths) as total_death_count
                  FROM covid_deaths
                  WHERE iso_code not like "%OWID_%"
                  AND total_deaths >= 1 
                  GROUP BY continent
                  ORDER BY total_death_count desc')
view(output8)
  ## Only shows MAX and not SUM for each continent

## Using location and finding the correct numbers for continent totals
## Showing the contients with the highest death counts
output9 <- query('SELECT location, MAX(total_deaths) as total_death_count
                  FROM covid_deaths
                  WHERE iso_code like "%OWID_%"
                  AND total_deaths >= 1 
                  GROUP BY location
                  ORDER BY total_death_count desc')
view(output9)

## Breaking out by continent
## Can go back and add in continent to past queries to add in drill down effects
## GLOBAL NUMBERS
## Showing global death percentage for each day (progressive sum)
output10 <- query('SELECT date, SUM(total_cases) as total_cases, SUM(total_deaths) as total_deaths, (SUM(total_deaths)/SUM(total_cases))*100 as death_percentage
                  FROM covid_deaths
                  WHERE iso_code not like "%OWID_%"
                  GROUP BY date
                  ORDER BY date, total_cases')
view(output10)
## Showing global death percentage for each day (new each day)
output11 <- query('SELECT date, SUM(new_cases) as new_daily_cases, SUM(new_deaths) as new_daily_deaths, (SUM(new_deaths)/SUM(new_cases))*100 as death_percentage
                  FROM covid_deaths
                  WHERE iso_code not like "%OWID_%"
                  GROUP BY date
                  ORDER BY date')
view(output11)

## Showing global death percentage total
output12 <- query('SELECT SUM(new_cases) as new_daily_cases, SUM(new_deaths) as new_daily_deaths, (SUM(new_deaths)/SUM(new_cases))*100 as death_percentage
                  FROM covid_deaths
                  WHERE iso_code not like "%OWID_%"')
view(output12)


## MOVING OVER TO THE VACCINES DATA SET

output13 <- query('SELECT *
                  FROM covid_vaccinations')
view(output13)

## How joining is being done in the video, can try here
## SQL join worked, shocked
covid_test <- query('SELECT *
                    FROM covid_deaths dea
                    JOIN covid_vaccinations vac
                      ON dea.location = vac.location
                      AND dea.date = vac.date')
view(covid_test)
colnames(covid_test)
covid_full <- covid_test
view(covid_full)
view(covid_deaths) ## covid_full and covid_death have the same number of entries

## Looking at total population vs vaccinations
output14 <- query('SELECT dea.continent, location, date, population, new_vaccinations
                  FROM covid_full
                  WHERE dea.iso_code not like "%OWID_%"
                  ORDER BY location, date')
view(output14)

## Adding a summative column that adds the vaccinations as time passes
## tidyquery doesn't support OVER clauses, time to test with dplyr
output15 <- query('SELECT dea.continent, location, date, population, new_vaccinations, 
                  SUM(new_vaccinations) OVER (PARTITION BY location) as rolling_people_vaccinated
                  FROM covid_full
                  WHERE dea.iso_code not like "%OWID_%"
                  ORDER BY location, date')
view(output15)

## Matched using dplyr
output14t <- output14

output14t[is.na(output14t)] <- 0

output14tf <- output14t %>% 
  select(dea.continent, location, date, population, new_vaccinations) %>%
  group_by(location) %>%
  arrange(location, date) %>%
  mutate(rolling_people_vaccinated = cumsum(new_vaccinations)) %>%
  arrange(location, date) %>%
  select(everything())
view(output14tf)

output15 <- output14tf
view(output15)

## Calculated total vaccinations differ from 'true' total vaccinations
  ## 'true' total vac sometimes changes without entered new vaccinations

## Modifications to make work
x <- query('SELECT dea.continent, location, date, population, new_vaccinations, 
                  SUM(new_vaccinations) OVER (PARTITION BY location ORDER BY locations, date)
                    as rolling_people_vaccinated, (rolling_people_vaccinated/population)*100
                  FROM covid_full
                  WHERE dea.iso_code not like "%OWID_%"
                  ORDER BY location, date')

## Making CTE is adding 'With PopvsVac (continent, location, date, population, rolling_people_vaccinated)
                      ## as ( our query )'
output16 <- output15 %>% 
  mutate(percent_population_vaccinated = (rolling_people_vaccinated/population)*100)
view(output16)

## rolling_people_vaccinated is more than total population possibly due to doubled vaccinations?
## checking on full data

test <- covid_full %>% 
  mutate(percent_population_vaccinated = (total_vaccinations/population)*100) %>% 
  select(location, date, population, total_vaccinations, percent_population_vaccinated)
view(test)

## this reports that Gibraltar was over %250 vaccinated
## note if running temp tables, add before making table 'drop table id exists "#table name"'



## CREATING A VIEW to store data for later visualizations
  ## running your query will make a table to view (output16)
  ## not like a temp table, like permanent thing now

## Saving and putting into github


































