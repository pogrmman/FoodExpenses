### Libraries ###
library(dplyr)
library(tidyr)

# Load data
noTaxnoTip <- read.csv("./Data/no_tax_no_tip.csv", skip = 4, header = TRUE)[1:21,]
withTaxTip <- read.csv("./Data/tax_tip_inc.csv", skip = 4, header = TRUE)[1:21,]
popData <- read.csv("./Data/population.csv", header = TRUE)

# Convert all to numeric
noTaxnoTip <- noTaxnoTip %>% mutate_all(function(x) as.numeric(as.character(gsub(",","",x))))
withTaxTip <- withTaxTip %>% mutate_all(function(x) as.numeric(as.character(gsub(",","",x))))

# Convert all from 1988 dollars to 2017 dollars -- $1 in 1988 = $2.09 in 2017
noTaxnoTip <- noTaxnoTip %>% mutate_at(vars(-Year), function(x) x * 2.09)
withTaxTip <- withTaxTip %>% mutate_at(vars(-Year), function(x) x * 2.09)

# Calculate tips and taxes
TaxesTips <- withTaxTip - noTaxnoTip
TaxesTips$Year <- withTaxTip$Year
rm(withTaxTip)

# Seperate into alcohol and food away from and at home
alcoholAtHome <- noTaxnoTip %>% 
  select(Year, 
         "Liquor Stores" = Liquor.stores, 
         "Food Stores" = Food.stores, 
         Other = Other.AAH.sales..NEC, 
         Total = Total.AAH) %>% gather("Location", "Sales", 2:5)
alcoholAwayFromHome <- noTaxnoTip %>%
  select(Year, 
         "Eating/Drinking Places" = Eating.and.drinking.places, 
         "Hotels/Motels" = Hotels.and.motels.1, 
         Other = Other.AAFH..NEC, 
         Total = Total.AAFH) %>% gather("Location", "Sales", 2:5)
foodAtHome <- noTaxnoTip %>% 
  select(Year, 
         "Grocery Stores" = Grocery.stores, 
         "Convenience Stores" = Convenience.stores, 
         "Other Food Stores" = Other.food.stores, 
         "Warehouse Clubs/Supercenters" = Warehouse.clubs.and.supercenters,
         "Mass Merchandisers" = Mass.merchandisers, 
         "Other Stores and Food Services" = Other.stores.and.foodservice,
         "Mail Order/Home Delivery" = Mail.order.and.home.delivery, 
         "Direct Sales from Producers" = Direct.selling.by.farmers..manufacturers.and.wholesalers, 
         "Home Production/Donations" = Home.production.and.donations,
         Total = Total.FAH) %>% gather("Location", "Sales", 2:11)
foodAwayFromHome <- noTaxnoTip %>%
  select(Year, 
         "Full Service Restaurants" = Full.service.restaurants, 
         "Limited Service Restaurants" = Limited.service.restaurants, 
         "Drinking Establishments" = Drinking.places, 
         "Hotels/Motels" = Hotels.and.motels, 
         "Retail Stores/Vending" = Retail.stores.and.vending, 
         "Recreational Places" = Recreational.places, 
         "Schools/Colleges" = Schools.and.colleges, 
         Other = Other.FAFH.sales..NEC, 
         "Food Furnished/Donated" = Food.furnished.and.donated, 
         Total = Total.FAFH) %>% gather("Location", "Sales", 2:11)

# Add taxes and tip info
alcoholAtHomeTaxesTips <- TaxesTips %>% 
  select(Year, 
         "Liquor Stores" = Liquor.stores, 
         "Food Stores" = Food.stores, 
         Other = Other.AAH.sales..NEC, 
         Total = Total.AAH) %>% gather("Location", "Tips.Taxes", 2:5)
alcoholAwayFromHomeTaxesTips <- TaxesTips %>%
  select(Year, 
         "Eating/Drinking Places" = Eating.and.drinking.places, 
         "Hotels/Motels" = Hotels.and.motels.1, 
         Other = Other.AAFH..NEC, 
         Total = Total.AAFH) %>% gather("Location", "Tips.Taxes", 2:5)
foodAtHomeTaxesTips <- TaxesTips %>% 
  select(Year, 
         "Grocery Stores" = Grocery.stores, 
         "Convenience Stores" = Convenience.stores, 
         "Other Food Stores" = Other.food.stores, 
         "Warehouse Clubs/Supercenters" = Warehouse.clubs.and.supercenters,
         "Mass Merchandisers" = Mass.merchandisers, 
         "Other Stores and Food Services" = Other.stores.and.foodservice,
         "Mail Order/Home Delivery" = Mail.order.and.home.delivery, 
         "Direct Sales from Producers" = Direct.selling.by.farmers..manufacturers.and.wholesalers, 
         "Home Production/Donations" = Home.production.and.donations,
         Total = Total.FAH) %>% gather("Location", "Tips.Taxes", 2:11)
foodAwayFromHomeTaxesTips <- TaxesTips %>%
  select(Year, 
         "Full Service Restaurants" = Full.service.restaurants, 
         "Limited Service Restaurants" = Limited.service.restaurants, 
         "Drinking Establishments" = Drinking.places, 
         "Hotels/Motels" = Hotels.and.motels, 
         "Retail Stores/Vending" = Retail.stores.and.vending, 
         "Recreational Places" = Recreational.places, 
         "Schools/Colleges" = Schools.and.colleges, 
         Other = Other.FAFH.sales..NEC, 
         "Food Furnished/Donated" = Food.furnished.and.donated, 
         Total = Total.FAFH) %>% gather("Location", "Tips.Taxes", 2:11)

foodAtHome <- foodAtHome %>% left_join(foodAtHomeTaxesTips, by=c("Year", "Location"))
foodAwayFromHome <- foodAwayFromHome %>% left_join(foodAwayFromHomeTaxesTips, 
                                                   by=c("Year", "Location"))
alcoholAtHome <- alcoholAtHome %>% left_join(alcoholAtHomeTaxesTips, by=c("Year", "Location"))
alcoholAwayFromHome <- alcoholAwayFromHome %>% left_join(alcoholAwayFromHomeTaxesTips,
                                                         by=c("Year", "Location"))

rm(foodAtHomeTaxesTips, foodAwayFromHomeTaxesTips, 
   alcoholAtHomeTaxesTips, alcoholAwayFromHomeTaxesTips)

# Sum taxes and tips
alcoholAtHome <- alcoholAtHome %>% mutate(SalesWithTaxTip = Sales + Tips.Taxes)
alcoholAwayFromHome <- alcoholAwayFromHome %>% mutate(SalesWithTaxTip = Sales + Tips.Taxes)
foodAtHome  <- foodAtHome %>% mutate(SalesWithTaxTip = Sales + Tips.Taxes)
foodAwayFromHome <- foodAwayFromHome %>% mutate(SalesWithTaxTip = Sales + Tips.Taxes)

# Get total food expenditures
totals <- alcoholAtHome %>% bind_rows(alcoholAwayFromHome) %>% 
  bind_rows(foodAtHome) %>% bind_rows(foodAwayFromHome) %>% filter(Location == "Total") %>%
  group_by(Year) %>% summarize(TotalNoTipsTaxes = sum(Sales),
                               TotalTipsTaxes = sum(Tips.Taxes),
                               Total = sum(SalesWithTaxTip))

# Calculate percentages
alcoholAtHome <- alcoholAtHome %>% left_join(totals, by = "Year") %>%
  mutate(PercentSales = Sales * 100 / TotalNoTipsTaxes,
         PercentTipsTaxes = Tips.Taxes * 100 / TotalTipsTaxes,
         PercentTotal = SalesWithTaxTip * 100 / Total) %>% select(-TotalNoTipsTaxes,
                                                                  -TotalTipsTaxes,
                                                                  -Total)
alcoholAwayFromHome <- alcoholAwayFromHome %>% left_join(totals, by = "Year") %>%
  mutate(PercentSales = Sales * 100 / TotalNoTipsTaxes,
         PercentTipsTaxes = Tips.Taxes * 100 / TotalTipsTaxes,
         PercentTotal = SalesWithTaxTip * 100 / Total) %>% select(-TotalNoTipsTaxes,
                                                                  -TotalTipsTaxes,
                                                                  -Total)
foodAtHome <- foodAtHome %>% left_join(totals, by = "Year") %>%
  mutate(PercentSales = Sales * 100 / TotalNoTipsTaxes,
         PercentTipsTaxes = Tips.Taxes * 100 / TotalTipsTaxes,
         PercentTotal = SalesWithTaxTip * 100 / Total) %>% select(-TotalNoTipsTaxes,
                                                                  -TotalTipsTaxes,
                                                                  -Total)
foodAwayFromHome <- foodAwayFromHome %>% left_join(totals, by = "Year") %>%
  mutate(PercentSales = Sales * 100 / TotalNoTipsTaxes,
         PercentTipsTaxes = Tips.Taxes * 100 / TotalTipsTaxes,
         PercentTotal = SalesWithTaxTip * 100 / Total) %>% select(-TotalNoTipsTaxes,
                                                                  -TotalTipsTaxes,
                                                                  -Total)

# Calculate per capita cost
alcoholAtHome <- alcoholAtHome %>% left_join(popData, by = "Year") %>%
  mutate(PerCapitaSales = Sales * 1000000 / POP,
         PerCapitaTipsTaxes = Tips.Taxes * 1000000 / POP,
         PerCapitaTotal = SalesWithTaxTip * 1000000 / POP) %>% select(-POP)
alcoholAwayFromHome <- alcoholAwayFromHome %>% left_join(popData, by = "Year") %>%
  mutate(PerCapitaSales = Sales * 1000000 / POP,
         PerCapitaTipsTaxes = Tips.Taxes * 1000000 / POP,
         PerCapitaTotal = SalesWithTaxTip * 1000000 / POP) %>% select(-POP)
foodAtHome <- foodAtHome %>% left_join(popData, by = "Year") %>%
  mutate(PerCapitaSales = Sales * 1000000 / POP,
         PerCapitaTipsTaxes = Tips.Taxes * 1000000 / POP,
         PerCapitaTotal = SalesWithTaxTip * 1000000 / POP) %>% select(-POP)
foodAwayFromHome <- foodAwayFromHome %>% left_join(popData, by = "Year") %>%
  mutate(PerCapitaSales = Sales * 1000000 / POP,
         PerCapitaTipsTaxes = Tips.Taxes * 1000000 / POP,
         PerCapitaTotal = SalesWithTaxTip * 1000000 / POP) %>% select(-POP)

# Get list of locations in each type
foodAtHomeChoices <- foodAtHome %>% select(Location) %>% 
  distinct() %>% filter(Location != "Total")
foodAwayFromHomeChoices <- foodAwayFromHome %>% select(Location) %>% 
  distinct() %>% filter(Location != "Total")
alcoholAtHomeChoices <- alcoholAtHome %>% select(Location) %>%
  distinct() %>% filter(Location != "Total")
alcoholAwayFromHomeChoices <- alcoholAwayFromHome %>% select(Location) %>%
  distinct() %>% filter(Location != "Total")

# Make both away and at home versions
bothAlcohol <- alcoholAwayFromHome %>% bind_rows(alcoholAtHome)
totals <- bothAlcohol %>% filter(Location == "Total") %>% group_by(Year) %>%
  summarize(Sales = sum(Sales), Tips.Taxes = sum(Tips.Taxes), 
            SalesWithTaxTip = sum(SalesWithTaxTip), PercentSales = sum(PercentSales),
            PercentTipsTaxes = sum(PercentTipsTaxes), PercentTotal = sum(PercentTotal),
            PerCapitaTipsTaxes = sum(PerCapitaTipsTaxes), PerCapitaTotal = sum(PerCapitaTotal),
            PerCapitaSales = sum(PerCapitaSales))
totals$Location = "Total"
bothAlcohol <- bothAlcohol %>% filter(Location != "Total") %>% bind_rows(totals)
totals <- bothAlcohol %>% filter(Location == "Other") %>% group_by(Year) %>%
  summarize(Sales = sum(Sales), Tips.Taxes = sum(Tips.Taxes), 
            SalesWithTaxTip = sum(SalesWithTaxTip), PercentSales = sum(PercentSales),
            PercentTipsTaxes = sum(PercentTipsTaxes), PercentTotal = sum(PercentTotal),
            PerCapitaTipsTaxes = sum(PerCapitaTipsTaxes), PerCapitaTotal = sum(PerCapitaTotal),
            PerCapitaSales = sum(PerCapitaSales))
totals$Location = "Other"
bothAlcohol <- bothAlcohol %>% filter(Location != "Other") %>% bind_rows(totals)

bothFood <- foodAwayFromHome %>% bind_rows(foodAtHome)
totals <- bothFood %>% filter(Location == "Total") %>% group_by(Year) %>%
  summarize(Sales = sum(Sales), Tips.Taxes = sum(Tips.Taxes), 
            SalesWithTaxTip = sum(SalesWithTaxTip), PercentSales = sum(PercentSales),
            PercentTipsTaxes = sum(PercentTipsTaxes), PercentTotal = sum(PercentTotal),
            PerCapitaTipsTaxes = sum(PerCapitaTipsTaxes), PerCapitaTotal = sum(PerCapitaTotal),
            PerCapitaSales = sum(PerCapitaSales))
totals$Location = "Total"
bothFood <- bothFood %>% filter(Location != "Total") %>% bind_rows(totals)

bothAlcoholChoices <- alcoholAtHomeChoices %>% bind_rows(alcoholAwayFromHomeChoices)
bothFoodChoices <- foodAtHomeChoices %>% bind_rows(foodAwayFromHomeChoices)