# FRYI script to re-format new yield data (2017-2018) and merge with old ones until 2016
#
# @author B. Schauberger (schauber@pik-potsdam.de)
# @version Mar 2020


rm(list = ls())
options(width = 120)
options(warn = 2)

library(stringr)
library(tidyr)

# STEPS:
# for each crop: 
#   read old data until 2016 (well formatted already)
#   read new data until 2018
#   merge data and write to new file

root_path <- "/home/schauber/research/French_yields/"


source(sprintf("%s/codes/common_definitions.R", root_path))

data_path_2016 <- sprintf("%sFrenchCropProduction_by_H_and_T_Kato_06302017/", root_path)

# read data 2016-2018
data_2018_fn <- sprintf("%sFrench_crops_2016-2018_mod_transposed.csv", root_path)
data_2018 <- read.table(data_2018_fn, sep = ";", header = TRUE, stringsAsFactors = FALSE)
# Variable;wheat_total;wheat_winter;wheat_spring;wheat_durum;barley_total;barley_winter;barley_spring;oats_total;oats_winter;oats_spring;maize_total;rape_total;rape_winter;rape_spring;sunflower_total;sugarbeet_total;potatoes_total
# Area/France/77 - Seine-et-Marne/2016;139 995;139 740;255;940;50 520;30 080;20 440;2 000;1 190;810;23 760;44 300;44 275;25;875;29 855;2 050

# extract area|yield, department, year and values and standardize
data_2018$var2 <- substring(data_2018$Variable, first = 1, last = 5)
data_2018$var2[data_2018$var2 == "Area/"] <- "area"
data_2018$var2[data_2018$var2 == "Yield"] <- "yield"
year_start <- stringr::str_length(data_2018$Variable) - 3
data_2018$year <- as.numeric(substring(data_2018$Variable, first = year_start))

departments <- stringr::str_trim(substring(data_2018$Variable, first = 18, last = year_start - 2), side = "both")
departments <- gsub(x = departments, pattern = "-", replacement = "_", fixed = TRUE)
departments <- gsub(x = departments, pattern = "'", replacement = "_", fixed = TRUE)
departments <- gsub(x = departments, pattern = " ", replacement = "_", fixed = TRUE)
departments <- gsub(x = departments, pattern = "(", replacement = "", fixed = TRUE)
departments <- gsub(x = departments, pattern = ")", replacement = "", fixed = TRUE)
departments <- gsub(x = departments, pattern = "+", replacement = "_", fixed = TRUE)
departments[departments == "Hautes_Pyrenees"] <- "Hautes_pyrenees"          # special case
data_2018$department <- departments


for (crop in all_crops) {
    
    season_types <- if (crop %in% winter_spring_crops) c("winter", "spring", "total") else "total"
    
    for (season_type in season_types) {
        cat(sprintf("***** CROP = %s, SEASON = %s *****\n", crop, season_type))
        
        full_crop_name <- sprintf("%s_%s", crop, season_type)
        if (! (full_crop_name %in% names(data_2018))) next       # crop with no updated data
        
        # read data until 2016
        data_fn_1900 <- sprintf("%sDepartmental/%s/%s_%s_data_1900-2016_RAW.txt", data_path_2016, crop, crop, season_type)
        data_1900 <- read.table(data_fn_1900, sep = ";", header = TRUE, stringsAsFactors = FALSE)
        # "department";"year";"yield";"area";"production"
        # "Ain";1900;1.28;3850;4928
        
        # extract crop column for 2016-2018 data
        data_2016 <- data_2018[, c("department", "year", "var2", full_crop_name)]
        names(data_2016) <- c("department", "year", "variable", "value")
        
        # rearrange columns and rows 2017-2018
        data_2016 <- spread(data_2016, key = variable, value = value)
        
        # standardize yields to t/ha (divide by 10)
        data_2016$yield <- data_2016$yield / 10
        
        # calculate production as area * yield 2017-2018
        data_2016$production <- data_2016$area * data_2016$yield
        data_2016 <- data_2016[, c("department", "year", "yield", "area", "production")]
        
        # compare values for 2016; signal differences
        merged2016 <- merge(x = data_1900, y = data_2016, by = c("department", "year"), all = FALSE)
        merged2016$yield_diff <- merged2016$yield.x - merged2016$yield.y
        print(merged2016[abs(merged2016$yield_diff) > 0.05, ])
        # -> it seems that the later data (2016-2018) contains some new and some updated values for 2016, so use the later data set
        
        # merge (remove 2016 from old data) and sort
        data_1900 <- data_1900[data_1900$year < 2016, ]
        merged_data <- rbind(data_1900, data_2016)
        merged_data <- merged_data[with(merged_data, order(department, year)), ]
        
        # store updated files
        result_fn <- sprintf("%sDepartmental/%s/%s_%s_data_1900-2018_RAW.txt", data_path_2016, crop, crop, season_type)
        write.table(merged_data, result_fn, sep = ";", row.names = FALSE)
        
        
        cat("\n\n\n")
    } # season types
} # crops

print("DONE. Now run 'filter_outliers.R'.")




print("DONE.")

