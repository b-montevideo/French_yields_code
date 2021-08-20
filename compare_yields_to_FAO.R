# Script to comppare aggregated national yields with FAO data
#
# @author schauber@pik-potsdam.de; bernhard.schauberger@lsce.ipsl.fr
# @date Apr 2021



rm(list=ls())
options(width = 190)
options(warn = 2)


# wd <- getwd()
# if (! is.na(stringr::str_locate(wd, "schauber")[1])) {
#     # start on laptop
#     root_path <- "/home/schauber/phd/French_yields/"
# } else {
#     # start on LSCE computer
#     root_path <- "/homel/bschaub/phd/French_yields/"
# }

root_path <- "/home/schauber/research/French_yields/"
data_path <- sprintf("%sFrenchCropProduction_by_H_and_T_Kato_06302017/", root_path)

source(sprintf("%s/codes/common_definitions.R", root_path))
source(sprintf("%s/codes/analysis_helper_functions.R", root_path))

# FAO_input_fn <- sprintf("%sFAO_stats/France_FAOSTAT_data_mod.csv", root_path)
# FAO_input_fn <- sprintf("%sFAO_stats/FAOSTAT_1961-2016_France.csv", root_path)
FAO_input_fn <- sprintf("%sFAO_stats/FAOSTAT_France_1961-2019_mod.csv", root_path)
fao_dat <- read.csv(FAO_input_fn, sep = ";", header = TRUE, stringsAsFactors = FALSE)
# country;element;crop;year;unit;value
# France;area;barley;1961;ha;2259100
# 
# units are: area = ha, yield = hg/ha, production = t

type <- "total"     # only total yields are compared

comparison_crops <- intersect(unique(fao_dat$crop), all_crops)

FAO_years <- 1961:2018
fao_dat <- fao_dat[fao_dat$year %in% FAO_years, ]       # FAO data would cover also 2019, but these are not necessary here

all_dat <- NULL

for (crop in comparison_crops) {
    print(crop)
    
#     crop_path <- sprintf("%sFrenchCropProduction_by_H_and_T_Kato_03102017/Departmental/%s/", root_path, crop)
    crop_path <- sprintf("%sDepartmental/%s/", data_path, crop)
    
    # read all data
#     fn <- sprintf("%s%s%s_yield_IRMA%s.csv", crop_path, crop, if (type > "") sprintf("_%s", type) else "", gapfill_adder)
    fn <- sprintf("%s%s%s_data_1900-2018_FILTERED.txt", crop_path, crop, if (type > "") sprintf("_%s", type) else "")
    
    dat_orig <- read.table(fn, sep = ";", header = TRUE, stringsAsFactors = FALSE)
    # "department";"year";"yield";"area";"production"
    # "Seine_et_Marne";1900;2.095;117067;7740
    
    # calculate area and production sums and average yield for the whole nation, with area weights
    # for this, all NA areas have to be filled with the average area for this department
    dat_orig$avg_dep_area <- ave(dat_orig$area, dat_orig$department, FUN = stable_mean)
    dat_orig$area[is.na(dat_orig$area)] <- dat_orig$avg_dep_area[is.na(dat_orig$area)]
    
    dat_orig$area_masked <- dat_orig$area
    dat_orig$area_masked[is.na(dat_orig$yield)] <- NA        # but omit all areas where there is no yield
    dat_orig$production_masked <- dat_orig$production
    dat_orig$production_masked[is.na(dat_orig$yield)] <- NA        # ... same for production
    
    # run 1: area and production data are masked before summation when no yield data are reported
    dat_orig$annual_production_masked <- ave(dat_orig$production_masked, dat_orig$year, FUN = stable_sum)
    dat_orig$annual_area_masked <- ave(dat_orig$area_masked, dat_orig$year, FUN = stable_sum)
    dat_orig$annual_yield <- dat_orig$annual_production_masked / dat_orig$annual_area_masked
    
    # run 2: area and production data are summed without masking 
    # (leads to a much higher agreement with FAO reported values and also makes more sense as the masking is somewhat arbitrary)
    dat_orig$annual_production <- ave(dat_orig$production, dat_orig$year, FUN = stable_sum)
    dat_orig$annual_area <- ave(dat_orig$area, dat_orig$year, FUN = stable_sum)
    
    # keep only unique entries (one per year)
    dat <- unique(dat_orig[, c("year", "annual_area", "annual_production", "annual_yield")])
    # keep only data for FAO years
    dat <- dat[dat$year %in% FAO_years, ]
    
    # correlate data with FAO data
    fao_crop_dat <- fao_dat[fao_dat$crop == crop, ]
    
    year_matcher <- match(FAO_years, dat$year)
    
    dev.new()
    layout(matrix(1:3, nrow = 1))
    
    for (compi in c("area", "production", "yield")) {
        fao_values <- fao_crop_dat$value[as.character(fao_crop_dat$element) == compi]
        if (compi == "yield") fao_values <- fao_values / 10000      # convert to 100g/ha to t/ha
        
        manual_values <- dat[year_matcher, sprintf("annual_%s", compi)]
        manual_values[manual_values <= 0] <- NA
        
        corri <- cor.test   (fao_values, manual_values, use = "complete.obs")
        
        new_entry <- data.frame(crop = crop, element = compi, correlation = round(corri$estimate, 3), pvalue = round(corri$p.value, 5))
        all_dat <- rbind(all_dat, new_entry)
        
        plot(manual_values ~ fao_values, main = compi, cex = 1.5, lwd = 2)
        abline(a = 0, b = 1, lwd = 3, col = "red", lty = 2)
    }
    title(outer = TRUE, main = crop, line = -2)
    
} # crops

print(all_dat)

print("DONE.")
