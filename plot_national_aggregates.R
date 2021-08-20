# Script to plot nationally aggregated crop statistics (with land-use weighting)
#
# @author schauber@pik-potsdam.de; bernhard.schauberger@lsce.ipsl.fr
# @date Apr 2021



## set up script
## ------------------------------------------------------------------------

rm(list=ls())
options(width=190)
options(warn=2)


root_path <- "/home/schauber/research/French_yields/"
data_path <- sprintf("%sFrenchCropProduction_by_H_and_T_Kato_06302017/", root_path)

image_output_path <- sprintf("%simages/trends/", root_path)

source(sprintf("%s/codes/common_definitions.R", root_path))
source(sprintf("%s/codes/analysis_helper_functions.R", root_path))
 
# # specify whether gap-filled (TRUE) or unfilled (FALSE) data should be used
# # note that also in the unfilled case some obvious outliers were corrected manually
# # gap-filling is only possible for crops with distinct winter and spring phenology
# use_gapfilled_data <- FALSE

yield_scales <- c(0.1, 0.1, 0.1)
names(yield_scales) <- c("sugarbeet_total", "potatoes_total", "wine_total")
prod_scales <- c(0.1) #, 0.1)
names(prod_scales) <- c("wine_total") #, "wheat_winter")

scaling_factors <- list()
scaling_factors[["yield"]] <- yield_scales
scaling_factors[["area"]] <- NA
scaling_factors[["production"]] <- prod_scales

# specify national percentiles to be calculated
percentile_probs <- c(0.05, 0.1, 0.5, 0.9, 0.95) # seq(0.1, 0.9, by = 0.1)



## aggregate yield, area and production for each crop
## ------------------------------------------------------------------------
# crop <- all_crops[1]; type <- "total"
all_dat <- NULL
for (crop in all_crops) {
    print(crop)
    
    crop_path <- sprintf("%sDepartmental/%s/", data_path, crop)
    
    # read all data
    
    if (crop %in% winter_spring_crops) {
        types <- c("winter", "spring", "total")
    } else {
        types <- "total"
    }
    
    for (type in types) {
        print(sprintf("Type = %s", type))
        
        fn <- sprintf("%s%s%s_data_1900-2018_FILTERED.txt", crop_path, crop, if (type > "") sprintf("_%s", type) else "")
        
        dat_orig <- read.table(fn, sep = ";", header = TRUE)
        # "department";"year";"yield";"area";"production"
        # "Seine_et_Marne";1900;2.095;117067;zxcv
        
        # calculate area and production sums and average yield for the whole nation, with area weights
        # for this, all NA areas have to be filled with the average area for this department (if a yield value is present)
        dat_tmp <- dat_orig
        dat_tmp$avg_dep_area <- ave(dat_tmp$area, dat_tmp$department, FUN = stable_mean)
        dat_tmp$area_for_yield <- dat_tmp$area
        dat_tmp$area_for_yield[is.na(dat_tmp$area)] <- dat_tmp$avg_dep_area[is.na(dat_tmp$area)]
        dat_tmp$area_for_yield[is.na(dat_tmp$yield)] <- NA        # but omit all areas where there is no yield
        dat_tmp$production_for_yield <- dat_tmp$production
        dat_tmp$production_for_yield[is.na(dat_tmp$yield)] <- NA        # ... same for production
        
        # note (2021/06/17): if a yield value was present but has been filtered, area and production sums are artificially reduced 
        # this was detected as area and production spikes for 2016 were sharp - which was completely wrong for area and less pronounced for production
        # => use the yield-filtered area and production sums to calculate average yield, but keep area & production sums separate
        
        dat_tmp$annual_area <- ave(dat_tmp$area, dat_tmp$year, FUN = stable_sum) / 1e6                  # convert ha to Mha
        dat_tmp$annual_production <- ave(dat_tmp$production, dat_tmp$year, FUN = stable_sum) / 1e6      # convert t to Mt
        
        dat_tmp$annual_area_for_yield <- ave(dat_tmp$area_for_yield, dat_tmp$year, FUN = stable_sum) / 1e6                  # convert ha to Mha
        dat_tmp$annual_production_for_yield <- ave(dat_tmp$production_for_yield, dat_tmp$year, FUN = stable_sum) / 1e6      # convert t to Mt
        dat_tmp$avg_annual_yield <- dat_tmp$annual_production_for_yield / dat_tmp$annual_area_for_yield
        
        # calculate yield percentiles per year, over all departments (not area-weighted)
        for (perc_prob in percentile_probs) {
            dat_tmp[, sprintf("perc%0.2f", perc_prob)] <- ave(dat_tmp$yield, dat_tmp$year, FUN = function (x) quantile(x, probs = perc_prob, na.rm = TRUE))
        } # percentile probabilities
        
        # calculate min/max difference, with 5%-trimmed extreme to avoid outliers dominating
        dat_tmp$min_max_diff_trim5 <- dat_tmp$"perc0.95" - dat_tmp$"perc0.05"
        dat_tmp$min_max_diff_trim10 <- dat_tmp$"perc0.90" - dat_tmp$"perc0.10"
        
        dat_tmp <- unique(dat_tmp[, c("year", "avg_annual_yield", "annual_area", "annual_production", "perc0.05", "perc0.50", "perc0.95", "min_max_diff_trim5")])
#         dat_tmp <- unique(dat_tmp[, c("year", "avg_annual_yield", "annual_area", "annual_production", "perc0.10", "perc0.90", "min_max_diff_trim10")])
        
        new_entry <- data.frame(crop = sprintf("%s_%s", crop, type), year = dat_tmp$year, yield = dat_tmp$avg_annual_yield, 
                                area = dat_tmp$annual_area, production = dat_tmp$annual_production, 
                                yield_min = dat_tmp$"perc0.05", yield_max = dat_tmp$"perc0.95", yield_median = dat_tmp$"perc0.50", 
                                yield_range = dat_tmp$min_max_diff_trim5)
#                                 yield_min = dat_tmp$"perc0.10", yield_max = dat_tmp$"perc0.90", yield_range = dat_tmp$min_max_diff_trim10)
        
        all_dat <- rbind(all_dat, new_entry)
    } # seasonal types
} # crops



## plot collected national averages
## ------------------------------------------------------------------------
variables <- c("yield", "area", "production")
variable_captions <- c("Yield (t/ha)", "Area (Mha)", "Production (Mt)")
names(variable_captions) <- variables

crops <- unique(as.character(all_dat$crop))       # 17 in total

season_types <- c("spring_winter", "total")

for (variable in variables) {
    for (season_type in season_types) {
        if (season_type == "total") {
            season_crops <- c(  "barley_total", "sugarbeet_total", "maize_total", "oats_total", "potatoes_total", "rape_total", "sunflower_total", "wheat_total", 
                                "wine_total", "wheat_durum_total")
        } else {
            season_crops <- c("barley_spring", "barley_winter", "oats_spring", "oats_winter", "rape_spring", "rape_winter", "wheat_spring", "wheat_winter")
        }
        
        if (season_type == "total") {
            xlim <- c(1900, 2018)
            legend_x <- 1900
        } else {
            xlim <- c(1940, 2018)
            legend_x <- 1940
        }
        
        img_fn <- sprintf("%snational_%s_data_%s.eps", image_output_path, variable, season_type)
        cat(sprintf("Plot into %s...\n", img_fn))
        
        postscript( img_fn, width = 12, height = 9, horizontal = FALSE, title = sprintf("%s_%s", variable, season_type), 
                    onefile = FALSE, paper = "special", encoding = "TeXtext.enc", family = "Helvetica")  #CenturySch") # "ComputerModern")

# par(mar = c(3, 5, 1, 1.45))
        
        yrange <- c(0, switch(variable, yield = 13, area = 10, production = 40))
        par(mar = c(5.5, 5.5, 1.5, 1.5))
        plot(NA, xlim = xlim, ylim = yrange, xlab = "Year", ylab = variable_captions[variable], cex.lab = 2.5, cex.axis = 2.5)
        
        abline(v = seq(1900, 2020, by = 10), lty = 2, col = "grey", lwd = 2)
        
        scaling_factors_vari <- scaling_factors[[variable]]
        
        for (crop in season_crops) {
            crop_dat <- all_dat[all_dat$crop == crop, ]
            
            y_dat <- crop_dat[, variable]
            
            sca_fac_crop <- scaling_factors_vari[crop]
            if (! is.na(sca_fac_crop)) {
                y_dat <- y_dat * sca_fac_crop
#                 print(sprintf("scaled %s by %0.1f", crop, sca_fac_crop))
            }
            
            lines(crop_dat$year, y_dat, lwd = 7, col = crop_colors[crop], type = "l", lty = line_types[crop]) #, cex = 0.2)
        }
        
        scaling_texts <- sapply(season_crops, function (cro) {sca_fac_crop <- scaling_factors_vari[cro]; if (! is.na(sca_fac_crop)) sprintf(" (*%0.1f)", sca_fac_crop) else ""})
        legi_crops <- sprintf("%s%s", crops_long[season_crops], scaling_texts)
        
#         legend(x = 1900, y = max(yrange), legend = legi_crops, lwd = 5, lty = line_types[season_crops], col = crop_colors[season_crops], bg = "white", bty = "n", ncol = 4, cex = 2.5)
        
        if ((variable == "production")) {
            ncols <- 2
        } else {
            ncols <- 3
        }
        legend( x = legend_x, y = max(yrange), legend = legi_crops, lwd = 5, col = crop_colors[season_crops], bg = "white", bty = "n", 
                ncol = ncols, cex = 1.7, lty = line_types[season_crops])
        
        dev.off()
    } # season types
} # variables



## plot collected national quantiles and yield range
## ------------------------------------------------------------------------

img_fn <- sprintf("%snational_percentiles.eps", image_output_path)
cat(sprintf("Plot into %s...\n", img_fn))
postscript( img_fn, width = 12, height = 14, horizontal = FALSE, title = "national_percentiles", 
            onefile = FALSE, paper = "special", encoding = "TeXtext.enc", family = "Helvetica")  #CenturySch") # "ComputerModern")
                
#                 par(mar = c(3, 5, 1, 1.45))
layout(matrix(c(1:18, 19, 19), ncol = 4, byrow = T))

years <- 1900:2018
# except for wine where we have 2016 as limit

for (crop in all_crops) {
    print(crop)
    
    if (crop %in% winter_spring_crops) {
        types <- c("winter", "spring", "total")
    } else {
        types <- "total"
    }
    
    for (type in types) {
        print(sprintf("     Type = %s", type))
        
        full_crop <- sprintf("%s_%s", crop, type)
        crop_dat <- all_dat[all_dat$crop == full_crop, ]
        
        yrange <- c(0, max(crop_dat$yield_max, na.rm = TRUE) * 1.1)
        #par(mar = c(3.5, 3.5, 1, 1))
        par(mar = c(3, 2.8, 0.4, 0.6))
        plot(NA, xlim = range(years), ylim = yrange, xlab = "", ylab = "", cex.axis = 1.8)
        
        if (crop == "wine") {
            plot_years <- 1900:2016
        } else {
            plot_years <- years
        }
        lines(plot_years, crop_dat[, "yield_min"], lwd = 6, col = "blue", type = "l")
        lines(plot_years, crop_dat[, "yield_max"], lwd = 6, col = "green", type = "l")
        lines(plot_years, crop_dat[, "yield_median"], lwd = 6, col = "grey", type = "l")
        
        abline(v = seq(1900, 2010, by = 10), lty = 2, col = "grey", lwd = 1)
        
        title(crops_long[full_crop], cex.main = 2, line = -2)
        
        box(lwd = 2)
        
    } # seasonal types
    
} # crops

plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE)

legend(x = 0, y = 1, legend = c("95%-Quantile", "Median", "5%-Quantile"), lwd = 8, col = c("green", "grey", "blue"), bg = "white", bty = "n", ncol = 1, cex = 2.3)

dev.off()

print("DONE.")

