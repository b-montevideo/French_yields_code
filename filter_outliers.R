# Script to filter yield, area and production outliers
# Adapted for merged data 1900-2018
# 
# @author B. Schauberger (schauber@pik-potsdam.de)
# @version Apr 2021


rm(list = ls())
options(width = 120)
options(warn = 2)


root_path <- "/home/schauber/research/French_yields/"
data_path <- sprintf("%sFrenchCropProduction_by_H_and_T_Kato_06302017/", root_path)

image_output_path <- sprintf("%simages/", root_path)

source(sprintf("%s/codes/common_definitions.R", root_path))
source(sprintf("%s/codes/analysis_helper_functions.R", root_path))


# specify aggregation levels for filtering
# note that regional & national data only runs until 2013
# geo_levels <- c("Departmental", "Regional", "National")
geo_levels <- c("Departmental")

# specify the variables to be analysed
## MUST BE ALL THREE AT THE MOMENT
variables <- c("yield", "area", "production")

# specify whether control plots should be produced (one per crop/geo unit/variable)
produce_control_plots <- TRUE

# specify the sd thresholds to be applied, globally and per decade
sd_threshold_global <- c(4, 4, 4)        # any value larger or smaller than x times the global mean+/-sd is filtered (separately per geo unit)
sd_threshold_decade <- c(2, 3, 3)        # any value larger or smaller than x times the decadal mean+/-sd is filtered (separately per geo unit)
names(sd_threshold_global) <- c("yield", "area", "production")
names(sd_threshold_decade) <- c("yield", "area", "production")

# specify the maximum allowed value as quantile over all years per decade and departments; all values above that are masked
max_allowed_quantiles <- c(0.99, 1, 1)
names(max_allowed_quantiles) <- variables

# specify values that are clearly beyond reach; these are filtered before any mean/sd/quantile-based filtering to avoid domination by these outliers
# their numbers are reported separately and not counted among the 'usual' outliers
absurd_value_thresholds <- max_yield_values     # see common_definitions.R

color_sample <- dist_colors


                    flag_outliers <- function (dat, vari, max_allowed_quantile_values) {
                        dat$outlier <- FALSE
                        
                        # filter for maximum quantiles per decade
                        dat$max_allowed <- max_allowed_quantile_values[sprintf("%s_%04d", vari, dat$decade)]
                        dat$outlier <- dat$outlier | (dat[, vari] > dat$max_allowed)
                        dat[, vari] <- ifelse(dat[, "outlier"], NA, dat[, vari])
                        
                        # mean * sd global
                        dat$mean_full <- mean(dat[, vari], na.rm = TRUE)
                        dat$sd_full <- sd(dat[, vari], na.rm = TRUE)
                        
                        # flag values below mean-sd or above mean+sd, global (flagging here, masking below)
                        dat$outlier <- dat$outlier | (dat[, vari] < dat$mean_full - sd_threshold_global[vari] * dat$sd_full)
                        dat$outlier <- dat$outlier | (dat[, vari] > dat$mean_full + sd_threshold_global[vari] * dat$sd_full)
                        
                        # mask global outliers to prevent them from biasing decadal values
                        dat[, vari] <- ifelse(dat[, "outlier"], NA, dat[, vari])
                        
                        # mean & sd decadal, neglecting values already flagged as global outliers
                        dat$mean_dec <- ave(dat[, vari], dat$decade, FUN = stable_mean)
                        dat$sd_dec <- ave(dat[, vari], dat$decade, FUN = stable_sd)
                        
                        # flag values as above, now decadal
                        dat$outlier <- dat$outlier | (dat[, vari] < dat$mean_dec - sd_threshold_decade[vari] * dat$sd_dec)
                        dat$outlier <- dat$outlier | (dat[, vari] > dat$mean_dec + sd_threshold_decade[vari] * dat$sd_dec)
                        
                        # mask decadal outliers
                        dat[, vari] <- ifelse(dat[, "outlier"], NA, dat[, vari])
                        
                        # the 'outlier' flag is now TRUE iff the value is an outlier
                        
                        return (dat[, c("year", vari, "outlier")])
                    } # flag_outliers
                    

# geo_level = geo_levels[1]; crop = "rape"; type = "winter"
for (geo_level in geo_levels) {

    yoc_dat_all <- NULL
    for (crop in all_crops) {
        cat("************************************************************************************\n")
        cat(sprintf("GEO = %s, CROP = %s\n", geo_level, crop))
        cat("************************************************************************************\n")
        
        if ((crop %in% winter_spring_crops) && (geo_level != "National")) {
            types <- c("winter", "spring", "total")
        } else {
            types <- "total"
        }
        
#         gapfill_adder <- if (use_gapfilled_data && (crop %in% winter_spring_crops)) "_gapfilled" else "_unfilled"
        
        for (type in types) {
            cat(sprintf("Type = %s\n", type))
            
            if (geo_level == "Departmental") {
                crop_path <- sprintf("%s%s/%s/", data_path, geo_level, crop)
                fn <- sprintf("%s%s%s_data_1900-2018_RAW.txt", crop_path, crop, if (type > "") sprintf("_%s", type) else "")
                
                if (! file.exists(fn)) next
                
                dat_orig <- read.table(fn, sep = ";", header = TRUE, stringsAsFactors = FALSE)
                # "department";"year";"yield";"area";"production"
                # "Seine_et_Marne";1900;1.66733;4500;7503
                geo_units <- sort(unique(dat_orig$department))
                
                # manual filters (necessary due to one-year cumulated wrong values that cannot be filtered automatically)
                if ((crop == "rape") && (type == "winter")) {
                    count_nonNA <- sum(! is.na(dat_orig$yield[dat_orig$year == 1944]))
                    cat(sprintf("winter rape 1944 lost values: %d\n", count_nonNA))
                    dat_orig$yield[dat_orig$year == 1944] <- NA
                }
                if ((crop == "rape") && (type == "spring")) {
                    count_nonNA <- sum(! is.na(dat_orig$yield[dat_orig$year == 1968]))
                    cat(sprintf("spring rape 1968 lost values: %d\n", count_nonNA))
                    dat_orig$yield[dat_orig$year == 1968] <- NA
                }
                if ((crop == "barley") && (type == "spring")) {
                    count_nonNA <- sum(! is.na(dat_orig$yield[dat_orig$year == 1980]))
                    cat(sprintf("spring barley 1980 lost values: %d\n", count_nonNA))
                    dat_orig$yield[dat_orig$year == 1980] <- NA
                }
            } else {
                crop_path <- sprintf("%s%s/normalized/", data_path, geo_level)
                combined_dat <- NULL
                for (vari in variables) {
                    fn <- sprintf("%s%s_%s_%s_RAW.csv", crop_path, crop, type, vari)
                    dat_orig <- read.table(fn, sep = ";", header = TRUE, stringsAsFactors = FALSE)
                    # "Year";"Ile_de_France";"Champagne_Ardenne";...
                    # 1900;367568;1296767;...
                    
                    # reformat regional or national yields to default format as for departments
                    regions <- setdiff(names(dat_orig), "Year")
                    tmp_dat <- NULL
                    for (region in regions) {
                        if (geo_level == "Regional") {
                            new_entry <- data.frame(year = dat_orig$Year, region = region)
                        } else {
                            new_entry <- data.frame(year = dat_orig$Year, region = "FRANCE")
                        }
                        new_entry[, vari] <- dat_orig[, region]     # column for region/nation as single rows
                        
                        tmp_dat <- rbind(tmp_dat, new_entry)
                    } # regions/nation
                    tmp_dat[tmp_dat[, vari] < 0, vari] <- NA
                    
                    if (is.null(combined_dat)) {
                        combined_dat <- tmp_dat
                    } else {
                        combined_dat <- merge(x = combined_dat, y = tmp_dat, by = c("year", "region"))
                    }
                } # variables
                dat_orig <- combined_dat
                geo_units <- sort(unique(as.character(dat_orig$region)))
            }
            
            dat_orig$decade <- dat_orig$year - dat_orig$year %% 10
            years <- sort(unique(dat_orig$year))
            
            # filter fully absurd values before any further filtering, which would otherwise be dominated by those outliers
            # only for yield
            geo_factor <- if (geo_level == "Departmental") 1 else 1000      # dep = t/ha, rest = kg/ha
            absurd_values <- dat_orig$yield > absurd_value_thresholds[crop] * geo_factor
            cat(sprintf("   # absurdly high values = %d\n", sum(absurd_values, na.rm = TRUE)))
            dat_orig$yield[absurd_values] <- NA
            
            # get max quantiles for each variable across spatial units (only for departments)
            filter_quantiles <- c()
            for (vari in variables) {
                for (decade in sort(unique(dat_orig$decade))) {
                    index <- sprintf("%s_%04d", vari, decade)
                    if (geo_level == "Departmental") {
                        filter_quantiles[index] <- quantile(dat_orig[dat_orig$decade == decade, vari], probs = max_allowed_quantiles[vari], na.rm = TRUE)
                    } else {
                        filter_quantiles[index] <- max(dat_orig[, vari], na.rm = TRUE)
                    }
                } # decades
            } # variables
            
            result_dat <- NULL
            for (gu in geo_units) {
                if (geo_level == "Departmental") {
                    gu_dat <- dat_orig[dat_orig$department == gu, ]
                } else {
                    gu_dat <- dat_orig[dat_orig$region == gu, ]
                }
                
                vari_dat <- NULL
                for (vari in variables) {
                    # obtain value means and their standard deviations, once global and once per decade
                    tmp_dat <- gu_dat[, c("year", "decade", vari)]
                    
                    flagged_dat <- flag_outliers(dat = tmp_dat, vari = vari, max_allowed_quantile_values = filter_quantiles)
                    names(flagged_dat) <- c("year", vari, sprintf("%s_outlier", vari))
                    
                    if (is.null(vari_dat)) {
                        vari_dat <- flagged_dat
                        
                        if (geo_level == "Departmental") {
                            vari_dat$department <- gu
                        } else {
                            vari_dat$region <- gu
                        }
                        
                        # bring geo unit into first column
                        vari_dat <- vari_dat[, c(4, 1, 2, 3)]
                    } else {
                        vari_dat <- merge(x = vari_dat, y = flagged_dat, by = "year")
                    }
                } # variables
                
                result_dat <- rbind(result_dat, vari_dat)
            } # geo units (e.g. departments)
            
            # print stats across geo units, per variable
            for (vari in variables) {
                num_vals <- sum(! is.na(result_dat[, vari]))        # note that these are already the filtered counts since outliers were removed in flag_outliers()
                num_out <- sum(result_dat[, sprintf("%s_outlier", vari)], na.rm = TRUE)
                cat(sprintf("   VAR = %10s, total #values = %3d, outliers flagged = %3d (%3.1f%%), remaining vals = %3d\n", vari, 
                            num_vals + num_out, num_out, num_out / (num_vals + num_out) * 100, num_vals))
            } # variables
            
            # count yield outliers per department
            result_dat$yoc <- ave(result_dat$yield_outlier, result_dat$department, FUN = function (x) sum(x, na.rm = TRUE))
            yoc_dat <- unique(result_dat[, c("department", "yoc")])
            yoc_dat$crop <- paste(crop, type, sep = "_")
            yoc_dat_all <- rbind(yoc_dat_all, yoc_dat)

# ALREADY DONE in flag_outliers()            
#             # finally mask outliers by NA
#             for (vari in variables) {
#                 result_dat[, vari] <- ifelse(result_dat[, sprintf("%s_outlier", vari)], NA, result_dat[, vari])
#             } # variables
            
            # remove unnecessary columns
            result_dat <- result_dat[, c(2, 1, 3, 5, 7)]
            
            # write filtered output
            if (geo_level == "Departmental") {
                result_fn <- sprintf("%s%s%s_data_1900-2018_FILTERED.txt", crop_path, crop, if (type > "") sprintf("_%s", type) else "")
            } else {
                result_fn <- sprintf("%s%s_%s_data_1900-2018_FILTERED.txt", crop_path, crop, type)
            }
#             write.table(result_dat, result_fn, sep = ";", row.names = FALSE)
            
            
            
            if (produce_control_plots) {
                # plot all values per variable in time series
                for (vari in variables) {
                    
                    if (all(is.na(result_dat[, vari]))) next
                    
                    img_fn <- sprintf("%sdata_inspection/filtered/%s_%s_%s_%s_FILTERED.png", image_output_path, crop, type, vari, geo_level)
                    png(img_fn, width = 1200, height = 720)
                    
                    max_y <- max(result_dat[, vari], na.rm = TRUE)
                    
                    plot(NA, xlab = "", ylab = vari, xlim = range(years), ylim = c(0, max_y))
                    for (gu in geo_units) {
                        x_vals <- years
                        if (geo_level == "Departmental") {
                            y_vals <- result_dat[result_dat$department == gu, vari]
                        } else {
                            y_vals <- result_dat[result_dat$region == gu, vari]
                        }
                        
                        if (length(y_vals) != length(x_vals)) next      # some departments have no data in 2017-2018
                        
                        lines(x_vals, y_vals, col = color_sample[round(runif(n = 1, min = 1, max = length(color_sample)))], lwd = 2)
                    } # geo units
                    dev.off()
                }
            }
        } # season type
    } # crop
} # aggregation levels

# print number of yield outliers per department
yoc_dat_all$total_count <- with(yoc_dat_all, ave(yoc, department, FUN = stable_sum))
yoc_dat_all <- unique(yoc_dat_all[, c("department", "total_count")])
yoc_dat_all <- yoc_dat_all[order(yoc_dat_all$total_count, decreasing = FALSE), ]
print(yoc_dat_all)
summary(yoc_dat_all)

print("DONE.")
