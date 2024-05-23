library(tidyverse)
library(haven)
library(ggrepel)
library(terra)
library(sf)
library(leaflet)
library(tidyterra)
library(RColorBrewer)
library(ggthemes)

library(tmap)

theme_set(theme_few())

smod_years <- c("1975", "1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")

fnames <- paste0("data/shared-data/geo/ghs/smod/GHS_SMOD_E", smod_years, "_GLOBE_R2023A_54009_1000_V1_0.tif")

fnames

rasters <- rast(fnames)

mexico_map_og <- vect("data/shared-data/geo/gadm/gadm36_2.shp") %>%
    filter(NAME_0 == "Mexico")

mex_map <- project(mexico_map_og, rasters)

mex2 <- mex_map %>%
    aggregate(by="NAME_1") 

ghs_cities <- vect("data/shared-data/geo/ghs/ucdb//GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

herm_city_shape <- ghs_cities %>% filter(UC_NM_MN == "Hermosillo")

# key
# 30: urban centre grid cell
# 23: dense urban cluster
# 22: semi-dense urban cluster
# 21: suburban or peri-urban grid cell
# 13: rural cluster
# 12: low density rural 
# 11: very low density rural
# 10: water 

city_limit_lower <- 14

gdp_dat <- rast("data/grid/aggdp2010.tif")
gdp_mask <- mask(crop(project(gdp_dat, mex_map), mex_map), mex_map)

cleaned_rasters <- rasters %>%
    crop(mex_map) %>%
    mask(mex_map) %>%
    clamp(lower=city_limit_lower, value=FALSE) 

names(cleaned_rasters) <- smod_years

pop_fnames <- paste0("data/shared-data/geo/ghs/pop/GHS_POP_E", smod_years, "_GLOBE_R2023A_54009_1000_V1_0.tif")
pop_raster <- rast(pop_fnames) %>%
    crop(mex_map) %>%
    mask(mex_map)

names(pop_raster) <- smod_years

cuencas <- vect("data/conagua-shapefiles/m00_cuencasPolygon.shp") %>%
    project(mex_map)

rios <- vect("data/conagua-shapefiles/m00_riosprincipalesLine.shp") %>%
    project(mex_map)

ggplot() +
    geom_spatvector(data = mex2, color='grey', fill=NA) +
    geom_spatvector(data = rios, color='blue', fill=NA) +
    geom_spatvector(data = rios %>% filter(rio == "Mayo"), color='red', fill=NA) +
    geom_spatvector(data = rios %>% filter(rio == "Yaqui"), color='red', fill=NA) 

plet(rios, tiles="CartoDB.Positron", col='blue')  |>
    lines(rios %>% filter(rio %in% c("Mayo", "Yaqui", "Sonora")), col='red') 




rios_df <- rios %>%
    as.data.frame() %>%
    tibble()


zones <- read_dta("data/zonamet_expanded.dta") %>%
    mutate(ZM = ifelse(ZM == "San Luis Potos\xed-Soledad", "San Luis Potos-Soledad", ZM)) 

concesions <- vect("data/conagua-shapefiles/m01_totconcesionesbmPolygon.shp")
approvals <- vect("data/conagua-shapefiles/m01_vwaprovsbmPoint.shp")
grouped_usage <- vect("data/conagua-shapefiles/m01_usoagrupadosbmPolygon.shp")
grouped_usage_superficial <- vect("data/conagua-shapefiles/m01_usoagrupadospmPolygon.shp")
grouped_usage_reclaimed <- vect("data/conagua-shapefiles/m01_usoagrupadoarmPolygon.shp")


approvals %>%
    filter(municipio == "Hermosillo") %>%
    select(grupouso, volsb)

herm_approvals <- approvals %>%
    filter(municipio == "Hermosillo") %>%
    project(hermosillo_map) %>%
    crop(hermosillo_map)

herm_rios <- rios %>%
    crop(hermosillo_map)

herm_city_area <- cleaned_rasters %>%
    crop(hermosillo_map) %>%
    mask(hermosillo_map)

unique(values(herm_city_area['2020']))

herm_city_area[is.nan(herm_city_area)] <- NA

herm_city_area[!is.na(herm_city_area)] <- 1

ggplot() +
    geom_spatraster(data = herm_city_area, aes(fill=`2020`)) +
    geom_spatvector(data=hermosillo_map, color='black', fill=NA) +
    geom_spatvector(data=herm_approvals, aes(color=grupouso, size=volsb), alpha=0.5) +
    geom_spatvector(data=herm_rios, color='blue', fill=NA) +
    scale_fill_viridis_c(na.value = 'transparent') +
    labs(
         title = "Water Concessions by type in Hermosillo"
    )

concesions_df <- tibble(as.data.frame(concesions))
approvals_df <- tibble(as.data.frame(approvals))
grouped_usage_df <- tibble(as.data.frame(grouped_usage)) %>%
    mutate(source = "Underground")
grouped_usage_superficial_df <- tibble(as.data.frame(grouped_usage_superficial)) %>%
    mutate(source = "Superficial")
grouped_usage_reclaim_df <- tibble(as.data.frame(grouped_usage_reclaimed)) %>%
    mutate(source = "Reclaimed")

grouped_usage_reclaim_df %>%
    arrange(-total)

grouped_usage_df
grouped_usage_superficial_df

grouped_usage_all <- rbind(grouped_usage_df, grouped_usage_superficial_df) %>%
    rbind(grouped_usage_reclaim_df) %>%
    select(-id) %>%
    pivot_longer(cols=c("Agropecuar", "Agua Potab", "Ecológico", "Hidroeléct", "Industrial", "Termoeléct", "total")) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    pivot_wider(names_from=source, values_from=value, values_fill=0) %>%
    mutate(
           source_all = Underground + Superficial + Reclaimed
    ) 

grouped_usage_all %>%
    filter(name == "Industrial") %>%
    filter(estado == "Sonora") %>%
    arrange(-source_all) %>%
    head(200) %>%
    ggplot(aes(x=source_all, y=reorder(municipio, source_all))) +
    geom_bar(stat='identity') +
    geom_bar(data = . %>% filter(municipio == "Hermosillo"), fill = "red", stat='identity') +
    labs(
         title = "Industrial Water Rights",
         subtitle = "Sonora",
         x = "Volume of Water",
         y = "",
         caption = "source: CONAGUA"
    )

zm_grouped_water <- grouped_usage_all %>%
    mutate(geocode = paste0(clvedo, clvmun)) %>%
    left_join(zones, by="geocode") %>%
    # filter(name %in% c("total", "Industrial"))  %>% %>%
    group_by(codeZM, ZM, name) %>%
    summarise(
              source_all = sum(source_all, na.rm=T),
              Underground = sum(Underground, na.rm=T),
              Reclaimed = sum(Reclaimed, na.rm=T),
              Superficial = sum(Superficial, na.rm=T)
    ) 

zm_grouped_water %>%
    write_csv("data/zm_grouped_water.csv")

comparators <- c(
                 "Tijuana", 
                 "Salitillo", 
                 "Aguascalientes", 
                 "Queretaro", 
                 "Monterrey", 
                 "Morelia",
                 "Chihuaha",
                 "Mexicali",
                 "Guaymas", 
                 "Reynosa-Rio Bravo", 
                 "Guadalajara", 
                 "Valle de Mexico", 
                 "Hermosillo")

# TODO: We have zm_grouped_water which tells us water quantities for industrial use. We have the economic census which tells us the payments made. Can we combine these to 
# show that Hermosillo is not using much water compared to other economies. Basically create a killer viz that shows that Hermosillos economy is one of camels

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) %>%
    pivot_wider(names_from=name, values_from=source_all) %>%
    arrange(-total) %>%
    head(100) %>%
    ggplot(aes(x=log10(`total`), y=log10(Industrial), label=ZM, alpha=(ZM %in% comparators))) +
    geom_point() +
    geom_text_repel() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red', size=2) +
    scale_alpha_manual(values=c(0.25, 1)) +
    scale_y_continuous(n.breaks = 10) +
    labs(
         title = "Industrial Water Usage, 2022",
         x = "Log of Total Volume of Water",
         y = "Log of Industrial Volume of Water Used",
         caption = "source: CONAGUA"
    ) +
    theme(legend.position = "none")

ggsave("imgs/industrial-water-usage.png", height=9, width=15)

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) %>%
    pivot_wider(names_from=name, values_from=source_all) %>%
    arrange(-total) %>%
    filter(Industrial > 1) %>%
    filter(ZM != "Tuxpan") %>%
    ggplot(aes(x=(`total`), y=(Industrial), label=ZM, alpha=(ZM %in% comparators))) +
    geom_point() +
    geom_text_repel() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red', size=2) +
    scale_alpha_manual(values=c(0.25, 1)) +
    scale_y_continuous(n.breaks = 10) +
    labs(
         title = "Industrial Water Usage, 2022",
         x = "Total Volume of Water",
         y = "Industrial Volume of Water Used",
         caption = "source: CONAGUA"
    ) +
    theme(legend.position = "none")

ggsave("imgs/industrial-water-usage-nolog.png", height=9, width=15)

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) %>%
    pivot_wider(names_from=name, values_from=source_all) %>%
    arrange(-total) %>%
    head(100) %>%
    ggplot(aes(x=log10(`total`), y=log10(`Agua Potab`), label=ZM, alpha=(ZM %in% comparators))) +
    geom_point() +
    geom_text_repel() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red') +
    scale_alpha_manual(values=c(0.25, 1)) +
    labs(
         title = "Drinking Water Usage, 2022",
         x = "Log of Total Volume of Water",
         y = "Log Volume of Potable Water",
         caption = "source: CONAGUA"
    ) +
    theme(legend.position = "none")

ggsave("imgs/potable-water-usage.png", height=9, width=15)

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) 

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) %>%
    pivot_wider(names_from=name, values_from=source_all) %>%
    arrange(-total) %>%
    head(100) %>%
    ggplot(aes(x=log10(`total`), y=log10(`Agropecuar`), label=ZM, alpha=(ZM %in% comparators))) +
    geom_point() +
    geom_text_repel() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red') +
    scale_alpha_manual(values=c(0.25, 1)) +
    labs(
         title = "Drinking Water Usage, 2022",
         x = "Log of Total Volume of Water",
         y = "Log Volume of Agricultural Water",
         caption = "source: CONAGUA"
    ) +
    theme(legend.position = "none")

ggsave("imgs/agri-water-usage.png", height=9, width=15)


zm_totals <- zm_grouped_water %>%
    filter(name == "total") %>%
    select(codeZM, ZM, source_all) %>%
    rename(total = source_all)

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) %>%
    left_join(zm_totals) %>%
    mutate(
           percent = source_all / total
    ) %>%
    filter(ZM == "Hermosillo") 

%>%
    pivot_wider(names_from=name, values_from=source_all) 
    

zm_grouped_water %>%
    select(codeZM, ZM, name, source_all) %>%
    pivot_wider(names_from=name, values_from=source_all) %>%
    arrange(-total) %>%
    head(100) %>%
    ggplot(aes(x=log10(`Agua Potab`), y=log10(Agropecuar), label=ZM, alpha=(ZM %in% comparators))) +
    geom_point() +
    geom_text_repel() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red') +
    scale_alpha_manual(values=c(0.25, 1)) +
    labs(
         title = "Industrial Water Usage",
         x = "Log of Total Volume of Water",
         y = "Log of Industrial Volume of Water Used",
         caption = "source: CONAGUA"
    )

zm_grouped_water %>%
    pivot_wider(names_from=name, values_from=source_all) %>%
    arrange(-total) %>%
    head(100) %>%
    ggplot(aes(x=log10(total), y=log10(Industrial), label=ZM, alpha=(ZM %in% comparators))) +
    geom_point() +
    geom_text_repel() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red') +
    scale_alpha_manual(values=c(0.25, 1)) +
    labs(
         title = "Industrial Water Usage",
         x = "Log of Total Volume of Water",
         y = "Log of Industrial Volume of Water Used",
         caption = "source: CONAGUA"
    )


grouped_usage_all %>%
    filter(name %in% c("total", "Industrial")) %>%
    arrange(-source_all) %>%
    head(200) %>%
    ggplot(aes(x=source_all, y=reorder(paste0(estado, ": ", municipio), source_all))) +
    geom_bar(stat='identity') +
    geom_bar(data = . %>% filter(municipio == "Hermosillo"), fill = "red", stat='identity') +
    labs(
         title = "Industrial Water Rights",
         subtitle = "Sonora",
         x = "Volume of Water",
         y = "",
         caption = "source: CONAGUA"
    )

concesions_df %>%
    arrange(-voltotsb) %>%
    head(100) %>%
    ggplot(aes(y=reorder(municipio, -voltotsb), x=voltotsb)) +
    geom_bar(stat='identity') +
    geom_bar(data = . %>% filter(municipio == "Hermosillo"), stat='identity', fill='red') +
    labs(
         title = "Underground Water Usage, 2022",
         x = "Volume (Million cubic Meters per Year)",
         y = ""
    )

grouped_usage_df %>%
    arrange(-total) %>%
    head(100) %>%
    ggplot(aes(x=total, y=reorder(municipio, total))) +
    geom_bar(stat = 'identity') 



approvals_df  %>%
    select(grupouso) %>% unique()

res <- approvals_df  %>%
    filter(municipio == "Hermosillo")  %>%
    filter(grupouso == "Industrial Integrado") %>%
    arrange(-volsb) 

res


approvals_df %>%
    group_by(municipio, acuifero, sciansecto, usuario) %>%
    summarise(vol = sum(volsb)) %>%
    arrange(-vol) %>%
    filter(municipio == "Hermosillo") %>%
    ggplot(aes(x=vol, y=sciansecto)) +
    geom_bar(stat="identity") +
    facet_wrap(~acuifero)

approvals_df %>%
    group_by(municipio, sciansecto) %>%
    summarise(vol = sum(volsb)) %>%
    filter(municipio == "Hermosillo") %>%
    arrange(-vol) %>%
    ggplot(aes(x=vol, y=reorder(sciansecto, vol))) +
    geom_bar(stat="identity") +
    labs(
         title = "Underwater Water use Rights by usage, Hermosillo",
         subtitle = "2022",
         y = "",
         x = "Volume"
    )

approvals_df %>%
    group_by(municipio, sciansecto) %>%
    summarise(vol = sum(volsb)) %>%
    filter(municipio == "San Luis Río Colorado") %>%
    arrange(-vol) %>%
    ggplot(aes(x=vol, y=reorder(sciansecto, vol))) +
    geom_bar(stat="identity") +
    labs(
         title = "Underwater Water use Rights by usage, Hermosillo",
         subtitle = "2022",
         y = "",
         x = "Volume"
    )
    
top_munis <- approvals_df %>%
    group_by(municipio) %>%
    summarise(vol = sum(volsb)) %>%
    arrange(-vol)


concesions %>%
    filter(municipio == "Hermosillo")

concesions %>%
    filter(rha == "Noroeste") %>%
    ggplot() +
        geom_spatvector(aes(fill=voltotsb))

grouped_usage %>%
    filter(Industrial < 200) %>%
    ggplot() +
    geom_spatvector(aes(fill=Industrial)) +
    geom_spatvector(data = . %>% filter(municipio == "Hermosillo"), color='red', aes(fill=Industrial))

#plot(concesions, col=)

plot(ag_gdp_sonora)
plot(rios, add=T, col='red')
plot(hermosillo_map, add=T)

#plot(log10(gdp_mask))
png("imgs/mexico-1975.png")
#plot(log10(gdp_mask))
plot(mex2)
plot(cleaned_rasters[[1]], col="blue", add=T, legend=F)
plot(aggregate(filter(mex_map, NAME_1 == "Sonora")), add=T, border="red")
plot(filter(mex_map, NAME_2 == "Hermosillo"), add=T, border="blue")
title("Mexico, 1975")
dev.off()
#plot(mex_map, add=TRUE)

png("imgs/mexico-2020.png")
#plot(log10(gdp_mask))
plot(mex2)
plot(cleaned_rasters[["2020"]], col="blue", add=T, legend=F)
plot(aggregate(filter(mex_map, NAME_1 == "Sonora")), add=T, border="red")
plot(filter(mex_map, NAME_2 == "Hermosillo"), add=T, border="blue")
title("Mexico, 2020")
dev.off()

# sonora only
sonora_map <- mex_map %>% filter(NAME_1 == "Sonora")
ag_gdp_sonora <- mask(crop(gdp_mask, sonora_map), sonora_map)

# plot(log10(clamp(ag_gdp_sonora, upper=100000000)))
png("imgs/sonora-1975.png")
plot(mask(crop(cleaned_rasters[[1]], sonora_map), sonora_map), add=F, col="blue", legend=F)
plot(sonora_map, add=T)
plot(filter(sonora_map, NAME_2 == "Hermosillo"), add=T, border="red")
title("Sonora, 1975")
dev.off()

# plot(log10(ag_gdp_sonora))
png("imgs/sonora-2020.png")
plot(mask(crop(cleaned_rasters[["2020"]], sonora_map), sonora_map), add=F, legend=F, col="blue")
plot(sonora_map, add=T)
plot(filter(sonora_map, NAME_2 == "Hermosillo"), add=T, border="red")
title("Sonora, 2020")
dev.off()

# only hermosillo

hermosillo_map <- mex_map %>% filter(NAME_2 == "Hermosillo")
ag_gdp_sonora <- mask(crop(gdp_mask, hermosillo_map), hermosillo_map)

png("imgs/hermosillo-1975.png")
plot(mask(crop(cleaned_rasters[[1]], hermosillo_map), hermosillo_map), add=F, col="blue", legend=F)
plot(hermosillo_map, add=T)
title("Hermosillo, 1975")
dev.off()

# plot(log10(ag_gdp_sonora))
png("imgs/hermosillo-2020.png")
plot(mask(crop(cleaned_rasters[['2020']], hermosillo_map), hermosillo_map), add=F, col="blue", legend=F)
plot(hermosillo_map, add=T)
title("Hermosillo, 2020")
dev.off()

city <- "Hermosillo"
year <- "2020"

render_city <- function(city, year, save) {
    #save = FALSE
    city_map <- mex_map %>% filter(NAME_2 == city)
    ag_gdp_city <- mask(crop(gdp_mask, city_map), city_map)

    if(save) {
        png(paste0("imgs/", city, "-", year, ".png"))
    }

    city_border <- cleaned_rasters[[year]] %>%
        crop(city_map) %>%
        mask(city_map) 

    pop_mask <- pop_raster[[year]] %>%
        crop(city_border) %>%
        mask(city_border)

    pop_mask[city_border == 0 | is.na(city_border)] <- NA

    pop <- sum(values(pop_mask), na.rm = T)

    plot(log10(pop_mask), add=F, col=map.pal('viridis'))
    plot(city_map, add=T)
    title(paste0(city, ", ", year))
    mtext(paste0("Population: ", round(pop)), side=3)

    if(save) {
        dev.off()
    }
}

render_city2 <- function(city, year, year2, save) {

    #save = FALSE
    city_map <- mex_map %>% filter(NAME_2 == city)
    ag_gdp_city <- mask(crop(gdp_mask, city_map), city_map)

    if(save) {
        png(paste0("imgs/", city, "-", year, "-", year2, ".png"))
    }

    base_map <- cleaned_rasters[[year]] %>%
        crop(city_map) %>%
        mask(city_map)

    base_pop_map <- pop_raster[[year]] %>%
        crop(base_map) %>%
        mask(base_map) 

    base_pop_map[base_map == 0 | is.na(base_map)] <- NA

    next_map <- cleaned_rasters[[year2]] %>%
        crop(city_map) %>%
        mask(city_map)

    next_pop_map <- pop_raster[[year2]] %>%
        crop(next_map) %>%
        mask(next_map)

    next_pop_map[next_map == 0 | is.na(next_map)] <- NA

    base_map[!is.na(base_map)] <- 1
    base_map[is.na(base_map)] <- 0

    next_map[!is.na(next_map)] <- 1
    next_map[is.na(next_map)] <- 0

    diff <- next_map - base_map
    diff[diff == 0] <- NA

    base_tiles <- sum(values(base_map), na.rm=T)
    base_pop = sum(values(base_pop_map), na.rm=T)

    next_tiles <- sum(values(next_map), na.rm=T)
    next_pop = sum(values(next_pop_map), na.rm=T)

    new_tiles <- sum(values(diff), na.rm=T)

    tile_growth <- round(new_tiles / base_tiles * 100, 2)
    pop_growth <- round((next_pop-base_pop) / base_pop * 100, 2)

    density1 <- base_pop / base_tiles
    density2 <- next_pop / next_tiles
    density_growth <- round((density2 - density1) / density1 * 100, 2)

    if(city == "Hermosillo") {

        xt <- herm_city_shape %>% project(city_map) %>% ext()

        tst <- herm_city_shape %>%
            project(city_map) 

        new_ext <- xt * 3
        
        tst %>%
            plot(
                col=NA, 
                border='black',
                xlim=c(xmin(new_ext), xmax(new_ext)),
                ylim=c(ymin(new_ext), ymax(new_ext))
            )
    }

    plot(log10(next_pop_map), add=(city == "Hermosillo"), col=map.pal('viridis'), alpha=1)
    plot(as.polygons(diff), add=T, border='red', lwd=2, legend=F, axes = F, alpha=0)
    plot(city_map, add=T, axes=F)
    title(paste0(city, ", ", year, "-", year2))
    mtext(paste0(
                "Tiles: ", base_tiles, " - ", next_tiles, " (", tile_growth, "%)", "\n",
                "Urban Population: ", round(base_pop/1e6, 2), "M - ", round(next_pop/1e6, 2), "M (", pop_growth, "%)\n",
                "Density: ", round(density1, 2), " - ", round(density2, 2), " (", density_growth, "%)"

    ), side=1)

    if(save) {
        dev.off()
    }
}

mex_map %>%
    select(GID_2, NAME_2, TYPE_2, ENGTYPE_2, CC_2, HASC_2) %>%
    arrange(GID_2) %>%
    filter(NAME_2 == "Hermosillo") %>%
    head(12)

render_city2('Hermosillo', '1975', '2020', T)
render_city2('Saltillo', '1975', '2020', T)
render_city2('Tijuana', '1975', '2020', TRUE)
render_city2('Aguascalientes', '1975', '2020', TRUE)
render_city2("Querétaro", '1975', '2020', TRUE)

# -----

render_city("Hermosillo", '1975', TRUE)
render_city("Hermosillo", '2020', F)

render_city('Saltillo', '1975', TRUE)
render_city('Saltillo', '2020', TRUE)

render_city('Tijuana', '1975', TRUE)
render_city('Tijuana', '2020', TRUE)

render_city('Aguascalientes', '1975', TRUE)
render_city('Aguascalientes', '2020', TRUE)

render_city("Querétaro", '1975', TRUE)
render_city("Querétaro", '2020', TRUE)


# ok lets make a scatter.
# we always look between 2 years...
# we want to see density before, density after
# tiles before, tiles after
# pop before, pop after. 
# this function will construct the dataframe given a list of cities
# and start vs end year. 

construct_dataframe <- function(cities) {

    df <- tibble(
                  city = "",
                  year = '',
                  tiles = as.numeric(),
                  pop = as.numeric())

    for(city in cities) {

        city_map <- mex_map %>% filter(NAME_2 == city)

        for(year in smod_years) {

            base_map <- cleaned_rasters[[year]] %>%
                crop(city_map) %>%
                mask(city_map)

            base_map[!is.na(base_map)] <- 1
            base_map[is.na(base_map)] <- 0

            base_pop_map <- pop_raster[[year]] %>%
                crop(base_map) %>%
                mask(base_map) 

            base_pop_map[base_map == 0 | is.na(base_map)] <- NA

            base_tiles <- sum(values(base_map), na.rm=T)
            base_pop = sum(values(base_pop_map), na.rm=T)

            # create row with city, year, base_tiles, base_pop
            # then add it to df
            row <- tibble(city = city, year = year, tiles = base_tiles, pop = base_pop)

            df <- bind_rows(df, row)

        }
    }

    return(df)

}

res <- construct_dataframe(c("Hermosillo", "Aguascalientes", "Saltillo", "Tijuana", "Querétaro"))

res <- res %>%
    mutate(
           density = pop / tiles,
           year = as.numeric(year)
    )

res %>%
    group_by(city) %>%
    arrange(year) %>%
    mutate(
           pop_growth = (pop - lag(pop)) / lag(pop),
           pop_cagr = (pop / lag(pop)) ^ (1 / (2020-1975)) - 1,
           density_growth = (density - lag(density)) / lag(density)
    ) %>%
    ungroup() %>%
    ggplot(aes(x=pop_growth, y=density_growth, label=city)) +
    geom_point() +
    geom_text_repel() +
    labs(
         x = "Population Growth",
         y = "Density Growth",
         title = "Population vs Density Growth",
         subtitle = "1975 - 2020",
         caption = "Source: Own Calculations from Global Human Settlement Layer Data"
    ) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent)

ggsave("imgs/pop-density-growth-scatter.png", height=6, width=8)

res %>%
    ggplot(aes(x=year, y=density, color=city)) +
    geom_line() +
    geom_point() +
    geom_line(data = . %>% filter(city == "Hermosillo"), color="red", size=2) +
    geom_point(data = . %>% filter(city == "Hermosillo"), color="red", size=2) +
    labs(
         title = "Density over Time",
         y = "People per Square KM",
         x = "Year",
         caption = "Source: Own Calculatiosn from Global Human SEttlement Layer Data"
    )

ggsave('imgs/pop-density-time.png', height=6, width=8)


# plot(log10(ag_gdp_sonora))
png("imgs/hermosillo-2020.png")
plot(mask(crop(cleaned_rasters[['2020']], hermosillo_map), hermosillo_map), add=F, col="blue", legend=F)
plot(hermosillo_map, add=T)
title("Hermosillo, 2020")
dev.off()


# loop and make pics
for(year in smod_years) {
    png(paste0("imgs/sonora-", year, ".png"))
    plot(mask(crop(cleaned_rasters[[year]], sonora_map), sonora_map))
    plot(sonora_map, add=T)
    plot(filter(sonora_map, NAME_2 == "Hermosillo"), add=T, border="blue")
    title(paste0("Sonora, ", year))
    dev.off()
}

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addRasterImage(log10(clamp(gdp_mask, upper=10000000)), colors="viridis", opacity=0.5) %>%
    addRasterImage(cleaned_rasters[["2020"]], colors="red", opacity=0.5) 

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addRasterImage(mask(crop(cleaned_rasters[['1975']], hermosillo_map), hermosillo_map), colors="red", opacity=0.5) 

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addRasterImage(mask(crop(cleaned_rasters[['2020']], hermosillo_map), hermosillo_map), colors="red", opacity=0.5) 


# show sonora / hermosillos agri 

# TODO: plot rivers
png("imgs/sonora-hermosillo-ag-city-2010.png")
ag_gdp_sonora %>%
    clamp(upper=4e9, value=FALSE) %>%
    log10() %>%
    plot()

cleaned_rasters[["2010"]] %>%
    crop(sonora_map) %>%
    mask(sonora_map) %>%
    plot(col="blue", add=T, legend=F)

plot(sonora_map, add=T)
title("Sonora, 2010")
dev.off()

#plot(mexico_map_og)

mexico_map_og %>%
    aggregate(by="NAME_1") %>%
    plot()

v <- vect("data/conagua/Mapa_que_muestra_ubicacion_de_cuerpos_de_agua/C_A_Etapa_I_a_VI_2016actual.shp")
plot(v)

