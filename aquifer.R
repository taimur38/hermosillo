library(terra)
library(tidyterra)
library(tidyverse)
library(ggthemes)
library(haven)
library(ggrepel)

# read in the aquifer data
# merge it into a single thing
# save it

theme_set(theme_few())

zones <- read_stata('data/zonamet_expanded.dta')

mex_map <- vect("data/shared-data/geo/gadm/gadm36_2.shp") %>%
    filter(NAME_0 == "Mexico")

herm_map <- mex_map %>% filter(NAME_2 == "Hermosillo")

fnames <- seq(1, 6) %>%
    paste0("data/conagua-shapefiles/aquifer/", . , "/m06_acuiferos_disponibilidadPolygon.shp") 

aqui_2023 <- vect(fnames[1])
aqui_2020 <- vect(fnames[2])
aqui_2018 <- vect(fnames[3])
aqui_2013 <- vect(fnames[4])
aqui_2008 <- vect(fnames[5])
aqui_2003 <- vect(fnames[6])


aqui_combined <- rbind(aqui_2023, aqui_2020, aqui_2018, aqui_2013, aqui_2008, aqui_2003) 

aqui_df <- aqui_combined %>%
    values() %>%
    tibble()

approvals <- vect("data/conagua-shapefiles/m01_vwaprovsbmPoint.shp")
surface_approvals <- vect('data/conagua-shapefiles/m01_vwaprovspmPoint.shp')

rios <- vect("data/conagua-shapefiles/m00_riosprincipalesLine.shp") 

wwtp <- vect("data/conagua-shapefiles/wwtp/m29_vwplantastarmPoint.shp")

mex_states_map <- mex_map %>%
    aggregate(by='NAME_1') 

herm_aquis_map <- aqui_combined %>%
    crop(herm_map) 

herm_aquis <- aqui_combined %>%
    crop(herm_map) %>%
    values() %>%
    tibble()

convert_liters <- function(x) {
    x*1e9/3.154e7
}

write_csv(herm_aquis, "data/hermosillo_aquifers.csv")

ghs_cities <- vect("data/shared-data/geo/ghs/ucdb/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

herm_city_shape <- ghs_cities %>% filter(UC_NM_MN == "Hermosillo")

comparators <- c(
                 "León",
                 "Tijuana", 
                 "Salitillo", 
                 "Saltillo",
                 "Aguascalientes", 
                 "Queretaro", 
                 "Querétaro",
                 "Monterrey", 
                 "Chihuahua",
                 "Mexicali",
                 "Guaymas", 
                 "Reynosa-Rio Bravo", 
                 "Guadalajara", 
                 "Valle de Mexico", 
                 "Hermosillo",
                 "Mexico City"
)

comps_city_map <- ghs_cities %>% 
    filter(grepl("Mexico", XC_NM_LST)) %>%
    arrange(desc(P15)) %>%
    #filter(P15 > 600000)
    filter(UC_NM_MN %in% comparators) %>%
    crop(mex_map)

# recarga total media anual = R = average annual total recharge
# descarga natural comprometida DNC commited natural discharge
# volumen de extraccion de aguas subterraneas VEAS groundwater extraction volume
# disponibilidad media anual (DMA) poistiva positive average annual availability
# disponibilidad media anual (DMA) negativa negative average annual availability
# fecha de publicacion en DOF 

herm_aquis %>%
    filter(anio == 2023) %>%
    arrange(-veas)


herm_aquis %>%
    ggplot(aes(x=anio, y=(vcas), color=acuifero)) +
    geom_line() +
    geom_point()

# a bar showing how much water comes from the aquifer and how much is the excess

herm_aquis %>% filter(anio == 2023) %>%
    select(acuifero, situacion, condicion) %>%
    arrange(situacion)

herm_aquis %>% 
    arrange(-r)

ggplot() +
    geom_spatvector(data = mex_states_map, fill=NA, color='grey') +
    geom_spatvector(data = rios, color='blue', fill=NA) +
    #geom_spatvector_text(data = rios, aes(label=rio), fontface='bold') +
    geom_spatvector(data = herm_city_shape, fill='red', fill=NA) +
    labs(
         title = "Rivers of Mexico",
         caption = "Source: CONAGUA"
    )

ggsave("imgs/mex_rivers.png", width=16, height=9)

herm_aquis %>%
    filter(anio == 2023) %>%
    arrange(-veas) %>%
    mutate(
           share = veas / sum(veas),
           pos = cumsum(share) - share
    ) %>%
    ggplot(aes(ymin = pos, ymax = pos + share, xmin = 0, xmax=convert_liters(r - veas))) +
    geom_rect(aes(fill=situacion), color='grey') +
    geom_text(aes(
                  y=pos + share/2, 
                  x=0, 
                  label=acuifero, 
                  hjust=(ifelse(r-veas > 0, 'right', 'left'))
    )) +
    labs(
         title = "Hermosillo Aquifer Water Balance 2023",
         y = "Share of Groundwater from Aquifer",
         x = "Recharge Rate - Extraction Rate (hm^3)",
         caption = "Source: CONAGUA"
    ) +
    scale_y_continuous(labels = scales::percent_format()) 

ggsave('imgs/herm_aquifer_balance.png', width=16, height=9)

herm_aquis_map %>%
    filter(anio == 2023) %>%
    ggplot() +
    geom_spatvector(aes(fill=convert_liters(veas-r)), color='grey') +
    geom_spatvector(data = herm_city_shape, color='blue', fill='black', alpha=0.5, lwd=0) +
    geom_spatvector_text( aes(label=acuifero), fontface='bold', halo=TRUE) +
    geom_spatvector(data = crop(rios, herm_map), color='blue', fill=NA) +
    scale_fill_distiller(type = 'div', limits=convert_liters(c(-80, 80)), palette = 'RdBu', labels=scales::comma) +
    labs(
        title = "Hermosillo Aquifer Water Balance 2023",
        fill = "Recharge - Extraction\nLiters per second",
        caption = "Source: CONAGUA",
        x = "",
        y = ""
    )


ggsave("imgs/herm_aquifer_balance_map.png", width=12, height=9)

herm_aquis_map %>%
    filter(anio == 2023) %>%
    ggplot() +
    geom_spatvector(aes(fill=(veas-r)/r)) +
    geom_spatvector(data = herm_city_shape, color='black', fill=NA, lwd=1) +
    geom_spatvector_text( aes(label=acuifero), fontface='bold') +
    geom_spatvector(data = crop(rios, herm_map), color='blue', fill=NA) +
    scale_fill_distiller(type = 'div', limits=c(-0.6, 0.6), palette = 'RdBu', labels=scales::comma) +
    labs(
        title = "Hermosillo Aquifer Water Balance 2023",
        fill = "% Extraction beyond Recharge Rate",
        caption = "Source: CONAGUA",
        x = "",
        y = ""
    )


herm_aquis %>%
    filter(acuifero != "Valle de Guaymas") %>%
    group_by(anio) %>%
    summarise(
        total_recharge = sum(r),
        total_extraction = sum(vcas)
    ) %>%
    ggplot(aes(x=anio, y=total_extraction)) +
    geom_line() +
    geom_point() +
    labs(
         title = "Hermosillo Aquifer Water Balance 2003-2023",
         y = "Recharge - Extraction (hm^3)",
         x = "Year",
         caption = "Source: CONAGUA"
    )

ggsave("imgs/herm_aquifer_balance_time.png", width=9, height=6)


herm_aquis %>%
    filter(acuifero != "Valle de Guaymas") %>%
    group_by(anio, acuifero) %>%
    summarise(
        total_recharge = sum(r),
        total_extraction = sum(vcas)
    ) %>%
    ggplot(aes(x=anio, y=total_recharge - total_extraction, color=acuifero)) +
    geom_line() +
    geom_point() +
    labs(
         title = "Hermosillo Aquifer Water Balance 2003-2023",
         y = "Recharge - Extraction (hm^3)",
         x = "Year",
         caption = "Source: CONAGUA"
    )

ggsave("imgs/herm_aquifer_balance_time.png", width=9, height=6)


herm_approvals <- approvals %>%
    crop(herm_map) 

herm_approvals_df <- herm_approvals %>%
    values() %>%
    tibble() 


aquifer_usages <- herm_approvals_df %>%
    group_by(acuifero, grupouso) %>%
    summarise(
              vol = sum(volsb)
    )

usages_merged <- herm_aquis %>%
    filter(anio == 2023) %>%
    left_join(aquifer_usages, by='acuifero') %>%
    mutate(
           vol = vol / 1e5,
           vol_share = vol / r
    )


herm_aquis %>%
    filter(anio == 2023) %>%
    filter(acuifero != "Valle de Guaymas") %>%
    arrange(-veas) %>%
    mutate(
           share = veas / sum(veas),
           pos = cumsum(share) - share
    ) %>%
    left_join(aquifer_usages, by='acuifero') %>%
    group_by(acuifero) %>%
    mutate(
           vol = vol / 1e5,
           vol_share = vol / sum(vol, na.rm=TRUE),

           vol_share_scaled = vol_share * convert_liters(r - veas),
           ypos = cumsum(vol_share_scaled) - vol_share_scaled
    ) %>%
    ungroup() %>%
    arrange(-veas) %>%
    ggplot(aes(ymin=pos, ymax=pos + share, xmin=ypos, xmax=ypos + vol_share_scaled, fill=grupouso)) +
    geom_rect(color='white') +
    geom_text(data = . %>% filter(grupouso == 'Agropecuario'), aes(
                  y=pos + share/2, 
                  x=0, 
                  label=acuifero, 
                  hjust=(ifelse(r-veas > 0, 'right', 'left'))
    ))  +
    labs(
         title = "Hermosillo Aquifer Water Balance 2023",
         subtitle = "Usage Estimates by Group",
         x = "Recharge Rate - Extraction Rate (liters per second)",
         y = "Share of Groundwater from Aquifer",
         caption = "Source: Own Calculations from CONAGUA"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(n.breaks = 10, labels=scales::comma) 

ggsave("imgs/herm_aquifer_balance_usage.png", width=12, height=8)


herm_approvals %>%
    filter(grupouso == 'Industrial Integrado') %>%
    ggplot() +
    geom_spatvector(aes(color=acuifero, size=volsb)) +
    geom_spatvector(data = herm_map, color='black', fill=NA) +
    geom_text_repel(data = . %>% filter(volsb > 1e5), aes(label=titular, y=latitud, x=longitud),
                    max.overlaps = 100) +
    labs(
         title = "Industrial Concessions for Underground water in Hermosillo",
         color = "Aquifer",
         size = "Volume"
    )

ggsave("imgs/herm_industrial_concessions.png", width=12, height=12)


herm_surface_approvals <- surface_approvals %>%
    crop(herm_map)

herm_surface_approvals_df <- herm_surface_approvals %>%
    values() %>%
    tibble()


names(herm_surface_approvals_df)

surface_approvals_df <- surface_approvals %>%
    values() %>%
    tibble()

surface_approvals_df %>%
    filter(grepl('HERMOSILLO', titular)) %>%
    mutate(volsp = volsp / 1e5)

surface_approvals_df %>%
    filter(grepl('COMISION ESTATAL', titular))

# approval id from el novillo: 420883

surface_approvals_df %>%
    filter((municipio == "Hermosillo") | (id == 420883)) %>%
    summarise(tot = sum(volsp)/1e5)

herm_surface_approvals_df %>%
    summarise(tot = sum(volsp)/1e5)

herm_approvals_df %>%
    summarise(tot = sum(volsb)/1e5)

herm_surface_approvals

surface_approvals_df %>%
    filter(nomlocal == 'El Novillo') %>%
    mutate(volsp = volsp / 1e5)


surface_approvals %>%
    filter(uso == "Público urbano") %>%
    ggplot() +
    geom_spatvector(aes(size=volsp)) +
    geom_spatvector(data = herm_map, color='black', fill=NA) +
    geom_text_repel(data = . %>% filter(nomlocal == "El Novillo"), aes(label=titular, y=latitud, x=longitud))


herm_surface_approvals_df %>%
    summarise(
        total = sum(volsp)
    )

herm_approvals_df %>%
    summarise(
        total = sum(volsb)
    )


surface_approvals_df %>%
    filter(municipio == 'Hermosillo') %>%
    summarise(
              total = sum(volsp)
    )

aqui_combined %>%
    filter(anio == 2023) %>%
    filter(r - veas < 7500) %>%
    arrange(-veas) %>%
    mutate(
           share = veas / sum(veas),
           pos = cumsum(share) - share
    ) %>%
    ggplot(aes(ymin = pos, ymax = pos + share, xmin = 0, xmax=convert_liters(r - veas))) +
    geom_rect(aes(fill=situacion), color='grey') +
    geom_text(data = . %>% filter(share > 0.005),
              aes(
                  y=pos + share/2, 
                  x=0, 
                  label=acuifero, 
                  hjust=(ifelse(r-veas > 0, 'right', 'left'))
    )) +
    labs(
         title = "Mexico Aquifer Water Balance 2023",
         y = "Share of Groundwater from Aquifer",
         x = "Recharge Rate - Extraction Rate (hm^3)",
         caption = "Source: CONAGUA"
    ) +
    scale_y_continuous(labels = scales::percent_format()) 


herm_aquis_ref <- unique(herm_aquis$acuifero)

aqui_combined %>%
    filter(anio == 2023) %>%
    filter(r - veas < 7500) %>%
    arrange(-veas) %>%
    mutate(
           share = veas / sum(veas),
           pos = cumsum(share) - share
    ) %>%
    ggplot(aes(ymin = pos, ymax = pos + share, xmin = 0, xmax=(r - veas))) +
    geom_rect(aes(fill=situacion), color='grey') +
    geom_text(data = . %>% filter(share > 0.02 | acuifero %in% herm_aquis_ref),
              aes(
                  y=pos + share/2, 
                  x=0, 
                  label=acuifero, 
                  hjust=(ifelse(r-veas > 0, 'right', 'left'))
    )) +
    labs(
         title = "Mexico Aquifer Water Balance 2023",
         y = "Share of Groundwater from Aquifer",
         x = "Recharge Rate - Extraction Rate (hm^3)",
         caption = "Source: CONAGUA"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(n.breaks = 10) 

ggsave("imgs/mex_aquifer_balance.png", width=16, height=9)


aqui_combined %>%
    filter(anio == 2023) %>%
    filter(r - veas < 7500) %>%
    arrange(-veas) %>%
    mutate(
           share = veas / sum(veas),
           pos = cumsum(share) - share
    ) %>%
    ggplot(aes(y=veas, x=r, color=situacion)) +
    geom_point() +
    geom_abline() +
    geom_text_repel(aes(label=acuifero)) 

mxcty <- ghs_cities %>%
    as.data.frame() %>%
    tibble() %>%
    filter(grepl("Mexico", XC_NM_LST)) %>%
    #filter(CTR_MN_NM == "Mexico") %>%
    arrange(desc(P15))

res <- mxcty %>%
    select(UC_NM_MN) 


aqui_combined %>%
    filter(anio == 2023) %>%
    ggplot() +
    geom_spatvector(aes(fill=convert_liters(r-veas)), color=NA) +
    scale_fill_distiller(
                         type = 'div', 
                         palette = 'RdBu',
                         direction = 1, 
                         limits=c(-16000, 16000),
                         labels=scales::comma
    ) +
    geom_spatvector(data = mex_states_map, color='grey', fill=NA) +
    geom_spatvector(data = comps_city_map, color='black', fill=NA, lwd=0.75) +
    geom_text_repel(data = comps_city_map, 
                    aes(label=UC_NM_MN, y=GCPNT_LAT, x=GCPNT_LON)
    ) +
    geom_spatvector(data = rios, color='blue', alpha=0.3) +
    labs(
        fill = "Recharge - Extraction\nLiters per second",
        title = "Aquifer Water Balance 2023",
        caption = "Source: CONAGUA",
        x = "",
        y = ""
    ) +
    theme_void()

ggsave("imgs/mex_aquifer_balance_map.png", width=16, height=9)


zones %>%
    filter(ZM == "Monterrey") 

nl_map %>%
    filter(NAME_2 %in% filter(zones, ZM == "Monterrey")$nombre_municipio) %>%
    values() %>% tibble()

(nl_map %>%
    filter(NAME_1 == "Nuevo León") %>%
    select(NAME_2))$NAME_2

nl_map <- mex_map %>%
    filter(NAME_1 == "Nuevo León")

    filter((NAME_1 == "Nuevo León") | NAME_1 == "Coahuila")

sal_map <- nl_map %>%
    filter(NAME_2 %in% filter(zones, ZM == "Salitillo")$nombre_municipio) %>%
    aggregate()

mont_map <- nl_map %>%
    filter(NAME_1 =="Nuevo León") %>%
    filter(NAME_2 %in% filter(zones, ZM == "Monterrey")$nombre_municipio) %>%
    aggregate()

aqui_combined %>%
    filter(anio == 2023) %>%
    crop(nl_map) %>%
    ggplot() +
    geom_spatvector(data = nl_map, color='black', lwd=2, fill=NA) +
    geom_spatvector(aes(fill=(r-veas)*1e9/3.154e7), color=NA) +
    #geom_spatvector(data = sal_map, color='red', fill=NA, lwd=1) +
    geom_spatvector(data = mont_map, color='black', fill=NA, lwd=0.5) +
    geom_spatvector(data = crop(rios, nl_map), color='blue', fill=NA) +
    geom_spatvector(data = ghs_cities %>% filter(UC_NM_MN == "Monterrey"), fill=NA, color='red', lwd=1.5) +
    geom_spatvector(data = nl_map %>% filter(NAME_2 == "Santa Catarina"), color='purple', fill=NA, lwd=2) +
    # geom_spatvector(
    #                 data = crop(wwtp, nl_map), 
    #                 aes(
    #                     size=capacidad,
    #                     color = caudal / capacidad
    #                 )
    # )+
    scale_fill_distiller(
                         type = 'div', 
                         palette = 'RdBu',
                         direction = 1, 
                         limits=c(-4000, 4000),
                         labels=scales::comma
    ) +
    scale_color_viridis_c() +
    labs(
         title = "Nuevo León Aquifer Water Balance 2023",
         subtitle = "Location of Tesla Factory in Purple",
         x = "",
         y = "",
         fill = "Recharge - Extraction\nLiters per second",
    )

ggsave("imgs/tesla-nl-aquifer.png", width=9, height=12)


aqui_combined %>%
    filter(anio == 2023) %>%
    filter(r - veas < 1) %>%
    ggplot() +
    geom_spatvector(aes(fill=(r-veas)*1e9/3.154e7 * -1), color=NA ) +
    scale_fill_distiller(type = 'seq', palette="Reds", direction = 1, labels=scales::comma) +
    geom_spatvector(data = mex_states_map, color='grey', fill=NA) +
    geom_spatvector(data = comps_city_map, color='black', fill=NA, lwd=0.5) +
    geom_text_repel(data = comps_city_map, 
                    aes(label=UC_NM_MN, y=GCPNT_LAT, x=GCPNT_LON),
                    bg.color='white'
    ) +
    scale_x_continuous(limits=c(-116.5, -98)) + 
    labs(
        title = "Unsustainably Extracted Aquifers, Mexico 2023",
        fill = "Non-Renewable Extraction\nLiters per second",
        caption = "Source: CONAGUA",
        x = "",
        y = ""
    )

ggsave("imgs/mex_unsustainable_aquifers-zoom.png", width=16, height=9)


aqui_combined %>%
    filter(anio == 2023) %>%
    mutate(net_rate = convert_liters(r - veas) * -1) %>%
    filter(net_rate > 0) %>%
    plot(
         y='net_rate', 
         type='continuous', 
         col=map.pal('reds'), 
         legend=T, 
         border=NA,
         #limits=c(-16000, 16000),
    ) %>%
    polys(x=mex_states_map, border='grey') %>%
    polys(x=comps_city_map, border='black', lwd=2)

text(comps_city_map, comps_city_map$UC_NM_MN, halo=TRUE, cex=0.85)



aqui_combined %>%
    filter(anio == 2023) %>%
    filter(r - veas < 1) %>%
    ggplot() +
    geom_spatvector(aes(fill=(r-veas)/r), color='black') +
    scale_fill_distiller(type = 'seq', palette="Reds", direction = -1, labels=scales::percent) +
    #geom_spatvector(data = herm_map, color='red', fill=NA, lwd=1) +
    geom_spatvector(data = mex_states_map, color='grey', fill=NA) +
    geom_spatvector(data = comps_city_map, color='black', fill=NA, lwd=1) +
    #geom_spatvector_text(data = comps_city_map, aes(label=UC_NM_MN)) +
    geom_label_repel(data = comps_city_map, 
                    aes(label=UC_NM_MN, y=GCPNT_LAT, x=GCPNT_LON)
    ) +
    # geom_spatvector(data = mex_map %>% 
    #                 filter(NAME_2 %in% comparators), 
    #             color='red', fill=NA, lwd=1) +
    # geom_spatvector(data = mex_comps_map, color='red', fill=NA, lwd=1) +
    # geom_spatvector_text(data = mex_comps_map, aes(label=ZM)) +
    labs(
        title = "Unsustainably Extracted Aquifers, Mexico 2023",
        fill = "Recharge - Extraction\nLiters per second",
        caption = "Source: CONAGUA",
        x = "",
        y = ""
    )


aqui_combined %>%
    filter(anio == 2023) %>%
    filter(r - veas < 1) %>%
    ggplot() +
    geom_spatvector(aes(fill=(r-veas)*1e9/3.154e7), color='black') +
    geom_spatvector(data = mex_map, color='grey', fill=NA) +
  #   geom_spatvector_label(data = . %>% filter(r-veas < 340), 
  #                     aes(label=acuifero)
  #   ) +
    scale_fill_distiller(type = 'seq', palette="Reds", direction = -1, labels=scales::comma) +
    labs(
        title = "Unsustainably Extracted Aquifers, Mexico 2023",
        fill = "Recharge - Extraction\nLiters per second",
        caption = "Source: CONAGUA",
        x = "",
        y = ""
    )

ggsave("imgs/mex_unsustainable_aquifers_nolabels.png", width=16, height=9)



# wells...

wells <- vect('data/conagua-shapefiles/piezometric/m06_vwpiezometria.shp')

wells_2000 <- vect('data/conagua-shapefiles/piezometric/2000/m06_vwpiezometria.shp')

herm_wells_2000 <- wells_2000 %>%
    crop(herm_map)

herm_wells <- wells %>%
    crop(herm_map)

herm_wells_df <- herm_wells %>%
    values() %>%
    tibble()

herm_wells_df_2000 <- herm_wells_2000 %>%
    values() %>%
    tibble()

names(herm_wells_df)

herm_wells_df %>%
    filter(numpozo == 16239) %>%
    select(elevbrocal, elevestati, profnivest)

herm_wells %>%
    ggplot() +
    geom_spatvector(data = herm_map, color='black', fill=NA) +
    geom_spatvector(data = herm_city_shape, color='red', fill=NA, lwd=1) +
    geom_spatvector(data = herm_wells_2000, color='red') +
    geom_spatvector(aes(color=profnivest)) 



wells_combined <- herm_wells_df_2000 %>%
    rbind(herm_wells_df)

names(wells_combined)

wells_combined %>%
    filter(anio == 2000) %>%
    select(nompozo, latitud, longitud, elevbrocal, elevestati, profnivest) %>%
    mutate(
           check_sum = elevbrocal - elevestati
    )

wells_combined %>%
    group_by(nompozo) %>%
    mutate(cnt = n()) %>%
    ungroup() %>%
    filter(cnt > 2)

wells_combined %>%
    group_by(nompozo) %>%
    summarise(
              height_2000 = mean(profnivest[anio == 2000]),
              height_2022 = mean(profnivest[anio == 2022]),
              elevestati_2000 = mean(elevestati[anio == 2000]),
              elevestati_2022 = mean(elevestati[anio == 2022]),
              elevebrocal_2000 = mean(elevbrocal[anio == 2000]),
              elevebrocal_2022 = mean(elevbrocal[anio == 2022]),
              lat = mean(latitud),
              lon = mean(longitud)
    ) %>%
    mutate(
           #diff = height_2000 - height_2022
           diff = elevestati_2000 - elevestati_2022
    )  %>%
    filter(!is.na(diff)) %>%
    ggplot() +
    geom_point(aes(x=lon, y=lat, color=diff)) +
    geom_spatvector(data = herm_map, color='black', fill=NA) +
    geom_spatvector(data = herm_city_shape, color='red', fill=NA, lwd=1) +
    scale_color_distiller(type = 'div', palette = 'RdBu', limits=c(-25, 25), labels=scales::comma, direction=-1) 


approvals_df <- approvals %>%
    values() %>%
    tibble()


approvals_df %>%
    mutate(geocode = paste0(clvedo, clvmun)) %>%
    left_join(zones, by='geocode') %>%
    group_by(codeZM, ZM, grupouso) %>%
    summarise(
              total = sum(volsb)
    ) %>%
    mutate(
           total_water = sum(total),
           share = total / sum(total)
    ) %>%
    filter(grupouso == 'Industrial Integrado') %>%
    ggplot(aes(x=total_water, y=total, label=ZM)) +
    geom_point() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red') +
    geom_text_repel() +
    labs(
         title = "Industrial Water Usage vs Total Concessions",
         subtitle = "Subterranean Water, Sonora, 2022",
         x = "Total Underground Water Concession",
         y = "Total Industrial Water Volume (hm^3)",
         caption = "Source: CONAGUA"
    )


surface_approvals_df %>%
    mutate(geocode = paste0(clvedo, clvmun)) %>%
    select(geocode, grupouso, volsp) %>% 
    rename(volume = volsp) %>%
    rbind(
          approvals_df %>% 
              mutate(geocode = paste0(clvedo, clvmun)) %>%
              select(geocode, grupouso, volsb) %>%
              rename(volume = volsb)
    ) %>%
    left_join(zones, by='geocode') %>%
    group_by(codeZM, ZM, grupouso) %>%
    summarise(
              total = sum(volume)
    ) %>%
    mutate(
           total_water = sum(total),
           share = total / sum(total)
    ) %>%
    filter(grupouso == 'Industrial Integrado') %>%
    ggplot(aes(x=total_water, y=total, label=ZM)) +
    geom_point() +
    geom_point(data = . %>% filter(ZM == "Hermosillo"), color='red') +
    geom_text_repel() +
    labs(
         title = "Industrial Water Usage vs Total Concessions",
         subtitle = "Sonora, 2022",
         x = "Total Water Concession",
         y = "Total Industrial Water Volume (hm^3)",
         caption = "Source: CONAGUA"
    )


    # what did i want

    # a list of 

herm_approvals_df %>%
    filter(municipio == "Hermosillo") %>% 
    #filter(acuifero == "Costa de Hermosillo") %>%
    filter(grupouso == 'Industrial Integrado') %>%
    arrange(-volsb) %>%
    select(
           titular, volsb, nomlocal, sciansecto, sciansubra, usuario, acuifero
    ) %>%
    write_csv("data/herm_industrial_concessions.csv")

herm_approvals_df %>%
    select(grupouso) %>% unique()

herm_approvals_df %>%
    filter(municipio == "Hermosillo") %>% 
    #filter(acuifero == "Costa de Hermosillo") %>%
    filter(grupouso == 'Agropecuario') %>%
    arrange(-volsb) %>%
    select(
           titular, volsb, nomlocal, sciansecto, sciansubra, usuario, acuifero
    ) %>%
    write_csv("data/herm_agri_concessions.csv")


ggplot() +
    geom_spatvector(data = herm_map, color='black', fill=NA) +
    geom_spatvector(data = herm_city_shape, color='red', fill=NA) +
    geom_spatvector(
                    data = crop(wwtp, herm_map), 
                    aes(size=capacidad),
                    color='red',
    ) +
    geom_spatvector(
                data = crop(rios, herm_map),
                color='blue',
                fill=NA
    ) 

ggplot() +
    geom_spatvector(data = mex_states_map, color='grey', fill=NA) +
    geom_spatvector(data = comps_city_map, color='black', fill=NA, lwd=2) +
    geom_spatvector(
                    data = wwtp %>%
                        filter(capacidad > 1000) %>%
                        filter(capacidad < 10000), 
                    aes(size=capacidad),
                    color='red',
    ) 

wwtp %>%
    as.data.frame() %>%
    tibble() %>%
    arrange(desc(capacidad)) %>%
    select(municipio, planta, capacidad, proceso, caudal)

    filter(municipio == "Hermosillo") %>%
