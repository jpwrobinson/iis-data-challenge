source('clean.R')
library(ggthemes)
library(ggchicklet)
library(waffle)

theme_set(theme_tufte())
th<-theme(legend.position = 'none',
          # axis.line = element_line(colour='black'),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "#ffffe5", color='#ffffe5'))

# pal
pal<-c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#762a83','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')

cols<-c('electricity_from_fossil_fuels_t_wh' = pal[2],
        'electricity_from_nuclear_t_wh' = pal[6],
        'electricity_from_renewables_t_wh' = pal[10],
        'geo_biomass_other_t_wh' = pal[8],
        'hydro_generation_t_wh' = pal[9],
        'solar_generation_t_wh' = pal[10],
        'wind_generation_t_wh' = pal[11])

## labels
ren<-ren %>% filter(year >= min(ff$year))

labber <- ff %>% filter(year == 2021) %>% 
    mutate(lab = recode(source, 
                           electricity_from_fossil_fuels_t_wh = 'Fossil fuels',
                           electricity_from_nuclear_t_wh = 'Nuclear',
                           electricity_from_renewables_t_wh = 'Renewables'),
           year = 2024)

labber2 <- ren %>% filter(year  %in% c(2010, 2021)) %>% 
    mutate(lab = recode(source, 
                           geo_biomass_other_t_wh = 'Other',
                           solar_generation_t_wh = 'Solar',
                           wind_generation_t_wh = 'Wind',
                           hydro_generation_t_wh = 'Hydro'),
           label_ypos=round(label_ypos,-1))


# 1.  total energy
g1<-ggplot(ff %>% filter(year >= 2010), aes(year, energy, fill = source)) +
    geom_chicklet(width=1.1, alpha=0.7,col='#ffffe5') +
    geom_line(aes(year, cumsum, col = source), size=1.5) +
    coord_flip() +
    labs(x = '', y = '') +
    geom_text(data = labber, aes(year,  label_ypos, label = lab, col = source)) +
    scale_x_continuous(expand=c(0,0), breaks=seq(2000, 2021, by=1), limits=c(1999, 2022)) +
    scale_y_continuous(expand=c(0,0), labels=scales::number_format(suffix = " Twh",big.mark = ","), 
                       breaks=seq(5000, 25000, 5000)) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    th  +
    theme(axis.text.y = element_text(hjust=0.5),
          panel.grid.major.x = element_line(colour='grey'))

# 2. renewable subset
g2<-ggplot(ren %>% filter(year %in% c(2010,2021)), aes(year, energy, fill=fct_reorder(source, energy))) + 
    geom_chicklet(col='#ffffe5') +
    coord_flip() +
    labs(x = '', y = '') +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    geom_text(data = labber2,size=3, aes(year,  label_ypos, label = lab), colour='white', fontface=2) +
    scale_x_reverse(expand=c(0,0)) +
    # scale_y_continuous(labels=scales::comma,
    #                    position = 'right',
    #                    breaks=labber2$label_ypos) +
    th +
    theme(axis.text = element_blank())

## 3. subsidies
ren_subs<-data.frame(year = 2017, fuel = 'renewable', billion_usd = 128)

subs2<-rbind(subs %>%
    filter(year==2017 & fuel != 'total'), ren_subs) %>% 
    dplyr::count(fuel, wt = billion_usd) 

g3<-waffle(subs2$n, size=.5, rows=5,
           colors = c(pal[1], pal[2],pal[3],pal[4], pal[10]),
           legend_pos = 'none') +
    theme(rect=element_rect(fill='#ffffe5',
                              color='#ffffe5'),
            plot.background=element_rect(fill='#ffffe5'),
            strip.background = element_rect(colour=NA, fill=NA),
            panel.background=element_rect(fill='#ffffe5', color='#ffffe5'))

g3$layers[[1]]$aes_params$colour <- '#ffffe5'

# subsidies per energy generation
fsubs<-sum(subs2$n[!subs2$fuel == 'renewable'])
rsubs<-sum(subs2$n[subs2$fuel == 'renewable'])

fenergy<-ff %>% filter(year==2017 & source =='electricity_from_fossil_fuels_t_wh') %>% pull(energy)
renergy<-ff %>% filter(year==2017 & source =='electricity_from_renewables_t_wh') %>% pull(energy)

fenergy/fsubs
renergy/rsubs

## 4. multipan
pdf(file = 'fig.pdf', height=6, width=10)
plot_grid(g2, g1, g3, nrow =3, rel_heights=c(0.4, 1, .4))
dev.off()

# data from
# Our World in Data
# https://fossilfuelsubsidytracker.org/
# IRENA 2020 Energy subsidies: Evolution in the global energy transformation to 2050

# ggplot(subs2, aes( values=n, fill=fuel)) +
#     geom_waffle(colour='white') +
#     coord_equal() +
#     scale_x_discrete(expand=c(0,0)) +
#     scale_y_discrete(expand=c(0,0)) +
#     ggthemes::scale_fill_tableau(name=NULL) +
#     theme_ipsum_rc(grid="")
#     # waffle::theme_enhance_waffle()



## summary stats
## FF = 732 Bn USD in 2020
## FF = 518 Bn USD in 2017
subs$billion_usd[subs$year==2017 & subs$fuel=='total'] 
subs2
ren %>% filter(year %in% c(2000,2021)) %>% 
    group_by(year) %>% summarise(sum(energy))
(7931 - 2864) / 2864 * 100

ff %>% filter(year %in% c(2000,2021) & source == 'electricity_from_fossil_fuels_t_wh') %>% 
    group_by(year) %>% summarise(sum(energy))
(17189 - 9604) / 9604 * 100

## renew as prop of total in 2021
7856 / 27783 * 100

## fossil as prop of total in 2021
17189 / 27783 * 100

## FF as prop of total subsidies
518 / (518 + 128) * 100
