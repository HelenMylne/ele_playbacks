#library(tidyverse); library(LaplacesDemon) ; library(patchwork); library(ggridges)
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')
library(ggridges, lib.loc = '../../packages/')

theme_set(theme_bw())

#### movement binomial  ####
age_contrast_long <- read_csv('../data_processed/movement_binomial_agecontrasts_check.csv')
as.data.frame(head(age_contrast_long))
plot_contrasts <- age_contrast_long %>%
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                f_age_num_org + 1)) %>%
  mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              '26-35 yrs'))),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt))

unique(plot_contrasts$comparison)

plot_contrasts %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 yrs to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mbm_noprev_contrastridges_splitbybda_new.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1600, width = 1900, unit = 'px')

print('movement binomial complete')
dev.off()

