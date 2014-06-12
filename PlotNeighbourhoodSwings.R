
library(dplyr)
library(ggplot2)

source('BarryFilter.R')


booths1993 = tbl_df(read.csv('electionmaps/booths_data/1993.csv')) %>% barryFilter()
booths1996 = tbl_df(read.csv('electionmaps/booths_data/1996.csv')) %>% barryFilter()
booths1998 = tbl_df(read.csv('electionmaps/booths_data/1998.csv')) %>% barryFilter()
booths2001 = tbl_df(read.csv('electionmaps/booths_data/2001.csv')) %>% barryFilter()
booths2004 = tbl_df(read.csv('electionmaps/booths_data/2004.csv')) %>% barryFilter()
booths2007 = tbl_df(read.csv('electionmaps/booths_data/2007.csv')) %>% barryFilter()
booths2010 = tbl_df(read.csv('electionmaps/booths_data/2010.csv')) %>% barryFilter()
booths2013 = tbl_df(read.csv('electionmaps/booths_data/2013.csv'))  %>% barryFilter()



nswSwing1996 = -6.94

nswBooths1996 = inner_join(booths1993 %>% filter(State=='NSW'),
                           booths1996 %>% filter(State=='NSW'),
                                by='Booth') %>%
                mutate(alp93 = ALP_2PP.x, alp96 = ALP_2PP.y)
ggplot(nswBooths1996) + aes(x=alp93,y=alp96) + geom_point(aes(alpha=Formal_votes.y)) +
  geom_smooth(size=1) + geom_abline(size=1, colour='goldenrod') +
  geom_abline(intercept=nswSwing1996, colour='darkgreen') +
  scale_alpha_continuous(guide=FALSE) + xlab('ALP 2pp (1993)') + ylab('ALP 2pp (1996)') +
  scale_x_continuous(limits=c(20,80)) + scale_y_continuous(limits=c(20,80)) +
  ggtitle('NSW, 1996 Election')
ggsave('nsw_booths_1996.png')


vicSwing1998 = 3.23
vicBooths1998 = inner_join(booths1996 %>% filter(State=='VIC'),
                           booths1998 %>% filter(State=='VIC'),
                           by='Booth') %>%
  mutate(alp96 = ALP_2PP.x, alp98 = ALP_2PP.y)
ggplot(vicBooths1998) + aes(x=alp96,y=alp98) + geom_point(aes(alpha=Formal_votes.y)) +
  geom_smooth(size=1) + geom_abline(size=1, colour='goldenrod') +
  geom_abline(intercept=vicSwing1998, colour='darkgreen') +
  scale_alpha_continuous(guide=FALSE) + xlab('ALP 2pp (1993)') + ylab('ALP 2pp (1996)') +
  scale_x_continuous(limits=c(20,80)) + scale_y_continuous(limits=c(20,80)) +
  ggtitle('VIC, 1998 Election')
ggsave('vic_booths_1998.png')


saSwing2007 = 6.76
saBooths2007 = inner_join(booths2004 %>% filter(State=='SA'),
                           booths2007 %>% filter(State=='SA'),
                           by='Booth') %>%
  mutate(alp04 = ALP_2PP.x, alp07 = ALP_2PP.y)
ggplot(saBooths2007) + aes(x=alp04,y=alp07) + geom_point(aes(alpha=Formal_votes.y)) +
  geom_smooth(size=1) + geom_abline(size=1, colour='goldenrod') +
  geom_abline(intercept=saSwing2007, colour='darkgreen') +
  scale_alpha_continuous(guide=FALSE) + xlab('ALP 2pp (2004)') + ylab('ALP 2pp (2007)') +
  scale_x_continuous(limits=c(20,80)) + scale_y_continuous(limits=c(20,80)) +
  ggtitle('SA, 2007 Election')
ggsave('sa_booths_2007.png')

qldSwing2007 = 7.53
qldBooths2007 = inner_join(booths2004 %>% filter(State=='QLD'),
                          booths2007 %>% filter(State=='QLD'),
                          by='Booth') %>%
  mutate(alp04 = ALP_2PP.x, alp07 = ALP_2PP.y)
ggplot(qldBooths2007) + aes(x=alp04,y=alp07) + geom_point(aes(alpha=Formal_votes.y)) +
  geom_smooth(size=1) + geom_abline(size=1, colour='goldenrod') +
  geom_abline(intercept=qldSwing2007, colour='darkgreen') +
  scale_alpha_continuous(guide=FALSE) + xlab('ALP 2pp (2004)') + ylab('ALP 2pp (2007)') +
  scale_x_continuous(limits=c(20,80)) + scale_y_continuous(limits=c(20,80)) +
  ggtitle('Qld, 2007 Election')
ggsave('qld_booths_2007.png')


