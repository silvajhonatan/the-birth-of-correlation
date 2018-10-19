library("HistData")
library("dplyr")
library("ggplot2")

data("GaltonFamilies")

# First sons
galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == 'male') %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

# Distributions
galton_heights %>% ggplot(aes(son)) + 
  geom_histogram(binwidth = 1,color='black',fill='#6697e8') + 
  xlab('Sons height')  + ggtitle('Distribution of Galton\'s family height') +
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figs/sons_height.png')

galton_heights %>% ggplot(aes(son)) + 
  geom_histogram(binwidth = 1,color='black',fill='#334d77') + 
  xlab('Fathers height') + ggtitle('Distribution of Galton\'s family height') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figs/fathers_height.png')

# Mean of sons heights
mean(galton_heights$son)

# Number of father's with 72 inches 
galton_heights %>% filter(father == 72) %>% nrow
galton_heights %>% filter(round(father) == 72) %>% nrow

galton_heights %>% filter(round(father) == 72) %>% ggplot(aes(son)) + 
  geom_histogram(color='black',fill='#334d77') + 
  xlab('Sons height') + ggtitle('Conditional distribution, X = 72') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figs/conditional_72fathers_height.png')

galton_heights %>% filter(round(father) == 72) %>% summarize(avg=mean(son))

galton_heights %>% mutate(father_strata= factor(round(father)))  %>%
  ggplot(aes(father_strata,son)) + geom_boxplot() + geom_point() + 
  xlab('Fathers height') + ylab('Sons height') + ggtitle('Looking at all groups') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figs/father_strata.png')

galton_heights %>% mutate(father = round(father))  %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father,son_conditional_avg)) + geom_point()+ 
  xlab('Fathers height') + ylab('Sons height') + ggtitle('Scatter of all groups') + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave('figs/father_linear.png')
