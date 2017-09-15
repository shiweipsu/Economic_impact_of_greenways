library(sf)

block <- st_read("Data/Portland_blocks.shp")
colnames(block)[23] <- "dis_bk_16"

# disaggregated visulization, omit distance greater than 2 miles
ggplot(melt( filter(block, dis_bk_16<10560)[,c("dis_bk_16","dis_bk_grw")]), aes(x=variable, y=value)) + 
  geom_boxplot() + 
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Boxplot of Bike Accessibility Comparison") +
  labs(x="Bike Facility Network Scenarios", y="Distance to Nearest Bike Facilities")

ggplot(melt(filter(block, dis_bk_16<10560)[,c("dis_bk_16","dis_bk_grw")]), aes(value, fill = variable),binwidth = 50) + 
  geom_histogram(position = "dodge") +
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank(),legend.position = "bottom") +
  ggtitle("Histogram of Bike Accessibility Comparison") +
  labs(x="Distance to Nearest Bike Facilities", y="Number of Blocks") +
  scale_fill_manual(values=c("yellow3","green3"),labels=c("Existing Bike Facilities in 2016", "Proposed Greenways in 2035"))

# disaggregated summary
summary(block$dis_bk_16)
summary(block$dis_bk_grw)
