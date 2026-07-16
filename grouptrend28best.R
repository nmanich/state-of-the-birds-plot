# Nicholas M. Anich
# 7-15-26
# "State of the Birds" style graphic plotting trends by atlas period
# Inspired by this Birds Canada report DOI:10.71842/8bab-ks08

library(ggplot2)
library(ggimage)
library(rcartocolor)
library(ggrepel)
library(systemfonts)
library(Cairo)
library(magick)

# This csv file contains values for the groups, the trends, and the icons
# Using icons from phylopic.org for endpoint dots, svgs are cleaner but pngs work ok
df2 <- read.csv("grouptrenddata23.csv",stringsAsFactors = F,check.names = F, encoding = "UTF-8")

# Conversion of the asterisk character (optional) causes problems in Scala Sans
df2$LabelName <- iconv(df2$LabelName, from = "Windows-1252", to = "UTF-8")
df2$LabelName <- gsub("\x86", "\u002a", df2$LabelName, useBytes = TRUE)

# Clean halos off png files
df2$Image <- sapply(df2$Image, function(img_path) {
  # check if file exists
  if (!file.exists(img_path)) return(img_path)
  # ignore svg files
  if (grepl("\\.svg$", img_path, ignore.case = TRUE)) {
    return(img_path)
  }
  # Clean pngs - load, strip hidden black borders, save a clean copy
  img <- image_read(img_path)
  img <- image_background(img, "transparent")
  clean_path <- tempfile(fileext = ".png")
  image_write(img, clean_path)
  return(clean_path)
})

# Year labels for ticks
labels <- data.frame(  x = c(1995, 2000, 2005, 2010, 2015, 2019),
  label = c("1995", "2000", "2005", "2010", "2015", "2019"))

# Custom atlas labels near horizontal axis
axis_bars <- data.frame(
  xmin  = c(1995, 2015),
  xmax  = c(2000, 2019),
  ymin  = c(-49, -49),
  ymax  = c(-46, -46),
  label = c("First\nAtlas", "Second\nAtlas") 
)

# Main ggplot call
p<-ggplot(df2, aes(x=Year, y=Index, group=Group, label = LabelName)) +
# Optional short bars near bottom of axis to identify atlas period, turned off for now
#  geom_rect(data = axis_bars, 
#            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
#            fill = "#e3d578", 
#            inherit.aes = FALSE) + 
  geom_text(data = axis_bars,
            aes(x = (xmin + xmax) / 2, 
                y = -46.5,               
                label = label),
            family = "ScalaSans",
            size = 3,
            vjust = 1,                 
            color = "black",
            inherit.aes = FALSE) +
  geom_line(aes(color=Group), show.legend = F)+
  geom_image(aes(image=Image), color=df2$Color) +
   scale_color_manual(values=c(
    "#F28C28",
    "#CC6677",
    "#117733",
    "#888888",
    "#332288",
    "#AA4499",
    "#52BCA3",
    "#999933",
    "#882255",
    "#661100",
    "#6699CC",
    "black")) +
  # xlim in the next line keeps text labels lined up
  geom_text_repel(hjust = -.2, segment.color = NA, xlim = c(2020, 2024), nudge_x = 0.1, color=df2$Color,family = "ScalaSans") +
    scale_x_continuous(limits = c(1995, 2032),
                       breaks = labels$x,
                       labels = labels$label,
                       minor_breaks = NULL) +
    scale_y_continuous(limits = c(-49, 22),
                       breaks = c(-40, -30, -20, -10, 0, 10, 20),
                       expand = c(0, 0),
                       labels = function(x) {
                       gsub("-", "\u2013", as.character(x)) } ) + #this is an en dash because minus wasn't working in scala sans
    labs(y= "Change Index", x = "Year") +
    coord_cartesian(clip = "off") + 
    theme_classic(base_family = "ScalaSans", base_size = 16) #have to use base when editing a theme font
  
# Close device
while (!is.null(dev.list())) dev.off()

# Now using cairo to print
gc()

#Name the output file
CairoPDF(
  file = "plot78gg.pdf",
  width = 6.3,
  height = 9.36)

print(p)

dev.off()