#
# Timeline
#

# Load data
deaths <- read.csv("all_monarchs_deaths.csv")

event_counts <- table(deaths$Table_Number)

# Sort the counts in descending order
sorted_counts <- sort(event_counts, decreasing = FALSE)
year_cols<-names(sorted_counts)
year_cols<-c("Euthanised", "Accidental death","Unknown", "Executed" ,"Assassinated","Murdered","Killed","Natural Causes" )
# Awards and colors
dyn_values<-unique(deaths$Dynasty)
col_values <- c("#506F0E", "#FFD289",  "#88E8D1", '#FFB0A3', '#5D3FD3','#FF6F61', '#D2B3FF','#FFEE59')
for (i in seq_along(dyn_values)) {
  deaths$colour[deaths$Dynasty == dyn_values[i]] <- col_values[i]
}


# Define your labels and their x-coordinates
labels <- c("Saxons","Normans","Plantagenets","Tudors","Stuarts","Interregnum" ,"", "Hanoverians","Windsors"  )
reigns<-c(800,1066, 1154, 1485,1603,1649,1660,1714,1901)
x_coords <- reigns


par(family = "Arial",  # Change to your preferred font
    las = 1, 
    mar = c(4, 12, 6, 2), 
    bg = '#F7EDE2',
    cex = 0.9,  # Set global font size (adjust as needed)
    col.axis = "gray",  # Set axis text color
    col.lab = "gray")  # Set label color

plot(NA, 
     xlim=c(800, 2023), ylim=c(0,8),
     xlab="Death Year", ylab="",
     yaxt="n", bty="n", type="n")

# Axis and grid
axis(side=2, at=1:8, labels = year_cols,
     tick=FALSE)
abline(v = reigns, lty = 2, col = "gray")

# Calculate the correct top margin
top_margin <- 8  # This should match the upper limit of your ylim

line_extension <- 1  # Increased for visibility
text_offset <- 0.1  # Increased for better positioning

for(i in 1:length(labels)) {
  # Draw inclined dashed line continuation
  segments(x0 = x_coords[i], y0 = top_margin, 
           x1 = x_coords[i], y1 = top_margin + line_extension,
           col = "gray", lty = 2, xpd = TRUE)
  
  
  # Add horizontal text label

  text(x = x_coords[i] + line_extension, 
       y = top_margin + line_extension + text_offset, 
       labels[i], adj = c(0, 0.5), cex = 0.8, xpd = TRUE,srt=50, font = 3, col = "gray")
}

for (j in 1:length(year_cols)) {
  endyrs <- range(deaths[deaths$Table_Number == year_cols[j], 'Year'])
  lines(endyrs, c(j, j),col="gray")
}
for (i in 1:length(year_cols)) {
  years<-deaths[deaths$Table_Number == year_cols[i], 'Year']
  house_colors <-deaths[deaths$Table_Number == year_cols[i], 'colour']
  points(years, rep(i,length(years)), 
         pch=21, cex=1.5,
         col="black",
         bg=house_colors)
}













