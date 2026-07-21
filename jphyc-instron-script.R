# Load packages:
library(ggplot2)	

# Load data:
data <- read.csv("jphyc-instron-data.csv", header = TRUE, sep = ",")

# Calculate tearing strength (N/mm)
data$tearing_strength <- data$maximum_load/data$thickness_mm

data$test_lod <- as.factor(data$test_lod)

levels(data$test_lod) <- c("Absent", "Present")

###### Analysis
t.test(tearing_strength ~ test_lod, data = data, var.equal = TRUE, alternative = "greater")

# Standard Error Calculations
by(data$tearing_strength, data$test_lod, function(x) {
  c(
    mean = mean(x),
    sd   = sd(x),
    se   = sd(x) / sqrt(length(x))
  )
})

#### Figure 6
p <- ggplot(data, aes(x = test_lod, y = tearing_strength)) +
    geom_boxplot(outlier.shape = NA) +
geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 2) +
	labs(x="Line of dehiscence", y = "Tearing strength (N/mm)") +
  theme_bw()	+
  theme(	panel.grid.major 		= element_blank(),
         panel.grid.minor 		= element_blank(),
         axis.line 			= element_line(colour = "black"),
         axis.title 			= element_text(size=16),
         axis.text = element_text(size=12))+
  annotate("text", x = Inf, y = Inf, label = "p = 0.009",
           hjust = 1.1, vjust = 1.5, size = 5)
p

#ggsave
	w <- 3
	h <- 4
	dpi <- 300
	ggsave("Figure 6.tiff",
	device = "tiff",	width = w, height = h, dpi = dpi, compression = "lzw")





