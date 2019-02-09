library(ggplot2)
library(gganimate)
library(ggimage)

colors <- c("red","orange","yellow","purple","pink")
color <- colors[1]
xspeed <- 3
yspeed <- 3
data <- data.frame(x=500, y=500, color=color, time=1)
for (i in 2:3000) {
  if (data$x[nrow(data)] + 200 >= 1920 | data$x[nrow(data)] - 200 <= 0) {
    xspeed <- -xspeed
    color <- colors[!colors %in% data$color[nrow(data)]][sample(1:4, 1)]
  }
  if (data$y[nrow(data)] + 50 >= 1080 | data$y[nrow(data)] - 50 <= 0) {
    yspeed <- -yspeed
    color <- colors[!colors %in% data$color[nrow(data)]][sample(1:4, 1)]
  }
  data <- rbind(data,data.frame(x=data$x[nrow(data)] + xspeed,
                                y=data$y[nrow(data)] + yspeed,
                                color=color,
                                time=i))
}

anim <- ggplot(data,aes(x=x,y=y)) + 
  xlim(0,1920) +
  ylim(0,1080) +
  theme_void() +
  theme(legend.position='none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.background = element_rect(fill = "black")) +
  geom_image(aes(image="https://upload.wikimedia.org/wikipedia/commons/7/78/DVD_video_logo.png"), color=data$color, size=0.3) + 
  transition_time(time)

anim <- animate(anim, nframes = 800, fps = 30)
anim_save("dvd.gif", animation = anim)
