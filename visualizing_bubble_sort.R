library(hrbrthemes)
library(tidyverse)
library(magrittr)
library(png)
library(magick)

id <- seq(1,52)
width <- 0.8
magnitude <- seq(1,52)
height <- 1.9
card_value <- rep(c("A",seq(2,10),"J","Q","K"),4)
card_suit <- c("♣","♣","♣","♣","♣","♣","♣","♣","♣","♣","♣","♣","♣","♠","♠","♠","♠","♠","♠","♠","♠","♠","♠","♠","♠","♠","♦","♦","♦","♦","♦","♦","♦","♦","♦","♦","♦","♦","♦","♥","♥","♥","♥","♥","♥","♥","♥","♥","♥","♥","♥","♥")
card_color <- rep(c("black","red"),each=26)
deck_properties=data.frame(id=id,magnitude=magnitude,card_value=card_value,card_suit=card_suit)


plot_deck <- function(deck_properties,current_order,comparison_count) {
  current_deck_order <- left_join(current_order %>% as.data.frame() %>% select(current_order=1),deck_properties,by=c("current_order"="id"))
  current_deck_order %<>% mutate(current_order=row_number())
  a<-ggplot() + 
    geom_rect(data=current_deck_order, mapping=aes(xmin=current_order, xmax=current_order+width, ymin=magnitude, ymax=magnitude+height), color="gray20", fill="white",size=0.2)+
    geom_text(data=current_deck_order, aes(x=current_order+0.41,y=magnitude+0.65,label=card_suit,color=card_color),size=4)+
    geom_text(data=current_deck_order, aes(x=current_order+0.41,y=magnitude+1.35,label=card_value,color=card_color),size=3)+
    scale_color_identity()+
    labs(x="",y="",title=paste("Bubble Sort. Iteration:",comparison_count),caption="github.com/Rorevilla")+
    theme_ipsum_rc()+
    theme(plot.caption =element_text(size=16,color="gray40"))
  
  if (TRUE) {
    ggsave(paste(comparison_count,".png",sep=""),width = 7,height = 4.5)
  }
}

bubble_sort <- function(x,deck_properties){
  comparison_count = 0 
  n<-length(x)
  for(j in 1:(n-1)){
    comparison_count= comparison_count + 1
    plot_deck(deck_properties,x,comparison_count)
    for(i in 1:(n-j)){
      if(x[i]>x[i+1]){
        temp<-x[i]
        x[i]<-x[i+1]
        x[i+1]<-temp
      }
    }
  }
  print(comparison_count)
  return(x)
}

initial_order = sample(1:52, 52, replace=F)
res<-bubble_sort(initial_order,deck_properties)

for (i in seq(51)) {
  if (i==1) {
    file_list=paste("C:/Users/rodrigo.revilla/Documents/Bubble_sort/",i,".png",sep="")
  } else {
  file_list<-c(file_list,paste("C:/Users/rodrigo.revilla/Documents/Bubble_sort/",i,".png",sep=""))
  }
}


file_list %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("FileName.gif") # write to current di
