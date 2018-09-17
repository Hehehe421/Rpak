plot_stackbar <- function(df, xvalues, yvalue, group){
  
  p = ggplot(df, aes_string(x = xvalues, y = yvalue, fill = group)) 
  p = p + geom_bar(position = 'fill', stat = "identity", width = .7)
  p = p + theme()
  p = p + xlab("") + ylab("")
  p = p + theme_bw()
  p = p + guides(fill=guide_legend(""))
  
  #Comment below if want to have a static plot
  return (p)
}

plot_stackbarh <- function(df, xvalues, yvalue, group){
  
  p = ggplot(df, aes_string(x = xvalues, y = yvalue, fill = group)) 
  p = p + geom_bar(position = 'fill', stat = "identity", width = .7)
  p = p + coord_flip()
  p = p + theme()
  p = p + xlab('') + ylab('')
  p = p + theme_bw()
  p = p + theme(legend.position = "bottom",axis.text.x=element_text(angle = 45, hjust = 1, vjust = 0.5, colour = 'black'),
                axis.text.y = element_blank())
  p = p + guides(fill=guide_legend(" "))
  
  #Comment below if want to have a static plot
  return (p)
}
# plot_stackbarlabel <- function(df, xvalues, yvalue, group,label){
#   
#   p = ggplot(df, aes_string(x = xvalues, y = yvalue, fill = group)) 
#   p = p + geom_bar(position = 'fill', stat = "identity", width = .7)
#   p = p + geom_text(aes_string(label = label), position = position_fill(vjust = 0.5), size = 3)
#   p = p + theme()
#   p = p + xlab("") + ylab("")
#   p = p + theme_bw()
#   p = p + guides(fill=guide_legend(""))
#   
#   #Comment below if want to have a static plot
#   return (p)
# }

plotly_donut<- function(df, xvalue, yvalue, hole, ...){
  p <- df %>%
    plot_ly(labels = df[[xvalue]], values = df[[yvalue]]) %>%
    add_pie(hole = hole) %>%
    layout(showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
}

