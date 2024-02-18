process<-function(x){
  fo <- readImage(x)#Read file
  fo_original<-readImage(x)
  fo_original<-fo_original
  r<-nrow(fo)
  c<-ncol(fo)
  fo<-fo[(round(0.2*r)):(round(0.8*r)), (round(0.025*c)):(round(0.975*c)), ]
  fo_original<-fo_original[round((0.2*r)):round((0.8*r)), round((0.025*c)):round((0.975*c)), ]
  fo<-channel(fo, 'luminance') #Convert to luminance
  fo<-max(fo)-fo #Revert color
  
  i=0
  for (i in seq(0.001, 0.5, 0.001)){
    if(max(fo*(1+i))>0.5){
      break
    } else {i=i+0.001}
  }
  fo<-fo*(1+i) 
  display(fo)
  
  threshold <- thresh(fo, w=0.1*r, h=0.1*c, offset = 0.005/(10*i+1)) 
  display(normalize(threshold))
  l = length(threshold)
  n = l/10
  pixels = sample(l, n)
  img_noisy = threshold
  img_noisy[pixels] = runif(n, min=0, max=1)
  display(img_noisy)
  img_median = medianFilter(img_noisy, 2)
  display(img_median)
  
  fo_lab <- bwlabel(normalize(img_median))
  fo_table <- table(fo_lab)
  cutoff<- sort(fo_table[fo_table!=max(fo_table)], decreasing = T)[3]
  cutoff<-ifelse(cutoff<20, 20, cutoff)
  fo_lab[fo_lab %in% as.numeric(names(fo_table)[fo_table < cutoff])] <- 0 
  #There are also a few stray pixels which have been assigned to their own groups, so we remove anything with fewer than a hundred pixels by writing 0s into fo_lab at these locations
  
  fo_wells <- as.numeric(names(table(fo_lab)))[-1]
  length(fo_wells)
  df <- as.data.frame(computeFeatures.moment(fo_lab))
  df$intensity <- sapply(fo_wells, function(x) mean(fo[fo_lab == x]))
  background<-mean(fo[fo_lab == 0])
  
  df<-df%>%
    filter(m.cy<0.45*c|m.cy>0.55*c)%>%
    filter(m.cy>0.1*c & m.cy<0.9*c)
  
  if (nrow(df)>2){
    df<-df%>%
      arrange(intensity)%>%
      slice(2:3)%>%
      arrange(m.cy)
  } else {
    df<-df%>%arrange(m.cy)
  }

  img_df <- reshape2::melt(as.matrix(as.raster(as.array(fo))))
  img_original<-reshape2::melt(as.matrix(as.raster(as.array(fo_original))))
  
  plot<-
    (ggplot(img_original, aes(Var1, Var2, fill = value)) + 
      geom_raster() +
      scale_fill_identity() +
      scale_y_reverse() +
      coord_equal()+xlab('')+ylab('')) +
    
    (ggplot(img_df, aes(Var1, Var2, fill = value)) + 
       geom_raster() +
       scale_fill_identity() +
       scale_y_reverse() +
       geom_text(inherit.aes = FALSE, data = df, color = "red",
                 aes(x = m.cx, y = m.cy, label = round(intensity, 3))) +
       coord_equal()+xlab('')+ylab(''))
  
  ir<-(df[2, ]$intensity-background)/(df[1, ]$intensity-background)
  intensity_ratio<-if(nrow(df)==1){
    0
  } else if(ir<0){0} else {ir}
  return(list(plot, intensity_ratio, background))
}
