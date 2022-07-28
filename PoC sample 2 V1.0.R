library(dplyr)
library(ggplot2)
# parameters
nv=0.075
dv=0.175
beta_a=0.0811
beta_b=1

df <- data.frame(r=rep(NA,400),
                 n=rep(NA,400),
                 significance=rep(NA,400),
                 postmedian=rep(NA,400),
                 justmedfl=rep(NA,400),
                 bothfl=rep(NA,400))

row <- 1

for (r in 1:10){
  for (n in 1:40){
    if (r>n) {
      next
    }
    rand <- rbeta(100000,beta_a+r,beta_b+n-r)
    sig <- mean(rand>=nv)
    med <- median(rand)
    df[row,1]=r
    df[row,2]=n
    df[row,3]=sig
    df[row,4]=med
    if (sig<0.95 & med>=0.175){df[row,5]=TRUE}
    if (sig>=0.95 & med>=0.175){df[row,6]=TRUE}
    row <- row+1
  }
}

# if there is at least one missing data in df then delete the row.
df_cc <- filter(df,justmedfl==TRUE | bothfl==TRUE)
df_cc['delete'] <- FALSE
# find the minumum sample size by visualization
len_rows <- dim(df_cc)[1]
for (row in (1:len_rows)){
  if ((!(is.na(df_cc[row,'justmedfl'])) & is.na(df_cc[row,'bothfl']))){
    nextn <- df_cc[row,'n']
    maxr <- df_cc[row,'r']
  }
  if (!is.na(df_cc[row,'bothfl']) & df_cc[row,'n']>nextn){
    nextn <- df_cc[row,'n']
    maxr <- df_cc[row,'r']
  }
  if (df_cc[row,'r']>maxr){
    if (df_cc[row,'n']<=nextn){
      df_cc[row,'delete'] <- TRUE
    }
  }
}

# plot
df_cc_plot <- filter(df_cc,delete==FALSE)
df_cc_plot$plotsigfl <- ifelse(is.na(df_cc_plot$bothfl),FALSE,TRUE)
p <- ggplot(data=df_cc_plot, mapping = aes(x=n,y=significance,shape=plotsigfl)) +
  geom_point() + 
  geom_hline(yintercept=0.95, linetype="dashed", color = "red") +
  geom_vline(xintercept=22, linetype="dashed", color = "red") +
  theme_bw()
p