#Code for Bins
tp_edge1=28.802
tp_edge2=37.850
tp_edge3=45.821
bt_edge1=26.302
bt_edge2=35.450
bt_edge3=43.321
lf_edge1=-82.693
lf_edge2=-80.209
lf_edge3=-71.520
rt_edge1=-80.193
rt_edge2=-77.709
rt_edge3=-69.270
longitude<-sp_df$longitude
latitude<-sp_df$latitude
data_table = sp_df
Add_Box<-function(data_table){
  data_table["box"]<-NA
  data_table$box<-
    ifelse(
      longitude<=rt_edge1 & longitude>=lf_edge1 & latitude<=tp_edge1 & latitude>=bt_edge1,1,
      ifelse(
        longitude<=rt_edge2 & longitude>=lf_edge2 & latitude<=tp_edge2 & latitude>=bt_edge2,2,
        ifelse(
          longitude<=rt_edge3 & longitude>=lf_edge3 & latitude<=tp_edge3 & latitude>=bt_edge3,3,0
        )
      )
    )
  sort_datatable<-data_table[,c(1,4,5,6,7)]
  return(sort_datatable)
}
