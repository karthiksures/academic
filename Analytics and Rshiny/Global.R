
#PLEASE CHANGE THE WORKING DIRECTORY TO THIS FOLDER
setwd("/home/hp/Trivago")
bi_data=read.csv("Case Study - Business Analyst - Advertiser Intelligence (2_2).csv")
bi_data$insert_date=as.Date(lubridate::ymd(bi_data$insert_date))
bi_data$week=week(bi_data$insert_date)
bi_data$month=month(bi_data$insert_date)
bi_data$click_share=bi_data$clicks/sum(bi_data$clicks)
bi_data$cost_share=bi_data$cost/sum(bi_data$cost)

display_data=bi_data%>%group_by(insert_date,pos)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                      cost_share=sum(cost_share))%>%mutate(click_share1=clicks/sum(clicks),
                                      cost_share1=cost/sum(cost))
plot(display_data$insert_date,display_data$click_share)

displaydata=melt(display_data[,c("click_share","pos","insert_date")],id.vars = c("insert_date","pos"))
ggplot(displaydata,aes(x=insert_date,y=value))+ 
  geom_bar(stat="identity", width=.5,fill="blue",color="black")+
  facet_wrap(~pos)


display_data1=bi_data%>%group_by(insert_date)%>%summarise(clicks=sum(clicks),cost=sum(cost),click_share=sum(click_share),
                                                             cost_share=sum(cost_share))%>%mutate(click_share1=clicks/sum(clicks),
                                                                                                  cost_share1=cost/sum(cost))

displaydata1=melt(display_data1[,c("click_share","insert_date")],id.vars = c("insert_date"))
ggplot(displaydata1,aes(x=insert_date,y=value))+ 
  geom_bar(stat="identity", width=.5,fill="blue",color="black")

ad_campaign<-bi_data%>%group_by(adv_id,week)%>%summarise(clicks=sum(clicks),cost=sum(cost),booking_volume=sum(booking_volume),bookings=sum(bookings),top_pos=sum(top_pos),
                                                         beat=sum(beat),meet=sum(meet),lose=sum(lose),hotel_impressions=sum(hotel_impressions),partner_impressions=sum(partner_impressions),click_share=sum(click_share))%>%
                                                            mutate(top_imression_ratio=top_pos/partner_impressions,beat_ratio=beat/(beat+ meet+lose),
                                                      unavailability=(hotel_impressions-(beat+ meet+lose)),pay_percent=(cost/booking_volume)*100,hotels_per_partner=hotel_impressions/partner_impressions,top_pos_share=top_pos/sum(top_pos))
is.na(ad_campaign)<-sapply(ad_campaign, is.infinite)
ad_campaign[is.na(ad_campaign)]<-0                                      
                            