
ggplot( filter(enrol, Coordinate=="1.2.1.13.9.1.1"), aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) 
ggplot(filter(grad, Coordinate=="1.2.13.9.1.1"),aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1)

temp=grad %>% filter( Coordinate=="1.2.1.9.1.1") %>% select(Ref_Date, Value)

ggplot( filter(enrol, Coordinate=="1.2.1.13.9.1.1"), aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) + 
   geom_line(data=filter(grad, Coordinate=="1.2.1.9.1.1"),  aes(x=Ref_Date,y=Value), lwd=1, col=2)

cudo_UT %>% ggplot() + geom_line(aes(x=Year, y=Enroll, group = FoS, colour=FoS)) + 
  geom_line(data=cudo_UT, aes(x=Year, y=Grad, group = FoS, colour=FoS))

