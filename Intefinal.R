


inte<-function(t){
  ifelse(t<12*12, 
         ifelse(t<6*12|t>18*12,0,rnorm(1,mean = 400/(144)*t, sd = t)),
         ifelse(t<6*12|t>18*12,0,rnorm(1,mean = -400/(144)*(t)+800, sd = t)))
  
}
dz<-function(epoc){
  ifelse(epoc%in%c("enero", "febrero","marzo"),14,30)
}
ka<-19.7
a<-19.7
B0<-1
z<-0.245

t<-seq(from=0, to=24*12, by=5)
epoc<-"septiembre"
I<-NULL

for (i in t) {
#res<-abs(inte(i))*exp(-ka*B0*z)
res<-inte(i)/(a*B0)*(1-exp(-a*B0))
  I<-c(I,res)
}
plot(x=t,y=I,type="b")
plot(x=t,y=inte(t),type="b")


###########
library(dplyr)
inte<-function(m,h){
  if(m==1&h==6) 0.9 else
    if(m==1&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
      if(m==1&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
        if(m==1&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
          if(m==1&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
            if(m==1&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.81708668219895232)else
              if(m==1&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                if(m==1&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                  if(m==1&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                    if(m==1&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                      if(m==1&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                        if(m==1&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                          if(m==1&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                            if(m==1&h==19)0.9 else
                              if(m==2&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                if(m==2&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                  if(m==2&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                    if(m==2&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                      if(m==2&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                        if(m==2&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                          if(m==2&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                            if(m==2&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                              if(m==2&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                if(m==2&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                  if(m==2&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                    if(m==2&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                      if(m==2&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                        if(m==2&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                          if(m==3&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                            if(m==3&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                              if(m==3&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                if(m==3&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                  if(m==3&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                    if(m==3&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                      if(m==3&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                        if(m==3&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                          if(m==3&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                            if(m==3&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                              if(m==3&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                if(m==3&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                  if(m==3&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                    if(m==3&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                      if(m==4&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                        if(m==4&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                          if(m==4&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                            if(m==4&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                              if(m==4&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                if(m==4&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                  if(m==4&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                    if(m==4&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                      if(m==4&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                        if(m==4&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                          if(m==4&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                            if(m==4&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                              if(m==4&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                if(m==4&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                  if(m==5&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                    if(m==5&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                      if(m==5&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                        if(m==5&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                          if(m==5&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                            if(m==5&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                              if(m==5&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                if(m==5&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                  if(m==5&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                    if(m==5&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                      if(m==5&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                        if(m==5&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                          if(m==5&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                            if(m==5&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                              if(m==6&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                if(m==6&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                  if(m==6&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                    if(m==6&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                      if(m==6&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                        if(m==6&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                          if(m==6&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                            if(m==6&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                              if(m==6&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                if(m==6&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                  if(m==6&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                    if(m==6&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                      if(m==6&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                        if(m==6&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                          if(m==7&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                                            if(m==7&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                                              if(m==7&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                                                if(m==7&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                                                  if(m==7&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                                                    if(m==7&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                                                      if(m==7&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                                                        if(m==7&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                                                          if(m==7&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                                            if(m==7&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                                              if(m==7&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                                                if(m==7&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                                                  if(m==7&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                                                    if(m==7&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                                                      if(m==8&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                                                                        if(m==8&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                                                                          if(m==8&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                                                                            if(m==8&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                                                                              if(m==8&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                                                                                if(m==8&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                                                                                  if(m==8&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                                                                                    if(m==8&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                                                                                      if(m==8&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                                                                        if(m==8&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                                                                          if(m==8&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                                                                            if(m==8&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                                                                              if(m==8&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                                                                                if(m==8&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                                                                                  if(m==9&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                                                                                                    if(m==9&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                                                                                                      if(m==9&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                                                                                                        if(m==9&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                                                                                                          if(m==9&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                                                                                                            if(m==9&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                                                                                                              if(m==9&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                                                                                                                if(m==9&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                                                                                                                  if(m==9&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                                                                                                    if(m==9&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                                                                                                      if(m==9&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                                                                                                        if(m==9&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                                                                                                          if(m==9&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                                                                                                            if(m==9&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                                                                                                              if(m==10&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                                                                                                                                if(m==10&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                                                                                                                                  if(m==10&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                                                                                                                                    if(m==10&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                                                                                                                                      if(m==10&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                                                                                                                                        if(m==10&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                                                                                                                                          if(m==10&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                                                                                                                                            if(m==10&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                                                                                                                                              if(m==10&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                                                                                                                                if(m==10&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                                                                                                                                  if(m==10&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                                                                                                                                    if(m==10&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                                                                                                                                      if(m==10&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                                                                                                                                        if(m==10&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                                                                                                                                          if(m==11&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                                                                                                                                                            if(m==11&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                                                                                                                                                              if(m==11&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                                                                                                                                                                if(m==11&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                                                                                                                                                                  if(m==11&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                                                                                                                                                                    if(m==11&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                                                                                                                                                                      if(m==11&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                                                                                                                                                                        if(m==11&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                                                                                                                                                                          if(m==11&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                                                                                                                                                            if(m==11&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                                                                                                                                                              if(m==11&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                                                                                                                                                                if(m==11&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                                                                                                                                                                  if(m==11&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                                                                                                                                                                    if(m==11&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                                                                                                                                                                      if(m==12&h==6) rPARETO2(n = 1,mu = 2.22e-16,sigma = 3.47789908)else
                                                                                                                                                                                                                                                                                                                        if(m==12&h==7)rBCCG(n = 1,mu = 15.5930087,sigma = 0.408061,nu = 1.01507176)else
                                                                                                                                                                                                                                                                                                                          if(m==12&h==7)rWEI2(n = 1,mu = 3.45e-6,sigma = 2.58219105,)else
                                                                                                                                                                                                                                                                                                                            if(m==12&h==9)rBCPEo(n = 1,mu = 293.681696,sigma = 0.36106837,nu = 1.3384107,tau = 6.19421793)else
                                                                                                                                                                                                                                                                                                                              if(m==12&h==10)rGB2(n = 1,mu = 644.956263,sigma = 321.923539,nu = 0.00677758,tau = 0.10807477)else
                                                                                                                                                                                                                                                                                                                                if(m==12&h==11)rBCPEo(n = 1,mu = 635.103331,sigma = 0.20759592,nu = 2.78112526,tau = 2.8171e17)else
                                                                                                                                                                                                                                                                                                                                  if(m==12&h==12)rGG(n = 1,mu = 869.459569,sigma = 0.02791512,nu = 417.902154)else
                                                                                                                                                                                                                                                                                                                                    if(m==12&h==13)rGG(n = 1,mu = 836.903168,sigma = 0.10870041,nu = 30.6344085)else
                                                                                                                                                                                                                                                                                                                                      if(m==12&h==14)rGG(n = 1,mu = 776.502327,sigma = 0.10443843,nu = 34.855255)else
                                                                                                                                                                                                                                                                                                                                        if(m==12&h==15)rGG(n = 1,mu = 677.012111,sigma = 0.05347242,nu = 146.655609)else
                                                                                                                                                                                                                                                                                                                                          if(m==12&h==16)rGG(n = 1,mu = 510.195609,sigma = 0.05092428,nu = 257.251237)else
                                                                                                                                                                                                                                                                                                                                            if(m==12&h==17)rGG(n = 1,mu = 248.545059,sigma = 0.22749761,nu = 15.7249711)else
                                                                                                                                                                                                                                                                                                                                              if(m==12&h==18)rGG(n = 1,mu = 50.4584026,sigma = 0.42263008,nu = 4.05341925)else
                                                                                                                                                                                                                                                                                                                                                if(m==12&h==19)rPARETO2(n = 1,mu = 2.22e-16,sigma = 7.35026657)else
                                                                                                                                                                                                                                                                                                                                                  0
}
#Crear una columna con los valores para el día
intedia<-function(m){
  resinte<-NULL
  i<-0
   for (val in i:23) {
   res1<-inte(m = m,h = i)
  resinte<-c(resinte,res1)
    i<-i+1
   }
resinte<-as.matrix(resinte)
  resinte
}

(inteenero<-intedia(m = 5))
#Cálculo para 1 día


a<-19.7
B0<-1
z<-0.245

###################
#para un día
#Constantes
B0=0.3#biomasa inicial OD750
#1*(2*6+(pi*2^2)/4)*1000*0.3*0.2/1.25
ka<-46#coeficiente de absorción de luz n.salina, 1/(OD*m)
mmax<-1.3#tasa de crecimiento máxima
imax<-489#Int maxima steele
volbiom<-1*(2*6+2*(pi*2^2)/4)*1000*B0*0.2/1.25
cc<-47.54
#Variables de seguimiento

resbiomg<-matrix(ncol=6)
resincid<-matrix(ncol=3)
biomasacosechada<-matrix(ncol=2)

for (k in 1:365) {
  #Variables de seguimiento
if (Bgl>=16) {
  B0=0.3#Cosecha
  cosecha<-cbind(k,volbiom)
  biomasacosechada<-rbind(biomasacosechada,cosecha)

  }
B<-B0#biomasa inicial OD750
h=0
biom1h <- matrix(ncol=6, nrow=10)
biom1d<-matrix(ncol=6, nrow=23)
for (j in 0:23) {
  j=h
z<-0.1#Altura
i=0#contador
i0<-inte(m = 1,h = j)#incidencia para la hora
  for (i in 1:10) {
    iz<-i0*exp(-ka*B*z)
    ifelse(i0==0,mbio<-0,
  mbio<-1+mmax*(iz/imax)*exp(1-(iz/imax)))
  B<-B0*exp(mbio*1/24)
  Bgl<-B*0.2/1.25#Constantes para pasar de OD750 a g/l
  biom1h[i,] <- c(z,i0,iz,mbio,B,Bgl)
  z<-z+0.1
}
colnames(biom1h)<-c("z","I0","Iz","Tasa_crecimiento","Biomasa OD750","Biomasag/l")
biom1h
gr<-mean(biom1h[,4])
B0<-mean(biom1h[,5])
volbiom=sum(0.1*(2*6+2*(pi*2^2)/4)*1000*biom1h[,6])#gramos de biomasa totales
biom1d[j,] <- c(k,i0,h,B0,volbiom,gr)
colnames(biom1d)<-c("dia","i0","h","BiomOD750","Biom(g)","Growthrate")
h=h+1
}
biom1d
#Rescatar la incidencia de todo el año
incid<-biom1d[,1:3]

resincid<-rbind(resincid,incid)

#Rescatar la biomasa de todo el año
biomg<-biom1d[,1:6]

resbiomg<-rbind(resbiomg,biomg)
resbiomg<-resbiomg[-1,]
k=k+1
}
cosecha<-cbind(k,volbiom)
biomasacosechada<-rbind(biomasacosechada,cosecha)
biomasacosechada[,2]<-biomasacosechada[,2]/10^6#pasa a ton

biomasacosechada<-as.data.frame(biomasacosechada[-1,])
biomasacosechada$cum<- cumsum (biomasacosechada[,2])

#carbono capturado
resbiomg
carbonocapturado<-biomasacosechada
carbonocapturado$cum<-carbonocapturado$cum*(cc/100*(44/12))
carbonocapturado
#write.csv(carbonocapturado,file = "carbonocapturado.csv")
#Gráficos

plot(biomasacosechada$k,biomasacosechada$volbiom,type = "b",ylab = "Biomasa[Ton]",xlab = "Día")
library(ggpubr)
ggline(data = biomasacosechada[,C(1,3)],x =biomasacosechada$k,y = biomasacosechada$cum,ylab = "Biomasa[Ton CO2e]",xlab = "Día")+
  line(biomasacosechada$k,biomasacosechada$cum/((cc/100*(44/12)))*821/1000)+
  line(biomasacosechada$k,biomasacosechada$cum)

volacum<-resbiomg[,2]
volacum<-cumsum(volacum[-1])
volacum<-cbind(resbiomg[,3],volacum)
volacum
plot(x=volacum[,2],y=volacum[,1]==10)
plot(x=resincid[,3],y=resincid[,2],xlab = "Hora del día",ylab = "Incidencia solar I0",main = "Resultados de la generación aleatoria de I0")
resbiomg<-as.data.frame(resbiomg)
y<-resbiomg[1:(23*30),4]
x<-resbiomg[1:(23*30),1]
plot(y = y/(10^3),x=x,type = "p",xlab = "Día",ylab = "Biomasa [Kg]",main = "Ciclos de generación de biomasa durante el proceso")
  
plot(y = carbonocapturado$cum*821/1000,x=carbonocapturado$k, type= "l", xlab = "Día",ylab = "Biomasa [Ton]",main = "Ciclos de generación de biomasa durante el proceso")+
  
  plot(carbonocapturado$k,                              # Draw first time series
       carbonocapturado$cum,
       type = "l",
       col = 2,
       ylim = c(- 15, 40),
       xlab = "Día",
       ylab = "[KgCO2e]")
lines(carbonocapturado$k,                             # Draw second time series
      carbonocapturado$cum/((cc/100*(44/12)))*821/1000,
      type = "l",
      col = 3)
legend("topright",                           # Add legend to plot
       c("Carbono capturado[Ton CO2e]", "Carbono emitido[Ton CO2e]"),
       lty = 1,
       col = 2:4)

