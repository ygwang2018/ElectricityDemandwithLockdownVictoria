%% daily change
t=1:48;
Lockdown=0;
COVID=0;
DailyChangeNormal=-517.21*sin(2*pi*t/(48))-278.00*cos(2*pi*t/(48))-360.76*sin(2*pi*t/(24))-146.97*cos(2*pi*t/(48))...
    -23.94*COVID-210.55*Lockdown+9.36*Lockdown*sin(2*pi*t/(48))+237.16*Lockdown*cos(2*pi*t/(48))...
    -115.83*Lockdown*sin(2*pi*t/(24))-51.83*Lockdown*cos(2*pi*t/(24));
Lockdown=0;
COVID=1;
DailyChangeNoLockdown=-517.21*sin(2*pi*t/(48))-278.00*cos(2*pi*t/(48))-360.76*sin(2*pi*t/(24))-146.97*cos(2*pi*t/(48))...
    -23.94*COVID-210.55*Lockdown+9.36*Lockdown*sin(2*pi*t/(48))+237.16*Lockdown*cos(2*pi*t/(48))...
    -115.83*Lockdown*sin(2*pi*t/(24))-51.83*Lockdown*cos(2*pi*t/(24));
Lockdown=1;
COVID=1;
DailyChangeLockdown=-517.21*sin(2*pi*t/(48))-278.00*cos(2*pi*t/(48))-360.76*sin(2*pi*t/(24))-146.97*cos(2*pi*t/(48))...
    -23.94*COVID-210.55*Lockdown+9.36*Lockdown*sin(2*pi*t/(48))+237.16*Lockdown*cos(2*pi*t/(48))...
    -115.83*Lockdown*sin(2*pi*t/(24))-51.83*Lockdown*cos(2*pi*t/(24));
figure(1)
subplot(1,2,1)
plot(DailyChangeNormal,"k")
hold on 
plot(DailyChangeNoLockdown,"b")
hold on
plot(DailyChangeLockdown,"r")
%% weekly change
t=1:48*7;
Workday=[ones(1,48*5),0*ones(1,48*2)];
Lockdown=0;
COVID=0;
WeeklyChangeNormal=496.11*Workday-88.57*sin(2*pi*t/(7*48))+81.71*cos(2*pi*t/(7*48))...
    -23.94*COVID-210.55*Lockdown-110.72*Lockdown*Workday-32.78*Lockdown*sin(2*pi*t/(7*48))...
    -57.11*Lockdown*cos(2*pi*t/(7*48));
Lockdown=0;
COVID=1;
WeeklyChangeNoLockdown=496.11*Workday-88.57*sin(2*pi*t/(7*48))+81.71*cos(2*pi*t/(7*48))...
    -23.94*COVID-210.55*Lockdown-110.72*Lockdown*Workday-32.78*Lockdown*sin(2*pi*t/(7*48))...
    -57.11*Lockdown*cos(2*pi*t/(7*48));
Lockdown=1;
COVID=1;
WeeklyChangeLockdown=496.11*Workday-88.57*sin(2*pi*t/(7*48))+81.71*cos(2*pi*t/(7*48))...
    -23.94*COVID-210.55*Lockdown-110.72*Lockdown*Workday-32.78*Lockdown*sin(2*pi*t/(7*48))...
    -57.11*Lockdown*cos(2*pi*t/(7*48));
subplot(1,2,2)
plot(WeeklyChangeNormal,"k")
hold on 
plot(WeeklyChangeNoLockdown,"b")
hold on
plot(WeeklyChangeLockdown,"r")
%figure: total weekly
t=1:48*7;
Workday=[ones(1,48*5),0*ones(1,48*2)];
Lockdown=0;
COVID=0;
TotalWeeklyDailyNormal=-517.21*sin(2*pi*t/(48))-278.00*cos(2*pi*t/(48))-360.76*sin(2*pi*t/(24))-146.97*cos(2*pi*t/(48))...
    -23.94*COVID-210.55*Lockdown+9.36*Lockdown*sin(2*pi*t/(48))+237.16*Lockdown*cos(2*pi*t/(48))...
    -115.83*Lockdown*sin(2*pi*t/(24))-51.83*Lockdown*cos(2*pi*t/(24))...
    +496.11*Workday-88.57*sin(2*pi*t/(7*48))+81.71*cos(2*pi*t/(7*48))...
    -110.72*Lockdown*Workday-32.78*Lockdown*sin(2*pi*t/(7*48))...
    -57.11*Lockdown*cos(2*pi*t/(7*48));
Lockdown=0;
COVID=1;
TotalWeeklyDailyNoLockdown=-517.21*sin(2*pi*t/(48))-278.00*cos(2*pi*t/(48))-360.76*sin(2*pi*t/(24))-146.97*cos(2*pi*t/(48))...
    -23.94*COVID-210.55*Lockdown+9.36*Lockdown*sin(2*pi*t/(48))+237.16*Lockdown*cos(2*pi*t/(48))...
    -115.83*Lockdown*sin(2*pi*t/(24))-51.83*Lockdown*cos(2*pi*t/(24))...
    +496.11*Workday-88.57*sin(2*pi*t/(7*48))+81.71*cos(2*pi*t/(7*48))...
    -110.72*Lockdown*Workday-32.78*Lockdown*sin(2*pi*t/(7*48))...
    -57.11*Lockdown*cos(2*pi*t/(7*48));
Lockdown=1;
COVID=1;
TotalWeeklyDailyLockdown=-517.21*sin(2*pi*t/(48))-278.00*cos(2*pi*t/(48))-360.76*sin(2*pi*t/(24))-146.97*cos(2*pi*t/(48))...
    -23.94*COVID-210.55*Lockdown+9.36*Lockdown*sin(2*pi*t/(48))+237.16*Lockdown*cos(2*pi*t/(48))...
    -115.83*Lockdown*sin(2*pi*t/(24))-51.83*Lockdown*cos(2*pi*t/(24))...
    +496.11*Workday-88.57*sin(2*pi*t/(7*48))+81.71*cos(2*pi*t/(7*48))...
    -110.72*Lockdown*Workday-32.78*Lockdown*sin(2*pi*t/(7*48))...
    -57.11*Lockdown*cos(2*pi*t/(7*48));
figure(2)
plot(TotalWeeklyDailyNormal,"k")
hold on 
plot(TotalWeeklyDailyNoLockdown,"b")
hold on
plot(TotalWeeklyDailyLockdown,"r")
%%calculation
A=(DailyChangeNoLockdown-DailyChangeLockdown);
B=(WeeklyChangeNormal-WeeklyChangeLockdown);
C=(TotalWeeklyDailyNormal-TotalWeeklyDailyLockdown);
min(C)
plot(C)
max(B)
min(B)
plot(B)