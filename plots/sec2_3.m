clc,clear
load totaldata
DailyIndex=repmat(1:48,1,length(totaldata(:,1))/48);
totaldata=[DailyIndex',totaldata];
WeeklyIndex=repmat([5*ones(1,48),6*ones(1,48),7*ones(1,48),1*ones(1,48),2*ones(1,48),3*ones(1,48),4*ones(1,48)],1,...
    length(totaldata(:,1))/48/7);
totaldata=[WeeklyIndex',totaldata];
NormalYear=[1*ones(1,48*31),2*ones(1,48*28),3*ones(1,48*31),4*ones(1,48*30),5*ones(1,48*31),6*ones(1,48*30),...
    7*ones(1,48*31),8*ones(1,48*31),9*ones(1,48*30),10*ones(1,48*31),11*ones(1,48*30),12*ones(1,48*31)];
AbNormalYear=[1*ones(1,48*31),2*ones(1,48*29),3*ones(1,48*31),4*ones(1,48*30),5*ones(1,48*31),6*ones(1,48*30),...
    7*ones(1,48*31),8*ones(1,48*31),9*ones(1,48*30),10*ones(1,48*31),11*ones(1,48*30),12*ones(1,48*31)];
MonthlyIndex=[AbNormalYear,NormalYear,NormalYear,NormalYear,AbNormalYear,...
    1*ones(1,48*31),2*ones(1,48*28),3*ones(1,48*31),4*ones(1,48*30),5*ones(1,48*31),6*ones(1,48*30),7*ones(1,48*29)];
totaldata=[MonthlyIndex',totaldata];
NormalSeasonal=[2*ones(1,48*(31+28)),3*ones(1,48*(31+30+31)),4*ones(1,48*(30+31+31)),...
    1*ones(1,48*(30+30+31)),2*ones(1,48*(31))];
AbNormalSeasonal=[2*ones(1,48*(31+29)),3*ones(1,48*(31+30+31)),4*ones(1,48*(30+31+31)),...
    1*ones(1,48*(30+30+31)),2*ones(1,48*(31))];
SeasonalIndex=[AbNormalSeasonal,NormalSeasonal,NormalSeasonal,NormalSeasonal,AbNormalSeasonal,...
    2*ones(1,48*(31+28)),3*ones(1,48*(31+30+31)),4*ones(1,48*(30+29))];
totaldata=[SeasonalIndex',totaldata];
%% figure 1
% column name: 1. seasonal 2. monthly 3. weekly 4. daily 5. demand 6. lockdown
% daily pattern
figure(1)
subplot(2,2,1)
boxplot(totaldata(:,5),string(totaldata(:,4)),'PlotStyle','compact','Colors','k','OutlierSize',3, 'Symbol','.k' )
subplot(2,2,2)
boxplot(totaldata(:,5),string(totaldata(:,3)),'Colors','k','OutlierSize',3, 'Symbol','.k' )
subplot(2,2,3)
boxplot(totaldata(:,5),string(totaldata(:,2)),'Colors','k','OutlierSize',3, 'Symbol','.k' )
subplot(2,2,4)
boxplot(totaldata(:,5),string(totaldata(:,1)),'Colors','k','OutlierSize',3, 'Symbol','.k' )
% define a table
daily=string(totaldata(:,4));
weekly=string(totaldata(:,3));
monthly=string(totaldata(:,2));
seasonal=string(totaldata(:,1));
demand=totaldata(:,5);
daily1Table=table(daily,demand);
%results calculation
tbl1stats1 = grpstats(daily1Table,"daily",'mean')
daily2Table=table(seasonal,demand);
%results calculation
tbl1stats2 = grpstats(daily2Table,"seasonal",'mean')
daily3Table=table(monthly,demand);
%results calculation
tbl1stats3mean = grpstats(daily3Table,"monthly",'mean')
daily4Table=table(weekly,demand);
%results calculation
tbl1stats4mean = grpstats(daily4Table,"weekly",'mean')
%% figure 4
data4fig=totaldata(:,[3,5,6]);
[datasize,~]=size(data4fig);
DailyCount=datasize/48;
weeklylockdown=[];
for i=1:DailyCount
    AA=mean(data4fig((48*(i-1)+1):48*i,:));
    weeklylockdown=[weeklylockdown;AA];
end
weeklylockdown=[weeklylockdown(4:end,:);weeklylockdown(1:3,:)];
% data division
nonlockdownweeklyset=[];
lockdownweeklyset=[];
for i=1:DailyCount
 if weeklylockdown(i,3)>0.5
   lockdownweeklyset=[lockdownweeklyset;weeklylockdown(i,1:2)];
 else
     nonlockdownweeklyset=[nonlockdownweeklyset;weeklylockdown(i,1:2)];
 end
end

figure(4)
subplot(1,2,1)
boxplot(lockdownweeklyset(:,2),string(lockdownweeklyset(:,1)),'Colors','k','OutlierSize',3, 'Symbol','.k' )
subplot(1,2,2)
boxplot(nonlockdownweeklyset(:,2),string(nonlockdownweeklyset(:,1)),'Colors','k','OutlierSize',3, 'Symbol','.k' )
%calculation
ldfig4weekly=string(string(lockdownweeklyset(:,1)));
ldfig4demand=lockdownweeklyset(:,2);
noldfig4weekly=string(string(nonlockdownweeklyset(:,1)));
noldfig4demand=nonlockdownweeklyset(:,2);
fig4tab1=table(ldfig4weekly,ldfig4demand);
fig4tab2=table(noldfig4weekly,noldfig4demand);
fig4mean1 = grpstats(fig4tab1,"ldfig4weekly",'mean')
fig4mean2 = grpstats(fig4tab2,"noldfig4weekly",'mean')

%% figure 3
lockdowndailyset=[];
nolockdowndailyset=[];
for i=1:97776
    if totaldata(i,6)>0.5
        lockdowndailyset=[lockdowndailyset;totaldata(i,4:5)];
    else
        nolockdowndailyset=[nolockdowndailyset;totaldata(i,4:5)];
    end  
end
lockdowndailyset=[lockdowndailyset(2:9360,:);lockdowndailyset(1,:)];
lockdowndaily=string(lockdowndailyset(:,1));
lockdowndemand=lockdowndailyset(:,2);
nolockdowndemand=nolockdowndailyset(:,2);
nolockdowndaily=string(nolockdowndailyset(:,1));
fig3tab1=table(lockdowndaily,lockdowndemand);
fig3tab2=table(nolockdowndaily,nolockdowndemand);
%% calculation
fig3mean1 = grpstats(fig3tab1,"lockdowndaily",'mean')
fig3mean2 = grpstats(fig3tab2,"nolockdowndaily",'mean')
fig3max1 = grpstats(fig3tab1,"lockdowndaily",'max')
fig3max2 = grpstats(fig3tab2,"nolockdowndaily",'max')
fig3min1 = grpstats(fig3tab1,"lockdowndaily",'min')
fig3min2 = grpstats(fig3tab2,"nolockdowndaily",'min')
ldmean=fig3mean1.mean_lockdowndemand;
noldmean=fig3mean2.mean_nolockdowndemand;
ldmax=fig3max1.max_lockdowndemand;
noldmax=fig3max2.max_nolockdowndemand;
ldmin=fig3min1.min_lockdowndemand;
noldmin=fig3min2.min_nolockdowndemand;
figure(3)
plot(ldmean,"-r")
hold on
plot(noldmean,".r")
hold on
plot(ldmax,"-b")
hold on
plot(noldmax,".b")
hold on
plot(ldmin,"-c")
hold on
plot(noldmin,".c")
min(noldmean)-min(ldmean)
%% statistical test
lockdowndailyset
nolockdowndailyset

%0:30-24:00
p=[];
for TT=1:48
DD1=lockdowndailyset(lockdowndailyset(:,1)==TT,2);
DD2=nolockdowndailyset(nolockdowndailyset(:,1)==TT,2);
[h,p(TT),ci,stats]=ttest2(DD1,DD2)
end
plot(p)
hold on
plot([1 48],[0.05 0.05],'r')
set(gca,'YScale','log')



