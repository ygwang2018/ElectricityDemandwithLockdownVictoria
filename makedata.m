clc,clear
a01=csvread('201601.csv',1,0);
a02=csvread('201602.csv',1,0);
a03=csvread('201603.csv',1,0);
a04=csvread('201604.csv',1,0);
a05=csvread('201605.csv',1,0);
a06=csvread('201606.csv',1,0);
a07=csvread('201607.csv',1,0);
a08=csvread('201608.csv',1,0);
a09=csvread('201609.csv',1,0);
a10=csvread('201610.csv',1,0);
a11=csvread('201611.csv',1,0);
a12=csvread('201612.csv',1,0);
a13=csvread('201701.csv',1,0);
a14=csvread('201702.csv',1,0);
a15=csvread('201703.csv',1,0);
a16=csvread('201704.csv',1,0);
a17=csvread('201705.csv',1,0);
a18=csvread('201706.csv',1,0);
a19=csvread('201707.csv',1,0);
a20=csvread('201708.csv',1,0);
a21=csvread('201709.csv',1,0);
a22=csvread('201710.csv',1,0);
a23=csvread('201711.csv',1,0);
a24=csvread('201712.csv',1,0);
Data=[a01;a02;a03;a04;a05;a06;a07;a08;a09;a10;a11;a12;...
    a13;a14;a15;a16;a17;a18;a19;a20;a21;a22;a23;a24];
[~,DataIndex]=sort(Data(:,1));
LoadData=Data(DataIndex,:);
plot(LoadData(:,2))
csvwrite('loaddata.csv',LoadData)
csvwrite('loaddata2.csv',data)
