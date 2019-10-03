T = xlsread('5.Iter_vs_Time.csv', '5.Iter_vs_Time');

x= T(:,1);
a= T(:,2);
b= T(:,3);
c= T(:,4);
d= T(:,5);

figure
plot(x,a,'g',x,b,'r',x,c,'b',x,d,'y');
xlabel('Iterations');
ylabel('Computation Time');
legend({'HC1','HC2','HC3', 'HC4'},'Location', 'northeast');
set(gca,'Fontsize', 18);