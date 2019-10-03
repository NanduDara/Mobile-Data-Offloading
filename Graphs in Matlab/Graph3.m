T = xlsread('3.LC_vs_Cost.csv', '3.LC_vs_Cost');

x= T(:,1);
a= T(:,2);
b= T(:,3);
c= T(:,4);
d= T(:,5);

figure
plot(x,a,'g',x,b,'r',x,c,'b',x,d,'y');
xlabel('Iterations');
ylabel('Computation Cost');
legend({'LC1','LC2','LC3', 'LC4'},'Location', 'northeast');
set(gca,'Fontsize', 18);