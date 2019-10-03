T = xlsread('1.LC_1.csv', '1.LC_1');

x= T(:,5);
a= T(:,2);
b= T(:,3);
c= T(:,4);

figure
plot(x,a,'g',x,b,'r',x,c,'b');
xlabel('Size of Data');
ylabel('Cost');
legend({'MD1','MD2','MD3'},'Location', 'northeast');
set(gca,'Fontsize', 18);