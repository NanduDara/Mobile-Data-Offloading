T = xlsread('2.LC1_1.csv', '2.LC1_1');

x= T(:,5);
a= T(:,2);
b= T(:,3);
c= T(:,4);

figure
plot(x,a,'g',x,b,'r',x,c,'b');
xlabel('Size of Data');
ylabel('Computation Time');
legend({'MD1','MD2','MD3'},'Location', 'northeast');
set(gca,'Fontsize', 18);