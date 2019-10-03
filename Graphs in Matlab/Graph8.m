T = xlsread('8.Iter_vs_MD_vs_Win.csv', '8.Iter_vs_MD_vs_Win');

x= T(:,1);
a= T(:,2);
b= T(:,3);
c= T(:,4);
d= T(:,5);

figure
plot(x,a,'g',x,b,'r',x,c,'b',x,d,'y');
xlabel('Iterations');
ylabel('Winning Counts');
legend({'MD1','MD2','MD3', 'MD4'},'Location', 'northeast');
set(gca,'Fontsize', 18);