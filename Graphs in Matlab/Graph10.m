clc,clear,close all

X = [12	14	17	19	21	22	23	25; 30	31	32	34	37	38	39	40];
Y = [0.4	0.6	0.6	0.8	0.9	0.9	0.9	0.9; 0.8 0.6 0.4 0.3 0.2 0.1 0.1 0.1];
Z = [4.8	8.4	10.2	15.2	18.9	19.8	20.7	22.5; 24	18.6	12.8	10.2	7.4	3.8	3.9	4];

figure
surf(X,Y,Z);
colorbar
grid on
xlabel('Size of Data','FontSize',22)
ylabel('Probablility','FontSize',22)
zlabel('Utility','FontSize',22)
set(get(gca,'zlabel'),'rotation',360)
set(gca,'Fontsize', 18);