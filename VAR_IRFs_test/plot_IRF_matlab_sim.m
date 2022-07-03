clear all
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/var_tbx') 
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/stvar_tbx') 
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/supportfct') 

IRF_2_NL = csvread('IRF_2_NL_sim.csv',1,1);
IRF_3_NL = csvread('IRF_3_NL_sim.csv',1,1);

IRF_2_LIN = csvread('IRF_2_LIN_sim.csv',1,1);
IRF_3_LIN = csvread('IRF_3_LIN_sim.csv',1,1);

IRF_2_true = csvread('IRF_2_true_sim.csv',1,1);
IRF_3_true = csvread('IRF_3_true_sim.csv',1,1);

 
 f= figure
 hold on
 plot(IRF_2_true,'k');
title('1 -> 2')
 
plot(IRF_2_NL(:,1),'r');
plot(IRF_2_LIN(:,1),'b');
plot(IRF_2_NL(:,2),'r--');
plot(IRF_2_NL(:,3),'r--');

plot(IRF_2_LIN(:,2),'b--');
plot(IRF_2_LIN(:,3),'b--');

grid on
legend('IRF th.','IRF NL','IRF LIN')
set(gca,'FontSize',20)
 
 
 f= figure
 hold on
plot(IRF_3_true,'k');
title('1 -> 3')
hold on 
plot(IRF_3_NL(:,1),'r');
plot(IRF_3_LIN(:,1),'b');
plot(IRF_3_NL(:,2),'r--');
plot(IRF_3_NL(:,3),'r--');

plot(IRF_3_LIN(:,2),'b--');
plot(IRF_3_LIN(:,3),'b--');

grid on
legend('IRF th.','IRF NL','IRF LIN')
set(gca,'FontSize',20)
 
 

 
 
