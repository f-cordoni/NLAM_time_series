clear all
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/var_tbx') 
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/stvar_tbx') 
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/supportfct') 

a = [0.1 0.5 0.9]
T = [250 1000]

 
 f= figure
 ii = 0;
for ia = 1:length(a)
    for it = 1:length(T)
        
aa = a(ia); TT = T(it);
file = sprintf(' _a=%g_T=%g.csv',aa,TT);
IRF_2_NL = csvread(strcat('IRF_2_NL_sim ',file),1,1);
IRF_3_NL = csvread(strcat('IRF_3_NL_sim ',file),1,1);

IRF_2_LIN = csvread(strcat('IRF_2_LIN_sim ',file),1,1);
IRF_3_LIN = csvread(strcat('IRF_3_LIN_sim ',file),1,1);

IRF_2_true = csvread(strcat('IRF_2_true_sim ',file),1,1);
IRF_3_true = csvread(strcat('IRF_3_true_sim ',file),1,1);


 ii = ii +1;
 subplot(3,2,ii)
 hold on 
 plot(IRF_2_true,'k');
  S = sprintf('1 -> 2, a = %g, T = %g', aa, TT);
title(S)

plot(IRF_2_NL(:,1),'r');
plot(IRF_2_LIN(:,1),'b');
plot(IRF_2_NL(:,2),'r--');
plot(IRF_2_NL(:,3),'r--');

plot(IRF_2_LIN(:,2),'b--');
plot(IRF_2_LIN(:,3),'b--');

grid on
legend('IRF th.','IRF NL','IRF LIN')
set(gca,'FontSize',20)
    end
end


 f= figure
 ii = 0;
for ia = 1:length(a)
    for it = 1:length(T)
        
aa = a(ia); TT = T(it);
file = sprintf(' _a=%g_T=%g.csv',aa,TT);
IRF_2_NL = csvread(strcat('IRF_2_NL_sim ',file),1,1);
IRF_3_NL = csvread(strcat('IRF_3_NL_sim ',file),1,1);

IRF_2_LIN = csvread(strcat('IRF_2_LIN_sim ',file),1,1);
IRF_3_LIN = csvread(strcat('IRF_3_LIN_sim ',file),1,1);

IRF_2_true = csvread(strcat('IRF_2_true_sim ',file),1,1);
IRF_3_true = csvread(strcat('IRF_3_true_sim ',file),1,1);


 ii = ii +1;
 subplot(3,2,ii)
hold on
plot(IRF_3_true,'k');
 S = sprintf('1 -> 3, a = %g, T = %g', aa, TT);
title(S)
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
    end
end
 
 

 
 
