clear all
close all
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/var_tbx')
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/stvar_tbx')
addpath('./../../../empirical/new_ACI_IP_CO2/_tbx/supportfct')

a = [0.1 0.5 0.9]
T = [250 500 1000]
H = 10+1
flag_oracle = 0;
t = 0:(H-1);

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
        
        if (flag_oracle == 1)
            IRF_2_orcl = csvread(strcat('IRF_2_orcl_sim',file),1,1);
            IRF_3_orcl = csvread(strcat('IRF_3_orcl_sim ',file),1,1);
            
        end
        
        ii = ii +1;
        subplot(3,3,ii)
        hold on
        plot(t,IRF_2_true(1:H),'k');
        xlim([0 H]);
        S = sprintf('1 -> 2, a = %g, T = %g', aa, TT);
        title(S)
        
        plot(t,IRF_2_NL(1:H,1),'r');
        plot(t,IRF_2_LIN(1:H,1),'b');
        plot(t,IRF_2_NL(1:H,2),'r--');
        plot(t,IRF_2_NL(1:H,3),'r--');
        
        plot(t,IRF_2_LIN(1:H,2),'b--');
        plot(t,IRF_2_LIN(1:H,3),'b--');
        
        if (flag_oracle == 1)
            plot(t,IRF_2_orcl(1:H,1),'g');
            plot(t,IRF_2_orcl(1:H,2),'g--');
            plot(t,IRF_2_orcl(1:H,3),'g--');
        end
        
        if (flag_oracle == 0)
            grid on
            legend('IRF th.','IRF NL','IRF LIN')
            set(gca,'FontSize',20)
        else
            grid on
            legend('IRF th.','IRF NL','IRF LIN','IRF ORCL')
            set(gca,'FontSize',20)
        end
        
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
        
        if (flag_oracle == 1)
            IRF_2_orcl = csvread(strcat('IRF_2_orcl_sim',file),1,1);
            IRF_3_orcl = csvread(strcat('IRF_3_orcl_sim ',file),1,1);
            
        end
        
        ii = ii +1;
        subplot(3,3,ii)
        hold on
        plot(t,IRF_3_true(1:H),'k');
        xlim([0 H])
        S = sprintf('1 -> 3, a = %g, T = %g', aa, TT);
        title(S)
        hold on
        plot(t,IRF_3_NL(1:H,1),'r');
        plot(t,IRF_3_LIN(1:H,1),'b');
        plot(t,IRF_3_NL(1:H,2),'r--');
        plot(t,IRF_3_NL(1:H,3),'r--');
        
        plot(t,IRF_3_LIN(1:H,2),'b--');
        plot(t,IRF_3_LIN(1:H,3),'b--');
        
        if (flag_oracle == 1)
            plot(t,IRF_3_orcl(1:H,1),'g');
            plot(t,IRF_3_orcl(1:H,2),'g--');
            plot(t,IRF_3_orcl(1:H,3),'g--');
        end
        
        
        if (flag_oracle == 0)
            grid on
            legend('IRF th.','IRF NL','IRF LIN')
            set(gca,'FontSize',20)
        else
            grid on
            legend('IRF th.','IRF NL','IRF LIN','IRF ORCL')
            set(gca,'FontSize',20)
        end
    end
end





