
clear all
close all

alfa = 0.75;
delta_all = [-1,1];
kstar_all = [1,2,3];
t = 0:20;
varnames = {'r','o','\pi'};
for ikstar = 1:3
    for idelta = 1:2
        
        kstar = kstar_all(ikstar);
        delta  = delta_all(idelta);
        S = sprintf('IRFs_delta=%d_kstar=%d.mat',delta,kstar);
        load(S)
        
        f = figure
        
        for ii = 1:3
            subplot(3,2,3*(ii-1)-ii+2)
            
            %             plot(t,irf_RESIT_avg(:,ii),'r')
            irf_resit_avg = quantile(irf_RESIT_boot(:,:,kstar,:),0.5,4);
            plot(t,irf_resit_avg(:,ii),'r')
            irf_UP_resit = quantile(irf_RESIT_boot(:,:,kstar,:),1-alfa,4);
            irf_LWR_resit = quantile(irf_RESIT_boot(:,:,kstar,:),alfa,4);
            
            irf_UP_SR = quantile(irfs_boot(:,:,kstar,:),1-alfa,4);
            irf_LWR_SR = quantile(irfs_boot(:,:,kstar,:),alfa,4);
            hold on
            
            plot(t,irf_UP_resit(:,ii),'r--')
            plot(t,irf_LWR_resit(:,ii),'r--')
            axis([0 20 min(min(irf_UP_SR(:,ii)), min(irf_UP_resit(:,ii)))-0.1, ...
                max(max(irf_LWR_SR(:,ii)), max(irf_LWR_resit(:,ii)))+0.1] )
            title(sprintf('%s \\rightarrow %s',varnames{kstar},varnames{ii}))
            plot(t,0*t,'k')
            set(gca,'FontSize',20)
            
            
            
            subplot(3,2,3*(ii-1)-ii+2+1)
            
            irf_sr_avg = quantile(irfs_boot(:,:,kstar,:),0.5,4);
            plot(t,irf_sr_avg(:,ii),'b')

            hold on
            axis([0 20 min(min(irf_UP_SR(:,ii)), min(irf_UP_resit(:,ii)))-0.1, ...
                max(max(irf_LWR_SR(:,ii)), max(irf_LWR_resit(:,ii)))+0.1] )
            plot(t,irf_UP_SR(:,ii),'b--')
            plot(t,irf_LWR_SR(:,ii),'b--')
            title(sprintf('%s \\rightarrow %s',varnames{kstar},varnames{ii}))
            plot(t,0*t,'k')
            set(gca,'FontSize',20)
            
            
            
            
        end
        f.Position = [1e3 1e3 1e3 1e3]
        Save_s = sprintf('IRFs_delta=%d_kstar=%d.eps',delta,kstar);
        saveas(f,Save_s,'epsc')
        
        
        
    end
end


