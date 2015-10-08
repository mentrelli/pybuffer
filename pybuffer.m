function [p, q, t] = prob_buffer(n0, L, M, tmax=6000, dt=1.0, nmax=30)
    
%        input:
%            n0:   stato iniziale del buffer
%            L:    MTBF macchina #1 [min]
%            M:    MTBF macchina #3 [min]
%            tmax: tempo massimo del calcolo [min] (default: 100 h)
%            dt:   intervallo temporale per il calcolo [min] (default: 1 min)
%            nmax: numero massimo di stati del buffer (default: 30)
%        output:
%            p: matrice delle probabilità
%               p(i,j) è la probabilità di essere nello stato (i-1)-esimo
%               al tempo (j-1)*dt
%            q: vettore delle probabilità di interruzione dovuta
%               ad esaurimento del buffer e concomitante rottura della
%               macchina a monte
%               q(j) è la probabilità di interruzione al tempo (j-1)*dt
%            t: vettore con gli istanti temporali 
%               t(j) contiene il tempo t=(j-1)*dt

% NOTA: Se si confrontano le versioni Python e Matlab/Octave, tenere 
% presente che gli indici in Python partono da 0 (come in C) e in 
% Matlab/Octave partono da 1

    Ndt = round(tmax/dt) ; % numero intervalli temporali
    
    p = zeros(nmax+1, Ndt+1) ;
    q = zeros(Ndt+1) ;
    t = zeros(Ndt+1) ;
    
    p(n0+1, 1) = 1 ; % si parte con il buffer nello stato n0
    
    Ldt = L*dt ;
    Mdt = M*dt ;
    oLMdt = 1-Ldt-Mdt ;
    
    idx_nmax = nmax + 1 ;

    for i = [1 : Ndt]
        
        p(idx_nmax, i+1) = p(idx_nmax, i)*oLMdt + p(idx_nmax-1, i)*Mdt ;

        for j = [idx_nmax-1 : -1 : 2]
            p(j, i+1) = p(j, i)*oLMdt + p(j+1, i)*Ldt + p(j-1, i)*Mdt ;
        end

        p(1, i+1) = p(1, i)*oLMdt + p(2, i)*Ldt ;

        q(i+1) = p(1, i)*Ldt ;
        
        t(i+1) = t(i) + dt ;
        
    end

    return

%n0 = 3 ;
%L = 1.0/(36*60.0) ; % MTBF macchina #1 [minuti]
%M = 1.0/(36*60.0) ; % MTBF macchina #3 [minuti]
%
%[p, q, t] = prob_buffer(n0, L, M, tmax=6000, dt=1.0, nmax=100) ;
%plot(t, p(n0+1,:))
%plot(t, q)
