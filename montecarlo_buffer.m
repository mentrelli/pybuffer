1;

function tt = get_time_failures(MTBF, tmax)

    % Costruzione di un processo di Poisson di parametro lambda=1/MTBF 
    % nell'intervallo [0, tmax]
    % 
    % input: 
    %    MTBF: mean time between failure (=1/lambda)
    %    tmax: estremo superiore dell'intervallo
    %
    % output:
    %    tt: vettore con i tempi (casuali) di rottura nell'intervallo [0, tmax].
    %        Poiché le rotture sono modellate come un processo di Poisson, 
    %        i tempi di rottura seguono una distribuzione esponenziale.
    % 
    % Il Processo di Poisson è caratterizzato dal fatto che gli eventi sono
    % indipendenti l'uno dall'altro e il parametro lambda rappresenta il tasso
    % di rottura (costante). Si dimostra che sotto queste ipotesi il tempo di 
    % occorrenza degli eventi ha una legge di distribuzione esponenziale.
    %
    % Nota: Se si vuole implementare un modello diverso da quello di Poisson,
    %       basta modificare la costruzione del vettore tt. (Invece della legge
    %       di distribuzione esponenziale, si potrebbe usare Weibull...)
    
    tt = [] ;
    t = 0 ;
    
    while ( (t += exprnd(MTBF)) < tmax )
        tt(end+1) = t ;
    endwhile
    
endfunction 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [p1, q1, p2, q2, tt] = mc_buffer(n01, n02, MTBF_M1, MTBF_M2, MTBF_M3, 
                                          Nmc, tmax=6000.0, dt=1.0)

    % input:
    %    n01: stato iniziale buffer 1
    %    n02: stato iniziale buffer 2
    %    MTBF_M1: mean time between failure macchina 1 [min]
    %    MTBF_M2: mean time between failure macchina 2 [min]
    %    MTBF_M3: mean time between failure macchina 3 [min]
    %    Nmc: numero di simulazioni Montecarlo da eseguire
    %    tmax: estensione dell'intervallo temporale [0, tmax] su cui viene
    %          eseguita la simulazione [min] (default: 60000 min)
    %    dt: passo temporale per la ragistrazine dei dati nell'intervallo
    %        [0, tmax] (default: 1 min)
    %
    % output:
    %    p1: matrice delle frequenze relative (~probabilità) degli stati del
    %        buffer della macchina 1. p1(k,i) è la frequenza relativa dello 
    %        stato (k-1)-esimo del buffer 1 al tempo tt(i)=(i-1)*dt
    %    q1: vettore delle frequenze relative (~probabilità) dello stato 
    %        non-operativo della macchina 1. q1(i) è la frequenza relativa 
    %        con cui la macchina 1 è fuori servizio al tempo tt(i)=(i-1)*dt
    %    p2: analogo di p1 per la macchina 2
    %    q2: analogo di q1 per la macchina 2
    %    tt: vettore degli istanti temporali in cui vengono raccolti i dati
    %        delle simulazioni Montecarlo; tt(i)=(i-1)*dt, tt(end)=tmax
    %
    % NB: Tenere presente che le frequenze relative tendono alle probabilità
    %     quando il numero di campioni (Nmc) tende all'infinito (quindi più
    %     grande è Nmc, meglio è... ma i tempi di calcolo si allugnano)
    %
    %                                   --- versione 0.1 (17/10/2015), by AM ---
    

    %tmax = 6000.0 ; % limite temporale della simulazione [min]
    %dt = 1.0 ; % intervallo di tempo per la raccolta dei dati [min]
    
    tt = [ 0.0 : dt : tmax ] ;
    ntt = length(tt) ;
    
    % MTBF delle 3 macchine
    %MTBF_M1 = 36*60.0 ;
    %MTBF_M2 = 36*60.0 ; 
    %MTBF_M3 = 36*60.0 ;
    
    % stato iniziale di riempimento dei due buffer
    %n01 = 3 ;
    %n02 = 3 ;
    
    % numero massimo di stati registrati del buffer
    nmax = 20 ;
    
    % stati del buffer in corrispondenza degli stati tt
    nn1 = zeros(1, ntt) ; %, "uint8") ;
    nn2 = zeros(1, ntt) ; %, "uint8") ;
    
    cp1 = zeros(nmax+1, ntt) ;
    cq1 = zeros(1, ntt) ;
    
    cp2 = zeros(nmax+1, ntt) ;
    cq2 = zeros(1, ntt) ;
    
    % numero di simulazioni Montecarlo
    %Nmc = 1000 ;
    
    % ID delle macchine
    ID_M1 = 1 ; ID_M2 = 2 ; ID_M3 = 3 ;
    
    % Simula Nmc volte la dinamica del sistema e conta la frequzenza con cui i
    % due buffer si vengono a trovare nei vari stati 
    for imc = [ 1 : Nmc ]
    
        % Costruzione dei vettori con i tempi di rottura (casuali) delle
        % tre macchine nell'intervallo [0, tmax]
        tf1 = get_time_failures(MTBF_M1, tmax) ;
        tf2 = get_time_failures(MTBF_M2, tmax) ;
        tf3 = get_time_failures(MTBF_M3, tmax) ;
        
        % Conteggio delle rotture di ciascuna macchina nell'intervallo [0, tmax]
        nf1 = length(tf1) ;
        nf2 = length(tf2) ;
        nf3 = length(tf3) ;
        
        % Numero totale degli "eventi" (eventi = stato iniziale + rotture)
        nf = 1 + nf1 + nf2 + nf3 ; 
        
        % Ordinamento cronologico di tutti gli eventi e creazione di un vettore
        % che contiene la sequenza degli ID delle macchine che si rompono.
        % Il vettore tf contiene i tempi di occorrenza di tutti gli eventi
        % (in ordine cronologico); il vettore id_evento contiene i
        % corrispondenti ID delle macchine. L'elemento idx(k) è l'ID (1, 2 o 3)
        % della macchina che si rompe all'istante tf(k)
        [ tf, idx ] = sort([0, tf1, tf2, tf3]) ;
        idx1 = (idx<=nf1+1) * ID_M1 ;
        idx2 = (idx>(nf1+1) & idx<=(nf1+nf2+1)) * ID_M2 ;
        idx3 = (idx>(nf1+nf2+1)) * ID_M3 ;
        id_evento = idx1 + idx2 + idx3 ;
        
        % Inizializzazione dei vettori che contengono gli stati dei buffer.
        % n1(k) e n2(k) conterranno gli stati in cui vengono a trovarsi i due
        % buffer all'istante tf(k), ovvero in seguito al verificarsi 
        % dell'evento k-esimo
        n1 = zeros(1, nf) ;
        n2 = zeros(1, nf) ;
        
        % L'evento all'istante tf(1) è lo stato iniziale (deterministico) 
        n1(1) = n01 ;
        n2(1) = n02 ;
        
        % Calcolo dell'evoluzione dei buffer 
        for k = [ 2 : nf ]
        
            if (id_evento(k) == ID_M1) % l'evento k-esimo è la rottura di M1
                n1(k) = n1(k-1)-1 ;
                n2(k) = n2(k-1) ;
            elseif (id_evento(k) == ID_M2) % l'evento k-esimo è la rottura di M2
                n1(k) = n1(k-1) ;
                n2(k) = n2(k-1) - 1 ;
            elseif (id_evento(k) == ID_M3) % l'evento k-esimo è la rottura di M3
                n1(k) = n1(k-1) + 1 ;
                n2(k) = n2(k-1) + 1 ;
            endif 
        
        endfor

        % Costruzione dei vettori nn1 e nn2 che contengono gli stati dei due 
        % buffer agli istanti temporali (equidistanti) tt(i) = i*dt, con
        % 1 <= i <= ntt  e tt(ntt) = tmax.
        % Gli 'nf' eventi sono distribuiti casualmente nell'intervallo 
        % [0, tmax], e occorrono agli istanti tf(k), con 1 <= k <= nf.
        % Gli stati dei due buffer sono costanti fra un evento e il successivo, 
        % per cui all'istante tt(i) i due buffer si trovano negli stati
        % n1(k) e n2(k), con k tale che: tf(k) <= tt(i) < tf(k+1)
        idt = [ 1 ] ;
        for i = [ 2 : ntt ]
            idt(i) = sum(tf<tt(i)) ;
        endfor
        nn1 = n1(idt) ;
        nn2 = n2(idt) ;
        
        % Analisi dei vettori nn1 e nn2 che contengono gli stati dei due buffer
        % nella simulazione corrente. All'istante tt(i) il buffer 1 si trova
        % nello stato ib1=nn1(i). Se 0<ib1<nmax viene incrementato di 1 il 
        % contatore cb1(ib1, i). Se ib1<0 (ovvero il buffer è in uno stato tale
        % da richiedere l'interruzione del servizio), viene aumentato di 1 il 
        % contatore cx1(i). Analogo discorso per il buffer 2.

        for i = [ 1 : ntt ]
        
            ib1 = nn1(i) ;
            if (ib1>=0 && ib1<nmax)
                cp1(ib1+1, i) += 1 ;
            elseif (ib1>=nmax)
                cp1(nmax+1, i) += 1 ;
            else
                cq1(i) += 1 ;
            endif
            
            ib2 = nn2(i) ;
            if (ib2>=0 && ib2<nmax)
                cp2(ib2+1, i) += 1 ;
            elseif (ib2>=nmax)
                cp2(nmax+1, i) += 1 ;
            else
                cq2(i) += 1 ;
            endif
            
        endfor
        
    endfor
    
    % Calcolo della frequenza relativa con cui i buffer 1 e 2 si trovano nei 
    % vari stati. 
    % NOTA: La frequenza relativa tende alla probabilità quando il numero
    % di campioni (Nmc) tende all'infinito 
    p1 = cp1 / Nmc ;
    q1 = cq1 / Nmc ;
    
    p2 = cp2 / Nmc ;
    q2 = cq2 / Nmc ;
    
endfunction


% esempio d'uso:
%
% n01 = 3 ;
% n02 = 5 ;
% [p1, q1, p2, q2, tt] = mc_buffer(n01, n02, MTBF_M1=36*60, MTBF_M2=36*60, MTBF_M3=36*60, Nmc=1000, tmax=6000, dt=20);
% figure(1); plot(tt, p1(n01+1,:), '.-') % plot delle frequenze relative (~probabilità) dello stato n01 di M1
% figure(2); plot(tt, p2(n02+1,:), '.-') % plot delle frequenze relative (~probabilità) dello stato n02 di M2


% altro esempio (confrontare il grafico generato con il primo grafico in 
%   https://github.com/mentrelli/pybuffer/blob/master/pybuffer.ipynb )
% 
% Attenzione: queste simulazioni con Matlab/Octave possono richiedere 
% qualche minuto di tempo!
% [p1, q1, p2, q2, tt] = mc_buffer(3, 3, 36*60, 36*60, 36*60, 5000, 6000, 10);
% figure(3); plot(tt, p1(1:6,:),'.-')


% I risultati salvati nel file "fortran_data.m" sono stati ottenuti con
% il codice Fortran con il seguente comando (su Linux):
%
%   $ ./f_mc_buffer.exe 3 4 2160 2160 2160 1000000 fortran_data.m 3000 10
%
% che corrisponde al seguenti dati di input:
%   n01 = 3, n02 = 4, MTBF_M1 = 36*60, MTBF_M2 = 36*60, MTBF_M3 = 36*60,
%   Nmc = 1000000, tmax = 3000, dt = 10
% (1 milione di iterazioni eseguite in 4 secondi su un Intel i7 920)
%
% Per visualizzare i risultati contenuti nel file "fortran_data.m" 
% eseguire, per esempio, in Matlab/Octave:
%
%   run fortran_data.m
%   figure(4); plot(tt, p1(1:6,:), '.-')

