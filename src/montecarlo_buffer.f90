!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! Compile with:
!!!   $ make
!!!
!!! (tested with gfortran 4.9.2 and gfortran 5.1.1 on x86_64 GNU/Linux)
!!!
!!! Usage:
!!!   pass the input parameters from the cmd line in the following order
!!!      n01, n02, MTBF_M1, MTBF_M2, MTBF_M3, Nmc, tmax, dt
!!!  
!!! Example of usage:
!!!    $ ./f_mc_buffer 3 3 2160 2160 2160 100000 out.m 
!!!
!!!                                    by AM / October 17, 2015
!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM f_mc_buffer

    use, intrinsic :: iso_fortran_env
    !use omp_lib
    
    use qsort_c_module
    use distributions_mod

    implicit none
    
    ! limite temporale della simulazione [min]
    double precision :: tmax 
    
    ! intervallo di tempo per la raccolta dei dati [min]
    double precision :: dt
    
    ! MTBF delle 3 macchine
    double precision :: MTBF_M1, MTBF_M2, MTBF_M3
    
    ! stato iniziale di riempimento dei due buffer
    integer :: n01, n02
    
    ! numero di samples della simulazione Montecarlo
    integer :: Nmc
    
    ! nome del file di output (Matlab/Octave)
    character(len=:), allocatable :: filename
    
    ! contatori degli stati dei buffer / frequenze relative
    double precision, allocatable :: p1(:,:), q1(:), p2(:,:), q2(:)
    
    ! vettori con gli istanti temporali degli eventi di rottura e
    ! numero di eventi di rottura su [0, tmax]
    double precision, allocatable :: tf1(:), tf2(:), tf3(:), tf(:)
    integer :: nf1, nf2, nf3, nf
    
    ! vettore con gli istanti temporali equidistanti su [0, tmax] e
    ! e numero di componenti
    double precision, allocatable :: tt(:)
    integer :: ntt
    
    ! vettori con gli stati temporali agli istanti tf
    integer, allocatable :: n1(:), n2(:)
    
    ! stati dei buffer agli istanti temporali tt
    integer, allocatable :: nn1(:), nn2(:)
    
    ! variabili ausiliarie
    integer, allocatable :: idx(:), idx_init(:), id_evento(:), idt(:)
    integer :: nfmax1, nfmax2, nfmax3, nfmax 
    integer :: ib1, ib2, i, k, m
    integer, parameter :: ifile = 6
    
    ! ID delle macchine
    integer, parameter :: ID_M1 = 1
    integer, parameter :: ID_M2 = 2
    integer, parameter :: ID_M3 = 3
    
    ! numero massimo di stati registrati del buffer
    integer, parameter :: nmax = 20
    
    logical :: flag_get_failures
    logical :: input_from_command_line
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Input data                                                   !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    input_from_command_line = .true.
    
    if (input_from_command_line) then
    
        call get_input
        
    else
    
        n01 = 3 ! stato iniziale del buffer 1
        n02 = 3 ! stato iniziale del buffer 2
        
        MTBF_M1 = 36*60 ! MTBF macchina 1 [min]
        MTBF_M2 = 36*60 ! MTBF macchina 2 [min]
        MTBF_M3 = 36*60 ! MTBF macchina 3 [min]
        
        Nmc = 1000000 ! numero di simulazioni Montecarlo
        
        tmax = 6000 ! limite temporale della simulazione [min]
        dt = 10 ! intervallo di tempo per la raccolta dei dati [min]

        filename = "fortran_data.m" ! file di output (Matlab/Octave)
    
    end if
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ! t1 = omp_get_wtime()
    
    ntt = int(tmax/dt) + 1
    
    ! stima (per eccesso) del numero di rotture delle macchine
    ! (i vettori vengono allocati una volta per tutte con dimensioni 
    ! largamente sovrastimata, per evitare l'allocazione dinamica 
    ! all'interno del ciclo Montecarlo, che sarebbe molto onerosa)
    nfmax1 = 10*int(tmax/MTBF_M1)+1
    nfmax2 = 10*int(tmax/MTBF_M2)+1
    nfmax3 = 10*int(tmax/MTBF_M3)+1
    nfmax = nfmax1 + nfmax2 + nfmax3 + 1
    
        
    ! allocazione array per raccogliere i dati della m-esima simulazione
    allocate(tf1(nfmax1), tf2(nfmax2), tf3(nfmax3), tf(nfmax))
    allocate(n1(nfmax), n2(nfmax))
    allocate(idx(nfmax), idx_init(nfmax), id_evento(nfmax), idt(ntt))
    
    ! L'evento all'istante tf(1) è lo stato iniziale (deterministico) 
    tf(1) = 0
    
    ! inizializzatore degli indici   
    do i = 1, nfmax
        idx_init(i) = i
    end do
    
    ! allocazione array per raccogliere i dati aggregati
    allocate(tt(ntt))
    allocate(nn1(ntt), nn2(ntt))
    allocate(p1(nmax+1, ntt), q1(ntt), p2(nmax+1, ntt), q2(ntt))
    
    ! reset dei contatori / frequenze relative
    p1 = 0 ; q1 = 0 ; p2 = 0 ; q2 = 0 ;
    
    ! vettore con gli istanti temporali per i dati aggregati
    do i = 1, ntt
        tt(i) = (i-1)*dt
    end do
        
    ! L'evento all'istante tf(1) è lo stato iniziale (deterministico) 
    n1(1) = n01
    n2(1) = n02
    
    ! inizializzazione del generatore di numeri pseudo-casuali
    call init_random_seed()
    
    
    ! Ciclo Montecarlo:
    !   Viene simulata Nmc volte la dinamica del sistema e vengono
    !   contate le frequzenze con cui i buffer si vengono a trovare nei 
    !   vari stati
        
    do m = 1, Nmc
        
        ! Costruzione dei vettori con i tempi di rottura (casuali) delle
        ! tre macchine nell'intervallo [0, tmax] e conteggio del
        ! numero di rotture
        flag_get_failures = .true.
        do while (flag_get_failures)
        
            call get_time_failures(MTBF_M1, tmax, nfmax1, tf1, nf1)
            call get_time_failures(MTBF_M2, tmax, nfmax2, tf2, nf2)
            call get_time_failures(MTBF_M3, tmax, nfmax3, tf3, nf3)
        
            ! Numero totale degli "eventi" 
            ! (eventi = stato iniziale + rotture)
            nf = 1 + nf1 + nf2 + nf3 
            
            ! Se le rotture sono superiori al numero massimo allocato
            ! (capita molto, ma molto di rado, allora si ripete la
            ! generazione casuali dei tempi di occorrenza)        
            if ( (nf1<0) .or. (nf2<0) .or. (nf3<0) ) then
                flag_get_failures = .true.
            else
                flag_get_failures = .false.
            end if
            
        end do
        
        ! Ordinamento cronologico di tutti gli eventi e creazione di un 
        ! vettore che contiene la sequenza degli ID delle macchine che 
        ! si rompono. Il vettore tf contiene i tempi di occorrenza di 
        ! tutti gli eventi (in ordine cronologico); il vettore id_evento
        ! contiene i corrispondenti ID delle macchine. L'elemento 
        ! idx(k) è l'ID (1, 2 o 3) della macchina che si rompe 
        ! all'istante tf(k)
        
        tf(2:nf) = [ tf1(1:nf1), tf2(1:nf2), tf3(1:nf3) ]
        
        call QsortC_idx(tf(1:nf), idx(1:nf))
        
        do i = 1, nf
            if (idx(i)<=nf1+1) then
                id_evento(i) = ID_M1
            elseif (idx(i)<=(nf1+nf2+1)) then
                id_evento(i) = ID_M2
            else
                id_evento(i) = ID_M3
            end if
        end do
        
        ! Calcolo dell'evoluzione dei buffer 
        do k = 2, nf
        
            if (id_evento(k) == ID_M1) then ! rottura di M1
                n1(k) = n1(k-1)-1 
                n2(k) = n2(k-1) 
            elseif (id_evento(k) == ID_M2) then ! rottura di M2
                n1(k) = n1(k-1) 
                n2(k) = n2(k-1) - 1 
            elseif (id_evento(k) == ID_M3) then ! rottura di M3
                n1(k) = n1(k-1) + 1 
                n2(k) = n2(k-1) + 1 
            end if 
        
        end do

        ! Costruzione dei vettori nn1 e nn2 che contengono gli stati dei 
        ! buffer agli istanti temporali (equidistanti) tt(i) = i*dt, con
        ! 1 <= i <= ntt  e tt(ntt) = tmax.
        ! Gli 'nf' eventi sono distribuiti casualmente nell'intervallo 
        ! [0, tmax], e occorrono agli istanti tf(k), con 1 <= k <= nf.
        ! Gli stati dei due buffer sono costanti fra un evento e il 
        ! successivo, per cui all'istante tt(i) i due buffer si trovano
        ! negli stati n1(k) e n2(k), con k tale che tf(k)<=tt(i)<tf(k+1)
        
        idt(1) = 1
        do i = 2, ntt
            idt(i) = count(tf(1:nf)<tt(i)) 
        end do
        nn1 = n1(idt) 
        nn2 = n2(idt) 
        
        ! Analisi dei vettori nn1 e nn2 che contengono gli stati dei due
        ! buffer nella simulazione corrente. All'istante tt(i) il 
        ! buffer 1 si trova nello stato ib1=nn1(i). Se 0<ib1<nmax viene
        ! incrementato di 1 il contatore cb1(ib1, i). Se ib1<0 (ovvero 
        ! il buffer è in uno stato tale da richiedere l'interruzione del
        ! servizio), viene aumentato di 1 il contatore cx1(i). 
        ! Analogo discorso per il buffer 2.

        do i = 1, ntt
        
            ib1 = nn1(i) 
            if (ib1>=0 .and. ib1<nmax) then
                p1(ib1+1, i) = p1(ib1+1, i) + 1
            elseif (ib1>=nmax) then
                p1(nmax+1, i) = p1(nmax+1, i) + 1 
            else
                q1(i) = q1(i) + 1 
            endif
            
            ib2 = nn2(i) 
            if (ib2>=0 .and. ib2<nmax) then
                p2(ib2+1, i) = p2(ib2+1, i) + 1 
            elseif (ib2>=nmax) then
                p2(nmax+1, i) = p2(nmax+1, i) + 1 
            else
                q2(i) = q2(i) + 1 
            endif
            
        end do
        
    end do
    
    ! Calcolo della frequenza relativa con cui i buffer 1 e 2 si trovano
    ! nei vari stati. 
    
    ! NOTA: La frequenza relativa tende alla probabilità quando il 
    ! numero di campioni (Nmc) tende all'infinito
    
    p1 = p1 / Nmc 
    q1 = q1 / Nmc 
    
    p2 = p2 / Nmc 
    q2 = q2 / Nmc
    
    ! salva l'output su un file testo da importare in Matlab/Octave
    call write_data_to_file(filename)
    
    
CONTAINS


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Read input from command line                                 !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE get_input
    
        integer :: narg, istat
        character(len=64) :: arg, tmpfilename
        logical, parameter :: verbose = .true.
        
        narg = command_argument_count()
        
        call get_command_argument(1, arg)
        read(arg, *, iostat=istat) n01
        if (istat/=0) stop 'Invalid initial state of buffer 1 (n01)'
        if (verbose) write(*, 201) "n01      : ", n01
        
        call get_command_argument(2, arg)
        read(arg, *, iostat=istat) n02
        if (istat/=0) stop 'Invalid initial state of buffer 2 (n02)'
        if (verbose) write(*, 201) "n02      : ", n02
        
        call get_command_argument(3, arg)
        read(arg, *, iostat=istat) MTBF_M1
        if (istat/=0) stop 'Invalid MTBF of machine 1 (MTBF_M1)'
        if (verbose) write(*, 211) "MTBF_M1  :", MTBF_M1
        
        call get_command_argument(4, arg)
        read(arg, *, iostat=istat) MTBF_M2
        if (istat/=0) stop 'Invalid MTBF of machine 2 (MTBF_M2)'
        if (verbose) write(*, 211) "MTBF_M2  :", MTBF_M2
        
        call get_command_argument(5, arg)
        read(arg, *, iostat=istat) MTBF_M3
        if (istat/=0) stop 'Invalid MTBF of machine 3 (MTBF_M3)'
        if (verbose) write(*, 211) "MTBF_M3  :", MTBF_M3
        
        call get_command_argument(6, arg)
        read(arg, *, iostat=istat) Nmc
        if (istat/=0) stop 'Invalid Numer of Montecarlo simulations (Nmc)'
        if (verbose) write(*, 201) "Nmc      : ", Nmc
        
        call get_command_argument(7, arg)
        read(arg, *, iostat=istat) tmpfilename
        if (istat/=0) stop 'Invalid name of output file (filename)'
        filename = trim(tmpfilename)
        if (verbose) write(*,'(3a)') "filename : '", filename, "'"
        
        if (narg>7) then
            call get_command_argument(8, arg)
            read(arg, *, iostat=istat) tmax
            if (istat/=0) stop 'Invalid maximum time (tmax)'
        else
            tmax = 6000
        end if
        if (verbose) write(*, 211) "tmax     :", tmax
        
        if (narg>8) then
            call get_command_argument(9, arg)
            read(arg, *, iostat=istat) dt
            if (istat/=0) stop 'Invalid time interval dt (dt)'
        else
            dt = 10
        end if
        if (verbose) write(*, 211) "dt       :", dt
        
        201 format(a,i0)
        211 format(a,e15.8)
        
    END SUBROUTINE get_input
    

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Costruzione dei vettori con gli istanti temporali degli      !!!
    !!! eventi di rottura (processo di Poisson)                      !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE get_time_failures(MTBF, tmax, nfmax, tf, nf)
        
        double precision, intent(in) :: MTBF
        double precision, intent(in) :: tmax
        integer, intent(in) :: nfmax 
        double precision, intent(out) :: tf(:)
        integer, intent(out) :: nf
        double precision :: t
        
        nf = 0
        t = rand_exp(MTBF)
                
        do while ( t < tmax .and. nf<nfmax )
            nf = nf + 1
            tf(nf) = t           
            t = t + rand_exp(MTBF)
        end do
        
        if (t < tmax .and. nf==nfmax) nf = -1
    
    END SUBROUTINE get_time_failures
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Scrittura su file dei dati della simulazione                 !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE write_data_to_file(filename)
    
        character(*), intent(in) :: filename
        character(len=6) :: stmp
        character(len=:), allocatable :: sntt
        integer :: ios

        open(unit=ifile, file=filename, action="write", iostat=ios)
        
        if (ios/=0) then
            write(0,'(3a)')"*** ERROR: Cannot Open '", filename, "' ***"
            stop 
        end if
        
        write(ifile, 101) "Nmc = ", Nmc, " ;"
        write(ifile, 121) "MTBF_M1 = ", MTBF_M1, " ;"
        write(ifile, 121) "MTBF_M2 = ", MTBF_M2, " ;"
        write(ifile, 121) "MTBF_M3 = ", MTBF_M3, " ;"
        write(ifile, 101) "n01 = ", n01, " ;"
        write(ifile, 101) "n02 = ", n02, " ;"
        write(ifile, 101) "nmax = ", nmax, " ;"
        write(ifile, 121) "tmax = ", tmax, " ;"
        write(ifile, 121) "dt = ", dt, " ;"
        write(ifile, *)
        
        write(stmp, '(i0)') ntt
        sntt = trim(stmp)
        
        write(ifile, '(a,'//sntt//'(e15.8),a)') "p1 = [ ", p1(1,:), " ;"
        do i = 2, nmax
            write(ifile, '('//sntt//'(e15.8),a)') p1(i,:), " ;"
        end do
        write(ifile, '('//sntt//'(e15.8),a)') p1(nmax+1,:), " ] ;"
        write(ifile, *)
        
        write(ifile, '(a,'//sntt//'(e15.8),a)') "q1 = [ ", q1, " ];"
        write(ifile, *)
        
        write(ifile, '(a,'//sntt//'(e15.8),a)') "p2 = [ ", p2(1,:), " ;"
        do i = 2, nmax
            write(ifile, '('//sntt//'(e15.8),a)') p2(i,:), " ;"
        end do
        write(ifile, '('//sntt//'(e15.8),a)') p2(nmax+1,:), " ] ;"
        write(ifile, *)
        
        write(ifile, '(a,'//sntt//'(e15.8),a)') "q2 = [ ", q2, " ];"
        write(ifile, *)
        
        write(ifile, '(a,'//sntt//'(e15.8),a)') "tt = [ ", tt, " ];"
        
        101 format(1(a,i0),a)
        121 format(1(a,e15.8),a)
        
        close(unit=ifile, iostat=ios)
        
        if (ios/=0) then
            write(0,'(2(a,i0),a)') "*** ERROR: Cannot Close Unit ", &
                  ifile, " (status: ", ios, ") ***"
            stop 
        end if
        
    END SUBROUTINE write_data_to_file
    
END PROGRAM f_mc_buffer
