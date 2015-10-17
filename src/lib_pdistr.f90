MODULE distributions_mod

    implicit none
    
    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = selected_real_kind(2*precision(1.0_sp))
    
    integer, parameter :: pr = dp
    
    private
    
    public :: rand_exp
    public :: rand_weibull
    public :: init_random_seed
    

CONTAINS


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Exponential distribution                                     !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    FUNCTION rand_exp(meanval) result (r)
    
        real(pr), intent(in) :: meanval  ! meanval = 1/lambda
        real(pr) :: r, tmp
        
        !if (meanval <= 0) return 0
        
        !call init_random_seed()
        call random_number(tmp)
        r = -meanval*log(tmp)
        
    END FUNCTION rand_exp
    
    
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !!! Weibull distribution                                         !!!
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     FUNCTION rand_weibull(shape_r, scale_r) result (r)
     
         real(pr), intent(in) :: shape_r, scale_r
         real(pr) :: r, tmp
         
         !if (scale_r <= 0.0d0) then
         !    write(*,*) "scale_r must be positive"
         !elseif (shape_r <= 0.0d0) then 
         !    write(*,*) "scale_r must be positive"
         !else
             !call init_random_seed()
             call random_number(tmp)
             r = scale_r*(-log(tmp))**(1.0d0/shape_r)
         !end if
         
     END FUNCTION rand_weibull
    
    
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    !!! Initialization of random seed                                !!!
!    !!! (https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html) !
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE init_random_seed()
    
!        use iso_fortran_env, only: int64
        
!        integer, allocatable :: seed(:)
!        integer :: i, n, un, istat, dt(8), pid
!        integer(int64) :: t
          
!        call random_seed(size = n)
!        allocate(seed(n))
!        ! First try if the OS provides a random number generator
!        open(newunit=un, file="/dev/urandom", access="stream", &
!                form="unformatted", action="read", status="old", iostat=istat)
!        if (istat == 0) then
!            read(un) seed
!            close(un)
!        else
!            ! Fallback to XOR:ing the current time and pid. The PID is
!            ! useful in case one launches multiple instances of the same
!            ! program in parallel.
!            call system_clock(t)
!            if (t == 0) then
!                call date_and_time(values=dt)
!                t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
!                    + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
!                    + dt(3) * 24_int64 * 60 * 60 * 1000 &
!                    + dt(5) * 60 * 60 * 1000 &
!                    + dt(6) * 60 * 1000 + dt(7) * 1000 &
!                    + dt(8)
!            end if
!            pid = getpid()
!            t = ieor(t, int(pid, kind(t)))
!            do i = 1, n
!                seed(i) = lcg(t)
!            end do
!        end if
!        call random_seed(put=seed)
        
!        contains
!        ! This simple PRNG might not be good enough for real work, but is
!        ! sufficient for seeding a better PRNG.
        
!        function lcg(s)
        
!            integer :: lcg
!            integer(int64) :: s
!            if (s == 0) then
!                s = 104729
!            else
!                s = mod(s, 4294967296_int64)
!            end if
!            s = mod(s * 279470273_int64, 4294967291_int64)
!            lcg = int(mod(s, int(huge(0), int64)), kind(0))
!        end function lcg

!    END SUBROUTINE init_random_seed
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Initialization of random seed                                !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    SUBROUTINE init_random_seed()
    
        integer, parameter :: ndim = 100
        integer, dimension(ndim) :: put
        integer, dimension(8) :: dt
        integer :: nrseed_size
        
        call random_seed(size=nrseed_size)
        
        if (nrseed_size > ndim) stop 'ndim is too small'
        
        call date_and_time(values=dt)
        dt = dt(8:1:-1) ! reverse order of array elements
        
        ! safe if seed size is larger than 8
        put(1:nrseed_size) = reshape(dt, (/ nrseed_size /), pad=dt) 
        
        call random_seed(put=put(1:nrseed_size))
        
    END SUBROUTINE init_random_seed


END MODULE distributions_mod
