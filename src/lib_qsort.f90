! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Cormen et al., Introduction to Algorithms,
! 1997 printing

! Made F conformant by Walt Brainerd

! Modified by Undy on October 17, 2015

MODULE qsort_c_module

    implicit none
    
    public :: QsortC
    public :: QsortC_idx
    private :: Partition
    
    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = selected_real_kind(2*precision(1.0_sp))
    
    integer, parameter :: pr = dp
    
CONTAINS

    SUBROUTINE QsortC_idx(A, idx)
    
        real(pr), intent(inout), dimension(:) :: A
        integer, intent(out), dimension(size(A)), optional :: idx
        integer :: i
        
        if (present(idx)) then
            do i = 1, size(A)
                idx(i) = i
            end do
            call QsortC(A, idx)
        else
            call QsortC(A)
        end if
        
    END SUBROUTINE QsortC_idx


    RECURSIVE SUBROUTINE QsortC(A, idx)
        
        real(pr), intent(inout), dimension(:) :: A
        integer, intent(inout), dimension(:), optional :: idx
        integer :: iq
        
        if (present(idx)) then
            if(size(A) > 1) then
                call Partition(A, iq, idx)
                call QsortC(A(:iq-1), idx(:iq-1))
                call QsortC(A(iq:), idx(iq:))
            endif
        else
            if(size(A) > 1) then
                call Partition(A, iq)
                call QsortC(A(:iq-1))
                call QsortC(A(iq:))
            endif
        end if
        
    END SUBROUTINE QsortC
    
    
    SUBROUTINE Partition(A, marker, idx)
    
        real(pr), intent(in out), dimension(:) :: A
        integer, intent(out) :: marker
        integer, intent(inout), dimension(:), optional :: idx
        integer :: i, j
        real(pr) :: temp
        real(pr) :: x      ! pivot point
        integer :: itemp
        
        x = A(1)
        i = 0
        j = size(A) + 1
        
        do
            j = j-1
            do
                if (A(j) <= x) exit
                j = j-1
            end do
            i = i + 1
            do
                if (A(i) >= x) exit
                i = i + 1
            end do
            if (i < j) then ! exchange A(i) and A(j)
                temp = A(i)
                A(i) = A(j)
                A(j) = temp
                if (present(idx)) then
                    itemp = idx(i)
                    idx(i) = idx(j)
                    idx(j) = itemp
                end if
            elseif (i == j) then
                marker = i + 1
                return
            else
                marker = i
                return
            endif
            
        END do
        
    END SUBROUTINE Partition
    
END MODULE qsort_c_module
