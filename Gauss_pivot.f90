module basic_types

    implicit none
	
    private
	
    public :: int4, real8
	
    ! define basic types
    integer, parameter :: int4	= selected_int_kind(8)
    integer, parameter :: real8 = selected_real_kind(p=15)
	
end module basic_types
	
module io_files
use basic_types
	
	implicit none
	
	private
	
	public :: output_unit
	
	! define basec units
	integer(int4), parameter :: output_unit = 6 ! sreen
	
end module io_files	
	
program Gauss_pivot
use io_files
use basic_types

    implicit none 
    
    integer(int4) :: m, n
    integer(int4) :: i, j, k, pivot_row
    real(real8), dimension(:,:), allocatable :: A
    real(real8), dimension(:), allocatable :: B, X      
    real(real8) :: pivot, multiplier, temp
    real(real8), parameter :: pi = 4.0_real8*atan(1.0_real8)
	
    m = 3; n = 3 ! Size of the sistem
    
    ! Allocate the matrix
    allocate(A(m,n))
    allocate(B(m))
    allocate(X(m))      	
	
    A = reshape([2.0_real8, 1.0_real8, 4.0_real8, 4.0_real8, 2.0_real8, 4.0_real8, 3.0_real8, -2.0_real8, 3.0_real8], [m,n])
    B = [1.0_real8, 11.0_real8, 3.0_real8]
    
    ! ---------------------------------------
    ! Task: do the same by an input file
    ! ---------------------------------------

    ! Plot the matrix on screen
    write(output_unit,'(/5X,A16/)') "The matrix A is:"
    do i = 1, n		
    	write(output_unit,'(5X,6F8.4)') A(i,:)		
    end do
	
    ! Plot the vector on screen
    write(output_unit,'(/5X,A16/)') "The vector B is:"
    do i = 1, n		
    	write(output_unit,'(5X,F10.4)') B(i)		
    end do 	
	
    ! Elimination of Gauss with pivoting
    do k = 1, n-1
        pivot = abs(A(k,k))
        pivot_row = k

        ! Find the maximum pivot in column k
        do i = k+1, m
            if (abs(A(i,k)) > pivot) then
                pivot = abs(A(i,k))
                pivot_row = i
            end if
        end do

        ! Swap rows to put the maximum pivot in row k
        if (pivot_row /= k) then
            do j = 1, n
                temp = A(k,j)
                A(k,j) = A(pivot_row,j)
                A(pivot_row,j) = temp
            end do
            temp = B(k)
            B(k) = B(pivot_row)
            B(pivot_row) = temp
        end if

        ! Elimination of elements below the pivot
        do i = k+1, m
            multiplier = A(i,k) / A(k,k)
            do j = k, n
                A(i,j) = A(i,j) - multiplier * A(k,j)
            end do
            B(i) = B(i) - multiplier * B(k)
        end do
    end do

    ! Back substitution to find the values of the variables
    X(n) = B(n) / A(n,n)
    do i = n-1, 1, -1
        X(i) = B(i)
        do j = i+1, n
            X(i) = X(i) - A(i,j) * X(j)
        end do
        X(i) = X(i) / A(i,i)
	end do

    ! Display the solution on screen
    write(output_unit,'(//5X,A16/)') "The solution is:"
    do i = 1, n		
    	write(output_unit,'(5X,A1,I1,A1,X,F10.4)') "x", i, "=", X(i)		
    end do
	
    write(output_unit,*)
end program Gauss_pivot
