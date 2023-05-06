module basic_types

	implicit none
	
	private
	
	public :: int1, int2, int4, real2, real4, real8
	
	! define basic types
	integer, parameter :: int1	= selected_int_kind(2)
	integer, parameter :: int2	= selected_int_kind(4)
	integer, parameter :: int4	= selected_int_kind(8)
	integer, parameter :: real2 = selected_real_kind(p=3)
	integer, parameter :: real4 = selected_real_kind(p=6)
	integer, parameter :: real8 = selected_real_kind(p=15)
	
end module basic_types
	
program fibo
use basic_types, only: int4, real8

    implicit none

    integer(int4) :: n, i
    integer(int4), parameter :: unit = 6

    write(unit,'(/A)')     "-----------------------------------------------"
    write(unit,'(18X,A9)') "Fibonacci"
    write(unit,'(A)')      "-----------------------------------------------"
	
    write(unit,'(A51)',advance='no') "Enter the number of Fibonacci numbers to generate: "	
    read(*,'(I2)') n

    write(unit,'(A19)') "Fibonacci sequence:"
    
    do i = 0, n - 1
    	write(unit,'(19X,I2)') fibonacci(i)
    end do
	
    write(unit,*)

    contains

    recursive function fibonacci(num) result(fib)
        integer(int4), intent(in) :: num
        integer(int4) :: fib

        if (num == 0) then
            fib = 0
        else if (num == 1) then
            fib = 1
        else
            fib = fibonacci(num - 1) + fibonacci(num - 2)
        end if

    end function fibonacci

end program fibo
