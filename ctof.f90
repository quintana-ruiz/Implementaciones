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
	
program ctof
use basic_types, only: int4, real8

	implicit none
	
	character(len=1) :: temp
	integer(int4), parameter :: unit = 6
	real(real8) :: celsius, fahrenheit, kelvin 
	
	write(unit,'(/A)')     "-----------------------------------------------"
	write(unit,'(8X,A31)') "Program to convert temperatures"
	write(unit,'(A)')      "-----------------------------------------------"
	
	write(unit,'(/A50)') "Do you want to convert to Fahrenheit or to Kelvin?"
	write(unit,'(A38)',advance='no') "Press F to Fahrenheit or K to Kelvin: "
	read(*,'(A1)') temp

	if(temp == "F" .or. temp == "f") then
		! Input the temperature
		write(unit,'(/A33)',advance='no') "Input the temperature in Celsius "
		read(*,*) celsius
		
		fahrenheit = (celsius * 9.0_real8/5.0_real8) + 32.0_real8

		write(unit,'(/A34,F8.2,A1)') "The temperature in Fahrenheit is: ", fahrenheit, "F" 		
	elseif(temp == "K" .or. temp == "k") then
		! Input the temperature
		write(unit,'(/A33)',advance='no') "Input the temperature in Celsius "
		read(*,*) celsius
		
		kelvin = celsius + 273.15_real8
		
		write(unit,'(/A30,F10.2,A1)') "The temperature in Kelvin is: ", Kelvin, "K"
	else
		write(unit,'(//A38)') "Warning: Check again the input values!"
	end if	
	
	write(unit,'(/A)') "-----------------------------------------------"
	write(unit,'(19X,A1,2X,A1,2X,A1)') "*","*","*"
	
	pause
	
end program ctof	
	
	
	