
program main

	implicit none

	integer :: argc

	write(*,*) 'cali 0.0.1'
	write(*,*)

	argc = command_argument_count()
	print *, 'argc = ', argc

end program main

