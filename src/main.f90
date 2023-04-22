
!===============================================================================

module cali_m

	integer :: EXIT_SUCCESS = 0, EXIT_FAILURE = -1

end module cali_m

!===============================================================================

program main

	implicit none

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc

	write(*,*) 'cali 0.0.1'
	write(*,*)

	argc = command_argument_count()
	print *, 'argc = ', argc
	if (argc /= 1) then
		write(*,*) 'Error: bad cmd args'
		write(*,*) 'Usage:'
		write(*,*) '	cali path/to/font.ttf'
		write(*,*)
		call exit(EXIT_FAILURE)
	end if

	call get_command_argument(1, buffer, iostat = io)
	if (io /= EXIT_SUCCESS) then
	end if
	argv = trim(buffer)
	print *, 'argv = ', argv

	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

