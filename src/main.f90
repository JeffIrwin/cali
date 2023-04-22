
!===============================================================================

program main

	use cali_m

	implicit none

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc, io

	type(ttf_t) :: ttf

	write(*,*) 'cali 0.0.1'
	write(*,*)

	argc = command_argument_count()
	print *, 'argc = ', argc
	if (argc /= 1) then
		write(*,*) 'Error: bad cmd args'
		write(*,*) 'Usage:'
		write(*,*) '	cali path/to/font.ttf'
		call exit(EXIT_FAILURE)
	end if

	call get_command_argument(1, buffer, status = io)
	if (io /= EXIT_SUCCESS) then
		write(*,*) 'Error: cannot get cmd arg'
		call exit(EXIT_FAILURE)
	end if
	argv = trim(buffer)
	print *, 'argv = ', argv

	ttf = read_ttf(argv)

	print *, 'num_tables  = ', ttf%num_tables
	print *, 'tag 1       = ', ttf%tables(1)%tag

	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

