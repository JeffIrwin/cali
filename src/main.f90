
!===============================================================================

module cali_m

	integer :: EXIT_SUCCESS = 0, EXIT_FAILURE = -1

end module cali_m

!===============================================================================

program main

	use cali_m

	implicit none

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc, io, ittf

	integer(kind = 4) :: scalar_type
	integer(kind = 2) :: num_tables

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
		write(*,*) 'Error: get cmd arg'
		call exit(EXIT_FAILURE)
	end if
	argv = trim(buffer)
	print *, 'argv = ', argv

	open(newunit = ittf, file = argv, action = 'read', iostat = io, access = 'stream')
	if (io /= EXIT_SUCCESS) then
		write(*,*) 'Error: cannot open file "', argv, '"'
		call exit(EXIT_FAILURE)
	end if

	read(ittf) scalar_type
	read(ittf) num_tables

	! TODO: interpret as unsigned instead of signed ints
	print *, 'scalar_type = ', scalar_type
	print *, 'num_tables  = ', num_tables

	close(ittf)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

