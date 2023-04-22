
!===============================================================================

program main

	use cali_m

	implicit none

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc, io, ittf, i

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

	open(newunit = ittf, file = argv, action = 'read', iostat = io, &
		access = 'stream', convert = 'big_endian')
	if (io /= EXIT_SUCCESS) then
		write(*,*) 'Error: cannot open file "', argv, '"'
		call exit(EXIT_FAILURE)
	end if

	! TODO: save in offset-tables struct
	ttf%scalar_type  = read_u32(ittf)
	ttf%num_tables   = read_u16(ittf)
	ttf%search_range = read_u16(ittf)
	ttf%entry_select = read_u16(ittf)
	ttf%range_shift  = read_u16(ittf)

	print *, 'scalar_type = ', ttf%scalar_type
	print *, 'num_tables  = ', ttf%num_tables

	allocate(ttf%tables( ttf%num_tables ))

	do i = 1, ttf%num_tables

		ttf%tables(i)%tag      = read_str(ittf, 4)
		ttf%tables(i)%checksum = read_u32(ittf)
		ttf%tables(i)%offset   = read_u32(ittf)
		ttf%tables(i)%length   = read_u32(ittf)

		print *, 'tag = ', ttf%tables(i)%tag

	end do

	close(ittf)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

