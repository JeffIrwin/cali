
!===============================================================================

module cali_m

	integer :: EXIT_SUCCESS = 0, EXIT_FAILURE = -1


contains

!===============================================================================

function read_u32(unit) result(u32)

	! Fortran does not have unsigned ints.  Read a signed int, cast it to a
	! bigger int size, then re-interpret as the correct positive value

	integer, intent(in) :: unit

	integer(kind = 8) :: u32

	!********

	integer(kind = 4) :: i32
	integer(kind = 8) :: i64

	u32 = 0

	read(unit) i32
	i64 = i32
	!print *, 'i64 = ', i64
	if (i64 < 0) i64 = i64 + huge(i32)
	u32 = i64

end function read_u32

!===============================================================================

function read_u16(unit) result(u16)

	integer, intent(in) :: unit

	integer(kind = 8) :: u16

	!********

	integer(kind = 2) :: i16
	integer(kind = 8) :: i64

	u16 = 0

	read(unit) i16
	i64 = i16
	if (i64 < 0) i64 = i64 + huge(i16)
	u16 = i64

end function read_u16

!===============================================================================

end module cali_m

!===============================================================================

program main

	use cali_m

	implicit none

	character(len = 4) :: tag
	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc, io, ittf, i

	integer(kind = 8) :: scalar_type, checksum, offset, length
	integer(kind = 8) :: num_tables, search_range, entry_select, range_shift

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

	open(newunit = ittf, file = argv, action = 'read', iostat = io, &
		access = 'stream', convert = 'big_endian')
	if (io /= EXIT_SUCCESS) then
		write(*,*) 'Error: cannot open file "', argv, '"'
		call exit(EXIT_FAILURE)
	end if

	! TODO: save in offset-tables struct
	scalar_type  = read_u32(ittf)
	num_tables   = read_u16(ittf)
	search_range = read_u16(ittf)
	entry_select = read_u16(ittf)
	range_shift  = read_u16(ittf)

	print *, 'scalar_type = ', scalar_type
	print *, 'num_tables  = ', num_tables

	do i = 1, num_tables
		read(ittf) tag
		print *, 'tag = ', tag

		! TODO: save in array of table structs
		checksum = read_u32(ittf)
		offset   = read_u32(ittf)
		length   = read_u32(ittf)

		!stop
	end do

	close(ittf)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

