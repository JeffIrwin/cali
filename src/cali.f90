
!===============================================================================

module cali_m

	implicit none

	integer :: EXIT_SUCCESS = 0, EXIT_FAILURE = -1

	!********

	type ttf_table
		character(len = :), allocatable :: tag
		integer(kind = 8) :: checksum, offset, length
	end type ttf_table

	!********

	type ttf_t

		integer(kind = 8) :: scalar_type

		integer(kind = 8) :: num_tables, search_range, entry_select, &
			range_shift

		type(ttf_table), allocatable :: tables(:)

	end type ttf_t

	!********

contains

!===============================================================================

function read_str(unit, str_len) result(str)

	integer, intent(in) :: unit, str_len
	character(len = :), allocatable :: str

	allocate(character(len = str_len) :: str)
	read(unit) str

end function read_str

!===============================================================================

function read_u32(unit) result(u32)

	! Fortran does not have unsigned ints.  Read a signed int, cast it to a
	! bigger int size, then re-interpret as the correct positive value

	integer, intent(in) :: unit

	integer(kind = 8) :: u32

	!********

	integer(kind = 4) :: i32
	integer(kind = 8) :: i64

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

	read(unit) i16
	i64 = i16
	if (i64 < 0) i64 = i64 + huge(i16)
	u16 = i64

end function read_u16

!===============================================================================

function read_ttf(filename) result(ttf)

	character(len = *), intent(in) :: filename

	type(ttf_t) :: ttf

	!********

	integer :: io, iu
	integer(kind = 8) :: i

	open(newunit = iu, file = filename, action = 'read', iostat = io, &
		access = 'stream', convert = 'big_endian')
	if (io /= EXIT_SUCCESS) then
		write(*,*) 'Error: cannot open file "', filename, '"'
		call exit(EXIT_FAILURE)
	end if

	ttf%scalar_type  = read_u32(iu)
	ttf%num_tables   = read_u16(iu)
	ttf%search_range = read_u16(iu)
	ttf%entry_select = read_u16(iu)
	ttf%range_shift  = read_u16(iu)

	print *, 'scalar_type = ', ttf%scalar_type
	print *, 'num_tables  = ', ttf%num_tables

	allocate(ttf%tables( ttf%num_tables ))

	do i = 1, ttf%num_tables

		ttf%tables(i)%tag      = read_str(iu, 4)
		ttf%tables(i)%checksum = read_u32(iu)
		ttf%tables(i)%offset   = read_u32(iu)
		ttf%tables(i)%length   = read_u32(iu)

		print *, 'tag = ', ttf%tables(i)%tag

	end do

	close(iu)

end function read_ttf

!===============================================================================

end module cali_m

!===============================================================================

