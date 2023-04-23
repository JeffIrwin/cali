
!===============================================================================

module cali_m

	implicit none

	integer, parameter :: &
		EXIT_SUCCESS = 0, &
		EXIT_FAILURE = -1, &
		SEEK_ABS = 0

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

		contains
			procedure :: get_table

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
	!print '(a,z0)', ' i64 = ', i64

	!if (i64 < 0) i64 = i64 + huge(i32)
	i64 = iand(i64, int(z'ffffffff', 8)) ! TODO: make macro to shorten z8 casting syntax

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
	!if (i64 < 0) i64 = i64 + huge(i16)
	i64 = iand(i64, z'ffff')
	u16 = i64

end function read_u16

!===============================================================================

function get_table(ttf, tag) result(table_id)

	! Lookup a ttf table id by its tag name

	class(ttf_t), intent(in) :: ttf
	character(len = *), intent(in) :: tag

	integer(kind = 8) :: table_id

	!********

	do table_id = 1, ttf%num_tables
		if (ttf%tables(table_id)%tag == tag) return
	end do
	table_id = -1

end function get_table

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

	write(*,*) 'Reading file "', filename, '" ...'

	ttf%scalar_type  = read_u32(iu)
	ttf%num_tables   = read_u16(iu)
	ttf%search_range = read_u16(iu)
	ttf%entry_select = read_u16(iu)
	ttf%range_shift  = read_u16(iu)

	!print *, 'scalar_type = ', ttf%scalar_type

	write(*, '(a,i0)') ' Number of tables = ', ttf%num_tables

	! Read offset tables which include the position and length of each table in
	! the ttf file
	allocate(ttf%tables( ttf%num_tables ))
	do i = 1, ttf%num_tables
		ttf%tables(i) = read_ttf_table(iu)
	end do

	! Read head table TODO

	close(iu)
	!print *, 'done read_ttf()'
	!print *, ''

end function read_ttf

!===============================================================================

function read_ttf_table(iu) result(table)

	integer, intent(in) :: iu

	type(ttf_table) :: table

	!********

	integer(kind = 8) :: i, old, sum

	table%tag      = read_str(iu, 4)
	table%checksum = read_u32(iu)
	table%offset   = read_u32(iu)
	table%length   = read_u32(iu)

	!print *, 'tag    = ', table%tag
	!print *, 'offset = ', table%offset
	!print *, 'length = ', table%length
	!print '(a,z0)', 'length = ', table%length
	!print *, 'offset + length = ', table%offset + table%length
	!print '(a,z0)', ' offset + length = ', table%offset + table%length
	!print *, ''

	! TODO: handle checkSumAdjustment somewhere
	if (table%tag == "head") return

	! Verify checksum
	old = ftell(iu)
	!print '(a,z0)', ' old = ', old

	call fseek(iu, table%offset, SEEK_ABS)

	sum = 0

	! Number of 4-byte words is ceiling(num_bytes / 4)
	do i = 1, (table%length + 3) / 4
		sum = sum + read_u32(iu)
	end do
	!print '(a,z0)', '  sum   = ', sum

	sum = iand(sum, z'ffffffff')
	!print '(a,z0)', ' final  sum   = ', sum
	!print '(a,z0)', ' final csum   = ', table%checksum

	if (sum /= table%checksum) then
		write(*,*) 'Error: bad checksum for table ', table%tag
		call exit(-1)
	end if

	call fseek(iu, old, SEEK_ABS)

end function read_ttf_table

!===============================================================================

end module cali_m

!===============================================================================

