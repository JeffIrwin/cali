
!===============================================================================

module cali_m

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

