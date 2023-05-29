
!===============================================================================

module cali_m

	implicit none

	integer, parameter :: &
		CALI_MAJOR = 0, &
		CALI_MINOR = 0, &
		CALI_PATCH = 4

	integer, parameter ::  &
		EXIT_SUCCESS =  0, &
		EXIT_FAILURE = -1, &
		ND           =  2, &
		SEEK_REL     =  1, &
		SEEK_ABS     =  0

	integer(kind = 2), parameter :: &
		ON_CURVE   = int(b'00000001'), &
		X_IS_BYTE  = int(b'00000010'), &
		!Y_IS_BYTE = int(b'00000100'), &  ! 2 * X_IS_BYTE
		REPEAT_    = int(b'00001000'), &
		X_DELTA    = int(b'00010000')!,  &
		!Y_DELTA   = int(b'00100000')     ! 2 * X_DELTA

	character, parameter :: &
			LINE_FEED       = char(10), &
			ESC             = char(27)

	character(len = *), parameter :: &
			FG_BOLD_BRIGHT_RED = ESC//'[91;1m', &
			FG_BOLD_BRIGHT_MAG = ESC//'[95;1m', &
			FG_BRIGHT_GREEN    = ESC//'[92m', &
			COLOR_RESET        = ESC//'[0m'

	character(len = *), parameter :: &
		WARNING = FG_BOLD_BRIGHT_MAG//'Warning'//COLOR_RESET//': ', &
		ERROR   = FG_BOLD_BRIGHT_RED//'Error'//COLOR_RESET//': '

	!********

	type ttf_table
		! Table metadata from ttf file header
		character(len = :), allocatable :: tag
		integer(kind = 8) :: checksum, offset, length
	end type ttf_table

	!********

	type transform_t
		! affine transformation
		double precision :: a = 1.d0, b = 0.d0, c = 0.d0, d = 1.d0, &
			e = 0.d0, f = 0.d0, m = 1.d0, n = 1.d0
	end type transform_t

	!********

	type glyph_comp_t
		! Component of a compound glyph
		integer(kind = 8) :: index_
		type(transform_t) :: t
	end type glyph_comp_t

	!********

	type glyph_t

		! simple
		integer(kind = 8) :: xmin, ymin, xmax, ymax, ninst
		integer(kind = 8), allocatable :: x(:,:) ! TODO: use 4-bytes for x
		integer(kind = 4), allocatable :: end_pts(:)
		integer(kind = 4) :: ncontours, npts
		integer(kind = 2), allocatable :: flags(:)

		! compound
		integer :: ncomps
		type(glyph_comp_t), allocatable :: comps(:)

	end type glyph_t

	!********

	type cmap_t

		integer(kind = 8) :: fmt_, length, language, nseg_x2, search_range, &
			entry_select, range_shift, platform_id, platform_sid, offset, &
			reserved_pad, first_code, entry_count

		integer(kind = 8), allocatable :: end_code(:), start_code(:), &
			id_delta(:), id_range_offset(:), glyph_ids(:)

	end type cmap_t

	!********

	type ttf_t

		integer(kind = 8) :: scalar_type, ntables, search_range, &
			entry_select, range_shift, checksum_adj, magic_num, flags, &
			units_per_em, xmin, ymin, xmax, ymax, mac_style, low_rec_ppem, &
			font_dir_hint, index2loc_fmt, glyph_data_fmt, nglyphs, &
			cmap_vers, ncmap_tables, ascent, descent, line_gap, &
			advance_width_max, min_lsb, min_rsb, xmax_extent, &
			caret_slope_rise, caret_slope_run, caret_offset, metric_data_fmt, &
			nlong_mtx

		! Table offsets, cached to avoid multiple calls to get_table()
		integer(kind = 8) :: glyf_offset, loca_offset, cmap_offset

		! Dates in ttf long date time format (i64 seconds since 1904)
		integer(kind = 8) :: created, modified

		double precision :: vers, font_rev, maxp_vers, hhea_vers

		integer(kind = 8), allocatable :: advance_widths(:)
		type(glyph_t  ), allocatable :: glyphs(:)
		type(ttf_table), allocatable :: tables(:)
		type(cmap_t) :: cmap

		contains
			procedure :: get_table

	end type ttf_t

	!********

	interface to_str
		procedure :: i32_to_str
		procedure :: i64_to_str
	end interface

contains

!===============================================================================

function read_str(unit, str_len) result(str)

	integer, intent(in) :: unit, str_len
	character(len = :), allocatable :: str

	allocate(character(len = str_len) :: str)
	read(unit) str

end function read_str

!===============================================================================

function read_fixed(unit) result(fixed)

	! TODO: error-handling for all IO fns?  If the error-handling is just 'call
	! exit()' then there's no point

	integer, intent(in) :: unit
	double precision :: fixed

	!********

	integer(kind = 4) :: i32

	read(unit) i32
	fixed = i32 * (2.d0 ** (-16))

end function read_fixed

!===============================================================================

function read_fword(unit) result(fword)

	! This is just an alias for read_i16(), but the ttf docs refer to fword and
	! i16 as separate types, so we do too

	integer, intent(in) :: unit
	integer(kind = 8) :: fword

	fword = read_i16(unit)

end function read_fword

!===============================================================================

function read_ufword(unit) result(fword)

	integer, intent(in) :: unit
	integer(kind = 8) :: fword

	fword = read_u16(unit)

end function read_ufword

!===============================================================================

function read_i8(unit) result(i8)

	integer, intent(in) :: unit
	integer(kind = 1) :: i8

	read(unit) i8

end function read_i8

!===============================================================================

function read_u8(unit) result(u8)

	integer, intent(in) :: unit
	integer(kind = 8) :: u8

	!********

	integer(kind = 1) :: i8

	read(unit) i8
	u8 = iand(int(i8,2), int(z'ff',2))

end function read_u8

!===============================================================================

function read_i16(unit) result(i16)

	integer, intent(in) :: unit
	integer(kind = 2) :: i16

	read(unit) i16

end function read_i16

!===============================================================================

function read_u16(unit) result(u16)

	integer, intent(in) :: unit
	integer(kind = 8) :: u16

	!********

	integer(kind = 2) :: i16

	read(unit) i16
	u16 = iand(int(i16,4), int(z'ffff',4))

end function read_u16

!===============================================================================

function read_u32(unit) result(u32)

	! Fortran does not have unsigned ints.  Read a signed int, cast it to a
	! bigger int size, then re-interpret as the correct positive value

	integer, intent(in) :: unit
	integer(kind = 8) :: u32

	!********

	integer(kind = 4) :: i32

	read(unit) i32
	u32 = iand(int(i32,8), int(z'ffffffff',8))

end function read_u32

!===============================================================================

function read_i64(unit) result(i64)

	integer, intent(in) :: unit
	integer(kind = 8) :: i64

	read(unit) i64

end function read_i64

!===============================================================================

function get_table(ttf, tag) result(table_id)

	! Lookup a ttf table id by its tag name

	class(ttf_t), intent(in) :: ttf
	character(len = *), intent(in) :: tag

	integer(kind = 8) :: table_id

	!********

	do table_id = 1, ttf%ntables
		if (ttf%tables(table_id)%tag == tag) return
	end do

	! TODO: error handling
	table_id = -1

end function get_table

!===============================================================================

function read_ttf(filename) result(ttf)

	character(len = *), intent(in) :: filename

	type(ttf_t) :: ttf

	!********

	integer :: io, iu
	integer(kind = 8) :: i, sum_, filesize, reserved, nglyph_indices

	open(newunit = iu, file = filename, action = 'read', iostat = io, &
		access = 'stream', convert = 'big_endian')
	if (io /= EXIT_SUCCESS) then
		write(*,*) ERROR//'cannot open file "', filename, '"'
		call exit(EXIT_FAILURE)
	end if

	write(*,*) 'Reading file "', filename, '" ...'

	! Verify head table checksum (whole file)

	inquire(file = filename, size = filesize)
	!print *, 'filesize = ', filesize

	! TODO: the issue with corbeli.ttf is that its number of bytes is not
	! divisible by 4, so this reads past the end of file on the last 4-byte
	! word.  It should be fixable by reading up until the last word, then
	! reading the last 1-4 bytes and padding it with zeros

	sum_ = 0
	do i = 1, filesize / 4
	!do i = 1, (filesize + 3) / 4
		sum_ = sum_ + read_u32(iu)
	end do
	sum_ = iand(sum_, int(z'ffffffff',8))

	if (sum_ /= int(z'b1b0afba', 8)) then
		write(*,*) ERROR//'bad checksum for head table'
		call exit(EXIT_FAILURE)
	end if
	call fseek(iu, 0, SEEK_ABS)
	!print *, 'verified head checksum'

	ttf%scalar_type  = read_u32(iu)
	ttf%ntables      = read_u16(iu)
	ttf%search_range = read_u16(iu)
	ttf%entry_select = read_u16(iu)
	ttf%range_shift  = read_u16(iu)

	!print *, 'scalar_type  = ', ttf%scalar_type
	!print *, 'search_range = ', ttf%search_range
	!print *, 'entry_select = ', ttf%entry_select
	!print *, 'range_shift  = ', ttf%range_shift

	!write(*, '(a,i0)') ' Number of tables = ', ttf%ntables

	! Read offset tables which include the position and length of each table in
	! the ttf file
	allocate(ttf%tables( ttf%ntables ))
	do i = 1, ttf%ntables
		ttf%tables(i) = read_ttf_table(iu)
	end do

	! Read head table. TODO: parameterize "head"
	!
	! Offsets are already saved, so we don't need to save old positions before
	! fseek() now unlike in table checksum verification
	call fseek_table(iu, ttf, "head")

	ttf%vers  = read_fixed(iu)
	ttf%font_rev = read_fixed(iu)

	write(*,'(a,f8.4)') ' TTF version   = ', ttf%vers
	write(*,'(a,f8.4)') ' Font revision = ', ttf%font_rev

	ttf%checksum_adj = read_u32(iu)
	ttf%magic_num    = read_u32(iu)
	!print '(a,z0)', ' magic_num = ', ttf%magic_num

	if (ttf%magic_num /= int(z'5f0f3cf5')) then
		write(*,*) ERROR//'bad magic number in ttf file'
		call exit(EXIT_FAILURE)
	end if

	ttf%flags        = read_u16(iu)
	ttf%units_per_em = read_u16(iu)

	write(*,*) 'Units per em = '//to_str(ttf%units_per_em)

	ttf%created  = read_i64(iu)
	ttf%modified = read_i64(iu)
	write(*,*) 'Created   = ', ldt2str(ttf%created)
	write(*,*) 'Modified  = ', ldt2str(ttf%modified)

	! These values are Fword's, which are just i16's
	ttf%xmin = read_fword(iu)
	ttf%ymin = read_fword(iu)
	ttf%xmax = read_fword(iu)
	ttf%ymax = read_fword(iu)

	!print *, 'xmin = ', ttf%xmin
	!print *, 'ymin = ', ttf%ymin
	!print *, 'xmax = ', ttf%xmax
	!print *, 'ymax = ', ttf%ymax

	ttf%mac_style      = read_u16(iu)
	ttf%low_rec_ppem   = read_u16(iu)
	ttf%font_dir_hint  = read_i16(iu)
	ttf%index2loc_fmt  = read_i16(iu)
	ttf%glyph_data_fmt = read_i16(iu)

	!print *, 'low_rec_ppem   = ', ttf%low_rec_ppem
	!print *, 'font_dir_hint  = ', ttf%font_dir_hint
	!print *, 'index2loc_fmt  = ', ttf%index2loc_fmt
	!print *, 'glyph_data_fmt = ', ttf%glyph_data_fmt

	call fseek_table(iu, ttf, "maxp")
	ttf%maxp_vers = read_fixed(iu)
	ttf%nglyphs   = read_u16(iu)

	!print *, 'maxp_vers      = ', ttf%maxp_vers
	write(*, '(a,i0)') ' Number of glyphs = ', ttf%nglyphs

	ttf%cmap_offset = ttf%tables( ttf%get_table("cmap") )%offset
	call fseek(iu, ttf%cmap_offset, SEEK_ABS)
	ttf%cmap_vers    = read_u16(iu)
	ttf%ncmap_tables = read_u16(iu)

	! TODO: iterate below for each ncmap_tables?  Maybe I only need the first
	! subtable?  verdana.ttf has cmap fmt 0, but fontdrop says it has fmt 4.
	! Try parsing its second cmap table?
	ttf%cmap%platform_id  = read_u16(iu)
	ttf%cmap%platform_sid = read_u16(iu)
	ttf%cmap%offset       = read_u32(iu)

	!print *, 'cmap_vers       = ', ttf%cmap_vers
	!print *, 'ncmap_tables    = ', ttf%ncmap_tables
	!print *, 'platform_id     = ', ttf%cmap%platform_id
	!print *, 'platform_sid    = ', ttf%cmap%platform_sid
	!print *, 'cmap_sub_offset = ', ttf%cmap%offset

	! Jump to cmap subtable offset relative to cmap_offset
	call fseek(iu, ttf%cmap_offset + ttf%cmap%offset, SEEK_ABS)
	ttf%cmap%fmt_ = read_u16(iu)
	write(*,*) 'cmap format = '//to_str(ttf%cmap%fmt_)

	select case (ttf%cmap%fmt_)
	case (0)

		ttf%cmap%length   = read_u16(iu)
		ttf%cmap%language = read_u16(iu)

		! Glyph index array
		allocate( ttf%cmap%glyph_ids(0: 255) )

		do i = 0, size( ttf%cmap%glyph_ids ) - 1
			ttf%cmap%glyph_ids(i) = read_u8(iu)
		end do

		!print *, 'glyph_ids = '
		!do i = 0, size( ttf%cmap%glyph_ids ) - 1
		!	print *, i, ttf%cmap%glyph_ids(i)
		!end do

	case (4)

		ttf%cmap%length   = read_u16(iu)
		ttf%cmap%language = read_u16(iu)

		ttf%cmap%nseg_x2 = read_u16(iu)
		ttf%cmap%search_range = read_u16(iu)
		ttf%cmap%entry_select = read_u16(iu)
		ttf%cmap%range_shift = read_u16(iu)

		allocate(ttf%cmap%end_code       ( ttf%cmap%nseg_x2 / 2 ))
		allocate(ttf%cmap%start_code     ( ttf%cmap%nseg_x2 / 2 ))
		allocate(ttf%cmap%id_delta       ( ttf%cmap%nseg_x2 / 2 ))
		allocate(ttf%cmap%id_range_offset( ttf%cmap%nseg_x2 / 2 ))

		do i = 1, ttf%cmap%nseg_x2 / 2
			ttf%cmap%end_code(i) = read_u16(iu)
		end do
		ttf%cmap%reserved_pad = read_u16(iu)
		do i = 1, ttf%cmap%nseg_x2 / 2
			ttf%cmap%start_code(i) = read_u16(iu)
		end do
		do i = 1, ttf%cmap%nseg_x2 / 2

			ttf%cmap%id_delta(i) = read_u16(iu)

			! "NOTE: All idDelta[i] arithmetic is modulo 65536." -- apple

			! cormorant garamond has a mixture of small positive and negative
			! offsets.  choose middle of u16 range for modulo cutoff
			if (ttf%cmap%id_delta(i) > int(z'fffe',4)/2) then
				ttf%cmap%id_delta(i) = ttf%cmap%id_delta(i) - int(z'10000')
			end if

			!if (ttf%cmap%id_delta(i) /= 0) then
			!	ttf%cmap%id_delta(i) = ttf%cmap%id_delta(i) - int(z'10000')
			!end if

		end do
		do i = 1, ttf%cmap%nseg_x2 / 2
			ttf%cmap%id_range_offset(i) = read_u16(iu)
		end do

		nglyph_indices = 0
		do i = 1, ttf%cmap%nseg_x2 / 2
			if (ttf%cmap%id_range_offset(i) /= 0) then

				nglyph_indices = nglyph_indices + &
					ttf%cmap%end_code(i) - ttf%cmap%start_code(i) + 1

			end if
		end do

		allocate(ttf%cmap%glyph_ids( 0: nglyph_indices - 1))

		do i = 0, size( ttf%cmap%glyph_ids ) - 1
			ttf%cmap%glyph_ids(i) = read_u16(iu)
		end do

		!print *, 'cmap fmt_         = ', ttf%cmap%fmt_
		!print *, 'cmap length       = ', ttf%cmap%length
		!print *, 'cmap language     = ', ttf%cmap%language
		!print *, 'cmap nseg_x2      = ', ttf%cmap%nseg_x2
		!print *, 'cmap search_range = ', ttf%cmap%search_range
		!print *, 'cmap entry_select = ', ttf%cmap%entry_select
		!print *, 'cmap range_shift  = ', ttf%cmap%range_shift
		!print *, 'cmap end_code = ', ttf%cmap%end_code
		!print *, ''
		!print *, 'cmap reserved_pad  = ', ttf%cmap%reserved_pad
		!print *, 'cmap start_code = ', ttf%cmap%start_code
		!print *, ''
		!print *, 'cmap id_delta = ', ttf%cmap%id_delta
		!print *, ''
		!print *, 'cmap id_range_offset = ', ttf%cmap%id_range_offset
		!print *, ''
		!print *, 'glyph_ids = '
		!do i = 0, size( ttf%cmap%glyph_ids ) - 1
		!	print *, i, ttf%cmap%glyph_ids(i)
		!end do

	case (6)
		! e.g. C:/Windows/fonts/Candaral.ttf

		ttf%cmap%length   = read_u16(iu)
		ttf%cmap%language = read_u16(iu)

		ttf%cmap%first_code  = read_u16(iu)
		ttf%cmap%entry_count = read_u16(iu)

		! Glyph index array
		allocate( ttf%cmap%glyph_ids(0: ttf%cmap%entry_count - 1) )

		do i = 0, size( ttf%cmap%glyph_ids ) - 1
			ttf%cmap%glyph_ids(i) = read_u16(iu)
		end do

		!print *, 'glyph_ids = '
		!do i = 0, size( ttf%cmap%glyph_ids ) - 1
		!	print *, i, ttf%cmap%glyph_ids(i)
		!end do

	case default
		write(*,*) ERROR//'cmap format '//to_str(ttf%cmap%fmt_) &
			//' is not supported'
		call exit(-1)

	end select  ! cmap fmt_

	!********

	call fseek_table(iu, ttf, "hhea")

	ttf%hhea_vers = read_fixed(iu)
	ttf%ascent    = read_fword(iu)
	ttf%descent   = read_fword(iu)
	ttf%line_gap  = read_fword(iu)
	ttf%advance_width_max = read_ufword(iu)
	ttf%min_lsb     = read_fword(iu)
	ttf%min_rsb     = read_fword(iu)
	ttf%xmax_extent = read_fword(iu)
	ttf%caret_slope_rise = read_i16(iu)
	ttf%caret_slope_run  = read_i16(iu)
	ttf%caret_offset = read_fword(iu)
	reserved = read_i16(iu)
	reserved = read_i16(iu)
	reserved = read_i16(iu)
	reserved = read_i16(iu)
	ttf%metric_data_fmt = read_i16(iu)
	ttf%nlong_mtx = read_u16(iu)

	!print *, 'hhea_vers       = ', ttf%hhea_vers
	!print *, 'nlong_mtx       = ', ttf%nlong_mtx

	!********

	call fseek_table(iu, ttf, "hmtx")

	allocate(ttf%advance_widths( 0: ttf%nlong_mtx - 1))
	do i = 0, ttf%nlong_mtx - 1
		ttf%advance_widths(i) = read_u16(iu)
		reserved = read_i16(iu)  ! could save leftSideBearing here
	end do
	!print *, 'advance_widths = ', ttf%advance_widths(0:20)

	!********

	!loca = ttf%get_table("loca")
	!glyf = ttf%get_table("glyf")
	!!print *, 'loca, glyf = ', loca, glyf

	ttf%loca_offset = ttf%tables( ttf%get_table("loca") )%offset
	ttf%glyf_offset = ttf%tables( ttf%get_table("glyf") )%offset

	allocate(ttf%glyphs( 0: ttf%nglyphs ))
	do i = 0, ttf%nglyphs - 1
		ttf%glyphs(i) = read_glyph(iu, ttf, i)
	end do

	close(iu)
	write(*,*)
	!print *, 'done read_ttf()'
	!print *, ''

end function read_ttf

!===============================================================================

subroutine fseek_table(iu, ttf, tag)

	integer, intent(in) :: iu
	type(ttf_t), intent(in) :: ttf
	character(len = *), intent(in) :: tag

	call fseek(iu, ttf%tables( ttf%get_table(tag) )%offset, SEEK_ABS)

end subroutine fseek_table

!===============================================================================

function get_index(char32, ttf) result(i)

	! Get the glyph index of a utf32 character codepoint in the ttf struct

	integer(kind = 4), intent(in) :: char32
	type(ttf_t), intent(in) :: ttf
	integer(kind = 8) :: i, offset

	!********

	integer :: seg

	!print '(a,z0)', 'char32 = U+', char32

	case_fmt: select case (ttf%cmap%fmt_)
	case (0)

		if (char32 > ubound (ttf%cmap%glyph_ids, 1) .or. &
		    char32 < lbound (ttf%cmap%glyph_ids, 1)) then
			! Missing character glyph
			i = 0
			exit case_fmt
		end if
		i = ttf%cmap%glyph_ids(char32)

	case (4)

		! Search for the first endCode that is greater than or equal to the
		! character code to be mapped.  Could use a binary search
		seg = 1
		do while (ttf%cmap%end_code(seg) < char32)
			seg = seg + 1
			if (seg > ttf%cmap%nseg_x2 / 2) then
				! Missing character glyph
				i = 0
				exit case_fmt
			end if
		end do
		!print *, 'seg = ', seg

		if (ttf%cmap%start_code(seg) > ttf%cmap%end_code(seg)) then
			i = 0
			exit case_fmt
		end if

		if (ttf%cmap%start_code(seg) > char32) then
			i = 0
			exit case_fmt
		end if

		if (ttf%cmap%id_range_offset(seg) /= 0) then
			! Ref:  https://stackoverflow.com/a/61804360/4347028
			i = ttf%cmap%glyph_ids(seg - 1 - ttf%cmap%nseg_x2/2 + &
				ttf%cmap%id_range_offset(seg) / 2 + &
				char32 - ttf%cmap%start_code(seg))
			!print *, 'ttf%cmap%id_range_offset(seg) = ', ttf%cmap%id_range_offset(seg)

		else
			i = char32 + ttf%cmap%id_delta(seg)
			!print *, 'ttf%cmap%id_delta(seg) = ', ttf%cmap%id_delta(seg)
		end if

	case (6)

		offset = char32 - ttf%cmap%first_code
		if (offset > ubound (ttf%cmap%glyph_ids, 1) .or. &
		    offset < lbound (ttf%cmap%glyph_ids, 1)) then
			! Missing character glyph
			i = 0
			exit case_fmt
		end if
		i = ttf%cmap%glyph_ids(offset)

	end select case_fmt

	! TODO: log character and codepoint too
	if (i == 0) then
		write(*,*) WARNING//'unknown glyph not defined for this font'
	end if

	!print *, 'glyph index = ', i
	!print *, ''

end function get_index

!===============================================================================

function read_glyph(iu, ttf, iglyph) result(glyph)

	! Read glyph index iglyph from file unit iu in the ttf struct
	!
	! TODO: encapsulate iu within ttf struct

	integer, intent(in) :: iu
	integer(kind = 8), intent(in) :: iglyph

	type(ttf_t), intent(in) :: ttf

	type(glyph_t) :: glyph

	!********

	double precision :: a, b, c, d, e, f, m, n  ! affine transformation

	integer, parameter :: MAX_COMP = 64

	integer(kind = 2), parameter :: &
		ARGS_ARE_WORDS = int(b'0000000000000001'), &
		ARGS_ARE_XY    = int(b'0000000000000010'), &
		!ROUND_TO_GRID  = int(b'0000000000000100'), &
		HAS_SCALE      = int(b'0000000000001000'), &
		MORE_COMPS     = int(b'0000000000100000'), &
		HAS_XY_SCALE   = int(b'0000000001000000'), &
		HAS_2X2        = int(b'0000000010000000')!, &
		!HAS_INST       = int(b'0000000100000000'), &
		!USE_MTX        = int(b'0000001000000000'), &
		!OVERLAP_COMP   = int(b'0000010000000000')

	integer :: i, j
	integer(kind = 8) :: offset, offset_next, length, pos
	integer(kind = 2) :: flag, nrepeat, is_byte, delta

	!print *, 'glyph index = ', iglyph

	if (ttf%index2loc_fmt == 0) then

		call fseek(iu, ttf%loca_offset + 2 * iglyph, SEEK_ABS)
		offset      = 2 * read_u16(iu)
		offset_next = 2 * read_u16(iu)

	else

		call fseek(iu, ttf%loca_offset + 4 * iglyph, SEEK_ABS)
		offset      = read_u32(iu)
		offset_next = read_u32(iu)

	end if

	! Get length of glyph.  The loca table always has 1 more offset than number
	! of glyphs so this is always valid.  Characters such as whitespace are
	! zero-length
	length = offset_next - offset
	!print *, 'length = ', length
	if (length <= 0) then
		glyph%ncontours = 0
		glyph%npts      = 0
		allocate(glyph%x(ND, glyph%npts))
		return
	end if

	offset = offset + ttf%glyf_offset
	call fseek(iu, offset, SEEK_ABS)
	!print '(a,z0)', ' glyph offset = ', offset

	glyph%ncontours = read_i16(iu)
	!print *, 'ncontours = ', glyph%ncontours

	glyph%xmin = read_fword(iu)
	glyph%ymin = read_fword(iu)
	glyph%xmax = read_fword(iu)
	glyph%ymax = read_fword(iu)

	!print *, "xmin = ", glyph%xmin
	!print *, "ymin = ", glyph%ymin
	!print *, "xmax = ", glyph%xmax
	!print *, "ymax = ", glyph%ymax

	if (glyph%ncontours < 0) then
		!print *, 'reading compound glyph ...'

		allocate(glyph%comps(MAX_COMP))

		i = 0
		do
			i = i + 1

			! TODO: make 2 passes to count num comps and then save
			if (i > MAX_COMP) then
				write(*,*) ERROR//'max compound glyph components exceeded'
				call exit(EXIT_FAILURE)
			end if

			flag = int(read_u16(iu),2)

			glyph%comps(i)%index_ = read_u16(iu)

			if (iand(flag, ARGS_ARE_WORDS) /= 0 .and. iand(flag, ARGS_ARE_XY) /= 0) then
				e = read_i16(iu)
				f = read_i16(iu)
			else if (iand(flag, ARGS_ARE_WORDS) == 0 .and. iand(flag, ARGS_ARE_XY) /= 0) then
				e = read_i8(iu)
				f = read_i8(iu)
			else if (iand(flag, ARGS_ARE_WORDS) /= 0 .and. iand(flag, ARGS_ARE_XY) == 0) then
				!1st short contains the index of matching point in compound being constructed
				!2nd short contains index of matching point in component
				write(*,*) ERROR//'anchor 1'
				call exit(EXIT_FAILURE)
			else if (iand(flag, ARGS_ARE_WORDS) == 0 .and. iand(flag, ARGS_ARE_XY) == 0) then
				!1st byte containing index of matching point in compound being constructed
				!2nd byte containing index of matching point in component
				write(*,*) ERROR//'anchor 2'
				call exit(EXIT_FAILURE)
			end if

			b = 0.d0
			c = 0.d0
			if (iand(flag, HAS_SCALE   ) == 0 .and. &
				iand(flag, HAS_XY_SCALE) == 0 .and. &
				iand(flag, HAS_2X2     ) == 0) then
				a = 1.d0
				d = 1.d0

			else if (iand(flag, HAS_SCALE)    /= 0) then
				a = read_i16(iu) * (2.d0 ** (-14))
				d = a

			else if (iand(flag, HAS_XY_SCALE) /= 0) then
				! tested in arial.ttf: "Я" glyph has negative x scale to mirror
				a = read_i16(iu) * (2.d0 ** (-14))
				d = read_i16(iu) * (2.d0 ** (-14))

			else if (iand(flag, HAS_2X2)      /= 0) then
				! tested in Amiri-BoldSlanted.ttf
				a = read_i16(iu) * (2.d0 ** (-14))
				b = read_i16(iu) * (2.d0 ** (-14))
				c = read_i16(iu) * (2.d0 ** (-14))
				d = read_i16(iu) * (2.d0 ** (-14))

			end if

			m = max(abs(a), abs(b))
			n = max(abs(c), abs(d))
			if (abs(abs(a) - abs(c)) <= 33.d0 / 65536.d0) then
				m = 2 * 0
			end if
			if (abs(abs(b) - abs(d)) <= 33.d0 / 65536.d0) then
				n = 2 * n
			end if

			!print *, 'a = ', a
			!print *, 'b = ', b
			!print *, 'c = ', c
			!print *, 'd = ', d
			!print *, 'e = ', e
			!print *, 'f = ', f
			!print *, 'm = ', m
			!print *, 'n = ', n

			glyph%comps(i)%t%a = a
			glyph%comps(i)%t%b = b
			glyph%comps(i)%t%c = c
			glyph%comps(i)%t%d = d
			glyph%comps(i)%t%e = e
			glyph%comps(i)%t%f = f
			glyph%comps(i)%t%m = m
			glyph%comps(i)%t%n = n

			if (iand(flag, MORE_COMPS) == 0) exit

		end do
		glyph%ncomps = i

		return
	end if

	! Read simple glyph

	allocate(glyph%end_pts( glyph%ncontours ))
	do i = 1, glyph%ncontours
		glyph%end_pts(i) = int(read_u16(iu))
	end do

	!print *, 'end_pts = ', glyph%end_pts

	! Skip instructions for now. TODO: read and save
	glyph%ninst = read_u16(iu)
	call fseek(iu, glyph%ninst, SEEK_REL)
	!print *, 'ninst = ', glyph%ninst

	glyph%npts = maxval( glyph%end_pts ) + 1  ! why is this off by one?
	!print *, 'npts = ', glyph%npts

	! Read flags
	allocate(glyph%flags( glyph%npts ))
	i = 0
	do while (i < glyph%npts)
		i = i + 1

		flag = int(read_u8(iu), 2)
		glyph%flags(i) = flag
		!print '(a,z2)', 'flag = ', flag

		if (iand(flag, REPEAT_) /= 0) then
			nrepeat = int(read_u8(iu), 2)
			!print *, 'nrepeat = ', nrepeat

			do j = 1, nrepeat
				i = i + 1
				glyph%flags(i) = flag
			end do

		end if

	end do
	!print *, 'flags = ', glyph%flags

	! Read coordinates
	allocate(glyph%x(ND, glyph%npts))
	do j = 1, ND  ! x/y loop

		is_byte = int(j * X_IS_BYTE, 2)
		delta   = int(j * X_DELTA  , 2)

		pos = 0
		do i = 1, glyph%npts
			flag = glyph%flags(i)

			if (iand(flag, is_byte) /= 0) then
				if (iand(flag, delta) /= 0) then
					!print *, '+ u8'
					pos = pos + read_u8(iu)
				else
					!print *, '- u8'
					pos = pos - read_u8(iu)
				end if

			! Fortran doesn't have bitwise not, so we use ieor with all f's
			else if (iand( ieor(int(flag,4), int(z'ffff',4)), int(delta,4) ) /= 0) then
				!print *, '+ i16'
				pos = pos + read_i16(iu)

			!else
			!	print *, 'nop'
			!	! pos is unchanged
			end if

			glyph%x(j,i) = pos
			!print *, 'pos = ', pos
		end do
	end do

	!print *, 'x, y = '
	!print '(2i6)', glyph%x
	!print *, ''

end function read_glyph

!===============================================================================

subroutine draw_str(cv, color, ttf, utf8_str, x0, y0, pix_per_em)

	! Draw a string on canvas cv starting at pixel x0 left, y0 bottom
	!
	! TODO: encapsulate color and pix_per_em into a "style" struct.  Also
	! include letter-spacing (tracking).  Keep this fn and make a copy for
	! compatibility, then update tests and delete current fn

	use utf_m

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color

	type(ttf_t  ), intent(in) :: ttf

	! This could just be an ASCII string, since UTF8 is compatible with ASCII
	character(len = *), intent(in) :: utf8_str

	integer, intent(in) :: x0, y0 ! translation
	double precision, intent(in) :: pix_per_em

	!********

	double precision :: pix_per_unit

	integer :: i, x
	integer(kind  = 4), allocatable :: utf32_str(:)
	integer(kind = 8) :: iglyph

	type(transform_t) :: t

	write(*,*) 'Drawing string "'//utf8_str//'" ...'

	!print *, 'len = ', len(utf8_str)

	allocate(utf32_str(0)) ! workaround

	! There's a little memory overhead here converting the whole string to
	! utf32. We only need to convert 1 char at a time to draw it
	utf32_str = to_utf32(utf8_str)

	! Convert from font units to pixels
	pix_per_unit = pix_per_em / ttf%units_per_em

	!print *, 'utf32_str = ', utf32_str

	! t is initialized to identity in type declaration

	x = x0
	do i = 1, size(utf32_str)
		!print *, utf32_str(i)
		iglyph = get_index(utf32_str(i), ttf)
		call draw_glyph(cv, color , ttf, ttf%glyphs(iglyph), &
			x, y0, pix_per_em, t)

		! "the advanceWidth is assumed to be the same as the advanceWidth for the
		! last entry above. The number of entries in this array is derived from
		! the total number of glyphs minus numOfLongHorMetrics. This generally
		! is used with a run of monospaced glyphs" -- hmtx docs
		x = x + nint(pix_per_unit * ttf%advance_widths(min(iglyph, ttf%nlong_mtx - 1 )))
		!print *, 'advance_width = ', ttf%advance_widths( iglyph )

	end do

end subroutine draw_str

!===============================================================================

subroutine draw_glyph(cv, color, ttf, glyph, x0, y0, pix_per_em, t)

	! Draw a glyph translated horizontally by x0 and vertically by y0

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color

	type(ttf_t  ), intent(in) :: ttf
	type(glyph_t), intent(in) :: glyph

	integer, intent(in) :: x0, y0 ! translation
	double precision, intent(in) :: pix_per_em

	type(transform_t), intent(in) :: t  ! general affine transform for compound glyphs

	!********

	double precision :: pix_per_unit, a(ND), b(ND), c(ND), p(ND), s
	double precision, allocatable :: x(:,:)

	integer :: it, n, ip(ND), ip0(ND), ipd(ND), ix, iy, xmin, ymin, xmax, ymax
	integer :: wind_inc, wind_inc0, wind_incd, wind_num
	integer, allocatable :: wind(:,:)
	integer(kind = 2) :: flag, flagn, flagp
	integer(kind = 8) :: i, j, start_pt, jp, jn

	logical :: first, defer

	!! TODO: debugging only
	!integer(kind = 4) :: red, grn
	!red = new_color(int(z'ff0000ff',8))
	!grn = new_color(int(z'00ff00ff',8))

	if (glyph%ncontours < 0) then
		!print *, 'drawing '//to_str(glyph%ncomps)//' components ...'

		do i = 1, glyph%ncomps
			call draw_glyph(cv, color, ttf, &
				ttf%glyphs( glyph%comps(i)%index_ ), x0, y0, pix_per_em, &
				            glyph%comps(i)%t)
		end do

		return
	end if

	!print *, 'npts      = ', glyph%npts
	!print *, 'ncontours = ', glyph%ncontours
	!print *, 'end_pts   = ', glyph%end_pts
	!print *, 'x = '
	!print '(2i6)', glyph%x

	! Convert from font units to pixels
	pix_per_unit = pix_per_em / ttf%units_per_em

	!x = glyph%x
	allocate(x( size(glyph%x,1), size(glyph%x,2)))

	do i = 1, glyph%npts

		! Apply t transform first
		x(:,i) = &
			[    &
				t%m * (t%a * glyph%x(1,i) / t%m + t%c * glyph%x(2,i) / t%m + t%e), &
				t%n * (t%b * glyph%x(1,i) / t%n + t%d * glyph%x(2,i) / t%n + t%f)  &
			]

		! Apply global transform (resolution, translation)
		x(1,i) =  pix_per_unit * x(1,i)
		x(2,i) = -pix_per_unit * x(2,i)  ! invert y for y-down img coords

	end do

	! Transformed bounding box
	xmin = nint(minval(x(1,:))) - 1
	xmax = nint(maxval(x(1,:))) + 1
	ymin = nint(minval(x(2,:))) - 1
	ymax = nint(maxval(x(2,:))) + 1

	allocate(wind(xmin: xmax, ymin: ymax))
	wind = 0
	ip0 = 0

	start_pt = 1
	do i = 1, glyph%ncontours
		first = .true.
		defer = .false.
		wind_inc0 = 0

		do j = start_pt, glyph%end_pts(i) + 1
			!print '(2i8)', x(:,j)
			!print *, 'j, x = ', j, x(:,j)

			! Next point index, which might loop back to beginning of contour
			if (j == glyph%end_pts(i) + 1) then
				jn = start_pt
			else
				jn = j + 1
			end if

			flag  = glyph%flags(j )
			flagn = glyph%flags(jn)

			if (iand(flag , ON_CURVE) /= 0 .and. &
			    iand(flagn, ON_CURVE) /= 0) then

				! Current and next points are on curve.  Draw a line segment.

				n = 2 * nint(maxval(abs(x(:,jn) - x(:,j)))) + 1
				do it = 0, n
			
					s = 1.d0 * it / n
					p = x(:,j) + s * (x(:,jn) - x(:,j))
					ip = nint(p)
					if (first .or. any(ip /= ip0)) then
						call draw_pixel(cv, color, ip + [x0,y0])
					end if

					wind_inc = sign_(ip(2) - ip0(2))
					if (.not. first .and. wind_inc /= 0) then

						if (wind_inc0 == 0 .and. .not. defer) then
							defer = .true.
							ipd = ip0
							wind_incd = wind_inc
						end if

						if (wind_inc0 + wind_inc == 0) then
							wind(ip0(1), ip0(2)) = wind(ip0(1), ip0(2)) + wind_inc
						end if

						wind(ip(1), ip(2)) = wind(ip(1), ip(2)) + wind_inc
						wind_inc0 = wind_inc

					end if

					ip0 = ip
					first = .false.
			
				end do

			else if (iand(flag , ON_CURVE) == 0) then

				! Current point j is off-curve
				!
				! Draw a quadratic Bezier spline with point index j as the
				! middle control point:
				!
				!     x(:,j-1), x(:,j), x(:,j+1)
				!     a       , b     , c
				!
				! In the general case, j-1 and j+1 may wrap around the start/end
				! of the contour.  Further, j-1 or j+1 may not be explicitly
				! defined but might be an implicit midpoint between succesive
				! off-curve points.  There is logic to handle this below.

				! Previous point index
				if (j == start_pt) then
					jp = glyph%end_pts(i) + 1
				else
					jp = j - 1
				end if
				flagp = glyph%flags(jp)

				! middle control point off curve
				b = x(:,j)

				! a is start point on curve
				if (iand(flagp, ON_CURVE) /= 0) then
					a = x(:,jp)
				else
					a = 0.5d0 * (x(:,j) + x(:,jp))
				end if

				! c is end point on curve
				if (iand(flagn, ON_CURVE) /= 0) then
					c = x(:,jn)
				else
					c = 0.5d0 * (x(:,j) + x(:,jn))
				end if

				n = 2 * nint(norm2(dble(b - a)) + norm2(dble(c - b))) + 1
				do it = 0, n

					s = 1.d0 * it / n
					p = (1-s)**2 * a + 2*(1-s)*s * b + s**2 * c
					ip = nint(p)
					if (first .or. any(ip /= ip0)) then
						call draw_pixel(cv, color, ip + [x0,y0])
					end if

					wind_inc = sign_(ip(2) - ip0(2))
					if (.not. first .and. wind_inc /= 0) then

						if (wind_inc0 == 0 .and. .not. defer) then
							defer = .true.
							ipd = ip0
							wind_incd = wind_inc
						end if

						if (wind_inc0 + wind_inc == 0) then
							wind(ip0(1), ip0(2)) = wind(ip0(1), ip0(2)) + wind_inc
						end if

						wind(ip(1), ip(2)) = wind(ip(1), ip(2)) + wind_inc
						wind_inc0 = wind_inc

					end if

					ip0 = ip
					first = .false.

				end do

			end if
		end do

		!! Mark special points for visual debugging (1st point and deferred point)
		!call draw_pixel(cv, new_color(int(z'0000ffff',8)), nint(x(:,start_pt)))
		!call draw_pixel(cv, new_color(int(z'ff00ffff',8)), ipd)

		! After wrapping around the entire contour, we can mark the deferred point
		if (defer .and. wind_incd /= wind_inc0) then
			wind(ipd(1), ipd(2)) = wind(ipd(1), ipd(2)) + wind_incd
		end if

		start_pt = glyph%end_pts(i) + 2
	end do

	! TODO: this could be optimized by cropping the glyph loop within the canvas
	! bounds and not checking bounds within draw_pixel().  y is trivial, but x
	! still needs to count winding number starting from xmin.  still need a
	! safe bound-checking version of draw_pixel() for the outlines above
	do iy = ymin, ymax
	wind_num = 0
	do ix = xmin, xmax

		!! Visualize entry/exit points for debugging
		!if (wind(ix,iy) > 0) call draw_pixel(cv, grn, [ix,iy])
		!if (wind(ix,iy) < 0) call draw_pixel(cv, red, [ix,iy])

		! Fill contours
		wind_num = wind_num + wind(ix,iy)
		if (wind_num /= 0) call draw_pixel(cv, color, [ix+x0, iy+y0])

	end do
	end do

end subroutine draw_glyph

!===============================================================================

integer function sign_(x)

	! -1, 0, or +1

	integer, intent(in) :: x

	if (x == 0) then
		sign_ = 0
		return
	end if

	sign_ = sign(1, x)

end function sign_

!===============================================================================

subroutine draw_pixel(cv, color, ip)

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color
	integer, intent(in) :: ip(ND)

	! Check bounds.  TODO: set a flag to log *one* warning (not a warning
	! per-pixel)
	if (1 <= ip(1) .and. ip(1) <= size(cv,1) .and. &
	    1 <= ip(2) .and. ip(2) <= size(cv,2)) then

		cv(ip(1), ip(2)) = color

	end if

end subroutine draw_pixel

!===============================================================================

function ldt2str(ldt) result(str)

	! Format ttf long date time to string.  ldt is number of seconds since 1904

	integer(kind = 8), intent(in) :: ldt
	character(len = :), allocatable :: str

	!********

	! Number of seconds from 1904 to unix epoch (1970)
	integer(kind = 8), parameter :: dt_s_1904_1970 = 2082844800

	str = ctime(ldt - dt_s_1904_1970)

end function ldt2str

!===============================================================================

function read_ttf_table(iu) result(table)

	! Read table metadata from the header of the ttf file

	integer, intent(in) :: iu

	type(ttf_table) :: table

	!********

	integer(kind = 8) :: i, old, sum_

	!print *, 'starting read_ttf_table()'

	table%tag      = read_str(iu, 4)
	table%checksum = read_u32(iu)
	table%offset   = read_u32(iu)
	table%length   = read_u32(iu)

	!print *, 'tag    = ', table%tag
	!print *, 'offset = ', table%offset
	!print *, 'length = ', table%length
	!print '(a,z0)', ' csum   = ', table%checksum
	!print '(a,z0)', 'length = ', table%length
	!print *, 'offset + length = ', table%offset + table%length
	!print '(a,z0)', ' offset + length = ', table%offset + table%length
	!print *, ''

	if (table%tag == "head") return

	! Verify checksum
	old = ftell(iu)
	!print '(a,z0)', ' old = ', old

	call fseek(iu, table%offset, SEEK_ABS)

	sum_ = 0

	! Number of 4-byte words is ceiling(num_bytes / 4)
	do i = 1, (table%length + 3) / 4
		sum_ = sum_ + read_u32(iu)
	end do
	!print '(a,z0)', '  sum_   = ', sum_

	sum_ = iand(sum_, int(z'ffffffff',8))
	!print '(a,z0)', ' final  sum_   = ', sum_
	!print '(a,z0)', ' final csum   = ', table%checksum

	if (sum_ /= table%checksum) then
		write(*,*) ERROR//'bad checksum for table ', table%tag
		call exit(EXIT_FAILURE)
	end if

	call fseek(iu, old, SEEK_ABS)

end function read_ttf_table

!===============================================================================

subroutine delete_file(filename)
	character(len = *), intent(in) :: filename
	!********
	integer :: iu, io
	open(newunit = iu, iostat = io, file = filename, status = 'old')
	if (io == 0) close(iu, status = 'delete')
end subroutine delete_file

!===============================================================================

subroutine write_img_diff(cv1, cv2, filename)

	! Write a diff between two canvases with differences highlighted in magenta

	integer(kind = 4), intent(in) :: cv1(:,:), cv2(:,:)
	character(len = *), intent(in) :: filename
	!********
	integer(kind = 4), allocatable :: cv(:,:)
	integer(kind = 4) :: diff_color
	integer :: ix, iy, ndiff

	allocate(cv(0,0))
	cv = cv1

	diff_color = new_color(int(z'ff00ffff',8))
	ndiff = 0
	do iy = 1, size(cv,2)
	do ix = 1, size(cv,1)
		if (cv1(ix,iy) /= cv2(ix,iy)) then
			cv(ix,iy) = diff_color
			ndiff = ndiff + 1
		end if
	end do
	end do

	write(*,*) FG_BOLD_BRIGHT_RED//to_str(ndiff)//" pixels changed"//COLOR_RESET
	call write_img(cv, filename)

end subroutine write_img_diff

!===============================================================================

subroutine write_img(cv, filename)

	! Write a canvas cv to a ppm image file

	integer(kind = 4), intent(in) :: cv(:,:)

	character(len = *), intent(in) :: filename

	!********

	integer :: iu, io, ix, iy

	call delete_file(filename)
	open(newunit = iu, file = filename, action = 'write', iostat = io, &
		access = 'stream', status = 'new')
	if (io /= EXIT_SUCCESS) then
		write(*,*) ERROR//'cannot open file "', filename, '"'
		call exit(EXIT_FAILURE)
	end if

	write(iu) 'P6' //LINE_FEED
	write(iu) to_str(size(cv, 1))//' '//to_str(size(cv, 2))//LINE_FEED
	write(iu) '255'//LINE_FEED

	do iy = 1, size(cv, 2)
		do ix = 1, size(cv, 1)
			write(iu) int(ishft(cv(ix,iy), -3 * 8), 1) ! R
			write(iu) int(ishft(cv(ix,iy), -2 * 8), 1) ! G
			write(iu) int(ishft(cv(ix,iy), -1 * 8), 1) ! B
			! No alpha in ppm
		end do
	end do

	close(iu)
	write(*,*) 'Finished writing file "', filename, '"'

end subroutine write_img

!===============================================================================

function read_img(filename) result(cv)

	! Read a ppm image file

	character(len = *), intent(in) :: filename

	integer(kind = 4), allocatable :: cv(:,:)

	!********

	character(len = :), allocatable :: str

	integer :: iu, io, ix, iy, width, height
	integer(kind = 8) :: pos_beg, pos_end

	open(newunit = iu, file = filename, action = 'read', iostat = io, &
		access = 'stream')
	if (io /= EXIT_SUCCESS) then
		write(*,*) ERROR//'cannot open file "', filename, '"'
		call exit(EXIT_FAILURE)
	end if
	write(*,*) 'Reading file "', filename, '" ...'

	! TODO: parameterize P6 and share between reader/writer
	str = read_str(iu, 2)
	if (str /= 'P6') then
		write(*,*) ERROR//'bad magic number in ppm file'
		call exit(EXIT_FAILURE)
	end if

	str = read_str(iu, 1)
	str = ''
	pos_beg = ftell(iu)
	do while (str /= LINE_FEED)
		str = read_str(iu, 1)
	end do
	pos_end = ftell(iu)
	!print *, 'pos beg/end = ', pos_beg, pos_end

	call fseek(iu, pos_beg, SEEK_ABS)
	str = read_str(iu, int(pos_end - pos_beg))  ! includes newline
	!print *, 'str = "'//str//'"'

	read(str, *) width, height
	!print *, 'width, height = ', width, height

	! Skip "255\n"
	str = ''
	do while (str /= LINE_FEED)
		str = read_str(iu, 1)
	end do

	allocate(cv(width, height))
	do iy = 1, size(cv, 2)
		do ix = 1, size(cv, 1)
			cv(ix,iy) = ior(int(ishft(read_u8(iu) , 3 * 8), 4), &
			            ior(int(ishft(read_u8(iu) , 2 * 8), 4), &
			            ior(int(ishft(read_u8(iu) , 1 * 8), 4), &
			                int(ishft(int(z'ff',2), 0 * 8), 4))))
		end do
	end do

	close(iu)

end function read_img

!===============================================================================

function i32_to_str(i) result(str)

	integer(kind = 4), intent(in) :: i
	character(len = :), allocatable :: str

	!********

	character(len = 64) :: buffer

	write(buffer, '(i0)') i
	str = trim(buffer)

end function i32_to_str

!===============================================================================

function i64_to_str(i) result(str)

	integer(kind = 8), intent(in) :: i
	character(len = :), allocatable :: str

	!********

	character(len = 64) :: buffer

	write(buffer, '(i0)') i
	str = trim(buffer)

end function i64_to_str

!===============================================================================

function new_canvas(width, height, bg_color) result(cv)

	integer, intent(in) :: width, height
	integer(kind = 4), intent(in) :: bg_color
	integer(kind = 4), allocatable :: cv(:,:) ! canvas

	allocate(cv(width, height))
	cv = bg_color

end function new_canvas

!===============================================================================

function new_color(i8) result(color)

	! This is a helper fn to cast a hex color in the format 0xrrggbbaa into a
	! 4-byte signed Fortran integer.  Call it like this:
	!
	!     !                       rrggbbaa
	!     green = new_color(int(z'00ff00ff',8))
	!
	! Casting the arg to a temporary 8-byte int is necessary to prevent overflow

	integer(kind = 8), intent(in) :: i8
	integer(kind = 4) :: color

	if (i8 <= huge(color)) then
		color = int(i8,4)
	else
		color = int(i8 - huge(i8) - 1, 4)
	end if

end function new_color

!===============================================================================

function basename(filename)
	character(len = *), intent(in)  :: filename
	character(len = :), allocatable :: basename
	!********
	integer :: beg_, end_, i

	beg_ = 1
	end_ = len(filename)

	!print *, 'len = ', end_

	i = scan(filename, '/\', .true.)
	if (i /= 0) beg_ = i + 1

	i = scan(filename(beg_:), '.')
	if (i /= 0) end_ = beg_ + i - 2

	basename = filename(beg_: end_)
	!print *, 'beg_, end_ = ', beg_, end_

end function basename

!===============================================================================

function rand_dark() result(color)

	integer(kind = 4) :: color

	color = ior(int(ishft(int(rand() * 85), 3 * 8), 4), &
	        ior(int(ishft(int(rand() * 85), 2 * 8), 4), &
	        ior(int(ishft(int(rand() * 85), 1 * 8), 4), &
	            int(ishft(int(z'ff',2), 0 * 8), 4))))

end function rand_dark

!===============================================================================

function rand_light() result(color)

	integer(kind = 4) :: color

	color = ior(int(ishft(int(255 - rand() * 85), 3 * 8), 4), &
	        ior(int(ishft(int(255 - rand() * 85), 2 * 8), 4), &
	        ior(int(ishft(int(255 - rand() * 85), 1 * 8), 4), &
	            int(ishft(int(z'ff',2), 0 * 8), 4))))

end function rand_light

!===============================================================================

subroutine specimen(ttf_filename)

	! Make a specimen for the given ttf file

	character(len = *), intent(in) :: ttf_filename

	!********

	character(len = :), allocatable :: str, ppm_filename

	double precision :: pix_per_em

	integer :: line_height, lmargin, factor, seed
	integer(kind = 4) :: fg, fg2, fg3, fg4, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:)!, cv2(:,:)

	type(ttf_t)  :: ttf!, ttfi

	ttf  = read_ttf(ttf_filename)
	!ttfi = read_ttf('./fonts/cormorant-garamond/CormorantGaramond-Italic.ttf')

	! Seed RNG with checksum adjustment (happens to be the same for a lot of fonts)
	seed = int(ttf%checksum_adj, 4)

	!! Seed RNG with filename char sum for random color generation
	!seed = 0
	!do i = 1, len(ttf_filename)
	!	seed = seed + iachar(ttf_filename(i:i))
	!	print *, 'iachar, seed = ', iachar(ttf_filename(i:i)), seed
	!end do

	!print *, 'seed = ', seed
	call srand(seed)

	! foreground/background colors
	fg  = new_color(int(z'000000ff',8))
	fg2 = rand_dark()
	fg3 = rand_dark()
	fg4 = new_color(int(z'ffffffff',8))
	bg  = rand_light()
	bg2 = rand_dark()

	allocate(cv(0,0))

	factor = 1

	cv = new_canvas(800*factor, 945*factor, bg)
	cv(:, 631*factor:) = bg2

	pix_per_em = 75.d0*factor
	line_height = nint(1.2 * pix_per_em)
	lmargin = 20*factor

	str = basename(ttf_filename)
	call draw_str(cv, fg, ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "Aa Ee Rr"
	call draw_str(cv, fg2, ttf , str, lmargin, 2 * line_height, pix_per_em)
	call draw_str(cv, fg2, ttf , str, lmargin, 3 * line_height, pix_per_em)

	str = "Q"
	call draw_str(cv, fg2, ttf, str, 450*factor, nint(4.5 * line_height), 5 * pix_per_em)

	str = "Calligraphy"
	call draw_str(cv, fg3, ttf, str, 150*factor, 6 * line_height, pix_per_em)

	! TODO: increase spacing for remaining strs.  need extra arg for draw_str.
	! maybe make style struct? fg, pix_per_em, and hor spacing factor
	str = "abcdefghijklm"
	!str = "გთხოვთ ახლავე გაიაროთ"
	call draw_str(cv, fg4, ttf , str, lmargin, 8 * line_height, pix_per_em)

	str = "nopqrstuvwxyz"
	!str = "კონფერენციაზე დასასწრებად"
	call draw_str(cv, fg4, ttf , str, lmargin, 9 * line_height, pix_per_em)

	str = "0123456789"
	call draw_str(cv, fg4, ttf , str, 200*factor, 10 * line_height, pix_per_em)

	ppm_filename = "./build/"//basename(ttf_filename)//".ppm"
	call write_img(cv, ppm_filename)

end subroutine specimen

!===============================================================================

end module cali_m

!===============================================================================

! TODO:
! - horizontal spacing
!   * letter-spacing (tracking) style arg: update pet sounds demo
!   * more advanced kerning using the 'kern' table
!   * advanceWidth from 'hmtx' table done
! - waterfall test
! - other config args?  json input for app only
!   * cmd arg for specimen vs markdown render vs waterfall etc.
!   * img size
!   * fg, bg colors
!   * font filename
!   * text filename (or directly as a string)
!   * resolution / font size
! - ligatures
! - anti-aliasing?  doubtful
! - png export, OpenGL/SDL bindings?
! - vector output format, e.g. ps, pdf, or svg?
! - other cmap formats?  need examples for testing.  formats 0, 4, and 6 cover
!   almost all ~500 Windows fonts (except seguiemj.ttf in format 14) and a
!   handful of examples for Google Open Fonts
! - cmap format 14, e.g. seguiemj.ttf?
!   * "This is also the only 'cmap' subtable which does not stand by itself and
!     is not completely independent of all others; a 'cmap' may not consist of a
!     type 14 subtable alone"

