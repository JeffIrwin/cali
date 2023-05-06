
!===============================================================================

module cali_m

	implicit none

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
			FG_BRIGHT_GREEN    = ESC//'[92m', &
			COLOR_RESET        = ESC//'[0m'

	character(len = *), parameter :: &
		ERROR = FG_BOLD_BRIGHT_RED//'Error'//color_reset//': '

	!********

	type ttf_table
		! Table metadata from ttf file header
		character(len = :), allocatable :: tag
		integer(kind = 8) :: checksum, offset, length
	end type ttf_table

	!********

	type glyph_t
		integer(kind = 8) :: ncontours, xmin, ymin, xmax, ymax, ninst, npts
		integer(kind = 8), allocatable :: x(:,:), end_pts(:)  ! TODO: use 4-bytes for x
		integer(kind = 2), allocatable :: flags(:)
	end type glyph_t

	!********

	type cmap_t
		integer(kind = 8) :: fmt_, length, language, nseg_x2, search_range, &
			entry_select, range_shift, platform_id, platform_sid, offset, &
			reserved_pad

		integer(kind = 8), allocatable :: end_code(:), start_code(:), &
			id_delta(:), id_range_offset(:), glyph_index_array(:)

	!UInt16	format	Format number is set to 4
	!UInt16	length	Length of subtable in bytes
	!UInt16	language	Language code (see above)
	!UInt16	segCountX2	2 * segCount
	!UInt16	searchRange	2 * (2**FLOOR(log2(segCount)))
	!UInt16	entrySelector	log2(searchRange/2)
	!UInt16	rangeShift	(2 * segCount) - searchRange
	!UInt16	endCode[segCount]	Ending character code for each segment, last = 0xFFFF.
	!UInt16	reservedPad	This value should be zero
	!UInt16	startCode[segCount]	Starting character code for each segment
	!UInt16	idDelta[segCount]	Delta for all character codes in segment
	!UInt16	idRangeOffset[segCount]	Offset in bytes to glyph indexArray, or 0
	!UInt16	glyphIndexArray[variable]	Glyph index array

	end type cmap_t

	!********

	type ttf_t

		integer(kind = 8) :: scalar_type, ntables, search_range, &
			entry_select, range_shift, checksum_adj, magic_num, flags, &
			units_per_em, xmin, ymin, xmax, ymax, mac_style, low_rec_ppem, &
			font_dir_hint, index2loc_fmt, glyph_data_fmt, nglyphs, &
			cmap_vers, ncmap_tables

		! Table offsets, cached to avoid multiple calls to get_table()
		integer(kind = 8) :: glyf_offset, loca_offset, cmap_offset

		! Dates in ttf long date time format (i64 seconds since 1904)
		integer(kind = 8) :: created, modified

		double precision :: vers, font_rev, maxp_vers

		type(glyph_t  ), allocatable :: glyphs(:)
		type(ttf_table), allocatable :: tables(:)
		type(cmap_t) :: cmap

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
	integer(kind = 2) :: fword

	fword = read_i16(unit)

end function read_fword

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
	integer(kind = 8) :: i, head, maxp, sum_, filesize

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
	head = ttf%get_table("head")
	call fseek(iu, ttf%tables(head)%offset, SEEK_ABS)

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

	write(*, '(a,i0)') ' Units per em = ', ttf%units_per_em

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

	!print *, 'mac_style      = ', ttf%mac_style
	!print *, 'low_rec_ppem   = ', ttf%low_rec_ppem
	!print *, 'font_dir_hint  = ', ttf%font_dir_hint
	!print *, 'index2loc_fmt  = ', ttf%index2loc_fmt
	!print *, 'glyph_data_fmt = ', ttf%glyph_data_fmt

	maxp = ttf%get_table("maxp")
	call fseek(iu, ttf%tables(maxp)%offset, SEEK_ABS)
	ttf%maxp_vers = read_fixed(iu)
	ttf%nglyphs   = read_u16(iu)

	!print *, 'maxp_vers      = ', ttf%maxp_vers
	write(*, '(a,i0)') ' Number of glyphs = ', ttf%nglyphs

	ttf%cmap_offset = ttf%tables( ttf%get_table("cmap") )%offset
	call fseek(iu, ttf%cmap_offset, SEEK_ABS)
	ttf%cmap_vers    = read_u16(iu)
	ttf%ncmap_tables = read_u16(iu)

	! TODO: iterate below for each ncmap_tables?  Maybe I only need the first
	! subtable?
	ttf%cmap%platform_id  = read_u16(iu)
	ttf%cmap%platform_sid = read_u16(iu)
	ttf%cmap%offset       = read_u32(iu)

	print *, 'cmap_vers       = ', ttf%cmap_vers
	print *, 'ncmap_tables    = ', ttf%ncmap_tables
	print *, 'platform_id     = ', ttf%cmap%platform_id
	print *, 'platform_sid    = ', ttf%cmap%platform_sid
	print *, 'cmap_sub_offset = ', ttf%cmap%offset

	!	integer :: fmt_, length, language, nseg_x2, search_range, &
	!		entry_select, range_shift, platform_id, platform_sid, offset

	! Jump to cmap sub offset relative to cmap_offset
	call fseek(iu, ttf%cmap_offset + ttf%cmap%offset, SEEK_ABS)
	ttf%cmap%fmt_ = read_u16(iu)

	if (ttf%cmap%fmt_ /= 4) then
		write(*,*) ERROR//'only cmap format 4 is supported'
		call exit(-1)
	end if

	ttf%cmap%length = read_u16(iu)
	ttf%cmap%language = read_u16(iu)
	ttf%cmap%nseg_x2 = read_u16(iu)
	ttf%cmap%search_range = read_u16(iu)
	ttf%cmap%entry_select = read_u16(iu)
	ttf%cmap%range_shift = read_u16(iu)


!		integer(kind = 8), allocatable :: end_code(:), start_code(:), &
!			id_delta(:), id_range_offset(:), glyph_index_array(:)

	allocate(ttf%cmap%end_code       ( ttf%cmap%nseg_x2 / 2 ))
	allocate(ttf%cmap%start_code     ( ttf%cmap%nseg_x2 / 2 ))
	allocate(ttf%cmap%id_delta       ( ttf%cmap%nseg_x2 / 2 ))
	allocate(ttf%cmap%id_range_offset( ttf%cmap%nseg_x2 / 2 ))
	!allocate(ttf%cmap%glyph_index_array( ??? ))

	do i = 1, ttf%cmap%nseg_x2 / 2
		ttf%cmap%end_code(i) = read_u16(iu)
	end do
	ttf%cmap%reserved_pad = read_u16(iu)
	do i = 1, ttf%cmap%nseg_x2 / 2
		ttf%cmap%start_code(i) = read_u16(iu)
	end do
	do i = 1, ttf%cmap%nseg_x2 / 2
		! "NOTE: All idDelta[i] arithmetic is modulo 65536." -- apple
		ttf%cmap%id_delta(i) = read_u16(iu)
		if (ttf%cmap%id_delta(i) /= 0) then
			!ttf%cmap%id_delta(i) = ttf%cmap%id_delta(i) - int(z'ffff')
			ttf%cmap%id_delta(i) = ttf%cmap%id_delta(i) - int(z'10000')
		end if
	end do
	do i = 1, ttf%cmap%nseg_x2 / 2
		ttf%cmap%id_range_offset(i) = read_u16(iu)
	end do
	!do i = 1, ???
	!	ttf%cmap%glyph_index_array(i) = read_u16(iu)
	!end do

	print *, 'cmap fmt_         = ', ttf%cmap%fmt_
	print *, 'cmap length       = ', ttf%cmap%length
	print *, 'cmap language     = ', ttf%cmap%language
	print *, 'cmap nseg_x2      = ', ttf%cmap%nseg_x2
	print *, 'cmap search_range = ', ttf%cmap%search_range
	print *, 'cmap entry_select = ', ttf%cmap%entry_select
	print *, 'cmap range_shift  = ', ttf%cmap%range_shift
	print *, 'cmap end_code = ', ttf%cmap%end_code
	print *, ''
	print *, 'cmap reserved_pad  = ', ttf%cmap%reserved_pad
	print *, 'cmap start_code = ', ttf%cmap%start_code
	print *, ''
	print *, 'cmap id_delta = ', ttf%cmap%id_delta
	print *, ''
	print *, 'cmap id_range_offset = ', ttf%cmap%id_range_offset
	print *, ''

	!UInt16	format	Format number is set to 4
	!UInt16	length	Length of subtable in bytes
	!UInt16	language	Language code (see above)
	!UInt16	segCountX2	2 * segCount
	!UInt16	searchRange	2 * (2**FLOOR(log2(segCount)))
	!UInt16	entrySelector	log2(searchRange/2)
	!UInt16	rangeShift	(2 * segCount) - searchRange
	!UInt16	endCode[segCount]	Ending character code for each segment, last = 0xFFFF.
	!UInt16	reservedPad	This value should be zero
	!UInt16	startCode[segCount]	Starting character code for each segment
	!UInt16	idDelta[segCount]	Delta for all character codes in segment
	!UInt16	idRangeOffset[segCount]	Offset in bytes to glyph indexArray, or 0
	!UInt16	glyphIndexArray[variable]	Glyph index array

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

function get_index(utf32, ttf) result(i)

	!character(len = *), intent(in) :: ch
	integer(kind = 4) :: utf32
	type(ttf_t), intent(in) :: ttf
	integer(kind = 8) :: i

	!********

	integer :: seg
	integer(kind = 4) :: codepoint

	!print *, 'get_index("p") = ', get_index("p", ttf)

	! TODO: remove unecessary variable codepoint

	!codepoint = iachar(ch)  ! ASCII only
	codepoint = utf32
	print *, 'codepoint = ', codepoint

	! Search for the first endCode that is greater than or equal to the
	! character code to be mapped.  Could use a binary search
	seg = 1
	do while (ttf%cmap%end_code(seg) < codepoint)
		seg = seg + 1
	end do
	print *, 'seg = ', seg

	if (ttf%cmap%start_code(seg) > ttf%cmap%end_code(seg)) then
		! Missing character glyph
		i = 0
		return
	end if

	if (ttf%cmap%id_range_offset(seg) /= 0) then
		! TODO
		write(*,*) ERROR//' non-zero id_range_offset not implemented'
		call exit(-1)
	end if

	!i = codepoint + ttf%cmap%id_delta(seg) - 1
	i = codepoint + ttf%cmap%id_delta(seg)

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

	integer :: j
	integer(kind = 8) :: i, offset, pos
	integer(kind = 2) :: flag, nrepeat, is_byte, delta

	if (ttf%index2loc_fmt == 0) then
		call fseek(iu, ttf%loca_offset + 2 * iglyph, SEEK_ABS)
		offset = 2 * read_u16(iu)
	else
		call fseek(iu, ttf%loca_offset + 4 * iglyph, SEEK_ABS)
		offset = read_u32(iu)
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
		return

		!! TODO: read compound glyph.  Maybe we could make this not a fatal
		!! error, unless you try to *draw* a compound glyph which wasn't loaded
		!write(*,*) ERROR// &
		!	'compound glyphs are not supported for glyph index ', iglyph
		!call exit(EXIT_FAILURE)
	end if

	! Read simple glyph

	allocate(glyph%end_pts( glyph%ncontours ))
	do i = 1, glyph%ncontours
		glyph%end_pts(i) = read_u16(iu)
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

end function read_glyph

!===============================================================================

subroutine draw_glyph(cv, color, ttf, glyph, x0, y0, pix_per_em)

	! Draw a glyph transated horizontally by x0

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color

	type(ttf_t  ), intent(in) :: ttf
	type(glyph_t), intent(in) :: glyph

	! TODO: units.  Maybe encapsulate x0, y0, scaling as more general transform?
	integer, intent(in) :: x0, y0 ! translation
	double precision, intent(in) :: pix_per_em

	!********

	double precision :: scaling

	integer(kind = 2) :: flag, flagn, flagp
	integer(kind = 8) :: i, j, start_pt, jp, jn, a(ND), b(ND), c(ND)
	integer(kind = 8), allocatable :: x(:,:)  ! TODO: double?  it's scaled

	if (glyph%ncontours < 0) then
		write(*,*) ERROR//'compound glyphs are not supported'
		call exit(EXIT_FAILURE)
	end if

	!print *, 'npts      = ', glyph%npts
	!print *, 'ncontours = ', glyph%ncontours
	!print *, 'end_pts   = ', glyph%end_pts
	!print *, 'x = '
	!print '(2i6)', glyph%x

	! Convert from font units to pixels
	scaling = pix_per_em / ttf%units_per_em

	! TODO: render onto a subcanvas with a local origin at x == 0, then
	! translate and blend that into the global canvas to avoid resolution
	! issues at high x0
	x = glyph%x
	do i = 1, glyph%npts
		x(1,i) = int( scaling * x(1,i) + x0)
		x(2,i) = int(-scaling * x(2,i) + y0)  ! invert y for y-down img coords
	end do

	start_pt = 1
	do i = 1, glyph%ncontours

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

			! TODO: make is_on_curve(flag) fn.  This is not readable and meaning
			! of == 0 or /= 0 is not clear
			if (iand(flag , ON_CURVE) /= 0 .and. &
			    iand(flagn, ON_CURVE) /= 0) then

				call draw_line(cv, color, x(:,j), x(:,jn))

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
					a = int(0.5d0 * (x(:,j) + x(:,jp)))
				end if

				! c is end point on curve
				if (iand(flagn, ON_CURVE) /= 0) then
					c = x(:,jn)
				else
					c = int(0.5d0 * (x(:,j) + x(:,jn)))
				end if

				call draw_bezier2(cv, color, a, b, c)

			end if
		end do

		start_pt = glyph%end_pts(i) + 2
	end do

end subroutine draw_glyph

!===============================================================================

subroutine draw_bezier2(cv, color, p1, p2, p3)

	! Draw a quadratic Bezier curve from start point p1 to end point p3 with
	! middle off-curve control point p2 on canvas cv

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color
	integer(kind = 8), intent(in) :: p1(ND), p2(ND), p3(ND)

	!********

	double precision :: t

	integer :: it, n
	integer(kind = 8) :: p(ND)

	n = 2 * int(norm2(dble(p2 - p1)) + norm2(dble(p3 - p2)))
	do it = 0, n

		t = 1.d0 * it / n
		p = nint((1-t)**2 * p1 + 2*(1-t)*t * p2 + t**2 * p3)
		call draw_pixel(cv, color, p)

	end do

end subroutine draw_bezier2

!===============================================================================

subroutine draw_pixel(cv, color, p)

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color
	integer(kind = 8), intent(in) :: p(ND)

	! Check bounds.  TODO: set a flag to log *one* warning (not a warning
	! per-pixel)
	if (1 <= p(1) .and. p(1) <= size(cv,1) .and. &
	    1 <= p(2) .and. p(2) <= size(cv,2)) then

		cv(p(1), p(2)) = color

	end if

end subroutine draw_pixel

!===============================================================================

subroutine draw_line(cv, color, p1, p2)

	! Draw a line segment from point p1 to p2 on canvas cv

	integer(kind = 4), allocatable, intent(inout) :: cv(:,:)
	integer(kind = 4), intent(in) :: color
	integer(kind = 8), intent(in) :: p1(ND), p2(ND)

	!********

	integer :: it, n
	integer(kind = 8) :: p(ND)
	double precision :: t

	n = 2 * int(maxval(abs(p2 - p1)))
	do it = 0, n

		t = 1.d0 * it / n
		p = nint(p1 + t * (p2 - p1))
		call draw_pixel(cv, color, p)

	end do

end subroutine draw_line

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

function to_str(i) result(str)

	integer, intent(in) :: i
	character(len = :), allocatable :: str

	!********

	character(len = 64) :: buffer

	write(buffer, '(i0)') i
	str = trim(buffer)

end function to_str

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

end module cali_m

!===============================================================================

! TODO:
! - testing
!   * ppm round-trip done for simple rectangle images
!   * cover multiple fonts
! - fill-in contours instead of just outlines
! - parse cmap to get unicode (or ascii) to glyph index mapping
!   * read string to typeset from file
! - other config args?  json input for app only
!   * img size
!   * fg, bg colors
!   * font filename
!   * text filename (or directly as a string)
!   * resolution / font size
! - compound glyphs
! - typeset multiple letters and maybe multiple lines
!   * read advanceWidth from 'hmtx' table (and numOfLongHorMetrics from the
!     'hhea' table) to get delta x for each glyph
!   * more advanced kerning using the 'kern' table
! - anti-aliasing?  doubtful
! - vector output format, e.g. ps, pdf, or svg?

