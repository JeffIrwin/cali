
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
		REPEAT     = int(b'00001000'), &
		X_DELTA    = int(b'00010000')!,  &
		!Y_DELTA   = int(b'00100000')     ! 2 * X_DELTA

	character, parameter :: &
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
		integer(kind = 8), allocatable :: x(:,:), end_pts(:)
		integer(kind = 2), allocatable :: flags(:)
	end type glyph_t

	!********

	type ttf_t

		integer(kind = 8) :: scalar_type, ntables, search_range, &
			entry_select, range_shift, checksum_adj, magic_num, flags, &
			units_per_em, xmin, ymin, xmax, ymax, mac_style, low_rec_ppem, &
			font_dir_hint, index2loc_fmt, glyph_data_fmt, nglyphs

		! Table offsets
		integer(kind = 8) :: glyf_offset, loca_offset

		! Dates in ttf long date time format (i64 seconds since 1904)
		integer(kind = 8) :: created, modified

		double precision :: vers, font_rev, maxp_vers

		type(glyph_t), allocatable :: glyphs(:)
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
	integer(kind = 8) :: i, head, maxp, sum, filesize

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

	sum = 0
	do i = 1, filesize / 4
	!do i = 1, (filesize + 3) / 4
		sum = sum + read_u32(iu)
	end do
	sum = iand(sum, int(z'ffffffff',8))

	if (sum /= int(z'b1b0afba', 8)) then
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

	write(*, '(a,i0)') ' Number of tables = ', ttf%ntables

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

	print * ,'ttf%vers     = ', ttf%vers
	print * ,'ttf%font_rev = ', ttf%font_rev

	ttf%checksum_adj = read_u32(iu)
	ttf%magic_num    = read_u32(iu)
	!print '(a,z0)', ' magic_num = ', ttf%magic_num

	if (ttf%magic_num /= int(z'5f0f3cf5')) then
		write(*,*) ERROR//'bad magic number'
		call exit(EXIT_FAILURE)
	end if

	ttf%flags        = read_u16(iu)
	ttf%units_per_em = read_u16(iu)

	!print *, 'units_per_em = ', ttf%units_per_em

	ttf%created  = read_i64(iu)
	ttf%modified = read_i64(iu)
	print *, 'created   = ', ldt2str(ttf%created)
	print *, 'modified  = ', ldt2str(ttf%modified)

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
	print *, 'nglyphs        = ', ttf%nglyphs

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
	!print *, 'done read_ttf()'
	!print *, ''

end function read_ttf

!===============================================================================

function read_glyph(iu, ttf, iglyph) result(glyph)

	! Read glyph index iglyph from file unit iu in the ttf struct

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

		if (iand(flag, REPEAT) /= 0) then
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

subroutine draw_glyph(glyph, x0)

	! Print scilab source code for plotting

	type(glyph_t), intent(in) :: glyph

	integer, intent(in) :: x0  ! translation.  TODO: y translation and scale

	!********

	double precision :: t

	integer(kind = 2) :: flag, flagn, flagp
	integer(kind = 8) :: i, j, start_pt, jp, jn, a(ND), b(ND), c(ND), it
	integer(kind = 8), allocatable :: x(:,:)
	integer, parameter :: NSPLINE = 10

	if (glyph%ncontours < 0) then
		write(*,*) ERROR//'compound glyphs are not supported'
		call exit(EXIT_FAILURE)
	end if

	!print *, 'npts      = ', glyph%npts
	!print *, 'ncontours = ', glyph%ncontours
	!print *, 'end_pts   = ', glyph%end_pts
	!print *, 'x = '
	!print '(2i6)', glyph%x

	x = glyph%x
	do i = 1, glyph%npts
		x(1,i) = x(1,i) + x0
	end do

	start_pt = 1
	do i = 1, glyph%ncontours

		do j = start_pt, glyph%end_pts(i) + 1
			!print '(2i8)', x(:,j)
			!print *, 'j, x = ', j, x(:,j)

			! Next point, which might loop back to beginning of contour
			if (j == glyph%end_pts(i) + 1) then
				jn = start_pt
			else
				jn = j + 1
			end if

			flag  = glyph%flags(j )
			flagn = glyph%flags(jn)

			if (iand(flag , ON_CURVE) /= 0 .and. &
			    iand(flagn, ON_CURVE) /= 0) then

				! Draw a straight line segment from j to jn
				print '(a,i0,a,i0,a,i0,a,i0,a)', 'plot([', &
					x(1,j), ', ', x(1,jn) , '], [', x(2,j), ', ', x(2,jn) , '])'

			else if (iand(flag , ON_CURVE) == 0) then

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

				! Current point j is off-curve

				! Previous point
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

				print *, 'x = ['
				do it = 0, NSPLINE
					t = 1.d0 * it / NSPLINE

					print *, (1-t)**2 * a + 2*(1-t)*t * b + t**2 * c

				end do
				print *, ']'
				print '(a)', 'plot(x(:,1), x(:,2))'

			end if
		end do

		start_pt = glyph%end_pts(i) + 2
	end do

end subroutine draw_glyph

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

	integer(kind = 8) :: i, old, sum

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

	sum = 0

	! Number of 4-byte words is ceiling(num_bytes / 4)
	do i = 1, (table%length + 3) / 4
		sum = sum + read_u32(iu)
	end do
	!print '(a,z0)', '  sum   = ', sum

	sum = iand(sum, int(z'ffffffff',8))
	!print '(a,z0)', ' final  sum   = ', sum
	!print '(a,z0)', ' final csum   = ', table%checksum

	if (sum /= table%checksum) then
		write(*,*) ERROR//'bad checksum for table ', table%tag
		call exit(EXIT_FAILURE)
	end if

	call fseek(iu, old, SEEK_ABS)

end function read_ttf_table

!===============================================================================

end module cali_m

!===============================================================================

! TODO:
! - visualize in Fortran, not scilab.  export ppm file
!   * support colors
!   * draw rectangles too for background (or highlighting)
! - testing
!   * cover multiple fonts
! - fill-in contours instead of just outlines
! - parse cmap to get unicode (or ascii) to glyph index mapping
! - compound glyphs
! - typeset multiple letters and maybe multiple lines
!   * proper kerning
! - anti-aliasing?  doubtful
! - vector output format, e.g. ps, pdf, or svg?

