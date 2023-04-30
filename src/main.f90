
!===============================================================================

module app_m

	use cali_m
	implicit none

	type args_t
		character(len = :), allocatable :: ttf_file
	end type args_t

contains

function parse_args() result(args)

	type(args_t) :: args

	!********

	character(len = 1024) :: buffer
	character(len = :), allocatable :: argv

	integer :: argc, io

	argc = command_argument_count()
	!print *, 'argc = ', argc
	if (argc /= 1) then
		write(*,*) error//'bad cmd args'
		write(*,*) 'Usage:'
		write(*,*) '	cali path/to/font.ttf'
		call exit(EXIT_FAILURE)
	end if

	call get_command_argument(1, buffer, status = io)
	if (io /= EXIT_SUCCESS) then
		write(*,*) error//'cannot get cmd arg'
		call exit(EXIT_FAILURE)
	end if
	argv = trim(buffer)
	!print *, 'argv = ', argv

	! Positional arg
	args%ttf_file = argv

end function parse_args

end module app_m

!===============================================================================

program main

	use app_m
	use cali_m

	implicit none

	integer :: i, height, width
	integer(kind = 4), allocatable :: cv(:,:) ! canvas
	integer, allocatable :: iglyphs(:), kern(:)

	type(args_t) :: args
	type(ttf_t)  :: ttf

	write(*,*) 'cali 0.0.1'
	write(*,*)

	args = parse_args()
	ttf  = read_ttf(args%ttf_file)

	iglyphs = [83, 82, 74]            ! pog
	iglyphs = [51, 82, 74]            ! Pog
	iglyphs = [90, 72, 76, 86]        ! weis (try cooper black font)
	iglyphs = [(i, i = 68, 68+26-1)]  ! [a-z]
	iglyphs = [(i, i = 36, 36+26-1)]  ! [A-Z]
	iglyphs = [(i, i = 345, 369)]     ! \alpha - \omega
	iglyphs = [(i, i = 314, 337)]     ! \Alpha - \Omega

	iglyphs = [323, 345, 355, 355, 353] ! Καλλι
	kern    = [ 10, 160, 270, 375, 470]

	! TODO: autoset kern if not explicitly set, at least until I can parse
	! advanceWidth

	! Allocate canvas and set background color
	width  = 700
	height = 250
	allocate(cv(width, height))
	cv = new_color(int(z'202020ff',8))

	do i = 1, size(iglyphs)
		!call draw_glyph(cv, ttf%glyphs( iglyphs(i) ), 140 * i)
		call draw_glyph(cv, ttf%glyphs( iglyphs(i) ), kern(i) )
	end do

	call write_img(cv, 'test.ppm')

	write(*,*) FG_BRIGHT_GREEN//'Finished cali'//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

