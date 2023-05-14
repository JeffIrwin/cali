
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
	!use utf8_m

	implicit none

	character(len = :), allocatable :: str

	double precision :: pix_per_em

	integer :: line_height, lmargin
	integer(kind = 4) :: fg, fg2, fg3, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:) ! canvas

	type(args_t) :: args
	type(ttf_t)  :: ttf, ttfi

	write(*,*) 'cali 0.0.1'
	write(*,*)

	args = parse_args()
	ttf  = read_ttf(args%ttf_file)
	!ttfi = read_ttf('./fonts/computer-modern/cmunti.ttf')  ! italic

	! foreground/background colors
	fg  = new_color(int(z'000000ff',8))
	fg2 = new_color(int(z'ffffffff',8))
	fg3 = new_color(int(z'2a7fffff',8))
	bg  = new_color(int(z'e8e6cbff',8))
	bg2 = fg3

	cv = new_canvas(1500, 600, bg)
	cv(:, 300:) = bg2

	pix_per_em = 140.d0
	line_height = nint(1.2 * pix_per_em)
	lmargin = 100

	!str = "a b!"
	!call draw_str(cv, fg , ttf, str, lmargin, 1 * line_height, pix_per_em)

	str = "The masculine urge"
	!str = "KΚαλλι"
	str = "Universitätsstraße"
	call draw_str(cv, fg , ttf, str, lmargin, 1 * line_height, pix_per_em)

	!str = "to typeset"
	str = "Привет"
	str = "Яблоко"
	str = "Hôtel français"
	call draw_str(cv, fg2, ttf, str, 300, 3 * line_height, pix_per_em)
	!call draw_str(cv, fg2, ttfi, str, 300, nint(2.5 * line_height), pix_per_em)

	call write_img(cv, 'test.ppm')

	write(*,*) FG_BRIGHT_GREEN//'Finished cali'//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

