
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

!program main
!
!	use cali_m
!
!	implicit none
!
!	double precision :: pixels_per_em
!
!	integer :: line_height, left_margin
!	integer(kind = 4) :: fg, fg2, bg
!	integer(kind = 4), allocatable :: canvas(:,:)
!
!	type(ttf_t)  :: ttf, ttfi
!
!	ttf  = read_ttf('./fonts/computer-modern/cmunrm.ttf')  ! roman
!	ttfi = read_ttf('./fonts/computer-modern/cmunti.ttf')  ! italic
!
!	! foreground/background colors
!	fg  = new_color(int(z'000000ff',8))
!	fg2 = new_color(int(z'2a7fffff',8))
!	bg  = new_color(int(z'e8e6cbff',8))
!
!	canvas = new_canvas(700, 400, bg)
!
!	pixels_per_em = 100.d0
!	line_height   = nint(1.2 * pixels_per_em)
!	left_margin   = 20
!
!	call draw_str(canvas, fg , ttf , "Hello, world!", &
!		left_margin, 1 * line_height, pixels_per_em)
!
!	call draw_str(canvas, fg2, ttfi, "föø, бар, βαζ", &
!		left_margin, 2 * line_height, pixels_per_em)
!
!	call write_img(canvas, 'my-file.ppm')
!
!end program main

!===============================================================================

program main

	use app_m
	use cali_m

	implicit none

	!character(len = :), allocatable :: str

	!double precision :: pix_per_em

	!integer :: line_height, lmargin
	!integer(kind = 4) :: fg, fg2, fg3, bg, bg2
	!integer(kind = 4), allocatable :: cv(:,:)

	type(args_t) :: args
	!type(ttf_t)  :: ttf, ttfi

	write(*,*) 'cali '// &
		to_str(CALI_MAJOR)//'.'// &
		to_str(CALI_MINOR)//'.'// &
		to_str(CALI_PATCH)
	write(*,*)

	args = parse_args()
	call specimen(args%ttf_file)

	!ttf  = read_ttf(args%ttf_file)
	!ttfi = read_ttf('./fonts/noto-sans/NotoSans-Italic.ttf')

	!! foreground/background colors
	!fg  = new_color(int(z'000000ff',8))
	!fg2 = new_color(int(z'ffffffff',8))
	!fg3 = new_color(int(z'2a7fffff',8))
	!bg  = new_color(int(z'e8e6cbff',8))
	!bg2 = fg3

	!cv = new_canvas(1500, 600, bg)
	!cv(:, 380:) = bg2

	!pix_per_em = 140.d0
	!line_height = nint(1.2 * pix_per_em)
	!lmargin = 100

	!str = "Universitätsstraße"
	!call draw_str(cv, fg, ttf, str, lmargin, 1 * line_height, pix_per_em)

	!str = "Вокзал ताज महल"
	!call draw_str(cv, fg, ttf, str, lmargin, 2 * line_height, pix_per_em)

	!str = "Καλλι"
	!str = "Hôtel français"
	!!str = "مرحبًا" ! calibri includes arabic
	!call draw_str(cv, fg2, ttfi, str, lmargin, 3 * line_height, pix_per_em)

	!call write_img(cv, 'test.ppm')

	write(*,*) FG_BRIGHT_GREEN//'Finished cali'//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

