
!===============================================================================

! These demos use Windows fonts which are not licensed for distribution, so I
! can't include them in tests that run on github

module demo_m

	use cali_m
	use utf_m

	implicit none

contains

!===============================================================================

subroutine foo_bar_baz

	implicit none

	double precision :: pixels_per_em

	integer :: line_height, left_margin
	integer(kind = 4) :: fg, fg2, bg
	integer(kind = 4), allocatable :: canvas(:,:)

	type(ttf_t)  :: ttf, ttfi

	ttf  = read_ttf('./fonts/computer-modern/cmunrm.ttf')  ! roman
	ttfi = read_ttf('./fonts/computer-modern/cmunti.ttf')  ! italic

	! foreground/background colors
	fg  = new_color(int(z'000000ff',8))
	fg2 = new_color(int(z'2a7fffff',8))
	bg  = new_color(int(z'e8e6cbff',8))

	canvas = new_canvas(2000, 1000, bg)

	pixels_per_em = 300.d0
	line_height   = nint(1.2 * pixels_per_em)
	left_margin   = 150

	call draw_str(canvas, fg , ttf , "Hello, world!", &
		left_margin, 1 * line_height, pixels_per_em)

	call draw_str(canvas, fg2, ttfi, "föø, бар, βαζ", &
		left_margin, 2 * line_height, pixels_per_em)

	call write_img(canvas, "doc/demo-program.ppm")
	call system("magick.exe doc/demo-program.ppm doc/demo-program.png")
	call delete_file("doc/demo-program.ppm")

end subroutine foo_bar_baz

!===============================================================================

end module demo_m

!===============================================================================

program main

	use demo_m
	implicit none

	write(*,*) repeat("=", 50)
	write(*,*) "starting cali demo suite ..."
	write(*,*)

	call foo_bar_baz()

	write(*,*) FG_BRIGHT_GREEN//"success!"//COLOR_RESET
	write(*,*) repeat("=", 50)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

