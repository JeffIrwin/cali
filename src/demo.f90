
!===============================================================================

! These demos use Windows fonts which are not licensed for distribution, so I
! can't include them in tests that run on github, or things with
! high-resolutions that make large files

module demo_m

	use cali_m
	use utf_m

	implicit none

contains

!===============================================================================

subroutine foo_bar_baz()

	implicit none

	double precision :: pixels_per_em

	integer :: line_height, left_margin
	integer(kind = 4) :: fg, fg2, bg
	integer(kind = 4), allocatable :: canvas(:,:)

	type(ttf_t)  :: ttf, ttfi

	ttf  = read_ttf("./fonts/computer-modern/cmunrm.ttf")  ! roman
	ttfi = read_ttf("./fonts/computer-modern/cmunti.ttf")  ! italic

	! foreground/background colors
	fg  = new_color(int(z"000000ff",8))
	fg2 = new_color(int(z"2a7fffff",8))
	bg  = new_color(int(z"e8e6cbff",8))

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

subroutine kalli()

	implicit none

	double precision :: pixels_per_em

	integer(kind = 4) :: fg, bg
	integer(kind = 4), allocatable :: canvas(:,:)

	type(ttf_t)  :: ttf

	ttf  = read_ttf("./fonts/computer-modern/cmunrm.ttf")  ! roman

	! foreground/background colors
	fg  = new_color(int(z"66ddaaff",8))
	bg  = new_color(int(z"202020ff",8))

	canvas = new_canvas(5322, 1746, bg)

	pixels_per_em = 2000.d0

	call draw_str(canvas, fg , ttf , "Καλλι", 100, 1600, pixels_per_em)

	call write_img(canvas, "doc/kalli.ppm")
	call system("magick.exe doc/kalli.ppm doc/kalli.png")
	call delete_file("doc/kalli.ppm")

end subroutine kalli

!===============================================================================

subroutine weis()

	implicit none

	double precision :: pixels_per_em

	integer(kind = 4) :: fg, bg
	integer(kind = 4), allocatable :: canvas(:,:)

	type(ttf_t)  :: ttf

	ttf  = read_ttf("/mnt/c/Windows/Fonts/COOPBL.TTF")

	! foreground/background colors
	fg  = new_color(int(z"ffffffff",8))
	bg  = new_color(int(z"ed1b24ff",8))

	canvas = new_canvas(800, 250, bg)

	pixels_per_em = 300.d0

	call draw_str(canvas, fg , ttf , "weis", 40, 225, pixels_per_em)

	call write_img(canvas, "doc/weis.ppm")
	call system("magick.exe doc/weis.ppm doc/weis.png")
	call delete_file("doc/weis.ppm")

end subroutine weis

!===============================================================================

subroutine pet_sounds()

	implicit none

	double precision :: pixels_per_em

	integer(kind = 4) :: fg, fg2, bg, f, tmargin
	integer(kind = 4), allocatable :: canvas(:,:)

	type(ttf_t)  :: ttf

	ttf  = read_ttf("/mnt/c/Windows/Fonts/COOPBL.TTF")

	! foreground/background colors
	fg  = new_color(int(z"fed84aff",8))
	fg2 = new_color(int(z"ffffffff",8))
	bg  = new_color(int(z"3d8351ff",8))

	! Scaling factor
	f = 3

	tmargin = 0
	tmargin = -25*f

	canvas = new_canvas(550*f, 420*f, bg)

	pixels_per_em = 120.d0 * f

	! TODO: the album cover uses a tighter tracking, but I don't have an option
	! to adjust that (yet)

	call draw_str(canvas, fg , ttf , "The"   , 130*f, 150*f + tmargin, pixels_per_em)
	call draw_str(canvas, fg , ttf , "Beach" ,  72*f, 232*f + tmargin, pixels_per_em)
	call draw_str(canvas, fg , ttf , "Boys"  ,  16*f, 315*f + tmargin, pixels_per_em)

	! TODO: remove pad to adjust for looser tracking
	call draw_str(canvas, fg2, ttf , "Pet"   , 291*f + 35*f, 315*f + tmargin, pixels_per_em)

	call draw_str(canvas, fg2, ttf , "Sounds",  59*f       , 402*f + tmargin, pixels_per_em)

	call write_img(canvas, "doc/pet-sounds.ppm")
	call system("magick.exe doc/pet-sounds.ppm doc/pet-sounds.png")
	call delete_file("doc/pet-sounds.ppm")

end subroutine pet_sounds

!===============================================================================

end module demo_m

!===============================================================================

program main

	use demo_m
	implicit none

	write(*,*) repeat("=", 50)
	write(*,*) "starting cali demo suite ..."
	write(*,*)

	call pet_sounds()
	call weis()
	call foo_bar_baz()
	call kalli()

	write(*,*) FG_BRIGHT_GREEN//"success!"//COLOR_RESET
	write(*,*) repeat("=", 50)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

