
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
	use utf8_m

	implicit none

	character(len = :), allocatable :: utf8_str
	character(len = :), allocatable :: utf8_in
	integer(kind = 4), allocatable :: utf32_str(:)

	double precision :: pix_per_em

	integer :: i, height, width, line_height
	integer(kind = 4) :: fg, fg2, bg, bg2
	integer(kind = 4), allocatable :: cv(:,:) ! canvas
	integer(kind = 8) :: iglyph
	integer, allocatable :: iglyphs(:), kern(:)

	type(args_t) :: args
	type(ttf_t)  :: ttf

	write(*,*) 'cali 0.0.1'
	write(*,*)

	args = parse_args()
	ttf  = read_ttf(args%ttf_file)

	! foreground/background colors
	fg  = new_color(int(z'000000ff',8))
	fg2 = new_color(int(z'ffffffff',8))
	bg  = new_color(int(z'e8e6cbff',8))
	bg2 = new_color(int(z'2a7fffff',8))

	! Allocate canvas and set background color.  TODO: constructor
	width  = 1000
	height = 600
	allocate(cv(width, height))
	cv = bg
	cv(:, 300:) = bg2

	pix_per_em = 200.d0
	line_height = nint(1.2 * pix_per_em)

	! String to be typeset
	!utf8_str = "Καλλι "
	utf8_in = "Καλλι"
	!utf8_in = "привет"

	! TODO: refactor as draw_str() fn

	print *, 'utf8_in = ', utf8_in
	print *, 'len = ', len(utf8_in)
	utf32_str = to_cp_vec(utf8_in)
	print *, 'utf32_str = ', utf32_str
	print *, ''
	do i = 1, size(utf32_str)
		!print *, utf32_str(i)
		iglyph = get_index(utf32_str(i), ttf)
		call draw_glyph(cv, fg , ttf, ttf%glyphs(iglyph), &
			int(0.6*pix_per_em*i), 1 * line_height, pix_per_em)
	end do

	!utf8_str = "graph"
	!do i = 1, len(utf8_str)
	!	print *, utf8_str(i:i)
	!	iglyph = get_index(utf8_str(i:i), ttf)  ! TODO: ASCII only.  utf not indexed by bytes
	!	call draw_glyph(cv, fg , ttf, ttf%glyphs(iglyph), &
	!		int(0.6*pix_per_em*i), 2 * line_height, pix_per_em)
	!end do

	call write_img(cv, 'test.ppm')

	write(*,*) FG_BRIGHT_GREEN//'Finished cali'//COLOR_RESET
	write(*,*)
	call exit(EXIT_SUCCESS)

end program main

!===============================================================================

!program main
!
!	use utf8_m
!	implicit none
!
!	character(len = :), allocatable :: utf8_in, utf8
!	integer(kind = 4), allocatable :: cpvec(:)
!
!	integer :: i, j, iu
!
!	! If these characters don't render correctly, try using nvim-qt.exe directly
!	! in Windows (not WSL and not in terminal)
!	utf8_in = "AöЖ€𝄞"
!	utf8_in = "Καλλι"
!	!utf8_in = "😀🔥💀🥵✔⚒🙀🥇⛩⚫🕧"  ! emoji
!	!utf8_in = " 😀a🔥ab💀abc🥵abcd✔abcde⚒ 🙀 🥇 ⛩⚫ 🕧 "  ! emoji
!
!	!! Test cases from https://www.w3.org/2001/06/utf-8-test/UTF-8-demo.html
!	!utf8_in = "∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i), ∀x∈ℝ: ⌈x⌉ = −⌊−x⌋, α ∧ ¬β = ¬(¬α ∨ β)"
!	!utf8_in = "  ℕ ⊆ ℕ₀ ⊂ ℤ ⊂ ℚ ⊂ ℝ ⊂ ℂ, ⊥ < a ≠ b ≡ c ≤ d ≪ ⊤ ⇒ (A ⇔ B),"
!	!utf8_in = "  2H₂ + O₂ ⇌ 2H₂O, R = 4.7 kΩ, ⌀ 200 mm"
!	!utf8_in = "  ði ıntəˈnæʃənəl fəˈnɛtık əsoʊsiˈeıʃn"//LINE_FEED &
!	!          //"  Y [ˈʏpsilɔn], Yen [jɛn], Yoga [ˈjoːgɑ]"
!	!utf8_in = "  ((V⍳V)=⍳⍴V)/V←,V    ⌷←⍳→⍴∆∇⊃‾⍎⍕⌈"
!	!utf8_in = "‘single’ and “double” quotes"
!	!utf8_in = "• ‚deutsche‘ „Anführungszeichen“"
!	!utf8_in = "ASCII safety test: 1lI|, 0OD, 8B "
!
!	print *, 'utf8 in  = "'//utf8_in//'"'
!	!print *, 'len(utf8_in) = ', len(utf8_in)  ! length in bytes
!
!	cpvec = to_cp_vec(utf8_in)
!	print *, 'codepoint vector = '
!	print '(z0)', cpvec
!
!	utf8 = to_utf8_str(cpvec)
!	print *, 'utf8 out = "'//utf8//'"'
!
!	!print *, 'lens = ', len(utf8_in), len(utf8)
!
!	if (utf8_in == utf8) then
!		write(*,*) 'Success!'
!	else
!		write(*,*) 'Error: utf8 string was not transcoded correctly'
!		call exit(-1)
!	end if
!
!	!open(newunit = iu, file = utf8_in)
!	!write(iu, *) 'hello world, hallo welt'
!	!close(iu)
!
!end program main

!===========================================================================

