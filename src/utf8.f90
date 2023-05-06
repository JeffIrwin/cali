
!===========================================================================

module utf8_m

	! This was mechanically translated from the C example on Rosetta code:
	!
	!     https://rosettacode.org/wiki/UTF-8_encode_and_decode#C

	implicit none

	character, parameter :: LINE_FEED = char(10)

	integer, parameter :: NUTF_T = 5

	! Char data will undergo bitwise and with this
	integer(kind = 2), parameter :: &
		utf_mask(NUTF_T) =        &
		[                &
			int(b'00111111',2), &
			int(b'01111111',2), &
			int(b'00011111',2), &
			int(b'00001111',2), &
			int(b'00000111',2)  &
		]

	! Start bytes of current char in utf-8 encoded character
	integer(kind = 2), parameter :: &
		utf_lead(NUTF_T) =        &
		[                &
			int(b'10000000',2), &
			int(b'00000000',2), &
			int(b'11000000',2), &
			int(b'11100000',2), &
			int(b'11110000',2)  &
		]

	! Beginning of codepoint range
	integer(kind = 4), parameter :: &
		utf_beg(NUTF_T) =        &
		[               &
			int(o'0'      ,4), &
			int(o'0000'   ), &
			int(o'0200'   ), &
			int(o'04000'  ), &
			int(o'0200000') &
		]

	! End of codepoint range
	integer(kind = 4), parameter :: &
		utf_end(NUTF_T) =         &
		[                &
			int(o'0'       ,4), &
			int(o'0177'    ,4), &
			int(o'03777'   ,4), &
			int(o'0177777' ,4), &
			int(o'04177777',4)  &
		]

	! The number of bits from the codepoint that fits in char
	integer, parameter :: &
		utf_bits(NUTF_T) = &
		[      &
			6, &
			7, &
			5, &
			4, &
			3  &
		]

contains

!===========================================================================

! A codepoint or cp is basically utf32

function codepoint_len(cp) result(len)
	integer(kind = 4), intent(in) :: cp
	integer :: len
	!********
	integer :: i
	len = 0
	do i = 1, NUTF_T
		if (cp >= utf_beg(i) .and. cp <= utf_end(i)) exit
		len = len + 1
	end do
	if (len > 4) then
		write(*,*) 'Error: out of bounds'
		call exit(-2)
	end if
end function codepoint_len

!********

function utf8_len(ch) result(len)
	character, intent(in) :: ch
	integer :: len
	!********
	integer :: i
	len = 0
	do i = 1, NUTF_T
		if (iand(iachar(ch), int(ieor(utf_mask(i), int(z'ff',2)),4)) == utf_lead(i)) then
			exit
		end if
		len = len + 1
	end do
	if (len > 4) then
		write(*,*) 'Error: malformed leading byte'
		call exit(-2)
	end if
end function utf8_len

!********

function to_utf8(cp) result(ret)
	integer(kind = 4), intent(in) :: cp
	character(len = :), allocatable :: ret
	!********
	integer :: i, bytes, shift
	bytes = codepoint_len(cp)
	allocate(character(len = bytes) :: ret)
	shift = utf_bits(1) * (bytes - 1)
	ret(1:1) =     achar(ior(iand(ishft(cp, -shift), int(utf_mask(bytes+1),4)), &
			int(utf_lead(bytes+1),4)))
	do i = 2, bytes
		shift = shift - utf_bits(1)
		ret(i:i) = achar(ior(iand(ishft(cp, -shift), int(utf_mask(1),4)), &
			int(utf_lead(1),4)))
	end do
end function to_utf8

!********

! TODO: rename to_utf32()?
function to_cp(chr) result(codep)
	character(len = *), intent(in) :: chr
	integer(kind = 4) :: codep
	!********
	integer :: i, bytes, shift

	bytes = utf8_len(chr(1:1))
	print * , 'bytes = ', bytes
	if (bytes == 0) then
		! TODO: parameterize -1 as bad codepoint.  Maybe 0 instead?
		codep = -1
		return
	end if
	shift = utf_bits(1) * (bytes - 1)
	codep = ishft(iand(iachar(chr(1:1)), utf_mask(bytes+1)), shift)

	do i = 2, bytes
		shift = shift - utf_bits(1)
		codep = ior(codep, ishft(iand(iachar(chr(i:i)), utf_mask(1)), shift))
	end do

end function to_cp

!********

function to_cp_vec(utf8_in) result(cpvec)
	! Convert utf8 string to codepoint array
	character(len = *), intent(in) :: utf8_in
	integer(kind = 4), allocatable :: cpvec(:)
	!********
	integer :: i, j
	integer(kind = 4) :: cp

	print *, 'starting to_cp_vec()'
	print *, 'len = ', len(utf8_in)
	print *, ''

	allocate(cpvec(len(utf8_in)))  ! worst-case size (all ASCII)

	!i = 0
	!j = 0
	!do while (i < len(utf8_in))
	!	! This could probably be optimized by incrementing i by utf8_len
	!	! instead of trying every single byte offset
	!	i = i + 1
	!	cp = to_cp(utf8_in(i:))
	!	if (cp /= -1) then
	!		j = j + 1
	!		cpvec(j) = cp
	!	end if
	!end do
	!cpvec = cpvec(1:j) ! trim

	i = 1
	j = 1
	do while (i <= len(utf8_in))
		! TODO: this could probably be optimized by incrementing i by utf8_len
		! instead of trying every single byte offset
		cp = to_cp(utf8_in(i:))
		cpvec(j) = cp

		! If we changed to_cp() to return len, this extra len call wouldn't be
		! required
		i = i + codepoint_len(cp)

		j = j + 1
	end do
	cpvec = cpvec(1: j-1) ! trim

end function to_cp_vec

!********

function to_utf8_str(cpvec) result(utf8_str)
	integer(kind = 4), intent(in) :: cpvec(:)
	character(len = :), allocatable :: utf8_str
	!********
	character(len = :), allocatable :: utf8
	integer :: i, j

	! Worst-case size (all 4-byte chars)
	allocate(character(len = 4 * size(cpvec)) :: utf8_str)
	!utf8_str = repeat(' ', 4 * size(cpvec))

	j = 1
	do i = 1, size(cpvec)
		utf8 = to_utf8(cpvec(i))
		utf8_str(j: j + len(utf8) - 1) = utf8
		j = j + len(utf8)
	end do
	utf8_str = utf8_str(1: j-1) ! trim

end function to_utf8_str

!********

end module utf8_m

!!===========================================================================
!
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

