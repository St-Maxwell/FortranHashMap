program main
    use mod_hash_map
    use iso_fortran_env, only: i4 => int32, i8 => int64, &
                               r4 => real32, r8 => real64
    implicit none
    type(hash_map) :: dict
    integer(kind=i4) :: i1
    integer(kind=i8) :: i2
    real(kind=r4) :: r1
    real(kind=r8) :: r2
    character(len=:), allocatable :: chars
    logical :: l

    call dict%init()

    call dict%add("a", 1_i4)
    call dict%add("b", 2_i8)
    call dict%add("c", 1._r4)
    call dict%add("d", 2._r8)
    call dict%add("e", "hello")
    call dict%add("f", .true.)

    call dict%get("a", i1)
    write(*,"(g0)") i1
    call dict%get("b", i2)
    write(*,"(g0)") i2
    call dict%get("c", r1)
    write(*,"(g0)") r1
    call dict%get("d", r2)
    write(*,"(g0)") r2
    call dict%get("e", chars)
    write(*,"(g0)") chars
    call dict%get("f", l)
    write(*,"(g0)") l

    call dict%remove("a")
    call dict%remove("b")
    call dict%get("a", i1, 10_i4)
    write(*,"(g0)") i1
    call dict%get("b", i2, 0_i8)
    write(*,"(g0)") i2

end program main