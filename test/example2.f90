! test time consuming
program main
    use mod_hash_map
    use iso_fortran_env, only: i4 => int32, i8 => int64, &
                               r4 => real32, r8 => real64
    implicit none
    type(hash_map) :: dict
    character(len=3), dimension(5000) :: keys
    character(len=1) :: char1, char2, char3
    integer(kind=i4) :: i, j, k, l
    integer(kind=i8) :: rate, tic, toc
    real(kind=r8) :: telaps

    l = 0
    do i = 1, 26
        char1 = achar(64+i)
        do j = 1, 26
            char2 = achar(64+j)
            do k = 1, 16
                char3 = achar(64+k)
                l = l + 1
                keys(l) = char1 // char2 // char3
                if (l == 5000) exit
            end do
        end do
    end do

    call dict%init()

    
    call system_clock(tic)
    do i = 1, 5000
        call dict%add(keys(i), i)
    end do
    call system_clock(count=toc, count_rate=rate)
    telaps = (toc - tic) / real(rate, r8)
    write(*,*) "Runtime: ", telaps, " s"


    call system_clock(tic)
    do i = 1, 5000
        call dict%get(keys(i), j)
    end do
    call system_clock(count=toc, count_rate=rate)
    telaps = (toc - tic) / real(rate, r8)
    write(*,*) "Runtime: ", telaps, " s"

end program main