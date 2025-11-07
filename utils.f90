module utils
    implicit none
    private
    public :: string_to_array, is_numeric

contains
    function string_to_array(str, sep) result(arr)
        implicit none
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: sep
        character(len=:), allocatable :: arr(:)

        integer :: i, start, n, len_str
        len_str = len_trim(str)

        n = 1
        do i = 1, len_str
            if (str(i:i) == sep) n = n + 1
        end do
        allocate(character(len=len_str) :: arr(n))

        start = 1
        n = 0
        do i = 1, len_str
            if (str(i:i) == sep .or. i == len_str) then
                if (i == len_str) then
                    n = n + 1
                    arr(n) = trim(adjustl(str(start:i)))
                else
                    n = n + 1
                    arr(n) = trim(adjustl(str(start:i-1)))
                    start = i + 1
                end if
            end if
        end do

        arr = arr(1:n)
    end function string_to_array


    function is_numeric(str) result(bool)
        implicit none
        character(len=*), intent(in) :: str
        logical :: bool
        real :: real_value
        integer :: io_status

        bool = .false.

        read(str, *, iostat = io_status) real_value

        if (io_status == 0) then
            bool = .true.
        end if
    end function is_numeric

    ! function esplit(str) result(arr)
        
    ! end function esplit
end module utils