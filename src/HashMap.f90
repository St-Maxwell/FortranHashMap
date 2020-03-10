! ref: https://github.com/jamesroutley/write-a-hash-table
module mod_hash_map
    use iso_fortran_env, only: i4 => int32, i8 => int64, &
                               r4 => real32, r8 => real64
    implicit none
    private
    public :: hash_map

    integer(kind=i4), parameter :: HT_PRIME_1 = 131 ! BKDRhash seed
    integer(kind=i4), parameter :: HT_PRIME_2 = 1313 ! BKDRhash seed
    integer(kind=i4), parameter :: LOAD_DOWN_LIMIT = 10 ! minimum load factor
    integer(kind=i4), parameter :: LOAD_UP_LIMIT = 70 ! ! maximum load factor

    type :: hash_item
        logical :: is_filled
        character(len=:), allocatable :: key_
        class(*), pointer :: value_
    end type

    type :: hash_map
        integer(kind=i4), private :: size_
        integer(kind=i4), private :: count_
        type(hash_item), dimension(:), allocatable, private :: items_
    contains
        procedure :: init => init_hash_map
        procedure, private :: add_item_i4
        procedure, private :: add_item_i8
        procedure, private :: add_item_r4
        procedure, private :: add_item_r8
        procedure, private :: add_item_chars
        procedure, private :: add_item_bool
        generic :: add => add_item_i4, add_item_i8, &
                          add_item_r4, add_item_r8, &
                          add_item_chars, add_item_bool
        procedure, private :: get_item_i4
        procedure, private :: get_item_i8
        procedure, private :: get_item_r4
        procedure, private :: get_item_r8
        procedure, private :: get_item_chars
        procedure, private :: get_item_bool
        generic :: get => get_item_i4, get_item_i8, &
                          get_item_r4, get_item_r8, &
                          get_item_chars, get_item_bool
        procedure :: remove => remove_item
        procedure :: free => delete_hash_map
        final :: destruct_hash_map
    end type

    interface new_hash_item
        module procedure :: new_hash_item_i4
        module procedure :: new_hash_item_i8
        module procedure :: new_hash_item_r4
        module procedure :: new_hash_item_r8
        module procedure :: new_hash_item_chars
        module procedure :: new_hash_item_bool
    end interface

contains
!===========================================================

    subroutine init_hash_map(this)
        class(hash_map), intent(inout) :: this
        integer :: i

        if (allocated(this%items_)) error stop "Attempt to initialize an initialized hash table"

        this%count_ = 0
        this%size_ = 31 ! initial size
        allocate (this%items_(this%size_))
        do i = 1, this%size_
            this%items_(i)%value_ => null()
        end do
        this%items_(:)%is_filled = .false.

    end subroutine init_hash_map

!===========================================================
! overload for new_hash_item interface
!===========================================================
    pure function new_hash_item_i4(key, val) result(item)
        character(len=*), intent(in) :: key
        integer(kind=i4), intent(in) :: val
        type(hash_item) :: item

        item%key_ = key
        allocate (item%value_, source=val)
        item%is_filled = .true.

    end function new_hash_item_i4

    pure function new_hash_item_i8(key, val) result(item)
        character(len=*), intent(in) :: key
        integer(kind=i8), intent(in) :: val
        type(hash_item) :: item

        item%key_ = key
        allocate (item%value_, source=val)
        item%is_filled = .true.

    end function new_hash_item_i8

    pure function new_hash_item_r4(key, val) result(item)
        character(len=*), intent(in) :: key
        real(kind=r4), intent(in) :: val
        type(hash_item) :: item

        item%key_ = key
        allocate (item%value_, source=val)
        item%is_filled = .true.

    end function new_hash_item_r4

    pure function new_hash_item_r8(key, val) result(item)
        character(len=*), intent(in) :: key
        real(kind=r8), intent(in) :: val
        type(hash_item) :: item

        item%key_ = key
        allocate (item%value_, source=val)
        item%is_filled = .true.

    end function new_hash_item_r8

    pure function new_hash_item_chars(key, val) result(item)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val
        type(hash_item) :: item

        item%key_ = key
        allocate (item%value_, source=val)
        item%is_filled = .true.

    end function new_hash_item_chars

    pure function new_hash_item_bool(key, val) result(item)
        character(len=*), intent(in) :: key
        logical, intent(in) :: val
        type(hash_item) :: item

        item%key_ = key
        allocate (item%value_, source=val)
        item%is_filled = .true.

    end function new_hash_item_bool

!===========================================================

    function hash(chars, num_backets, attempt)
        character(len=*), intent(in) :: chars
        integer(kind=i4), intent(in) :: num_backets
        integer(kind=i4), intent(in) :: attempt
        !===================================================
        integer(kind=i4) :: hash
        integer(kind=i4) :: hash_a
        integer(kind=i4) :: hash_b

        hash_a = generic_hash(chars, HT_PRIME_1, num_backets)
        ! num_backets-2 in case of hash_b returning value of num_backets-1,
        ! where double hash fails
        hash_b = generic_hash(chars, HT_PRIME_2, num_backets - 2)

        hash = mod((hash_a + attempt*(hash_b + 1)), num_backets) + 1

    end function hash


    pure function generic_hash(chars, prime, num_backets)
        !! BKDRhash for string
        character(len=*), intent(in) :: chars
        integer(kind=i4), intent(in) :: prime
        integer(kind=i4), intent(in) :: num_backets
        integer(kind=i4) :: generic_hash
        !===================================================
        integer(kind=i4), parameter :: MAX_NUM = huge(generic_hash)
        integer :: i, len_chars

        generic_hash = 0
        len_chars = len(chars)

        do i = 1, len_chars
            generic_hash = generic_hash*prime + iachar(chars(i:i))
        end do

        ! generic_hash may overflow integer upbound
        ! equivalent to hash & 0x7FFFFFFF
        generic_hash = iand(generic_hash, MAX_NUM)

        ! project to [0, num_backets-1]
        generic_hash = mod(generic_hash, num_backets)

    end function generic_hash

!===========================================================

    subroutine add_item_(this, new_item)
        class(hash_map), intent(inout) :: this
        type(hash_item), intent(in) :: new_item
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: load
        integer(kind=i4) :: i

        ! lood > maximum load factor, extend the size of hash table
        load = this%count_*100/this%size_
        if (load > LOAD_UP_LIMIT) call resize_hash_table(this, .true.)

        idx = hash(new_item%key_, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                ! the item is empty, save key-value here
                this%items_(idx) = new_item
                this%count_ = this%count_ + 1
                return
            end if

            if (this%items_(idx)%key_ == new_item%key_) then
                ! an item with the same key is already exist
                ! replace it
                deallocate (this%items_(idx)%value_)
                allocate (this%items_(idx)%value_, source=new_item%value_)
                return
            end if

            ! the item is occupied, double hash for next item
            idx = hash(new_item%key_, this%size_, i)
            i = i + 1
        end do

    end subroutine add_item_

!===========================================================
! overload for hash_table component function add_item
!===========================================================
    subroutine add_item_i4(this, key, val)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(kind=i4), intent(in) :: val
        !===================================================
        type(hash_item) :: new_item

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"

        new_item = new_hash_item(key, val)
        call add_item_(this, new_item)

    end subroutine add_item_i4

    subroutine add_item_i8(this, key, val)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(kind=i8), intent(in) :: val
        !===================================================
        type(hash_item) :: new_item

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"

        new_item = new_hash_item(key, val)
        call add_item_(this, new_item)

    end subroutine add_item_i8

    subroutine add_item_r4(this, key, val)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        real(kind=r4), intent(in) :: val
        !===================================================
        type(hash_item) :: new_item

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"

        new_item = new_hash_item(key, val)
        call add_item_(this, new_item)

    end subroutine add_item_r4

    subroutine add_item_r8(this, key, val)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        real(kind=r8), intent(in) :: val
        !===================================================
        type(hash_item) :: new_item

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"

        new_item = new_hash_item(key, val)
        call add_item_(this, new_item)

    end subroutine add_item_r8

    subroutine add_item_chars(this, key, val)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val
        !===================================================
        type(hash_item) :: new_item

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"

        new_item = new_hash_item(key, val)
        call add_item_(this, new_item)

    end subroutine add_item_chars

    subroutine add_item_bool(this, key, val)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        logical, intent(in) :: val
        !===================================================
        type(hash_item) :: new_item

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"

        new_item = new_hash_item(key, val)
        call add_item_(this, new_item)

    end subroutine add_item_bool

!===========================================================

!===========================================================
! overload for hash_table component function get_item
!===========================================================
    subroutine get_item_i4(this, key, val, default)
        class(hash_map), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(kind=i4), intent(inout) :: val
        integer(kind=i4), optional :: default
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"
        idx = hash(key, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                if (present(default)) then
                    ! key not found, use default value
                    val = default
                    return
                end if

                error stop "Key not found: '"//key//"'"
            end if

            if (this%items_(idx)%key_ == key) then
                select type (v=>this%items_(idx)%value_)
                type is (integer(kind=i4))
                    val = v
                    return
                class default
                    error stop "Type mismatch: '"//key//"'"
                end select
            end if

            idx = hash(key, this%size_, i)
            i = i + 1
        end do

    end subroutine get_item_i4

    subroutine get_item_i8(this, key, val, default)
        class(hash_map), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(kind=i8), intent(inout) :: val
        integer(kind=i8), optional :: default
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"
        idx = hash(key, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                if (present(default)) then
                    ! key not found, use default value
                    val = default
                    return
                end if

                error stop "Key not found: '"//key//"'"
            end if

            if (this%items_(idx)%key_ == key) then
                select type (v=>this%items_(idx)%value_)
                type is (integer(kind=i8))
                    val = v
                    return
                class default
                    error stop "Type mismatch: '"//key//"'"
                end select
            end if

            idx = hash(key, this%size_, i)
            i = i + 1
        end do

    end subroutine get_item_i8

    subroutine get_item_r4(this, key, val, default)
        class(hash_map), intent(in) :: this
        character(len=*), intent(in) :: key
        real(kind=r4), intent(inout) :: val
        real(kind=r4), optional :: default
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"
        idx = hash(key, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                if (present(default)) then
                    ! key not found, use default value
                    val = default
                    return
                end if

                error stop "Key not found: '"//key//"'"
            end if

            if (this%items_(idx)%key_ == key) then
                select type (v=>this%items_(idx)%value_)
                type is (real(kind=r4))
                    val = v
                    return
                class default
                    error stop "Type mismatch: '"//key//"'"
                end select
            end if

            idx = hash(key, this%size_, i)
            i = i + 1
        end do

    end subroutine get_item_r4

    subroutine get_item_r8(this, key, val, default)
        class(hash_map), intent(in) :: this
        character(len=*), intent(in) :: key
        real(kind=r8), intent(inout) :: val
        real(kind=r8), optional :: default
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"
        idx = hash(key, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                if (present(default)) then
                    ! key not found, use default value
                    val = default
                    return
                end if

                error stop "Key not found: '"//key//"'"
            end if

            if (this%items_(idx)%key_ == key) then
                select type (v=>this%items_(idx)%value_)
                type is (real(kind=r8))
                    val = v
                    return
                class default
                    error stop "Type mismatch: '"//key//"'"
                end select
            end if

            idx = hash(key, this%size_, i)
            i = i + 1
        end do

    end subroutine get_item_r8

    subroutine get_item_chars(this, key, val, default)
        class(hash_map), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(inout) :: val
        character(len=*), optional :: default
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"
        idx = hash(key, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                if (present(default)) then
                    ! key not found, use default value
                    val = default
                    return
                end if

                error stop "Key not found: '"//key//"'"
            end if

            if (this%items_(idx)%key_ == key) then
                select type (v=>this%items_(idx)%value_)
                type is (character(len=*))
                    val = v
                    return
                class default
                    error stop "Type mismatch: '"//key//"'"
                end select
            end if

            idx = hash(key, this%size_, i)
            i = i + 1
        end do

    end subroutine get_item_chars

    subroutine get_item_bool(this, key, val, default)
        class(hash_map), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(inout) :: val
        logical, optional :: default
        !===================================================
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '" // key // "'"
        idx = hash(key, this%size_, 0)

        i = 1
        do
            if (.not. this%items_(idx)%is_filled) then
                if (present(default)) then
                    ! key not found, use default value
                    val = default
                    return
                end if

                error stop "Key not found: '"//key//"'"
            end if

            if (this%items_(idx)%key_ == key) then
                select type (v=>this%items_(idx)%value_)
                type is (logical)
                    val = v
                    return
                class default
                    error stop "Type mismatch: '"//key//"'"
                end select
            end if

            idx = hash(key, this%size_, i)
            i = i + 1
        end do

    end subroutine get_item_bool

!===========================================================

    subroutine resize_hash_table(this, increase)
        !! increase = .true., extend the size of hash table
        !! increase = .false., reduce the size of hash table
        class(hash_map), intent(inout) :: this
        logical, intent(in) :: increase
        !===================================================
        type(hash_item), dimension(:), allocatable :: old_items
        integer(kind=i4) :: new_size
        character(len=:), allocatable :: key
        integer(kind=i4) :: i, j, idx

        if (increase) then
            new_size = next_prime(this%size_*2)
        else
            new_size = next_prime(this%size_/2)
        end if

        call move_alloc(this%items_, old_items)

        this%size_ = new_size
        allocate (this%items_(new_size))
        do i = 1, new_size
            this%items_(i)%value_ => null()
        end do
        this%items_(:)%is_filled = .false.

        ! re-hash current items
        do i = 1, size(old_items)

            if (.not. old_items(i)%is_filled) cycle

            ! get key of the i-th item
            key = old_items(i)%key_

            ! the index of i-th item in new table
            idx = hash(key, this%size_, 0)

            j = 1
            do
                if (.not. this%items_(idx)%is_filled) exit

                idx = hash(key, this%size_, j)
                j = j + 1

            end do

            this%items_(idx)%key_ = old_items(i)%key_
            allocate (this%items_(idx)%value_, source=old_items(i)%value_)
            this%items_(idx)%is_filled = .true.

            ! clean old_items(i), or memery leak may occur
            call delete_item(old_items(i))

        end do

    end subroutine resize_hash_table


    subroutine remove_item(this, key)
        class(hash_map), intent(inout) :: this
        character(len=*), intent(in) :: key
        !===================================================
        integer(kind=i4) :: load
        integer(kind=i4) :: idx
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        if (.not. key_is_valid(key)) error stop "Invalid key: '"// key // "'"

        load = this%count_*100/this%size_
        if (load < LOAD_DOWN_LIMIT) call resize_hash_table(this, .false.)

        idx = hash(key, this%size_, 0)

        i = 1
        do
            ! the inquired key not exist
            if (.not. this%items_(idx)%is_filled) error stop "Key not found: '"// key // "'"

            if (this%items_(idx)%key_ == key) then
                call delete_item(this%items_(idx))
                this%count_ = this%count_ - 1
                return
            end if

            idx = hash(key, this%size_, i)
            i = i + 1

        end do

    end subroutine remove_item

!===========================================================

    subroutine delete_item(item)
        !! manually delete a hash_item pointer
        type(hash_item), intent(inout) :: item
        integer :: ierr

        if (item%is_filled) then
            deallocate (item%key_, stat=ierr)
            deallocate (item%value_, stat=ierr)
            item%value_ => null()
            item%is_filled = .false.
        end if

    end subroutine delete_item

    subroutine delete_hash_map(this)
        !! manually delete all key-pair item
        class(hash_map), intent(inout) :: this
        integer(kind=i4) :: i

        if (.not. allocated(this%items_)) error stop "Hash table not initialized"
        do i = 1, this%size_
            call delete_item(this%items_(i))
        end do
        deallocate (this%items_)
        this%size_ = 0
        this%count_ = 0

    end subroutine delete_hash_map

!===========================================================

    subroutine destruct_hash_map(this)
        !! destructor of hash_table
        type(hash_map), intent(inout) :: this
        integer(kind=i4) :: i

        if (allocated(this%items_)) then
            do i = 1, this%size_
                call delete_item(this%items_(i))
            end do
            deallocate (this%items_)
            this%size_ = 0
            this%count_ = 0
        end if

    end subroutine destruct_hash_map

!===========================================================

    pure function is_prime(n)
        integer(kind=i4), intent(in) :: n
        logical :: is_prime
        integer(kind=i4) :: i
        integer(kind=i4) :: up_limit

        select case (n)
        case (:1) ! n <= 1, not defined
            is_prime = .false.
            return
        case (2:3) ! n = 2, 3, is prime
            is_prime = .true.
            return
        case default
            if (mod(n, 2) == 0) then ! even numbers are prime
                is_prime = .false.
                return
            end if

            up_limit = floor(sqrt(real(n)), kind=i4)
            do i = 3, up_limit, 2 ! check for odd numbers
                if (mod(n, i) == 0) then
                    is_prime = .false.
                    return
                end if
            end do
        end select

        is_prime = .true.

    end function is_prime


    pure function next_prime(n)
        integer(kind=i4), intent(in) :: n
        integer(kind=i4) :: next_prime

        next_prime = n
        do
            if (is_prime(next_prime)) exit
            next_prime = next_prime + 1
        end do

    end function next_prime


    pure function key_is_valid(chars)
        character(len=*), intent(in) :: chars
        logical :: key_is_valid
        character(len=1) :: char
        integer :: i

        do i = 1, len(chars)
            char = chars(i:i)

            select case (ichar(char))
            case (32, 48:57, 65:90, 95, 97:122)
                ! SPACE, 0-9, A-Z, _, a-z
                ! key_is_valid = .true.
            case default
                key_is_valid = .false.
                return
            end select

        end do

        key_is_valid = .true.

    end function key_is_valid

end module mod_hash_map
