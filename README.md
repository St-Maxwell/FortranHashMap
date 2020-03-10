# FortranHashMap
 
A Fortran implementation of hash map, which stores dictionary-like key-value pairs. 

## Features:
* auto-resizing
* various types of value, including:
   * `integer(kind=4)`
   * `integer(kind=8)`
   * `real(kind=4)`
   * `real(kind=8)`
   * `character(len=*)`
   * `logical`

## Usage
### Intialize
You can't initialize an initialized hash map, unless it has been deleted.
```fortran
use mod_hash_map
type(hash_map) :: dict
call dict%init()
```


### Add key-value pair
`call hash_map_object%add(key, val)`

* `key`: character type, valid characters: `'A-Z''a-z''_'' '`
* `value`: valid types described [above](##Feature)

If key is new to hash map, a new hash item is created. If key is already in hash map, the value is updated.

```fortran
call dict%add("fortran", 1)
call dict%add("pi", 3.1415926)
call dict%add("message", "Just Monika")
call dict%add("isGood", .true.)
```

### Get value
`call hash_map_object%get(key, val [, default])`

* If key is non-existent and default is not present, error stop occurs.
* If key is non-existent and default is present, val is equal to default.
* If the type of value stored in hash map doesn't match the type of actual argument val, error stop occurs.

```fortran
integer :: i
call dict%get("fortran", i)
call dict%get("lua", i, default=0)

character(len=:), allocatable :: msg
call dict%get("message", msg)
```

### Remove key-value pair
`call hash_map_object%remove(key)`

* If key is non-existent , error stop occurs.

```fortran
call dict%remove("pi")
```

### Delete a hash map
You can't delete an uninitialized hash map.

In some cases, you have no need to manually delete a hash map. When a hash map is out of its scope, a destructor is called automatically.
```fortran
call dict%free()
```

## Note
The code used Fortran 2018 error stop feature which allows variable character. e.g.

```fortran
error stop "Invalid key: '"// key // "'"
```

Your compiler may not support this feature. To get rid of compile errors, you should replace the codes with whatever you like. e.g.

```fortran
error stop "Invalid key"
stop "Invalid key"
stop
stat = 1; return ! return an error code
```
