# new_decimal works

    `bases` must be a numeric vector of length 2 for tripartite values or 3 for tetrapartite values.
    x You've supplied a <integer> vector of length 4.

# deb_decimal works

    Code
      deb_decimal(x, unit = "hello")
    Condition
      Error in `deb_decimal()`:
      ! `unit` must be one of "l", "s", "d", or "f", not "hello".

---

    Code
      deb_decimal("hello")
    Condition
      Error in `deb_decimal()`:
      ! Can't convert `x` <character> to <double>.

---

    Code
      deb_decimal(c(1, Inf))
    Condition
      Error in `deb_decimal()`:
      ! `x` cannot contain infinite (Inf) values.

---

    Code
      deb_decimal(x, bases = c(20, 12, 4, 8))
    Condition
      Error in `deb_decimal()`:
      ! `bases` must be a numeric vector of length 2 for tripartite values or 3 for tetrapartite values.
      x You've supplied a <numeric> vector of length 4.

---

    Code
      deb_decimal(x, unit = "f")
    Condition
      Error in `deb_decimal()`:
      ! `bases` must be a numeric vector of length 3 to use 'f' unit.

# deb_decimal prints

    <deb_decimal[3]>
    [1] 1.125 2.500 3.300
    # Unit: libra
    # Bases: 20s 12d

---

    <deb_decimal[1]>
    [1] NA
    # Unit: libra
    # Bases: 20s 12d

---

    <deb_decimal[3]>
    [1] 1.125 2.500 3.300
    # Unit: libra
    # Bases: 20s 12d 4f

