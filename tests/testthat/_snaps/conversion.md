# Bases conversion works with deb_lsd vectors

    `x` must be a <deb_lsd>, <deb_tetra>, or <deb_decimal> vector.
    x You've supplied a <character> vector.

---

    Code
      deb_convert_bases(lsd1, c(20, 12, 4))
    Condition
      Error in `deb_convert_bases()`:
      ! `to` must be a numeric vector of length 2.
      x You've supplied a <numeric> vector of length 3.

---

    Code
      deb_convert_bases(lsd1, to = c(NA, 20))
    Condition
      Error in `deb_convert_bases()`:
      ! Values in `to` cannot be missing (`NA`).
      i `to` must be natural numbers greater than zero.

---

    Code
      deb_convert_bases(lsd1, to = c(-10, -20))
    Condition
      Error in `deb_convert_bases()`:
      ! `to` must be natural numbers greater than zero.
      x -10 and -20 are not natural numbers.

# Bases conversion works with deb_tetra vectors

    Code
      deb_convert_bases(tetra1, c(40, 24))
    Condition
      Error in `deb_convert_bases()`:
      ! `to` must be a numeric vector of length 3.
      x You've supplied a <numeric> vector of length 2.

---

    Code
      deb_convert_bases(tetra1, to = c(NA, 20, 12))
    Condition
      Error in `deb_convert_bases()`:
      ! Values in `to` cannot be missing (`NA`).
      i `to` must be natural numbers greater than zero.

---

    Code
      deb_convert_bases(tetra1, to = c(-10, -20, -12))
    Condition
      Error in `deb_convert_bases()`:
      ! `to` must be natural numbers greater than zero.
      x -10, -20, and -12 are not natural numbers.

# Unit conversion works

    `x` must be a <deb_decimal> vector.
    x You've supplied a <deb_lsd> vector.

---

    `to` must be one of "l", "s", "d", or "f", not "hello".

---

    `x` must be a <deb_decimal> vector with tetrapartite bases to convert to the 'f' unit.
    i Choose to = 'l', 's', or 'd' instead.

