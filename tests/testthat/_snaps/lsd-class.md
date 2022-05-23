# deb_lsd works

    Code
      deb_lsd(1)
    Condition
      Error in `deb_lsd()`:
      ! `s` and `d` are absent but must be supplied.
      x `l`, `s`, and `d` must all have values.
      i You may have forgotten a value or need to use 0.

---

    Code
      deb_lsd(1, d = 2)
    Condition
      Error in `deb_lsd()`:
      ! `s` is absent but must be supplied.
      x `l`, `s`, and `d` must all have values.
      i You may have forgotten a value or need to use 0.

---

    Code
      deb_lsd("hello", 3, 4)
    Condition
      Error in `deb_lsd()`:
      ! `l` must be a numeric vector.
      x You've supplied a <character> vector.

---

    Code
      deb_lsd(3, "hello", 4)
    Condition
      Error in `deb_lsd()`:
      ! `s` must be a numeric vector.
      x You've supplied a <character> vector.

---

    Code
      deb_lsd(1:3, 1:2, 0)
    Condition
      Error in `deb_lsd()`:
      ! `l`, `s`, and `d` must have compatible lengths
      i Only values of length one are recycled.
      * Unit `l` has length 3.
      * Unit `s` has length 2.
      * Unit `d` has length 1.

---

    Code
      deb_lsd(1, 2, 3, bases = c(20, 12, 4))
    Condition
      Error in `deb_lsd()`:
      ! `bases` must be a numeric vector of length 2.
      x You've supplied a <numeric> vector of length 3.

# deb_lsd prints

    <deb_lsd[3]>
    [1] 1:4s:7d 2:5s:8d 3:6s:9d
    # Bases: 20s 12d

---

    <deb_lsd[3]>
    [1] 1:4s:7d 2:5s:8d <NA>   
    # Bases: 20s 12d

