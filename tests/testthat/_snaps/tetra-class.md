# deb_tetra works

    Code
      deb_tetra(1)
    Condition
      Error in `deb_tetra()`:
      ! `s`, `d`, and `f` are absent but must be supplied.
      x `l`, `s`, `d`, and `f` must all have values.
      i You may have forgotten a value or need to use 0.

---

    Code
      deb_tetra(1, 3, f = 2)
    Condition
      Error in `deb_tetra()`:
      ! `d` is absent but must be supplied.
      x `l`, `s`, `d`, and `f` must all have values.
      i You may have forgotten a value or need to use 0.

---

    Code
      deb_tetra("hello", 3, 4, 2)
    Condition
      Error in `deb_tetra()`:
      ! `l` must be a numeric vector.
      x You've supplied a <character> vector.

---

    Code
      deb_tetra(3, "hello", 4, 2)
    Condition
      Error in `deb_tetra()`:
      ! `s` must be a numeric vector.
      x You've supplied a <character> vector.

---

    Code
      deb_tetra(1:3, 1:2, 0, 1:5)
    Condition
      Error in `deb_tetra()`:
      ! `l`, `s`, and `d` must have compatible lengths.
      i Only values of length one are recycled.
      * Unit `l` has length 3.
      * Unit `s` has length 2.
      * Unit `d` has length 1.
      * Unit `f` has length 5.

---

    Code
      deb_tetra(1, 2, 3, 4, bases = c(20, 12))
    Condition
      Error in `deb_tetra()`:
      ! `bases` must be a numeric vector of length 3.
      x You've supplied a <numeric> vector of length 2.

# deb_tetra prints

    <deb_tetra[3]>
    [1] 1:4s:7d:2f 2:5s:8d:4f 3:6s:9d:6f
    # Bases: 20s 12d 4f

---

    <deb_tetra[3]>
    [1] 1:4s:7d:2f 2:5s:8d:4f <NA>      
    # Bases: 20s 12d 4f

