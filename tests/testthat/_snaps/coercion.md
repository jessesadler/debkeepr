# deb_lsd coerces to itself

    Code
      vec_c(lsd, lsd_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      c(lsd, lsd_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# incompatible types do not work

    Can't combine `..1` <deb_lsd> and `..2` <character>.

---

    Can't combine `..1` <deb_lsd> and `..2` <logical>.

---

    Can't combine `..1` <deb_decimal> and `..2` <character>.

---

    Can't combine `..1` <deb_decimal> and `..2` <logical>.

---

    Can't combine `..1` <deb_tetra> and `..2` <character>.

---

    Can't combine `..1` <deb_tetra> and `..2` <logical>.

# deb_decimal coerces to itself

    Code
      vec_c(dec, deb_decimal(4.5, "l", c(50, 16)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      c(dec, deb_decimal(4.5, "l", c(50, 16)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_c(deb_decimal(1.5, "l", c(50, 16, 8)), deb_decimal(1.5, "l", bases3))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 50, d = 16, and f = 8.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      c(dec, deb_decimal(1.5, "l", c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      c(deb_decimal(1.5, "l", c(50, 16, 8)), dec)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# deb_tetra coerces to itself

    Code
      vec_c(tetra, deb_tetra(1, 2, 3, 4, c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: `..2` <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      c(tetra, deb_tetra(1, 2, 3, 4, c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: `..2` <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# deb_decimal coerces to deb_lsd

    Code
      vec_c(lsd, deb_decimal(5.5, "l", c(50, 16)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_c(dec, lsd_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_c(lsd, deb_decimal(1.5, "l", c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# deb_tetra coerces to deb_lsd

    Code
      vec_c(lsd, deb_tetra(1, 2, 3, 4, c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_c(tetra, deb_lsd(1, 2, 3, c(50, 16)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# deb_decimal coerces to deb_tetra

    Code
      vec_c(tetra, deb_decimal(1.5, "l", c(50, 16)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_c(tetra, deb_decimal(1.5, bases = c(20, 12, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20, d = 12, and f = 8.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

