# vec_math has error message for unimplemented functions

    `prod.deb_lsd()` not implemented.

---

    `sin.deb_lsd()` not implemented.

---

    `prod.deb_tetra()` not implemented.

# sum and mean with deb_lsd work

    Code
      sum(lsd3, lsd_bases)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# sum and mean work with deb_decimal

    Code
      sum(dec_l, dec_l_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(dec_l, dec_tetra_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# sum and mean with deb_tetra work

    Code
      sum(tetra1, tetra_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: `..2` <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# sum works between deb-style vectors

    Code
      sum(lsd1, dec_l_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(lsd1, dec_tetra_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(lsd1, tetra_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(tetra_alt, dec_tetra)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(tetra_alt, dec_d)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(lsd1, dec_l_alt, dec_tetra_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      sum(tetra1, lsd_bases, dec_tetra_alt)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

