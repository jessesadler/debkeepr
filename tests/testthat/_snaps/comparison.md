# Equality works with deb_lsd

    Code
      lsd1 == lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# Equality works with deb_decimal

    Code
      dec1 == deb_decimal(1.1125, bases = c(50, 16))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      deb_decimal(1.1125, bases = c(20, 12, 4)) == deb_decimal(1.1125, bases = c(50,
        16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec1 == deb_decimal(1.1125, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# Equality works with deb_tetra

    Code
      tetra1 == tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: `..2` <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# Equality works with mixed types

    Code
      lsd1 == deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd1 == deb_decimal(1.1125, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# Comparison logical operators work

    Code
      lsd1 < deb_lsd(1, 2, 3, bases = c(50, 16))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      deb_decimal(1.1125) < deb_decimal(1.1125, bases = c(50, 16))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      tetra1 < deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: `..2` <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd1 < deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd1 < deb_decimal(1.1125, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      deb_decimal(267, "d") < deb_decimal(1.1125, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: `..1` <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: `..2` <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

