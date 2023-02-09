# vec_cast works for deb_lsd

    Code
      vec_cast(lsd_alt, deb_lsd())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Can't convert `lsd` <deb_lsd> to <integer>.

---

    Can't convert `factor("hello")` <factor<48b69>> to <deb_lsd>.

# vec_cast works for deb_decimal

    Code
      vec_cast(dec_alt, deb_decimal())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(dec_l, deb_decimal(bases = c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(deb_decimal(bases = c(50, 16, 8)), deb_decimal())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(dec, integer())
    Condition
      Error:
      ! Can't convert `dec` <deb_decimal> to <integer>.

---

    Code
      vec_cast(factor("hello"), deb_decimal())
    Condition
      Error:
      ! Can't convert `factor("hello")` <factor<48b69>> to <deb_decimal>.

# vec_cast works for deb_decimal with tetra bases

    Code
      vec_cast(deb_decimal(), deb_decimal(unit = "f"))
    Condition
      Error in `deb_decimal()`:
      ! `bases` must be a numeric vector of length 3 to use 'f' unit.

# vec_cast works for deb_tetra

    Code
      vec_cast(tetra_alt, deb_tetra())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Can't convert `tetra` <deb_tetra> to <integer>.

---

    Can't convert `factor("hello")` <factor<48b69>> to <deb_tetra>.

# vec_cast works with lists

    Code
      deb_as_list(deb_decimal(1:5))
    Condition
      Error in `deb_as_list()`:
      ! `x ` must be a <deb_lsd> or <deb_tetra> vector.
      x You've supplied a <deb_decimal>.

---

    Code
      vec_cast(c(y, 5), deb_lsd())
    Condition
      Error in `vec_cast.deb_lsd.list()`:
      ! `x` must be a list of numeric vectors of length 3.
      x You've supplied a list of <numeric> vectors of length(s) 3, 3, 3, 3, and 1.

---

    Code
      vec_cast(c(y, 5), deb_decimal())
    Condition
      Error in `vec_cast.deb_decimal.list()`:
      ! `x` must be a list of numeric vectors of length 3.
      x You've supplied a list of <numeric> vectors of length(s) 3, 3, 3, 3, and 1.

---

    Code
      vec_cast(c(tetra_list, 5), deb_tetra())
    Condition
      Error in `vec_cast.deb_tetra.list()`:
      ! `x` must be a list of numeric vectors of length 4.
      x You've supplied a list of <numeric> vectors of length(s) 4, 4, 4, 4, and 1.

# vec_cast works between deb_lsd and deb_decimal

    Code
      vec_cast(lsd_alt, deb_lsd())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(dec_alt, deb_decimal())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(dec_alt, deb_lsd())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(lsd_alt, deb_decimal())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(dec_s_tetra_alt, deb_lsd())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(lsd1, deb_decimal(bases = c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# vec_cast works between deb_lsd and deb_tetra

    Code
      vec_cast(tetra_alt, deb_tetra())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(tetra_alt, deb_lsd())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(deb_lsd(1, 2, 3, c(50, 16)), deb_tetra())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# vec_cast works between deb_decimal and deb_tetra

    Code
      vec_cast(dec_alt, deb_tetra())
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      vec_cast(dec_l_tetra, deb_tetra(bases = c(50, 16, 8)))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# deb_as_lsd works

    Code
      deb_as_lsd(factor("hello"))
    Condition
      Error in `deb_as_lsd()`:
      ! Can't convert `x` <factor<48b69>> to <deb_lsd>.

# deb_as_decimal works

    Code
      deb_as_decimal(factor("hello"))
    Condition
      Error in `deb_as_decimal()`:
      ! Can't convert `x` <factor<48b69>> to <deb_decimal>.

# deb_as_tetra works

    Code
      deb_as_tetra(factor("hello"))
    Condition
      Error in `deb_as_tetra()`:
      ! Can't convert `x` <factor<48b69>> to <deb_tetra>.

---

    Code
      deb_as_tetra(deb_lsd(1, 2, 3))
    Condition
      Error in `deb_as_tetra()`:
      ! `f` is absent but must be supplied.

---

    Code
      deb_as_tetra(deb_decimal(1.1125))
    Condition
      Error in `deb_as_tetra()`:
      ! `f` is absent but must be supplied.

# assignment subsetting works

    Code
      lsd[[1]] <- lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd[[1]] <- dec_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec[[1]] <- dec_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec[[1]] <- lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      tetra[[1]] <- tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd[[1]] <- tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      tetra_alt[[1]] <- lsd
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec[[1]] <- dec_tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec_tetra_alt[[1]] <- dec_l
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd[[1]] <- dec_tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec_tetra_alt[[1]] <- lsd1
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

