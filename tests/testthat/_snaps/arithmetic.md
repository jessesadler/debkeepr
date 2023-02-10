# Arithmetic operators work with two deb_lsd vectors

    Code
      lsd1 + lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_lsd> * <deb_lsd> is not permitted

# Arithmetic operators work with deb_lsd and numeric

    <deb_lsd> + <double> is not permitted

---

    <double> %% <deb_lsd> is not permitted

# Arithmetic operators work with two deb_decimal vectors

    Code
      deb_decimal(1.8375) + deb_decimal(1.5, bases = bases2)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_decimal> * <deb_decimal> is not permitted

# Arithmetic ops work with deb_decimal vectors with tetra bases

    Code
      dec_tetra_l + deb_decimal(1.5, bases = bases4)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_decimal> * <deb_decimal> is not permitted

# Arithmetic ops work with deb_decimal vectors with mixed bases

    Code
      dec_l_alt + dec_tetra_l
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec_tetra_l - dec_l_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# Arithmetic operators work with deb_decimal and numeric

    <double> %% <deb_decimal> is not permitted

# Arithmetic operators work with tetra deb_decimal and numeric

    <double> %% <deb_decimal> is not permitted

# Arithmetic operators work with two deb_tetra vectors

    Code
      tetra1 + tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_tetra> * <deb_tetra> is not permitted

# Arithmetic operators work with deb_tetra and numeric

    <deb_tetra> + <double> is not permitted

---

    <double> %% <deb_tetra> is not permitted

# Arithmetic operators work with deb_lsd and deb_decimal

    Code
      lsd_alt + dec
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec + lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      lsd + deb_decimal(1.5, bases = c(50, 16, 8))
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec_tetra + lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_lsd> * <deb_decimal> is not permitted

---

    <deb_decimal> * <deb_lsd> is not permitted

# Arithmetic operators work with deb_lsd and deb_tetra

    Code
      lsd_alt + tetra
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      tetra + lsd_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_lsd> * <deb_tetra> is not permitted

---

    <deb_tetra> * <deb_lsd> is not permitted

# Arithmetic operators work with deb_tetra and deb_decimal

    Code
      tetra_alt + dec
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      dec + tetra_alt
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20, d = 12, and f = 4.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50, d = 16, and f = 8.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      tetra_alt + deb_decimal(1.8375)
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
      x Cannot combine: <deb_tetra> vector with `bases` s = 50 and d = 16.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Code
      deb_decimal(1.5, bases = c(50, 16)) - x
    Condition
      Error:
      ! Incompatible `bases`.
      i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
      x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
      x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
      i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    <deb_tetra> * <deb_decimal> is not permitted

---

    <deb_decimal> * <deb_tetra> is not permitted

