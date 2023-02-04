# lsd: non-numeric or Inf is an error

    `l` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `s` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `d` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `d` cannot contain infinite (Inf) values.

# lsd: length of l, s, and d all have values or are all length 0

    `s` and `d` are absent but must be supplied.
    x `l`, `s`, and `d` must all have values.
    i You may have forgotten a value or need to use 0.

---

    `d` is absent but must be supplied.
    x `l`, `s`, and `d` must all have values.
    i You may have forgotten a value or need to use 0.

---

    `d` is absent but must be supplied.
    x `l`, `s`, and `d` must all have values.
    i You may have forgotten a value or need to use 0.

# lsd: length of l, s, and d are same length, length 1, or length 0

    `l`, `s`, and `d` must have compatible lengths
    i Only values of length one are recycled.
    * Unit `l` has length 3.
    * Unit `s` has length 2.
    * Unit `d` has length 1.

# tetra: non-numeric or Inf is an error

    `l` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `s` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `d` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `f` must be a numeric vector.
    x You've supplied a <character> vector.

---

    `f` cannot contain infinite (Inf) values.

# tetra: length of units all have values or are all length 0

    `s`, `d`, and `f` are absent but must be supplied.
    x `l`, `s`, `d`, and `f` must all have values.
    i You may have forgotten a value or need to use 0.

---

    `d` and `f` are absent but must be supplied.
    x `l`, `s`, `d`, and `f` must all have values.
    i You may have forgotten a value or need to use 0.

---

    `f` is absent but must be supplied.
    x `l`, `s`, `d`, and `f` must all have values.
    i You may have forgotten a value or need to use 0.

---

    `f` is absent but must be supplied.
    x `l`, `s`, `d`, and `f` must all have values.
    i You may have forgotten a value or need to use 0.

# tetra: length of units are same length, length 1, or length 0

    `l`, `s`, and `d` must have compatible lengths.
    i Only values of length one are recycled.
    * Unit `l` has length 3.
    * Unit `s` has length 2.
    * Unit `d` has length 1.
    * Unit `f` has length 4.

# lsd: bases is numeric vector of length 2

    `bases` must be a numeric vector of length 2.
    x You've supplied a <logical> vector of length 1.

---

    `bases` must be a numeric vector of length 2.
    x You've supplied a <character> vector of length 2.

---

    `bases` must be a numeric vector of length 2.
    x You've supplied a <numeric> vector of length 1.

---

    `bases` must be a numeric vector of length 2.
    x You've supplied a <numeric> vector of length 3.

# lsd: bases does not have any missing values

    Values in `bases` cannot be missing (`NA`).
    i `bases` must be natural numbers greater than zero.

---

    Values in `bases` cannot be missing (`NA`).
    i `bases` must be natural numbers greater than zero.

# lsd: bases are natural numbers

    `bases` must be natural numbers greater than zero.
    x -12 and -3 are not natural numbers.

---

    `bases` must be natural numbers greater than zero.
    x 0 is not a natural number.

---

    `bases` must be natural numbers greater than zero.
    x 20.5 and 8.23 are not natural numbers.

# tetra: bases is numeric vector of length 2

    `bases` must be a numeric vector of length 3.
    x You've supplied a <logical> vector of length 1.

---

    `bases` must be a numeric vector of length 3.
    x You've supplied a <character> vector of length 2.

---

    `bases` must be a numeric vector of length 3.
    x You've supplied a <numeric> vector of length 1.

---

    `bases` must be a numeric vector of length 3.
    x You've supplied a <numeric> vector of length 2.

---

    `bases` must be a numeric vector of length 3.
    x You've supplied a <numeric> vector of length 4.

# tetra: bases does not have any missing values

    Values in `bases` cannot be missing (`NA`).
    i `bases` must be natural numbers greater than zero.

---

    Values in `bases` cannot be missing (`NA`).
    i `bases` must be natural numbers greater than zero.

# tetra: bases are natural numbers

    `bases` must be natural numbers greater than zero.
    x -12, -3, and -6 are not natural numbers.

---

    `bases` must be natural numbers greater than zero.
    x 0 is not a natural number.

---

    `bases` must be natural numbers greater than zero.
    x 20.5, 8.23, and 8.75 are not natural numbers.

# decimal bases checks

    `bases` must be a numeric vector of length 2 for tripartite values or 3 for tetrapartite values.
    x You've supplied a <numeric> vector of length 1.

---

    Values in `bases` cannot be missing (`NA`).
    i `bases` must be natural numbers greater than zero.

---

    `bases` must be natural numbers greater than zero.
    x -12 and -3 are not natural numbers.

# f unit bases check

    `f` must be a numeric vector of length 1.
    x You've supplied a <character> vector of length 1.

---

    `f` must be a numeric vector of length 1.
    x You've supplied a <numeric> vector of length 3.

---

    `f` cannot be `NA`.
    i `f` must be natural numbers greater than zero.

---

    `f` must be a natural number greater than zero.
    x -12 is not a natural number.

# bases equal tests equivalency

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
    x Cannot combine: <deb_lsd> vector with `bases` s = 60 and d = 12.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 12.
    x Cannot combine: <deb_lsd> vector with `bases` s = 20 and d = 16.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
    x Cannot combine: <deb_decimal> vector with `bases` s = 60 and d = 12.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
    x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 16.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
    x Cannot combine: <deb_tetra> vector with `bases` s = 60, d = 12, and f = 4.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
    x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 16, and f = 4.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` must be compatible to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 4.
    x Cannot combine: <deb_tetra> vector with `bases` s = 20, d = 12, and f = 8.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# mixed bases equality

    Incompatible `bases`.
    i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_tetra> vector with `bases` s = 20 and d = 12.
    x Cannot combine: <deb_lsd> vector with `bases` s = 50 and d = 16.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

---

    Incompatible `bases`.
    i `bases` of the 's' and 'd' units must be equal to combine <deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.
    x Cannot combine: <deb_decimal> vector with `bases` s = 20 and d = 12.
    x Cannot combine: <deb_decimal> vector with `bases` s = 50 and d = 16.
    i Use `deb_convert_bases()` to convert one or more of the vectors to compatible `bases`.

# lsd list check works

    `x` must be a list of numeric vectors.

---

    `x` must be a list of numeric vectors of length 3.
    x You've supplied a list of <numeric> vectors of length(s) 4 and 4.

---

    `x` must be a list of numeric vectors of length 3.
    x You've supplied a list of <numeric> vectors of length(s) 3, 3, and 1.

# tetra list check works

    `x` must be a list of numeric vectors.

---

    `x` must be a list of numeric vectors of length 4.
    x You've supplied a list of <numeric> vectors of length(s) 3 and 3.

