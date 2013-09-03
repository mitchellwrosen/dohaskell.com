# Functions

*dohaskell.com* will provide an interface for implementing Haskell functions, which are compiled and checked for
correctness on the server. There are two different types of functions: **library functions** and **user-defined
functions**.

## Library Functions

A **library function** exists in a standard library already installed on the server. Checking the student's implementation
for correctness is thus feasible by comparing the output to the library version with QuickCheck. The functions must have
different names (e.g. 'foldl' vs 'my_foldl'), which may be transparent to the student.

## User-Defined Functions

A **user-defined function** is created and uploaded by a *developer*. In contrast with a library function, a user-defined
function does not exist in a standard library and thus does not belong to a specific module. In addition, it contains
implementation code.

A user-defined function must also contain information relevant to checking it for correctness. This will exist as
code supplied by the developer. The developer should be able to *easily* and *optionally* **not** have to write his/her
own testing code, and instead opt to automatically use QuickCheck. This will auto-generate a test with a single property that
compares the student implementation to the developer implementation, much like the library functions are tested.

### *Technical aside*

```LibFunction``` and ```UserFunction``` are separate database tables, but because they share so many
attributes, you'll find the unification of the two types in ```Types.hs```: ```Function = LibF LibFunction | UserF
UserFunction```, which makes it possible to write treat them like the same type when you want to, e.g. in
```Function/Utils.hs```.

---

[Home](home.html)
