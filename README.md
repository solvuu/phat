# [Phat](https://github.com/solvuu/phat)
Phat, an anagram of "path", provides a strongly typed representation
of file paths and file system operations.

## Quick Start
Install by doing `opam install phat`. Then launch `utop`.

```ocaml
# #require "phat.async_unix";;
# module Phat = Phat_async_unix.Std;;
```

The API is structured as one monolothic module. Presently only the
Async backend is fully developed, but future work will add support for
Lwt and blocking APIs. Now take a look at the API documentation of
[Path](https://github.com/solvuu/phat/blob/master/lib/base/phat_path.mli)
and
[Filesys](https://github.com/solvuu/phat/blob/master/lib/async_unix/filesys.mli). Both
modules are included in `Std`, so constructs therein are available
directly. Also see below for a high level description of the main
concepts.

## File Paths
Conceptually a path is a non-empty list of reserved items such as
`Root ("/")`, `Dot (".")`, or `Dotdot ".."`, or any user chosen `name`
where certain characters such as slash are disallowed. The empty
string `""` is equivalent to `"."`, and both are parsed to `Dot`.

We represent such a list with a GADT `t`, which has 2 type parameters:

- `'kind`: is either `abs` or `rel`. The first item in an absolute
  path is `"/"`, the root directory. The first item in a relative path
  is guaranteed not to be `"/"`.

- `'typ`: can be `file`, `dir`, or `link` and indicates the type of
  file system object identified by the path. This is a property of the
  last item in a path; every non-last item must be a
  directory. Crucially, this parameter indicates the type of the
  object *after resolving symbolic links*. Thus, `'typ = link`
  actually indicates a broken link. If it is unknown whether a link
  points to a regular file or directory, the best we can do is denote
  it as a link.

These type parameters restrict operations in sensible ways. For
example, you can request the items under a directory but not under a
regular file.

Only absolute paths can resolve to an actual file or directory in your
filesystem. A relative path cannot; it is incomplete in the sense that
it is unknown what (absolute) directory the path is relative to. Said
another way, an absolute path is truly a path, but a relative path is
an abstraction, which, when given an absolute directory prefix, will
yield a (absolute) path.

A **normalized** path contains the minimum number of `Dot`s and
`Dotdot`s possible. For an absolute path, this means there are no
`Dot`s or `Dotdot`s. For a relative path, it means `Dot`s and
`Dotdot`s occur only in cases where the path is: a lone `Dot`, or a
consecutive sequence of `Dotdot`s at the beginning followed by only
named items.

A **resolved** path does not contain any links. Resolving a path
replaces links by its target. The kind of the path may change in the
process: a relative path may become absolute after resolution (but not
vice versa). However, resolving a path will not change the type of the
object it identifies. If a path has the `link` type, then this means
it represents a broken link: by convention we resolve a broken link to
itself.

We have defined `equal p q` to mean `p` and `q` are the same after
normalization, i.e. disregarding `Dot`s and `Dotdot`s that don't
affect the path ultimately being referred to. This is not the only
possible choice; one might also want to disregard links and say paths
are equal if they resolve to the same path. However, we believe that
users of this library will often care about the link structure of a
path, so we did not choose this definition. One can check this
alternate definition of equality by manually calling `resolve` first,
so there is no limitation. On the other hand, one might care even
about the `Dot`s and `Dotdot`s, in which case you can call OCaml's
polymorphic equality `Pervasives.(=)`. Only paths of the same kind and
type can be tested for equality, even though the string
representations could be equal for paths not satisfying these
criteria.

## Limitations
* We currently only support Unix-like systems. The library is untested
  on Windows, but we welcome pull requests.

* Only an
  [Async](https://github.com/janestreet/?utf8=%E2%9C%93&query=async)
  version of file system operations are supported. There are no
  blocking or [Lwt](http://ocsigen.org/lwt/) versions. However, the
  code is already structured to extend to these cases. Let us know if
  you need it, or better, please submit a pull request.

## License
Released under the ISC license. See the LICENSE file.

## Contact
Please [post](https://github.com/solvuu/phat/issues) bug reports,
feature requests, and questions as on GitHub.
