# dot-utils

A set of small command-line tools for transforming GraphViz dot-files.


## dot-closure

Keep the sub-graph which is accessible from a given set of nodes.

```
$ cat input.dot
digraph {
  a1;
  b1;
  a2;
  b2;
  a3;
  b3;
  z;
  a1 -> b1;
  b1 -> z;
  a2 -> b2;
  b2 -> z;
  a3 -> b3;
  b3 -> z;
}
$ dot-closure a1 b2 < input.dot
digraph {
  a1;
  b1;
  b2;
  z;
  a1 -> b1;
  b1 -> z;
  b2 -> z;
}
```


## dot-reverse

Flip all the edges.

```
$ cat input.dot
strict digraph {
  a -> b;
  b -> c;
}
$ dot-reverse < input.dot
strict digraph {
  b -> a
  c -> b
}
```
## dot-transitive

Remove edges which are implied by the transitive closure of the existing edges.

```
$ cat input.dot
strict digraph {
  a -> a;
  a -> b;
  a -> c;
  b -> c;
}
$ dot-transitive < input.dot
strict digraph {
  a -> b;
  b -> c;
}
```
