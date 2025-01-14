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


## dot-group

The first node of each group is the "leader". Remove all non-leaders from each
group and move all the arrows pointing into or out of the group so that they
point into or out of the leader instead.

```
$ cat groups.txt
[1]
a1
b1
c1

[2]
a2
b2
c2
$ cat input.dot
strict digraph {
  a1;
  a2;
  b1;
  b2;
  c1;
  c2;
  d;

  a1 -> a2;
  b1 -> c2;
  b2 -> d;
}
$ dot-group --groupFile=groups.txt < input.dot
digraph {
  "[1]"
  "[2]"
  "d"
  "[1]" -> "[2]"
  "[2]" -> "d"
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

## dot-sinkify

Delete the outgoing edges from a few nodes, thus turning them into sinks.
Especially useful when combined with dot-closure, to find all the nodes which
can be reached by paths which do _not_ go through these
nodes-converted-into-sinks.

```
$ cat example.dot
strict digraph {
  source -> x;
  source -> y;
  x -> blocker;
  blocker -> z;
  blocker -> unreachable;
  y -> z;
}
$ cat example.dot | dot-sinkify --targets=blocker
strict digraph {
  source -> x
  source -> y
  x -> blocker
  y -> z
}
$ cat example.dot | dot-sinkify --targets=blocker | dot-closure --roots=source
strict digraph {
  source -> x
  source -> y
  x -> blocker
  y -> z
}
```

## dot-subset

Restrict a graph to the nodes you're interested in, collapsing indirect paths to
direct paths.

```
$ cat input.dot
digraph {
  x0
  x1
  x2
  x3
  x4
  x5
  a
  b
  c
  x0 -> a
  a -> x1
  a -> x2
  x1 -> b
  x2 -> b
  b -> x3
  x3 -> x4
  x4 -> c
  c -> x5
}
$ dot-subset --subset="a b c" < input.dot
digraph {
  a
  b
  c
  a -> b
  b -> c
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
