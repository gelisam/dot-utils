# dot-transitive

Remove edges which are implied by the transitive closure of the existing edges.

## example input

```
strict digraph {
  a -> b;
  a -> c;
  b -> c;
}
```

## example output

```
strict digraph {
  a -> b;
  b -> c;
}
```
