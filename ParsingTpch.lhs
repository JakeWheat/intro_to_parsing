
= Overview

In this file we are going to get the TPC-H queries parsing. This can
be an example of taking a parser, identifying some missing features,
and adding them. The TPC-H queries are slightly more complex than
trivial, and will also serve to exercise the human readable layout
feature of the pretty printer.

Let's try the parser out on the TPC-H queries.

```
```

Summary of errors so far:
q1: typed literal: type_name 'literal value'
q2: scalar subquery
q3: typed literal
q4: typed literal
q5: typed literal
q6: typed literal
q7: ??
q8: extract??
q9: extract ??
q10: typed literal
q11: scalar sub query
q12: ??
q13: not like??
q14: decimal literal
q15: cte
q16: count distinct
q17: decimal literal
q18: in subquery
q19: in literal list
q20: in subquery
q21: exists subquery
q22: substring
