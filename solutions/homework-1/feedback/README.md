# От себя

Task3 реализована некорректно, я не учитываю что на вход может прийти элемент который уже есть в дереве, считая что всегда элементы уникальны и это гарантируется условием.

Как результат - оказался неправ, за что потерял 2 балла из 5

# От препов

Сняли 0.8 баллов за варнинги:
 * Non-exhaustive pattern-matchings. -0.4
 * Unused matches. -0.2
 * Name shadowing. -0.2

# На чём попались студенты ещё

*Будет дополняться. Если есть что добавить, сообщите*

1. -0.2: don't use `case` with numbers or pattern-match on numbers, use guards instead

2. -0.1: code style is good overall, but some lines are too long, so it's better to split such lines to several ones:
e.g.

`f args = longBody`

to

```
f args = 
  longBody
```

3. -0.2: try to avoid `undefined` in your code. use `Maybe` or pass default value instead. if you _really_ need to throw error, use `error` with meaningful error msg

...

