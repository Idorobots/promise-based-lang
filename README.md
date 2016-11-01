# Promised language

This is an exploration of weird programming language features & a benchmark of different approaches to implementing concurrency in a programming language runtime. Two approaches are considered:

- Promise-based,
- Continuation-Passing-Style-based (both with and without yielding execution flow).

The benchmark shows general performance of each of the approaches in terms of execution time of a known piece of code - a recursive Fibonacci sequence computation. No valid conclusions are drawn from the benchmark results, but some baller language features were implemented, so it's fine.

## Rationale

One day, while drowning in `Future`'s (a.k.a. programming) in Scala, I was thinking how nice it is that Haskell hides non-strictness (a.k.a. lazyness) underneath, so no obnoxious lazy stream creation/force'ing needs to be done. And then it struck me: how cool would it be to treat Promises/Futures in a language the same way that Haskell treats lazy values? Would such a language even make sense? How would you program in such a language?
So I've implemented a simple Promise in Scheme along with a bunch of primitives & syntax transformations, so I could test this proposition. Here's a example piece of code:

```scheme
(define (fib n)
  (&if (&<= n (& 1))
       (& 1)
       (&+ (fib (&- n (& 1)))
           (fib (&- n (& 2))))))
```

As you can see, this code looks very similar to standard Scheme. In fact, a transformation from strict Scheme to purely Promise-based language could be done at compile time, assuming all the primitives are converted to `&-` versions and all values are wrapped in promises `(& ...)`. 
The striking thing is, that `&<=` or `&+` could perform all kinds of asynchronous operations under the hood and the code would still look and work the same way. There would be no difference between synchronous and asynchronous code and there would be no need for endless `Future` `flatMap`'ping. Pretty decent, if you ask me. 
So to answer the questions: "pretty cool", "deffinitely" and "just as usual, maybe even smoother".

Oh, and afterwards I've noticed some parallels [sic] with CPS-based Actor Model I've implemented earlier, so I guess this has turned into a benchmark of different concurrency implementation approaches. ¯\\\_(ツ)\_/¯
