# A small raytracer side project

This is a simple raytracer project which was started as a learning exercise, multiple sources were used, and can be found in the form of links strewn around raytracer.lisp. 

To run, clone repo and navigate to the directory, and run `(load "test.lisp")` in whatever Common Lisp dialect you favour.

## Why in Lisp?

I find it useful for prototyping as it is:

* dynamic
* object oriented
* highly interactive
* functional

I would like to re-implement this in ocaml at some point, also for funsies.

## Requirements

* A conforming Common Lisp implementation, I only tested on SBCL, but there should be nothing "interesting" about what I am doing
* test.lisp uses Quicklisp to load the system and the excellent `iterate` library (used throughout), but if you are comfortable loading things manually you need not actually install it.

## Performance

I haven't made any attempt to optimise the process, in fact there is a compiler directive in test.lisp to optimise debug and safety over performance.

It could benefit heavily from some parallel processing.

## Output

![Raytraced Image](https://github.com/cjallen88/raytracer/blob/master/scene-test.png)

## License

GPL

