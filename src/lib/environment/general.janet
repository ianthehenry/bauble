(use ./import)

(deftransform color [shape color]
  "Set a shape's color field."
  (shape/with shape :color (typecheck color jlsl/type/vec3)))

(setdyn '@+ (table/getproto (dyn '+)))
(setdyn '@- (table/getproto (dyn '-)))
(setdyn '@* (table/getproto (dyn '*)))
(setdyn '@/ (table/getproto (dyn '/)))

(defn ss
  ````
  This is a wrapper around `smoothstep` with a different argument order, which also
  allows the input edges to occur in descending order.

  There are several overloads:

  ```
  (ss x)
  # becomes
  (smoothstep 0 1 x)
  ```

  ```
  (ss x [from-start from-end])
  # becomes
  (if (< from-start from-end)
    (smoothstep from-start from-end x)
    (1 - smoothstep from-end from-start x))
  ``` 

  ```
  (ss x from [to-start to-end])
  # becomes
  (ss x from * (- to-end to-start) + to-start)
  ```
  ````
  [x &opt from-range to-range]
  (when to-range
    (def [to-start to-end] to-range)
    (break
      (cond
        (@and (number? to-start) (= to-start 0)) (sugar (ss x from-range * to-end))
        (@and (number? to-start) (number? to-end))
          (sugar (ss x from-range * (to-end - to-start) + to-start))
        (gl/let [to-start to-start]
          (sugar (ss x from-range * (to-end - to-start) + to-start))))))
  (when from-range
    (def [from-start from-end] from-range)
    (break (if (@and (number? from-start) (number? from-end))
      # if these are both constants, we can decide right here what to do
      (if (< from-start from-end)
        (smoothstep from-start from-end x)
        (- 1 (smoothstep from-end from-start x)))
      (gl/let [from-start from-start from-end from-end]
        (jlsl/do
          (if (< from-start from-end)
            (smoothstep from-start from-end x)
            (- 1 (smoothstep from-end from-start x))))))))
  (smoothstep 0 1 x))

(defn sin+ "Like `sin`, but returns a number in the range `0` to `1`." [x] (remap+ (sin x)))
(defn sin- "Like `sin`, but returns a number in the range `0` to `-1`." [x] (remap- (sin x)))
(defn cos+ "Like `cos`, but returns a number in the range `0` to `1`." [x] (remap+ (cos x)))
(defn cos- "Like `cos`, but returns a number in the range `0` to `-1`." [x] (remap- (cos x)))

(sugar (defn- oscillate [trig x period from to]
  (def sig (trig (tau * x / period)))
  (if (number? from)
    (to - from * sig + from)
    (gl/let [from from] (to - from * sig + from)))))

(def osc ````(osc &opt period lo hi)

Returns a number that oscillates with the given period. There are several overloads:

```
# 0 to 1 to 0 every second
(osc t)

# 0 to 1 to 0 every 10 seconds
(osc t 10)

# 0 to 100 to 0 every 10 seconds
(osc t 10 100)

# 50 to 100 to 50 every 10 seconds
(osc t 10 50 100)
```
````
  (fn osc [x & args]
    (case (@length args)
      0 (osc x 1 0 1)
      1 (osc x (args 0) 0 1)
      2 (osc x (args 0) 0 (args 1))
      3 (oscillate (fn [theta] (- 1 (cos+ theta))) x ;args)
      (error "too many arguments"))
    ))

(def oss ````(oss &opt period lo hi)

Like `osc`, but uses a sine wave instead of a cosine wave,
so the output begins halfway between `lo` and `hi`.
````
  (fn oss [x & args]
    (case (@length args)
      0 (oss x 1 0 1)
      1 (oss x (args 0) 0 1)
      2 (oss x (args 0) 0 (args 1))
      3 (oscillate sin+ x ;args)
      (error "too many arguments"))
    ))
