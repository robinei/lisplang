(do
  (if #t (print 123))

  (do (print 666))

  (let x 1
    (print (+ x (inc x))))

  (let x 1
       y 100
    (print (+ x y)))

  (do (let x 2)
      (let y 5
           z 10)
      (print (+ x (+ y z))))

  (let (x 0
        g (fun (x :: i32) (* x 2))
        f (fun (x :: i32) (g x)))
    (= x (g 10))
    (print x))

  (let (n :: i32 100000000
        a 0
        b 1
        temp 0
        zero 0)
      (tagbody
        start
          (when (> (dec n) zero)
            (= temp b)
            (= b (+ b a))
            (= a temp)
            (go start))
          (print b))))
