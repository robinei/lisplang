(do
  (if #t (print 123))

  (let x 1
    (print (+ x (inc x))))

  (let (x 0
        f (fun (x :: i32)
            (* x 2)))
    (= x (f 10))
    (print x))

  (let (n :: i32 100000000
        a 0
        b 1
        temp 0
        zero 0)
      (tagbody
        start
          (if (> (dec n) zero)
              (do (= temp b)
                  (= b (+ b a))
                  (= a temp)
                  (go start)))
          (print b))))
