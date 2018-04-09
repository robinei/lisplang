(tagbody
  (let (x 1)
    (print (+ x (inc x))))

  (let (f (fun (x :: i32)
            (* x 2)))
    (print (f 10)))

  (let (n :: i32 100000000
        a 0
        b 1
        temp 0
        zero 0)
      (tagbody
        start
          (if (> (dec n) zero)
              (tagbody
                  (= temp b)
                  (= b (+ b a))
                  (= a temp)
                  (go start)))
          (print b))))
