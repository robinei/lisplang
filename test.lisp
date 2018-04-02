
(let (n 100000000
      a 0
      b 1
      temp 0
      zero 0)
    (tagbody
      start
        (if (> (dec n) zero)
            (tagbody
                (= temp b)
                (= b (+ a b))
                (= a temp)
                (go start)))
        (print b)))
