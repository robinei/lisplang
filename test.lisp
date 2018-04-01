
(let (n 10000000
      a 0
      b 1
      temp 0)
    (tagbody
      start
        (= n (- n 1))
        (if (> n 0)
            (tagbody
                (= temp b)
                (= b (+ a b))
                (= a temp)
                (go start)))
        (print b)))
