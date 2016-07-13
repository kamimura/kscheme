(begin
  ;; Equivalence predicates
  ;; union-find algorithm
  (define equal?
    (lambda (obj1 obj2)
      (define union-find
        (lambda (table x y)
          (define find
            (lambda (pair)
              (define n (car pair))
              (if (pair? n)
                  ((lambda ()
                     (define iter
                       (lambda (pair n)
                         (define n0 (car n))
                         (if (pair? n0)
                             ((lambda ()
                                (set-car! pair n0)
                                (iter n n0)))
                             n)))
                     (iter pair n)))
                  pair)))
          (define bx (assq x (cdr table)))
          (define by (assq y (cdr table)))
          (if (not bx)
              (if (not by)
                  ((lambda ()
                     (define b (list 1))
                     (set-cdr! table (cons (cons x b) (cdr table)))
                     (set-cdr! table (cons (cons y b) (cdr table)))
                     #f))
                  ((lambda ()
                     (define ry (find (cdr by)))
                     (set-cdr! table (cons (cons x ry) (cdr table)))
                     #f)))
              (if (not by)
                  ((lambda ()
                     (define rx (find (cdr bx)))
                     (set-cdr! table (cons (cons y rx) (cdr table)))
                     #f))
                  ((lambda ()
                     (define rx (find (cdr bx)))
                     (define ry (find (cdr by)))
                     (or (eq? rx ry)
                         ((lambda ()
                            (define nx (car rx))
                            (define ny (car ry))
                            (if (> nx ny)
                                ((lambda ()
                                   (set-car! ry rx)
                                   (set-car! rx (+ nx ny))
                                   #f))
                                ((lambda ()
                                   (set-car! rx ry)
                                   (set-car! ry (+ ny nx))
                                   #f))))))))))))
      (define table (list '*table))
      (define e?
        (lambda (x y)
          (if (eq? x y)
              #t
              (if (pair? x)
                  (and (pair? y)
                       (or (union-find table x y)
                           (and (e? (car x) (car y))
                                (e? (cdr x) (cdr y)))))
                  (if (vector? x)
                      (and (vector? y)
                           ((lambda ()
                              (define n (vector-length x))
                              (and (= (vector-length y) n)
                                   (or (union-find table x y)
                                       ((lambda ()
                                          (define proc
                                            (lambda (i)
                                              (or (= i n)
                                                  (and (e? (vector-ref x i)
                                                           (vector-ref y i))
                                                       (proc (+ i 1))))))
                                          (proc 0))))))))
                      (if (bytevector? x)
                          (and (vector? y)
                               ((lambda ()
                                  (define n (bytevector-length x))
                                  (and
                                   (= (bytevector-length y) n)
                                   (or
                                    (union-find table x y)
                                    ((lambda ()
                                       (define proc
                                         (lambda (i)
                                           (or (= i n)
                                               (and (e? (bytevector-u8-ref x i)
                                                        (bytevector-u8-ref y i))
                                                    (proc (+ i 1))))))
                                       (proc 0))))))))                        
                          (if (string? x)
                              (and (string? y) (string=? x y))
                              (eqv? x y))))))))
      (e? obj1 obj2)))

  ;; Numbers
  (define <
    (lambda (x1 x2 . rest)
      (define iter
        (lambda (y1 y2 rest0)
          (if (not (and (real? y1) (real? y2)))
              (error "(<) wrong type argument --" (cons x1 (cons x2 rest)))
              (if (not (negative? (- y1 y2)))
                  #f
                  (if (null? rest0)
                      #t
                      (iter y2 (car rest0) (cdr rest0)))))))
      (iter x1 x2 rest)))

  (define >
    (lambda (x1 x2 . rest)
      (define iter
        (lambda (y1 y2 rest0)
          (if (not (and (real? y1) (real? y2)))
              (error "(>) wrong type argument --" (cons x1 (cons x2 rest)))
              (if (not (positive? (- y1 y2)))
                  #f
                  (if (null? rest0)
                      #t
                      (iter y2 (car rest0) (cdr rest0)))))))
      (iter x1 x2 rest)))

  (define <=
    (lambda (x1 x2 . rest)
      (define iter
        (lambda (y1 y2 rest0)
          (if (not (and (real? y1) (real? y2)))
              (error "(<=) wrong type argument --" (cons x1 (cons x2 rest)))
              (if (positive? (- y1 y2))
                  #f
                  (if (null? rest0)
                      #t
                      (iter y2 (car rest0) (cdr rest0)))))))
      (iter x1 x2 rest)))

  (define >=
    (lambda (x1 x2 . rest)
      (define iter
        (lambda (y1 y2 rest0)
          (if (not (and (real? y1) (real? y2)))
              (error "(>=) wrong type argument --" (cons x1 (cons x2 rest)))
              (if (negative? (- y1 y2))
                  #f
                  (if (null? rest0)
                      #t
                      (iter y2 (car rest0) (cdr rest0)))))))
      (iter x1 x2 rest)))

  (define odd?
    (lambda (n)
      (if (integer? n)
          (= (floor-remainder n 2) 1)
          (error "(odd?) wrong type of argument --" n))))
  (define even?
    (lambda (n)
      (if (integer? n)
          (= (floor-remainder n 2) 0)
          (error "(even?) wrong type of argument --" n))))
  (define max
    (lambda (x . rest)
      (define gt
        (lambda (n1 n2)
          (define convert (if (or (inexact? n1)
                                  (inexact? n2))
                              inexact
                              exact))
          (if (> n1 n2)
              (convert n1)
              (convert n2))))
      (define iter
        (lambda (x rest)
          (if (null? rest)
              x
              (if (not (real? (car rest)))
                  (error "(max) wrong type argument --" (car rest))
                  (iter (gt x (car rest)) (cdr rest))))))
      (if (real? x)        
          (iter x rest)
          (error "(max) wrong type argument --" x))))
  (define min
    (lambda (x . rest)
      (define lt
        (lambda (n1 n2)
          (define convert (if (or (inexact? n1)
                                  (inexact? n2))
                              inexact
                              exact))
          (if (< n1 n2)
              (convert n1)
              (convert n2))))
      (define iter
        (lambda (x rest)
          (if (null? rest)
              x
              (if (not (real? (car rest)))
                  (error "(min) wrong type argument --" (car rest))
                  (iter (lt x (car rest)) (cdr rest))))))
      (if (real? x)        
          (iter x rest)
          (error "(min) wrong type argument --" x))))

  (define floor/
    (lambda (n1 n2)
      (if (and (integer? n1) (integer? n2))
          (values (floor-quotient n1 n2) (floor-remainder n1 n2))
          (error "(floor/) wrong type argument -- " (list n1 n2)))))

  (define floor-quotient  
    (lambda (n1 n2)
      (if (and (integer? n1) (integer? n2))
          (floor (/ n1 n2))
          (error "(floor-quotient) wrong type argument --" (list n1 n2)))))

  (define floor-remainder
    (lambda (n1 n2)
      (if (and (integer? n1) (integer? n2))
          (- n1 (* n2 (floor (/ n1 n2))))
          (error "(floor-remainder) wrong type argument --" (list n1 n2)))))


  (define truncate/
    (lambda (n1 n2)
      (if (and (integer? n1) (integer? n2))
          (values (truncate-quotient n1 n2) (truncate-remainder n1 n2))
          (error "(truncate/) wrong type argument --" (list n1 n2)))))

  (define truncate-quotient
    (lambda (n1 n2)
      (if (and (integer? n1) (integer? n2))
          (truncate (/ n1 n2))
          (error "(trundate-quotient/) wrong type argument --" (list n1 n2)))))

  (define truncate-remainder
    (lambda (n1 n2)
      (if (and (integer? n1) (integer? n2))
          (- n1 (* n2 (truncate (/ n1 n2))))
          (error "(truncate-remainder) wrong type argument --" (list n1 n2)))))

  (define quotient truncate-quotient)
  (define remainder truncate-remainder)
  (define modulo floor-remainder)

  ;; Stern-Brocot tree
  (define rationalize
    (lambda (x y)
      (if (not (and (real? x) (real? y)))
          (error "(rationalize) wrong type argument -- " (list x y))
          ((lambda ()
             (define diff (abs y))           
             (define low (- x diff))
             (define high (+ x diff))
             (define proc (if (and (exact? x) (exact? y)) exact inexact))
             (if (<= (* low high) 0)
                 (proc 0)
                 (if (= low high)
                     (proc low)
                     ((lambda ()
                        (define sign (if (positive? x) 1 -1))
                        (define low0 (if (positive? sign) low (abs high)))
                        (define high0 (if (positive? sign) high (abs low)))
                        (define between?
                          (lambda (x) (and (<= low0 x) (<= x high0))))
                        (define stern-brocot-tree
                          (lambda (pnum pden qnum qden)
                            (define a (/ (+ pnum qnum)
                                         (+ pden qden)))
                            (if (between? a)
                                a
                                ((lambda ()
                                   (define num (numerator a))
                                   (define den (denominator a))
                                   (if (< high0 a)
                                       (stern-brocot-tree pnum pden
                                                          num den)
                                       (stern-brocot-tree num den
                                                          qnum qden)))))))
                        (proc (* sign (stern-brocot-tree 0 1 1 0))))))))))))
  
  ;; Numbers end

  ;; Booleans
  ;; Booleans endx
  ;; Pairs and lists
  (define caar (lambda (pair) (car (car pair))))
  (define cadr (lambda (pair) (car (cdr pair))))
  (define cdar (lambda (pair) (cdr (car pair))))
  (define cddr (lambda (pair) (cdr (cdr pair))))

  ;; cxr library
  (define caaar (lambda (pair) (car (car (car pair)))))
  (define caadr (lambda (pair) (car (car (cdr pair)))))
  (define cadar (lambda (pair) (car (cdr (car pair)))))
  (define caddr (lambda (pair) (car (cdr (cdr pair)))))
  (define cdaar (lambda (pair) (cdr (car (car pair)))))
  (define cdadr (lambda (pair) (cdr (car (cdr pair)))))
  (define cddar (lambda (pair) (cdr (cdr (car pair)))))
  (define cdddr (lambda (pair) (cdr (cdr (cdr pair)))))
  (define caaaar (lambda (pair) (car (car (car (car pair))))))
  (define caaadr (lambda (pair) (car (car (car (cdr pair))))))
  (define caadar (lambda (pair) (car (car (cdr (car pair))))))
  (define caaddr (lambda (pair) (car (car (cdr (cdr pair))))))
  (define cadaar (lambda (pair) (car (cdr (car (car pair))))))
  (define cadadr (lambda (pair) (car (cdr (car (cdr pair))))))
  (define caddar (lambda (pair) (car (cdr (cdr (car pair))))))
  (define cadddr (lambda (pair) (car (cdr (cdr (cdr pair))))))
  (define cdaaar (lambda (pair) (cdr (car (car (car pair))))))
  (define cdaadr (lambda (pair) (cdr (car (car (cdr pair))))))
  (define cdadar (lambda (pair) (cdr (car (cdr (car pair))))))
  (define cdaddr (lambda (pair) (cdr (car (cdr (cdr pair))))))
  (define cddaar (lambda (pair) (cdr (cdr (car (car pair))))))
  (define cddadr (lambda (pair) (cdr (cdr (car (cdr pair))))))
  (define cdddar (lambda (pair) (cdr (cdr (cdr (car pair))))))
  (define cddddr (lambda (pair) (cdr (cdr (cdr (cdr pair))))))
  
  (define append
    (lambda args
      (if (null? args)
          '()
          ((lambda ()
             (define reversed (reverse args))
             (define iter
               (lambda (rest result)
                 (define iter0
                   (lambda (items result)
                     (if (null? items)
                         result
                         (iter0 (cdr items) (cons (car items) result)))))
                 (if (null? rest)
                     result
                     (if (list? (car rest))
                         (iter (cdr rest)
                               (iter0 (reverse (car rest)) result))
                         (error "(append) wrong type argument -- "  args)))))
             ((lambda (last)
                (if (or (pair? last) (list? last))
                    (iter (cdr reversed) last)
                    last))
              (car reversed)))))))

  (define reverse
    (lambda (items)
      (define iter
        (lambda (items result)
          (if (null? items)
              result
              (iter (cdr items) (cons (car items) result)))))
      (if (list? items)
          (iter items '())
          (error "(append) wrong type argument -- "  (list items)))))
  
  (define list-tail
    (lambda (x k)
      (if (zero? k)
          x
          (list-tail (cdr x) (- k 1)))))

  (define list-ref
    (lambda (items k)
      (if (zero? k)
          (car items)
          (list-ref (cdr items) (- k 1)))))

  (define list-set!
    (lambda  (items k obj)
      (if (= k 0)
          (set-car! items obj)
          (list-set! (cdr items) (- k 1) obj))))

  (define memq (lambda (obj items) (member obj items eq?)))
  (define memv (lambda (obj items) (member obj items eqv?)))
  (define member
    (lambda (obj items . compare)
      (define iter
        (lambda (items compare)
          (if (null? items)
              #f
              (if (compare obj (car items))
                  items
                  (iter (cdr items) compare)))))
      (if (null? compare)
          (iter items equal?)
          (iter items (car compare)))))

  (define assq (lambda (obj alist) (assoc obj alist eq?)))
  (define assv (lambda (obj alist) (assoc obj alist eqv?)))
  (define assoc
    (lambda (obj alist . compare)
      (define iter
        (lambda (alist compare)
          (if (null? alist)
              #f
              ((lambda (pair)
                 (if (compare obj (car pair))
                     pair
                     (iter (cdr alist) compare)))
               (car alist)))))
      (if (null? compare)
          (iter alist equal?)
          (iter alist (car compare)))))
  (define list-copy
    (lambda (obj)
      (define iter
        (lambda (items)
          (if (or (null? items) (not (pair? items)))
              items
              (cons (car items) (iter (cdr items))))))
      (if (list? obj)
          (iter obj)
          (if (pair? obj)
              ((lambda ()
                 (define cycle?
                   (lambda (a b)
                     (if (eq? a b)
                         #t
                         (if (or (null? b)
                                 (not (pair? b))
                                 (null? (cdr b))
                                 (not (pair? (cdr b))))
                             #f
                             (cycle? (cdr a) (cddr b))))))
                 (if (cycle? obj (cdr obj))
                     (error "(list-copy) wrong type argument -- " (list obj))
                     (iter obj))))
              obj))))
  ;; Pairs and lists end

  ;; Symbols
  ;; Symbols end
  
  ;; Characters
  (define char=?
    (lambda (char1 char2 . args)
      (define n (char->integer char1))
      (define iter
        (lambda (rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (= n m)
                     (iter (cdr rest))
                     #f))
               (char->integer (car rest))))))
      (if (= n (char->integer char2))
          (iter args)
          #f)))
  (define char<?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (< n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (car rest))))))
      (define n (char->integer char2))
      (if (< (char->integer char1) n)
          (iter n args)
          #f)))
  (define char>?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (> n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (car rest))))))
      (define n (char->integer char2))
      (if (> (char->integer char1) n)
          (iter n args)
          #f)))
  (define char<=?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (<= n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (car rest))))))
      (define n (char->integer char2))
      (if (<= (char->integer char1) n)
          (iter n args)
          #f)))
  (define char>=?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (>= n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (car rest))))))
      (define n (char->integer char2))
      (if (>= (char->integer char1) n)
          (iter n args)
          #f)))

  (define char-ci=?
    (lambda (char1 char2 . args)
      (define n (char->integer (char-foldcase char1)))
      (define iter
        (lambda (rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (= n m)
                     (iter (cdr rest))
                     #f))
               (char->integer (char-foldcase (car rest)))))))
      (if (= n (char->integer (char-foldcase char2)))
          (iter args)
          #f)))
  (define char-ci<?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (< n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (char-foldcase (car rest)))))))
      (define n (char->integer (char-foldcase char2)))
      (if (< (char->integer (char-foldcase char1)) n)
          (iter n args)
          #f)))
  (define char-ci>?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (> n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (char-foldcase (car rest)))))))
      (define n (char->integer (char-foldcase char2)))
      (if (> (char->integer (char-foldcase char1)) n)
          (iter n args)
          #f)))
  (define char-ci<=?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (<= n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (char-foldcase (car rest)))))))
      (define n (char->integer (char-foldcase char2)))
      (if (<= (char->integer (char-foldcase char1)) n)
          (iter n args)
          #f)))
  (define char-ci>=?
    (lambda (char1 char2 . args)
      (define iter
        (lambda (n rest)
          (if (null? rest)
              #t
              ((lambda (m)
                 (if (>= n m)
                     (iter m (cdr rest))
                     #f))
               (char->integer (char-foldcase (car rest)))))))
      (define n (char->integer (char-foldcase char2)))
      (if (>= (char->integer (char-foldcase char1)) n)
          (iter n args)
          #f)))

  ;; Characters end
  ;; Strings
  (define string (lambda args (list->string args)))

  (define string=?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (rest)
          (if (null? rest)
              #t
              (if (equal? s1 (car args))
                  (iter (cdr args))
                  #f))))
      (if (equal? s1 s2)
          (iter args)
          #f)))
  (define string-ci=?
    (lambda (s1 s2 . args)
      (define s (string-foldcase s1))
      (define iter
        (lambda (rest)
          (if (null? rest)
              #t
              (if (equal? s (car args))
                  (iter (cdr args))
                  #f))))
      (if (equal? s (string-foldcase s2))
          (iter args)
          #f)))

  (define string<?
    (lambda (s1 s2 . args)
      (define cmp<?
        (lambda (s1 s2)
          (define len1 (string-length s1))
          (define len2 (string-length s2))
          (define iter
            (lambda (i)
              (if (= i len2)
                  #f
                  (if (= i len1)
                      #t
                      ((lambda (c1 c2)
                         (if (char<? c1 c2)
                             #t
                             (if (char>? c1 c2)
                                 #f
                                 (iter (+ i 1)))))
                       (string-ref s1 i) (string-ref s2 i))))))
          (iter 0)))
      (define iter
        (lambda (s1 s2 rest)
          (if (not (cmp<? s1 s2))
              #f
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest))))))
      (iter s1 s2 args)))

  (define string-ci<?
    (lambda (s1 s2 . args)
      (define cmp<?
        (lambda (s1 s2)
          (define t1 (string-foldcase s1))
          (define t2 (string-foldcase s2))
          (define len1 (string-length t1))
          (define len2 (string-length t2))
          (define iter
            (lambda (i)
              (if (= i len2)
                  #f
                  (if (= i len1)
                      #t
                      ((lambda (c1 c2)
                         (if (char<? c1 c2)
                             #t
                             (if (char>? c1 c2)
                                 #f
                                 (iter (+ i 1)))))
                       (string-ref t1 i) (string-ref t2 i))))))
          (iter 0)))
      (define iter
        (lambda (s1 s2 rest)
          (if (not (cmp<? s1 s2))
              #f
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest))))))
      (iter s1 s2 args)))

  (define string>?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (s1 s2 rest)
          (if (not (string<? s2 s1))
              #f
              (if (null? rest)
                  #t
                  (iter x2 (car rest) (cdr rest))))))
      (iter s1 s2 args)))

  (define string-ci>?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (s1 s2 rest)
          (if (not (string-ci<? s2 s1))
              #f
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest))))))
      (iter s1 s2 args)))

  (define string<=?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (s1 s2 rest)
          (if (not (string<? s1 s2))
              (if (not (string=? s1 s2))
                  #f
                  (if (null? rest)
                      #t
                      (iter s2 (car rest) (cdr rest))))
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest))))))
      (iter s1 s2 args)))

  (define string-ci<=?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (s1 s2 rest)
          (if (not (string-ci<? s1 s2))
              (if (not (string-ci=? s1 s2))
                  #f
                  (if (null? rest)
                      #t
                      (iter s2 (car rest) (cdr rest))))
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest))))))
      (iter s1 s2 args)))

  (define string>=?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (s1 s2 rest)
          (if (string>? s1 s2)
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest)))
              (if (string=? s1 s2)
                  (if (null? rest)
                      #t
                      (iter s2 (car rest) (cdr rest)))
                  #f))))
      (iter s1 s2 args)))

  (define string-ci>=?
    (lambda (s1 s2 . args)
      (define iter
        (lambda (s1 s2 rest)
          (if (string-ci>? s1 s2)
              (if (null? rest)
                  #t
                  (iter s2 (car rest) (cdr rest)))
              (if (string-ci=? s1 s2)
                  (if (null? rest)
                      #t
                      (iter s2 (car rest) (cdr rest)))
                  #f))))
      (iter s1 s2 args)))

  (define string-upcase
    (lambda (s)
      (define len (string-length s))
      (define s1 (make-string len))
      (define iter
        (lambda (k)
          (if (= k len)
              s1
              ((lambda ()
                 (string-set! s1 k (char-upcase (string-ref s k)))
                 (iter (+ k 1)))))))
      (iter 0)))
  (define string-downcase
    (lambda (s)
      (define len (string-length s))
      (define s1 (make-string len))
      (define iter
        (lambda (k)
          (if (= k len)
              s1
              ((lambda ()
                 (string-set! s1 k (char-downcase (string-ref s k)))
                 (iter (+ k 1)))))))
      (iter 0)))

  (define string-foldcase
    (lambda (s)
      (define len (string-length s))
      (define s1 (make-string len))
      (define iter
        (lambda (k)
          (if (= k len)
              s1
              ((lambda ()
                 (string-set! s1 k (char-foldcase (string-ref s k)))
                 (iter (+ k 1)))))))
      (iter 0)))

  (define substring (lambda (str start end) (string-copy str start end)))
  (define string-append
    (lambda args
      (define iter
        (lambda (items)
          (if (null? items)
              '()
              (append (string->list (car items))
                      (iter (cdr items))))))
      (list->string (iter args))))

  (define string->list
    (lambda (s . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (pair? args)
                      (if (null? (cdr args))
                          (string-length s)
                          (cadr args))
                      (string-length s)))
      (define iter
        (lambda (i)
          (if (>= i end)
              '()
              (cons (string-ref s i)
                    (iter (+ i 1))))))
      (iter start)))
  (define list->string
    (lambda (items)
      (define s (make-string (length items)))
      (define iter
        (lambda (i items)
          (if (null? items)
              s
              ((lambda ()
                 (string-set! s i (car items))
                 (iter (+ i 1) (cdr items)))))))
      (iter 0 items)))

  (define string-copy
    (lambda (s . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (pair? args)
                      (if (null? (cdr args))
                          (string-length s)
                          (cadr args))
                      (string-length s)))
      (define s1 (make-string (- end start)))
      (define iter
        (lambda (i j)
          (if (>= j end)
              s1
              ((lambda ()
                 (string-set! s1 i (string-ref s j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter 0 start)))
  (define string-copy!
    (lambda (to at from . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (pair? args)
                      (if (null? (cdr args))
                          (string-length from)
                          (cadr args))
                      (string-length from)))
      (define iter
        (lambda (i j)
          (if (< j end)
              ((lambda ()
                 (string-set! to i (string-ref from j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter at start)))

  (define string-fill!
    (lambda (str fill . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (string-length str)
                      (if (null? (cdr args))
                          (string-length str)
                          (cadr args))))
      (define iter
        (lambda (i)
          (if (< i end)
              ((lambda ()
                 (string-set! str i fill)
                 (iter (+ i 1)))))))
      (iter start)))
  ;; Strings end
  ;; Vectors
  (define make-vector
    (lambda (k . args)
      (if (and (exact-integer? k) (>= k 0))
          ((lambda ()
             (define len (length args))
             (define fill (if (= len 0)
                              #f
                              (if (= len 1)
                                  (car args)
                                  (error "(make-vector) wrong type argument -- "
                                         (cons k args)))))
              (list->vector (make-list k fill))))
          (error "(make-vector) wrong type argument -- " (cons k args)))))
  (define vector->list
    (lambda (vect . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (vector-length vect)
                      (if (null? (cdr args))
                          (vector-length vect)
                          (cadr args))))
      (define items (make-list (- end start)))
      (define iter
        (lambda (i j)
          (if (>= j end)
              items
              ((lambda ()
                 (list-set! items i (vector-ref vect j))
                 (iter (+ i 1) (+ j 1)))))))
      (if (and (vector? vect)
               (exact-integer? start) (>= start 0)
               (exact-integer? end) (> end start) (<= end (vector-length vect)))
          (iter 0 start)
          (error "(vector->list) wrong type argument -- " (cons vect args)))))
  (define vector->string
    (lambda (vect . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (vector-length vect)
                      (if (null? (cdr args))
                          (vector-length vect)
                          (cadr args))))
      (define s (make-string (- end start)))
      (define iter
        (lambda (i j)
          (if (>= j end)
              s
              ((lambda ()
                 (string-set! s i (vector-ref vect j))
                 (iter (+ i 1) (+ j 1)))))))
      (if (and (vector? vect)
               (exact-integer? start) (>= start 0)
               (exact-integer? end) (> end start) (<= end (vector-length vect)))
          (iter 0 start)
          (error "(vector->string) wrong type argument -- " (cons vect args)))))
  (define string->vector
    (lambda (s . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (string-length s)
                      (if (null? (cdr args))
                          (string-length s)
                          (cadr args))))
      (define vect (make-vector (- end start)))
      (define iter
        (lambda (i j)
          (if (>= j end)
              vect
              ((lambda ()
                 (vector-set! vect i (string-ref s j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter 0 start)))

  (define vector-copy
    (lambda (vect . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (vector-length vect)
                      (if (null? (cdr args))
                          (vector-length vect)
                          (cadr args))))
      (define v (make-vector (- end start)))
      (define iter
        (lambda (i j)
          (if (>= j end)
              v
              ((lambda ()
                 (vector-set! v i (vector-ref vect j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter 0 start)))

  (define vector-copy!
    (lambda (to at from . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (vector-length from)
                      (if (null? (cdr args))
                          (vector-length from)
                          (cadr args))))
      (define iter
        (lambda (i j)
          (if (< j end)
              ((lambda ()
                 (vector-set! to i (vector-ref from j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter at start)))

  (define vector-append
    (lambda args
      (define iter
        (lambda (items)
          (if (null? items)
              '()
              (append (vector->list (car items))
                      (iter (cdr items))))))
      (list->vector (iter args))))

  (define vector-fill!
    (lambda (vect fill . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (vector-length vect)
                      (if (null? (cdr args))
                          (vector-length vect)
                          (cadr args))))
      (define iter
        (lambda (i)
          (if (< i end)
              ((lambda ()
                 (vector-set! vect i fill)
                 (iter (+ i 1)))))))
      (iter start)))

  ;; Vectors end
  ;; Bytevectors
  (define make-bytevector
    (lambda (k . args)
      (define byte (if (null? args) 0 (car args)))
      (apply vector (make-list k byte))))
  (define bytevector-copy
    (lambda (bv . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (vector-length bv)
                      (if (null? (cdr args))
                          (vector-length bvx)
                          (cadr args))))
      (define v (make-bytevector (- end start)))
      (define iter
        (lambda (i j)
          (if (>= j end)
              v
              ((lambda ()
                 (bytevector-u8-set! v i (bytevector-u8-ref bv j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter 0 start)))

  (define bytevector-copy!
    (lambda (to at from . args)
      (define start (if (null? args) 0 (car args)))
      (define end (if (null? args)
                      (bytevector-length from)
                      (if (null? (cdr args))
                          (bytevector-length from)
                          (cadr args))))
      (define iter
        (lambda (i j)
          (if (< j end)
              ((lambda ()
                 (bytevector-u8-set! to i (bytevector-u8-ref from j))
                 (iter (+ i 1) (+ j 1)))))))
      (iter at start)))

  (define bytevector-append
    (lambda args
      (define iter
        (lambda (items k)
          (if (null? items)
              k
              (iter (cdr items) (+ k (bytevector-length (car items)))))))
      (define k (iter args 0))
      (define bv (make-bytevector k))
      (define iter0
        (lambda (i j end bv0)
          (if (>= j end)
              i
              ((lambda ()
                 (bytevector-u8-set! bv i (bytevector-u8-ref bv0 j))
                 (iter0 (+ i 1) (+ j 1) end bv0))))))
      (define iter1
        (lambda (i bvs)
          (if (not (null? bvs))
              ((lambda (bv)
                 (iter1 (+ i (iter0 i 0 (bytevector-length bv) bv))
                        (cdr bvs)))
               (car bvs)))))
      (iter1 0 args)
      bv))

  ;; Bytevectors end
  ;; Control features
  (define map
    (lambda (proc items . args)
      (define exist?
        (lambda (items)
          (if (null? items)
              #f
              (if (list? (car items))
                  #t
                  (exist? (cdr items))))))
      (define map0
        (lambda (proc items)
          (if (null? items)
              '()
              (cons (proc (car items))
                    (map0 proc (cdr items))))))
      (define map1
        (lambda (proc items)
          (if (member '() items)
              '()
              (cons (apply proc (map0 car items))
                    (map1 proc (map0 cdr items))))))
      (if (null? args)
          (if (exist? (list items))
              (map0 proc items)
              (error "(map) wrong type argument -- " (list proc items)))
          (if (exist? (cons items args))
              (map1 proc (cons items args))
              (error "(map) wrong type argument -- "
                     (cons proc (cons items args)))))))

  (define string-map
    (lambda (proc str . args)
      (list->string
       (apply map proc (map string->list (cons str args))))))

  (define vector-map
    (lambda (proc vect . args)
      (list->vector
       (apply map proc (map vector->list (cons vect args))))))

  (define for-each
    (lambda (proc items . args)
      (define exist?
        (lambda (items)
          (if (null? items)
              #f
              (if (list? (car items))
                  #t
                  (exist? (cdr items))))))      
      (define for-each0
        (lambda (proc items)
          (if (not (null? items))
              ((lambda ()
                 (proc (car items))
                 (for-each0 proc (cdr items)))))))
      (define for-each1
        (lambda (proc items)
          (if (not (member '() items))
              ((lambda ()
                 (apply proc (map car items))
                 (for-each1 proc (map cdr items)))))))
      (if (null? args)
          (if (exist? (list items))
              (for-each0 proc items)
              (eror "(for-each wrong type argument -- " (list proc items)))
          (if (exist? (cons items args))
              (for-each1 proc (cons items args))
              (error "(for-each) wrong type argument -- "
                     (cons proc (cons items args)))))))

  (define string-for-each
    (lambda (proc str . args)
      (apply for-each proc (map string->list (cons str args)))))

  (define vector-map
    (lambda (proc vect . args)
      (apply for-each proc (map vector->list (cons vect args)))))

  ;; (define values
  ;;   (lambda things (call-with-current-continuation
  ;;                   (lambda (cont) (apply cont things)))))
  (define values
    ((lambda ()
       (define c/c call-with-current-continuation)
       (lambda things (c/c (lambda (cont) (apply cont things)))))))

  (define call/cc call-with-current-continuation)
  (define dynamic-wind #f)
  ((lambda ()
     (define wind '())
     (define common-list-tail
       (lambda (list0 list1)
         ((lambda (length0 length1)
            (if (= length0 length1)
                (if (eq? list0 list1)
                    list0
                    (common-list-tail (cdr list0) (cdr list1)))
                (if (> length0 length1)
                    (common-list-tail (cdr list0) list1)
                    (common-list-tail list0 (cdr list1)))))
          (length list0) (length list1))))
     (define unwind
       (lambda (wind0)
         ((lambda (tail)
            (define after
              (lambda (w)
                (if (eq? w tail)
                    (set! wind tail)
                    ((lambda ()
                       ((cdar w))
                       (after (cdr w)))))))
            (define before
              (lambda (w)
                (if (eq? w tail)
                    (set! wind wind0)
                    ((lambda ()                     
                       (before (cdr w))
                       ((caar w)))))))
            (after wind)
            (before wind0))
          (common-list-tail wind wind0))))
     (set! call/cc
           ((lambda (c/c)
              (lambda (proc)
                (c/c (values (lambda (cont)
                               (proc ((lambda (wind0)
                                        (lambda (obj)
                                          (if (not (eq? wind0 wind))
                                              (unwind wind0))
                                          (cont obj)))
                                      wind)))))))
            ;; (lambda (proc)
            ;;   (c/c (lambda (cont)
            ;;          (proc ((lambda (wind0)
            ;;                   (lambda (obj)
            ;;                     (if (not (eq? wind0 wind))
            ;;                         (unwind wind0))
            ;;                     (cont obj)))
            ;;                 wind))))))
            call/cc))
     (set! call-with-current-continuation call/cc)
     (set! dynamic-wind
           (lambda (before thunk after)
             (before)
             (set! wind (cons (cons before after) wind))
             ((lambda (result)
                (set! wind (cdr wind))
                (after)
                result)
              (thunk))))))

  ;; Control features end
  ;; Exceptions
  (define with-exception-handler
    (lambda (handler thunk)
      (define raise-primitive raise)
      (define raise-continuable-primitive raise-continuable)
      (dynamic-wind
          (lambda ()
            (set! raise
                  (lambda (obj)
                    (call/cc
                     (lambda (c)
                       (c (handler obj))))))
            (set! raise-continuable
                  (lambda (obj)
                    (call/cc
                     (lambda (c)
                       (c (handler obj))))))
            (set! error
                  (lambda (msg . args)
                    (raise
                     (apply error-implementation-defined-boejct msg args)))))
          thunk
          (lambda ()
            (set! raise raise-primitive)
            (set! raise-continuable raise-continuable-primitive)))))

  (define raise-continuable
    (lambda (obj)
      (display "Exception: ")
      (write obj)
      (newline)
      obj))
  (define error
    (lambda (message . args)
      (raise (apply error-implementation-defined-object message args))))
  ;; Exceptions end

  ;; Environments and evaluation
  ;; Environments and evaluation end

  ;; Input and output
  (define call-with-port
    (lambda (proc port)
      (define v (proc port))
      (close-port port)
      v))

  (define call-with-input-file
    (lambda (str proc)
      (call-with-port proc (open-input-port str))))
  (define call-with-output-file
    (lambda (str proc)
      (call-with-port proc (open-output-port str))))

  (define with-input-from-file
    (lambda (s thunk)
      (if (and (string? s) (procedure? thunk))
          ((lambda ()
             (define port (open-input-file s))
             (define primitive-read read)
             (define primitive-read-char read-char)
             (define primitive-peek-char peek-char)
             (define primitive-read-line read-line)
             (define primitive-char-ready? char-ready?)
             (define primitive-read-string read-string)
             (define primitive-read-u8 read-u8)
             (define primitive-peek-u8 peek-u8)
             (define primitive-u8-ready? u8-ready?)
             (define primitive-read-bytevector read-bytevector)
             (define primitive-read-bytevector! read-bytevector!)
             (set! read
                   (lambda args
                     (primitive-read (if (null? args) port (car args)))))
             (set! read-char
                   (lambda args
                     (primitive-reac-char (if (null? args) port (car args)))))
             (set! peek-char
                   (lambda args
                     (primitive-peek-char (if (null? args) port (car args)))))
             (set! read-line
                   (lambda args
                     (primitive-read-line (if (null? args) port (car args)))))
             (set! char-ready?
                   (lambda args
                     (primitive-char-ready (if (null? args) port (car args)))))
             (set! read-string
                   (lambda (k . args)
                     (primitive-read-string k (if (null? args)
                                                  port
                                                  (car args)))))
             (set! read-u8
                   (lambda args
                     (primitive-read-u8 (if (null? args) port (car args)))))
             (set! peek-u8
                   (lambda args
                     (primitive-peek-u8 (if (null? args) port (car args)))))
             (set! u8-ready?
                   (lambda args
                     (primitive-u8-read? (if (null? args) port (car args)))))
             (set! read-bytevector
                   (lambda (k . args)
                     (primitive-read k (if (null? args) port (car args)))))
             (set! read-bytevector!
                   (lambda (bytevector . args)
                     (define len (length args))
                     (define p (if (= 0 len) port (car args)))
                     (define start (if (< len 2) 0 (cadr args)))
                     (define end (if (< len 3)
                                     (bytevector-length bytevector)
                                     (caddr args)))
                     (primitive-read bytevector bytevector p start end)))
             (dynamic-wind
                 (lambda () 'before)
                 thunk
                 (lambda ()
                   (close-port port)
                   (set! read primitive-read)
                   (set! read-char primitive-read-char)
                   (set! peek-char primitive-peek-char)
                   (set! read-line primitive-read-line)
                   (set! char-ready? primitive-char-ready?)
                   (set! read-string primitive-read-string)
                   (set! read-u8 primitive-read-u8)
                   (set! peek-u8 primitive-peek-u8)
                   (set! u8-ready? primitive-u8-ready?)
                   (set! read-bytevector primitive-read-bytevector)
                   (set! read-bytevector! primitive-read-bytevector!)))))
          (error "(with-input-from-file) wrong type argument -- "
                 (list s thunk)))))

  (define with-output-from-file
    (lambda (s thunk)
      (if (and (string? s) (procedure? thunk))
          ((lambda ()
             (define port (open-output-file s))
             (define primitive-write write)
             (define primitive-write-shared write-shared)
             (define primitive-write-simple write-simple)
             (define primitive-display display)
             (define primitive-newline newline)
             (define primitive-write-char write-char)
             (define primitive-write-string write-string)
             (define primitive-write-u8 write-u8)
             (define primitive-write-bytevector write-bytevector)
             (define primitive-flush-output-port flush-output-port)
             (set! write
                   (lambda args
                     (primitive-write (if (null? args) port (car args)))))
             (set! write-shared
                   (lambda args
                     (primitive-write-shared (if (null? args) port
                                                 (car args)))))
             (set! write-simple
                   (lambda args
                     (primitive-write-simple (if (null? args)
                                                 port
                                                 (car args)))))
             (set! display
                   (lambda args
                     (primitive-display (if (null? args)
                                            port
                                            (car args)))))
             (set! newline
                   (lambda args
                     (primitive-newline (if (null? args)
                                            port
                                            (car args)))))
             (set! write-char
                   (lambda (ch  . args)
                     (primitive-write-char ch (if (null? args)
                                                  port
                                                  (car args)))))
             (set! write-string
                   (lambda args
                     (primitive-write-string (if (null? args)
                                                 port
                                                 (car args)))))
             (set! write-u8
                   (lambda (s . args)
                     (define len (length args))
                     (primitive-write-u8 (if (= len 0) port (car args))
                                         (if (< len 2) 0 (cadr args))
                                         (if (< len 3)
                                             (string-length s)
                                             (caddr args)))))
             (set! write-bytevector
                   (lambda (bv . args)
                     (define len (length args))
                     (primitive-write-bytevector bv
                                                 (if (= len 0) port (car args))
                                                 (if (< len 2) 0 (cadr args))
                                                 (if (< len 3)
                                                     (bytevector-length s)
                                                     (caddr args)))))
             (set! flush-output-port
                   (lambda args
                     (primitive-flush-output-port (if (null? args)
                                                      port
                                                      (car args)))))
             (dynamic-wind
                 (lambda () 'before)
                 thunk
                 (lambda ()
                   (close-port port)
                   (set! write primitive-write)
                   (set! write-shared primitive-write-shared)
                   (set! write-simple primitive-write-simple)
                   (set! display primitive-display)
                   (set! newline primitive-newline)
                   (set! write-char primitive-write-char)
                   (set! write-string primitive-write-string)
                   (set! write-u8 primitive-write-u8)
                   (set! write-bytevector primitive-write-bytevector)
                   (set! flush-output-port primitive-flush-output-port)))))
          (error "(with-output-from-file) wrong type argument -- "
                 (list s thunk)))))
  
  (define close-input-port
    (lambda (port)
      (if (input-port? port)
          (close-port port)
          (error "(close-input-port) wrong type argument -- " (list port)))))
  (define close-output-port
    (lambda (port)
      (if (output-port? port)
          (close-port port)
          (error "(close-output-port) wrong type argument -- " (list port)))))
  (define read-line
    (lambda args
      (define iter
        (lambda (result port)
          ((lambda (char)
             (if (eof-object? char)
                 (if (null? result)
                     (eof-object)
                     (list->string (reverse result)))
                 (if (eq? char #\newline)
                     (list->string (reverse result))
                     (if (eq? char #\return)
                         (if (eq? (peek-char port) #\newline)
                             ((lambda ()
                                (read-char port)
                                (list->string (reverse result))))
                             (list->string (reverse result)))
                         (iter (cons char result) port)))))
           (read-char port))))
      (iter '() (if (null? args)
                    (current-input-port)
                    (car args)))))

  (define newline
    (lambda args
      (if (null? args)
          (display #\newline)
          (display #\newline (car args)))))

  (define write-char
    (lambda (char . args)
      (if (null? args)
          (display char)
          (display char (car args)))))
  (define write-string
    (lambda (s . args)
      (if (null? args)
          (display s)
          ((lambda ()
             (define port (car args))
             (define start (if (null? (cdr args))
                               0
                               (cadr args)))
             (define end (if (null? (cdr args))
                             (string-length s)
                             (if (null? (cddr args))
                                 (string-length s)
                                 (caddr args))))
             (display (string-copy port)))))))

  ;; Input and output end

  ;; System interface
  (define delete-file
    (lambda (filename)
      (if (string? filename)
          ((lambda ()
             (define obj (primitive-delete-file filename))
             (if (file-error? obj)
                 (raise obj)))))))

  (define exit
    (lambda args
      (dynamic-wind
          (lambda () #f)
          (lambda ()
            (if (null? args)
                (emergency-exit)
                (if (null? (cdr args))
                    (emergency-exit (car args))
                    #f)))
          (lambda () #f))))
  ;; System interface end

  ;; Derived expression types
  (define make-parameter
    (lambda (init . o)
      (define converter (if (pair? o) (car o) (lambda (x) x)))
      (define value (converter init))
      (lambda args
        (if (null? args)
            value
            (if (eq? (car args) <param-set!>)
                (set! value (cadr args))
                (if (eq? (car args) <param-convert>)
                    converter
                    (error "bad parameter syntax")))))))
  )
