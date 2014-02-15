;;;;
;; sudoku.scm
;;  Solves Sudoku puzzles
;;
;; Author: Nicholas Russell
;;
;; ------------------------------------
;;
;; Based on [this write-up by Peter Norvig](http://norvig.com/sudoku.html).
;; This is a pretty straight-forward translation of Peter's Python code into
;; Scheme. I tried to keep variable and function names as similar as possible
;; to the Python names. I wanted to use symbols for the hash keys but digits
;; by themselves are not valid symbols so in the interest of keeping the code
;; somewhat similar to the original, I decided to use strings for the keys.
;;
;; ------------------------------------
;;

;; Takes a list of nested lists and turns it into a flat list
(define (flatten lst)
  (if (null? lst)
    '()
    (if (list? (car lst))
      (append (flatten (car lst)) (flatten (cdr lst)))
      (cons (car lst) (flatten (cdr lst))))))

;; for funsies
;(define (flatten lst)
;  (((lambda (func)
;      ((lambda (f)
;        (f f))
;       (lambda (f)
;        (func (lambda (x) ((f f) x))))))
;    (lambda (f)
;      (lambda (x)
;        (if (null? x)
;          '()
;          (if (list? (car x))
;            (append (f (car x)) (f (cdr x)))
;            (cons (car x) (f (cdr x)))))))) lst))

;; Cross product of elements in A and elements in B
(define (cross a b)
  (append-map (lambda (a) (map (lambda (b) (string-append a b)) b)) a))

(define digits (list "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(define rows (list "a" "b" "c" "d" "e" "f" "g" "h" "i"))
(define cols digits)
(define squares (cross rows cols))

(define unit-c (map (lambda (c) (cross rows (list c))) cols))
(define unit-r (map (lambda (r) (cross (list r) cols)) rows))
(define unit-s
  (append-map (lambda (rs)
      (map (lambda (cs)
          (cross rs cs))
        (list (sublist cols 0 3) (sublist cols 3 6) (sublist cols 6 9))))
    (list (sublist rows 0 3) (sublist rows 3 6) (sublist rows 6 9))))
(define unit-list (append unit-c unit-r unit-s))

(define units (make-string-hash-table))
(for-each
  (lambda (s)
    (let ((u (filter (lambda (u) (member s u)) unit-list)))
      (hash-table/put! units s u)))
  squares)

(define peers (make-string-hash-table))
(for-each
  (lambda (s)
    (hash-table/put!
      peers
      s
      (lset-difference equal? (delete-duplicates (flatten (hash-table/get units s '()))) (list s))))
  squares)

;;;;;;;; Parse a Grid

;; Convert grid to a hash of possible values, (square digits), or return
;; #f is a contradiction is detected.
(define (parse-grid grid)
  ; to start, every square can be any digit; then assign values for the grid
  (let ((values
          (let ((values-hash (make-string-hash-table)))
            (for-each
              (lambda (s) (hash-table/put! values-hash s digits))
              squares)
            values-hash))
        (grid-vals (grid-values grid)))
      (let loop ((pair (hash-table->alist grid-vals)))
        (if (null? pair)
          values
            (if (and (member (cdar pair) digits) (not (assign values (caar pair) (cdar pair))))
              #f ; Fail if we can't assign d (cdar pair) to square s (caar pair)
              (loop (cdr pair)))))))

;; Convert grid into a hash (square char) with '0' or '.' for empties
(define (grid-values grid)
  (let ((chars
          (filter (lambda (c) (or (member c digits) (member c (list "0" ".")))) grid)))
    (if (or (not (= (length chars) 81)) (not (= (length chars) (length squares))))
      (error "grid values are invalid"))
    (let ((sq-ch-hash (make-string-hash-table)))
      (for-each 
        (lambda (p) (hash-table/put! sq-ch-hash (car p) (cadr p)))
        (zip squares chars))
      sq-ch-hash)))

;;;;;;;; Constraint Propagation

;; Eliminate all the other values (except d) from values for key s and propagate
;; Return values, except return #f if a contradiction is detected
(define (assign values s d)
  (let ((other-values (remove (lambda (x) (equal? x d)) (hash-table/get values s '()))))
    (let loop ((d2 other-values))
      (if (null? d2)
        values
        (if (eliminate values s (car d2))
          (loop (cdr d2))
          #f)))))
; couldn't use this because Python's all() is short-circuiting
; (reduce-left (lambda (a b) (and a b)) '() (map (lambda (d2) (eliminate values s d2)) other-values))

;; Eliminate d from values for key s; propagate when values or places <= 2
;; Return values, except return #f if a contradiction is detected
(define (eliminate values s d)
  (if (not (member d (hash-table/get values s '())))
    values ; already eliminated
    (begin
      (hash-table/put! values s (remove (lambda (x) (equal? x d)) (hash-table/get values s '())))
      ; (1) If a square s is reduced to one value d2, then eliminate d2 from the peers
      (let ((case-one-return
        (if (= (length (hash-table/get values s '())) 0)
          #f ; Contradiction: removed last value
          (if (= (length (hash-table/get values s '())) 1)
            (let ((d2 (car (hash-table/get values s '()))))
              (let eliminate-loop ((s2 (hash-table/get peers s '())))
                (if (null? s2)
                  #t
                  (if (eliminate values (car s2) d2)
                    (eliminate-loop (cdr s2))
                    #f))))
            #t))))
        ; (2) If a unit u is reduced to only one place for a value d, then put it there
        (if (not case-one-return)
          #f
          (let ((unit-loop-return
                  (let unit-loop ((u (hash-table/get units s '())))
                    (if (null? u)
                      #t
                      (if (let ((dplaces
                                  (filter
                                    (lambda (s)
                                      (member d (hash-table/get values s '())))
                                    (car u))))
                            (if (= (length dplaces) 0)
                              #f ; Contradiction: no place for this value
                              (if (= (length dplaces) 1)
                                ; d can only be in one place in unit; assign it there
                                (if (not (assign values (car dplaces) d))
                                  #f
                                  #t)
                                #t)))
                        (unit-loop (cdr u))
                        #f)))))
            (if unit-loop-return
              values
              #f)))))))

;;;;;;;; Display as 2-D grid

;; Display these values as a 2-D grid
(define (display-grid values)
  (let* ((cell-values 
          (map
            (lambda (s)
              (let ((val (hash-table/get values s (list "."))))
                (list->string
                  (if (list? val)
                    (map name->char (map (lambda (x) (if (member x digits) x ".")) val))
                    (list (name->char (if (member val digits) val ".")))))))
            squares))
         (max-length (apply max (map string-length cell-values)))
         (cell-padding (lambda (cell) (make-string (- max-length (string-length cell)) #\space)))
         (make-display-cell (lambda (cell) (string-append (cell-padding cell) cell)))
         (row-separator
          (let ((under (make-string (+ (* max-length 3) 4) #\-)))
            (string-append under "+" under "+" under)))
         (disp-rows
          (map
            (lambda (i)
              (map make-display-cell (sublist cell-values (* i 9) (* (1+ i) 9))))
            (iota (length rows)))))

    (set! disp-rows
      (let row-loop ((r disp-rows))
        (if (null? r)
          '()
          (cons
            (let col-loop ((c (car r))
                           (count 0))
              (if (null? c)
                '()
                (if (and (> count 0) (< count 8) (= (remainder (1+ count) 3) 0))
                  (cons (car c) (cons "|" (col-loop (cdr c) (1+ count))))
                  (cons (car c) (col-loop (cdr c) (1+ count))))))
            (row-loop (cdr r))))))
    
    (define count 0)
    (for-each
      (lambda (r)
        (display " ")
        (display (apply string-append (map (lambda (x) (string-append x " ")) r)))
        (newline)
        (if (and (> count 0) (< count 8) (= (remainder (1+ count) 3) 0))
          (begin (display row-separator) (newline)))
        (set! count (1+ count)))
      disp-rows)))

;;;;;;;; Search

;; Solve a puzzle
(define (solve grid)
  (search (parse-grid grid)))

;; Using depth-first search and propagation, try all possible values
(define (search values)
  (if (false? values)
    #f ; failed earlier
    (if (let solved-loop ((s squares))
          (if (null? s)
            #t
            (if (= (length (hash-table/get values (car s) '())) 1)
              (solved-loop (cdr s))
              #f)))
      values ; solved
      ; Chose the unfilled square s with the fewest possibilities
      (let ((ns-pair
              (let ns-pair-loop ((s squares)
                                 (ns (cons 10 ".")))
                (if (null? s)
                  ns
                  (let ((len (length (hash-table/get values (car s) '()))))
                    (if (and (> len 1) (< len (car ns)))
                      (ns-pair-loop (cdr s) (cons len (car s)))
                      (ns-pair-loop (cdr s) ns)))))))
        (let some-loop ((d (hash-table/get values (cdr ns-pair) '())))
          (if (null? d)
            #f
            (let ((search-ret (search (assign (hash-table/copy values) (cdr ns-pair) (car d)))))
              (if search-ret
                search-ret
                (some-loop (cdr d))))))))))

;;;;;;;; Utilities

;; Makes a copy of a hash-table
(define (hash-table/copy hash-table)
  (define new-hash-table (make-string-hash-table))
  (let loop ((key-pairs (hash-table->alist hash-table)))
    (if (null? key-pairs)
      new-hash-table
      (begin
        (hash-table/put! new-hash-table (caar key-pairs) (cdar key-pairs))
        (loop (cdr key-pairs))))))

;; Converts a grid string to a list of strings of the individual characters
(define (grid-str-to-list grid)
  (map (lambda (c) (char->name c)) (string->list grid)))

;;;;;;;; Test Grids

(define grid1 (grid-str-to-list "003020600900305001001806400008102900700000008006708200002609500800203009005010300"))
(define grid2 (grid-str-to-list "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"))
(define hard1 (grid-str-to-list ".....6....59.....82....8....45........3........6..3.54...325..6.................."))

'sudoku-solver
