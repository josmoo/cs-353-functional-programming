#lang racket

;;csv stuff
(require csv-reading)

(define csv-reader
  (make-csv-reader-maker
   '((separator-chars            ",")
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
(define sales-lstlst
  (csv->list
    (csv-reader
      (open-input-file "Video Games Sales.csv"))))


;;sort by review instead of ranking
(define (end-search lstlst)
  (let*
    (
     [display-options (displayln "Order by:\n1: Ranking\n2: Review")]
     [user-input (read-line (current-input-port) 'any)]
    )
    (if (equal? user-input "1")
        lstlst
        (sort-review lstlst))
  )
)

(define (sort-review lstlst)
  (sort lstlst
    (λ (x y) (string>? (last x) (last y)))))


;;search by year range
(define (filter-range lstlst)
  (let*
    (
     [display-options (displayln "Enter the lower bound:\n")]
     [lower-bound (read-line (current-input-port) 'any)]
     [display-options (displayln "Enter the upper bound:\n")]
     [upper-bound (read-line (current-input-port) 'any)]
    )
    (filter-range/sort (string->number lower-bound) (string->number upper-bound) lstlst)
  )
)

(define (filter-range/sort lowbound upbound lstlst)
  (if (>= upbound lowbound)
      (filter-range/query lowbound upbound lstlst)
      (filter-range/query upbound lowbound
        (sort lstlst (λ (x y) (string>? (fifth x) (fifth y)))))
  )
)

(define (filter-range/query lowbound upbound lstlst);uses rest to remove column label
  (filter (λ (x) (member (string->number (fifth x)) (range lowbound (add1 upbound)))) (rest lstlst)))


;;region logic
(define (filter-region lstlst)
  (let*
    (
     [display-options (displayln "Enter the region:\n1: North America\n2: Europe\n3: Japan\n4: Rest\n5: Global")]
     [user-input (read-line (current-input-port) 'any)]
    )
    (cond
      [(equal? user-input "1")
        (na-region lstlst)]
      [(equal? user-input "2")
        (eu-region lstlst)]
      [(equal? user-input "3")
        (jpn-region lstlst)]
      [(equal? user-input "4")
        (rest-region lstlst)]
      [(equal? user-input "5")
        (global-region lstlst)]
      [else "please don't make me error check this i don't have time to sorry. thank you :)"]
    )
  )
)

(define (na-region lstlst)
  (map (λ(x)(append (take x 8) (drop x 12))) lstlst))

(define (eu-region lstlst)
  (map (λ (x)(append (take x 7) (list (list-ref x 8) (list-ref x 12))))lstlst))

(define (jpn-region lstlst)
  (map (λ (x)(append (take x 7) (list (list-ref x 9) (list-ref x 12)))) lstlst))

(define (rest-region lstlst)
  (map (λ (x) (append (take x 7) (list (list-ref x 10) (list-ref x 12)))) lstlst))

(define (global-region lstlst)
  (map (λ (x) (append (take x 7) (drop x 11))) lstlst))


;;search logic
;;(the use of rest is to remove first row (column labels))
(define (search function lstlst)
  (function lstlst (get-target)))

(define (get-target)
  (let*
    (
     [display-options (displayln "Enter the search target:")]
    )
    (read-line (current-input-port) 'any)
  )
)

(define (search-titles lstlst target)
  (filter (λ(x) (string-contains? (string-downcase (third x)) (string-downcase target))) (rest lstlst)))

(define (search-genres lstlst target)
  (filter (λ(x) (string-contains? (string-downcase (sixth x)) (string-downcase target))) (rest lstlst)))

(define (search-publishers lstlst target)
  (filter (λ(x) (string-contains? (string-downcase (seventh x)) (string-downcase target))) (rest lstlst)))


;;primary user input functions begin
(define (user-input options-taken lstlst)
  (let*
    (
     [continue (display-available-options options-taken)]
     [input (when continue (read-line (current-input-port) 'any))]
    )
    (cond
      [(or (equal? input "0") (> (length options-taken) 2))
        (end-search lstlst)]
      [(equal? input "1")
        (user-input (append options-taken (list input)) (search search-titles lstlst))]
      [(equal? input "2")
        (user-input (append options-taken (list input)) (filter-range lstlst))]
      [(equal? input "3")
        (user-input (append options-taken (list input)) (search search-publishers lstlst))]
      [(equal? input "4")
        (user-input (append options-taken (list input)) (filter-region lstlst))]
      [(equal? input "5")
        (user-input (append options-taken (list input)) (search search-genres lstlst))]
    )
  )
)

(define (display-available-options options-taken)
  (if (or (member "0" options-taken) (> (length options-taken) 2)) ;if user wants to quit or has chosen 3 options
      #f
      (display-available-options/1 options-taken "Search options:\n"))) ;otherwise print menu

(define (display-available-options/1 options-taken display-string)
  (if (member "1" options-taken) ;if this option has already been applied
      (display-available-options/2 options-taken display-string) ;then ignore it
      (display-available-options/2 options-taken (string-append display-string "1: Title\n"))));otherwise print it (later)

(define (display-available-options/2 options-taken display-string)
  (if (member "2" options-taken) 
      (display-available-options/3 options-taken display-string) 
      (display-available-options/3 options-taken (string-append display-string "2: Date range\n"))))

(define (display-available-options/3 options-taken display-string)
  (if (member "3" options-taken) 
      (display-available-options/4 options-taken display-string) 
      (display-available-options/4 options-taken (string-append display-string "3: Publisher\n"))))

(define (display-available-options/4 options-taken display-string)
  (if (member "4" options-taken) 
      (display-available-options/5 options-taken display-string) 
      (display-available-options/5 options-taken (string-append display-string "4: Region\n"))))

(define (display-available-options/5 options-taken display-string)
  (if (member "5" options-taken) 
      (display-available-options/0 display-string) 
      (display-available-options/0 (string-append display-string "5: Genre\n"))))

(define (display-available-options/0 display-string)
  (displayln (string-append display-string "0: Display results")))

(define (loop)
  (main)
  (let*
    (
     [display-options (displayln "\nAgain?\n1: Yes\n2: No\n")]
     [user-input (read-line (current-input-port) 'any)]
    )
    (when (equal? user-input "1")
        (loop))
        
  )
)

(define (main)
  (map displayln (user-input '() sales-lstlst)))

(loop)