#lang racket

;"remove" team name from game-data and "update" team-data with the team name
;returns pair of updated team-data-lyst and rest of game-data-lyst
(define (get-team-name team-data-lyst game-data-lyst)
    (list (list-set team-data-lyst 0 (first game-data-lyst)) (rest game-data-lyst)))

;"remove" player name from game-data and "update" team data 
(define (get-player-name lyst)
    (list (list->string '((first lyst)(second lyst))) (rest lyst)))

;return the integer only if the character is a number
(define (string-to-int char)
    (if (string->number char 10 'number-or-false)
        (string->number char)
        char
    )
)

;@param data       A team-data, game-data pair
;@param round-num  How many rounds have been added to the team. 5 players, 3 rounds each means 15 rounds (hard coded)
;@returns          a team-data, game-data pair. team-data is updated, and game-data starts where left off. (at the start of next team)
(define (add-teams-rounds data round-num)
    (let*
        ([GAME_DATA_LIST (string-split (first (second data)))]
         [TEAM_DATA_LIST (first data)]
         [PLAYER_NAME (string-join (list (first GAME_DATA_LIST) (second GAME_DATA_LIST)))]
         [PLAYER_SCORE (check-frame (map string-to-int (rest (rest GAME_DATA_LIST))) 0 1)]
         [ADD_ROUND (list (list-set
              (list-set TEAM_DATA_LIST 1 (+ (second TEAM_DATA_LIST) PLAYER_SCORE)) ;add player score to team total
              2 (update-record (third TEAM_DATA_LIST) PLAYER_NAME PLAYER_SCORE)) ;add the match results 
          (rest (second data)))] ;and return as a team-data, game-data pair
        )
      (if (< round-num 14)
          (add-teams-rounds ADD_ROUND (add1 round-num))
          ADD_ROUND
      )
    ) 
)

;returns the new team score record given the team record, name and new score
(define (update-record hsh name score)
    (if (hash-has-key? hsh name) ;if key exists
        (hash-set hsh name (append (hash-ref hsh name) (list score))) ;just add the new score
        (hash-set hsh name (list score)) ;otherwise make it
    )
)

(define (is-strike char)
    (equal? "X" char))

(define (is-spare lyst)
  (equal? "/" (second lyst)))

;behold the skyscraping pyramid of death, doom, fear, and loathing
(define (evaluate-frame lyst)
  
  (if (is-spare lyst) ;if current is spare
      (if (is-strike (third lyst)) ;and next is strike
          20
          (+ 10 (third lyst))
      )
      (if (is-strike (first lyst)) ;elif current is strike
          (if (is-strike (second lyst)) ;second is also strike
              (if (is-strike (third lyst)) ;and third is strike
                  30
                  (+ 20 (third lyst))
              )
              (if (is-spare (rest lyst)) ;is next is a spare
                  20
                  (+ 10 (second lyst) (third lyst))
              )
          )
          (+ (first lyst) (second lyst);else current is open frame
          ) 
      )
   )
)

;todo i dont think this is tail end recursion
(define (check-frame lyst sum frame-num)
  
    (if (or (empty? lyst) (> frame-num 10))
        sum 
        (if (is-strike (first lyst)) ;if the current frame is a strike
            (check-frame (rest lyst) (+ sum (evaluate-frame lyst)) (add1 frame-num)) ;#t => evaluate it and recurse
            (check-frame (rest (rest lyst)) (+ sum (evaluate-frame lyst)) (add1 frame-num)) ;#f => evaluate it and recurse
        )
    )
)

(define (print-winner team-1-data team-2-data)
    (define (find-winning-team)
        (cond [(> (second team-1-data) (second team-2-data)) (first team-1-data)]
              [(= (second team-1-data) (second team-2-data)) "It's a tie!"]
              [else (first team-2-data)]
        )
    )
    (display ( list
        "The winning team is: "
        (find-winning-team)
        "\n\n"
    ))
)

(define (print-team-info team-data)
    (display (list
        "Team " (first team-data) ", total score: " (second team-data) "\n"
    ))
)

(define (print-player-info hsh pos)
    (define (key) (hash-iterate-key hsh pos))
    (define (value) (hash-ref hsh (key)))
    (display (list
        "Player:" (key)
        ", games: " (value)
        ", total score: " (foldl + 0 (value))
        "\n"
    ))
    (if (hash-iterate-next hsh pos)
        (print-player-info hsh (add1 pos))
        (display "\n\n")
    )
)

(define (get-player-total hsh name)
    (define (player-scores)(hash-ref hsh name))
    (+ (first player-scores)(second player-scores)(third player-scores))
)

;@returns difference in hsh1 @ pos1 total score to hsh2 @ pos2 total score
(define (compare-scores hsh1 pos1 hsh2 pos2)
    (- (foldl + 0 (hash-iterate-value hsh1 pos1)) (foldl + 0 (hash-iterate-value hsh2 pos2)))
)

;@param hsh                The hashtable to find the highest score of
;@param pos                The current position in the hashtable
;@param hiscore-poss-lyst  The list of position(s) for the highest score.
;@param continue           Boolean flag. True if we haven't finished, false once we've reached the end
;@returns    hiscore-poss-lyst
(define (find-highest-scoring-player hsh pos hiscore-poss-lyst continue)
    (define (recurse lyst)
        (find-highest-scoring-player hsh (add1 pos) lyst (hash-iterate-next hsh pos)))
    (define (compare-val)
      (compare-scores hsh pos hsh (first hiscore-poss-lyst)))
    
    (if continue
        (cond 
            [(> (compare-val) 0)
                (recurse (list pos))]
            [(< (compare-val) 0)
                (recurse hiscore-poss-lyst)]
            [else (recurse (append (list pos) hiscore-poss-lyst))]
        )
        hiscore-poss-lyst
    )
)

(define (print-highest-scoring-player hsh1 hsh2)
    (display "Highest scoring player(s):")
    (let*
        (
        [POSS_LYST1 (find-highest-scoring-player hsh1 0 (list 0) #t)]
        [POSS_LYST2 (find-highest-scoring-player hsh2 0 (list 0) #t)]
        [SCORE_DIFFERENCE (compare-scores hsh1 (first POSS_LYST1) hsh2 (first POSS_LYST2))]
        )
    
        (cond
            [(> SCORE_DIFFERENCE 0)
                 (print-players hsh1 POSS_LYST1)]
            [(< SCORE_DIFFERENCE 0)
                 (print-players hsh2 POSS_LYST2)]
            [(= SCORE_DIFFERENCE 0)
                 (print-players hsh1 POSS_LYST1)
                 (print-players hsh2 POSS_LYST2)]
        )
    )
)

(define (print-players hsh poslyst)
    (define (key) (hash-iterate-key hsh (first poslyst)))
    (define (value) (hash-ref hsh (key)))
    (display (list (key) (foldl + 0 (value))))
    (when (not (null? (rest poslyst)))
        (print-players hsh poslyst))
)

(let*
    (
     [TEAM_1_DATA_GAME_DATA_PAIR (add-teams-rounds(get-team-name (list "name" 0 (hash)) (port->lines (open-input-file "scores.txt"))) 0)] 
     [TEAM_2_DATA_GAME_DATA_PAIR (add-teams-rounds(get-team-name (list "name" 0 (hash)) (second  TEAM_1_DATA_GAME_DATA_PAIR))0)]
     [TEAM_1 (first TEAM_1_DATA_GAME_DATA_PAIR)]
     [TEAM_2 (first TEAM_2_DATA_GAME_DATA_PAIR)]
    )
  (print-team-info TEAM_1)
  (print-player-info (third TEAM_1) 0)
  (print-team-info TEAM_2)
  (print-player-info (third TEAM_2) 0)
  (print-winner TEAM_1 TEAM_2)
  (print-highest-scoring-player (third TEAM_1) (third TEAM_2))
)
