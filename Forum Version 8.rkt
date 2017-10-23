;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment8A-Forum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

(define scene (empty-scene 800 800))

; A History is a List of Strings
; INTERPRETATION: the prior posts received from the server.
;A history is one of:
; - '()
; - (cons String History)                                 
(define history1 (cons "hello" (cons "jerry" (cons "I love fundies" (cons "sometimes" '())))))

;History-temp
#;(define (history-temp h)
    (cond
      [(empty? h)...]
      [(cons? h)...(first h)...(history-temp rest h)...]))
 
; A Edit is a String
; INTERPRETATION: the contents of the post the user is currently
; editing.
(define edit1 "editing")

; A Search is a String 
; INTERPRETATION: the current search term the user is looking for
(define search1 "hel")
(define search2 "")
(define search3 "i")

(define-struct editview [edit history search])
; A EditviewPosts is a (make-editview Edit History Search)
; INTERPRETATION: Means the user is viewing all posts and
; potentially typing in a new one.
(define editview1 (make-editview edit1 history1 search1))

;editview-temp
#;(define (editview e)
    (...(editview-edit e)...(editview-history e)...(editview-search e)...))
                     
(define-struct searchpost [edit history search])
; A SearchPosts   is a (make-searchpost Edit History Search)
; INTERPRETATION: Means the user is trying to view only a subset
; of the existing messages.
(define searchpost1 (make-searchpost edit1 history1 search1))
(define searchpost2 (make-searchpost edit1 history1 search2))
(define searchpost3 (make-searchpost edit1 history1 search3))

;SearchPost-temp
#;(define (searchpost-temp s)
    (...(searchh-edit s)...(search-history s)...(search-search s)))


; A World is one of
; - EditviewPosts
; - SearchPosts
; INTERPRETATION: Represents two different "views" in your program.
(define world0 editview1)
(define world1 searchpost1)

;World-temp
#;(define (world-temp ws)
    (cond
      [(editview? ws)...(editview-edit ws)...(editview-history ws)...(editview-search ws)...]
      [(searchpost? ws)...(search-edit ws)...(search-history ws)...(search-search ws)]...))

; A SentWorld is one of
; - a World
; - a (make-package World Edit)
(define Sentworld1 (make-package world0 (editview-edit world0)))

;;simple-forum
;World -> World
;Creates a chat client that allows a user to type and submit message
;and also displays past messages as images
(define (simple-forum world)
  (big-bang world
            [name "ahluwalia.pr:0893"]
            [register "dictionary.ccs.neu.edu"]
            [port 10003]
            [on-key select-world]
            [to-draw render]
            [on-receive recieve-world]))

;select-world
;World KeyEvent -> World
;selects the world state and passes the world to the (change-world) or (type-world) functions
(define (select-world ws k)
  (cond
    [(editview? ws) (if (string=? k "f2")
                        (change-world ws) (type-world ws k))]
      
    [(searchpost? ws) (if (string=? k "f1") (change-world ws) (type-world ws k))]))

(check-expect (select-world editview1 "f2") (make-searchpost edit1 history1 edit1))
(check-expect (select-world searchpost1 "f1") (make-editview search1 history1 search1))
(check-expect (select-world editview1 "l") (type-world editview1 "l"))
(check-expect (select-world searchpost1 "l") (type-world searchpost1 "l"))

;;change-world
;;Changes the state of a world from Editview to SearchView and vice versa
;World -> World
(define (change-world ws)
  (cond
    [(editview? ws) (make-searchpost (editview-edit ws) (editview-history ws)
                                     (editview-edit ws))]
    [(searchpost? ws) (make-editview (searchpost-search ws)
                                     (searchpost-history ws) (searchpost-search ws))]))

(check-expect (change-world editview1) (make-searchpost edit1 history1 edit1))
(check-expect (change-world searchpost1) (make-editview search1 history1 search1))

;;type-World
;World KeyEvent -> SentWorld
;appends the typed character to the (editview-edit branch) or the (searchpost-search) branch
;In the case of "\b" or "\r" the (keyhandle ws key) function is called
(define (type-world ws k)
  (cond
    [(editview? ws) (if (or (key=? k "\b") (key=? k "\r"))
                        (keyhandle ws k)
                        (type-world-helper ws k))]
    [(searchpost? ws) (if (or (key=? k "\b") (key=? k "\r"))
                          (keyhandle ws k)
                          (type-world-helper ws k))]))
  

(check-expect (type-world (make-editview "hell" history1 search1) "o")
              (make-editview "hello" history1 search1))
(check-expect (type-world (make-searchpost edit1 history1 "hell") "o")
              (make-searchpost edit1 history1 "hello"))
(check-expect (type-world (make-editview "hello" history1 search1) "\b")
              (make-editview "hell" history1 search1))
(check-expect (type-world (make-searchpost edit1 history1 "hello") "\b")
              (make-searchpost edit1 history1 "hell"))
(check-expect (type-world (make-editview "hello" history1 search1) "\r")
              (make-package (make-editview "" history1 search1) "hello"))
(check-expect (type-world (make-editview "hello" history1 search1) "shift")
              (make-editview "hello" history1 search1))
(check-expect (type-world (make-searchpost edit1 history1 "hello") "shift")
              (make-searchpost edit1 history1 "hello"))
              

;;keyhandle
;World KeyEvent -> SentWorld
;handles the "/r" and "\b" keys from type-world. "\r" will call (send-world ws) "\b" calls
;(backspace ws)
(define (keyhandle ws key)
  (cond [(key=? "\b" key) (backspace ws)]
        [(key=? "\r" key) (if (editview? ws) (send-world ws) ws)]))

(check-expect (keyhandle (make-searchpost edit1 history1 "hello") "\b")
              (make-searchpost edit1 history1 "hell"))
(check-expect (keyhandle (make-editview "hello" history1 search1) "\r")
              (make-package (make-editview "" history1 search1) "hello"))

;type-world-helper
;Prevents "shift" "cntrl" and longer keyevents from being displayed
;World KeyEvent -> World
(define (type-world-helper ws k)
  (cond
    [(editview? ws) (if (= (string-length k) 1) (make-editview (string-append (editview-edit ws) k )
                                                               (editview-history ws)
                                                               (editview-search ws))
                        (make-editview (editview-edit ws)
                                       (editview-history ws) (editview-search ws)))]
    [(searchpost? ws) (if (= (string-length k) 1)
                          (make-searchpost (searchpost-edit ws)
                                           (searchpost-history ws)
                                           (string-append (searchpost-search ws) k))
                          (make-searchpost (searchpost-edit ws)
                                           (searchpost-history ws)
                                           (searchpost-search ws)))]))

(check-expect (type-world-helper (make-editview "hello" history1 search1) "shift")
              (make-editview "hello" history1 search1))
(check-expect (type-world-helper (make-searchpost edit1 history1 "hello") "shift")
              (make-searchpost edit1 history1 "hello"))
(check-expect (type-world-helper (make-editview "hell" history1 search1) "o")
              (make-editview "hello" history1 search1))
(check-expect (type-world-helper (make-searchpost edit1 history1 "hell") "o")
              (make-searchpost edit1 history1 "hello"))

;;backspace
;World -> World
;;removes the right most character from the typed string in a world
(define (backspace ws)
  (cond
    [(editview? ws) (if (> (string-length (editview-edit ws)) 0)
                        (make-editview (substring (editview-edit ws) 0
                                                  (- (string-length (editview-edit ws)) 1))
                                       (editview-history ws) (editview-search ws)) ws)]
    [(searchpost? ws) (if (> (string-length (searchpost-search ws)) 0)
                          (make-searchpost (searchpost-edit ws) (searchpost-history ws)
                                           (substring (searchpost-search ws) 0
                                                      (- (string-length
                                                          (searchpost-search ws)) 1))) ws)]))
    

(check-expect (backspace (make-editview "hello" history1 search1))
              (make-editview "hell" history1 search1))
(check-expect (backspace (make-searchpost edit1 history1 "hello"))
              (make-searchpost edit1 history1 "hell"))
(check-expect (backspace (make-searchpost edit1 history1 "")) (make-searchpost edit1 history1 ""))
(check-expect (backspace (make-editview "" history1 search1)) (make-editview "" history1 search1))


;;send-world
;Sends the given world state to the server
;EditViewPost -> SentWorld
(define (send-world ws)
  (make-package (make-editview "" (editview-history ws) (editview-search ws)) (editview-edit ws)))
  
(check-expect (send-world (make-editview "hello" history1 search1))
              (make-package (make-editview "" history1 search1) "hello"))
  
;;recieve-world
; World String -> World
;Takes the world recieved from the server as well as the string recieved and updates the History
;component of the world. Makes sure that messages are recieved while in search mode.

(define (recieve-world ws st)
  (cond
    [(editview? ws) (if (string-prefix? st "ERROR")
                        (make-editview (editview-edit ws) (cons "Invalid Input" (editview-history ws))
                                       (editview-search ws))
                        (make-editview (editview-edit ws) (cons st (editview-history ws))
                                       (editview-search ws)))]
    [(searchpost? ws) (if (string-prefix? st "ERROR")
                          (make-searchpost (searchpost-edit ws) (cons "Invalid Input"
                                                                      (searchpost-history ws))
                                           (searchpost-search ws))
                          (make-searchpost (searchpost-edit ws) (cons st (searchpost-history ws))
                                           (searchpost-search ws)))])) 

(check-expect (recieve-world world0 "Globalism")
              (make-editview (editview-edit world0) (cons "Globalism" history1) search1))
(check-expect (recieve-world searchpost1 "Globalism")
              (make-searchpost edit1 (cons "Globalism" history1) search1))
(check-expect (recieve-world editview1 "ERROR")
              (make-editview edit1 (cons "Invalid Input" history1) search1))
(check-expect (recieve-world searchpost1 "ERROR")
              (make-searchpost edit1 (cons "Invalid Input" history1) search1))
;;render
;;World -> Image
;Renders the string being typed as well along with the history. Displays the correct information in
;SearchPosts mode
(define (render ws)
  (cond
    [(editview? ws) (above/align "right" (text "Edit View" 24 "green")
                                 (above/align "left" (text (editview-edit ws) 16 "black")
                                              (viewclient ws)))]
    [(searchpost? ws) (above/align "right" (text "Search View" 24 "green")
                                   (above/align "left" (text (searchpost-search ws) 16 "black")
                                                (viewclient ws)))]))

(check-expect (render editview1)
              (above/align "right" (text "Edit View" 24 "green")
                           (above/align "left" (text (editview-edit editview1) 16 "black")
                                        (viewclient editview1))))
(check-expect (render searchpost1)
              (above/align "right" (text "Search View" 24 "green")
                           (above/align "left" (text (searchpost-search searchpost1) 16 "black")
                                        (viewclient searchpost1))))

;;viewclient
;World -> Image
;Recursively cycles through the History list of a world and returns a text image. If the
;world is a SearchPosts, then only words containing the characters in the search component
;of the world are displayed. 
(define (viewclient ws)
  (cond
    [(editview? ws) (render-edit-history (editview-history ws))]
    [(searchpost? ws) (render-edit-history (search-post ws (searchpost-history ws)))]))
  

(check-expect (viewclient editview1) (above/align "left"
                                                  (text "hello" 16 "black")
                                                  (text "jerry" 16 "black")
                                                  (text "I love fundies" 16 "black")
                                                  (text "sometimes" 16 "black") scene))
(check-expect (viewclient searchpost1) (above/align "left"
                                                    (text "hello" 16 "black") scene))

;;render-edit-history
;History -> Image
;Generates in image of all the items in History
(define (render-edit-history his)
  (local
    ((define (gen-image st)
       (text st 16 "black"))
     (define (place-image h sc)
       (above/align "left" h sc)))
    ;; - IN -
    (foldr place-image scene (map gen-image his))))

(check-expect (render-edit-history '()) scene)
(check-expect (render-edit-history history1) (above/align "left"
                                                          (text "hello" 16 "black")
                                                          (text "jerry" 16 "black")
                                                          (text "I love fundies" 16 "black")
                                                          (text "sometimes" 16 "black") scene))

;;search-post
;SearchPost History -> History
;Recursively cycles through the history of the world and generates a new list of strings
;containing charactes that are present in the search branch of the world.
(define (search-post ws his)
  (local
    ((define search-bar (searchpost-search ws))
     (define (contains-search? st)
       (if (string-contains? st search-bar) #true #false)))
    ;; - IN -
    (filter contains-search? his)))
 

(check-expect (search-post searchpost1 history1) (cons "hello" '()))
(check-expect (search-post searchpost2 history1)
              (cons "hello" (cons "jerry" (cons "I love fundies" (cons "sometimes" '())))))
(check-expect (search-post searchpost3 history1) (cons "I love fundies" (cons "sometimes" '())))


              

                             

