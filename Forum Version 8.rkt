(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

(define scene (empty-scene 1000 800))

;; A Reply is a (make-reply String String)
(define-struct reply [author message])
; INTERPRETATION: a struct containing the author and the contents
(define reply1 (make-reply "Alex-Jones" "Get Behind me Satan"))
(define reply2 (make-reply "Hillary" "What Happened?"))
(define reply3 (make-reply "Bush" "Visit my library"))

;reply-temp
;Reply -> ???
#;(define (reply-temp r)
    (...(reply-author r)...(reply-message r)...))

;;A list of replies (LoR) is one of:
;'()
;(cons reply LoR)
(define lor1 (list reply1 reply2 reply3))
(define lor2 '())
(define lor3 (list reply2 reply3 reply1))

;LoR-temp
;LoR -> ???
#;(define (lor-temp l)
    (cond
      [(empty? l)...]
      [(cons? l)...(reply-temp (first l))...(rest l)]))

; A Post is a (list Number (make-post String String (list-of Reply))
(define-struct post [author message replies])
; INTERPRETATION: The ID# of a post, as well as a struct containing the author, the message and a
;list of all replies contained in the post?
(define post1 (list 0 (make-post "Pranav" "Evening!" lor1)))
(define post2 (list 0 (make-post "Jason" "I am Jason" lor1)))
(define post3 (list 0 (make-post "Bob" "I am Bob" lor2)))

;post-temp
;Post -> ???
#;(define (post-temp p)
    (...(first p)...(post-author (second p))...(post-message (second p))...
        (lor-temp (post-replies (second p)))))


; A History is a [List-of Post]
; INTERPRETATION: the history of top-level posts seen so far.
; A History is a List of Posts
; - '()
; - (cons Post History)
(define history1 (list post1 post2 post3 (list "ERROR" "Invalid Input")))
(define history2 '())

;History-temp
#;(define (history-temp h)
    (cond
      [(empty? h)...]
      [(cons? h)...(post-temp (first h))...(history-temp rest h)...]))
 
; A Edit is a String
; INTERPRETATION: the contents of the post the user is currently
; editing.
(define edit1 "editing")

; A Search is a String 
; INTERPRETATION: the current search term the user is looking for
(define search1 "hel")
(define search2 "a")
(define search3 "I")

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


; A ClientMsg is one of
; - "CATCHUP"
; - (list "POST" String)
; - (list "REPLY" Natural String)
; INTERPRETATION:
; – Sending the message "CATCHUP" tells the server you would like it
;    to send you all the prior posts the server has received.  You are only
;    allowed to ask the server for catch-up messages once; a second request
;    will result in an error.
; – Sending the message (list "POST" String) – i.e., a two-item
;    list whose first item is the string "POST", and whose second item is a
;    String – indicates you are writing a new post, where the string provides
;    the text of the post
; – Sending the message (list "REPLY" Natural String) indicates that
;    you are replying to an existing post (Note: you cannot reply to a reply).
;    The number indicates the post's id, and the string is the text of the post.
(define clientmsg0 "CATCHUP")
(define clientmsg1 (list "POST" "Hello"))
(define clientmsg2 (list "REPLY" 0 "Hey There"))

#;(define (clientmsg-temp c)
    (cond
      [(and (string? c) (string=? c "CATCHUP"))...] 
      [(string=? (first c) "POST")...(second c)]
      [(string=? (first c) "REPLY")...(second c)...(third c)...]))

; A SentWorld is one of
; - a World
; - a (make-package ClientMsg)
(define sentworld1 (make-package world0 clientmsg0))
(define sentworld2 (make-package world0 clientmsg1))
(define sentworld3 (make-package world0 clientmsg2))


; A ServerMsg is one of:
; - (list "POST" Natural String String)
; - (list "REPLY" Natural String String)
; - (list "ERROR" String)
; INTERPRETATION:
; – Receiving a "POST" message means there is a new post with the given ID#,
;    author, and contents.  This is the same
;    information as you've been receiving via "id:author:contents", except
;    that the data is properly broken apart for you, instead of mashed into
;    one string.
; – Receiving a "REPLY" message there is a new reply containing the ID# of
;    the parent post (that this is a reply to), the author of the reply as 
;    the next string, and whose content is the final string.
; – Receiving an "ERROR" message means the client made a mistake, with the
;    error message given as the string.
(define servermsg0 (list "POST" 3 "ahluwalia.pr" "Globalism"))
(define servermsg1 (list "POST" 3 "ahluwalia.pr" "AlexJones"))
(define servermsg2 (list "ERROR" "Invalid Input"))
(define servermsg3 (list "REPLY" 5 "Nice Meme bro"))

#;(define (servermsg-temp s)
    (cond
      [(string=? (first s) "POST")...(second s)...(third s)...(fourth s)]
      [(string=? (first s) "REPLY")...(second s)...(third s)...(fourth s)...]
      [(string=? (first s) "ERROR")...(second s)...]))


;;simple-forum
;World -> World
;Creates a chat client that allows a user to type and submit message
;and also displays past messages as images
(define (simple-forum world)
  (big-bang world
            [name "ahluwalia.pr:0893"]
            [register "dictionary.ccs.neu.edu"]
            [port 10007]
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
              (make-package (make-editview "" history1 search1) (list "POST" "hello")))
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
        [(key=? "\r" key) (if (editview? ws) (world-to-client ws) ws)]))

(check-expect (keyhandle (make-searchpost edit1 history1 "hello") "\b")
              (make-searchpost edit1 history1 "hell"))
(check-expect (keyhandle (make-searchpost edit1 history1 "hello") "\b")
              (make-searchpost edit1 history1 "hell"))
(check-expect (keyhandle (make-editview "hello" history1 search1) "\r")
              (make-package (make-editview "" history1 search1)
                            (list "POST" "hello")))
(check-expect (keyhandle (make-searchpost edit1 history1 "hello") "\r")
              (make-searchpost edit1 history1 "hello"))
                         

;;World-to-client
;Converts an EditviewPost to a ClientMsg. and calls the send-world function
; EditviewPost -> ClientMsg
(define (world-to-client ws)
  (cond
    [(string=? (editview-edit ws) "CATCHUP") (send-world ws "CATCHUP")]
    [#false (send-world ws (list "REPLY" "Natural" (editview-edit ws)))];Note: this is set to false
    ;since there are no features that allow us to make a reply in this implementation.
    [else (send-world ws (list "POST" (editview-edit ws)))]))

(check-expect (world-to-client (make-editview "CATCHUP" history1 search1))
              (send-world (make-editview "CATCHUP" history1 search1) "CATCHUP"))
(check-expect (world-to-client (make-editview "HELLOOOOO" history1 search1))
              (send-world (make-editview "HELLOOOOO" history1 search1) (list "POST" "HELLOOOOO")))

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
;Sends the given ClientMSG to the server
;EditviewPost ClientMSG -> SentWorld
(define (send-world ws c)
  (cond
    [(and (string? c) (string=? c "CATCHUP"))
     (make-package (make-editview "" (editview-history ws) (editview-search ws)) c)]
                                                            
    [(string=? (first c) "REPLY") (make-package (make-editview
                                                 "" (editview-history ws) (editview-search ws)) c)]
    [(string=? (first c) "POST") (make-package (make-editview
                                                "" (editview-history ws) (editview-search ws)) c)]))
;I could consolidate post and reply conditions but I want to stick to my template
         
(check-expect (send-world world0 clientmsg0)
              (make-package (make-editview "" history1 search1) clientmsg0))
(check-expect (send-world world0 clientmsg1 )
              (make-package (make-editview "" history1 search1) clientmsg1))
(check-expect (send-world world0 clientmsg2)
              (make-package (make-editview "" history1 search1) clientmsg2))
  
;;recieve-world
; World ServerMSG -> World
;Takes the world recieved from the server as well as the ServerMSG recieved and updates the History
;component of the world. Makes sure that messages are recieved while in search mode.
(define (recieve-world ws sm) 
  (local (
          (define (write-to-history ws sm)
            (cond
              [(editview? ws) (if (and (string? (first sm)) (string-prefix? (first sm) "ERROR"))
                                  (make-editview (editview-edit ws)
                                                 (cons sm (editview-history ws))
                                                 (editview-search ws))
                                  (make-editview (editview-edit ws) (cons sm (editview-history ws))
                                                 (editview-search ws)))]
              [(searchpost? ws) (if (and (string? (first sm)) (string-prefix? (second sm) "ERROR"))
                                    (make-searchpost (searchpost-edit ws)
                                                     (cons sm (searchpost-history ws))
                                                     (searchpost-search ws))
                                    (make-searchpost (searchpost-edit ws)
                                                     (cons sm (searchpost-history ws))
                                                     (searchpost-search ws)))]))
          (define (sm-to-post sm)
            (cond
              [(string=? (first sm) "POST")
               (list (second sm) (make-post (third sm) (fourth sm) '()))]
              [(string=? (first sm) "REPLY") null]
              ;;Told to not impliment this yet in assignment
              [(string=? (first sm) "ERROR") sm])))
    (write-to-history ws (sm-to-post sm))))
  

(check-expect (recieve-world world0 servermsg0)
              (make-editview
               edit1 (cons (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                           (editview-history world0)) search1))
(check-expect (recieve-world searchpost1 servermsg0)
              (make-searchpost
               edit1 (cons (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                           (editview-history world0)) search1))
(check-expect (recieve-world world0 servermsg1)
              (make-editview edit1 (cons (list 3 (make-post "ahluwalia.pr" "AlexJones" '()))
                                         (editview-history world0)) search1))
(check-expect (recieve-world editview1 (list "ERROR" "Invalid Input"))
              (make-editview edit1 (cons (list "ERROR" "Invalid Input") history1) search1))
(check-expect (recieve-world searchpost1 (list "ERROR" "Invalid Input"))
              (make-searchpost edit1 (cons (list "ERROR" "Invalid Input") history1) search1))
;;render
;;World -> Image
;Renders the string being typed as well along with the history. Displays the correct information in
;SearchPosts mode
          
(define (render ws)
  (cond
    [(editview? ws) (above/align "right" (text "Edit View" 24 "green")
                                 (above/align "left"
                                              (text (editview-edit ws) 16 "black") (viewclient ws)))]
                                 
                                 
                                              
    [(searchpost? ws) (above/align "right" (text "Search View" 24 "green")
                                   (above/align
                                    "left"
                                    (text (searchpost-search ws) 16 "black") (viewclient ws)))]))
                                   

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
  

(check-expect (viewclient editview1)
              (overlay/align "left" "top" (above/align "left"
                                                       (above/align "left"
                                                                    (text "0:Pranav: Evening!" 16
                                                                          "black")
                                                                    (text "0:Jason: I am Jason" 16
                                                                          "black")
                                                                    (text "0:Bob: I am Bob" 16
                                                                          "black")
                                                                    (text "Invalid Input" 16
                                                                          "black"))
                                                       (text "~" 16 "black")) scene))  
(check-expect (viewclient searchpost3)
              (overlay/align "left" "top" (above/align "left"
                                                       (above/align "left"
                                                                    
                                                                    (text "0:Jason: I am Jason" 16
                                                                          "black")
                                                                    (text "0:Bob: I am Bob" 16
                                                                          "black"))
                                                       (text "~" 16 "black")) scene))


;;render-edit-history
;History -> Image
;Generates in image of all the items in History
(define (render-edit-history his)
  (local
    ((define (gen-image sm)
       (cond  
         [(post? (second sm))
          (text (string-append (number->string (first sm)) ":" (post-author (second sm)) ": "
                               (post-message (second sm))) 16 "black")]
         [(string=? (first sm) "ERROR") (text (second sm) 16 "black")])) 
     ; Context: st = (list "POST" 5 "Pranav: " "hello" -> (text "5:Pranav: hello" 16 "black")
     ; Context: st = (list "REPLY" 5 "Jim: " "Hi there!") -> (text "5: "Jim: Hi there!" 16 "black")
     ; Context: st = (list "ERROR" "Not valid" -> (text "Not valid" 16 "black")
     (define (place-image1 h sc)
       (above/align "left" "bottom" h sc))

     (define (align-image i b)
       (above/align "left" i b)));;Context: h = "hi", sc = scene -> (above/align "left"
    ;(text "hi" 16 "black") sc)
    
    ;; - IN -
    (overlay/align "left" "top"
                   (foldr align-image (text "~" 16 "black") (map gen-image his)) scene)))

(check-expect (render-edit-history '()) (overlay/align "left" "top" (text "~" 16 "black") scene))
(check-expect (render-edit-history history1)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (above/align "left"
                                                       (text "0:Pranav: Evening!" 16 "black")
                                                       (text "0:Jason: I am Jason" 16 "black")
                                                       (text "0:Bob: I am Bob" 16 "black")
                                                       (text "Invalid Input" 16 "black"))
                                          (text "~" 16 "black")) scene))

;;search-post
;SearchPost History -> History
;Recursively cycles through the history of the world and generates a new list of strings
;containing charactes that are present in the search branch of the world.
(define (search-post ws his)
  (local
    ((define search-bar (searchpost-search ws))
     (define (contains-search? st)
       (if (string-contains? (post-search st) search-bar) #true #false))
     (define (post-search p)
       (if (number? (first p)) (string-append (post-author (second p))
                                              (post-message (second p))) "Error")))
    ;Context: st = "hi" search-bar = "hi" -> (cons "hi" '())
    ;; - IN -
    (filter contains-search? his)))
 

(check-expect (search-post searchpost1 history1) '())
(check-expect (search-post searchpost2 history1) 
              (list
               (list
                0
                (make-post
                 "Pranav"
                 "Evening!"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list
                0
                (make-post
                 "Jason"
                 "I am Jason"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list 0 (make-post "Bob" "I am Bob" '()))))
(check-expect (search-post searchpost3 history1)
              (list
               (list
                0
                (make-post
                 "Jason"
                 "I am Jason"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list 0 (make-post "Bob" "I am Bob" '()))))


              

                             

