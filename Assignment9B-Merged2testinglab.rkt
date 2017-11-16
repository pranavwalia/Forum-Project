;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Assignment9B-Merged2testinglab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

(define scene (empty-scene 1000 800))


;;;;;DATA DEFINITIONS;;;;;;;;;;;;;;;;;

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
(define post1 (list 1 (make-post "Pranav" "Evening!" lor1)))
(define post2 (list 2 (make-post "Jason" "I am Jason" lor1)))
(define post3 (list 3 (make-post "Bob" "I am Bob" lor2)))

;post-temp
;Post -> ???
#;(define (post-temp p)
    (...(first p)...(post-author (second p))...(post-message (second p))...
        (lor-temp (post-replies (second p)))))

;A ClientPost is one of:
;Post
;(list "ERROR" "Invalid Input")
(define clientpost1 post1)
(define clientpost2 (list "ERROR" "Invalid Input"))

;;clientpost-temp
#;(define (clientpost-temp c)
    (cond
      [post? (...(first p)...(post-author (second p))...(post-message (second p))...
                 (lor-temp (post-replies (second p))))]
      [(and (string? (first c)) (string=? (first c) "ERROR"))...]))
    


; A History is a [List-of ClientPost]
; INTERPRETATION: the history of top-level posts seen so far.
; A History is a List of Posts
; - '()
; - (cons Post History)
; - (cons (list "ERROR" "Invalid Input") History)
(define history1 (list post1 post2 post3 (list "ERROR" "Invalid Input")))
(define history2 '())

;History-temp
#;(define (history-temp h)
    (cond
      [(empty? h)...]
      [(cons? h)...(clientpost-temp (first h))...(history-temp rest h)...]))
 
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

(define-struct viewall [edit history])
; A Viewall is a (make-viewall String History)
; INTERPRETATION: The user is viewing all posts (but not replies), 
;   and possibly typing a Command.
(define viewall1 (make-viewall "" history1))
(define viewall2 (make-viewall "Globalism" history1))

;viewall-temp
;viewall -> ???
#;(define (viewall-temp v)
    (...(viewall-edit v)...(viewwall-history v)...))

(define-struct threadview [post history])
; A Threadview is a (make-threadview Post History)
; INTERPRETATION: The user is viewing a specific Post and its replies.
(define threadview1 (make-threadview post1 history1))
(define threadview2 (make-threadview post2 history1))

#;(define (threadview-temp t)
    (...(threadview-post t)...(threadview-history t)...))

(define-struct newitem [nat edit history])
; A Newitem is a (make-newitem [Maybe Natural] String History)
; INTERPRETATION: The user is entering a new post (if the maybe is #false),
;   or a new reply (if the maybe is a number).
(define newitem1 (make-newitem 3 "reply" history1))
(define newitem2 (make-newitem #false "newpost" history1))

;newitem-temp
#;(define (newitem-temp n)
    (...(newitem-nat n)...(newitem-edit n)...(newitem-history n)...))

                     
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

(define-struct searchview [history search])
;A Search is a (make-search History String)
; INTERPRETATION: The user is trying to view only a subset
;   of the existing messages
(define searchview1 (make-searchview history1 search1))
(define searchview2 (make-searchview history1 search2))
(define searchview3 (make-searchview history1 search3))

#;(define (searchview-temp s)
    (...(searchview-history s)...(searchview-search s)...))

; A World is one of
; - Viewall
; - Threadview
; - Newitem
; - Search
(define world0 viewall1)
(define world1 searchview1)

;World-temp
#;(define (world-temp ws)
    (cond
      [(viewall? ws)...(viewall-edit v)...(viewwall-history v)...]
      [(threadview? ws)...(threadview-post t)...(threadview-history t)...]
      [(newitem? ws)...(newitem-nat n)...(newitem-edit n)...(newitem-history n)...]
      [(searchview? ws)...(searchview-history s)...(searchview-search s)...]))


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
(define servermsg3 (list "REPLY" 3 "davis.j" "Nice Meme bro"))
(define servermsg4 (list "REPLY" 4 "davis.j" "Dank Meme bro"))

#;(define (servermsg-temp s)
    (cond
      [(string=? (first s) "POST")...(second s)...(third s)...(fourth s)]
      [(string=? (first s) "REPLY")...(second s)...(third s)...(fourth s)...]
      [(string=? (first s) "ERROR")...(second s)...]))

; A Command is a string of the form
; – "catchup"                      <<=== send the "CATCHUP" message
; – "new"                          <<=== start creating a new post
; – "reply ", followed by a number <<=== start replying to the post with that ID#
; – "view ", followed by a number  <<=== view the full thread of post of that ID#
(define catchup "CATCHUP")
(define commandnewpost "new")
(define replycom1 "reply 2")
(define viewcom1 "view 3")

#;(define (command-temp c)
    (cond
      [(string=? c "catchup")...]
      [(string=? c "new")...]
      [(string-contains? c "reply")...]
      [(string-contains? c "view")...]))

;;simple-forum
;World -> World
;Creates a chat client that allows a user to type and submit message
;and also displays past messages as images
(define (simple-forum world)
  (big-bang world
            [name "ahluwalia.pr:0893"]
            [register "dictionary.ccs.neu.edu"]
            [port 10008]
            [on-key select-world]
            [to-draw render]
            [on-receive recieve-world]))


;;;;;;;;;;;;;Key-Handlers;;;;;;;;;;;;;;;;;;;;;;;;;

;select-world
;World KeyEvent -> World
;selects the world state and passes the world to the (change-world) or (type-world) functions
(define (select-world ws k)
  (cond
    [(viewall? ws) (if (or (string=? k "f1") (string=? k "f2"))
                       (change-world ws k) (type-world ws k))]
    [(threadview? ws) (if (or (string=? k "f1") (string=? k "f2"))
                          (change-world ws k) (type-world ws k))]
    [(newitem? ws) (if (or (string=? k "f1") (string=? k "f2"))
                       (change-world ws k) (type-world ws k))]
    [(searchview? ws) (if (or (string=? k "f1") (string=? k "f2"))
                          (change-world ws k) (type-world ws k))]))

(check-expect (select-world viewall1 "f1") (change-world viewall1 "f1"))
(check-expect (select-world viewall1 "f2") (change-world viewall1 "f2"))
(check-expect (select-world viewall1 "l") (type-world viewall1 "l"))
(check-expect (select-world threadview1 "f1") (change-world threadview1 "f1"))
(check-expect (select-world threadview1 "f2") (change-world threadview1 "f2"))
(check-expect (select-world threadview1 "l") (type-world threadview1 "l"))
(check-expect (select-world newitem1 "f1") (change-world newitem1 "f1"))
(check-expect (select-world newitem1 "f2") (change-world newitem1 "f2"))
(check-expect (select-world newitem1 "l") (type-world newitem1 "l"))
(check-expect (select-world newitem2 "f1") (change-world newitem2 "f1"))
(check-expect (select-world newitem2 "f2") (change-world newitem2 "f2"))
(check-expect (select-world newitem2 "l") (type-world newitem2 "l"))
(check-expect (select-world searchview1 "f1") (change-world searchview1 "f1"))
(check-expect (select-world searchview2 "f2") searchview2)
(check-expect (select-world searchview1 "l") (type-world searchview1 "l"))
(check-expect (select-world searchview2 "l") (type-world searchview2 "l"))

;;change-world
;;Changes the state of a world from to viewall and search according to the data defiition for
;the world state
;World Key -> World
(define (change-world ws k)
  (cond
    [(viewall? ws) (if (key=? k "f1") ws (make-searchview (viewall-history ws) ""))]
    [(threadview? ws) (if (key=? k "f1") (make-viewall "" (threadview-history ws))
                          (make-searchview (threadview-history ws) ""))]
    [(newitem? ws) (if (key=? k "f1") (make-viewall "" (newitem-history ws))
                       (make-searchview (newitem-history ws) ""))]
    [(searchview? ws) (if (key=? k "f1") (make-viewall "" (searchview-history ws)) ws)]))
    
(check-expect (change-world viewall1 "f1") viewall1)
(check-expect (change-world viewall1 "f2") (make-searchview (viewall-history viewall1) ""))
(check-expect (change-world threadview1 "f1") (make-viewall "" (threadview-history threadview1)))
(check-expect (change-world threadview1 "f2") (make-searchview (threadview-history threadview1) ""))
(check-expect (change-world newitem1 "f1") (make-viewall "" (newitem-history newitem1)))
(check-expect (change-world newitem1 "f2") (make-searchview (newitem-history newitem1) ""))
(check-expect (change-world newitem2 "f1") (make-viewall "" (newitem-history newitem2)))
(check-expect (change-world newitem2 "f2") (make-searchview (newitem-history newitem2) ""))
(check-expect (change-world searchview1 "f1") (make-viewall "" (searchview-history searchview1)))
(check-expect (change-world searchview1 "f2") searchview1)

;;type-World
;World KeyEvent -> SentWorld
;appends the typed character to the (editview-edit branch) or the (searchpost-search) branch
;In the case of "\b" or "\r" the (keyhandle ws key) function is called
(define (type-world ws k)
  (cond
    [(viewall? ws) (if (or (key=? k "\b") (key=? k "\r")) (keyhandle ws k) (type-world-helper ws k))]
    [(threadview? ws) ws]
    [(newitem? ws) (if (or (key=? k "\b") (key=? k "\r"))
                       (keyhandle ws k)
                       (type-world-helper ws k))]
    
    [(searchview? ws) (if (or (key=? k "\b") (key=? k "\r"))
                          (keyhandle ws k)
                          (type-world-helper ws k))]))
  
(check-expect (type-world viewall1 "o") (type-world-helper viewall1 "o"))
(check-expect (type-world viewall1 "\b") (keyhandle viewall1 "\b"))
(check-expect (type-world viewall1 "\r") (keyhandle viewall1 "\r"))
(check-expect (type-world threadview1 "o") threadview1)
(check-expect (type-world newitem1 "o") (type-world-helper newitem1 "o"))
(check-expect (type-world newitem1 "\b") (keyhandle newitem1 "\b"))
(check-expect (type-world newitem1 "\r") (keyhandle newitem1 "\r"))
(check-expect (type-world newitem2 "o") (type-world-helper newitem2 "o"))
(check-expect (type-world newitem2 "\b") (keyhandle newitem2 "\b"))
(check-expect (type-world newitem2 "\r") (keyhandle newitem2 "\r"))
(check-expect (type-world searchview1 "o") (type-world-helper searchview1 "o"))
(check-expect (type-world searchview1 "\b") (keyhandle searchview1 "\b"))
(check-expect (type-world searchview1 "\r") searchview1)

;;keyhandle
;World KeyEvent -> SentWorld
;handles the "/r" and "\b" keys from type-world. "\r" will call (send-world ws) "\b" calls
;(backspace ws)
(define (keyhandle ws key)
  (cond ;(the only key that can get passed to this func besides a "\b" is a "\r")
    [(viewall? ws) (if (key=? "\b" key) (backspace ws) (process-command ws (viewall-edit ws)))]
    [(threadview? ws) ws]
    [(newitem? ws) (if (key=? "\b" key) (backspace ws) (world-to-client ws))]
    [(searchview? ws) (if (key=? "\b" key) (backspace ws) ws)]))
  

(check-expect (keyhandle viewall1 "\b") (backspace viewall1))
(check-expect (keyhandle viewall1 "\r") (process-command viewall1 (viewall-edit viewall1)))
(check-expect (keyhandle threadview1 "\r") threadview1)
(check-expect (keyhandle newitem1 "\b") (backspace newitem1))
(check-expect (keyhandle newitem2 "\r") (world-to-client newitem2))
(check-expect (keyhandle searchview1 "\b") (backspace searchview1))
(check-expect (keyhandle searchview1 "\r") searchview1)

;type-world-helper
;Prevents "shift" "cntrl" and longer keyevents from being displayed
;World KeyEvent -> World
(define (type-world-helper ws k)
  (cond
    [(viewall? ws) (if (= (string-length k) 1)
                       (make-viewall (string-append (viewall-edit ws) k) (viewall-history ws)) ws)]
    [(threadview? ws) ws]
    [(newitem? ws) (if (= (string-length k) 1)
                       (make-newitem (newitem-nat ws)
                                     (string-append (newitem-edit ws) k) (newitem-history ws)) ws)]
    [(searchview? ws) (if (= (string-length k) 1)
                          (make-searchview (searchview-history ws)
                                           (string-append (searchview-search ws) k)) ws)]))

(check-expect (type-world-helper viewall1 "shift") viewall1)
(check-expect (type-world-helper viewall1 "o")
              (make-viewall
               "o"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list (make-reply "Alex-Jones"
                                                                         "Get Behind me Satan")
                                                             (make-reply "Hillary"
                                                                         "What Happened?")
                                                             (make-reply "Bush"
                                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list (make-reply "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                              (make-reply "Hillary"
                                                                          "What Happened?")
                                                              (make-reply "Bush"
                                                                          "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))
(check-expect (type-world-helper threadview1 "o") threadview1)
(check-expect (type-world-helper newitem1 "shift") newitem1)
(check-expect (type-world-helper newitem1 "o")
              (make-newitem
               3
               "replyo"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list (make-reply "Alex-Jones"
                                                                         "Get Behind me Satan")
                                                             (make-reply "Hillary" "What Happened?")
                                                             (make-reply "Bush"
                                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list (make-reply "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                              (make-reply "Hillary" "What Happened?")
                                                              (make-reply "Bush"
                                                                          "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))

(check-expect (type-world-helper searchview1 "o")
              (make-searchview
               (list
                (list 1 (make-post "Pranav" "Evening!" (list (make-reply "Alex-Jones"
                                                                         "Get Behind me Satan")
                                                             (make-reply "Hillary"
                                                                         "What Happened?")
                                                             (make-reply "Bush"
                                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list (make-reply "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                              (make-reply "Hillary" "What Happened?")
                                                              (make-reply "Bush"
                                                                          "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))
               "helo"))
(check-expect (type-world-helper searchview1 "shift") searchview1)


;;backspace
;World -> World
;;removes the right most character from the typed string in a world
(define (backspace ws)
  (cond
    [(viewall? ws) (if (> (string-length (viewall-edit ws)) 0)
                       (make-viewall (substring (viewall-edit ws) 0
                                                (- (string-length (viewall-edit ws)) 1))
                                     (viewall-history ws)) ws)]
    [(threadview? ws) ws]
    [(newitem? ws) (if (> (string-length (newitem-edit ws)) 0)
                       (make-newitem
                        (newitem-nat ws)
                        (substring (newitem-edit ws) 0
                                   (- (string-length (newitem-edit ws)) 1))
                        (newitem-history ws)) ws)]
    [(searchview? ws) (if (> (string-length (searchview-search ws)) 0)
                          (make-searchview
                           (searchview-history ws) (substring
                                                    (searchview-search ws) 0
                                                    (- (string-length
                                                        (searchview-search ws)) 1))) ws)]))
    

(check-expect (backspace viewall1) viewall1)
(check-expect (backspace viewall2)
              (make-viewall
               "Globalis"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list (make-reply "Alex-Jones"
                                                                         "Get Behind me Satan")
                                                             (make-reply "Hillary"
                                                                         "What Happened?")
                                                             (make-reply "Bush"
                                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list (make-reply "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                              (make-reply "Hillary"
                                                                          "What Happened?")
                                                              (make-reply "Bush"
                                                                          "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))
(check-expect (backspace threadview1) threadview1)
(check-expect (backspace newitem1)
              (make-newitem
               3
               "repl"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply "Alex-Jones"
                                                                    "Get Behind me Satan")
                                                        (make-reply "Hillary" "What Happened?")
                                                        (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply "Alex-Jones"
                                                                     "Get Behind me Satan")
                                                         (make-reply "Hillary" "What Happened?")
                                                         (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))
(check-expect (backspace newitem2)
              (make-newitem
               #false
               "newpos"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply "Alex-Jones"
                                                                    "Get Behind me Satan")
                                                        (make-reply "Hillary" "What Happened?")
                                                        (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list (make-reply "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                              (make-reply "Hillary" "What Happened?")
                                                              (make-reply "Bush"
                                                                          "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))
(check-expect (backspace searchview1)
              (make-searchview
               (list
                (list 1 (make-post "Pranav" "Evening!" (list (make-reply "Alex-Jones"
                                                                         "Get Behind me Satan")
                                                             (make-reply "Hillary" "What Happened?")
                                                             (make-reply "Bush"
                                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list (make-reply "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                              (make-reply "Hillary"
                                                                          "What Happened?")
                                                              (make-reply "Bush"
                                                                          "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))
               "he"))


;;Process-command
;Viewall Command -> SentWorld
;Handles the "catchup" "view" "new" "reply" commands for Viewall
(define (process-command ws cmd)
  (local    
    (
     (define cmdlist (string-split cmd))
     (define cmdisreplycommand?
       (and (= (length cmdlist) 2)
            (number? (string->number (second cmdlist)))
            (string-contains? (first cmdlist) "reply")))
     (define cmdisview?
       (and (= (length (string-split cmd)) 2) (number? (string->number (second cmdlist)))
            (string-contains? (first (string-split cmd)) "view"))))   
    (cond
      [(string=? cmd "catchup") (world-to-client ws)]
      [(string=? cmd "new") (make-newitem #false "" (viewall-history ws))]
      [cmdisreplycommand?
       (make-newitem (string->number (second cmdlist)) "" (viewall-history ws))]
      [cmdisview?
       (if (and (list? (assoc (string->number (second cmdlist)) (viewall-history ws)))
                (post? (second (assoc (string->number (second cmdlist)) (viewall-history ws)))))
           (make-threadview (assoc (string->number (second cmdlist)) (viewall-history ws))
                            (viewall-history ws)) ws)]
      [else ws])))

(check-expect (process-command (make-viewall "catchup" history1) "catchup")
              (world-to-client (make-viewall "catchup" history1)))
(check-expect (process-command (make-viewall "new" history1) "new")
              (make-newitem #false "" history1))
(check-expect (process-command viewall1 "reply 1")
              (make-newitem 1 "" (viewall-history viewall1)))
(check-expect (process-command viewall1 "d;ladf;lja") viewall1)

;A VieworNew is one of:
;ViewItem
;NewItem

;;World-to-client
;Converts an Newitem to a ClientMsg. and calls the send-world function
; VieworNew -> Sentworld
(define (world-to-client ws)
  (cond
    [(and (viewall? ws) (string=? (viewall-edit ws) "catchup")) (send-world ws "CATCHUP")]
    [(number? (newitem-nat ws)) (send-world ws (list "REPLY" (newitem-nat ws) (newitem-edit ws)))]
    [else (send-world ws (list "POST" (newitem-edit ws)))]))

;;There is no case where world-to-client has a viewall with anything other than "catchup"
;since it only gets called in that case
(check-expect (world-to-client (make-viewall "catchup" history1))
              (send-world (make-viewall "catchup" history1) "CATCHUP"))
(check-expect (world-to-client newitem1) (send-world newitem1 (list "REPLY" (newitem-nat newitem1)
                                                                    (newitem-edit newitem1))))
(check-expect (world-to-client newitem2) (send-world newitem2 (list "POST" (newitem-edit newitem2))))


;;send-world
;Sends the given ClientMSG to the server
;World ClientMSG -> SentWorld
(define (send-world ws c)
  (cond
    [(and (viewall? ws) (string? c) (string=? c "CATCHUP"))
     (make-package (make-viewall "" (viewall-history ws)) c)]                         
    [(string=? (first c) "REPLY") (make-package (make-newitem
                                                 (newitem-nat ws) "" (newitem-history ws)) c)]
    [(string=? (first c) "POST") (make-package (make-newitem
                                                #false "" (newitem-history ws)) c)]))

(check-expect (send-world viewall1 "CATCHUP")
              (make-package (make-viewall "" (viewall-history viewall1)) "CATCHUP"))
(check-expect (send-world newitem1 clientmsg2)
              (make-package (make-newitem 3 "" (newitem-history newitem1)) clientmsg2))
(check-expect (send-world newitem2 clientmsg1)
              (make-package (make-newitem #false "" (newitem-history newitem2)) clientmsg1))

;;;;;;;Server Side Handlers;;;;;;;

;;recieve-world
; World ServerMSG -> World
;Takes the world recieved from the server as well as the ServerMSG recieved and updates the History
;component of the world. Makes sure that messages are recieved while in search mode.
(define (recieve-world ws sm) 
  (local 
    (
     ;History ServerMSG -> History
     (define (sm-to-history h sm)
       (cond
         [(string=? (first sm) "POST")
          (replace-or-add-post h (list (second sm) (make-post (third sm) (fourth sm) '())))]
         [(string=? (first sm) "REPLY")
          (add-reply h sm)]
         [(string=? (first sm) "ERROR") (cons sm h)])))
    ;Context h = history1 sm = servermsg0 -> (expected result below) sorry it's so long
    #;(list
       (list 1 (make-post "Pranav" "Evening!" (list
                                               (make-reply
                                                "Alex-Jones"
                                                "Get Behind me Satan")
                                               (make-reply
                                                "Hillary"
                                                "What Happened?")
                                               (make-reply
                                                "Bush"
                                                "Visit my library"))))
       (list 2 (make-post "Jason" "I am Jason" (list
                                                (make-reply
                                                 "Alex-Jones"
                                                 "Get Behind me Satan")
                                                (make-reply
                                                 "Hillary"
                                                 "What Happened?")
                                                (make-reply
                                                 "Bush"
                                                 "Visit my library"))))
       (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
       (list "ERROR" "Invalid Input"))

    (cond
      [(viewall? ws) (make-viewall (viewall-edit ws) (sm-to-history (viewall-history ws) sm))]
      [(threadview? ws) (make-threadview (threadview-post ws)
                                         (sm-to-history (threadview-history ws) sm))]
      [(newitem? ws) (make-newitem (newitem-nat ws) (newitem-edit ws)
                                   (sm-to-history (newitem-history ws) sm))]
      [(searchview? ws) (make-searchview (sm-to-history (searchview-history ws) sm)
                                         (searchview-search ws))])))

(check-expect (recieve-world searchview1 (list "POST" 3 "ahluwalia.pr" "Globalism"))
              (make-searchview
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply
                                                         "Alex-Jones"
                                                         "Get Behind me Satan")
                                                        (make-reply
                                                         "Hillary"
                                                         "What Happened?")
                                                        (make-reply
                                                         "Bush"
                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones"
                                                          "Get Behind me Satan")
                                                         (make-reply
                                                          "Hillary"
                                                          "What Happened?")
                                                         (make-reply
                                                          "Bush"
                                                          "Visit my library"))))
                (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                (list "ERROR" "Invalid Input")) search1))

(check-expect (recieve-world searchview1 (list "POST" 4 "ahluwalia.pr" "Globalism"))
              (make-searchview
               (list ;this test purposely shows that regardless of the order of the history itself
                ;a post that does not match a previous one is slapped to the top of the history.
                (list 4 (make-post "ahluwalia.pr" "Globalism" '()))
                (list 1 (make-post "Pranav" "Evening!"
                                   (list (make-reply "Alex-Jones" "Get Behind me Satan")
                                         (make-reply "Hillary" "What Happened?")
                                         (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason"
                                   (list
                                    (make-reply "Alex-Jones" "Get Behind me Satan")
                                    (make-reply "Hillary" "What Happened?")
                                    (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input")) search1))

(check-expect (recieve-world searchview1 (list "REPLY" 3 "davis.j" "Dank Meme bro"))
              (make-searchview  (list
                                 (list 1 (make-post "Pranav" "Evening!" (list
                                                                         (make-reply
                                                                          "Alex-Jones"
                                                                          "Get Behind me Satan")
                                                                         (make-reply
                                                                          "Hillary"
                                                                          "What Happened?")
                                                                         (make-reply
                                                                          "Bush"
                                                                          "Visit my library"))))
                                 (list 2 (make-post "Jason" "I am Jason" (list
                                                                          (make-reply
                                                                           "Alex-Jones"
                                                                           "Get Behind me Satan")
                                                                          (make-reply
                                                                           "Hillary"
                                                                           "What Happened?")
                                                                          (make-reply
                                                                           "Bush"
                                                                           "Visit my library"))))
                                 (list 3 (make-post "Bob" "I am Bob" (list
                                                                      (make-reply "davis.j"
                                                                                  "Dank Meme bro"))))
                                 (list "ERROR" "Invalid Input")) search1))
                                                        
(check-expect (recieve-world viewall1 (list "ERROR" "Invalid Input"))
              (make-viewall "" (cons (list "ERROR" "Invalid Input") history1)))

(check-expect (recieve-world threadview1 (list "ERROR" "Invalid Input"))
              (make-threadview post1 (cons (list "ERROR" "Invalid Input") history1)))

(check-expect (recieve-world newitem1 (list "ERROR" "Invalid Input"))
              (make-newitem 3 "reply" (cons (list "ERROR" "Invalid Input") history1)))

(check-expect (recieve-world searchview1 (list "ERROR" "Invalid Input"))
              (make-searchview (cons (list "ERROR" "Invalid Input") history1) search1))

(check-expect (recieve-world viewall1 (list "POST" 3 "ahluwalia.pr" "Globalism"))
              (make-viewall
               ""
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply
                                                         "Alex-Jones"
                                                         "Get Behind me Satan")
                                                        (make-reply
                                                         "Hillary"
                                                         "What Happened?")
                                                        (make-reply
                                                         "Bush"
                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones"
                                                          "Get Behind me Satan")
                                                         (make-reply
                                                          "Hillary"
                                                          "What Happened?")
                                                         (make-reply
                                                          "Bush"
                                                          "Visit my library"))))
                (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world viewall1 (list "POST" 4 "ahluwalia.pr" "Globalism"))
              (make-viewall
               ""
               (list ;this test purposely shows that regardless of the order of the history itself
                ;a post that does not match a previous one is slapped to the top of the history.
                (list 4 (make-post "ahluwalia.pr" "Globalism" '()))
                (list 1 (make-post "Pranav" "Evening!"
                                   (list (make-reply "Alex-Jones" "Get Behind me Satan")
                                         (make-reply "Hillary" "What Happened?")
                                         (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason"
                                   (list
                                    (make-reply "Alex-Jones" "Get Behind me Satan")
                                    (make-reply "Hillary" "What Happened?")
                                    (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))
              

(check-expect (recieve-world viewall1 (list "REPLY" 3 "davis.j" "Dank Meme bro"))
              (make-viewall
               "" (list
                   (list 1 (make-post "Pranav" "Evening!" (list
                                                           (make-reply
                                                            "Alex-Jones"
                                                            "Get Behind me Satan")
                                                           (make-reply
                                                            "Hillary"
                                                            "What Happened?")
                                                           (make-reply
                                                            "Bush"
                                                            "Visit my library"))))
                   (list 2 (make-post "Jason" "I am Jason" (list
                                                            (make-reply
                                                             "Alex-Jones"
                                                             "Get Behind me Satan")
                                                            (make-reply
                                                             "Hillary"
                                                             "What Happened?")
                                                            (make-reply
                                                             "Bush"
                                                             "Visit my library"))))
                   (list 3 (make-post "Bob" "I am Bob" (list
                                                        (make-reply "davis.j"
                                                                    "Dank Meme bro"))))
                   (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world threadview1 (list "REPLY" 3 "davis.j" "Dank Meme bro"))
              (make-threadview
               post1 (list
                      (list 1 (make-post "Pranav" "Evening!" (list
                                                              (make-reply
                                                               "Alex-Jones"
                                                               "Get Behind me Satan")
                                                              (make-reply
                                                               "Hillary"
                                                               "What Happened?")
                                                              (make-reply
                                                               "Bush"
                                                               "Visit my library"))))
                      (list 2 (make-post "Jason" "I am Jason" (list
                                                               (make-reply
                                                                "Alex-Jones"
                                                                "Get Behind me Satan")
                                                               (make-reply
                                                                "Hillary"
                                                                "What Happened?")
                                                               (make-reply
                                                                "Bush"
                                                                "Visit my library"))))
                      (list 3 (make-post "Bob" "I am Bob" (list
                                                           (make-reply "davis.j"
                                                                       "Dank Meme bro"))))
                      (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world threadview1 (list "POST" 4 "ahluwalia.pr" "Globalism"))
              (make-threadview
               post1
               (list ;this test purposely shows that regardless of the order of the history itself
                ;a post that does not match a previous one is slapped to the top of the history.
                (list 4 (make-post "ahluwalia.pr" "Globalism" '()))
                (list 1 (make-post "Pranav" "Evening!"
                                   (list (make-reply "Alex-Jones" "Get Behind me Satan")
                                         (make-reply "Hillary" "What Happened?")
                                         (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason"
                                   (list
                                    (make-reply "Alex-Jones" "Get Behind me Satan")
                                    (make-reply "Hillary" "What Happened?")
                                    (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world threadview1 (list "POST" 3 "ahluwalia.pr" "Globalism"))
              (make-threadview
               post1
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply
                                                         "Alex-Jones"
                                                         "Get Behind me Satan")
                                                        (make-reply
                                                         "Hillary"
                                                         "What Happened?")
                                                        (make-reply
                                                         "Bush"
                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones"
                                                          "Get Behind me Satan")
                                                         (make-reply
                                                          "Hillary"
                                                          "What Happened?")
                                                         (make-reply
                                                          "Bush"
                                                          "Visit my library"))))
                (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                (list "ERROR" "Invalid Input"))))
              
              

(check-expect (recieve-world newitem1 (list "POST" 3 "ahluwalia.pr" "Globalism"))
              (make-newitem
               3
               "reply"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply
                                                         "Alex-Jones"
                                                         "Get Behind me Satan")
                                                        (make-reply
                                                         "Hillary"
                                                         "What Happened?")
                                                        (make-reply
                                                         "Bush"
                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones"
                                                          "Get Behind me Satan")
                                                         (make-reply
                                                          "Hillary"
                                                          "What Happened?")
                                                         (make-reply
                                                          "Bush"
                                                          "Visit my library"))))
                (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                (list "ERROR" "Invalid Input"))))

              
(check-expect (recieve-world newitem1 (list "REPLY" 3 "davis.j" "Dank Meme bro"))
              (make-newitem
               3
               "reply"
               (list
                (list 1
                      (make-post "Pranav" "Evening!" (list
                                                      (make-reply "Alex-Jones" "Get Behind me Satan")
                                                      (make-reply "Hillary" "What Happened?")
                                                      (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones" "Get Behind me Satan")
                                                         (make-reply "Hillary" "What Happened?")
                                                         (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" (list (make-reply "davis.j" "Dank Meme bro"))))
                (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world newitem1 (list "POST" 4 "ahluwalia.pr" "Globalism"))
              (make-newitem
               3
               "reply"
               (list ;this test purposely shows that regardless of the order of the history itself
                ;a post that does not match a previous one is slapped to the top of the history.
                (list 4 (make-post "ahluwalia.pr" "Globalism" '()))
                (list 1 (make-post "Pranav" "Evening!"
                                   (list (make-reply "Alex-Jones" "Get Behind me Satan")
                                         (make-reply "Hillary" "What Happened?")
                                         (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason"
                                   (list
                                    (make-reply "Alex-Jones" "Get Behind me Satan")
                                    (make-reply "Hillary" "What Happened?")
                                    (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world newitem2 (list "POST" 3 "ahluwalia.pr" "Globalism"))
              (make-newitem
               #false
               "newpost"
               (list
                (list 1 (make-post "Pranav" "Evening!" (list
                                                        (make-reply
                                                         "Alex-Jones"
                                                         "Get Behind me Satan")
                                                        (make-reply
                                                         "Hillary"
                                                         "What Happened?")
                                                        (make-reply
                                                         "Bush"
                                                         "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones"
                                                          "Get Behind me Satan")
                                                         (make-reply
                                                          "Hillary"
                                                          "What Happened?")
                                                         (make-reply
                                                          "Bush"
                                                          "Visit my library"))))
                (list 3 (make-post "ahluwalia.pr" "Globalism" '()))
                (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world newitem2 (list "REPLY" 3 "davis.j" "Dank Meme bro"))
              (make-newitem
               #false
               "newpost"
               (list
                (list 1
                      (make-post "Pranav" "Evening!" (list
                                                      (make-reply "Alex-Jones" "Get Behind me Satan")
                                                      (make-reply "Hillary" "What Happened?")
                                                      (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason" (list
                                                         (make-reply
                                                          "Alex-Jones" "Get Behind me Satan")
                                                         (make-reply "Hillary" "What Happened?")
                                                         (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" (list (make-reply "davis.j" "Dank Meme bro"))))
                (list "ERROR" "Invalid Input"))))

(check-expect (recieve-world newitem2 (list "POST" 4 "ahluwalia.pr" "Globalism"))
              (make-newitem
               #false
               "newpost"
               (list ;this test purposely shows that regardless of the order of the history itself
                ;a post that does not match a previous one is slapped to the top of the history.
                (list 4 (make-post "ahluwalia.pr" "Globalism" '()))
                (list 1 (make-post "Pranav" "Evening!"
                                   (list (make-reply "Alex-Jones" "Get Behind me Satan")
                                         (make-reply "Hillary" "What Happened?")
                                         (make-reply "Bush" "Visit my library"))))
                (list 2 (make-post "Jason" "I am Jason"
                                   (list
                                    (make-reply "Alex-Jones" "Get Behind me Satan")
                                    (make-reply "Hillary" "What Happened?")
                                    (make-reply "Bush" "Visit my library"))))
                (list 3 (make-post "Bob" "I am Bob" '()))
                (list "ERROR" "Invalid Input"))))




;;replace-or-add-post
;History Post -> History
;Searches a history for a post with the same ID and replaces it. If a post with the same id
;is not present, it adds it to the history
(define (replace-or-add-post h p)
  (local
    ;ph is  Post
    ((define (same-id? ph)
       (if (and (number? (first ph)) (= (first p) (first ph))) p ph))
     ; context p = (list 2 (make-post "Pranav" "New Post" '())
     ;ph = (list 2 (make-post "Pranav" "Replaced post" '()) 
     ;-> (list 2 (make-post "Pranav" "Replaced post" '())
     (define new-history (map same-id? h)))
    
    (if (equal? new-history h) (cons p h) new-history)))


(check-expect (replace-or-add-post '() (list 3 (make-post "Pranav" "New Post" '())))
              (list (list 3 (make-post "Pranav" "New Post" '()))))
(check-expect (replace-or-add-post history1 (list 3 (make-post "Pranav" "New Post" '())))
              (list post1 post2 (list 3 (make-post "Pranav" "New Post" '()))
                    (list "ERROR" "Invalid Input")))
(check-expect (replace-or-add-post history1 (list 5 (make-post "Pranav" "New Post" '())))
              (cons (list 5 (make-post "Pranav" "New Post" '())) history1))

;add-reply
;History ServerMsg -> History
;Adds a reply to the relevant post in the history.
(define (add-reply h smg)
  (local
    ((define find-post (assoc (second smg) h))
     (define smgtoreply (make-reply (third smg) (fourth smg)))
     (define new-post ;not a function
       (if (not (false? find-post))
           (list (second smg) (make-post (post-author (second find-post))
                                         (post-message (second find-post))
                                         (cons
                                          smgtoreply
                                          (post-replies
                                           (second find-post))))) #false)))
    (if (not (false? new-post)) (replace-or-add-post h new-post) h)))

(check-expect (add-reply history1 servermsg3)
              (list
               (list
                1
                (make-post
                 "Pranav"
                 "Evening!"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list
                2
                (make-post "Jason" "I am Jason"
                           (list
                            (make-reply "Alex-Jones" "Get Behind me Satan")
                            (make-reply "Hillary" "What Happened?")
                            (make-reply "Bush" "Visit my library"))))
               (list 3 (make-post "Bob" "I am Bob" (list (make-reply "davis.j" "Nice Meme bro"))))
               (list "ERROR" "Invalid Input")))
(check-expect (add-reply history1 servermsg4) history1)


;;;;;Render Functions;;;;;;;;;;;

;;render
;;World -> Image
;Renders the string being typed as well along with the history. Displays the correct information in
;SearchPosts mode       
(define (render ws)
  (cond
    [(viewall? ws) (above/align "right" (text "View All" 24 "green")
                                (above/align "left"
                                             (text (viewall-edit ws) 16 "black")
                                             (viewclient ws)))]
    [(threadview? ws) (above/align "right" (text "Thread View" 24 "green")
                                   (overlay/align "left" "top" (viewclient ws) scene))]
    [(newitem? ws) (if (number? (newitem-nat ws))
                       (above/align "right" (text (string-append "Replying to post "
                                                                 (number->string (newitem-nat ws)))
                                                  24 "green")
                                    (above/align "left" (text (newitem-edit ws) 16 "black")
                                                 (viewclient ws)))
                       (above/align "right" (text "Creating New Post"
                                                  24 "green")
                                    (above/align "left" (text (newitem-edit ws) 16 "black")
                                                 (viewclient ws))))]
    [(searchview? ws)(above/align "right" (text "Searching" 24 "green")
                                  (above/align "left" (text (searchview-search ws) 16 "black")
                                               (viewclient ws)))]))


                                   
(check-expect (render viewall1) (above/align "right" (text "View All" 24 "green")
                                             (above/align "left" (text (viewall-edit viewall1)
                                                                       16 "black")
                                                          (viewclient viewall1))))
(check-expect (render threadview1) (above/align "right" (text "Thread View" 24 "green")
                                                (overlay/align "left" "top"
                                                               (viewclient threadview1) scene)))
(check-expect (render newitem1) (above/align "right" (text "Replying to post 3" 24 "green")
                                             (above/align "left" (text "reply" 16 "black")
                                                          (viewclient newitem1))))
(check-expect (render searchview1)
              (above/align "right" (text "Searching" 24 "green")
                           (above/align "left" (text (searchview-search searchview1) 16 "black")
                                        (viewclient searchview1))))

 
;;viewclient
;World -> Image
;Recursively cycles through the History list of a world and returns a text image. If the
;world is a SearchPosts, then only words containing the characters in the search component
;of the world are displayed. 
(define (viewclient ws)
  (cond
    [(viewall? ws) (render-edit-history (viewall-history ws))]
    [(threadview? ws) (draw-post-and-replies (threadview-post ws))]
    [(newitem? ws) (render-edit-history (newitem-history ws))]
    [(searchview? ws) (render-edit-history (search-post ws (searchview-history ws)))]))

(check-expect (viewclient viewall1)
              (overlay/align "left" "top" (above/align "left"
                                                       (above/align "left"
                                                                    (draw-post-and-replies post1)
                                                                    (draw-post-and-replies post2)
                                                                    (draw-post-and-replies post3)
                                                                    (text "Invalid Input" 16 "black"))
                                                       (text "~" 16 "black")) scene))
(check-expect (viewclient threadview1) (draw-post-and-replies (threadview-post threadview1)))
(check-expect (viewclient newitem1) (render-edit-history (newitem-history newitem1)))              
(check-expect (viewclient searchview3)
              (overlay/align "left" "top" (above/align "left"
                                                       (above/align "left"
                                                                    
                                                                    (draw-post-and-replies post2)
                                                                    (draw-post-and-replies post3))
                                                       (text "~" 16 "black")) scene))

;;render-edit-history
;History -> Image
;Generates in image of all the items in History
(define (render-edit-history his)
  (local
    ((define (gen-image p)
       (cond  
         [(post? (second p))
          (draw-post-and-replies p)]
         [(string=? (first p) "ERROR") (text (second p) 16 "black")]))
     ; Context: st = (list 5 (make-post "Pranav: " "hello" '()) -> (text "5:Pranav: hello" 16 "black")
     ; Context: st = (list "ERROR" "Not valid" -> (text "Not valid" 16 "black")
     (define (align-image i b)
       (above/align "left" i b)))
    ;; - IN -
    (overlay/align "left" "top"
                   (foldr align-image (text "~" 16 "black") (map gen-image his)) scene)))

(check-expect (render-edit-history '()) (overlay/align "left" "top" (text "~" 16 "black") scene))
(check-expect (render-edit-history history1)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (above/align "left"
                                                       (draw-post-and-replies post1)
                                                       (draw-post-and-replies post2)
                                                       (draw-post-and-replies post3)
                                                       (text "Invalid Input" 16 "black"))
                                          (text "~" 16 "black")) scene))

;draw-post-and-replies
;Post -> Image
;Renders a post and all of it's replies.
(define (draw-post-and-replies p)
  (local
    ((define post-main-image
       (text (string-append (number->string (first p)) ":" (post-author (second p)) ": "
                            (post-message (second p))) 16 "black"))
     (define replies (post-replies (second p)))
     (define (replies-image r)
       (text (string-append "     " (reply-author r) ": " (reply-message r)) 12 "blue"))
     ;context r = (make-reply "alex" "hello") -> (text "     alex: hello" 12 "blue")
     (define (align-replies ri sc)      
       (above/align "left" ri sc)))
    ;context ri = (text "hello" 12 "blue") sc = (text "--------" 16 "black")
    ;----> (above/align "left" (text "hello" 12 "blue") (text "--------" 16 "black")
    (above/align "left" post-main-image (foldl align-replies (text "--------" 16 "black")
                                               (map replies-image replies)))))
     
  
     
(check-expect (draw-post-and-replies post1)
              (above/align "left" (text "1:Pranav: Evening!" 16 "black")
                           (above/align "left" (text "     Bush: Visit my library" 12 "blue")
                                        (text "     Hillary: What Happened?" 12 "blue")
                                        (text "     Alex-Jones: Get Behind me Satan" 12 "blue")
                                        (text "--------" 16 "black"))))

(check-expect (draw-post-and-replies post3) (above/align "left" (text "3:Bob: I am Bob" 16 "black")
                                                         (text "--------" 16 "black")))

;;search-post
;SearchPost History -> History
;Recursively cycles through the history of the world and generates a new list of strings
;containing charactes that are present in the search branch of the world.
(define (search-post ws his)
  (local
    ((define search-bar (searchview-search ws))
     (define (contains-search? st)
       (if (string-contains? (post-search st) search-bar) #true #false))
     ;Context (contains-search?) st = "Hi" search-bar = "Hi" -> #true
     (define (post-search p)
       (if (number? (first p)) (string-append (post-author (second p))
                                              (post-message (second p))) "Error")))
    ;Context (post-search): p = (list 5 "Pranav " "Hi ") -> "Pranav Hi"
   
    ;; - IN -
    (filter contains-search? his)))
 

(check-expect (search-post searchview1 history1) '())
(check-expect (search-post searchview2 history1) 
              (list
               (list
                1
                (make-post
                 "Pranav"
                 "Evening!"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list
                2
                (make-post
                 "Jason"
                 "I am Jason"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list 3 (make-post "Bob" "I am Bob" '()))))
(check-expect (search-post searchview3 history1)
              (list
               (list
                2
                (make-post
                 "Jason"
                 "I am Jason"
                 (list
                  (make-reply "Alex-Jones" "Get Behind me Satan")
                  (make-reply "Hillary" "What Happened?")
                  (make-reply "Bush" "Visit my library"))))
               (list 3 (make-post "Bob" "I am Bob" '()))))


              

                             

