#lang racket

(require db)

;;;;;;;;;;;;;;;;
;; Blog Model ;;
;;;;;;;;;;;;;;;;

(struct blog (db)
  #:mutable #:prefab)

(define (blog-insert-post! a-blog title body)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO posts (title, body) VALUES (?, ?)"
   title body))

(define (blog-posts a-blog)
  (define (id->post an-id)
    (post a-blog an-id))
  (map id->post
       (query-list
        (blog-db a-blog)
        "SELECT id FROM posts")))

(define (initialize-blog! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
                (string-append
                 "CREATE TABLE posts "
                 "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)"))
    (blog-insert-post!
     the-blog "First Post!" "Tjo, första inlägget va?!")
    (blog-insert-post!
     the-blog "New Post!" "Bättre och bättre?!"))
  (unless (table-exists? db "comments")
    (query-exec db
                (string-append
                 "CREATE TABLE comments "
                 "(pid INTEGER, content TEXT)"))
    (post-insert-comment!
     the-blog (first (blog-posts the-blog))
     "First comment!"))
  the-blog)

;;;;;;;;;;;;;;;;
;; Post Model ;;
;;;;;;;;;;;;;;;;

(struct post (blog id)
  #:mutable #:prefab)

(define (post-insert-comment! a-blog a-post a-comment)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO comments (pid, content) VALUES (?, ?)"
   (post-id a-post) a-comment))

(define (post-title a-post)
  (query-value
   (blog-db (post-blog a-post))
   "SELECT title FROM posts WHERE id = ?"
   (post-id a-post)))

(define (post-body a-post)
  (query-value
   (blog-db (post-blog a-post))
   "SELECT body FROM posts WHERE id = ?"
   (post-id a-post)))

(define (post-comments a-post)
  (query-list
   (blog-db (post-blog a-post))
   "SELECT content FROM comments WHERE pid = ?"
   (post-id a-post)))

;;;;;;;;;;
;; Data ;;
;;;;;;;;;;

(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)
