#lang web-server/insta

(require "models.rkt")

(define (start request)
   (render-blog-page
    (initialize-blog!
     (build-path (find-system-path 'home-dir)
                 "the-blog.db"))
     request))

(define (render-blog-page a-blog request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My blog"))
            (link ((rel "stylesheet")
                   (href "/main.css")
                   (type "text/css")))
            (body (h1 "My blog")
                  ,(render-posts a-blog embed/url)
                  (form ((action
                          ,(embed/url insert-post-handler))
                         (method "POST"))
                   (input ((name "title")))
                   (input ((name "body")))
                   (input ((type "submit"))))))))
  (define (insert-post-handler request)
    (blog-insert-post!
     a-blog
     (extract-binding/single 'title (request-bindings request))
     (extract-binding/single 'body (request-bindings request)))
    (render-blog-page a-blog (redirect/get)))
  (send/suspend/dispatch response-generator))

(define (render-post-detail-page a-blog a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Post Details"))
            (link ((rel "stylesheet")
                   (href "/main.css")
                   (type "text/css")))
            (body
             (h1 "Post Details")
             (h2 ,(post-title a-post))
             (p ,(post-body a-post))
             ,(render-as-itemized-list
               (post-comments a-post))
             (form ((action
                     ,(embed/url insert-comment-handler)))
                   (input ((name "comment")))
                   (input ((type "submit"))))))))
  (define (parse-comment bindings)
    (extract-binding/single 'comment bindings))
  (define (insert-comment-handler a-request)
    (post-insert-comment!
     a-blog a-post (parse-comment (request-bindings a-request)))
    (render-post-detail-page a-blog a-post (redirect/get)))
  (send/suspend/dispatch response-generator))

;; (define (render-comment comment)
;;   `(li ,comment))

(define (render-post a-blog a-post embed/url)
  (define (view-post-handler request)
    (render-post-detail-page a-blog a-post request))
  `(div ((class "post"))
        (a ((href ,(embed/url view-post-handler)))
           ,(post-title a-post))
        (p ,(post-body a-post))
        (div ,(number->string (length (post-comments a-post)))
             " comment(s)")))

(define (render-posts a-blog embed/url)
  (define (render-post/embed/url a-post)
    (render-post a-blog a-post embed/url))
  `(div ((class "posts"))
        ,@(map render-post/embed/url (blog-posts a-blog))))

(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
        (exists-binding? 'body bindings)))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))

(static-files-path "static")
