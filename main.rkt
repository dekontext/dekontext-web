#lang racket

(require web-server/servlet
         web-server/dispatch
         racket/place/distributed)

(provide/contract (start (request? . -> . response?)))

(require web-server/formlets
         "models.rkt")

(define the-blog
  (initialize-blog!
          (build-path (find-system-path 'home-dir)
                      "the-blog.db")))

(define (start request)
  (define-values (blog-dispatch blog-url)
    (dispatch-rules
     [("") (lambda (req)
             (render-blog-page the-blog req))]
     [("posts" (string-arg)) render-post-detail-page]
     [else render-blog-page]))
  (blog-dispatch request))

(define new-post-formlet-2
  (formlet
   (#%# (label "asod")
        ,((to-string
           (required
            (text-input)))
          . => . title)
        (label "asod")
        ,((to-string
           (required
            (text-input)))
          . => . body))
  (values title body)))


(define new-comment-formlet
  (formlet
   (#%# ,{input-string . => . comment})
   (values comment)))

(define (render-blog-page a-blog request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My blog"))
            (link ((rel "stylesheet")
                   (href "/main.css")
                   (type "text/css")))
            (body ((class "dkx")) (h1 "My blog")
                  ,(render-posts a-blog embed/url)
                  (form ([action
                          ,(embed/url insert-post-handler)])
                        ,@(formlet-display new-post-formlet-2)
                   (input ((type "submit"))))))))
  (define (insert-post-handler request)
    (define-values (title body)
      (formlet-process new-post-formlet-2 request))
    (blog-insert-post! a-blog title body)
    (render-blog-page a-blog (redirect/get)))
  (send/suspend/dispatch response-generator))

(define (render-post-detail-page a-blog a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Post Details"))
            (link ((rel "stylesheet")
                   (href "/main.css")
                   (type "text/css")))
            (body ((class "dkx"))
             (h1 "Post Details")
             (h2 ,(post-title a-post))
             (p ,(post-body a-post))
             ,(render-as-itemized-list
               (post-comments a-post))
             (form ([action
                     ,(embed/url insert-comment-handler)])
                   ,@(formlet-display new-comment-formlet)
                   (input ((type "submit"))))))))
  (define (parse-comment bindings)
    (extract-binding/single 'comment bindings))
  (define (insert-comment-handler a-request)
    (define-values (comment)
      (formlet-process new-comment-formlet a-request))
    (post-insert-comment!
     a-blog a-post comment)
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

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8008
               #:extra-files-paths
               (list (build-path (current-directory) "static"))
               #:servlet-path
               "/")

