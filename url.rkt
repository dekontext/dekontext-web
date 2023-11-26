#lang web-server/insta

(define (start request)
  (phase-1 request))

(define (phase-1 request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (body (h1 "Phase 1")
             (a ((href ,(embed/url phase-2)))
                "Click me!")))))
  (send/suspend/dispatch response-generator))

(define (phase-2 request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (body (h1 "Phase 2")
             (a ((href ,(embed/url phase-1)))
                "Click me?")))))
  (send/suspend/dispatch response-generator))
