;;;; package.lisp

(defpackage #:lyrips
  (:use #:cl)
  (:export #:get-metrolyrics
           #:get-azlyrics
           #:get-genius
           #:get-wikialyric
           #:get-songlyrics
           #:get-lyric-from-sites
           #:get-lyric-from-sites-async))

