;;;; lyrips.lisp

(in-package #:lyrips) 
;; (ql:quickload '(:dexador :plump :lquery :lparallel))

(defun artist&song->metrolyrics-url (artist song)
  (let* ((->dash (lambda (x)  (values (cl-ppcre:regex-replace-all "\\s" x "-"))))
         (artist-metro (funcall ->dash artist))
         (song-metro (funcall ->dash song)))
    (format nil "http://www.metrolyrics.com/~a-lyrics-~a.html" song-metro artist-metro)))

(defun get-metrolyric (artist song)
  (let* ((request (dex:get (artist&song->metrolyrics-url artist song)))
         (parsed-content (plump:parse request))
         (lyric-vec (lquery:$  parsed-content "p.verse" (text))))
    (reduce (lambda (x acc)  (concatenate 'string x acc))  lyric-vec)))
