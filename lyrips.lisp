;;;; lyrips.lisp

(in-package #:lyrips) 
;; (ql:quickload '(:dexador :plump :lquery :lparallel :cl-ppcre :cl-strings))

(defun space-> (str replacer)
  (values (cl-ppcre:regex-replace-all "\\s" str replacer)))

(defun space->- (x)
  (values (cl-ppcre:regex-replace-all "\\s" x "-")))

(defun dex-get (artist song f-url-creator)
  (dex:get (funcall f-url-creator artist song)))

(defun get-lyric (html-string selector f-html-lyric-mapper)
  (let* ((parsed-content (plump:parse html-string))
         (lyric-vec (lquery:$  parsed-content selector (text))))
    (funcall f-html-lyric-mapper lyric-vec)))

(defun get-lyric-with-dex (artist song f-url-creator selector f-html-lyric-mapper)
  (handler-case
      (let ((html-string (dex-get artist
                                  song
                                  f-url-creator)))
        (get-lyric html-string selector f-html-lyric-mapper))
    (dex:http-request-bad-request (e) nil)
    (dex:http-request-failed (e) nil)))

(defmacro def-lyric (name selector url-creator mapper)
  `(defun ,name (artist song)
     (get-lyric-with-dex artist
                         song
                         ,url-creator
                         ,selector
                         ,mapper)))
;;; metrolyrics
(defun metrolyrics-url-creator (artist song)
  (let ((artist-metro (space->- artist))
        (song-metro (space->- song)))
    (format nil "http://www.metrolyrics.com/~a-lyrics-~a.html" song-metro artist-metro)))

(defun metrolyrics-mapper (xs)
  (reduce (lambda (x acc)  (concatenate 'string x acc)) xs))

(def-lyric get-metrolyrics "p.verse" #'metrolyrics-url-creator #'metrolyrics-mapper)
;;;


;;; azlyrics
(defun azlyrics-url-creator (artist song)
  (let* ((->trim-all (lambda (x) (cl-ppcre:regex-replace-all " +" x "")))
         (artist-az (funcall ->trim-all artist))
         (song-az (funcall ->trim-all song)))
    (format nil "https://www.azlyrics.com/lyrics/~a/~a.html" artist-az song-az)))


(defun azlyrics-mapper (xs)
  (cl-ppcre:regex-replace-all
   ".*ba.*"
   (cl-ppcre:regex-replace
    "(?s)if  \\(.*"
    (cl-ppcre:regex-replace
     "(?s).+Usage of azlyrics\.com .+ Sorry about that\."
     (elt xs 3)"")"")""))

(def-lyric get-azlyrics "div.text-center" #'azlyrics-url-creator #'azlyrics-mapper)
;;;

;; (with-open-file (str file-path
;;                      :direction :output
;;                      :if-exists :supersede
;;                      :if-does-not-exist :create)
;;   (format str large-string))

;;; genius
(defun genius-url-creator (artist song)
  (let* ((artist-genius (space->- artist))
         (song-genius (space->- song)))
    (format nil "https://genius.com/~a-~a-lyrics" artist-genius song-genius)))

(defun genius-mapper (xs)
  (values
   (cl-ppcre:regex-replace "(?s).*Verse 1"
                           (cl-ppcre:regex-replace
                            "(?s)/sse.*/sse"
                            (reduce (lambda (x acc) (concatenate 'string x acc)) xs)
                            "")
                           "[Verse 1")))

(def-lyric get-genius "div.lyrics" #'genius-url-creator #'genius-mapper)
;;;

;;; wikialyric
(defun wikialyric-url-creator (artist song)
  (flet ((space->title-case-with-_ (x)
           (space-> (cl-strings:title-case x) "_")))
    (format nil
            "http://lyrics.wikia.com/wiki/~a:~a"
            (space->title-case-with-_ artist)
            (space->title-case-with-_ song))))

(defun wikialyric-mapper (xs)
  (values
   (reduce (lambda (x acc) (concatenate 'string x acc)) xs)))

(def-lyric get-wikialyric "div.lyricbox" #'wikialyric-url-creator #'wikialyric-mapper)
;;;

;;; songlyrics
(defun songlyrics-url-creator (artist song)
  (format nil
          "http://www.songlyrics.com/~a/~a-lyrics/"
          (space->- artist)
          (space->- song)))

(defun songlyrics-mapper (xs)
  (values
   (reduce (lambda (x acc) (concatenate 'string x acc)) xs)))

(def-lyric get-songlyrics "#songLyricsDiv" #'songlyrics-url-creator #'songlyrics-mapper)
;;;

(defun get-lyric-from-sites (f-xs artist song)
  (let ((func (car f-xs)))
    (when (not (null func))
      (let ((result (funcall func artist song)))
        (if (null result)
            (get-lyric-from-sites (cdr f-xs) artist song)
            result)))))

