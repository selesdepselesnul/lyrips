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
  (let ((html-string (dex-get artist
                              song
                              f-url-creator)))
    (get-lyric html-string selector f-html-lyric-mapper)))

(defun metrolyrics-url-creator (artist song)
  (let ((artist-metro (space->- artist))
        (song-metro (space->- song)))
    (format nil "http://www.metrolyrics.com/~a-lyrics-~a.html" song-metro artist-metro)))

(defun metrolyrics-mapper (xs)
  (reduce (lambda (x acc)  (concatenate 'string x acc)) xs))

(defun get-metrolyrics (artist song)
  (get-lyric-with-dex artist
                      song
                      #'metrolyrics-url-creator
                      "p.verse"
                      #'metrolyrics-mapper))

(defun azlyrics-url-creator (artist song)
  (let* ((->trim-all (lambda (x) (cl-ppcre:regex-replace-all " +" x "")))
         (artist-az (funcall ->trim-all artist))
         (song-az (funcall ->trim-all song)))
    (format nil "https://www.azlyrics.com/lyrics/~a/~a.html" artist-az song-az)))

(defun azlyrics-mapper (xs)
  (values
   (cl-ppcre:regex-replace-all
    ".*ba.*"
    (cl-ppcre:regex-replace
     "(?s)if  \\(.*"
     (cl-ppcre:regex-replace
      "(?s).+Usage of azlyrics\.com .+ Sorry about that\."
      (elt xs 3)"")"")"")))

(defun get-azlyrics (artist song)
  (get-lyric-with-dex artist
                      song
                      #'azlyrics-url-creator
                      "div.text-center" 
                      #'azlyrics-mapper))

;; (with-open-file (str file-path
;;                      :direction :output
;;                      :if-exists :supersede
;;                      :if-does-not-exist :create)
;;   (format str large-string))

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

(defun get-genius (artist song)
  (get-lyric-with-dex artist
                      song
                      #'genius-url-creator
                      "div.lyrics"
                      #'genius-mapper))

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

(defun get-wikialyric (artist song)
  (get-lyric-with-dex artist
                      song
                      #'wikialyric-url-creator
                      "div.lyricbox"
                      #'wikialyric-mapper))

(defun songlyrics-url-creator (artist song)
  (format nil
          "http://www.songlyrics.com/~a/~a-lyrics/"
          (space->- artist)
          (space->- song)))

(defun songlyrics-mapper (xs)
  (values
   (reduce (lambda (x acc) (concatenate 'string x acc)) xs)))

(defun get-songlyrics (artist song)
  (get-lyric-with-dex artist
                      song
                      #'songlyrics-url-creator
                      "#songLyricsDiv"
                      #'songlyrics-mapper))
