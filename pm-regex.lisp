(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-ppcre")

(defparameter *all-groups*
  (list "Aarau" "ABE" "Aberdeen" "Adelaide" "Advent" "Albany" "Albuquerque" "Amsterdam" "AmsterdamX" "Argentina" "Arnhem"
        "Athens" "Atlanta" "AtlanticCity" "Augsburg" "Austin" "Baltimore" "Bandung" "Bangalore" "Bangkok" "Barcelona"
        "Basel" "Bath" "Beijing" "Belgrade" "Bergen" "Berlin" "BH" "Bielefeld" "Birmingham" "Bordeaux" "Boston" "Boulder"
        "Braga" "BragancaPaulista" "Brasil" "Brasilia" "Bratislava" "Brisbane" "Bristol" "Brno" "Bruxelles" "Bucharest"
        "Budapest" "Buffalo" "CaFe" "Cali" "CAMEL" "Campinas" "CapeTown" "Caracas" "Cascavel" "Champaign-Urbana"
        "Chicago" "Chico" "China" "Chisinau" "Cincinnati" "Cleveland" "Cluj" "Cochin" "Coimbatore" "Coimbra" "Cologne"
        "Colombo" "Columbus" "Copenhagen" "CorpusChristi" "CostaRica" "Curitiba" "Dahut" "Darmstadt" "DC" "Delhi"
        "Denver" "DesMoines" "Devon and Cornwall" "DFW" "Dresden" "Duesseldorf" "Edinburgh" "Erlangen" "Fortaleza"
        "Frankfurt" "Fredericton" "Fukuoka" "Gainesville" "Gdansk" "Geneva" "Glasgow" "Gotanda" "Goteborg" "Granada"
        "Groningen" "Guatemala" "Guimaraes" "Hamburg" "Hannover" "Hardware" "Harrisburg" "Helsingborg" "Helsinki"
        "Hokkaido" "HongKong" "Houston" "HudsonValley" "Hyderabad" "Iasi" "Innsbruck" "Israel" "Italia" "Ithaca"
        "Jakarta" "Jerusalem" "Kaiserslautern" "Kamakura" "Kampala" "Kansai" "KansasCity" "Kathmandu" "Kerman" "Kiel"
        "Kielce" "Kiev" "Kolkata" "Kostroma" "Krasnodar" "Kushiro" "Kw" "Kyoto" "Leipzig" "Lima" "Linz" "Lisbon" "Logan"
        "London" "Los_Angeles" "Lukavac" "Lyon" "Madison" "Madras" "Madrid" "Madurai" "Makati" "Marseille" "Melbourne"
        "MexicoCity" "Milan" "MiltonKeynes" "Milwaukee" "Minsk" "Montréal" "Moscow" "Mumbai" "Munich" "Nagoya"
        "NewOrleans" "NewTaipeiCity" "Niederrhein" "Niigata" "Nordest" "Northwestengland" "Nottingham" "NY" "Odessa"
        "Omaha" "OrangeCounty" "Orlando" "Oslo" "Ottawa" "Paderborn" "Paris" "PDX" "Perth" "Petropolis" "Philadelphia"
        "Phoenix" "Pisa" "Pittsburgh" "Plovdiv" "Plzen" "Porto" "Poznan" "Prague" "Pune" "Purdue" "Qatar" "Quito"
        "Raleigh" "Recife" "Rehovot" "Rio" "Roederbergweg" "Roma" "RostovOnDon" "Rousse" "Ruhr" "Saarland" "Salem"
        "SaltLake" "SanCristobal" "SanDiego" "SanFrancisco" "Santa Fe-Los Alamos" "Santiago" "Sao-Paulo" "Seattle"
        "Sendai" "Seneca" "Seoul" "Shanghai" "Shibuya" "SiliconValley" "Singapore" "Sofia" "Sonoma" "Sophia"
        "Southampton" "SouthernOregon" "SPb" "StLouis" "Stockholm" "Stuttgart" "Swindon" "Sydney" "Szczecin"
        "Tallahassee" "Tallinn" "TelAviv" "Tempe" "Terere" "ThamesValley" "ThousandOaks" "Timisoara" "Tokyo" "Tomar"
        "Torino" "Toronto" "Toulouse" "TriCo" "Tucson" "UKCoordinators" "Ulm" "Vancouver" "Victoria" "Vienna"
        "Vlaanderen" "Vladivostok" "Voronezh" "Warszawa" "Wellington" "WesternMontana" "Weston" "WhitePlains" "Yokohama"
        "ZA" "Zagreb" "Zurich"))

(defun pick-with-accumulated-scores (accumulated-scores)
  (let* ((length (length accumulated-scores))
         (top (coerce (aref accumulated-scores (1- length)) 'double-float)))
    ;(format t "top: ~A, as:~%~A~%~%" top accumulated-scores)
    (if (> top 0)
        (let* ((r (random top))
               (a 0)
               (b length))
          (do ()
              ((>= a b) (values a r top))
            ;(format t "a: ~A, b ~A~%" a b)
            (let* ((pivot (floor (/ (+ a b) 2)))
                   (pivot-accumulated-score (aref accumulated-scores pivot)))
              (if (< pivot-accumulated-score r)
                  (setf a (1+ pivot))
                  (setf b pivot)))))
        (random length))))
        
(defparameter *alphabet*
  "abcdefghijklmnopqrstuvwxyz0123456789 !\"#$%&'()*+'-./:;<=>?@[\\]^_`")

(defun pick-from-alphabet ()
  (let* ((alphabet-length (length *alphabet*))
         (ix (random alphabet-length)))
    (values (aref *alphabet* ix) ix)))

(defun mutate-add-char (a)
  (let* ((length-a (length a))
         (ix (random length-a))
         (start-a (subseq a 0 ix))
         (middle (vector (pick-from-alphabet)))
         (end-a (subseq a ix length-a)))
    (concatenate 'string start-a middle end-a)))

(defun  mutate-overwrite-char (a)
  (let* ((length-a (length a))
         (ix (random length-a))
         (start-a (subseq a 0 ix))
         (middle (vector (pick-from-alphabet)))
         (end-a (subseq a (1+ ix) length-a)))
    (concatenate 'string start-a middle end-a)))

(defun mutate-delete-char (a)
  (let* ((length-a (length a))
         (ix (random length-a))
         (start-a (subseq a 0 ix))
         (end-a (subseq a (1+ ix) length-a)))
    (concatenate 'string start-a end-a)))

(defun cross (a b)
  (let* ((length-a (length a))
         (length-b (length b))
         (cut-a (1+ (random length-a)))
         (cut-b (random length-b))
         (start-a (subseq a 0 cut-a))
         (end-b (subseq b cut-b length-b)))
    (concatenate 'string start-a (vector (pick-from-alphabet)) end-b)))

(defun mutate (a)
  (ecase (random 3)
    (0 (mutate-add-char a))
    (1 (mutate-delete-char a))
    (2 (mutate-overwrite-char a))))

(defparameter *bad-scanner-score* 0.1)
(defparameter *factor-when-matching-good* 2)
(defparameter *factor-when-matching-bad* 1.1)
(defparameter *factor-pedigree* 0.5)
(defparameter *factor-length* #(1 1 1 1 0.95 .94 .93 .92 .91 .84 .83 .82 .81 .73 .72 .71 .62 .61 .5 .4 .3 .2 .1 .01))

(defparameter *factor-length-halve* 5)

(defun square (a) (* a a))

(defun factor-regex-length (regex)
  (let ((regex-length (length regex)))
    (exp (* (/ -0.6931472 *factor-length-halve*) regex-length))))

(defun score (regex goods bads)
  (let ((score 1)
        (scanner (ignore-errors (cl-ppcre:create-scanner regex))))
    (if scanner
        (progn
          (dolist (good goods)
            (when (cl-ppcre:scan scanner good)
              (setf score (* score *factor-when-matching-good*))))
              ;(format t "regex ~A matches ~A, score: ~A~%" regex good score)))
          (dolist (bad bads)
            (unless (cl-ppcre:scan scanner bad)
              (setf score (* score *factor-when-matching-bad*)))))
              ;(format t "regex ~A bad-matches ~A, score: ~A~%" regex bad score))))
        (setf score *bad-scanner-score*))
    (coerce (* (square score) (factor-regex-length regex)) 'double-float)))

(defun generate (population accumulated-scores goods bads)
  (let* ((a (aref population (pick-with-accumulated-scores accumulated-scores)))
         (a-car (car a)))
    ; (format t "a: ~A~%" a)
    (multiple-value-bind (child parents-score)
        (if (= 0 (random 2))
            (let* ((b (aref population (pick-with-accumulated-scores accumulated-scores)))
                   (b-car (car b)))
              ; (format t "b: ~A~%" b)
              (values (cross a-car b-car) (/ (+ (/ (cdr a) (factor-regex-length a-car))
                                                (/ (cdr b) (factor-regex-length b-car)))
                                             2)))
            (values (mutate a-car) (/ (cdr a) (factor-regex-length a-car))))
      (if (> (length child) 0)
          (let ((score (score child goods bads)))
            (cons child (max score (* *factor-pedigree* parents-score
                                      (factor-regex-length child)))))
          nil))))

(defun append-child (population accumulated-scores child)
  (when child
    (let* ((ix (length accumulated-scores))
           (top (if (<= ix 0) 0 (aref accumulated-scores (1- ix))))
           (squared-score (square (cdr child))))
      (vector-push-extend child population)
      (vector-push-extend (+ top squared-score) accumulated-scores))))

(defun sum-scores (population)
  (let ((length (length population))
        (acu 0))
    (dotimes (i length acu)
      (incf acu (cdr (aref population i))))))

(defun pick-n-as-scores (population n)
  (let ((length (length population))
        (top (coerce (sum-scores population) 'double-float))
        (remaining n)
        (population-out (make-array n :fill-pointer 0 :adjustable t))
        (accumulated-scores-out (make-array n :fill-pointer 0 :adjustable t)))
    (dotimes (i length (values population-out accumulated-scores-out))
      (when (> remaining 0)
        (let* ((element (aref population i))
               (score (cdr element)))
          (when (< (random top) (* score remaining))
            (append-child population-out accumulated-scores-out element)
            (decf remaining))
          (decf top score))))))

(defun show-n (population goods bads n)
  (dotimes (i n)
    (let ((element (aref population i)))
      ; (format t "element ~A~%" element)
      (let* ((regex (car element))
             (score (cdr element))
             (scanner (ignore-errors (cl-ppcre:create-scanner regex)))
             (my-goods)
             (my-bads))
        (when scanner
          (dolist (good goods)
            (when (cl-ppcre:scan scanner good)
              (setf my-goods (cons good my-goods))))
          (dolist (bad bads)
            (when (cl-ppcre:scan scanner bad)
              (setf my-bads (cons bad my-bads)))))
        (format t "i: ~A, regex: >~A<, score: ~A~%goods: ~A~%bads: ~A~%"
                i regex score my-goods my-bads))))
    (format t "~%"))

(defun evolve-impl (population accumulated-scores goods bads low high)
  (do ((n (length population) (length population)))
      ((>= n high))
    (append-child population accumulated-scores
                  (generate population accumulated-scores goods bads)))
  (pick-n-as-scores population low))

(defun evolve-n-impl (population accumulated-scores goods bads low high n)
  (if (= n 0)
      population
      (multiple-value-bind (population-2 accumulated-scores-2)
          (evolve-impl population accumulated-scores goods bads low high)
        (show-n population goods bads 6)
        (evolve-n-impl population-2 accumulated-scores-2 goods bads low high (1- n)))))

(defun evolve-n (population-list goods low high n)
  (let* ((bads (set-difference *all-groups* goods))
         (population (make-array 10 :fill-pointer 0 :adjustable t))
         (accumulated-scores (make-array 10 :fill-pointer 0 :adjustable t)))
    (dolist (string population-list)
      (let ((score (score string goods bads)))
        (append-child population accumulated-scores (cons string score))))
    (evolve-n-impl population accumulated-scores goods bads low high n)))

