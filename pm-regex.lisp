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

(defparameter *alphabet*
  "abcdefghijklmnopqrstuvwxyz0123456789 !\"#$%&'()*+'-./:;<=>?@[\\]^_`")

(defparameter *bad-scanner-score* 0.1)
(defparameter *factor-when-matching-good* 2)
(defparameter *factor-when-matching-bad* 1.1)
(defparameter *factor-pedigree* 0.5)
(defparameter *factor-length-halve* 5)

(defstruct (evolver
             (:constructor make-evolver-internal))
  (old-generation (make-array 100 :fill-pointer 0 :adjustable t)
                  :type (vector))
  (new-generation (make-array 100 :fill-pointer 0 :adjustable t)
                  :type (vector))
  (old-scores (make-array 100 :fill-pointer 0 :adjustable t :element-type 'double-float)
              :type (vector double-float))
  (old-accumulated-scores (make-array 100 :fill-pointer 0 :adjustable t :element-type 'double-float)
              :type (vector double-float))
  (new-scores (make-array 100 :fill-pointer 0 :adjustable t :element-type 'double-float)
              :type (vector double-float))
  (scorer nil :type (function (t) (double-float)))
  crosser
  mutator
  populator)


(defun evolver-old-accumulated-scores-reset (evolver)
  (setf (fill-pointer (evolver-old-accumulated-scores evolver)) 0))

(defun evolver-old-scores-reset (evolver)
  (evolver-old-accumulated-scores-reset evolver)
  (setf (fill-pointer (evolver-old-scores evolver)) 0))

(defun evolver-old-generation-reset (evolver)
  (evolver-old-scores-reset evolver)
  (setf (fill-pointer (evolver-old-generation evolver)) 0))

(defun evolver-seed (evolver size)
  (evolver-old-generation-reset evolver)
  (with-slots (old-generation populator) evolver
    (do ()
        ((>= (length old-generation) size))
      (let ((element (funcall populator)))
        (when element
          (vector-push-extend element old-generation))))))

(defun evolver-old-generation-set (evolver population)
  (evolver-old-generation-reset evolver)
  (with-slots (old-generation) evolver
    (dotimes (i (length population))
      (let ((element (aref population i)))
        (when element
          (vector-push-extend element old-generation))))))

(defun make-evolver (&key scorer crosser mutator
                       populator population population-size)
  (let ((evolver (make-evolver-internal :scorer scorer :crosser crosser :mutator mutator
                                        :populator populator)))
    (if population
        (evolver-old-generation-set evolver population)
        (if populator
            (evolver-seed evolver (or population-size 1000))
            (error "Both population and populator arguments are missing")))
    evolver))

(defun evolver-old-scores-init (evolver)
  (evolver-old-scores-reset evolver)
  (with-slots (old-generation old-scores scorer) evolver
    (dotimes (i (length old-generation))
      (let* ((element (aref old-generation i))
             (score (funcall scorer element)))
        (vector-push-extend score old-scores)))))

(defun evolver-old-scores* (evolver)
  (with-slots (old-scores) evolver
    (when (= (length old-scores) 0)
      (evolver-old-scores-init evolver))
    old-scores))

(defun evolver-old-accumulated-scores-init (evolver)
  (evolver-old-accumulated-scores-reset evolver)
  (let ((old-scores (evolver-old-scores* evolver))
        (old-accumulated-scores (evolver-old-accumulated-scores evolver))
        (acu 0.0))
    (dotimes (i (length old-scores))
      (let ((old-score (aref old-scores i)))
        (incf acu old-score)
        (vector-push-extend old-accumulated-scores acu)))))

(defun evolver-old-accumulated-scores* (evolver)
  (with-slots (old-accumulated-scores) evolver
    (when (= (length old-accumulated-scores) 0)
      (evolver-old-accumulated-scores-init evolver))
    old-accumulated-scores))



(defun pick-from-alphabet ()
  (let* ((alphabet-length (length *alphabet*))
         (ix (random alphabet-length)))
    (values (aref *alphabet* ix) ix)))

(defun binary-search-sorted-vector-index (accumulated-scores r)
  (do ((a 0)
       (b (length accumulated-scores)))
      ((>= a b) a)
    (let ((pivot (floor (/ (+ a b) 2))))
      (if (> (aref accumulated-scores pivot) r)
          (setf a (1+ pivot))
          (setf b pivot)))))

(defun evolver-pick-old-element-weighted WIP!)

(defun vector-top (vector)
  (let (length ((length vector)))
    (if (> length 0)
        (aref vector (1- length))
        nil)))

(defun pick-with-accumulated-scores (accumulated-scores)
  (let ((top (vector-top accumulated-scores)))
    (if (> top 0)
        (binary-search-sorted-vector-index accumulated-scores (random top))
        (random (length accumulated-scores)))))



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
