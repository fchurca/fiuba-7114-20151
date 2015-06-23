; Esto nos va a ayudar a definir el problema
(defmacro make-partido (&body body)
  (let*
    ((solucion (gensym))
     (asuntos ())
     (sectores
       (labels
         ((sector (body) `(lambda (,solucion) ,(walk body)))
          (walk (body)
            (loop for clause in body collect
                  (if (listp clause)
                    (walk clause)
                    (if (member clause '(and not or))
                      clause
                      (progn
                        (unless (member clause asuntos)
                          (setf asuntos (cons clause asuntos)))
                        `(getf ,solucion ',clause)))))))
         `(list
            ,@(loop for (id evaluador) on body by #'cddr append
                `(',id ,(sector evaluador)))))))
    `(list ',asuntos ,sectores)))

(defun sector-satisfecho (sectores sector solucion)
  (or (eq sector (first solucion))
      (funcall (getf sectores sector) (second solucion))))

(defun partido-satisfecho (partido solucion)
  (let ((sectores (second partido)))
    (loop for (sector) on sectores by #'cddr
          if (sector-satisfecho sectores sector solucion)
          collect sector into satisfechos
          else
          collect sector into insatisfechos
          finally
          (return (values (null insatisfechos) satisfechos insatisfechos)))))

(defun fitness (sectores solucion)
  (length (second (multiple-value-list
                    (partido-satisfecho sectores solucion)))))

(defun build-seed (partido &optional (generator (constantly nil)))
  `(,(first (second partido))
     ,(loop for asunto in (first partido) appending
            `(,asunto ,(funcall generator)))))

(defun neighbours (problem solution)
  (loop for (sector) on (second problem) by #'cddr appending
        (loop for (asunto valor) on (second solution) by #'cddr collecting
              (let ((neighbour (copy-list (second solution))))
                (setf (getf neighbour asunto) (not valor))
                `(,sector ,neighbour)))))

(defun hillclimb (problem seed)
  (loop
    with visited = (make-hash-table :test #'equal)
    with max-fitness = 0
    with candidates = ()
    for to-visit = (list seed) then (rest to-visit)
    for current = (first to-visit)
    while to-visit do
    (unless (gethash current visited)
      (setf (gethash current visited) t)
      (let ((fitness (fitness problem current)))
        (format t "Candidate: ~a~% Fitness:~a~%"
                current fitness)
        (cond
          ((< fitness max-fitness)
           (format t " Candidate is worse. Skipped.~%"))
          ((= fitness max-fitness)
           (format t " Candidate is fit! Added to pool~%")
           (setf candidates (append (list current) candidates)
                 to-visit (append (neighbours problem current) to-visit)))
          ((> fitness max-fitness)
           (format t " Candidate is better! Replacing pool~%")
           (setf candidates (list current)
                 to-visit (neighbours problem current)
                 max-fitness fitness)))))
    finally (return (values candidates max-fitness))))

; Definimos lista de sectores
(defparameter sectores
  (make-partido
    corriente-historica
    (not (and huelga-empresarios
              diputados-juventud
              importaciones-abrir))
    juventud
    (and huelga-empresarios
         diputados-juventud)
    sindicatos
    (not (or huelga-empresarios
             importaciones-abrir))
    empresarios
    (or huelga-empresarios
        importaciones-abrir)))

; Corremos resolvedor e imprimimos todos los mejores candidatos
(multiple-value-bind
  (soluciones aptitud) (hillclimb sectores (build-seed sectores))
  (format t "Cantidad de soluciones encontradas: ~a~%" (length soluciones))
  (format t "Aptitud de las soluciones: ~a~%" aptitud)
  (dolist (solucion soluciones)
    (multiple-value-bind
      (satisfecho satisfechos insatisfechos)
      (partido-satisfecho sectores solucion)
      (format t " Vicepresidencia: ~a~% Decisiones:~a~%"
              (first solucion) (second solucion))
      (format t "  Satisfechos: ~a~%  Insatisfechos: ~a~%"
              satisfechos insatisfechos))))

