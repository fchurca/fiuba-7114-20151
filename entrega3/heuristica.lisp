; Esto nos va a ayudar a definir todos los sectores
(defmacro defsectores (&body body)
  (let
    ((solucion (gensym)))
    (labels
      ((sector (body) `(lambda (,solucion) ,(walk body)))
       (walk (body)
         (loop for clause in body collect
               (if (listp clause)
                 (walk clause)
                 (if (member clause '(and not or))
                   clause
                   `(getf ,solucion ',clause))))))
      `(list
         ,@(loop for (id evaluador) on body by #'cddr append
             `(',id ,(sector evaluador)))))))

(defun sector-satisfecho (sectores sector solucion)
  (or (eq sector (first solucion))
      (funcall (getf sectores sector) (second solucion))))

(defun partido-satisfecho (sectores solucion)
  (loop for (sector) on sectores by #'cddr
        if (sector-satisfecho sectores sector solucion)
        collect sector into satisfechos
        else
        collect sector into insatisfechos
        finally
        (return (values (null insatisfechos) satisfechos insatisfechos))))

(defun fitness (sectores solucion)
  (length (second (multiple-value-list
                   (partido-satisfecho sectores solucion)))))

(defun build-seed (partido asuntos &optional (generator (constantly nil)))
  `(,(first partido)
     ,(loop for asunto in asuntos appending
            `(,asunto ,(funcall generator)))))

(defun neighbours (solution)
  (loop for (sector) on sectores by #'cddr appending
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
                 to-visit (append (neighbours current) to-visit)))
          ((> fitness max-fitness)
           (format t " Candidate is better! Replacing pool~%")
           (setf candidates (list current)
                 to-visit (neighbours current)
                 max-fitness fitness)))))
    finally (return (values candidates max-fitness))))

; Definimos lista de asuntos
(defparameter asuntos
  '(huelga-empresarios diputados-juventud importaciones-abrir))

; Definimos lista de sectores
(defparameter sectores
  (defsectores
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
  (soluciones aptitud) (hillclimb sectores (build-seed sectores asuntos))
  (format t "Cantidad de soluciones: ~a~%" (length soluciones))
  (dolist (solucion soluciones)
    (multiple-value-bind
      (satisfecho satisfechos insatisfechos)
      (partido-satisfecho sectores solucion)
      (format t " Vicepresidencia: ~a~% Decisiones:~a~%"
              (first solucion) (second solucion))
      (format t "  Satisfechos: ~a~%  Insatisfechos: ~a~%"
              satisfechos insatisfechos))))

