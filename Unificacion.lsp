;Ejemplo de como ejecutar:
;
;	(setf clausula2 '(p (A) (B)))
;	(setf clausula1 '(p (? x) (? x)))
;	(setf solucion (unificar clausula1 clausula2))
;	(print '(La solucion es))
;	(print solucion)

(setf false 'False)
(setf nada 'Nada)


(defun isAtom(var)
	(cond ((atom var) T)
		  ((eq (first var) '?) T)
		  (T NIL)
	)
)


(defun isVariable(var)
	(cond ((and (listp var) (= (length var) 2) (eq (first var) '?)) T)
		  (T NIL)
	)
)


(defun insertAfter (lst index newEle)
	(push newEle (cdr (nthcdr index lst)))
)


(defun build(tem1 tem2)
	(prog (result var1 var2)
		(setf var1 tem1)
		(setf var2 tem2)
		(setf result (list var1 var2))
		(insertAfter result 0 '/)
		(setf result (rest (list T result)))
		(return result)
	)
)


(defun aplicar(z lista)
        (prog (myZ result numElementos elemento)
                (setf myZ z)
                (when (eq myZ nada)
                	(return lista)
                )
                (setf result lista)
                (loop for elemento in myZ do
                        (setf sustituyente (first elemento))
                        (setf sustituido (nth 2 elemento))
                        (setf result (subst sustituyente sustituido result :test #'equal))
                )

                (return result)
        )
)


(defun componer(s1 s2)
        (prog (myS1 myS2 elementoS1 elementoS2 numeradorMyS1 denominadorMyS1 numeradorMyS2 denominadorMyS2 exist)
                (setf myS1 s1)
                (setf myS2 s2)

                (when (eq myS1 nada)
                	(return myS2)
                )

                (when (eq myS2 nada)
                	(return myS1)
                )

                (loop for elementoS2 in myS2 do
                        (setf numeradorMyS2 (first elementoS2))
                        (setf denominadorMyS2 (nth 2 elementoS2))
                        (setf exist '0)

                        (loop for elementoS1 in myS1 do
                                (setf numeradorMyS1 (first elementoS1))
                                (setf denominadorMyS1 (nth 2 elementoS1))

                                (if (equalp denominadorMyS2 denominadorMyS1)
                                        (setf exist '1)
                                )
                        )

                        (when (eq exist '0)
                                (loop for elementoS1 in myS1 do
                                        (setf myS1 (subst numeradorMyS2 denominadorMyS2 myS1 :test #'equal))
                                )
                                (insertAfter myS1 (- (length myS1) 1) elementoS2)
                        )
                )
                (return myS1)
        )
)


(defun unificar(h1 h2)
	(prog (e1 e2 temp result b f1 f2 t1 t2 g1 g2 z1 z2)
		(setf e1 h1)
		(setf e2 h2)

		(when (or (isAtom e1) (isAtom e2))
			(unless (isAtom e1)
				(print 'Intercambio)
				(setf temp e1)
				(setf e1 e2)
				(setf e2 temp)
			)

			(when (equalp e1 e2)
				(return nada) 
			)

			(when (isVariable e1)
				(when (member e1 e2)
					(return false) 
				)
				(setf b (build e2 e1))
				(return b)
			)

			(when (isVariable e2)
				(setf b (buid e1 e2))
				(return b)
			)

			(return false)
		)

		(setf f1 (first e1))
		(setf t1 (rest e1))
		(setf f2 (first e2))
		(setf t2 (rest e2))
		(setf z1 (unificar f1 f2))

		(when (eq z1 false)
			(print '(z1 vale false))
			(return false)
		)

		(setf g1 (aplicar z1 t1))
		(setf g2 (aplicar z1 t2))	
		(setf z2 (unificar g1 g2))

		(when (eq z2 false)
			(return false)
		)

		(return (componer z1 z2))
	)
)


	(setf clausula2 '(p (A) (B)))
	(setf clausula1 '(p (? x) (? y)))
	(setf solucion (unificar clausula1 clausula2))
	(print '(La solucion es))
	(print solucion)