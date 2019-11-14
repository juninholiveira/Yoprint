(vl-load-com)

; IMPRESSORAS
(setq pdfPlotter "DWG TO PDF.PC5")                                              ; Impressora para PDF
(setq brotherPlotter "Brother DCP-7065DN Printer")                              ; Impressora A4 Brother
(setq A3PlotterRede "\\\\Desk-interiores\\EPSON L1300 Series")                  ; Impressora colorida A3 Epson  BACKUP EPSON: \\\\Desk-interiores\\EPSON L1300 Series
(setq A3PlotterServidor "EPSON L1300 Series")

; ctb
(setq ctbmanual "ctb - paula e bruna.ctb")                                      ; Escreve aqui o CTB que vai ser usado pelo ZWCAD (OBS.: Não esquecer de colocar ".ctb" no final)

; PAPÉIS
(setq a4fullbleed "ISO FULL BLEED A4 (297.00 x 210.00 MM)")
(setq a3fullbleed "ISO FULL BLEED A3 (420.00 x 297.00 MM)")
(setq a2fullbleed "ISO FULL BLEED A2 (594.00 x 420.00 MM)")

(setq epsonA3 "A3 (297 x 420 mm)")
(setq epsonA4 "A4")                                                             ; BACKUP da folha da Epson: "A4 (210 x 297 mm)"

; BLOCOS DE FOLHA
(setq a4-20 "A4-20")
(setq a4-25 "A4-25")
(setq a4-50 "A4-50")
(setq a4-75 "A4-75")
(setq a4-100 "A4-100")
(setq a4-125 "A4-125")

(setq a3-20 "A3-20")
(setq a3-25 "A3-25")
(setq a3-50 "A3-50")
(setq a3-75 "A3-75")
(setq a3-100 "A3-100")
(setq a3-125 "A3-125")

(setq a2-20 "A2-20")
(setq a2-25 "A2-25")
(setq a2-50 "A2-50")
(setq a2-75 "A2-75")
(setq a2-100 "A2-100")
(setq a2-125 "A2-125")

(setvar "BACKGROUNDPLOT" 2)	                                                    ;Aqui eu seto a variável do sistema pra PLOT em foreground e PUBLISH em background (Número 2)

;Aui eu reseto os
(SETQ ORIGPATH (STRCAT (GETENV "ACAD")";"))
(SETQ ONEPATH (STRCAT "R:\\INTERIORES\\1.Padrão\\Padrão PB - AutoCad\\Plugins\\PluginsToLoad;R:\\INTERIORES\\1.Padrão\\Padrão PB - AutoCad\\Hachuras"));ADD PATHS HERE, NOT TO MANY OR IT GETS CUT OFF
(SETQ MYENV (STRCAT ORIGPATH ONEPATH))
(SETENV "ACAD" MYENV)
(strlen (getenv "ACAD"));DON'T GO OVER 800 OR BAD THINGS HAPPEN
; ****************************************************************************************************************************

;PRINTALLTOPDF ***************************************************************************************************************
(defun C:printalltopdf (/ dwg file hnd i len llpt lst mn mx ss tab urpt subfolder cpath newpath currententity scale)

  (setq p1 (getpoint "\nFaça a seleção das pranchas à serem impressas:"))
  (setq p2 (getcorner p1))

  (setq nomeescolhido(GetString T "\nDigite um nome para as pranchas:"))

    (if (setq ss (ssget "_C" p1 p2 '((0 . "INSERT") (2 . "A4-20,A4-25,A4-50,A4-75,A4-100,A4-125,A3-25,A3-50,A3-75,A3-100,A3-125,A2-25,A2-50,A2-75,A2-100,A2-125"))))
        (progn
            (repeat (setq i (sslength ss))
                (setq hnd (ssname ss (setq i (1- i)))
                      tab (cdr (assoc 410 (entget hnd)))
                      lst (cons (cons tab hnd) lst)
                )
            )
            (setq lst (vl-sort lst '(lambda (x y) (> (car x) (car y)))))
            (setq i 0)

            (foreach x lst

            ;Nesta parte, faço a lógica para decidir se a planta é Landscape ou Portrait,
            ;pegando o Bounding Box dela para fazer a matemática
            (vla-GetBoundingBox (vlax-ename->vla-object (cdr x)) 'minExt 'maxExt)
            (setq minExt (vlax-safearray->list minExt) maxExt (vlax-safearray->list maxExt))
            (setq orientation "Landscape")
            (if
              (<
                (- (nth 0 maxExt) (nth 0 minExt))
                (- (nth 1 maxExt) (nth 1 minExt))
              )
              (setq orientation "Portrait")
            )

              ;Pego o nome do bloco (A4025, A4-50, etc)
              (setq entityname (vla-get-effectivename (vlax-ename->vla-object (cdr x))))
              (if
                (= entityname "A4-20")
                (progn
                  (setq papersize a4fullbleed)
                  (setq escala "1=2")
                )
              )
                (if
                  (= entityname "A4-25")
                  (progn
                    (setq papersize a4fullbleed)
                    (setq escala "1=2.5")
                  )
                )
                (if
                  (= entityname "A4-50")
                  (progn
                    (setq papersize a4fullbleed)
                    (setq escala "1=5")
                  )
                )
                (if
                  (= entityname "A4-75")
                  (progn
                    (setq papersize a4fullbleed)
                    (setq escala "1=7.5")
                  )
                )
                (if
                  (= entityname "A4-100")
                  (progn
                    (setq papersize a4fullbleed)
                    (setq escala "1=10")
                  )
                )
                (if
                  (= entityname "A4-125")
                  (progn
                    (setq papersize a4fullbleed)
                    (setq escala "1=12.5")
                  )
                )

                ;A3
                (if
                  (= entityname "A3-25")
                  (progn
                    (setq papersize a3fullbleed)
                    (setq escala "1=2.5")
                  )
                )
                (if
                  (= entityname "A3-50")
                  (progn
                    (setq papersize a3fullbleed)
                    (setq escala "1=5")
                  )
                )
                (if
                  (= entityname "A3-75")
                  (progn
                    (setq papersize a3fullbleed)
                    (setq escala "1=7.5")
                  )
                )
                (if
                  (= entityname "A3-100")
                  (progn
                    (setq papersize a3fullbleed)
                    (setq escala "1=10")
                  )
                )
                (if
                  (= entityname "A3-125")
                  (progn
                    (setq papersize a3fullbleed)
                    (setq escala "1=12.5")
                  )
                )

                ;A2 com FitToPaper para sair na A3
                (if
                  (= entityname "A2-25")
                  (progn
                    (setq papersize a2fullbleed)
                    (setq escala "1=2.5")
                  )
                )
                (if
                  (= entityname "A2-50")
                  (progn
                    (setq papersize a2fullbleed)
                    (setq escala "1=5")
                  )
                )
                (if
                  (= entityname "A2-75")
                  (progn
                    (setq papersize a2fullbleed)
                    (setq escala "1=7.5")
                  )
                )
                (if
                  (= entityname "A2-100")
                  (progn
                    (setq papersize a2fullbleed)
                    (setq escala "1=10")
                  )
                )
                (if
                  (= entityname "A2-125")
                  (progn
                    (setq papersize a2fullbleed)
                    (setq escala "1=12.5")
                  )
                )

;Faço a seleção da área da prancha
(vla-getboundingbox (vlax-ename->vla-object (cdr x)) 'mn 'mx)
                (setq llpt (vlax-safearray->list mn)
                      urpt (vlax-safearray->list mx)
                      len  (distance llpt (list (car urpt) (cadr llpt)))
                )

; Aqui embaixo eu crio uma Subpasta, onde será salvo o Output
; Var "SUBFOLDER" indica o nome da pasta a ser criada
		(setq subfolder "PDFs")
		(setq cpath (getvar "dwgprefix"))
		(Setq newpath (strcat cpath subfolder))
		(if (not (findfile newpath))(vl-mkdir newpath))

                (setq file (strcat newpath "\\" nomeescolhido " " (substr (setq ptx (rtos (cadr urpt) 2 0)) 1 5) " - " (substr (setq ptx (rtos (car urpt) 2 0)) 1 5) ".pdf"))

                ;Deleto arquivos antigos
                (if (findfile file) (vl-file-delete file))

                (command "-plot"
                         "yes"
                         (car x)
                         pdfplotter
                         papersize
                         "Millimeters"
                         orientation
                         "No"
                         "Window"
                         llpt
                         urpt
                         escala
                         "Center"
                         "yes"
                         ctbmanual
                         "yes"
                         ""
                )

                (if (/= (car x) "Model")
                    (command "No" "No" file "no" "Yes")
                    (command
                        file
                        "no"
                        "Yes"
                    )
                )
            );foreach
            (setq lst nil)
        );progn
    );if
    (princ)
);defun
;PRINTALLTOPDF ***************************************************************************************************************

;PRINTALLA3 ***************************************************************************************************************
(defun C:printalla3 (/ dwg file hnd i len llpt mn mx  tab urpt subfolder cpath newpath currententity scale
		       lst2
		       ss2
		    )

(initget "A4-25 A4-50 A4-75 A4-100 A4-125 A3-50 A3-75 A3-100 A3-125 A2-50 A2-75 A2-100 A2-125")
(setq blocksize (cond ( (getkword "\nChoose [A4-25/A4-50/A4-75/A4-100/A4-125/A3-50/A3-75/A3-100/A3-125/A2-50/A2-75/A2-100/A2-125] <A3-100>: ") ) ( "A3-100" )))


(if (setq ss2 (ssget "_X" (list '(0 . "INSERT") (cons 2 blocksize))))
        (progn
            (repeat (setq i (sslength ss2))
                (setq hnd (ssname ss2 (setq i (1- i)))
                      tab (cdr (assoc 410 (entget hnd)))
                      lst2 (cons (cons tab hnd) lst2)
                )
            )
            (setq lst2 (vl-sort lst2 '(lambda (x y) (> (car x) (car y)))))
            (setq i 0)

			(setq count 0)

			(repeat 8

        (setq PlotThis "0")

				(if (= count 0) (setq none(layout)))
				(if (= count 1) (setq none(hidraulico)))
				(if (= count 2) (setq none(eletrico)))
				(if (= count 3) (setq none(luminotecnico)))
				(if (= count 4) (setq none(secoes)))
				(if (= count 5) (setq none(forro)))
				(if (= count 6) (setq none(piso)))
				(if (= count 7) (setq none(arcondicionado)))

				(if (= count 0) (setq none(changecolorsback)))
				(if (= count 1) (setq none(changecolorstogrey)))
				(if (= count 2) (setq none(changecolorstogrey)))
				(if (= count 3) (setq none(changecolorstogrey)))
				(if (= count 4) (setq none(changecolorstogrey)))
				(if (= count 5) (setq none(changecolorsback)))
				(if (= count 6) (setq none(changecolorsback)))
				(if (= count 7) (setq none(changecolorstogrey)))

				(setq nomedaplanta "error")
				(if (= count 0) (setq nomedaplanta "Layout"))
				(if (= count 1) (setq nomedaplanta "Hidráulico"))
				(if (= count 2) (setq nomedaplanta "Elétrico"))
				(if (= count 3) (setq nomedaplanta "Luminotécnico"))
				(if (= count 4) (setq nomedaplanta "Seções"))
				(if (= count 5) (setq nomedaplanta "Forro"))
				(if (= count 6) (setq nomedaplanta "Piso"))
				(if (= count 7) (setq nomedaplanta "Ar Condicionado"))

				(setq escala "Fit")
        (if (= blocksize "A4-25") (setq escala "1=2.5"))
				(if (= blocksize "A4-50") (setq escala "1=5"))
				(if (= blocksize "A4-75") (setq escala "1=7.5"))
				(if (= blocksize "A4-100") (setq escala "1=10"))
				(if (= blocksize "A4-125") (setq escala "1=12.5"))

				(if (= blocksize "A3-50") (setq escala "1=5"))
				(if (= blocksize "A3-75") (setq escala "1=7.5"))
				(if (= blocksize "A3-100") (setq escala "1=10"))
				(if (= blocksize "A3-125") (setq escala "1=12.5"))

				(if (= blocksize "A2-50") (setq escala "1=5"))
				(if (= blocksize "A2-75") (setq escala "1=7.5"))
				(if (= blocksize "A2-100") (setq escala "1=10"))
				(if (= blocksize "A2-125") (setq escala "1=12.5"))

				(setq papersize "Fit")
        (if (= blocksize "A4-25") (setq papersize a4fullbleed))
				(if (= blocksize "A4-50") (setq papersize a4fullbleed))
				(if (= blocksize "A4-75") (setq papersize a4fullbleed))
				(if (= blocksize "A4-100") (setq papersize a4fullbleed))
				(if (= blocksize "A4-125") (setq papersize a4fullbleed))
				(if (= blocksize "A3-50") (setq papersize a3fullbleed))
				(if (= blocksize "A3-75") (setq papersize a3fullbleed))
				(if (= blocksize "A3-100") (setq papersize a3fullbleed))
				(if (= blocksize "A3-125") (setq papersize a3fullbleed))
				(if (= blocksize "A2-50") (setq papersize a2fullbleed))
				(if (= blocksize "A2-75") (setq papersize a2fullbleed))
				(if (= blocksize "A2-100") (setq papersize a2fullbleed))
				(if (= blocksize "A2-125") (setq papersize a2fullbleed))


            (foreach x lst2

            ;Nesta parte, faço a lógica para decidir se a planta é Landscape ou Portrait,
            ;pegando o Bounding Box dela para fazer a matemática
            (vla-GetBoundingBox (vlax-ename->vla-object (cdr x)) 'minExt 'maxExt)
            (setq minExt (vlax-safearray->list minExt) maxExt (vlax-safearray->list maxExt))
            (setq orientation "Landscape")
            (if
              (<
                (- (nth 0 maxExt) (nth 0 minExt))
                (- (nth 1 maxExt) (nth 1 minExt))
              )
              (setq orientation "Portrait")
            )

; ---------------
;Faço a seleção da área da prancha
(vla-getboundingbox (vlax-ename->vla-object (cdr x)) 'mn 'mx)
                (setq llpt (vlax-safearray->list mn)
                      urpt (vlax-safearray->list mx)
                      len  (distance llpt (list (car urpt) (cadr llpt)))
                )
; ----------------

  ; Aqui embaixo eu crio uma Subpasta, onde será salvo o Output
  ; Var "SUBFOLDER" indica o nome da pasta a ser criada
  (setq subfolder "Plantas Gerais")
  (setq cpath (getvar "dwgprefix"))
  (Setq newpath (strcat cpath subfolder))
  (if (not (findfile newpath))(vl-mkdir newpath))

  (setq file (strcat newpath "\\" nomedaplanta " - " (substr (setq ptx (rtos (car urpt) 2 0)) 1 5) ".pdf"))

  ;Deleto arquivos antigos
  (if (findfile file) (vl-file-delete file))

; ---------------
                (command "-plot"
                         "yes"
                         (car x)
                         pdfplotter
                         papersize
                         "Millimeters"
                         orientation
                         "No"
                         "Window"
                         llpt
                         urpt
                         escala
                         "Center"
                         "yes"
                         ctbmanual
                         "yes"
                         ""
                )

                (if (/= (car x) "Model")
                    (command "No" "No" file "no" "Yes")
                    (command
                        file
                        "no"
                        "Yes"
                    )
                )
            ) ;foreach


            ;;Adiciono mais um na var COUNT pra lógica funcionar lá em cima
						(setq count (1+ count))
					) ;repeat
          (setq lst2 nil)
        ) ;progn
    ) ;if

(command "_laythw")     ;EXIBE TODAS AS LAYERS NOVAMENTE
(setq none(changecolorsback))

) ;end defun
;PRINTALLA3 ***************************************************************************************************************

;PRINTSINGLESHEET ***************************************************************************************************************
(defun printsinglesheet()

;(initget "Servidor Rede")
;(setq tipoDoComputador (cond ( (getkword "\nChoose [Servidor/Rede] <Rede>: ") ) ( "Rede" )))

  ;LÓGICA PARA DECIDIR SE ESTÁ NO SERVIDOR OU NA REDE E ESCOLHER A IMPRESSORA CERTA
  ;Pega todas as Plotters e armazena na lista "plottersList"
  (setq ad (vla-get-activedocument (vlax-get-acad-object)))
  (vla-RefreshPlotDeviceInfo (vla-get-activelayout ad))
  (setq plottersList (vlax-safearray->list (vlax-variant-value (vla-getplotdevicenames (vla-item (vla-get-layouts ad) "Model")))))
  (setq plotter A3PlotterRede)
  (foreach a plottersList
    (if (= a A3PlotterServidor) (setq plotter A3PlotterServidor))
  );END foreach)

;Seleciona todas as pranchas
(setq p1 (getpoint "\nFaça a seleção das pranchas à serem impressas:"))
(setq p2 (getcorner p1))

(if (setq ss (ssget "_C" p1 p2 '((0 . "INSERT") (2 . "A4-20,A4-25,A4-50,A4-75,A4-100,A4-125,A3-25,A3-50,A3-75,A3-100,A3-125,A2-25,A2-50,A2-75,A2-100,A2-125"))))
    (progn
        (repeat (setq i (sslength ss))
            (setq hnd (ssname ss (setq i (1- i)))
                  tab (cdr (assoc 410 (entget hnd)))
                  lst (cons (cons tab hnd) lst)
            )
        )
        (setq lst (vl-sort lst '(lambda (x y) (> (car x) (car y)))))
        (setq i 0)

        (foreach x lst

        ;Nesta parte, faço a lógica para decidir se a planta é Landscape ou Portrait,
        ;pegando o Bounding Box dela para fazer a matemática
        (vla-GetBoundingBox (vlax-ename->vla-object (cdr x)) 'minExt 'maxExt)
        (setq minExt (vlax-safearray->list minExt) maxExt (vlax-safearray->list maxExt))
        (setq orientation "Landscape")
        (if
          (<
            (- (nth 0 maxExt) (nth 0 minExt))
            (- (nth 1 maxExt) (nth 1 minExt))
          )
          (setq orientation "Portrait")
        )

        (setq entityname (vla-get-effectivename (vlax-ename->vla-object (cdr x))))

        (if
          (= entityname "A4-20")
          (progn
            (setq papersize epsonA4)
            (setq escala "1=2")
          )
        )
          (if
            (= entityname "A4-25")
            (progn
              (setq papersize epsonA4)
              (setq escala "1=2.5")
            )
          )
          (if
            (= entityname "A4-50")
            (progn
              (setq papersize epsonA4)
              (setq escala "1=5")
            )
          )
          (if
            (= entityname "A4-75")
            (progn
              (setq papersize epsonA4)
              (setq escala "1=7.5")
            )
          )
          (if
            (= entityname "A4-100")
            (progn
              (setq papersize epsonA4)
              (setq escala "1=10")
            )
          )
          (if
            (= entityname "A4-125")
            (progn
              (setq papersize epsonA4)
              (setq escala "1=12.5")
            )
          )

          ;A3
          (if
            (= entityname "A3-25")
            (progn
              (setq papersize epsonA3)
              (setq escala "1=2.5")
            )
          )
          (if
            (= entityname "A3-50")
            (progn
              (setq papersize epsonA3)
              (setq escala "1=5")
            )
          )
          (if
            (= entityname "A3-75")
            (progn
              (setq papersize epsonA3)
              (setq escala "1=7.5")
            )
          )
          (if
            (= entityname "A3-100")
            (progn
              (setq papersize epsonA3)
              (setq escala "1=10")
            )
          )
          (if
            (= entityname "A3-125")
            (progn
              (setq papersize epsonA3)
              (setq escala "1=12.5")
            )
          )

          ;A2 com FitToPaper para sair na A3
          (if
            (= entityname "A2-25")
            (progn
              (setq papersize epsonA3)
              (setq escala "Fit")
            )
          )
          (if
            (= entityname "A2-50")
            (progn
              (setq papersize epsonA3)
              (setq escala "Fit")
            )
          )
          (if
            (= entityname "A2-75")
            (progn
              (setq papersize epsonA3)
              (setq escala "Fit")
            )
          )
          (if
            (= entityname "A2-100")
            (progn
              (setq papersize epsonA3)
              (setq escala "Fit")
            )
          )
          (if
            (= entityname "A2-125")
            (progn
              (setq papersize epsonA3)
              (setq escala "Fit")
            )
          )
          ; ******************************************************************************
                      (vla-getboundingbox (vlax-ename->vla-object (cdr x)) 'mn 'mx)
                            (setq llpt (vlax-safearray->list mn)
                                  urpt (vlax-safearray->list mx)
                                  len  (distance llpt (list (car urpt) (cadr llpt)))
                            )


          ; ---------------
                          (command "-plot"
                                   "yes"
                                   (car x)
                                   plotter
                                   papersize
                                   "Millimeters"
                                   orientation
                                   "No"
                                   "Window"
                                   llpt
                                   urpt
                                   escala
                                   "Center"
                                   "yes"
                                   ctb
                                   "yes"
                                   ""
                          )

;                          (setq currentItemInList (+ currentItemInList 1))
;                          (if
;                            (= currentItemInList lstLength)
;                            (command "No" "No" "Yes")
;                            (command "No" "No" "Yes")
;                          )
                          (command "No" "No" "Yes")



                          ;(if (/= (car x) "Model")
                          ;    (command "No" "No" "Yes")
                          ;)

        );foreach
        (setq lst nil)
    );progn

);if
);defun
;PRINTSINGLESHEET ***************************************************************************************************************

;FUNÇÕES DE SUPORTE ***************************************************************************************************************

;MÉTODO PARA TROCAR COR DOS LAYOUTS PARA CINZA
(defun changecolorstogrey()
	(command "_.layer" "_thaw" "1 Layout 01,1 Layout 02,1 Layout 03" "")
	(command "_.layer" "_color" 252 "1 Layout 01,1 Layout 02,1 Layout 03" "")
)

;MÉTODO PARA TROCAR COR DOS LAYOUTS DE VOLTA PARA VERMELHO E AMARELO
(defun changecolorsback()
	(command "_.layer" "_color" 1 "1 Layout 01" "")
	(command "_.layer" "_color" 2 "1 Layout 02" "")
	(command "_.layer" "_color" 3 "1 Layout 03" "")
)

; MOSTRAR LAYOUT
(defun layout ()
	(command "setvar" "clayer" "0")   ;Seta a Layer Atual como Layer 0
	(command "_laythw")               ;Exibe todas as layers para depois apagar as específicas
	(command "_.layer" "_freeze" "2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
)

; MOSTRAR HIDRAULICO
(defun hidraulico ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout Cotas,1 Layout Texto,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")

)

; MOSTRAR ELÉTRICO
(defun eletrico ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
)

;MOSTRAR LUMINOTÉCNICO
(defun luminotecnico ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
)

; MOSTRAR SEÇÕES
(defun secoes ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico Cotas,5 Forro,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
)

; MOSTRAR FORRO
(defun forro ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
)

; MOSTRAR PISO
(defun piso ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
)

; MOSTRAR ARCONDICIONADO
(defun arcondicionado ()
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas" "")
)

; REEXIBIR TUDO
(defun reexibir ()
  (command "setvar" "clayer" "0")
  (command "_laythw")
)

; DEFUN PARA CORRIGIR SOMENTE AS COTAS SELECIONADAS
(defun fixsomecotas ( / ss textString)

  (if
  	(and (setq ss (ssget '((0 . "DIMENSION")))) (setq textString ""))
    ;THEN
  	(progn
  		(vla-startundomark
  			(cond
  				(*activeDoc*)
  				(
  					(setq *activeDoc* (vla-get-activedocument (vlax-get-acad-object)))
  				)
  			)
  		)
  		(vlax-for oDim (setq ss (vla-get-activeselectionset *activeDoc*)) (vla-put-textoverride oDim textString))
  		(vla-delete ss)
  		(vla-endundomark *activeDoc*)
  	);_progn
  	;ELSE
  	(prompt "\n** Nada selecionado! ** ")
  );_if
  (princ)
);_defun

; DEFUN PARA CORRIGIR TODAS AS COTAS
(defun fixallcotas ( / ss textString)

  (if
  	;PREDICATE
  	(and (setq ss (ssget "x" '((0 . "DIMENSION"))))(setq textString ""))
  	;THEN
  	(progn
  		(vla-startundomark
  			(cond
  				(*activeDoc*)
  				(
  					(setq *activeDoc* (vla-get-activedocument (vlax-get-acad-object)))
  				)
  			)
  		)
  		(vlax-for oDim(setq ss (vla-get-activeselectionset *activeDoc*))(vla-put-textoverride oDim textString))
  		(vla-delete ss)
  		(vla-endundomark *activeDoc*)
  	);_progn
  	;ELSE
  	(prompt "\n** Nada selecionado ** ")
  );_if
  (princ)
);_defun


; BELTB (Corrigir o bloco para Layer 0)
; ************************************************************************************************************************************************
 (defun C:beltb (/ *error* doc nametolist blkss inc blk lay blknames ent edata)

   (defun *error* (errmsg)
     (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
       (princ (strcat "\nError: " errmsg))
     ); if
     (vla-endundomark doc)
     (princ)
   ); defun - *error*

   (defun nametolist (blk / blkobj blkname); get Block name and put it into list of names
     (if (= (logand (cdr (assoc 70 (entget blk))) 4) 0) ; not an Xref
       (progn
         (setq
           blkobj (vlax-ename->vla-object blk)
           blkname
             (vlax-get-property blkobj
               (if (vlax-property-available-p blkobj 'EffectiveName) 'EffectiveName 'Name)
                 ; to work with older versions that don't have dynamic Blocks
             ); ...get-property & blkname
         ); setq
         (if
           (not (member blkname blknames)); name not already in list
           (setq blknames (append blknames (list blkname))); then -- add to end of list
         ); if
       ); progn
     ); if
   ); defun -- nametolist

   (setq doc (vla-get-activedocument (vlax-get-acad-object)))
   (vla-startundomark doc); = Undo Begin

   (if (setq blkss (ssget "_+.:S" '((0 . "INSERT")))); User selection of a Block/Minsert/Xref
     (progn ; then
       (setq
         blk (ssname blkss 0); top-level Block insertion
         lay (cdr (assoc 8 (entget blk))); Layer it's inserted on
       ); setq
       (nametolist blk); put it in blknames list
       (while (setq blk (car blknames)); as long as there's another Block name in list
         ;; done this way instead of via (repeat) or (foreach), so it can add nested Blocks' names to list
         (setq ent (tblobjname "block" blk)); Block definition as entity
         (while (setq ent (entnext ent)); then -- proceed through sub-entities in definition
           (setq edata (entget ent)); entity data list
           (if (member '(0 . "INSERT") edata) (nametolist ent)); if nested Block, add name to end of list
           (entmod (subst (cons 8 lay) (assoc 8 edata) edata)); change to top-level Block's Layer
         ); while -- sub-entities
         (setq blknames (cdr blknames)); take first one off
       ); while
       (command "_.regen")
     ); progn
     (prompt "\nNenhum bloco selecionado.")
   ); if [user selection]

   (vla-endundomark doc); = Undo End
   (princ)
 ); DEFUN

 ; ********************************************************************************************************************************************
 ; END beltb
 ; ********************************************************************************************************************************************
