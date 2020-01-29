(vl-load-com)

; IMPRESSORAS
(setq pdfPlotter "DWG TO PDF.PC3")                                              ; Impressora para PDF
(setq brotherPlotter "Brother DCP-7065DN Printer")                              ; Impressora A4 Brother
(setq A3PlotterRede "\\\\Desk-interiores\\EPSON L1300 Series")                  ; Impressora colorida A3 Epson  BACKUP EPSON: \\\\Desk-interiores\\EPSON L1300 Series
(setq A3PlotterServidor "EPSON L1300 Series")

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


(defun C:yoprint ( / *error* )

(defun *error* ( msg )
  (princ)
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
  )
  (princ)
)

    (if (> (substr (rtos (getvar 'cdate) 2 0) 3) "210101")                      ;(YY/MM/DD)

        (progn                                                                  ;Se o programa tiver expirado
          (princ "Licença Expirada")                                            ;Mensagem que exibe
          (princ "\n")
          (exit)                                                                ;Cancela toda a rotina

        ); progn

        (progn                                                                  ;Se o programa NÃO tiver expirado
          (setq dcl_id (load_dialog "interface.dcl"))
          (if (not (new_dialog "interface" dcl_id))
            (exit )
          );if
        ); progn
    );if

;CANCEL
(action_tile "cancel" "(done_dialog)(exit)(princ)")

(action_tile "printalltopdf" "(done_dialog 0)")
(action_tile "printalla3" "(done_dialog 1)")
(action_tile "printsinglesheet" "(done_dialog 2)")

(set_tile "ctbescolhido" "ctb - escritório.ctb")
(action_tile "ctbescolhido" "(setq ctb $value)")
(setq ctb (get_tile "ctbescolhido"))

(set_tile "togglelayout" "1")
(action_tile "togglelayout" "(setq plotLayout $value)")
(setq plotLayout (get_tile "togglelayout"))

(set_tile "togglehidraulico" "1")
(action_tile "togglehidraulico" "(setq plotHidraulico $value)")
(setq plotHidraulico (get_tile "togglehidraulico"))

(set_tile "toggleeletrico" "1")
(action_tile "toggleeletrico" "(setq plotEletrico $value)")
(setq plotEletrico (get_tile "toggleeletrico"))

(set_tile "toggleluminotecnico" "1")
(action_tile "toggleluminotecnico" "(setq plotLuminotecnico $value)")
(setq plotLuminotecnico (get_tile "toggleluminotecnico"))

(set_tile "togglesecoes" "1")
(action_tile "togglesecoes" "(setq plotSecoes $value)")
(setq plotSecoes (get_tile "togglesecoes"))

(set_tile "toggleforro" "1")
(action_tile "toggleforro" "(setq plotForro $value)")
(setq plotForro (get_tile "toggleforro"))

(set_tile "togglepiso" "1")
(action_tile "togglepiso" "(setq plotPiso $value)")
(setq plotPiso (get_tile "togglepiso"))

(set_tile "togglearcondicionado" "1")
(action_tile "togglearcondicionado" "(setq plotArcondicionado $value)")
(setq plotArcondicionado (get_tile "togglearcondicionado"))

(action_tile "layout" "(done_dialog 3)")
(action_tile "hidraulico" "(done_dialog 4)")
(action_tile "eletrico" "(done_dialog 5)")
(action_tile "luminotecnico" "(done_dialog 6)")
(action_tile "secoes" "(done_dialog 15)")
(action_tile "forro" "(done_dialog 7)")
(action_tile "piso" "(done_dialog 8)")
(action_tile "arcondicionado" "(done_dialog 9)")
(action_tile "reexibir" "(done_dialog 10)")

(action_tile "fixallcotas" "(done_dialog 11)")
(action_tile "fixsomecotas" "(done_dialog 12)")

(action_tile "changelayercolor" "(done_dialog 13)")
(action_tile "changelayercolorback" "(done_dialog 14)")

(action_tile "beltb" "(done_dialog 17)")
(action_tile "listagemindividual" "(done_dialog 16)")

(setq main_flag (start_dialog))

(cond
   ((= main_flag 0) (printalltopdf))
   ((= main_flag 1)
     (progn
       (printalla3)
       (setq plotLayout (get_tile "togglelayout"))
       (setq plotHidraulico (get_tile "togglehidraulico"))
       (setq plotEletrico (get_tile "toggleeletrico"))
       (setq plotLuminotecnico (get_tile "toggleluminotecnico"))
       (setq plotSecoes (get_tile "togglesecoes"))
       (setq plotForro (get_tile "toggleforro"))
       (setq plotPiso (get_tile "togglepiso"))
       (setq plotArcondicionado (get_tile "togglearcondicionado"))
     )
   )
   ((= main_flag 2)(printsinglesheet))

   ((= main_flag 3) (layout))
   ((= main_flag 4) (hidraulico))
   ((= main_flag 5) (eletrico))
   ((= main_flag 6) (luminotecnico))
   ((= main_flag 15) (secoes))
   ((= main_flag 7) (forro))
   ((= main_flag 8) (piso))
   ((= main_flag 9) (arcondicionado))
   ((= main_flag 10) (reexibir))

   ((= main_flag 11) (fixallcotas))
   ((= main_flag 12) (fixsomecotas))

   ((= main_flag 13) (changecolorstogrey))
   ((= main_flag 14) (changecolorsback))

   ((= main_flag 17) (beltb))
   ((= main_flag 16) (listagemindividual))
)


;Start and unload the DCL
(start_dialog)
(unload_dialog dcl_id)

(princ)

);defun

; ****************************************************************************************************************************

;PRINTALLTOPDF ***************************************************************************************************************
(defun printalltopdf (/ dwg file hnd i len llpt lst mn mx ss tab urpt subfolder cpath newpath currententity scale)

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
                         ctb
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
(defun printalla3 (/ dwg file hnd i len llpt mn mx  tab urpt subfolder cpath newpath currententity scale
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

	    ;Crio uma variável para armazenar um numero que será usado dentro do FOREACH, e zerado quando sair
	    ;(setq plantanumber 1)

			(setq count 0)
      ;Verificação da necessidade de imprimir essa planta


			(repeat 8

        (setq PlotThis "0")

        (if (= count 0) (if (= plotLayout "1")(setq PlotThis "1")))
        (if (= count 1) (if (= plotHidraulico "1")(setq PlotThis "1")))
        (if (= count 2) (if (= plotEletrico "1")(setq PlotThis "1")))
        (if (= count 3) (if (= plotLuminotecnico "1")(setq PlotThis "1")))
        (if (= count 4) (if (= plotSecoes "1")(setq PlotThis "1")))
        (if (= count 5) (if (= plotForro "1")(setq PlotThis "1")))
        (if (= count 6) (if (= plotPiso "1")(setq PlotThis "1")))
        (if (= count 7) (if (= plotArcondicionado "1")(setq PlotThis "1")))

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

        (if (= PlotThis "1")
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
                         ctb
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

            ) ;if

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
  (setq oldlayer (getvar "CLAYER")) ;Pega a layer atual
	(command "setvar" "clayer" "0")   ;Seta a Layer Atual como Layer 0
	(command "_laythw")               ;Exibe todas as layers para depois apagar as específicas
	(command "_.layer" "_freeze" "2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)        ;Retorna a layer
)

; MOSTRAR HIDRAULICO
(defun hidraulico ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout Cotas,1 Layout Texto,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)
)

; MOSTRAR ELÉTRICO
(defun eletrico ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)
)

;MOSTRAR LUMINOTÉCNICO
(defun luminotecnico ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)
)

; MOSTRAR SEÇÕES
(defun secoes ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico Cotas,5 Forro,5 Forro Cotas,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)
)

; MOSTRAR FORRO
(defun forro ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,6 Piso,6 Piso Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)
)

; MOSTRAR PISO
(defun piso ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,7 Ar Condicionado,7 Ar Condicionado Cotas" "")
  (setvar "CLAYER" oldlayer)
)

; MOSTRAR ARCONDICIONADO
(defun arcondicionado ()
  (setq oldlayer (getvar "CLAYER"))
	(command "setvar" "clayer" "0")
	(command "_laythw")
	(command "_.layer" "_freeze" "1 Layout 01,1 Layout 02,1 Layout 03,1 Layout Cotas,1 Layout Texto,2 Hidráulica,2 Hidráulica Cotas,3 Elétrico,3 Elétrico Cotas,4 Luminotécnico,4 Luminotécnico Cotas,4 Luminotécnico Seções,5 Forro,5 Forro Contorno,5 Forro Cotas,6 Piso,6 Piso Cotas" "")
  (setvar "CLAYER" oldlayer)
)

; REEXIBIR TUDO
(defun reexibir ()
  (setq oldlayer (getvar "CLAYER"))
  (command "setvar" "clayer" "0")
  (command "_laythw")
  (setvar "CLAYER" oldlayer)
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


; ***********************************************************************************************************************************************
; LISTAGEM INDIVIDUAL
; ***********************************************************************************************************************************************
(defun listagemindividual (/ blk_id blk_len blk_name blks ent h header_lsp height i j Total
                 len0 lst_blk msp pt row ss str tblobj width width1 width2 x y)

 (vl-load-com)

 (defun TxtWidth (val h msp / txt minp maxp)
   (setq  txt (vla-AddText msp val (vlax-3d-point '(0 0 0)) h))
   (vla-getBoundingBox txt 'minp 'maxp)
   (vla-Erase txt)
   (-(car(vlax-safearray->list maxp))(car(vlax-safearray->list minp))))

 (defun GetOrCreateTableStyle (tbl_name / name namelst objtblsty objtblstydic tablst txtsty)
   (setq objTblStyDic (vla-item (vla-get-dictionaries *adoc) "ACAD_TABLESTYLE"))
   (foreach itm (vlax-for itm objTblStyDic
                 (setq tabLst (append tabLst (list itm))))
     (if (not
          (vl-catch-all-error-p
             (setq name (vl-catch-all-apply 'vla-get-Name (list itm)))))
       (setq nameLst (append nameLst (list name)))))
   (if (not (vl-position tbl_name nameLst))
     (vla-addobject objTblStyDic tbl_name "AcDbTableStyle"))
   (setq objTblSty (vla-item objTblStyDic tbl_name)
    TxtSty (variant-value (vla-getvariable *adoc "TextStyle")))
   (mapcar '(lambda (x)(vla-settextstyle objTblSty x TxtSty))
        (list acTitleRow acHeaderRow acDataRow))
   (vla-setvariable *adoc "CTableStyle" tbl_name))


 (defun GetObjectID (obj)
   (if (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
     (vlax-invoke-method *util 'GetObjectIdString obj :vlax-false)
     (vla-get-Objectid obj)))


;PROGRAMA COMEÇA AQUI
 (if (setq ss (ssget (list (cons 0 "INSERT"))))                                 ;(SSGET pra fazer a seleção de blocos)
   (progn
     (vl-load-com)
     (setq i -1 len0 8)
     (while (setq ent (ssname ss (setq i (1+ i))))                              ;(SSNAME retorna o nome do bloco, da lista SS, na posição i)
        (setq blk_name (cdr (assoc 2 (entget ent))))                            ;(Isso aqui deve pegar a string do nome certinho)
        (if (> (setq blk_len (strlen blk_name)) len0)
         (setq str blk_name len0 blk_len))
         (if (not (assoc blk_name lst_blk))
         (setq lst_blk (cons (cons blk_name 1) lst_blk))
         (setq lst_blk (subst (cons blk_name (1+ (cdr (assoc blk_name lst_blk))))
                        (assoc blk_name lst_blk) lst_blk)))
     );while
     (setq lst_blk (vl-sort lst_blk '(lambda (x y) (< (car x) (car y)))))

     (setq Total 0)                                                             ;Cria a variável que armazenará a quantidade total de blocos
     (foreach I lst_blk (setq Total (+ Total (cdr I))))                         ;Acha a quantidade de blocos

     (or *h* (setq *h* (* (getvar "dimtxt")(getvar "dimscale"))))
     (initget 6)

     (setq AmbienteName (GetString T "Digite o nome do ambiente:"))             ;Aqui eu peço o nome do ambiente

    ;(setq h (getreal (strcat "\nText Height <" (rtos *h*) "> :")))             ;BACKUP do Input para altura do texto
     (setq h 10)                                                                ;Altura fixa do texto armazenada na var h

     (if h (setq *h* h) (setq h *h*))
     (or *adoc (setq *adoc (vla-get-ActiveDocument (vlax-get-acad-object))))
     (setq msp (vla-get-modelspace *adoc)
      *util (vla-get-Utility *adoc)
      blks (vla-get-blocks *adoc))
     (setq width1 (* 4 (TxtWidth "    " h msp))
      width (* 2 (TxtWidth "Text Height" h msp))
      height (* 2 h))
     (if str
      (setq width2 (* 1.5 (TxtWidth (strcase str) h msp)))
      (setq width2 width))
     (if (> h 3)
      (setq width   (* (fix (/ width  8))8)
             width1 (* (fix (/ width1 8))8)
             width2 (* (fix (/ width2 8))8)
             height (* (fix (/ height 5))5)))
     (GetOrCreateTableStyle "CadEng")
     (setq pt
       (getpoint "\nColocar a tabela:")                                             ;Pede o ponto onde a tabela vai ser inserida
       TblObj (vla-addtable
                msp                     ;ModelSpace
                (vlax-3d-point pt)      ;Passa o PT 2d pra 3d
                (+ (length lst_blk) 2)  ;Pega a quantidade de blocos na lista e adiciona 3
                2                       ;Número de colunas
                height                  ;RowHeight
                width                   ;ColWidth
              ) ;addtable
     );setq pt
     (vla-put-regeneratetablesuppressed TblObj :vlax-true)                      ;Desativa regeneração da tabela
     (vla-SetColumnWidth TblObj 0 width1)                                       ;Altera o width da coluna 0

     (setq width2 400)

     (vla-SetColumnWidth TblObj 1 width2)                                       ;Altera o width da coluna 1
     (vla-put-vertcellmargin TblObj (* 0.75 h))
     (vla-put-horzcellmargin TblObj (* 0.75 h))
     (mapcar '(lambda (x)(vla-setTextHeight TblObj x h))
       (list acTitleRow acHeaderRow acDataRow))
     (mapcar '(lambda (x)(vla-setAlignment TblObj x 8))
       (list acTitleRow acHeaderRow acDataRow))
     (vla-MergeCells TblObj 0 0 0 1)                                            ;Faz o Merge da célula do título (OBS.: o ultimo numero tem que bater com o numero de colunas -1)
     (vla-setText TblObj 0 0 AmbienteName)                                      ;Coloca o nome do ambiente na célula 0,0
     (setq j -1 header_lsp (list "Qtd." "Item"))                                ;Defino uma lista de strings para o cabeçálio; A var J é definida como -1 para o "Repeat" de baixo setar o J com +1 já no primeiro loop (resultando em um loop começando no 0 {coluna 0 = primeira coluna})
     (repeat (length header_lsp)                                                ;Repeat que pega a quantidade de strings como quantidade de de voltas
      (vla-setText TblObj 1 (setq j (1+ j)) (nth j header_lsp)))                ;Seta o texto da Row 1, Coluna J {J = -1, já no prieiro loop vira 0}, com a string da var "header_lsp" na posição J

     (setq row 2 i 1)                                                           ;Aqui define a numeração de ordem, começando na linha "2" pelo número "1"
     (foreach pt lst_blk
      (setq blk_name (car pt) j -1)
      (mapcar '(lambda (x) (vla-setText TblObj row (setq j (1+ j)) x))          ;uso um Mapcar para aplicar as strings nas células
          (list (cdr pt) blk_name))                                             ;Aqui eu defino a ordem, primeiro o número de quantidade (cdr pt) e depois o nome do bloco
         ;(list i blk_name  (cdr pt)))                                          ;BACKUP => Número ordinal, Nome do Bloco, Quantidade

      (vla-SetCellAlignment TblObj row 0 2)                                     ;Alinhamento do texto
      (vla-SetCellAlignment TblObj row 1 1)
      (setq row (1+ row) i (1+ i))

      );foreach

      (setq row (- 1 row))                                                      ;Retiro 1 da var "row" para não criar uma linha sobrando na tabela

    (vla-put-regeneratetablesuppressed TblObj :vlax-false)                      ;Reativa a regeneração da tabela
    (vlax-release-object TblObj)
    );progn
    );if
 (princ))

 ; ************************************************************************************************************************************************
 ; END LISTAGEM INDIVIDUAL
 ; ************************************************************************************************************************************************
 ; BELTB (Corrigir o bloco para Layer 0)
 ; ************************************************************************************************************************************************
 (defun beltb (/ *error* doc nametolist blkss inc blk lay blknames ent edata)

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
