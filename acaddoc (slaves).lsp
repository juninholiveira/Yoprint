(vl-load-com)

;loads all needed subroutines
(if (findfile "R:\\Arquitetura\\Yoprint")
  (mapcar '(lambda (x)
             (if (wcmatch (strcase x) "*.LSP,*.VLX")
               (load
                 (strcat (findfile "R:\\Arquitetura\\Yoprint") "\\" x)
               ) ;_ load
             ) ;_ if
           ) ;_ lambda
          (vl-directory-files
            (findfile "R:\\Arquitetura\\Yoprint")
          ) ;_ vl-directory-files
  ) ;_ mapcar
) ;_ if