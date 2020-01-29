(vl-load-com)

;loads all needed subroutines
(if (findfile "C:\\Arquitetura\\Yoprint")
  (mapcar '(lambda (x)
             (if (wcmatch (strcase x) "*.LSP,*.VLX")
               (load
                 (strcat (findfile "C:\\Arquitetura\\Yoprint") "\\" x)
               ) ;_ load
             ) ;_ if
           ) ;_ lambda
          (vl-directory-files
            (findfile "C:\\Arquitetura\\Yoprint")
          ) ;_ vl-directory-files
  ) ;_ mapcar
) ;_ if