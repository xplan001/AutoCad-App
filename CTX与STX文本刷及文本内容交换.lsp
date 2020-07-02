;;------------------=={ Copy or Swap Text }==-----------------;;
;;                                                            ;;
;;  Allows a user to select a source text object              ;;
;;  (Text, MText, Attribute, Multileader), and proceed to     ;;
;;  copy (or swap) its text contents to (with) a multitude of ;;
;;  destination objects.                                      ;;
;;                                                            ;;
;;  The destination objects may be selected individually, or, ;;
;;  when copying text, upon opting for the 'Multiple'         ;;
;;  selection, the user may copy to a SelectionSet of         ;;
;;  objects.                                                  ;;
;;                                                            ;;
;;  Upon choosing 'Settings' the user may alter whether       ;;
;;  MText formatting is retained when copying (or swapping)   ;;
;;  text to (with) objects which permit the use of such       ;;
;;  formatting.                                               ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.1    -    17-12-2010                            ;;
;;                                                            ;;
;;  Added ability to retain/remove MText formatting. Setting  ;;
;;  is stored as a global variable (*retain*)                 ;;
;;------------------------------------------------------------;;
;;  Version 1.2    -    20-12-2010                            ;;
;;                                                            ;;
;;  Entire program rewritten to include SwapText capability.  ;;
;;------------------------------------------------------------;;
;;  Version 1.3    -    05-01-2011                            ;;
;;                                                            ;;
;;  Fixed minor formatting bugs.                              ;;
;;------------------------------------------------------------;;

(defun c:CTx nil (CopyorSwapText nil))

(defun c:WZHH nil (CopyorSwapText   t))

;;------------------------------------------------------------;;

(defun CopyorSwapText

  ( swap

    /

    *error*
    _StartUndo
    _EndUndo
    _UnFormat
    _AllowsFormatting

    doc
    entity
    ms1
    ms2
    mstr
    o1
    o2
    ostr
    regexp
    ss
    string
    ts1
    ts2
    tstr
  )
  
  (vl-load-com)

  (setq *retain* (cond ( *retain* ) ( "Yes" )))
  
;;------------------------------------------------------------;;
;;                     Local Functions                        ;;
;;------------------------------------------------------------;;
  
  (defun *error* ( msg )
    (LM:ReleaseObject RegExp) (if doc (_EndUndo doc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

;;------------------------------------------------------------;;

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

;;------------------------------------------------------------;;

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

;;------------------------------------------------------------;;

  (defun _UnFormat ( regex entity textstring mtextstring / *error* _Replace )

    (defun _Replace ( new old string )
      (vlax-put-property regex 'pattern old) (vlax-invoke regex 'replace string new)
    )

    (
      (lambda ( string )
        (if (_AllowsFormatting entity)
          (mapcar
            (function
              (lambda ( x ) (setq string (_Replace (car x) (cdr x) string)))
            )
           '(
              (""       . "\\\\\\\\")
              (" "       . "\\\\P|\\n|\\t")
              ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
              ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
              ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
              ("$1"      . "[\\\\]({)|{")
            )
          )
          (setq string (_Replace "" "%%[OoUu]" (_Replace "" "\\\\" string)))
        )
        (set mtextstring (_Replace "\\\\" "" (_Replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" string)))
        (set  textstring (_Replace "\\"   "" string))
      )
      (LM:GetTextString entity)
    )
    nil
  )

;;------------------------------------------------------------;;

  (defun _Selectif ( pred func str keyW / e result )
    (while
      (progn (setvar 'ERRNO 0) (if keyW (initget keyW)) (setq e (func str))
        (cond
          ( (= 7 (getvar 'ERRNO))
             
            (princ "\n** Missed, Try again **")
          )
          ( (and keyW (eq 'STR (type e)))

            (not (setq result e))
          )
          ( (vl-consp e)

            (if (and pred (not (pred (car e))))
              (princ "\n** Invalid Object Selected **")
              (not (setq result (car e)))
            )
          )
        )
      )
    )
    result
  )

;;------------------------------------------------------------;;

  (defun _AllowsFormatting ( entity / object )
    
    (or (wcmatch (cdr (assoc 0 (entget entity))) "MTEXT,MULTILEADER")      
      (and
        (eq "ATTRIB" (cdr (assoc 0 (entget entity))))
        (vlax-property-available-p (setq object (vlax-ename->vla-object entity)) 'MTextAttribute)
        (eq :vlax-true (vla-get-MTextAttribute object))
      )
    )
  )

;;------------------------------------------------------------;;
;;                      Main Function                         ;;
;;------------------------------------------------------------;;
      
  (setq RegExp (vlax-get-or-create-object "VBScript.RegExp"))

  (mapcar
    (function
      (lambda ( x ) (vlax-put-property RegExp (car x) (cdr x)))
    )
    (list (cons 'global actrue) (cons 'ignorecase acfalse) (cons 'multiline actrue))
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (cond
    (
      swap
      (while
        (and
          (progn
            (while
              (and (princ (strcat "\n--> Formatting Retained: " *retain*))
                (setq o1
                  (_Selectif
                    (lambda ( entity )
                      (wcmatch (cdr (assoc 0 (entget entity))) "*TEXT,ATTRIB,MULTILEADER")
                    )
                    nentsel "\nSelect Text to Swap [Settings/Exit] <Exit>: " "Settings Exit"
                  )
                )
                (eq 'STR (type o1)) (not (eq "Exit" o1))
              )
              (initget "Yes No")
              (setq *retain*
                (cond
                  (
                    (getkword
                      (strcat "\nRetain MText Formatting [Yes/No] <" *retain* "> : ")
                    )
                  )
                  ( *retain* )
                )
              )
            )
            o1
          )
          (setq o2
            (_Selectif
              (lambda ( entity )
                (wcmatch (cdr (assoc 0 (entget entity))) "*TEXT,ATTRIB,MULTILEADER")
              )
              nentsel "\nAnd Text to Swap it With [Exit] <Exit>: " "Exit"
            )
          )
          (not (eq "Exit" o2))
        )

        (_StartUndo doc)

        (setq s1 (LM:GetTextString o1)
              s2 (LM:GetTextString o2)
        )         

        (_Unformat RegExp o1 'ts1 'ms1)
        (_Unformat RegExp o2 'ts2 'ms2)

        (apply
          (function
            (lambda ( retain MText1 MText2 )
              
              (setq o1 (vlax-ename->vla-object o1)
                    o2 (vlax-ename->vla-object o2)
              )
              (cond
                (
                  (and MText1 MText2)

                  (vla-Put-TextString o1 (if retain s2 ms2))
                  (vla-Put-TextString o2 (if retain s1 ms1))
                )
                (
                  MText1

                  (vla-Put-TextString o1 ms2)
                  (vla-Put-TextString o2 ts1)
                )
                (
                  MText2

                  (vla-Put-TextString o1 ts2)
                  (vla-Put-TextString o2 ms1)
                )
                (
                  t
                 
                  (vla-Put-TextString o1 (if retain s2 ts2))
                  (vla-Put-TextString o2 (if retain s1 ts1))
                )
              )                  
            )
          )
          (cons (eq "Yes" *retain*) (mapcar '_AllowsFormatting (list o1 o2)))
        )

        (_EndUndo doc)
      )
    )
    (t
      (if
        (progn
          (while
            (and (princ (strcat "\n--> Formatting Retained: " *retain*))
              (setq o1
                (_Selectif
                  (lambda ( entity )
                    (wcmatch (cdr (assoc 0 (entget entity))) "*TEXT,ATTRIB,MULTILEADER")
                  )
                  nentsel "\nSelect Source Object [Settings/Exit] <Exit>: " "Settings Exit"
                )
              )
              (eq 'STR (type o1)) (not (eq "Exit" o1))
            )
            (initget "Yes No")
            (setq *retain*
              (cond
                (
                  (getkword
                    (strcat "\nRetain MText Formatting [Yes/No] <" *retain* "> : ")
                  )
                )
                ( *retain* )
              )
            )
          )
          o1
        )
        (progn
          (setq ostr (LM:GetTextString o1))

          (_Unformat RegExp o1 'tstr 'mstr)

          (if (eq "Yes" *retain*)
            (set (if (_AllowsFormatting o1) 'mstr 'tstr) ostr)
          )

          (_StartUndo doc) (terpri)

          (while
            (and
              (setq o2
                (_Selectif
                  (lambda ( entity )
                    (wcmatch (cdr (assoc 0 (entget entity))) "*TEXT,ATTRIB,MULTILEADER")
                  )
                  nentsel "\rSelect Destination Object [Multiple/Exit] <Exit>: " "Multiple Exit"
                )
              )
              (not (eq "Exit" o2))
            )
            (cond
              (
                (eq "Multiple" o2)

                (if
                  (setq ss
                    (ssget "_:L"
                     '(
                        (-4 . "<OR")
                          (0 . "TEXT,MTEXT,MULTILEADER")
                          (-4 . "<AND")
                            (0 . "INSERT")
                            (66 . 1)
                          (-4 . "AND>")
                        (-4 . "OR>")
                      )
                    )
                  )
                  (
                    (lambda ( i / _type e )
                      (while (setq e (ssname ss (setq i (1+ i))))
                        (cond
                          (
                            (eq "INSERT" (setq _type (cdr (assoc 0 (entget e)))))

                            (mapcar
                              (function
                                (lambda ( attrib )
                                  (vla-put-TextString attrib
                                    (if
                                      (and
                                        (vlax-property-available-p attrib 'MTextAttribute)
                                        (eq :vlax-true (vla-get-MTextAttribute attrib))
                                      )
                                      mstr tstr
                                    )
                                  )
                                )
                              )
                              (vlax-invoke (vlax-ename->vla-object e) 'GetAttributes)
                            )
                          )
                          (t
                            (vla-put-TextString (vlax-ename->vla-object e) (if (_AllowsFormatting e) mstr tstr))
                          )
                        )
                      )
                    )
                    -1
                  )
                )
                t
              )
              ( (vla-put-TextString (vlax-ename->vla-object o2) (if (_AllowsFormatting o2) mstr tstr)) )
            )
          )

          (_EndUndo doc)
        )
      )
    )
  )

  (LM:ReleaseObject RegExp)
  (princ)
)

;;--------------------=={ Get TextString }==------------------;;
;;                                                            ;;
;;  Returns the TexString associated with an object,          ;;
;;  retaining all special symbols.                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  object - VLA-Object/ename for which to return TextString  ;;
;;------------------------------------------------------------;;
;;  Returns:  TextString associated with object, else nil     ;;
;;------------------------------------------------------------;;

(defun LM:GetTextString ( object )
  ;; ?Lee Mac 2010
  (
    (lambda ( entity / _type elist )    
      (cond
        (
          (wcmatch
            (setq _type
              (cdr
                (assoc 0
                  (setq elist
                    (entget entity)
                  )
                )
              )
            )
            "TEXT,*DIMENSION"
          )
          (cdr (assoc 1 elist))
        )
        (
          (eq "MULTILEADER" _type)

          (cdr (assoc 304 elist))
        )
        (
          (wcmatch _type "ATTRIB,MTEXT")

          (
            (lambda ( string )
              (mapcar
                (function
                  (lambda ( pair )
                    (if (member (car pair) '(1 3))
                      (setq string (strcat string (cdr pair)))
                    )
                  )
                )
                elist
              )
              string
            )
            ""
          )
        )
      )
    )
    (if (eq 'VLA-OBJECT (type object))
      (vlax-vla-object->ename object)
      object
    )
  )
)

;;------------------=={ Release Object }==--------------------;;
;;                                                            ;;
;;  Releases a VLA Object from memory via plentiful error     ;;
;;  trapping                                                  ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2010 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  obj - VLA Object to be released from memory               ;;
;;------------------------------------------------------------;;
;;  Returns:  T if Object Released, else nil                  ;;
;;------------------------------------------------------------;;

(defun LM:ReleaseObject ( obj ) (vl-load-com)
  ;; ?Lee Mac 2010
  (and obj (eq 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj))
    (not
      (vl-catch-all-error-p
        (vl-catch-all-apply
          (function vlax-release-object) (list obj)
        )
      )
    )
  )
)


(princ)
(princ "\n:: CopyText.lsp | Version 1.3 | ?Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"CTx\" to Copy or \"WZHH\" to Swap ::")
(princ)