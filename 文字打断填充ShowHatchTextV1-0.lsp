;;-----------------------=={ Show Hatch Text }==------------------------;;
;;                                                                      ;;
;;  This program enables the user to clear the area of a hatch pattern  ;;
;;  surrounding selected Text or MText objects, or Text, MText or       ;;
;;  Attributes contained within selected Block References.              ;;
;;                                                                      ;;
;;  Upon issuing the command syntax 'sht' (Show Hatch Text) at the      ;;
;;  command-line, the user is first prompted to make a selection of     ;;
;;  Text, MText and/or Blocks for which to clear the surrounding hatch  ;;
;;  pattern, and then to select the obscuring hatch to be modified.     ;;
;;                                                                      ;;
;;  Following valid selections, the program will proceed to generate    ;;
;;  new hatch boundaries surrounding every selected Text and MText      ;;
;;  object, and furthermore for every Text, MText or Attribute object   ;;
;;  found within each selected block reference, including within any    ;;
;;  nested block references (nested to any depth) found within the      ;;
;;  selected block references.                                          ;;
;;                                                                      ;;
;;  In order to generate the appropriate hatch boundary for nested      ;;
;;  Text, MText or Attributes, the program will recreate the nested     ;;
;;  object as a temporary primary object, before adding the new hatch   ;;
;;  boundary and deleting the temporary object. As a consequence of     ;;
;;  this method, the hatch must become disassociative when nested       ;;
;;  objects are processed by the program.                               ;;
;;                                                                      ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright ?2013  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    14-11-2013                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:tcdd ( / cmd en1 en2 ent enx hat idx sel )

    (defun *error* ( msg )
        (foreach ent en2
            (if (entget ent) (entdel ent))
        )
        (if (= 'int (type cmd)) (setvar 'cmdecho cmd))
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (LM:startundo (LM:acdoc))
    (cond
        (   (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" "0")))))
            (princ "\nLayer \"0\" is locked.")
        )
        (   (and
                (setq sel (LM:ssget "\nSelect text and blocks: "   '(((0 . "INSERT,TEXT,MTEXT")))))
                (setq hat (LM:ssget "\nSelect hatch: " '("_+.:E:S:L" ((0 . "HATCH")))))
            )
            (repeat (setq idx (sslength sel))
                (setq ent (ssname sel (setq idx (1- idx)))
                      enx (entget ent)
                )
                (if (wcmatch (cdr (assoc 0 enx)) "*TEXT")
                    (setq en1 (cons ent en1))
                    (progn
                        (setq en2
                            (append en2
                                (fixhatch:processblock
                                    (apply 'fixhatch:tmatrix (refgeom ent))
                                    (cdr (assoc 2 enx))
                                )
                            )
                        )
                        (if (= 1 (cdr (assoc 66 enx)))
                            (setq en2 (append en2 (fixhatch:processattributes ent)))
                        )
                    )
                )
            )
            (if (or en1 en2)
                (progn
                    (setq cmd (getvar 'cmdecho))
                    (setvar 'cmdecho 0)
                    (if en2 (command "_.-hatchedit" (ssname hat 0) "_DI"))
                    (command "_.-hatchedit" (ssname hat 0) "_AD" "_S")
                    (apply 'command (append en1 en2))
                    (command "" "")
                    (setvar 'cmdecho cmd)
                    (foreach ent en2 (entdel ent))
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

(defun fixhatch:processblock ( mat blk / ent enx lst tmp )
    (if (setq ent (tblobjname "block" blk))
        (while (setq ent (entnext ent))
            (setq enx (entget ent))
            (cond
                (   (= 1 (cdr (assoc 60 enx))))
                (   (wcmatch (cdr (assoc 0 enx)) "TEXT,MTEXT")
                    (if (setq tmp (fixhatch:entmakex enx))
                        (setq lst (cons tmp lst))
                    )
                )
                (   (= "INSERT" (cdr (assoc 0 enx)))
                    (if (= 1 (cdr (assoc 66 enx)))
                        (setq lst (append lst (fixhatch:processattributes ent)))
                    )
                    (setq lst
                        (append lst
                            (fixhatch:processblock
                                (apply 'fixhatch:tmatrix (refgeom ent))
                                (cdr (assoc 2 enx))
                            )
                        )
                    )
                )
            )
        )
    )
    (foreach ent lst
        (vla-transformby (vlax-ename->vla-object ent) mat)
    )
    lst
)

(defun fixhatch:processattributes ( ent / att atx lst tmp )
    (setq att (entnext ent)
          atx (entget  att)
    )
    (while (= "ATTRIB" (cdr (assoc 0 atx)))
        (if
            (and (zerop (logand 1 (cdr (assoc 70 atx))))
                (setq tmp
                    (fixhatch:entmakex
                        (if (member '(101 . "Embedded Object") atx)
                            (append '((0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText"))
                                (fixhatch:remove1stpairs  '(001 007 010 011 040 041 050 071 072 073 210)
                                    (fixhatch:removepairs '(000 002 042 043 051 070 074 100 101 102 280 330 360) atx)
                                )
                            )
                            (append '((0 . "TEXT"))
                                (fixhatch:removepairs '(000 002 070 074 100 280)
                                    (subst (cons 73 (cdr (assoc 74 atx))) (assoc 74 atx) atx)
                                )
                            )
                        )
                    )
                )
             )
             (setq lst (cons tmp lst))
        )
        (setq att (entnext att)
              atx (entget  att)
        )
    )
    lst
)

(defun fixhatch:tmatrix ( mat vec )
    (vlax-tmatrix
        (append
            (mapcar '(lambda ( a b ) (append a (list b))) mat vec)
           '((0.0 0.0 0.0 1.0))
        )
    )
)

(defun fixhatch:entmakex ( enx )
    (entmakex
        (append
            (vl-remove-if
               '(lambda ( x )
                    (or (member (car x) '(005 006 008 039 048 062 102 370))
                        (= 'ename (type (cdr x)))
                    )
                )
                enx
            )
           '(
                (006 . "CONTINUOUS")
                (008 . "0")
                (039 . 0.0)
                (048 . 1.0)
                (062 . 7)
                (370 . 0)
            )
        )
    )
)

(defun fixhatch:removepairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (member (car x) itm)) lst)
)
 
(defun fixhatch:remove1stpairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (if (member (car x) itm) (progn (setq itm (vl-remove (car x) itm)) t))) lst)
)

;; RefGeom (gile)
;; Returns a list whose first item is a 3x3 transformation matrix and
;; second item the object insertion point in its parent (xref, block or space)
 
(defun refgeom ( ent / ang enx mat ocs )
    (setq enx (entget ent)
          ang (cdr (assoc 050 enx))
          ocs (cdr (assoc 210 enx))
    )
    (list
        (setq mat
            (mxm
                (mapcar '(lambda ( v ) (trans v 0 ocs t))
                   '(
                        (1.0 0.0 0.0)
                        (0.0 1.0 0.0)
                        (0.0 0.0 1.0)
                    )
                )
                (mxm
                    (list
                        (list (cos ang) (- (sin ang)) 0.0)
                        (list (sin ang) (cos ang)     0.0)
                       '(0.0 0.0 1.0)
                    )
                    (list
                        (list (cdr (assoc 41 enx)) 0.0 0.0)
                        (list 0.0 (cdr (assoc 42 enx)) 0.0)
                        (list 0.0 0.0 (cdr (assoc 43 enx)))
                    )
                )
            )
        )
        (mapcar '- (trans (cdr (assoc 10 enx)) ocs 0)
            (mxv mat (cdr (assoc 10 (tblsearch "block" (cdr (assoc 2 enx))))))
        )
    )
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - selection prompt
;; arg - list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)

;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: ShowHatchText.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"sht\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;