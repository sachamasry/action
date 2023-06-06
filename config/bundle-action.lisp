(and
 (ql:bundle-systems
  '(net.didierverna.clon ubiquitous uuid alexandria
    cl-strings cl-fad nibbles local-time 
    cl-ppcre ratify dexador jonathan cl-syntax cl-annot xsubseq smart-buffer)
  :to #P"tmp/action" :include-local-projects t :overwrite t)
 (print "System dependencies bundled in tmp/action")
 (terpri))

(exit)
