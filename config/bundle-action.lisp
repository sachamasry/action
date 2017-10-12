(and
 (ql:bundle-systems
  '(net.didierverna.clon uuid alexandria
    cl-strings cl-fad nibbles local-time 
    cl-ppcre ratify) 
  ;:to #P"tmp/action" :include-local-projects t :overwrite t)
  :to #P"tmp/action" :overwrite t)
 (print "System dependencies bundled in tmp/action")
 (terpri))

(exit)
