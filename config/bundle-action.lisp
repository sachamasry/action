(and
 (ql:bundle-systems
  '(net.didierverna.clon uuid alexandria
    cl-strings cl-fad nibbles local-time 
    cl-ppcre ratify) 
  :to #P"/tmp/act" :include-local-projects t :overwrite t)
 (print "System dependencies bundled in /tmp/act")
 (terpri))

(exit)