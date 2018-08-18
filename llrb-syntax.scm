(module
 llrb-syntax
 *
 (import scheme)
 (cond-expand
  (chicken-4 (import chicken))
  (else (import (chicken base))))
 (include "llrbsyn.scm"))
