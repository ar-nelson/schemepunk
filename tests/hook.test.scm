(import (scheme base)
        (schemepunk syntax)
        (schemepunk hook)
        (schemepunk test))

(test-group "Hooks"
  (test "run empty hook"
    (hook-run (make-hook 0))
    (hook-run (make-hook 2) 'foo 'bar))

  (test "list->hook, then run"
    (let* ((side-effects '())
           (put-foo! (λ x (set! side-effects `((foo ,x) ,@side-effects))))
           (hook (list->hook 1 (list put-foo! put-foo!))))
      (hook-run hook 1)
      (assert-equal side-effects '((foo 1) (foo 1)))
      (hook-run hook 2)
      (assert-equal side-effects '((foo 2) (foo 2) (foo 1) (foo 1)))))

  (test "add and delete hook listeners"
    (let* ((foos '())
           (bars '())
           (put-foo! (λ x (set! foos (cons x foos))))
           (put-bar! (λ x (set! bars (cons x bars))))
           (hook (make-hook 1)))
      (hook-add! hook put-foo!)
      (hook-run hook 'a)
      (hook-add! hook put-bar!)
      (hook-run hook 'b)
      (hook-delete! hook put-foo!)
      (hook-run hook 'c)
      (hook-reset! hook)
      (hook-run hook 'd)
      (assert-equal foos '(b a))
      (assert-equal bars '(c b)))))
