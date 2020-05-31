(import (scheme base)
        (schemepunk syntax)
        (schemepunk path)
        (schemepunk test))

(cond-expand
  ((or windows dos)
    (test-group "Path Utilities (DOS)"
      (test-equal "." (path-assemble "." '()))
      (test-equal ".." (path-assemble "." '("..")))
      (test-equal "C:\\" (path-assemble "C:\\" '()))
      (test-equal "\\" (path-assemble "\\" '()))
      (test-equal "\\\\foo\\" (path-assemble "\\\\foo\\" '()))
      (test-equal "C:\\foo\\bar\\baz" (path-assemble "C:\\" '("foo" "bar" "baz")))
      (test-equal "foo\\bar\\baz" (path-assemble "." '("foo" "bar" "baz")))
      (test-equal ".\\foo\\bar\\baz" (path-assemble "." '("." "foo" "bar" "baz")))

      (test-equal "C:\\foo\\bar\\baz" (path-join "C:\\foo\\bar" "baz"))
      (test-equal "C:\\foo\\bar\\..\\baz" (path-join "C:\\foo\\bar" ".." "baz"))
      (test-equal "C:\\" (path-join "C:\\"))
      (test-equal "\\" (path-join "\\"))
      (test-equal "." (path-join "."))
      (test-equal ".\\foo\\bar" (path-join "." "foo" "bar"))

      (test-equal "C:\\foo\\bar\\baz" (path-normalize "C:\\foo\\bar\\baz"))
      (test-equal "C:\\foo\\bar\\baz" (path-normalize "C:/foo/bar/baz"))
      (test-equal "C:\\foo\\bar\\baz" (path-normalize "C:\\foo\\bar\\.\\.\\.\\.\\.\\baz"))
      (test-equal "C:\\foo\\bar\\baz" (path-normalize "C:\\foo\\bar\\\\\\baz\\"))
      (test-equal "C:\\foo\\baz" (path-normalize "C:\\foo\\bar\\..\\baz"))
      (test-equal "." (path-normalize ""))
      (test-equal "." (path-normalize "."))
      (test-equal "C:\\" (path-normalize "C:\\foo\\bar\\..\\baz\\..\\.."))
      (test-equal "C:\\" (path-normalize "C:\\..\\foo\\bar\\..\\baz\\..\\.."))
      (test-equal "." (path-normalize "foo\\bar\\..\\baz\\..\\.."))
      (test-equal ".." (path-normalize "foo\\bar\\..\\baz\\..\\..\\.."))
      (test-equal "..\\foo" (path-normalize "..\\foo\\bar\\..\\baz\\..\\."))

      (test-equal "C:\\foo\\bar" (path-directory "C:\\foo\\bar\\baz"))
      (test-equal "C:\\" (path-directory "C:\\"))
      (test-equal "\\\\foo\\" (path-directory "\\\\foo\\"))
      (test-equal ".." (path-directory "."))

      (test-equal "baz" (path-strip-directory "\\foo\\bar\\baz"))

      (test-assert (path-root? "C:\\"))
      (test-assert (path-root? "\\\\foo\\"))
      (test-assert (path-root? "\\"))
      (test-assert (not (path-root? "C:\\foo")))
      (test-assert (not (path-root? ".")))
      (test-assert (not (path-root? "")))

      (test-assert (path-absolute? "C:\\foo"))
      (test-assert (path-relative? "foo"))

      (test-assert (string? (current-directory)))
      (test-equal (path-normalize (current-directory)) (relative-path->absolute-path "."))))
  (else
    (test-group "Path Utilities (Unix)"
      (test-equal "." (path-assemble "." '()))
      (test-equal ".." (path-assemble "." '("..")))
      (test-equal "/" (path-assemble "/" '()))
      (test-equal "/foo/bar/baz" (path-assemble "/" '("foo" "bar" "baz")))
      (test-equal "foo/bar/baz" (path-assemble "." '("foo" "bar" "baz")))
      (test-equal "./foo/bar/baz" (path-assemble "." '("." "foo" "bar" "baz")))

      (test-equal "/foo/bar/baz" (path-join "/foo/bar" "baz"))
      (test-equal "/foo/bar/../baz" (path-join "/foo/bar" ".." "baz"))
      (test-equal "/" (path-join "/"))
      (test-equal "." (path-join "."))
      (test-equal "./foo/bar" (path-join "." "foo" "bar"))

      (test-equal "/foo/bar/baz" (path-normalize "/foo/bar/baz"))
      (test-equal "/foo/bar/baz" (path-normalize "/foo/bar/./././././baz"))
      (test-equal "/foo/bar/baz" (path-normalize "//foo/bar///baz/"))
      (test-equal "/foo/baz" (path-normalize "/foo/bar/../baz"))
      (test-equal "." (path-normalize ""))
      (test-equal "." (path-normalize "."))
      (test-equal "/" (path-normalize "/foo/bar/../baz/../.."))
      (test-equal "/" (path-normalize "/../foo/bar/../baz/../.."))
      (test-equal "." (path-normalize "foo/bar/../baz/../.."))
      (test-equal ".." (path-normalize "foo/bar/../baz/../../.."))
      (test-equal "../foo" (path-normalize "../foo/bar/../baz/../."))

      (test-equal "/foo/bar" (path-directory "/foo/bar/baz"))
      (test-equal "/" (path-directory "/"))
      (test-equal ".." (path-directory "."))

      (test-equal "baz" (path-strip-directory "/foo/bar/baz"))

      (test-assert (path-root? "/"))
      (test-assert (not (path-root? "/foo")))
      (test-assert (not (path-root? ".")))
      (test-assert (not (path-root? "")))

      (test-assert (path-absolute? "/foo"))
      (test-assert (path-relative? "foo"))

      (test-assert (string? (current-directory)))
      (test-equal (path-normalize (current-directory)) (relative-path->absolute-path ".")))))
