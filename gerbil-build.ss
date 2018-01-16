#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make)

(def build-spec
  '("gerbil-swank"))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["meta"]
     (write '("spec" "deps" "compile"))
     (newline))
    (["spec"]
     (pretty-print build-spec))
    (["deps"]
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "gerbil-build-deps" (cut write build-deps <>))))
    (["compile"]
     (let (depgraph (call-with-input-file "gerbil-build-deps" read))
       (make srcdir: srcdir
             depgraph: depgraph
             optimize: #t
             static: #f
             debug: 'src
             prefix: "ecraven"
             build-spec)))
    ([]
     (displayln "... make deps")
     (main "deps")
     (displayln "... compile")
     (main "compile"))))
