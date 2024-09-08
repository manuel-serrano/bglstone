;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Iselect/asm.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; asm-instr
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-instr::asm-instr shape1257::bstring)
    (inline asm-instr?::bool ::obj)
    (asm-instr-nil::asm-instr)
    (inline asm-instr-shape::bstring ::asm-instr))))

;; asm-oper
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-oper::asm-oper shape1249::bstring live-in1250::obj live-out1251::obj trash1252::obj succ1253::obj use1254::obj def1255::obj)
    (inline asm-oper?::bool ::obj)
    (asm-oper-nil::asm-oper)
    (inline asm-oper-def::obj ::asm-oper)
    (inline asm-oper-use::obj ::asm-oper)
    (inline asm-oper-succ::obj ::asm-oper)
    (inline asm-oper-succ-set! ::asm-oper ::obj)
    (inline asm-oper-trash::obj ::asm-oper)
    (inline asm-oper-trash-set! ::asm-oper ::obj)
    (inline asm-oper-live-out::obj ::asm-oper)
    (inline asm-oper-live-out-set! ::asm-oper ::obj)
    (inline asm-oper-live-in::obj ::asm-oper)
    (inline asm-oper-live-in-set! ::asm-oper ::obj)
    (inline asm-oper-shape::bstring ::asm-oper))))

;; asm-move
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-move::asm-move shape1241::bstring live-in1242::obj live-out1243::obj trash1244::obj succ1245::obj use1246::obj def1247::obj)
    (inline asm-move?::bool ::obj)
    (asm-move-nil::asm-move)
    (inline asm-move-def::obj ::asm-move)
    (inline asm-move-use::obj ::asm-move)
    (inline asm-move-succ::obj ::asm-move)
    (inline asm-move-succ-set! ::asm-move ::obj)
    (inline asm-move-trash::obj ::asm-move)
    (inline asm-move-trash-set! ::asm-move ::obj)
    (inline asm-move-live-out::obj ::asm-move)
    (inline asm-move-live-out-set! ::asm-move ::obj)
    (inline asm-move-live-in::obj ::asm-move)
    (inline asm-move-live-in-set! ::asm-move ::obj)
    (inline asm-move-shape::bstring ::asm-move))))

;; asm-jump
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-jump::asm-jump shape1232::bstring live-in1233::obj live-out1234::obj trash1235::obj succ1236::obj use1237::obj def1238::obj asm-label1239::label)
    (inline asm-jump?::bool ::obj)
    (asm-jump-nil::asm-jump)
    (inline asm-jump-asm-label::label ::asm-jump)
    (inline asm-jump-def::obj ::asm-jump)
    (inline asm-jump-use::obj ::asm-jump)
    (inline asm-jump-succ::obj ::asm-jump)
    (inline asm-jump-succ-set! ::asm-jump ::obj)
    (inline asm-jump-trash::obj ::asm-jump)
    (inline asm-jump-trash-set! ::asm-jump ::obj)
    (inline asm-jump-live-out::obj ::asm-jump)
    (inline asm-jump-live-out-set! ::asm-jump ::obj)
    (inline asm-jump-live-in::obj ::asm-jump)
    (inline asm-jump-live-in-set! ::asm-jump ::obj)
    (inline asm-jump-shape::bstring ::asm-jump))))

;; asm-cjump
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-cjump::asm-cjump shape1223::bstring live-in1224::obj live-out1225::obj trash1226::obj succ1227::obj use1228::obj def1229::obj asm-label1230::label)
    (inline asm-cjump?::bool ::obj)
    (asm-cjump-nil::asm-cjump)
    (inline asm-cjump-asm-label::label ::asm-cjump)
    (inline asm-cjump-def::obj ::asm-cjump)
    (inline asm-cjump-use::obj ::asm-cjump)
    (inline asm-cjump-succ::obj ::asm-cjump)
    (inline asm-cjump-succ-set! ::asm-cjump ::obj)
    (inline asm-cjump-trash::obj ::asm-cjump)
    (inline asm-cjump-trash-set! ::asm-cjump ::obj)
    (inline asm-cjump-live-out::obj ::asm-cjump)
    (inline asm-cjump-live-out-set! ::asm-cjump ::obj)
    (inline asm-cjump-live-in::obj ::asm-cjump)
    (inline asm-cjump-live-in-set! ::asm-cjump ::obj)
    (inline asm-cjump-shape::bstring ::asm-cjump))))

;; asm-label
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-label::asm-label framedecl1217::obj ident1218::ident live-in1219::obj live-out1220::obj succ1221::obj)
    (inline asm-label?::bool ::obj)
    (asm-label-nil::asm-label)
    (inline asm-label-succ::obj ::asm-label)
    (inline asm-label-succ-set! ::asm-label ::obj)
    (inline asm-label-live-out::obj ::asm-label)
    (inline asm-label-live-out-set! ::asm-label ::obj)
    (inline asm-label-live-in::obj ::asm-label)
    (inline asm-label-live-in-set! ::asm-label ::obj)
    (inline asm-label-ident::ident ::asm-label)
    (inline asm-label-framedecl::obj ::asm-label)
    (inline asm-label-framedecl-set! ::asm-label ::obj))))

;; asm-pseudo
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-asm-pseudo::asm-pseudo shape1207::bstring live-in1208::obj live-out1209::obj trash1210::obj succ1211::obj use1212::obj def1213::obj framedecl1214::obj)
    (inline asm-pseudo?::bool ::obj)
    (asm-pseudo-nil::asm-pseudo)
    (inline asm-pseudo-framedecl::obj ::asm-pseudo)
    (inline asm-pseudo-def::obj ::asm-pseudo)
    (inline asm-pseudo-use::obj ::asm-pseudo)
    (inline asm-pseudo-succ::obj ::asm-pseudo)
    (inline asm-pseudo-succ-set! ::asm-pseudo ::obj)
    (inline asm-pseudo-trash::obj ::asm-pseudo)
    (inline asm-pseudo-trash-set! ::asm-pseudo ::obj)
    (inline asm-pseudo-live-out::obj ::asm-pseudo)
    (inline asm-pseudo-live-out-set! ::asm-pseudo ::obj)
    (inline asm-pseudo-live-in::obj ::asm-pseudo)
    (inline asm-pseudo-live-in-set! ::asm-pseudo ::obj)
    (inline asm-pseudo-shape::bstring ::asm-pseudo))))

;; proc-entry
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-proc-entry::proc-entry shape1198::bstring live-in1199::obj live-out1200::obj trash1201::obj succ1202::obj use1203::obj def1204::obj framedecl1205::obj)
    (inline proc-entry?::bool ::obj)
    (proc-entry-nil::proc-entry)
    (inline proc-entry-framedecl::obj ::proc-entry)
    (inline proc-entry-def::obj ::proc-entry)
    (inline proc-entry-use::obj ::proc-entry)
    (inline proc-entry-succ::obj ::proc-entry)
    (inline proc-entry-succ-set! ::proc-entry ::obj)
    (inline proc-entry-trash::obj ::proc-entry)
    (inline proc-entry-trash-set! ::proc-entry ::obj)
    (inline proc-entry-live-out::obj ::proc-entry)
    (inline proc-entry-live-out-set! ::proc-entry ::obj)
    (inline proc-entry-live-in::obj ::proc-entry)
    (inline proc-entry-live-in-set! ::proc-entry ::obj)
    (inline proc-entry-shape::bstring ::proc-entry))))

;; proc-exit
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-proc-exit::proc-exit shape1189::bstring live-in1190::obj live-out1191::obj trash1192::obj succ1193::obj use1194::obj def1195::obj framedecl1196::obj)
    (inline proc-exit?::bool ::obj)
    (proc-exit-nil::proc-exit)
    (inline proc-exit-framedecl::obj ::proc-exit)
    (inline proc-exit-def::obj ::proc-exit)
    (inline proc-exit-use::obj ::proc-exit)
    (inline proc-exit-succ::obj ::proc-exit)
    (inline proc-exit-succ-set! ::proc-exit ::obj)
    (inline proc-exit-trash::obj ::proc-exit)
    (inline proc-exit-trash-set! ::proc-exit ::obj)
    (inline proc-exit-live-out::obj ::proc-exit)
    (inline proc-exit-live-out-set! ::proc-exit ::obj)
    (inline proc-exit-live-in::obj ::proc-exit)
    (inline proc-exit-live-in-set! ::proc-exit ::obj)
    (inline proc-exit-shape::bstring ::proc-exit)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; asm-instr
(define-inline (make-asm-instr::asm-instr shape1257::bstring) (instantiate::asm-instr (shape shape1257)))
(define-inline (asm-instr?::bool obj::obj) ((@ isa? __object) obj (@ asm-instr iselect_asm)))
(define (asm-instr-nil::asm-instr) (class-nil (@ asm-instr iselect_asm)))
(define-inline (asm-instr-shape::bstring o::asm-instr) (with-access::asm-instr o (shape) shape))
(define-inline (asm-instr-shape-set! o::asm-instr v::bstring) (with-access::asm-instr o (shape) (set! shape v)))

;; asm-oper
(define-inline (make-asm-oper::asm-oper shape1249::bstring live-in1250::obj live-out1251::obj trash1252::obj succ1253::obj use1254::obj def1255::obj) (instantiate::asm-oper (shape shape1249) (live-in live-in1250) (live-out live-out1251) (trash trash1252) (succ succ1253) (use use1254) (def def1255)))
(define-inline (asm-oper?::bool obj::obj) ((@ isa? __object) obj (@ asm-oper iselect_asm)))
(define (asm-oper-nil::asm-oper) (class-nil (@ asm-oper iselect_asm)))
(define-inline (asm-oper-def::obj o::asm-oper) (with-access::asm-oper o (def) def))
(define-inline (asm-oper-def-set! o::asm-oper v::obj) (with-access::asm-oper o (def) (set! def v)))
(define-inline (asm-oper-use::obj o::asm-oper) (with-access::asm-oper o (use) use))
(define-inline (asm-oper-use-set! o::asm-oper v::obj) (with-access::asm-oper o (use) (set! use v)))
(define-inline (asm-oper-succ::obj o::asm-oper) (with-access::asm-oper o (succ) succ))
(define-inline (asm-oper-succ-set! o::asm-oper v::obj) (with-access::asm-oper o (succ) (set! succ v)))
(define-inline (asm-oper-trash::obj o::asm-oper) (with-access::asm-oper o (trash) trash))
(define-inline (asm-oper-trash-set! o::asm-oper v::obj) (with-access::asm-oper o (trash) (set! trash v)))
(define-inline (asm-oper-live-out::obj o::asm-oper) (with-access::asm-oper o (live-out) live-out))
(define-inline (asm-oper-live-out-set! o::asm-oper v::obj) (with-access::asm-oper o (live-out) (set! live-out v)))
(define-inline (asm-oper-live-in::obj o::asm-oper) (with-access::asm-oper o (live-in) live-in))
(define-inline (asm-oper-live-in-set! o::asm-oper v::obj) (with-access::asm-oper o (live-in) (set! live-in v)))
(define-inline (asm-oper-shape::bstring o::asm-oper) (with-access::asm-oper o (shape) shape))
(define-inline (asm-oper-shape-set! o::asm-oper v::bstring) (with-access::asm-oper o (shape) (set! shape v)))

;; asm-move
(define-inline (make-asm-move::asm-move shape1241::bstring live-in1242::obj live-out1243::obj trash1244::obj succ1245::obj use1246::obj def1247::obj) (instantiate::asm-move (shape shape1241) (live-in live-in1242) (live-out live-out1243) (trash trash1244) (succ succ1245) (use use1246) (def def1247)))
(define-inline (asm-move?::bool obj::obj) ((@ isa? __object) obj (@ asm-move iselect_asm)))
(define (asm-move-nil::asm-move) (class-nil (@ asm-move iselect_asm)))
(define-inline (asm-move-def::obj o::asm-move) (with-access::asm-move o (def) def))
(define-inline (asm-move-def-set! o::asm-move v::obj) (with-access::asm-move o (def) (set! def v)))
(define-inline (asm-move-use::obj o::asm-move) (with-access::asm-move o (use) use))
(define-inline (asm-move-use-set! o::asm-move v::obj) (with-access::asm-move o (use) (set! use v)))
(define-inline (asm-move-succ::obj o::asm-move) (with-access::asm-move o (succ) succ))
(define-inline (asm-move-succ-set! o::asm-move v::obj) (with-access::asm-move o (succ) (set! succ v)))
(define-inline (asm-move-trash::obj o::asm-move) (with-access::asm-move o (trash) trash))
(define-inline (asm-move-trash-set! o::asm-move v::obj) (with-access::asm-move o (trash) (set! trash v)))
(define-inline (asm-move-live-out::obj o::asm-move) (with-access::asm-move o (live-out) live-out))
(define-inline (asm-move-live-out-set! o::asm-move v::obj) (with-access::asm-move o (live-out) (set! live-out v)))
(define-inline (asm-move-live-in::obj o::asm-move) (with-access::asm-move o (live-in) live-in))
(define-inline (asm-move-live-in-set! o::asm-move v::obj) (with-access::asm-move o (live-in) (set! live-in v)))
(define-inline (asm-move-shape::bstring o::asm-move) (with-access::asm-move o (shape) shape))
(define-inline (asm-move-shape-set! o::asm-move v::bstring) (with-access::asm-move o (shape) (set! shape v)))

;; asm-jump
(define-inline (make-asm-jump::asm-jump shape1232::bstring live-in1233::obj live-out1234::obj trash1235::obj succ1236::obj use1237::obj def1238::obj asm-label1239::label) (instantiate::asm-jump (shape shape1232) (live-in live-in1233) (live-out live-out1234) (trash trash1235) (succ succ1236) (use use1237) (def def1238) (asm-label asm-label1239)))
(define-inline (asm-jump?::bool obj::obj) ((@ isa? __object) obj (@ asm-jump iselect_asm)))
(define (asm-jump-nil::asm-jump) (class-nil (@ asm-jump iselect_asm)))
(define-inline (asm-jump-asm-label::label o::asm-jump) (with-access::asm-jump o (asm-label) asm-label))
(define-inline (asm-jump-asm-label-set! o::asm-jump v::label) (with-access::asm-jump o (asm-label) (set! asm-label v)))
(define-inline (asm-jump-def::obj o::asm-jump) (with-access::asm-jump o (def) def))
(define-inline (asm-jump-def-set! o::asm-jump v::obj) (with-access::asm-jump o (def) (set! def v)))
(define-inline (asm-jump-use::obj o::asm-jump) (with-access::asm-jump o (use) use))
(define-inline (asm-jump-use-set! o::asm-jump v::obj) (with-access::asm-jump o (use) (set! use v)))
(define-inline (asm-jump-succ::obj o::asm-jump) (with-access::asm-jump o (succ) succ))
(define-inline (asm-jump-succ-set! o::asm-jump v::obj) (with-access::asm-jump o (succ) (set! succ v)))
(define-inline (asm-jump-trash::obj o::asm-jump) (with-access::asm-jump o (trash) trash))
(define-inline (asm-jump-trash-set! o::asm-jump v::obj) (with-access::asm-jump o (trash) (set! trash v)))
(define-inline (asm-jump-live-out::obj o::asm-jump) (with-access::asm-jump o (live-out) live-out))
(define-inline (asm-jump-live-out-set! o::asm-jump v::obj) (with-access::asm-jump o (live-out) (set! live-out v)))
(define-inline (asm-jump-live-in::obj o::asm-jump) (with-access::asm-jump o (live-in) live-in))
(define-inline (asm-jump-live-in-set! o::asm-jump v::obj) (with-access::asm-jump o (live-in) (set! live-in v)))
(define-inline (asm-jump-shape::bstring o::asm-jump) (with-access::asm-jump o (shape) shape))
(define-inline (asm-jump-shape-set! o::asm-jump v::bstring) (with-access::asm-jump o (shape) (set! shape v)))

;; asm-cjump
(define-inline (make-asm-cjump::asm-cjump shape1223::bstring live-in1224::obj live-out1225::obj trash1226::obj succ1227::obj use1228::obj def1229::obj asm-label1230::label) (instantiate::asm-cjump (shape shape1223) (live-in live-in1224) (live-out live-out1225) (trash trash1226) (succ succ1227) (use use1228) (def def1229) (asm-label asm-label1230)))
(define-inline (asm-cjump?::bool obj::obj) ((@ isa? __object) obj (@ asm-cjump iselect_asm)))
(define (asm-cjump-nil::asm-cjump) (class-nil (@ asm-cjump iselect_asm)))
(define-inline (asm-cjump-asm-label::label o::asm-cjump) (with-access::asm-cjump o (asm-label) asm-label))
(define-inline (asm-cjump-asm-label-set! o::asm-cjump v::label) (with-access::asm-cjump o (asm-label) (set! asm-label v)))
(define-inline (asm-cjump-def::obj o::asm-cjump) (with-access::asm-cjump o (def) def))
(define-inline (asm-cjump-def-set! o::asm-cjump v::obj) (with-access::asm-cjump o (def) (set! def v)))
(define-inline (asm-cjump-use::obj o::asm-cjump) (with-access::asm-cjump o (use) use))
(define-inline (asm-cjump-use-set! o::asm-cjump v::obj) (with-access::asm-cjump o (use) (set! use v)))
(define-inline (asm-cjump-succ::obj o::asm-cjump) (with-access::asm-cjump o (succ) succ))
(define-inline (asm-cjump-succ-set! o::asm-cjump v::obj) (with-access::asm-cjump o (succ) (set! succ v)))
(define-inline (asm-cjump-trash::obj o::asm-cjump) (with-access::asm-cjump o (trash) trash))
(define-inline (asm-cjump-trash-set! o::asm-cjump v::obj) (with-access::asm-cjump o (trash) (set! trash v)))
(define-inline (asm-cjump-live-out::obj o::asm-cjump) (with-access::asm-cjump o (live-out) live-out))
(define-inline (asm-cjump-live-out-set! o::asm-cjump v::obj) (with-access::asm-cjump o (live-out) (set! live-out v)))
(define-inline (asm-cjump-live-in::obj o::asm-cjump) (with-access::asm-cjump o (live-in) live-in))
(define-inline (asm-cjump-live-in-set! o::asm-cjump v::obj) (with-access::asm-cjump o (live-in) (set! live-in v)))
(define-inline (asm-cjump-shape::bstring o::asm-cjump) (with-access::asm-cjump o (shape) shape))
(define-inline (asm-cjump-shape-set! o::asm-cjump v::bstring) (with-access::asm-cjump o (shape) (set! shape v)))

;; asm-label
(define-inline (make-asm-label::asm-label framedecl1217::obj ident1218::ident live-in1219::obj live-out1220::obj succ1221::obj) (instantiate::asm-label (framedecl framedecl1217) (ident ident1218) (live-in live-in1219) (live-out live-out1220) (succ succ1221)))
(define-inline (asm-label?::bool obj::obj) ((@ isa? __object) obj (@ asm-label iselect_asm)))
(define (asm-label-nil::asm-label) (class-nil (@ asm-label iselect_asm)))
(define-inline (asm-label-succ::obj o::asm-label) (with-access::asm-label o (succ) succ))
(define-inline (asm-label-succ-set! o::asm-label v::obj) (with-access::asm-label o (succ) (set! succ v)))
(define-inline (asm-label-live-out::obj o::asm-label) (with-access::asm-label o (live-out) live-out))
(define-inline (asm-label-live-out-set! o::asm-label v::obj) (with-access::asm-label o (live-out) (set! live-out v)))
(define-inline (asm-label-live-in::obj o::asm-label) (with-access::asm-label o (live-in) live-in))
(define-inline (asm-label-live-in-set! o::asm-label v::obj) (with-access::asm-label o (live-in) (set! live-in v)))
(define-inline (asm-label-ident::ident o::asm-label) (with-access::asm-label o (ident) ident))
(define-inline (asm-label-ident-set! o::asm-label v::ident) (with-access::asm-label o (ident) (set! ident v)))
(define-inline (asm-label-framedecl::obj o::asm-label) (with-access::asm-label o (framedecl) framedecl))
(define-inline (asm-label-framedecl-set! o::asm-label v::obj) (with-access::asm-label o (framedecl) (set! framedecl v)))

;; asm-pseudo
(define-inline (make-asm-pseudo::asm-pseudo shape1207::bstring live-in1208::obj live-out1209::obj trash1210::obj succ1211::obj use1212::obj def1213::obj framedecl1214::obj) (instantiate::asm-pseudo (shape shape1207) (live-in live-in1208) (live-out live-out1209) (trash trash1210) (succ succ1211) (use use1212) (def def1213) (framedecl framedecl1214)))
(define-inline (asm-pseudo?::bool obj::obj) ((@ isa? __object) obj (@ asm-pseudo iselect_asm)))
(define (asm-pseudo-nil::asm-pseudo) (class-nil (@ asm-pseudo iselect_asm)))
(define-inline (asm-pseudo-framedecl::obj o::asm-pseudo) (with-access::asm-pseudo o (framedecl) framedecl))
(define-inline (asm-pseudo-framedecl-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (framedecl) (set! framedecl v)))
(define-inline (asm-pseudo-def::obj o::asm-pseudo) (with-access::asm-pseudo o (def) def))
(define-inline (asm-pseudo-def-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (def) (set! def v)))
(define-inline (asm-pseudo-use::obj o::asm-pseudo) (with-access::asm-pseudo o (use) use))
(define-inline (asm-pseudo-use-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (use) (set! use v)))
(define-inline (asm-pseudo-succ::obj o::asm-pseudo) (with-access::asm-pseudo o (succ) succ))
(define-inline (asm-pseudo-succ-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (succ) (set! succ v)))
(define-inline (asm-pseudo-trash::obj o::asm-pseudo) (with-access::asm-pseudo o (trash) trash))
(define-inline (asm-pseudo-trash-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (trash) (set! trash v)))
(define-inline (asm-pseudo-live-out::obj o::asm-pseudo) (with-access::asm-pseudo o (live-out) live-out))
(define-inline (asm-pseudo-live-out-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (live-out) (set! live-out v)))
(define-inline (asm-pseudo-live-in::obj o::asm-pseudo) (with-access::asm-pseudo o (live-in) live-in))
(define-inline (asm-pseudo-live-in-set! o::asm-pseudo v::obj) (with-access::asm-pseudo o (live-in) (set! live-in v)))
(define-inline (asm-pseudo-shape::bstring o::asm-pseudo) (with-access::asm-pseudo o (shape) shape))
(define-inline (asm-pseudo-shape-set! o::asm-pseudo v::bstring) (with-access::asm-pseudo o (shape) (set! shape v)))

;; proc-entry
(define-inline (make-proc-entry::proc-entry shape1198::bstring live-in1199::obj live-out1200::obj trash1201::obj succ1202::obj use1203::obj def1204::obj framedecl1205::obj) (instantiate::proc-entry (shape shape1198) (live-in live-in1199) (live-out live-out1200) (trash trash1201) (succ succ1202) (use use1203) (def def1204) (framedecl framedecl1205)))
(define-inline (proc-entry?::bool obj::obj) ((@ isa? __object) obj (@ proc-entry iselect_asm)))
(define (proc-entry-nil::proc-entry) (class-nil (@ proc-entry iselect_asm)))
(define-inline (proc-entry-framedecl::obj o::proc-entry) (with-access::proc-entry o (framedecl) framedecl))
(define-inline (proc-entry-framedecl-set! o::proc-entry v::obj) (with-access::proc-entry o (framedecl) (set! framedecl v)))
(define-inline (proc-entry-def::obj o::proc-entry) (with-access::proc-entry o (def) def))
(define-inline (proc-entry-def-set! o::proc-entry v::obj) (with-access::proc-entry o (def) (set! def v)))
(define-inline (proc-entry-use::obj o::proc-entry) (with-access::proc-entry o (use) use))
(define-inline (proc-entry-use-set! o::proc-entry v::obj) (with-access::proc-entry o (use) (set! use v)))
(define-inline (proc-entry-succ::obj o::proc-entry) (with-access::proc-entry o (succ) succ))
(define-inline (proc-entry-succ-set! o::proc-entry v::obj) (with-access::proc-entry o (succ) (set! succ v)))
(define-inline (proc-entry-trash::obj o::proc-entry) (with-access::proc-entry o (trash) trash))
(define-inline (proc-entry-trash-set! o::proc-entry v::obj) (with-access::proc-entry o (trash) (set! trash v)))
(define-inline (proc-entry-live-out::obj o::proc-entry) (with-access::proc-entry o (live-out) live-out))
(define-inline (proc-entry-live-out-set! o::proc-entry v::obj) (with-access::proc-entry o (live-out) (set! live-out v)))
(define-inline (proc-entry-live-in::obj o::proc-entry) (with-access::proc-entry o (live-in) live-in))
(define-inline (proc-entry-live-in-set! o::proc-entry v::obj) (with-access::proc-entry o (live-in) (set! live-in v)))
(define-inline (proc-entry-shape::bstring o::proc-entry) (with-access::proc-entry o (shape) shape))
(define-inline (proc-entry-shape-set! o::proc-entry v::bstring) (with-access::proc-entry o (shape) (set! shape v)))

;; proc-exit
(define-inline (make-proc-exit::proc-exit shape1189::bstring live-in1190::obj live-out1191::obj trash1192::obj succ1193::obj use1194::obj def1195::obj framedecl1196::obj) (instantiate::proc-exit (shape shape1189) (live-in live-in1190) (live-out live-out1191) (trash trash1192) (succ succ1193) (use use1194) (def def1195) (framedecl framedecl1196)))
(define-inline (proc-exit?::bool obj::obj) ((@ isa? __object) obj (@ proc-exit iselect_asm)))
(define (proc-exit-nil::proc-exit) (class-nil (@ proc-exit iselect_asm)))
(define-inline (proc-exit-framedecl::obj o::proc-exit) (with-access::proc-exit o (framedecl) framedecl))
(define-inline (proc-exit-framedecl-set! o::proc-exit v::obj) (with-access::proc-exit o (framedecl) (set! framedecl v)))
(define-inline (proc-exit-def::obj o::proc-exit) (with-access::proc-exit o (def) def))
(define-inline (proc-exit-def-set! o::proc-exit v::obj) (with-access::proc-exit o (def) (set! def v)))
(define-inline (proc-exit-use::obj o::proc-exit) (with-access::proc-exit o (use) use))
(define-inline (proc-exit-use-set! o::proc-exit v::obj) (with-access::proc-exit o (use) (set! use v)))
(define-inline (proc-exit-succ::obj o::proc-exit) (with-access::proc-exit o (succ) succ))
(define-inline (proc-exit-succ-set! o::proc-exit v::obj) (with-access::proc-exit o (succ) (set! succ v)))
(define-inline (proc-exit-trash::obj o::proc-exit) (with-access::proc-exit o (trash) trash))
(define-inline (proc-exit-trash-set! o::proc-exit v::obj) (with-access::proc-exit o (trash) (set! trash v)))
(define-inline (proc-exit-live-out::obj o::proc-exit) (with-access::proc-exit o (live-out) live-out))
(define-inline (proc-exit-live-out-set! o::proc-exit v::obj) (with-access::proc-exit o (live-out) (set! live-out v)))
(define-inline (proc-exit-live-in::obj o::proc-exit) (with-access::proc-exit o (live-in) live-in))
(define-inline (proc-exit-live-in-set! o::proc-exit v::obj) (with-access::proc-exit o (live-in) (set! live-in v)))
(define-inline (proc-exit-shape::bstring o::proc-exit) (with-access::proc-exit o (shape) shape))
(define-inline (proc-exit-shape-set! o::proc-exit v::bstring) (with-access::proc-exit o (shape) (set! shape v)))
))
