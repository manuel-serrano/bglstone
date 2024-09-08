;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Ir/inode.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; ir
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ir::ir text-segment1291::obj bss-segment1292::obj data-segment1293::obj)
    (inline ir?::bool ::obj)
    (ir-nil::ir)
    (inline ir-data-segment::obj ::ir)
    (inline ir-data-segment-set! ::ir ::obj)
    (inline ir-bss-segment::obj ::ir)
    (inline ir-bss-segment-set! ::ir ::obj)
    (inline ir-text-segment::obj ::ir)
    (inline ir-text-segment-set! ::ir ::obj))))

;; ir-stmt
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ir-stmt::ir-stmt)
    (inline ir-stmt?::bool ::obj)
    (ir-stmt-nil::ir-stmt))))

;; framedecl
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-framedecl::framedecl location1274::obj id1275::obj type1276::obj params1277::obj body1278::stmt depth1279::obj prolog1280::label link1281::obj ir-stmt1282::ir-stmt epilogue1283::label frame-pointer-saved?1284::obj local-num1285::int saved-registers1286::obj spill-num1287::int spilled1288::obj)
    (inline framedecl?::bool ::obj)
    (framedecl-nil::framedecl)
    (inline framedecl-spilled::obj ::framedecl)
    (inline framedecl-spilled-set! ::framedecl ::obj)
    (inline framedecl-spill-num::int ::framedecl)
    (inline framedecl-spill-num-set! ::framedecl ::int)
    (inline framedecl-saved-registers::obj ::framedecl)
    (inline framedecl-saved-registers-set! ::framedecl ::obj)
    (inline framedecl-local-num::int ::framedecl)
    (inline framedecl-local-num-set! ::framedecl ::int)
    (inline framedecl-frame-pointer-saved?::obj ::framedecl)
    (inline framedecl-frame-pointer-saved?-set! ::framedecl ::obj)
    (inline framedecl-epilogue::label ::framedecl)
    (inline framedecl-ir-stmt::ir-stmt ::framedecl)
    (inline framedecl-ir-stmt-set! ::framedecl ::ir-stmt)
    (inline framedecl-link::obj ::framedecl)
    (inline framedecl-prolog::label ::framedecl)
    (inline framedecl-depth::obj ::framedecl)
    (inline framedecl-depth-set! ::framedecl ::obj)
    (inline framedecl-body::stmt ::framedecl)
    (inline framedecl-params::obj ::framedecl)
    (inline framedecl-params-set! ::framedecl ::obj)
    (inline framedecl-type::obj ::framedecl)
    (inline framedecl-type-set! ::framedecl ::obj)
    (inline framedecl-id::obj ::framedecl)
    (inline framedecl-id-set! ::framedecl ::obj)
    (inline framedecl-location::obj ::framedecl))))

;; basic-block
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-basic-block::basic-block mark1269::obj prolog1270::label body1271::obj epilogue1272::ir-stmt)
    (inline basic-block?::bool ::obj)
    (basic-block-nil::basic-block)
    (inline basic-block-epilogue::ir-stmt ::basic-block)
    (inline basic-block-body::obj ::basic-block)
    (inline basic-block-prolog::label ::basic-block)
    (inline basic-block-mark::obj ::basic-block)
    (inline basic-block-mark-set! ::basic-block ::obj))))

;; ir-expr
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ir-expr::ir-expr)
    (inline ir-expr?::bool ::obj)
    (ir-expr-nil::ir-expr))))

;; ir-const
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ir-const::ir-const value1266::int)
    (inline ir-const?::bool ::obj)
    (ir-const-nil::ir-const)
    (inline ir-const-value::int ::ir-const))))

;; name
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-name::name label1264::label)
    (inline name?::bool ::obj)
    (name-nil::name)
    (inline name-label::label ::name))))

;; temp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-temp::temp key1259::obj name1260::bstring interference1261::obj hardware1262::obj)
    (inline temp?::bool ::obj)
    (temp-nil::temp)
    (inline temp-hardware::obj ::temp)
    (inline temp-hardware-set! ::temp ::obj)
    (inline temp-interference::obj ::temp)
    (inline temp-interference-set! ::temp ::obj)
    (inline temp-name::bstring ::temp)
    (inline temp-key::obj ::temp)
    (inline temp-key-set! ::temp ::obj))))

;; ireg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ireg::ireg key1253::obj name1254::bstring interference1255::obj hardware1256::obj temp1257::obj)
    (inline ireg?::bool ::obj)
    (ireg-nil::ireg)
    (inline ireg-temp::obj ::ireg)
    (inline ireg-temp-set! ::ireg ::obj)
    (inline ireg-hardware::obj ::ireg)
    (inline ireg-hardware-set! ::ireg ::obj)
    (inline ireg-interference::obj ::ireg)
    (inline ireg-interference-set! ::ireg ::obj)
    (inline ireg-name::bstring ::ireg)
    (inline ireg-key::obj ::ireg)
    (inline ireg-key-set! ::ireg ::obj))))

;; opfx
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-opfx::opfx op1249::symbol left1250::ir-expr right1251::ir-expr)
    (inline opfx?::bool ::obj)
    (opfx-nil::opfx)
    (inline opfx-right::ir-expr ::opfx)
    (inline opfx-right-set! ::opfx ::ir-expr)
    (inline opfx-left::ir-expr ::opfx)
    (inline opfx-left-set! ::opfx ::ir-expr)
    (inline opfx-op::symbol ::opfx))))

;; mem
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-mem::mem addr1247::ir-expr)
    (inline mem?::bool ::obj)
    (mem-nil::mem)
    (inline mem-addr::ir-expr ::mem)
    (inline mem-addr-set! ::mem ::ir-expr))))

;; call
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-call::call framedecl1244::fundecl args1245::obj)
    (inline call?::bool ::obj)
    (call-nil::call)
    (inline call-args::obj ::call)
    (inline call-framedecl::fundecl ::call))))

;; move-temp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-move-temp::move-temp temp1241::temp expr1242::ir-expr)
    (inline move-temp?::bool ::obj)
    (move-temp-nil::move-temp)
    (inline move-temp-expr::ir-expr ::move-temp)
    (inline move-temp-expr-set! ::move-temp ::ir-expr)
    (inline move-temp-temp::temp ::move-temp)
    (inline move-temp-temp-set! ::move-temp ::temp))))

;; move-mem
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-move-mem::move-mem addr1237::ir-expr k1238::int expr1239::ir-expr)
    (inline move-mem?::bool ::obj)
    (move-mem-nil::move-mem)
    (inline move-mem-expr::ir-expr ::move-mem)
    (inline move-mem-expr-set! ::move-mem ::ir-expr)
    (inline move-mem-k::int ::move-mem)
    (inline move-mem-addr::ir-expr ::move-mem)
    (inline move-mem-addr-set! ::move-mem ::ir-expr))))

;; estmt
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-estmt::estmt >expr1235::ir-expr)
    (inline estmt?::bool ::obj)
    (estmt-nil::estmt)
    (inline estmt->expr::ir-expr ::estmt)
    (inline estmt->expr-set! ::estmt ::ir-expr))))

;; jump
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-jump::jump addr1233::label)
    (inline jump?::bool ::obj)
    (jump-nil::jump)
    (inline jump-addr::label ::jump))))

;; cjump
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cjump::cjump op1227::symbol left1228::ir-expr right1229::ir-expr true1230::label false1231::label)
    (inline cjump?::bool ::obj)
    (cjump-nil::cjump)
    (inline cjump-false::label ::cjump)
    (inline cjump-true::label ::cjump)
    (inline cjump-right::ir-expr ::cjump)
    (inline cjump-left::ir-expr ::cjump)
    (inline cjump-op::symbol ::cjump))))

;; seq
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-seq::seq stmts1225::obj)
    (inline seq?::bool ::obj)
    (seq-nil::seq)
    (inline seq-stmts::obj ::seq))))

;; label
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-label::label framedecl1222::obj ident1223::ident)
    (inline label?::bool ::obj)
    (label-nil::label)
    (inline label-ident::ident ::label)
    (inline label-framedecl::obj ::label)
    (inline label-framedecl-set! ::label ::obj))))

;; label-bb
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-label-bb::label-bb framedecl1218::obj ident1219::ident basic-block1220::basic-block)
    (inline label-bb?::bool ::obj)
    (label-bb-nil::label-bb)
    (inline label-bb-basic-block::basic-block ::label-bb)
    (inline label-bb-ident::ident ::label-bb)
    (inline label-bb-framedecl::obj ::label-bb)
    (inline label-bb-framedecl-set! ::label-bb ::obj))))

;; pseudo-fundef
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-pseudo-fundef::pseudo-fundef name1215::bstring framedecl1216::framedecl)
    (inline pseudo-fundef?::bool ::obj)
    (pseudo-fundef-nil::pseudo-fundef)
    (inline pseudo-fundef-framedecl::framedecl ::pseudo-fundef)
    (inline pseudo-fundef-name::bstring ::pseudo-fundef))))

;; pseudo-return
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-pseudo-return::pseudo-return framedecl1213::framedecl)
    (inline pseudo-return?::bool ::obj)
    (pseudo-return-nil::pseudo-return)
    (inline pseudo-return-framedecl::framedecl ::pseudo-return)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ir
(define-inline (make-ir::ir text-segment1291::obj bss-segment1292::obj data-segment1293::obj) (instantiate::ir (text-segment text-segment1291) (bss-segment bss-segment1292) (data-segment data-segment1293)))
(define-inline (ir?::bool obj::obj) ((@ isa? __object) obj (@ ir ir_node)))
(define (ir-nil::ir) (class-nil (@ ir ir_node)))
(define-inline (ir-data-segment::obj o::ir) (with-access::ir o (data-segment) data-segment))
(define-inline (ir-data-segment-set! o::ir v::obj) (with-access::ir o (data-segment) (set! data-segment v)))
(define-inline (ir-bss-segment::obj o::ir) (with-access::ir o (bss-segment) bss-segment))
(define-inline (ir-bss-segment-set! o::ir v::obj) (with-access::ir o (bss-segment) (set! bss-segment v)))
(define-inline (ir-text-segment::obj o::ir) (with-access::ir o (text-segment) text-segment))
(define-inline (ir-text-segment-set! o::ir v::obj) (with-access::ir o (text-segment) (set! text-segment v)))

;; ir-stmt
(define-inline (make-ir-stmt::ir-stmt) (instantiate::ir-stmt))
(define-inline (ir-stmt?::bool obj::obj) ((@ isa? __object) obj (@ ir-stmt ir_node)))
(define (ir-stmt-nil::ir-stmt) (class-nil (@ ir-stmt ir_node)))

;; framedecl
(define-inline (make-framedecl::framedecl location1274::obj id1275::obj type1276::obj params1277::obj body1278::stmt depth1279::obj prolog1280::label link1281::obj ir-stmt1282::ir-stmt epilogue1283::label frame-pointer-saved?1284::obj local-num1285::int saved-registers1286::obj spill-num1287::int spilled1288::obj) (instantiate::framedecl (location location1274) (id id1275) (type type1276) (params params1277) (body body1278) (depth depth1279) (prolog prolog1280) (link link1281) (ir-stmt ir-stmt1282) (epilogue epilogue1283) (frame-pointer-saved? frame-pointer-saved?1284) (local-num local-num1285) (saved-registers saved-registers1286) (spill-num spill-num1287) (spilled spilled1288)))
(define-inline (framedecl?::bool obj::obj) ((@ isa? __object) obj (@ framedecl ir_node)))
(define (framedecl-nil::framedecl) (class-nil (@ framedecl ir_node)))
(define-inline (framedecl-spilled::obj o::framedecl) (with-access::framedecl o (spilled) spilled))
(define-inline (framedecl-spilled-set! o::framedecl v::obj) (with-access::framedecl o (spilled) (set! spilled v)))
(define-inline (framedecl-spill-num::int o::framedecl) (with-access::framedecl o (spill-num) spill-num))
(define-inline (framedecl-spill-num-set! o::framedecl v::int) (with-access::framedecl o (spill-num) (set! spill-num v)))
(define-inline (framedecl-saved-registers::obj o::framedecl) (with-access::framedecl o (saved-registers) saved-registers))
(define-inline (framedecl-saved-registers-set! o::framedecl v::obj) (with-access::framedecl o (saved-registers) (set! saved-registers v)))
(define-inline (framedecl-local-num::int o::framedecl) (with-access::framedecl o (local-num) local-num))
(define-inline (framedecl-local-num-set! o::framedecl v::int) (with-access::framedecl o (local-num) (set! local-num v)))
(define-inline (framedecl-frame-pointer-saved?::obj o::framedecl) (with-access::framedecl o (frame-pointer-saved?) frame-pointer-saved?))
(define-inline (framedecl-frame-pointer-saved?-set! o::framedecl v::obj) (with-access::framedecl o (frame-pointer-saved?) (set! frame-pointer-saved? v)))
(define-inline (framedecl-epilogue::label o::framedecl) (with-access::framedecl o (epilogue) epilogue))
(define-inline (framedecl-epilogue-set! o::framedecl v::label) (with-access::framedecl o (epilogue) (set! epilogue v)))
(define-inline (framedecl-ir-stmt::ir-stmt o::framedecl) (with-access::framedecl o (ir-stmt) ir-stmt))
(define-inline (framedecl-ir-stmt-set! o::framedecl v::ir-stmt) (with-access::framedecl o (ir-stmt) (set! ir-stmt v)))
(define-inline (framedecl-link::obj o::framedecl) (with-access::framedecl o (link) link))
(define-inline (framedecl-link-set! o::framedecl v::obj) (with-access::framedecl o (link) (set! link v)))
(define-inline (framedecl-prolog::label o::framedecl) (with-access::framedecl o (prolog) prolog))
(define-inline (framedecl-prolog-set! o::framedecl v::label) (with-access::framedecl o (prolog) (set! prolog v)))
(define-inline (framedecl-depth::obj o::framedecl) (with-access::framedecl o (depth) depth))
(define-inline (framedecl-depth-set! o::framedecl v::obj) (with-access::framedecl o (depth) (set! depth v)))
(define-inline (framedecl-body::stmt o::framedecl) (with-access::framedecl o (body) body))
(define-inline (framedecl-body-set! o::framedecl v::stmt) (with-access::framedecl o (body) (set! body v)))
(define-inline (framedecl-params::obj o::framedecl) (with-access::framedecl o (params) params))
(define-inline (framedecl-params-set! o::framedecl v::obj) (with-access::framedecl o (params) (set! params v)))
(define-inline (framedecl-type::obj o::framedecl) (with-access::framedecl o (type) type))
(define-inline (framedecl-type-set! o::framedecl v::obj) (with-access::framedecl o (type) (set! type v)))
(define-inline (framedecl-id::obj o::framedecl) (with-access::framedecl o (id) id))
(define-inline (framedecl-id-set! o::framedecl v::obj) (with-access::framedecl o (id) (set! id v)))
(define-inline (framedecl-location::obj o::framedecl) (with-access::framedecl o (location) location))
(define-inline (framedecl-location-set! o::framedecl v::obj) (with-access::framedecl o (location) (set! location v)))

;; basic-block
(define-inline (make-basic-block::basic-block mark1269::obj prolog1270::label body1271::obj epilogue1272::ir-stmt) (instantiate::basic-block (mark mark1269) (prolog prolog1270) (body body1271) (epilogue epilogue1272)))
(define-inline (basic-block?::bool obj::obj) ((@ isa? __object) obj (@ basic-block ir_node)))
(define (basic-block-nil::basic-block) (class-nil (@ basic-block ir_node)))
(define-inline (basic-block-epilogue::ir-stmt o::basic-block) (with-access::basic-block o (epilogue) epilogue))
(define-inline (basic-block-epilogue-set! o::basic-block v::ir-stmt) (with-access::basic-block o (epilogue) (set! epilogue v)))
(define-inline (basic-block-body::obj o::basic-block) (with-access::basic-block o (body) body))
(define-inline (basic-block-body-set! o::basic-block v::obj) (with-access::basic-block o (body) (set! body v)))
(define-inline (basic-block-prolog::label o::basic-block) (with-access::basic-block o (prolog) prolog))
(define-inline (basic-block-prolog-set! o::basic-block v::label) (with-access::basic-block o (prolog) (set! prolog v)))
(define-inline (basic-block-mark::obj o::basic-block) (with-access::basic-block o (mark) mark))
(define-inline (basic-block-mark-set! o::basic-block v::obj) (with-access::basic-block o (mark) (set! mark v)))

;; ir-expr
(define-inline (make-ir-expr::ir-expr) (instantiate::ir-expr))
(define-inline (ir-expr?::bool obj::obj) ((@ isa? __object) obj (@ ir-expr ir_node)))
(define (ir-expr-nil::ir-expr) (class-nil (@ ir-expr ir_node)))

;; ir-const
(define-inline (make-ir-const::ir-const value1266::int) (instantiate::ir-const (value value1266)))
(define-inline (ir-const?::bool obj::obj) ((@ isa? __object) obj (@ ir-const ir_node)))
(define (ir-const-nil::ir-const) (class-nil (@ ir-const ir_node)))
(define-inline (ir-const-value::int o::ir-const) (with-access::ir-const o (value) value))
(define-inline (ir-const-value-set! o::ir-const v::int) (with-access::ir-const o (value) (set! value v)))

;; name
(define-inline (make-name::name label1264::label) (instantiate::name (label label1264)))
(define-inline (name?::bool obj::obj) ((@ isa? __object) obj (@ name ir_node)))
(define (name-nil::name) (class-nil (@ name ir_node)))
(define-inline (name-label::label o::name) (with-access::name o (label) label))
(define-inline (name-label-set! o::name v::label) (with-access::name o (label) (set! label v)))

;; temp
(define-inline (make-temp::temp key1259::obj name1260::bstring interference1261::obj hardware1262::obj) (instantiate::temp (key key1259) (name name1260) (interference interference1261) (hardware hardware1262)))
(define-inline (temp?::bool obj::obj) ((@ isa? __object) obj (@ temp ir_node)))
(define (temp-nil::temp) (class-nil (@ temp ir_node)))
(define-inline (temp-hardware::obj o::temp) (with-access::temp o (hardware) hardware))
(define-inline (temp-hardware-set! o::temp v::obj) (with-access::temp o (hardware) (set! hardware v)))
(define-inline (temp-interference::obj o::temp) (with-access::temp o (interference) interference))
(define-inline (temp-interference-set! o::temp v::obj) (with-access::temp o (interference) (set! interference v)))
(define-inline (temp-name::bstring o::temp) (with-access::temp o (name) name))
(define-inline (temp-name-set! o::temp v::bstring) (with-access::temp o (name) (set! name v)))
(define-inline (temp-key::obj o::temp) (with-access::temp o (key) key))
(define-inline (temp-key-set! o::temp v::obj) (with-access::temp o (key) (set! key v)))

;; ireg
(define-inline (make-ireg::ireg key1253::obj name1254::bstring interference1255::obj hardware1256::obj temp1257::obj) (instantiate::ireg (key key1253) (name name1254) (interference interference1255) (hardware hardware1256) (temp temp1257)))
(define-inline (ireg?::bool obj::obj) ((@ isa? __object) obj (@ ireg ir_node)))
(define (ireg-nil::ireg) (class-nil (@ ireg ir_node)))
(define-inline (ireg-temp::obj o::ireg) (with-access::ireg o (temp) temp))
(define-inline (ireg-temp-set! o::ireg v::obj) (with-access::ireg o (temp) (set! temp v)))
(define-inline (ireg-hardware::obj o::ireg) (with-access::ireg o (hardware) hardware))
(define-inline (ireg-hardware-set! o::ireg v::obj) (with-access::ireg o (hardware) (set! hardware v)))
(define-inline (ireg-interference::obj o::ireg) (with-access::ireg o (interference) interference))
(define-inline (ireg-interference-set! o::ireg v::obj) (with-access::ireg o (interference) (set! interference v)))
(define-inline (ireg-name::bstring o::ireg) (with-access::ireg o (name) name))
(define-inline (ireg-name-set! o::ireg v::bstring) (with-access::ireg o (name) (set! name v)))
(define-inline (ireg-key::obj o::ireg) (with-access::ireg o (key) key))
(define-inline (ireg-key-set! o::ireg v::obj) (with-access::ireg o (key) (set! key v)))

;; opfx
(define-inline (make-opfx::opfx op1249::symbol left1250::ir-expr right1251::ir-expr) (instantiate::opfx (op op1249) (left left1250) (right right1251)))
(define-inline (opfx?::bool obj::obj) ((@ isa? __object) obj (@ opfx ir_node)))
(define (opfx-nil::opfx) (class-nil (@ opfx ir_node)))
(define-inline (opfx-right::ir-expr o::opfx) (with-access::opfx o (right) right))
(define-inline (opfx-right-set! o::opfx v::ir-expr) (with-access::opfx o (right) (set! right v)))
(define-inline (opfx-left::ir-expr o::opfx) (with-access::opfx o (left) left))
(define-inline (opfx-left-set! o::opfx v::ir-expr) (with-access::opfx o (left) (set! left v)))
(define-inline (opfx-op::symbol o::opfx) (with-access::opfx o (op) op))
(define-inline (opfx-op-set! o::opfx v::symbol) (with-access::opfx o (op) (set! op v)))

;; mem
(define-inline (make-mem::mem addr1247::ir-expr) (instantiate::mem (addr addr1247)))
(define-inline (mem?::bool obj::obj) ((@ isa? __object) obj (@ mem ir_node)))
(define (mem-nil::mem) (class-nil (@ mem ir_node)))
(define-inline (mem-addr::ir-expr o::mem) (with-access::mem o (addr) addr))
(define-inline (mem-addr-set! o::mem v::ir-expr) (with-access::mem o (addr) (set! addr v)))

;; call
(define-inline (make-call::call framedecl1244::fundecl args1245::obj) (instantiate::call (framedecl framedecl1244) (args args1245)))
(define-inline (call?::bool obj::obj) ((@ isa? __object) obj (@ call ir_node)))
(define (call-nil::call) (class-nil (@ call ir_node)))
(define-inline (call-args::obj o::call) (with-access::call o (args) args))
(define-inline (call-args-set! o::call v::obj) (with-access::call o (args) (set! args v)))
(define-inline (call-framedecl::fundecl o::call) (with-access::call o (framedecl) framedecl))
(define-inline (call-framedecl-set! o::call v::fundecl) (with-access::call o (framedecl) (set! framedecl v)))

;; move-temp
(define-inline (make-move-temp::move-temp temp1241::temp expr1242::ir-expr) (instantiate::move-temp (temp temp1241) (expr expr1242)))
(define-inline (move-temp?::bool obj::obj) ((@ isa? __object) obj (@ move-temp ir_node)))
(define (move-temp-nil::move-temp) (class-nil (@ move-temp ir_node)))
(define-inline (move-temp-expr::ir-expr o::move-temp) (with-access::move-temp o (expr) expr))
(define-inline (move-temp-expr-set! o::move-temp v::ir-expr) (with-access::move-temp o (expr) (set! expr v)))
(define-inline (move-temp-temp::temp o::move-temp) (with-access::move-temp o (temp) temp))
(define-inline (move-temp-temp-set! o::move-temp v::temp) (with-access::move-temp o (temp) (set! temp v)))

;; move-mem
(define-inline (make-move-mem::move-mem addr1237::ir-expr k1238::int expr1239::ir-expr) (instantiate::move-mem (addr addr1237) (k k1238) (expr expr1239)))
(define-inline (move-mem?::bool obj::obj) ((@ isa? __object) obj (@ move-mem ir_node)))
(define (move-mem-nil::move-mem) (class-nil (@ move-mem ir_node)))
(define-inline (move-mem-expr::ir-expr o::move-mem) (with-access::move-mem o (expr) expr))
(define-inline (move-mem-expr-set! o::move-mem v::ir-expr) (with-access::move-mem o (expr) (set! expr v)))
(define-inline (move-mem-k::int o::move-mem) (with-access::move-mem o (k) k))
(define-inline (move-mem-k-set! o::move-mem v::int) (with-access::move-mem o (k) (set! k v)))
(define-inline (move-mem-addr::ir-expr o::move-mem) (with-access::move-mem o (addr) addr))
(define-inline (move-mem-addr-set! o::move-mem v::ir-expr) (with-access::move-mem o (addr) (set! addr v)))

;; estmt
(define-inline (make-estmt::estmt >expr1235::ir-expr) (instantiate::estmt (>expr >expr1235)))
(define-inline (estmt?::bool obj::obj) ((@ isa? __object) obj (@ estmt ir_node)))
(define (estmt-nil::estmt) (class-nil (@ estmt ir_node)))
(define-inline (estmt->expr::ir-expr o::estmt) (with-access::estmt o (>expr) >expr))
(define-inline (estmt->expr-set! o::estmt v::ir-expr) (with-access::estmt o (>expr) (set! >expr v)))

;; jump
(define-inline (make-jump::jump addr1233::label) (instantiate::jump (addr addr1233)))
(define-inline (jump?::bool obj::obj) ((@ isa? __object) obj (@ jump ir_node)))
(define (jump-nil::jump) (class-nil (@ jump ir_node)))
(define-inline (jump-addr::label o::jump) (with-access::jump o (addr) addr))
(define-inline (jump-addr-set! o::jump v::label) (with-access::jump o (addr) (set! addr v)))

;; cjump
(define-inline (make-cjump::cjump op1227::symbol left1228::ir-expr right1229::ir-expr true1230::label false1231::label) (instantiate::cjump (op op1227) (left left1228) (right right1229) (true true1230) (false false1231)))
(define-inline (cjump?::bool obj::obj) ((@ isa? __object) obj (@ cjump ir_node)))
(define (cjump-nil::cjump) (class-nil (@ cjump ir_node)))
(define-inline (cjump-false::label o::cjump) (with-access::cjump o (false) false))
(define-inline (cjump-false-set! o::cjump v::label) (with-access::cjump o (false) (set! false v)))
(define-inline (cjump-true::label o::cjump) (with-access::cjump o (true) true))
(define-inline (cjump-true-set! o::cjump v::label) (with-access::cjump o (true) (set! true v)))
(define-inline (cjump-right::ir-expr o::cjump) (with-access::cjump o (right) right))
(define-inline (cjump-right-set! o::cjump v::ir-expr) (with-access::cjump o (right) (set! right v)))
(define-inline (cjump-left::ir-expr o::cjump) (with-access::cjump o (left) left))
(define-inline (cjump-left-set! o::cjump v::ir-expr) (with-access::cjump o (left) (set! left v)))
(define-inline (cjump-op::symbol o::cjump) (with-access::cjump o (op) op))
(define-inline (cjump-op-set! o::cjump v::symbol) (with-access::cjump o (op) (set! op v)))

;; seq
(define-inline (make-seq::seq stmts1225::obj) (instantiate::seq (stmts stmts1225)))
(define-inline (seq?::bool obj::obj) ((@ isa? __object) obj (@ seq ir_node)))
(define (seq-nil::seq) (class-nil (@ seq ir_node)))
(define-inline (seq-stmts::obj o::seq) (with-access::seq o (stmts) stmts))
(define-inline (seq-stmts-set! o::seq v::obj) (with-access::seq o (stmts) (set! stmts v)))

;; label
(define-inline (make-label::label framedecl1222::obj ident1223::ident) (instantiate::label (framedecl framedecl1222) (ident ident1223)))
(define-inline (label?::bool obj::obj) ((@ isa? __object) obj (@ label ir_node)))
(define (label-nil::label) (class-nil (@ label ir_node)))
(define-inline (label-ident::ident o::label) (with-access::label o (ident) ident))
(define-inline (label-ident-set! o::label v::ident) (with-access::label o (ident) (set! ident v)))
(define-inline (label-framedecl::obj o::label) (with-access::label o (framedecl) framedecl))
(define-inline (label-framedecl-set! o::label v::obj) (with-access::label o (framedecl) (set! framedecl v)))

;; label-bb
(define-inline (make-label-bb::label-bb framedecl1218::obj ident1219::ident basic-block1220::basic-block) (instantiate::label-bb (framedecl framedecl1218) (ident ident1219) (basic-block basic-block1220)))
(define-inline (label-bb?::bool obj::obj) ((@ isa? __object) obj (@ label-bb ir_node)))
(define (label-bb-nil::label-bb) (class-nil (@ label-bb ir_node)))
(define-inline (label-bb-basic-block::basic-block o::label-bb) (with-access::label-bb o (basic-block) basic-block))
(define-inline (label-bb-basic-block-set! o::label-bb v::basic-block) (with-access::label-bb o (basic-block) (set! basic-block v)))
(define-inline (label-bb-ident::ident o::label-bb) (with-access::label-bb o (ident) ident))
(define-inline (label-bb-ident-set! o::label-bb v::ident) (with-access::label-bb o (ident) (set! ident v)))
(define-inline (label-bb-framedecl::obj o::label-bb) (with-access::label-bb o (framedecl) framedecl))
(define-inline (label-bb-framedecl-set! o::label-bb v::obj) (with-access::label-bb o (framedecl) (set! framedecl v)))

;; pseudo-fundef
(define-inline (make-pseudo-fundef::pseudo-fundef name1215::bstring framedecl1216::framedecl) (instantiate::pseudo-fundef (name name1215) (framedecl framedecl1216)))
(define-inline (pseudo-fundef?::bool obj::obj) ((@ isa? __object) obj (@ pseudo-fundef ir_node)))
(define (pseudo-fundef-nil::pseudo-fundef) (class-nil (@ pseudo-fundef ir_node)))
(define-inline (pseudo-fundef-framedecl::framedecl o::pseudo-fundef) (with-access::pseudo-fundef o (framedecl) framedecl))
(define-inline (pseudo-fundef-framedecl-set! o::pseudo-fundef v::framedecl) (with-access::pseudo-fundef o (framedecl) (set! framedecl v)))
(define-inline (pseudo-fundef-name::bstring o::pseudo-fundef) (with-access::pseudo-fundef o (name) name))
(define-inline (pseudo-fundef-name-set! o::pseudo-fundef v::bstring) (with-access::pseudo-fundef o (name) (set! name v)))

;; pseudo-return
(define-inline (make-pseudo-return::pseudo-return framedecl1213::framedecl) (instantiate::pseudo-return (framedecl framedecl1213)))
(define-inline (pseudo-return?::bool obj::obj) ((@ isa? __object) obj (@ pseudo-return ir_node)))
(define (pseudo-return-nil::pseudo-return) (class-nil (@ pseudo-return ir_node)))
(define-inline (pseudo-return-framedecl::framedecl o::pseudo-return) (with-access::pseudo-return o (framedecl) framedecl))
(define-inline (pseudo-return-framedecl-set! o::pseudo-return v::framedecl) (with-access::pseudo-return o (framedecl) (set! framedecl v)))
))
