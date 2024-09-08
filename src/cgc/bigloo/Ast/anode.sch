;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Wed Mar 14 07:20:00 CET 2012 
;; (bigloo Ast/anode.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; ast
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ast::ast decl-list1345::obj)
    (inline ast?::bool ::obj)
    (ast-nil::ast)
    (inline ast-decl-list::obj ::ast)
    (inline ast-decl-list-set! ::ast ::obj))))

;; expr
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-expr::expr location1343::obj)
    (inline expr?::bool ::obj)
    (expr-nil::expr)
    (inline expr-location::obj ::expr))))

;; decl
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-decl::decl location1340::obj id1341::obj)
    (inline decl?::bool ::obj)
    (decl-nil::decl)
    (inline decl-id::obj ::decl)
    (inline decl-id-set! ::decl ::obj)
    (inline decl-location::obj ::decl))))

;; typespec
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-typespec::typespec location1337::obj id1338::obj)
    (inline typespec?::bool ::obj)
    (typespec-nil::typespec)
    (inline typespec-id::obj ::typespec)
    (inline typespec-id-set! ::typespec ::obj)
    (inline typespec-location::obj ::typespec))))

;; typespec-alias
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-typespec-alias::typespec-alias location1333::obj id1334::obj of1335::ident)
    (inline typespec-alias?::bool ::obj)
    (typespec-alias-nil::typespec-alias)
    (inline typespec-alias-of::ident ::typespec-alias)
    (inline typespec-alias-id::obj ::typespec-alias)
    (inline typespec-alias-id-set! ::typespec-alias ::obj)
    (inline typespec-alias-location::obj ::typespec-alias))))

;; typespec-record
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-typespec-record::typespec-record location1329::obj id1330::obj fields1331::obj)
    (inline typespec-record?::bool ::obj)
    (typespec-record-nil::typespec-record)
    (inline typespec-record-fields::obj ::typespec-record)
    (inline typespec-record-id::obj ::typespec-record)
    (inline typespec-record-id-set! ::typespec-record ::obj)
    (inline typespec-record-location::obj ::typespec-record))))

;; typespec-array
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-typespec-array::typespec-array location1325::obj id1326::obj of1327::ident)
    (inline typespec-array?::bool ::obj)
    (typespec-array-nil::typespec-array)
    (inline typespec-array-of::ident ::typespec-array)
    (inline typespec-array-id::obj ::typespec-array)
    (inline typespec-array-id-set! ::typespec-array ::obj)
    (inline typespec-array-location::obj ::typespec-array))))

;; vardecl
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-vardecl::vardecl location1320::obj id1321::obj fetch1322::obj type1323::obj)
    (inline vardecl?::bool ::obj)
    (vardecl-nil::vardecl)
    (inline vardecl-type::obj ::vardecl)
    (inline vardecl-type-set! ::vardecl ::obj)
    (inline vardecl-fetch::obj ::vardecl)
    (inline vardecl-fetch-set! ::vardecl ::obj)
    (inline vardecl-id::obj ::vardecl)
    (inline vardecl-id-set! ::vardecl ::obj)
    (inline vardecl-location::obj ::vardecl))))

;; local
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-local::local location1314::obj id1315::obj fetch1316::obj type1317::obj escape?1318::bool)
    (inline local?::bool ::obj)
    (local-nil::local)
    (inline local-escape?::bool ::local)
    (inline local-escape?-set! ::local ::bool)
    (inline local-type::obj ::local)
    (inline local-type-set! ::local ::obj)
    (inline local-fetch::obj ::local)
    (inline local-fetch-set! ::local ::obj)
    (inline local-id::obj ::local)
    (inline local-id-set! ::local ::obj)
    (inline local-location::obj ::local))))

;; global
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-global::global location1309::obj id1310::obj fetch1311::obj type1312::obj)
    (inline global?::bool ::obj)
    (global-nil::global)
    (inline global-type::obj ::global)
    (inline global-type-set! ::global ::obj)
    (inline global-fetch::obj ::global)
    (inline global-fetch-set! ::global ::obj)
    (inline global-id::obj ::global)
    (inline global-id-set! ::global ::obj)
    (inline global-location::obj ::global))))

;; fundecl
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-fundecl::fundecl location1302::obj id1303::obj type1304::obj params1305::obj body1306::stmt depth1307::obj)
    (inline fundecl?::bool ::obj)
    (fundecl-nil::fundecl)
    (inline fundecl-depth::obj ::fundecl)
    (inline fundecl-depth-set! ::fundecl ::obj)
    (inline fundecl-body::stmt ::fundecl)
    (inline fundecl-params::obj ::fundecl)
    (inline fundecl-params-set! ::fundecl ::obj)
    (inline fundecl-type::obj ::fundecl)
    (inline fundecl-type-set! ::fundecl ::obj)
    (inline fundecl-id::obj ::fundecl)
    (inline fundecl-id-set! ::fundecl ::obj)
    (inline fundecl-location::obj ::fundecl))))

;; stmt
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-stmt::stmt location1300::obj)
    (inline stmt?::bool ::obj)
    (stmt-nil::stmt)
    (inline stmt-location::obj ::stmt))))

;; block
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-block::block location1296::obj decl-list1297::obj stmt-list1298::obj)
    (inline block?::bool ::obj)
    (block-nil::block)
    (inline block-stmt-list::obj ::block)
    (inline block-decl-list::obj ::block)
    (inline block-location::obj ::block))))

;; if-then
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-if-then::if-then location1292::obj test1293::expr then1294::stmt)
    (inline if-then?::bool ::obj)
    (if-then-nil::if-then)
    (inline if-then-then::stmt ::if-then)
    (inline if-then-test::expr ::if-then)
    (inline if-then-test-set! ::if-then ::expr)
    (inline if-then-location::obj ::if-then))))

;; if-then-else
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-if-then-else::if-then-else location1285::obj test1288::expr then1289::stmt otherwise1290::stmt)
    (inline if-then-else?::bool ::obj)
    (if-then-else-nil::if-then-else)
    (inline if-then-else-otherwise::stmt ::if-then-else)
    (inline if-then-else-then::stmt ::if-then-else)
    (inline if-then-else-test::expr ::if-then-else)
    (inline if-then-else-test-set! ::if-then-else ::expr)
    (inline if-then-else-location::obj ::if-then-else))))

;; setq
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-setq::setq location1281::obj varref1282::obj value1283::expr)
    (inline setq?::bool ::obj)
    (setq-nil::setq)
    (inline setq-value::expr ::setq)
    (inline setq-value-set! ::setq ::expr)
    (inline setq-varref::obj ::setq)
    (inline setq-varref-set! ::setq ::obj)
    (inline setq-location::obj ::setq))))

;; aset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-aset::aset location1277::obj aref1278::aref value1279::expr)
    (inline aset?::bool ::obj)
    (aset-nil::aset)
    (inline aset-value::expr ::aset)
    (inline aset-value-set! ::aset ::expr)
    (inline aset-aref::aref ::aset)
    (inline aset-location::obj ::aset))))

;; rset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rset::rset location1273::obj rref1274::rref value1275::expr)
    (inline rset?::bool ::obj)
    (rset-nil::rset)
    (inline rset-value::expr ::rset)
    (inline rset-value-set! ::rset ::expr)
    (inline rset-rref::rref ::rset)
    (inline rset-location::obj ::rset))))

;; return
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-return::return location1271::obj)
    (inline return?::bool ::obj)
    (return-nil::return)
    (inline return-location::obj ::return))))

;; break
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-break::break location1269::obj)
    (inline break?::bool ::obj)
    (break-nil::break)
    (inline break-location::obj ::break))))

;; return-value
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-return-value::return-value location1266::obj value1267::expr)
    (inline return-value?::bool ::obj)
    (return-value-nil::return-value)
    (inline return-value-value::expr ::return-value)
    (inline return-value-value-set! ::return-value ::expr)
    (inline return-value-location::obj ::return-value))))

;; while
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-while::while location1262::obj test1263::expr body1264::stmt)
    (inline while?::bool ::obj)
    (while-nil::while)
    (inline while-body::stmt ::while)
    (inline while-test::expr ::while)
    (inline while-test-set! ::while ::expr)
    (inline while-location::obj ::while))))

;; exprstmt
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-exprstmt::exprstmt location1260::obj)
    (inline exprstmt?::bool ::obj)
    (exprstmt-nil::exprstmt)
    (inline exprstmt-location::obj ::exprstmt))))

;; exprstmt-value
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-exprstmt-value::exprstmt-value location1257::obj >expr1258::expr)
    (inline exprstmt-value?::bool ::obj)
    (exprstmt-value-nil::exprstmt-value)
    (inline exprstmt-value->expr::expr ::exprstmt-value)
    (inline exprstmt-value->expr-set! ::exprstmt-value ::expr)
    (inline exprstmt-value-location::obj ::exprstmt-value))))

;; binop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-binop::binop location1252::obj id1253::ident left1254::expr right1255::expr)
    (inline binop?::bool ::obj)
    (binop-nil::binop)
    (inline binop-right::expr ::binop)
    (inline binop-right-set! ::binop ::expr)
    (inline binop-left::expr ::binop)
    (inline binop-left-set! ::binop ::expr)
    (inline binop-id::ident ::binop)
    (inline binop-location::obj ::binop))))

;; funcall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-funcall::funcall location1248::obj fun1249::obj actuals1250::obj)
    (inline funcall?::bool ::obj)
    (funcall-nil::funcall)
    (inline funcall-actuals::obj ::funcall)
    (inline funcall-actuals-set! ::funcall ::obj)
    (inline funcall-fun::obj ::funcall)
    (inline funcall-fun-set! ::funcall ::obj)
    (inline funcall-location::obj ::funcall))))

;; aref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-aref::aref location1244::obj array1245::expr offset1246::expr)
    (inline aref?::bool ::obj)
    (aref-nil::aref)
    (inline aref-offset::expr ::aref)
    (inline aref-offset-set! ::aref ::expr)
    (inline aref-array::expr ::aref)
    (inline aref-array-set! ::aref ::expr)
    (inline aref-location::obj ::aref))))

;; rref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rref::rref location1239::obj record1240::expr record-type1241::obj field-id1242::ident)
    (inline rref?::bool ::obj)
    (rref-nil::rref)
    (inline rref-field-id::ident ::rref)
    (inline rref-field-id-set! ::rref ::ident)
    (inline rref-record-type::obj ::rref)
    (inline rref-record-type-set! ::rref ::obj)
    (inline rref-record::expr ::rref)
    (inline rref-record-set! ::rref ::expr)
    (inline rref-location::obj ::rref))))

;; const
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-const::const location1235::obj value1236::obj type1237::obj)
    (inline const?::bool ::obj)
    (const-nil::const)
    (inline const-type::obj ::const)
    (inline const-type-set! ::const ::obj)
    (inline const-value::obj ::const)
    (inline const-location::obj ::const))))

;; varref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-varref::varref location1231::obj depth1232::int vardecl1233::obj)
    (inline varref?::bool ::obj)
    (varref-nil::varref)
    (inline varref-vardecl::obj ::varref)
    (inline varref-vardecl-set! ::varref ::obj)
    (inline varref-depth::int ::varref)
    (inline varref-depth-set! ::varref ::int)
    (inline varref-location::obj ::varref)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ast
(define-inline (make-ast::ast decl-list1345::obj) (instantiate::ast (decl-list decl-list1345)))
(define-inline (ast?::bool obj::obj) ((@ isa? __object) obj (@ ast ast_node)))
(define (ast-nil::ast) (class-nil (@ ast ast_node)))
(define-inline (ast-decl-list::obj o::ast) (with-access::ast o (decl-list) decl-list))
(define-inline (ast-decl-list-set! o::ast v::obj) (with-access::ast o (decl-list) (set! decl-list v)))

;; expr
(define-inline (make-expr::expr location1343::obj) (instantiate::expr (location location1343)))
(define-inline (expr?::bool obj::obj) ((@ isa? __object) obj (@ expr ast_node)))
(define (expr-nil::expr) (class-nil (@ expr ast_node)))
(define-inline (expr-location::obj o::expr) (with-access::expr o (location) location))
(define-inline (expr-location-set! o::expr v::obj) (with-access::expr o (location) (set! location v)))

;; decl
(define-inline (make-decl::decl location1340::obj id1341::obj) (instantiate::decl (location location1340) (id id1341)))
(define-inline (decl?::bool obj::obj) ((@ isa? __object) obj (@ decl ast_node)))
(define (decl-nil::decl) (class-nil (@ decl ast_node)))
(define-inline (decl-id::obj o::decl) (with-access::decl o (id) id))
(define-inline (decl-id-set! o::decl v::obj) (with-access::decl o (id) (set! id v)))
(define-inline (decl-location::obj o::decl) (with-access::decl o (location) location))
(define-inline (decl-location-set! o::decl v::obj) (with-access::decl o (location) (set! location v)))

;; typespec
(define-inline (make-typespec::typespec location1337::obj id1338::obj) (instantiate::typespec (location location1337) (id id1338)))
(define-inline (typespec?::bool obj::obj) ((@ isa? __object) obj (@ typespec ast_node)))
(define (typespec-nil::typespec) (class-nil (@ typespec ast_node)))
(define-inline (typespec-id::obj o::typespec) (with-access::typespec o (id) id))
(define-inline (typespec-id-set! o::typespec v::obj) (with-access::typespec o (id) (set! id v)))
(define-inline (typespec-location::obj o::typespec) (with-access::typespec o (location) location))
(define-inline (typespec-location-set! o::typespec v::obj) (with-access::typespec o (location) (set! location v)))

;; typespec-alias
(define-inline (make-typespec-alias::typespec-alias location1333::obj id1334::obj of1335::ident) (instantiate::typespec-alias (location location1333) (id id1334) (of of1335)))
(define-inline (typespec-alias?::bool obj::obj) ((@ isa? __object) obj (@ typespec-alias ast_node)))
(define (typespec-alias-nil::typespec-alias) (class-nil (@ typespec-alias ast_node)))
(define-inline (typespec-alias-of::ident o::typespec-alias) (with-access::typespec-alias o (of) of))
(define-inline (typespec-alias-of-set! o::typespec-alias v::ident) (with-access::typespec-alias o (of) (set! of v)))
(define-inline (typespec-alias-id::obj o::typespec-alias) (with-access::typespec-alias o (id) id))
(define-inline (typespec-alias-id-set! o::typespec-alias v::obj) (with-access::typespec-alias o (id) (set! id v)))
(define-inline (typespec-alias-location::obj o::typespec-alias) (with-access::typespec-alias o (location) location))
(define-inline (typespec-alias-location-set! o::typespec-alias v::obj) (with-access::typespec-alias o (location) (set! location v)))

;; typespec-record
(define-inline (make-typespec-record::typespec-record location1329::obj id1330::obj fields1331::obj) (instantiate::typespec-record (location location1329) (id id1330) (fields fields1331)))
(define-inline (typespec-record?::bool obj::obj) ((@ isa? __object) obj (@ typespec-record ast_node)))
(define (typespec-record-nil::typespec-record) (class-nil (@ typespec-record ast_node)))
(define-inline (typespec-record-fields::obj o::typespec-record) (with-access::typespec-record o (fields) fields))
(define-inline (typespec-record-fields-set! o::typespec-record v::obj) (with-access::typespec-record o (fields) (set! fields v)))
(define-inline (typespec-record-id::obj o::typespec-record) (with-access::typespec-record o (id) id))
(define-inline (typespec-record-id-set! o::typespec-record v::obj) (with-access::typespec-record o (id) (set! id v)))
(define-inline (typespec-record-location::obj o::typespec-record) (with-access::typespec-record o (location) location))
(define-inline (typespec-record-location-set! o::typespec-record v::obj) (with-access::typespec-record o (location) (set! location v)))

;; typespec-array
(define-inline (make-typespec-array::typespec-array location1325::obj id1326::obj of1327::ident) (instantiate::typespec-array (location location1325) (id id1326) (of of1327)))
(define-inline (typespec-array?::bool obj::obj) ((@ isa? __object) obj (@ typespec-array ast_node)))
(define (typespec-array-nil::typespec-array) (class-nil (@ typespec-array ast_node)))
(define-inline (typespec-array-of::ident o::typespec-array) (with-access::typespec-array o (of) of))
(define-inline (typespec-array-of-set! o::typespec-array v::ident) (with-access::typespec-array o (of) (set! of v)))
(define-inline (typespec-array-id::obj o::typespec-array) (with-access::typespec-array o (id) id))
(define-inline (typespec-array-id-set! o::typespec-array v::obj) (with-access::typespec-array o (id) (set! id v)))
(define-inline (typespec-array-location::obj o::typespec-array) (with-access::typespec-array o (location) location))
(define-inline (typespec-array-location-set! o::typespec-array v::obj) (with-access::typespec-array o (location) (set! location v)))

;; vardecl
(define-inline (make-vardecl::vardecl location1320::obj id1321::obj fetch1322::obj type1323::obj) (instantiate::vardecl (location location1320) (id id1321) (fetch fetch1322) (type type1323)))
(define-inline (vardecl?::bool obj::obj) ((@ isa? __object) obj (@ vardecl ast_node)))
(define (vardecl-nil::vardecl) (class-nil (@ vardecl ast_node)))
(define-inline (vardecl-type::obj o::vardecl) (with-access::vardecl o (type) type))
(define-inline (vardecl-type-set! o::vardecl v::obj) (with-access::vardecl o (type) (set! type v)))
(define-inline (vardecl-fetch::obj o::vardecl) (with-access::vardecl o (fetch) fetch))
(define-inline (vardecl-fetch-set! o::vardecl v::obj) (with-access::vardecl o (fetch) (set! fetch v)))
(define-inline (vardecl-id::obj o::vardecl) (with-access::vardecl o (id) id))
(define-inline (vardecl-id-set! o::vardecl v::obj) (with-access::vardecl o (id) (set! id v)))
(define-inline (vardecl-location::obj o::vardecl) (with-access::vardecl o (location) location))
(define-inline (vardecl-location-set! o::vardecl v::obj) (with-access::vardecl o (location) (set! location v)))

;; local
(define-inline (make-local::local location1314::obj id1315::obj fetch1316::obj type1317::obj escape?1318::bool) (instantiate::local (location location1314) (id id1315) (fetch fetch1316) (type type1317) (escape? escape?1318)))
(define-inline (local?::bool obj::obj) ((@ isa? __object) obj (@ local ast_node)))
(define (local-nil::local) (class-nil (@ local ast_node)))
(define-inline (local-escape?::bool o::local) (with-access::local o (escape?) escape?))
(define-inline (local-escape?-set! o::local v::bool) (with-access::local o (escape?) (set! escape? v)))
(define-inline (local-type::obj o::local) (with-access::local o (type) type))
(define-inline (local-type-set! o::local v::obj) (with-access::local o (type) (set! type v)))
(define-inline (local-fetch::obj o::local) (with-access::local o (fetch) fetch))
(define-inline (local-fetch-set! o::local v::obj) (with-access::local o (fetch) (set! fetch v)))
(define-inline (local-id::obj o::local) (with-access::local o (id) id))
(define-inline (local-id-set! o::local v::obj) (with-access::local o (id) (set! id v)))
(define-inline (local-location::obj o::local) (with-access::local o (location) location))
(define-inline (local-location-set! o::local v::obj) (with-access::local o (location) (set! location v)))

;; global
(define-inline (make-global::global location1309::obj id1310::obj fetch1311::obj type1312::obj) (instantiate::global (location location1309) (id id1310) (fetch fetch1311) (type type1312)))
(define-inline (global?::bool obj::obj) ((@ isa? __object) obj (@ global ast_node)))
(define (global-nil::global) (class-nil (@ global ast_node)))
(define-inline (global-type::obj o::global) (with-access::global o (type) type))
(define-inline (global-type-set! o::global v::obj) (with-access::global o (type) (set! type v)))
(define-inline (global-fetch::obj o::global) (with-access::global o (fetch) fetch))
(define-inline (global-fetch-set! o::global v::obj) (with-access::global o (fetch) (set! fetch v)))
(define-inline (global-id::obj o::global) (with-access::global o (id) id))
(define-inline (global-id-set! o::global v::obj) (with-access::global o (id) (set! id v)))
(define-inline (global-location::obj o::global) (with-access::global o (location) location))
(define-inline (global-location-set! o::global v::obj) (with-access::global o (location) (set! location v)))

;; fundecl
(define-inline (make-fundecl::fundecl location1302::obj id1303::obj type1304::obj params1305::obj body1306::stmt depth1307::obj) (instantiate::fundecl (location location1302) (id id1303) (type type1304) (params params1305) (body body1306) (depth depth1307)))
(define-inline (fundecl?::bool obj::obj) ((@ isa? __object) obj (@ fundecl ast_node)))
(define (fundecl-nil::fundecl) (class-nil (@ fundecl ast_node)))
(define-inline (fundecl-depth::obj o::fundecl) (with-access::fundecl o (depth) depth))
(define-inline (fundecl-depth-set! o::fundecl v::obj) (with-access::fundecl o (depth) (set! depth v)))
(define-inline (fundecl-body::stmt o::fundecl) (with-access::fundecl o (body) body))
(define-inline (fundecl-body-set! o::fundecl v::stmt) (with-access::fundecl o (body) (set! body v)))
(define-inline (fundecl-params::obj o::fundecl) (with-access::fundecl o (params) params))
(define-inline (fundecl-params-set! o::fundecl v::obj) (with-access::fundecl o (params) (set! params v)))
(define-inline (fundecl-type::obj o::fundecl) (with-access::fundecl o (type) type))
(define-inline (fundecl-type-set! o::fundecl v::obj) (with-access::fundecl o (type) (set! type v)))
(define-inline (fundecl-id::obj o::fundecl) (with-access::fundecl o (id) id))
(define-inline (fundecl-id-set! o::fundecl v::obj) (with-access::fundecl o (id) (set! id v)))
(define-inline (fundecl-location::obj o::fundecl) (with-access::fundecl o (location) location))
(define-inline (fundecl-location-set! o::fundecl v::obj) (with-access::fundecl o (location) (set! location v)))

;; stmt
(define-inline (make-stmt::stmt location1300::obj) (instantiate::stmt (location location1300)))
(define-inline (stmt?::bool obj::obj) ((@ isa? __object) obj (@ stmt ast_node)))
(define (stmt-nil::stmt) (class-nil (@ stmt ast_node)))
(define-inline (stmt-location::obj o::stmt) (with-access::stmt o (location) location))
(define-inline (stmt-location-set! o::stmt v::obj) (with-access::stmt o (location) (set! location v)))

;; block
(define-inline (make-block::block location1296::obj decl-list1297::obj stmt-list1298::obj) (instantiate::block (location location1296) (decl-list decl-list1297) (stmt-list stmt-list1298)))
(define-inline (block?::bool obj::obj) ((@ isa? __object) obj (@ block ast_node)))
(define (block-nil::block) (class-nil (@ block ast_node)))
(define-inline (block-stmt-list::obj o::block) (with-access::block o (stmt-list) stmt-list))
(define-inline (block-stmt-list-set! o::block v::obj) (with-access::block o (stmt-list) (set! stmt-list v)))
(define-inline (block-decl-list::obj o::block) (with-access::block o (decl-list) decl-list))
(define-inline (block-decl-list-set! o::block v::obj) (with-access::block o (decl-list) (set! decl-list v)))
(define-inline (block-location::obj o::block) (with-access::block o (location) location))
(define-inline (block-location-set! o::block v::obj) (with-access::block o (location) (set! location v)))

;; if-then
(define-inline (make-if-then::if-then location1292::obj test1293::expr then1294::stmt) (instantiate::if-then (location location1292) (test test1293) (then then1294)))
(define-inline (if-then?::bool obj::obj) ((@ isa? __object) obj (@ if-then ast_node)))
(define (if-then-nil::if-then) (class-nil (@ if-then ast_node)))
(define-inline (if-then-then::stmt o::if-then) (with-access::if-then o (then) then))
(define-inline (if-then-then-set! o::if-then v::stmt) (with-access::if-then o (then) (set! then v)))
(define-inline (if-then-test::expr o::if-then) (with-access::if-then o (test) test))
(define-inline (if-then-test-set! o::if-then v::expr) (with-access::if-then o (test) (set! test v)))
(define-inline (if-then-location::obj o::if-then) (with-access::if-then o (location) location))
(define-inline (if-then-location-set! o::if-then v::obj) (with-access::if-then o (location) (set! location v)))

;; if-then-else
(define-inline (make-if-then-else::if-then-else location1285::obj test1288::expr then1289::stmt otherwise1290::stmt) (instantiate::if-then-else (location location1285) (test test1288) (then then1289) (otherwise otherwise1290)))
(define-inline (if-then-else?::bool obj::obj) ((@ isa? __object) obj (@ if-then-else ast_node)))
(define (if-then-else-nil::if-then-else) (class-nil (@ if-then-else ast_node)))
(define-inline (if-then-else-otherwise::stmt o::if-then-else) (with-access::if-then-else o (otherwise) otherwise))
(define-inline (if-then-else-otherwise-set! o::if-then-else v::stmt) (with-access::if-then-else o (otherwise) (set! otherwise v)))
(define-inline (if-then-else-then::stmt o::if-then-else) (with-access::if-then-else o (then) then))
(define-inline (if-then-else-then-set! o::if-then-else v::stmt) (with-access::if-then-else o (then) (set! then v)))
(define-inline (if-then-else-test::expr o::if-then-else) (with-access::if-then-else o (test) test))
(define-inline (if-then-else-test-set! o::if-then-else v::expr) (with-access::if-then-else o (test) (set! test v)))
(define-inline (if-then-else-location::obj o::if-then-else) (with-access::if-then-else o (location) location))
(define-inline (if-then-else-location-set! o::if-then-else v::obj) (with-access::if-then-else o (location) (set! location v)))

;; setq
(define-inline (make-setq::setq location1281::obj varref1282::obj value1283::expr) (instantiate::setq (location location1281) (varref varref1282) (value value1283)))
(define-inline (setq?::bool obj::obj) ((@ isa? __object) obj (@ setq ast_node)))
(define (setq-nil::setq) (class-nil (@ setq ast_node)))
(define-inline (setq-value::expr o::setq) (with-access::setq o (value) value))
(define-inline (setq-value-set! o::setq v::expr) (with-access::setq o (value) (set! value v)))
(define-inline (setq-varref::obj o::setq) (with-access::setq o (varref) varref))
(define-inline (setq-varref-set! o::setq v::obj) (with-access::setq o (varref) (set! varref v)))
(define-inline (setq-location::obj o::setq) (with-access::setq o (location) location))
(define-inline (setq-location-set! o::setq v::obj) (with-access::setq o (location) (set! location v)))

;; aset
(define-inline (make-aset::aset location1277::obj aref1278::aref value1279::expr) (instantiate::aset (location location1277) (aref aref1278) (value value1279)))
(define-inline (aset?::bool obj::obj) ((@ isa? __object) obj (@ aset ast_node)))
(define (aset-nil::aset) (class-nil (@ aset ast_node)))
(define-inline (aset-value::expr o::aset) (with-access::aset o (value) value))
(define-inline (aset-value-set! o::aset v::expr) (with-access::aset o (value) (set! value v)))
(define-inline (aset-aref::aref o::aset) (with-access::aset o (aref) aref))
(define-inline (aset-aref-set! o::aset v::aref) (with-access::aset o (aref) (set! aref v)))
(define-inline (aset-location::obj o::aset) (with-access::aset o (location) location))
(define-inline (aset-location-set! o::aset v::obj) (with-access::aset o (location) (set! location v)))

;; rset
(define-inline (make-rset::rset location1273::obj rref1274::rref value1275::expr) (instantiate::rset (location location1273) (rref rref1274) (value value1275)))
(define-inline (rset?::bool obj::obj) ((@ isa? __object) obj (@ rset ast_node)))
(define (rset-nil::rset) (class-nil (@ rset ast_node)))
(define-inline (rset-value::expr o::rset) (with-access::rset o (value) value))
(define-inline (rset-value-set! o::rset v::expr) (with-access::rset o (value) (set! value v)))
(define-inline (rset-rref::rref o::rset) (with-access::rset o (rref) rref))
(define-inline (rset-rref-set! o::rset v::rref) (with-access::rset o (rref) (set! rref v)))
(define-inline (rset-location::obj o::rset) (with-access::rset o (location) location))
(define-inline (rset-location-set! o::rset v::obj) (with-access::rset o (location) (set! location v)))

;; return
(define-inline (make-return::return location1271::obj) (instantiate::return (location location1271)))
(define-inline (return?::bool obj::obj) ((@ isa? __object) obj (@ return ast_node)))
(define (return-nil::return) (class-nil (@ return ast_node)))
(define-inline (return-location::obj o::return) (with-access::return o (location) location))
(define-inline (return-location-set! o::return v::obj) (with-access::return o (location) (set! location v)))

;; break
(define-inline (make-break::break location1269::obj) (instantiate::break (location location1269)))
(define-inline (break?::bool obj::obj) ((@ isa? __object) obj (@ break ast_node)))
(define (break-nil::break) (class-nil (@ break ast_node)))
(define-inline (break-location::obj o::break) (with-access::break o (location) location))
(define-inline (break-location-set! o::break v::obj) (with-access::break o (location) (set! location v)))

;; return-value
(define-inline (make-return-value::return-value location1266::obj value1267::expr) (instantiate::return-value (location location1266) (value value1267)))
(define-inline (return-value?::bool obj::obj) ((@ isa? __object) obj (@ return-value ast_node)))
(define (return-value-nil::return-value) (class-nil (@ return-value ast_node)))
(define-inline (return-value-value::expr o::return-value) (with-access::return-value o (value) value))
(define-inline (return-value-value-set! o::return-value v::expr) (with-access::return-value o (value) (set! value v)))
(define-inline (return-value-location::obj o::return-value) (with-access::return-value o (location) location))
(define-inline (return-value-location-set! o::return-value v::obj) (with-access::return-value o (location) (set! location v)))

;; while
(define-inline (make-while::while location1262::obj test1263::expr body1264::stmt) (instantiate::while (location location1262) (test test1263) (body body1264)))
(define-inline (while?::bool obj::obj) ((@ isa? __object) obj (@ while ast_node)))
(define (while-nil::while) (class-nil (@ while ast_node)))
(define-inline (while-body::stmt o::while) (with-access::while o (body) body))
(define-inline (while-body-set! o::while v::stmt) (with-access::while o (body) (set! body v)))
(define-inline (while-test::expr o::while) (with-access::while o (test) test))
(define-inline (while-test-set! o::while v::expr) (with-access::while o (test) (set! test v)))
(define-inline (while-location::obj o::while) (with-access::while o (location) location))
(define-inline (while-location-set! o::while v::obj) (with-access::while o (location) (set! location v)))

;; exprstmt
(define-inline (make-exprstmt::exprstmt location1260::obj) (instantiate::exprstmt (location location1260)))
(define-inline (exprstmt?::bool obj::obj) ((@ isa? __object) obj (@ exprstmt ast_node)))
(define (exprstmt-nil::exprstmt) (class-nil (@ exprstmt ast_node)))
(define-inline (exprstmt-location::obj o::exprstmt) (with-access::exprstmt o (location) location))
(define-inline (exprstmt-location-set! o::exprstmt v::obj) (with-access::exprstmt o (location) (set! location v)))

;; exprstmt-value
(define-inline (make-exprstmt-value::exprstmt-value location1257::obj >expr1258::expr) (instantiate::exprstmt-value (location location1257) (>expr >expr1258)))
(define-inline (exprstmt-value?::bool obj::obj) ((@ isa? __object) obj (@ exprstmt-value ast_node)))
(define (exprstmt-value-nil::exprstmt-value) (class-nil (@ exprstmt-value ast_node)))
(define-inline (exprstmt-value->expr::expr o::exprstmt-value) (with-access::exprstmt-value o (>expr) >expr))
(define-inline (exprstmt-value->expr-set! o::exprstmt-value v::expr) (with-access::exprstmt-value o (>expr) (set! >expr v)))
(define-inline (exprstmt-value-location::obj o::exprstmt-value) (with-access::exprstmt-value o (location) location))
(define-inline (exprstmt-value-location-set! o::exprstmt-value v::obj) (with-access::exprstmt-value o (location) (set! location v)))

;; binop
(define-inline (make-binop::binop location1252::obj id1253::ident left1254::expr right1255::expr) (instantiate::binop (location location1252) (id id1253) (left left1254) (right right1255)))
(define-inline (binop?::bool obj::obj) ((@ isa? __object) obj (@ binop ast_node)))
(define (binop-nil::binop) (class-nil (@ binop ast_node)))
(define-inline (binop-right::expr o::binop) (with-access::binop o (right) right))
(define-inline (binop-right-set! o::binop v::expr) (with-access::binop o (right) (set! right v)))
(define-inline (binop-left::expr o::binop) (with-access::binop o (left) left))
(define-inline (binop-left-set! o::binop v::expr) (with-access::binop o (left) (set! left v)))
(define-inline (binop-id::ident o::binop) (with-access::binop o (id) id))
(define-inline (binop-id-set! o::binop v::ident) (with-access::binop o (id) (set! id v)))
(define-inline (binop-location::obj o::binop) (with-access::binop o (location) location))
(define-inline (binop-location-set! o::binop v::obj) (with-access::binop o (location) (set! location v)))

;; funcall
(define-inline (make-funcall::funcall location1248::obj fun1249::obj actuals1250::obj) (instantiate::funcall (location location1248) (fun fun1249) (actuals actuals1250)))
(define-inline (funcall?::bool obj::obj) ((@ isa? __object) obj (@ funcall ast_node)))
(define (funcall-nil::funcall) (class-nil (@ funcall ast_node)))
(define-inline (funcall-actuals::obj o::funcall) (with-access::funcall o (actuals) actuals))
(define-inline (funcall-actuals-set! o::funcall v::obj) (with-access::funcall o (actuals) (set! actuals v)))
(define-inline (funcall-fun::obj o::funcall) (with-access::funcall o (fun) fun))
(define-inline (funcall-fun-set! o::funcall v::obj) (with-access::funcall o (fun) (set! fun v)))
(define-inline (funcall-location::obj o::funcall) (with-access::funcall o (location) location))
(define-inline (funcall-location-set! o::funcall v::obj) (with-access::funcall o (location) (set! location v)))

;; aref
(define-inline (make-aref::aref location1244::obj array1245::expr offset1246::expr) (instantiate::aref (location location1244) (array array1245) (offset offset1246)))
(define-inline (aref?::bool obj::obj) ((@ isa? __object) obj (@ aref ast_node)))
(define (aref-nil::aref) (class-nil (@ aref ast_node)))
(define-inline (aref-offset::expr o::aref) (with-access::aref o (offset) offset))
(define-inline (aref-offset-set! o::aref v::expr) (with-access::aref o (offset) (set! offset v)))
(define-inline (aref-array::expr o::aref) (with-access::aref o (array) array))
(define-inline (aref-array-set! o::aref v::expr) (with-access::aref o (array) (set! array v)))
(define-inline (aref-location::obj o::aref) (with-access::aref o (location) location))
(define-inline (aref-location-set! o::aref v::obj) (with-access::aref o (location) (set! location v)))

;; rref
(define-inline (make-rref::rref location1239::obj record1240::expr record-type1241::obj field-id1242::ident) (instantiate::rref (location location1239) (record record1240) (record-type record-type1241) (field-id field-id1242)))
(define-inline (rref?::bool obj::obj) ((@ isa? __object) obj (@ rref ast_node)))
(define (rref-nil::rref) (class-nil (@ rref ast_node)))
(define-inline (rref-field-id::ident o::rref) (with-access::rref o (field-id) field-id))
(define-inline (rref-field-id-set! o::rref v::ident) (with-access::rref o (field-id) (set! field-id v)))
(define-inline (rref-record-type::obj o::rref) (with-access::rref o (record-type) record-type))
(define-inline (rref-record-type-set! o::rref v::obj) (with-access::rref o (record-type) (set! record-type v)))
(define-inline (rref-record::expr o::rref) (with-access::rref o (record) record))
(define-inline (rref-record-set! o::rref v::expr) (with-access::rref o (record) (set! record v)))
(define-inline (rref-location::obj o::rref) (with-access::rref o (location) location))
(define-inline (rref-location-set! o::rref v::obj) (with-access::rref o (location) (set! location v)))

;; const
(define-inline (make-const::const location1235::obj value1236::obj type1237::obj) (instantiate::const (location location1235) (value value1236) (type type1237)))
(define-inline (const?::bool obj::obj) ((@ isa? __object) obj (@ const ast_node)))
(define (const-nil::const) (class-nil (@ const ast_node)))
(define-inline (const-type::obj o::const) (with-access::const o (type) type))
(define-inline (const-type-set! o::const v::obj) (with-access::const o (type) (set! type v)))
(define-inline (const-value::obj o::const) (with-access::const o (value) value))
(define-inline (const-value-set! o::const v::obj) (with-access::const o (value) (set! value v)))
(define-inline (const-location::obj o::const) (with-access::const o (location) location))
(define-inline (const-location-set! o::const v::obj) (with-access::const o (location) (set! location v)))

;; varref
(define-inline (make-varref::varref location1231::obj depth1232::int vardecl1233::obj) (instantiate::varref (location location1231) (depth depth1232) (vardecl vardecl1233)))
(define-inline (varref?::bool obj::obj) ((@ isa? __object) obj (@ varref ast_node)))
(define (varref-nil::varref) (class-nil (@ varref ast_node)))
(define-inline (varref-vardecl::obj o::varref) (with-access::varref o (vardecl) vardecl))
(define-inline (varref-vardecl-set! o::varref v::obj) (with-access::varref o (vardecl) (set! vardecl v)))
(define-inline (varref-depth::int o::varref) (with-access::varref o (depth) depth))
(define-inline (varref-depth-set! o::varref v::int) (with-access::varref o (depth) (set! depth v)))
(define-inline (varref-location::obj o::varref) (with-access::varref o (location) location))
(define-inline (varref-location-set! o::varref v::obj) (with-access::varref o (location) (set! location v)))
))
