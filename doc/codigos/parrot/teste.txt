
.HLL "cardinal"

.namespace []
.sub "_block1000" :load :main :anon :subid("10_1471304588.19528")
    .param pmc param_1002 :optional :named("!BLOCK")
    .param int has_param_1002 :opt_flag
.annotate 'file', "teste.rb"
.annotate 'line', 0
    .const 'Sub' $P1005 = "11_1471304588.19528" 
    capture_lex $P1005
.annotate 'line', 1
    if has_param_1002, optparam_13
    new $P100, "Undef"
    set param_1002, $P100
  optparam_13:
    .lex "!BLOCK", param_1002
    get_hll_global $P101, "puts"
    unless_null $P101, vivify_14
    new $P101, "Undef"
  vivify_14:
    new $P102, "CardinalString"
    assign $P102, "Digite a venda:"
    $P101($P102)
.annotate 'line', 2
    get_hll_global $P103, "gets"
    unless_null $P103, vivify_15
    new $P103, "Undef"
  vivify_15:
    $P104 = $P103."chomp"()
    $P105 = $P104."to_f"()
    set $P1003, $P105
    .lex "venda", $P1003
.annotate 'line', 4
    get_hll_global $P106, "puts"
    unless_null $P106, vivify_16
    new $P106, "Undef"
  vivify_16:
    find_lex $P107, "venda"
    unless_null $P107, vivify_17
    new $P107, "Undef"
  vivify_17:
    $P108 = $P106($P107)
.annotate 'line', 1
    .return ($P108)
.end


.HLL "cardinal"

.namespace []
.sub "" :load :init :subid("post12") :outer("10_1471304588.19528")
.annotate 'file', "teste.rb"
.annotate 'line', 0
    .const 'Sub' $P1001 = "10_1471304588.19528" 
    .local pmc block
    set block, $P1001
.end


.HLL "parrot"

.namespace []
.sub "_block1004" :init :load :anon :subid("11_1471304588.19528") :outer("10_1471304588.19528")
.annotate 'file', "teste.rb"
.annotate 'line', 0
$P0 = compreg "cardinal"
unless null $P0 goto have_cardinal
load_bytecode "cardinal.pbc"
have_cardinal:
    .return ()
.end

