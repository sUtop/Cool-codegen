
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <string>

long __offset = 0;
#define INFO_IN
//#define INFO_IN { std::string tmp_in; for(long i_in =0; i_in < __offset; ++i_in) tmp_in += " ";\
    fprintf(stderr, "%s %s: in\n", tmp_in.c_str(), __PRETTY_FUNCTION__); ++__offset; }

//#define INFO_IN_AS
#define INFO_IN_AS { std::string tmp_in; for(long i_in =0; i_in < __offset; ++i_in) tmp_in += " ";\
    s << "\n # " << tmp_in << " " << __PRETTY_FUNCTION__ << " in\n"; }

#define INFO_OUT
//#define INFO_OUT { --__offset; std::string tmp_out; for(long i_out =0; i_out < __offset; ++i_out) tmp_out += " ";\
    fprintf(stderr, "%s %s: out\n", tmp_out.c_str(), __PRETTY_FUNCTION__);}

//#define INFO_OUT_AS
#define INFO_OUT_AS { std::string tmp_out; for(long i_out =0; i_out < __offset; ++i_out) tmp_out += " ";\
    s << "\n # " << tmp_out << " " << __PRETTY_FUNCTION__ << " out\n"; }


extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
arg,
arg2,
Bool,
concat,
cool_abort,
copy,
Int,
in_int,
in_string,
IO,
length,
Main,
main_meth,
No_class,
No_type,
Object,
out_int,
out_string,
prim_slot,
self,
SELF_TYPE,
Str,
str_field,
substr,
type_name,
val;
//
// Initializing the predefined symbols.
//

static void initialize_constants(void) {
    INFO_IN;
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
    INFO_OUT;
}

static char *gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char *gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

CgenClassTableP codegen_classtable;

void program_class::cgen(ostream &s) {
    INFO_IN_AS;
    // spim wants comments to start with '#'
    s << "# start of generated code\n";

    initialize_constants();
    codegen_classtable = new CgenClassTable(classes, s);
    codegen_classtable->code();

    s << "\n# end of generated code\n";
    INFO_OUT_AS;
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(const char *dest_reg, const int offset, const char *source_reg, ostream& s) {
    INFO_IN_AS;
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
        << endl;
    INFO_OUT_AS;
}

static void emit_store(const char *source_reg, const int offset, const char *dest_reg, ostream& s) {
    INFO_IN_AS;
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
        << endl;
    INFO_OUT_AS;
}

static void emit_load_imm(const char *dest_reg, const int val, ostream& s) {
    INFO_IN_AS;
    s << LI << dest_reg << " " << val << endl;
    INFO_OUT_AS;
}

static void emit_load_address(const char *dest_reg, const char *address, ostream& s) {
    INFO_IN_AS;
    s << LA << dest_reg << " " << address << endl;
    INFO_OUT_AS;
}

static void emit_partial_load_address(const char *dest_reg, ostream& s) {
    INFO_IN_AS;
    s << LA << dest_reg << " ";
    INFO_OUT_AS;
}

static void emit_load_bool(const char *dest, const BoolConst& b, ostream& s) {
    INFO_IN_AS;
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_load_string(const char *dest, StringEntry *str, ostream& s) {
    INFO_IN_AS;
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s) {
    INFO_IN_AS;
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s) {
    INFO_IN_AS;
    s << MOVE << dest_reg << " " << source_reg << endl;
    INFO_OUT_AS;
}

static void emit_neg(char *dest, char *src1, ostream& s) {
    INFO_IN_AS;
    s << NEG << dest << " " << src1 << endl;
    INFO_OUT_AS;
}

static void emit_add(char *dest, char *src1, char *src2, ostream& s) {
    INFO_IN_AS;
    s << ADD << dest << " " << src1 << " " << src2 << endl;
    INFO_OUT_AS;
}

static void emit_addu(char *dest, char *src1, char *src2, ostream& s) {
    INFO_IN_AS;
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
    INFO_OUT_AS;
}

static void emit_addiu(char *dest, char *src1, int imm, ostream& s) {
    INFO_IN_AS;
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
    INFO_OUT_AS;
}

static void emit_div(char *dest, char *src1, char *src2, ostream& s) {
    INFO_IN_AS;
    s << DIV << dest << " " << src1 << " " << src2 << endl;
    INFO_OUT_AS;
}

static void emit_mul(char *dest, char *src1, char *src2, ostream& s) {
    INFO_IN_AS;
    s << MUL << dest << " " << src1 << " " << src2 << endl;
    INFO_OUT_AS;
}

static void emit_sub(char *dest, char *src1, char *src2, ostream& s) {
    INFO_IN_AS;
    s << SUB << dest << " " << src1 << " " << src2 << endl;
    INFO_OUT_AS;
}

static void emit_sll(char *dest, char *src1, int num, ostream& s) {
    INFO_IN_AS;
    s << SLL << dest << " " << src1 << " " << num << endl;
    INFO_OUT_AS;
}

static void emit_jalr(char *dest, ostream& s) {
    INFO_IN_AS;
    s << JALR << dest << endl;
    INFO_OUT_AS;
}

static void emit_jal(const char *address, ostream &s) {
    INFO_IN_AS;
    s << JAL << address << endl;
    INFO_OUT_AS;
}

static void emit_return(ostream& s) {
    INFO_IN_AS;
    s << RET << endl;
    INFO_OUT_AS;
}

static void emit_gc_assign(ostream& s) {
    INFO_IN_AS;
    s << JAL << "_GenGC_Assign" << endl;
    INFO_OUT_AS;
}

static void emit_disptable_ref(Symbol sym, ostream& s) {
    INFO_IN_AS;
    s << sym << DISPTAB_SUFFIX;
    INFO_OUT_AS;
}

static void emit_init_ref(Symbol sym, ostream& s) {
    INFO_IN_AS;
    s << sym << CLASSINIT_SUFFIX;
    INFO_OUT_AS;
}

static void emit_label_ref(int l, ostream &s) {
    INFO_IN_AS;
    s << "label" << l;
    INFO_OUT_AS;
}

static void emit_protobj_ref(Symbol sym, ostream& s) {
    INFO_IN_AS;
    s << sym << PROTOBJ_SUFFIX;
    INFO_OUT_AS;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s) {
    INFO_IN_AS;
    s << classname << METHOD_SEP << methodname;
    INFO_OUT_AS;
}

static void emit_label_def(int l, ostream &s) {
    INFO_IN_AS;
    emit_label_ref(l, s);
    s << ":" << endl;
    INFO_OUT_AS;
}

static void emit_beqz(char *source, int label, ostream &s) {
    INFO_IN_AS;
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s) {
    INFO_IN_AS;
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s) {
    INFO_IN_AS;
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s) {
    INFO_IN_AS;
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s) {
    INFO_IN_AS;
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_blti(char *src1, int imm, int label, ostream &s) {
    INFO_IN_AS;
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s) {
    INFO_IN_AS;
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
    INFO_OUT_AS;
}

static void emit_branch(int l, ostream& s) {
    INFO_IN_AS;
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
    INFO_OUT_AS;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//

static void emit_push(char *reg, ostream& s) {
    INFO_IN_AS;
    emit_store(reg, 0, SP, s);
    emit_addiu(SP, SP, -4, s);
    INFO_OUT_AS;
}

#define HEADER_SIZE 3

// Push entry function header
//Object_init:
//        addiu   $sp $sp -12 # push 1
//        sw      $fp 12($sp) # push fp to stack
//        sw      $s0 8($sp)  # push SELF to stack
//        sw      $ra 4($sp)  # push $ra to stack (return address!)
//        addiu   $fp $sp 4   # increese $fp
//        move    $s0 $a0     # save previous return value to s0
static void emit_push_header(ostream& s, int size)
{
    emit_addiu(SP, SP, -size * WORD_SIZE, s);
    // Save frame pointer
    emit_store(FP, size, SP, s);
    // save self pointer
    emit_store(SELF, size - 1, SP, s);
    // save return address
    emit_store(RA, size - 2, SP, s);
    // save register $s1

    emit_addiu(FP, SP, WORD_SIZE, s);
}

static void emit_push_header_class(ostream& s)
{
    emit_push_header(s, HEADER_SIZE + 1);
    // save register $s1
    emit_store("$s1", HEADER_SIZE - 3, SP, s);
    // Change self to acc
    emit_move(SELF, ACC, s);
};

//        move    $a0 $s0     # pop previous return value to a0
//        lw      $fp 12($sp) # pop previous frame pointer
//        lw      $s0 8($sp)  # pop previous SELF
//        lw      $ra 4($sp)  # pop previous return address
//        addiu   $sp $sp 12  # decrease sp

//        jr      $ra
static void emit_pop_header(ostream& s, int size)
{
    // restore previous fp
    emit_load(FP, size, SP, s);
    // restore previous SELF
    emit_load(SELF, size - 1, SP, s);
    // restore previous return address
    emit_load(RA, size - 2, SP, s);
    // decreese sp
    emit_addiu(SP, SP, size * WORD_SIZE, s);
}

static void emit_pop_header_class(ostream& s)
{
    // Change acc to self
    emit_move(ACC, SELF, s);
    // restore register $s1
    emit_load("$s1", 0, SP, s);
    emit_pop_header(s, HEADER_SIZE + 1);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//

static void emit_fetch_int(char *dest, char *source, ostream& s) {
    INFO_IN_AS;
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
    INFO_OUT_AS;
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//

static void emit_store_int(char *source, char *dest, ostream& s) {
    INFO_IN_AS;
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
    INFO_OUT_AS;
}

/// Begin here

static void emit_test_collector(ostream &s) {
    INFO_IN_AS;
    emit_push(ACC, s);
    emit_move(ACC, SP, s); // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
    INFO_OUT_AS;
}

static void emit_gc_check(char *source, ostream &s) {
    INFO_IN_AS;
    if (source != (char*) A1) emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
    INFO_OUT_AS;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//

void StringEntry::code_ref(ostream& s) {
    INFO_IN_AS;
    s << STRCONST_PREFIX << index;
    INFO_OUT_AS;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag) {
    INFO_IN_AS;
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL // label
        << WORD << stringclasstag << endl // tag
        << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
        << WORD;


    /***** Add dispatch information for class String ******/
    s << "String" << DISPTAB_SUFFIX;
    s << endl; // dispatch table
    s << WORD;
    lensym->code_ref(s);
    s << endl; // string length
    emit_string_constant(s, str); // ascii string
    s << ALIGN; // align to word
    INFO_OUT_AS;
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//

void StrTable::code_string_table(ostream& s, int stringclasstag) {
    INFO_IN_AS;
    for (List<StringEntry> *l = tbl; l; l = l->tl())
    {
        l->hd()->code_def(s, stringclasstag);
    }
    INFO_OUT_AS;
}

//
// Ints
//

void IntEntry::code_ref(ostream &s) {
    INFO_IN_AS;
    s << INTCONST_PREFIX << index;
    INFO_OUT_AS;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag) {
    INFO_IN_AS;
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL // label
        << WORD << intclasstag << endl // class tag
        << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
        << WORD;

    /***** Add dispatch information for class Int ******/
    s << "Int" << DISPTAB_SUFFIX;
    s << endl; // dispatch table
    s << WORD << str << endl; // integer value
    INFO_OUT_AS;
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//

void IntTable::code_string_table(ostream &s, int intclasstag) {
    INFO_IN_AS;
    for (List<IntEntry> *l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
    INFO_OUT_AS;
}


//
// Bools
//

BoolConst::BoolConst(int i) : val(i) {
    assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const {
    INFO_IN_AS;
    s << BOOLCONST_PREFIX << val;
    INFO_OUT_AS;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag) {
    INFO_IN_AS;
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL // label
        << WORD << boolclasstag << endl // class tag
        << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
        << WORD;

    /***** Add dispatch information for class Bool ******/
    s << "Bool" << DISPTAB_SUFFIX;
    s << endl; // dispatch table
    s << WORD << val << endl; // value (0 or 1)
    INFO_OUT_AS;
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data() {
    INFO_IN;
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL
        << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL
        << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL
        << WORD << stringclasstag << endl;
    INFO_OUT;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text() {
    INFO_IN;
    str << GLOBAL << HEAP_START << endl
        << HEAP_START << LABEL
        << WORD << 0 << endl
        << "\t.text" << endl
        << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
    INFO_OUT;
}

void CgenClassTable::code_bools(int boolclasstag) {
    INFO_IN;
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
    INFO_OUT;
}

void CgenClassTable::code_select_gc() {
    INFO_IN;
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
    INFO_OUT;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants() {
    INFO_IN;
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
    INFO_OUT;
}

void CgenClassTable::code_protObjs() {
    // Add -1 eye catcher

    str << "# coding prototype Objects. \n";
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        CgenNode* tmp = l->hd();
        tmp->code_prot(str);
    };
}

void CgenClassTable::code_dispTabs() {
    str << "# coding disp Tables. \n";
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        CgenNode* tmp = l->hd();
        tmp->code_ref(str);
        str << DISPTAB_SUFFIX << LABEL;
        tmp->code_disp(str);
    };
}

void method_class::code(ostream& s)
{
    // first -> attributes
    //
    for( int i = formals->first(); formals->more(i) ; i = formals->next(i))
    {
        formal_class* f= dynamic_cast<formal_class*>(formals->nth(i));
        if (f)
        {
            s << " # formal " << f->name->get_string() << endl;
        } else
        {
            s << " # unknown formal" << endl;
        }
    }
    expr->code(s);

    //s <<
}


void CgenClassTable::code_methods() {
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        CgenNode* tmp = l->hd();
        if (tmp->basic())
        {
            continue;
        }
        for(int i = tmp->features->first();
                tmp->features->more(i);
                i = tmp->features->next(i))
        {
            Feature_class * f = tmp->features->nth(i);
            method_class * m = dynamic_cast<method_class*>(f);
            if (m)
            {
                tmp->code_ref(str);
                str << METHOD_SEP;
                str << m->name << LABEL;

                emit_push_header(str, HEADER_SIZE);

                m->code(str);

                emit_pop_header(str, HEADER_SIZE);

                emit_return(str);
            }
        }
    }
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL), str(s) {
    INFO_IN;
    // Filled automatically
    stringclasstag = 0;
    intclasstag = 0;
    boolclasstag = 0;
    currclasstag = 0;

    enterscope();
    if (cgen_debug) cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();

//    exitscope();
    INFO_OUT;
}

void CgenClassTable::install_basic_classes() {
    INFO_IN;
    // The tree package uses these globals to annotate the classes built below.
    //curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    addid(No_class,
          new CgenNode(class_(No_class, No_class, nil_Features(), filename),
                       Basic, this));
    addid(SELF_TYPE,
          new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename),
                       Basic, this));
    addid(prim_slot,
          new CgenNode(class_(prim_slot, No_class, nil_Features(), filename),
                       Basic, this));

    // 
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class name
    //        copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(
                  new CgenNode(
                               class_(Object,
                                      No_class,
                                      append_Features(
                                                      append_Features(
                                                                      single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                                                      single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                                                      single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                                      filename),
                               Basic, this));

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(
                  new CgenNode(
                               class_(IO,
                                      Object,
                                      append_Features(
                                                      append_Features(
                                                                      append_Features(
                                                                                      single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                                                                             SELF_TYPE, no_expr())),
                                                                                      single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                                                                             SELF_TYPE, no_expr()))),
                                                                      single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                                                      single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
                                      filename),
                               Basic, this));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    install_class(
                  new CgenNode(
                               class_(Int,
                                      Object,
                                      single_Features(attr(val, prim_slot, no_expr())),
                                      filename),
                               Basic, this));

    //
    // Bool also has only the "val" slot.
    //
    install_class(
                  new CgenNode(
                               class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename),
                               Basic, this));

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //       
    install_class(
                  new CgenNode(
                               class_(Str,
                                      Object,
                                      append_Features(
                                                      append_Features(
                                                                      append_Features(
                                                                                      append_Features(
                                                                                                      single_Features(attr(val, Int, no_expr())),
                                                                                                      single_Features(attr(str_field, prim_slot, no_expr()))),
                                                                                      single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                                                      single_Features(method(concat,
                                                                                             single_Formals(formal(arg, Str)),
                                                                                             Str,
                                                                                             no_expr()))),
                                                      single_Features(method(substr,
                                                                             append_Formals(single_Formals(formal(arg, Int)),
                                                                                            single_Formals(formal(arg2, Int))),
                                                                             Str,
                                                                             no_expr()))),
                                      filename),
                               Basic, this));

    INFO_OUT;
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//

void CgenClassTable::install_class(CgenNodeP nd) {
    INFO_IN;
    Symbol name = nd->get_name();

    CgenNodeP cl= probe(name);
    if (cl) {
        nd->set_id(cl->get_id());
        INFO_OUT;
        return;
    }
    
//    std::cout << " For " << nd->get_name()->get_string() << " \n";
//    std::cout << " geted id : " <<  currclasstag;
    if (nd->get_name() == Int)
    {
        intclasstag = currclasstag;
    } else if (nd->get_name() == Bool)
    {
        boolclasstag = currclasstag;
    } else if (nd->get_name() == Str)
    {
        stringclasstag = currclasstag;
    }
    nd->set_id(currclasstag);
    currclasstag++;
    
    // The class name is legal, so add it to the list of classes
    // and the symbol table.
    nds = new List<CgenNode>(nd, nds);
    addid(name, nd);
    INFO_OUT;
}

void CgenClassTable::install_classes(Classes cs) {
    INFO_IN;
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
        install_class(new CgenNode(cs->nth(i), NotBasic, this));
    INFO_OUT;
}

//
// CgenClassTable::build_inheritance_tree
//

void CgenClassTable::build_inheritance_tree() {
    INFO_IN;
    for (List<CgenNode> *l = nds; l; l = l->tl())
        set_relations(l->hd());
    INFO_OUT;
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//

void CgenClassTable::set_relations(CgenNodeP nd) {
    INFO_IN;
    CgenNode *parent_node = probe(nd->get_parent());
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
    INFO_OUT;
}

void CgenNode::add_child(CgenNodeP n) {
    INFO_IN;
    children = new List<CgenNode>(n, children);
    INFO_OUT;
}

void CgenNode::set_parentnd(CgenNodeP p) {
    INFO_IN;
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
    INFO_OUT;
}

void CgenNode::code_disp(ostream& s)
{
    if (parentnd != NULL && get_name() != Object)
        parentnd->code_disp(s);
    
    for(int i = features->first();
            features->more(i);
            i = features->next(i))
    {
        Feature_class * f = features->nth(i);
        method_class * m = dynamic_cast<method_class*>(f);
        if (m)
        {
            s << WORD << get_name() << METHOD_SEP << m->name << "\n";
        }

    }
    // str << WORD << get_name() << CLASSINIT_SUFFIX <<"\n";
};

void CgenNode::code_ref(ostream& s)
{
    s << this->get_name();
}

int CgenNode::get_attr_num()
{
    int attrnum = 0;
    if (parentnd != NULL && get_name() != Object)
        attrnum = parentnd->get_attr_num();

    for(int i = features->first();
            features->more(i);
            i = features->next(i))
    {
        Feature_class * f = features->nth(i);
        attr_class * a = dynamic_cast<attr_class*>(f);
        if (a) attrnum++;
    }
    return attrnum;
};

void CgenNode::code_attr_prot(ostream& s)
{
    INFO_IN_AS;
    if (parentnd != NULL && get_name() != Object)
        parentnd->code_attr_prot(s);

    for(int i = features->first();
            features->more(i);
            i = features->next(i))
    {
        Feature_class * f = features->nth(i);
        attr_class * a = dynamic_cast<attr_class*>(f);
        if (a)
        {
            s << WORD;
            // here need get default value for all classes
            if (a->type_decl == Int)
            {
                IntEntry * zero = inttable.lookup_string("0");                    
                zero->code_ref(s);                
            } else if (a->type_decl == Str)
            {
                StringEntry * zero = stringtable.lookup_string("");
                zero->code_ref(s);

            } else if (a->type_decl == Bool)
            {
                s << "0" << "   # Bool default";
            } else
            {
                s << "0";
                s << " # " << a->type_decl;
            }
            s << endl;
        }
    }
    INFO_OUT_AS;
}

void CgenNode::emit_init(ostream& s)
{
    emit_push_header(s, HEADER_SIZE);
    if (parentnd != NULL && get_name() != Object)
    {
        std::string str = parentnd->get_name()->get_string();
        str += CLASSINIT_SUFFIX;
        emit_jal(str.c_str(), s);
    }
    emit_pop_header(s, HEADER_SIZE);
    emit_return(s);
}

void CgenNode::code_prot(ostream& s)
{
    INFO_IN_AS;
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << PROTOBJ_SUFFIX << LABEL;
    s << WORD << this->get_id() << endl; // class tag

    s << WORD << DEFAULT_OBJFIELDS + get_attr_num() << endl; // FIXME !!!

    /***** Add dispatch information for class Int ******/
    s << WORD;
    code_ref(s);
    s << DISPTAB_SUFFIX << endl; // dispatch table
    
    code_attr_prot(s);
    // <- here need put inforormation for all 
    
    INFO_OUT_AS;
}

void CgenClassTable::code() {
    INFO_IN;
    str << "# coding global data" << endl;
    code_global_data();

    str << "# choosing gc" << endl;
    code_select_gc();

    str << "# coding constants" << endl;
    code_constants();

    //                 Add your code to emit
    //                   - prototype objects
    //                   - class_nameTab
    //                   - dispatch tables
    //

    str << "# coding class name Table. \n";
    str << CLASSNAMETAB << LABEL;    
    for (int i = 0; i < currclasstag; ++i)
    {
        // Looking for i id-s
        // std::cout << " looking for " << i << " :";
        List<CgenNode> *l = nds;
        for (;  l && (l->hd()->get_id() != i);
                l = l->tl())
        {
        }
        // We must find this node.
        assert(l);
        // std::cout << " found " << l->hd()->get_name()->get_string() << " \n";
        // Looking for string
        StringEntry* se = stringtable.lookup_string(l->hd()->get_name()->get_string());
        str << WORD; 
        se->code_ref(str);
        str << "\n";        
    }
    
    // str << "# coding class object Table. \n";
    str << CLASSOBJTAB << LABEL;
    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        CgenNode* tmp = l->hd();
        str << WORD;
        tmp->code_ref(str);
        str << PROTOBJ_SUFFIX <<"\n";
        
        str << WORD;
        tmp->code_ref(str);
        str << CLASSINIT_SUFFIX <<"\n";
    };
 
    code_dispTabs();
    
    code_protObjs();
    
    str << "# coding global text" << endl;
    code_global_text();

    for (List<CgenNode> *l = nds; l; l = l->tl())
    {
        // str << "  # Initialization code \n";
        CgenNode* tmp = l->hd();
        tmp->code_ref(str);
        str << CLASSINIT_SUFFIX << LABEL;
        tmp->emit_init(str);
    };
    
    code_methods();

    //                 Add your code to emit
    //                   - object initializer
    //                   - the class methods
    //                   - etc...

    INFO_OUT;
}

CgenNodeP CgenClassTable::root() {
    INFO_IN;
    return probe(Object);
//    INFO_OUT;
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
    class__class((const class__class &) *nd),
    parentnd(NULL),
    children(NULL),
    basic_status(bstatus) {
    INFO_IN;
    stringtable.add_string(name->get_string()); // Add class name to string table
    INFO_OUT;
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void static_dispatch_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

static int create_label()
{
    static int label = 0;
    return label++;
}

void dispatch_class::code(ostream &s) {
    INFO_IN_AS;
    Symbol type = expr->get_type();
    if ( type == SELF_TYPE)
    {
//            type = global_node->get_name();
    }

    for ( int i = actual->first(); actual->more( i); i = actual->next( i))
    {
            actual->nth( i)->code( s);
            emit_push( ACC, s);
    }
    expr->code( s);

    int jump_label = create_label();
    emit_bne( ACC, ZERO, jump_label, s);
//    emit_load_string( ACC, stringtable.lookup_string( global_node->filename->get_string()), s);
    emit_load_string( ACC, stringtable.lookup_string(""), s);
    emit_load_imm( T1, line_number, s);
    emit_jal( "_dispatch_abort", s);

    CgenNodeP node = codegen_classtable->lookup( type);
//    int offset = ( ( int)( node->lookup_method_offset( name))) - DEFAULT_METHOD_OFFSET;

    emit_label_def( jump_label, s);
    emit_load( T1, DISPTABLE_OFFSET, ACC, s);
    emit_load( T1, offset, T1, s);
    emit_jalr( T1, s);

    expr_is_const = 1;
    if ( cgen_debug)
            cout  << "Dispatch " << name << " is const ? " << bool(expr_is_const) << endl;

    INFO_OUT_AS;
}

void cond_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void loop_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void typcase_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void block_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void let_class::code(ostream &s) {
    INFO_IN_AS;
    init->code( s);
    s << " # let code \n";
//	int offset = alloc_temp();
    emit_store( ACC, 0, FP, s);
    body->code( s);
    INFO_OUT_AS;
}

void plus_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void sub_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void mul_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void divide_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void neg_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void lt_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void eq_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void leq_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void comp_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void int_const_class::code(ostream& s) {
    INFO_IN_AS;
    //
    // Need to be sure we have an IntEntry *, not an arbitrary Symbol
    //
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
    INFO_OUT_AS;
}

void string_const_class::code(ostream& s) {
    INFO_IN_AS;
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
    INFO_OUT_AS;
}

void bool_const_class::code(ostream& s) {
    INFO_IN_AS;
    emit_load_bool(ACC, BoolConst(val), s);
    INFO_OUT_AS;
}

void new__class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void isvoid_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void no_expr_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}

void object_class::code(ostream &s) {
    INFO_IN_AS;
    INFO_OUT_AS;
}


