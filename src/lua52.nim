#
#* $Id: lua.h,v 1.283 2012/04/20 13:18:26 roberto Exp $
#* Lua - A Scripting Language
#* Lua.org, PUC-Rio, Brazil (http://www.lua.org)
#* See Copyright Notice at the end of this file
#
const 
  LUA_VERSION_MAJOR* = "5"
  LUA_VERSION_MINOR* = "2"
  LUA_VERSION_NUM* = 502
  LUA_VERSION_RELEASE* = "1"
  LUA_VERSION* = "Lua " & LUA_VERSION_MAJOR & "." & LUA_VERSION_MINOR
  LUA_RELEASE = LUA_VERSION &"."& LUA_VERSION_RELEASE
##define LUA_COPYRIGHT	LUA_RELEASE "  Copyright (C) 1994-2012 Lua.org, PUC-Rio"
##define LUA_AUTHORS	"R. Ierusalimschy, L. H. de Figueiredo, W. Celes"

when defined(useLuaJIT): 
  {.warning: "Lua JIT does not support Lua 5.2 at this time."}

when not defined(useLuaJIT):
  when defined(MACOSX):
    const
      LIB_NAME* = "liblua5.2.dylib"
  elif defined(UNIX):
    const
      LIB_NAME* = "liblua(5.2.so|.so.5.2)"
  else:
    const 
      LIB_NAME* = "lua5.2.dll"
else:
  when defined(MACOSX):
    const
      LIB_NAME* = "libluajit.dylib"
  elif defined(UNIX):
    const
      LIB_NAME* = "libluajit.so"
  else:
    const
      LIB_NAME* = "luajit.dll" 

# mark for precompiled code ('<esc>Lua') 
const 
  LUA_SIGNATURE* = "\x1BLua"
# option for multiple returns in 'lua_pcall' and 'lua_call' 
const 
  LUA_MULTRET* = (- 1)
#
#* pseudo-indices
#
#@@ LUAI_MAXSTACK limits the size of the Lua stack.
#* CHANGE it if you need a different limit. This limit is arbitrary;
#* its only purpose is to stop Lua to consume unlimited stack
#* space (and to reserve some numbers for pseudo-indices).
#
when sizeof(int) >= 4: #LUAI_BITSINT >= 32: 
  const 
    LUAI_MAXSTACK* = 1000000
else: 
  const 
    LUAI_MAXSTACK* = 15000
# reserve some space for error handling 
const 
  LUAI_FIRSTPSEUDOIDX* = (- LUAI_MAXSTACK - 1000)

const 
  LUA_REGISTRYINDEX* = LUAI_FIRSTPSEUDOIDX
template lua_upvalueindex*(i: expr): expr = 
  (LUA_REGISTRYINDEX - (i))

# thread status
type TThreadStatus* {.size:sizeof(cint).}= enum
  Thread_OK = 0, Thread_Yield, Thread_ErrRun, Thread_ErrSyntax,
  Thread_ErrMem, Thread_ErrGCMM, Thread_ErrErr 
discard """ const 
  LUA_OK* = 0
  LUA_YIELD* = 1
  LUA_ERRRUN* = 2
  LUA_ERRSYNTAX* = 3
  LUA_ERRMEM* = 4
  LUA_ERRGCMM* = 5
  LUA_ERRERR* = 6 """

type 
  PState* = distinct pointer
  TCFunction* = proc (L: PState): cint{.cdecl.}
#
#* functions that read/write blocks when loading/dumping Lua chunks
#
type 
  TReader* = proc (L: PState; ud: pointer; sz: ptr csize): cstring
  TWriter* = proc (L: PState; p: pointer; sz: csize; ud: pointer): cint
#
#* prototype for memory-allocation functions
#
type 
  TAlloc* = proc (ud, `ptr`: pointer; osize, nsize: csize): pointer
#
#* basic types
#
const 
  LUA_TNONE* = (- 1)
  LUA_TNIL* = 0
  LUA_TBOOLEAN* = 1
  LUA_TLIGHTUSERDATA* = 2
  LUA_TNUMBER* = 3
  LUA_TSTRING* = 4
  LUA_TTABLE* = 5
  LUA_TFUNCTION* = 6
  LUA_TUSERDATA* = 7
  LUA_TTHREAD* = 8
  LUA_NUMTAGS* = 9
# minimum Lua stack available to a C function 
const 
  LUA_MINSTACK* = 20
# predefined values in the registry 
const 
  LUA_RIDX_MAINTHREAD* = 1
  LUA_RIDX_GLOBALS* = 2
  LUA_RIDX_LAST* = LUA_RIDX_GLOBALS
type 
  lua_Number* = cdouble 
    # type of numbers in Lua 
type 
  lua_Integer* = uint # ptrdiff_t \
    # type for integer functions

type 
  lua_Unsigned* = uint32
    # unsigned integer type 

{.push callconv: cdecl, dynlib: LIB_NAME .} # importc: "lua_$1"  was not allowed?
{.pragma: ilua, importc: "lua_$1".} # lua.h
{.pragma: iluaLIB, importc: "lua$1".} # lualib.h
{.pragma: iluaL, importc: "luaL_$1".} # lauxlib.h

proc newstate*(f: TAlloc; ud: pointer): PState {.ilua.}
proc close*(L: PState) {.ilua.}
proc newthread*(L: PState): PState {.ilua.}
proc atpanic*(L: PState; panicf: TCFunction): TCFunction {.ilua.}
proc version*(L: PState): ptr lua_Number {.ilua.}
#
#* basic stack manipulation
#
proc absindex*(L: PState; idx: cint): cint {.ilua.}
proc gettop*(L: PState): cint {.ilua.}
proc settop*(L: PState; idx: cint) {.ilua.}
proc pushvalue*(L: PState; idx: cint) {.ilua.}
proc remove*(L: PState; idx: cint) {.ilua.}
proc insert*(L: PState; idx: cint) {.ilua.}
proc replace*(L: PState; idx: cint) {.ilua.}
proc copy*(L: PState; fromidx: cint; toidx: cint) {.ilua.}
proc checkstack*(L: PState; sz: cint): cint {.ilua.}
proc xmove*(`from`: PState; to: PState; n: cint) {.ilua.}
#
#* access functions (stack -> C)
#
proc isnumber*(L: PState; idx: cint): cint {.ilua.}
proc isstring*(L: PState; idx: cint): cint {.ilua.}
proc iscfunction*(L: PState; idx: cint): cint {.ilua.}
proc isuserdata*(L: PState; idx: cint): cint {.ilua.}
proc luatype*(L: PState; idx: cint): cint {.importc: "lua_type".}
proc typename*(L: PState; tp: cint): cstring {.ilua.}
proc tonumberx*(L: PState; idx: cint; isnum: ptr cint): lua_Number {.ilua.}
proc tointegerx*(L: PState; idx: cint; isnum: ptr cint): lua_Integer {.ilua.}
proc tounsignedx*(L: PState; idx: cint; isnum: ptr cint): lua_Unsigned {.ilua.}
proc toboolean*(L: PState; idx: cint): cint {.ilua.}
proc tolstring*(L: PState; idx: cint; len: ptr csize): cstring {.ilua.}
proc rawlen*(L: PState; idx: cint): csize {.ilua.}
proc tocfunction*(L: PState; idx: cint): TCFunction {.ilua.}
proc touserdata*(L: PState; idx: cint): pointer {.ilua.}
proc tothread*(L: PState; idx: cint): PState {.ilua.}
proc topointer*(L: PState; idx: cint): pointer {.ilua.}
#
#* Comparison and arithmetic functions
#
const 
  LUA_OPADD* = 0            # ORDER TM 
  LUA_OPSUB* = 1
  LUA_OPMUL* = 2
  LUA_OPDIV* = 3
  LUA_OPMOD* = 4
  LUA_OPPOW* = 5
  LUA_OPUNM* = 6
proc arith*(L: PState; op: cint) {.ilua.}
const 
  LUA_OPEQ* = 0
  LUA_OPLT* = 1
  LUA_OPLE* = 2
proc rawequal*(L: PState; idx1: cint; idx2: cint): cint {.ilua.}
proc compare*(L: PState; idx1: cint; idx2: cint; op: cint): cint {.ilua.}
#
#* push functions (C -> stack)
#
proc pushnil*(L: PState) {.ilua.}
proc pushnumber*(L: PState; n: lua_Number) {.ilua.}
proc pushinteger*(L: PState; n: lua_Integer) {.ilua.}
proc pushunsigned*(L: PState; n: lua_Unsigned) {.ilua.}
proc pushlstring*(L: PState; s: cstring; len: csize): cstring {.ilua.}
proc pushstring*(L: PState; s: cstring): cstring {.ilua.}
proc pushvfstring*(L: PState; fmt: cstring): cstring {.varargs,ilua.}
proc pushfstring*(L: PState; fmt: cstring): cstring {.varargs,ilua.}
proc pushcclosure*(L: PState; fn: TCFunction; n: cint) {.ilua.}
proc pushboolean*(L: PState; b: cint) {.ilua.}
proc pushlightuserdata*(L: PState; p: pointer) {.ilua.}
proc pushthread*(L: PState): cint {.ilua.}
#
#* get functions (Lua -> stack)
#
proc getglobal*(L: PState; variable: cstring) {.ilua.}
proc gettable*(L: PState; idx: cint) {.ilua.}
proc getfield*(L: PState; idx: cint; k: cstring) {.ilua.}
proc rawget*(L: PState; idx: cint) {.ilua.}
proc rawgeti*(L: PState; idx: cint; n: cint) {.ilua.}
proc rawgetp*(L: PState; idx: cint; p: pointer) {.ilua.}
proc createtable*(L: PState; narr: cint; nrec: cint) {.ilua.}
proc newuserdata*(L: PState; sz: csize): pointer {.ilua.}
proc getmetatable*(L: PState; objindex: cint): cint {.ilua.}
proc getuservalue*(L: PState; idx: cint) {.ilua.}
#
#* set functions (stack -> Lua)
#
proc setglobal*(L: PState; variable: cstring) {.ilua.}
proc settable*(L: PState; idx: cint) {.ilua.}
proc setfield*(L: PState; idx: cint; k: cstring) {.ilua.}
proc rawset*(L: PState; idx: cint) {.ilua.}
proc rawseti*(L: PState; idx: cint; n: cint) {.ilua.}
proc rawsetp*(L: PState; idx: cint; p: pointer) {.ilua.}
proc setmetatable*(L: PState; objindex: cint): cint {.ilua.}
proc setuservalue*(L: PState; idx: cint) {.ilua.}
#
#* 'load' and 'call' functions (load and run Lua code)
#
proc callk*(L: PState; nargs: cint; nresults: cint; ctx: cint; 
                k: TCFunction) {.ilua.}
template lua_call*(L, n, r: expr): expr = 
  lua_callk(L, (n), (r), 0, nil)

proc getctx*(L: PState; ctx: ptr cint): cint {.ilua.}
proc pcallk*(L: PState; nargs: cint; nresults: cint; errfunc: cint; 
                 ctx: cint; k: TCFunction): cint {.ilua.}
proc pcall* (L: PState; 
             nargs, nresults, errFunc: cint): cint {.inline.} =
  L.pcallK nargs, nresults, errFunc, 0, nil

discard """ template lua_pcall*(L, n, r, f: expr): expr = 
  lua_pcallk(L, (n), (r), (f), 0, nil)
 """
proc load*(L: PState; reader: TReader; dt: pointer; 
               chunkname: cstring; mode: cstring): cint {.ilua.}
proc dump*(L: PState; writer: TWriter; data: pointer): cint {.ilua.}
#
#* coroutine functions
#
proc yieldk*(L: PState; nresults: cint; ctx: cint; k: TCFunction): cint {.ilua.}
template lua_yield*(L, n: expr): expr = 
  lua_yieldk(L, (n), 0, nil)

proc resume*(L: PState; fromState: PState; narg: cint): cint {.ilua.}
proc status*(L: PState): cint {.ilua.}
#
#* garbage-collection function and options
#
const 
  LUA_GCSTOP* = 0
  LUA_GCRESTART* = 1
  LUA_GCCOLLECT* = 2
  LUA_GCCOUNT* = 3
  LUA_GCCOUNTB* = 4
  LUA_GCSTEP* = 5
  LUA_GCSETPAUSE* = 6
  LUA_GCSETSTEPMUL* = 7
  LUA_GCSETMAJORINC* = 8
  LUA_GCISRUNNING* = 9
  LUA_GCGEN* = 10
  LUA_GCINC* = 11
proc gc*(L: PState; what: cint; data: cint): cint {.ilua.}
#
#* miscellaneous functions
#
proc error*(L: PState): cint {.ilua.}
proc next*(L: PState; idx: cint): cint {.ilua.}
proc concat*(L: PState; n: cint) {.ilua.}
proc len*(L: PState; idx: cint) {.ilua.}
proc getallocf*(L: PState; ud: var pointer): TAlloc {.ilua.}
proc setallocf*(L: PState; f: TAlloc; ud: pointer) {.ilua.}
#
#* ===============================================================
#* some useful macros
#* ===============================================================
#
template lua_tonumber*(L, i: expr): expr = 
  lua_tonumberx(L, i, nil)

template lua_tointeger*(L, i: expr): expr = 
  lua_tointegerx(L, i, nil)

template lua_tounsigned*(L, i: expr): expr = 
  lua_tounsignedx(L, i, nil)

template lua_pop*(L, n: expr): expr = 
  lua_settop(L, - (n) - 1)

template lua_newtable*(L: expr): expr = 
  lua_createtable(L, 0, 0)

##define lua_register(L,n,f) (lua_pushcfunction(L, (f)); lua_setglobal(L, (n)))
proc pushcfunction* (L: PState; fn: TCfunction) {.inline.} = L.pushCclosure(fn, 0)
discard """ pushcclosure*(L: PState; fn: TCFunction; n: cint) {.ilua.}
template lua_pushcfunction*(L, f: expr): expr = 
  lua_pushcclosure(L, (f), 0)
 """

template lua_isfunction*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TFUNCTION)

template lua_istable*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TTABLE)

template lua_islightuserdata*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TLIGHTUSERDATA)

template lua_isnil*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TNIL)

template lua_isboolean*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TBOOLEAN)

template lua_isthread*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TTHREAD)

template lua_isnone*(L, n: expr): expr = 
  (lua_type(L, (n)) == LUA_TNONE)

template lua_isnoneornil*(L, n: expr): expr = 
  (lua_type(L, (n)) <= 0)

template lua_pushliteral*(L, s: expr): expr = 
  lua_pushlstring(L, s, (sizeof(s) div sizeof(char)) - 1)

template lua_pushglobaltable*(L: expr): expr = 
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS)

template lua_tostring*(L, i: expr): expr = 
  lua_tolstring(L, (i), nil)

#
#* {======================================================================
#* Debug API
#* =======================================================================
#
#
#* Event codes
#
const 
  LUA_HOOKCALL* = 0
  LUA_HOOKRET* = 1
  LUA_HOOKLINE* = 2
  LUA_HOOKCOUNT* = 3
  LUA_HOOKTAILCALL* = 4
#
#* Event masks
#
const 
  LUA_MASKCALL* = (1 shl LUA_HOOKCALL)
  LUA_MASKRET* = (1 shl LUA_HOOKRET)
  LUA_MASKLINE* = (1 shl LUA_HOOKLINE)
  LUA_MASKCOUNT* = (1 shl LUA_HOOKCOUNT)
# activation record 


#@@ LUA_IDSIZE gives the maximum size for the description of the source
#@* of a function in debug information.
#* CHANGE it if you want a different size.
#
const 
  LUA_IDSIZE* = 60
# Functions to be called by the debugger in specific events 
type 
  PDebug* = ptr lua52.TDebug
  TDebug* {.pure, final.} = object 
    event*: cint
    name*: cstring        # (n) 
    namewhat*: cstring    # (n) 'global', 'local', 'field', 'method' 
    what*: cstring        # (S) 'Lua', 'C', 'main', 'tail' 
    source*: cstring      # (S) 
    currentline*: cint    # (l) 
    linedefined*: cint    # (S) 
    lastlinedefined*: cint # (S) 
    nups*: cuchar         # (u) number of upvalues 
    nparams*: cuchar      # (u) number of parameters 
    isvararg*: char       # (u) 
    istailcall*: char     # (t) 
    short_src*: array[LUA_IDSIZE, char] # (S) \ 
            # private part
    i_ci: pointer#ptr CallInfo   # active function 


type 
  lua_Hook* = proc (L: PState; ar: PDebug) {.cdecl.}
proc getstack*(L: PState; level: cint; ar: PDebug): cint {.ilua.}
proc getinfo*(L: PState; what: cstring; ar: PDebug): cint {.ilua.}
proc getlocal*(L: PState; ar: PDebug; n: cint): cstring {.ilua.}
proc setlocal*(L: PState; ar: PDebug; n: cint): cstring {.ilua.}
proc getupvalue*(L: PState; funcindex: cint; n: cint): cstring {.ilua.}
proc setupvalue*(L: PState; funcindex: cint; n: cint): cstring {.ilua.}
proc upvalueid*(L: PState; fidx: cint; n: cint): pointer {.ilua.}
proc upvaluejoin*(L: PState; fidx1: cint; n1: cint; fidx2: cint; 
                      n2: cint) {.ilua.}
proc sethook*(L: PState; func: lua_Hook; mask: cint; count: cint): cint {.ilua.}
proc gethook*(L: PState): lua_Hook {.ilua.}
proc gethookmask*(L: PState): cint {.ilua.}
proc gethookcount*(L: PState): cint {.ilua.}

# }====================================================================== 
#*****************************************************************************
# Copyright (C) 1994-2012 Lua.org, PUC-Rio.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#****************************************************************************




#
#* $Id: lualib.h,v 1.43 2011/12/08 12:11:37 roberto Exp $
#* Lua standard libraries
#* See Copyright Notice in lua.h
#

proc open_base*(L: PState): cint {.iluaLIB.}
const 
  LUA_COLIBNAME* = "coroutine"
proc open_coroutine*(L: PState): cint {.iluaLIB.}
const 
  LUA_TABLIBNAME* = "table"
proc open_table*(L: PState): cint {.iluaLIB.}
const 
  LUA_IOLIBNAME* = "io"
proc open_io*(L: PState): cint {.iluaLIB.}
const 
  LUA_OSLIBNAME* = "os"
proc open_os*(L: PState): cint {.iluaLIB.}
const 
  LUA_STRLIBNAME* = "string"
proc open_string*(L: PState): cint {.iluaLIB.}
const 
  LUA_BITLIBNAME* = "bit32"
proc open_bit32*(L: PState): cint {.iluaLIB.}
const 
  LUA_MATHLIBNAME* = "math"
proc open_math*(L: PState): cint {.iluaLIB.}
const 
  LUA_DBLIBNAME* = "debug"
proc open_debug*(L: PState): cint {.iluaLIB.}
const 
  LUA_LOADLIBNAME* = "package"
proc open_package*(L: PState): cint {.iluaLIB.}
# open all previous libraries 
proc openlibs*(L: PState) {.iluaL.}
when not defined(lua_assert): 
  template lua_assert*(x: expr): expr = 
    (cast[nil](0))











#
#* $Id: lauxlib.h,v 1.120 2011/11/29 15:55:08 roberto Exp $
#* Auxiliary functions for building Lua libraries
#* See Copyright Notice in lua.h
#

# extra error code for `luaL_load' 
const 
  LUA_ERRFILE* = Thread_ErrErr.cint + 1'i32 #(LUA_ERRERR + 1)
type 
  luaL_Reg* = tuple[ 
    name: cstring,
    func: TCFunction]

### IMPORT FROM "luaL_$1"
proc checkversion*(L: PState; ver: lua_Number) {.importc: "luaL_checkversion_".}
template luaL_checkversion*(L: expr): expr = 
  checkversion(L, LUA_VERSION_NUM)

proc getmetafield*(L: PState; obj: cint; e: cstring): cint {.iluaL.}
proc callmeta*(L: PState; obj: cint; e: cstring): cint {.iluaL.}
#proc tolstring*(L: PState; idx: cint; len: ptr csize): cstring {.importc: "luaL_tolstring".}
# ^ duplicate?
proc argerror*(L: PState; numarg: cint; extramsg: cstring): cint {.iluaL.}
proc checklstring*(L: PState; numArg: cint; len: ptr csize): cstring {.iluaL.}
proc optlstring*(L: PState; numArg: cint; def: cstring; len: ptr csize): cstring {.iluaL.}
proc checknumber*(L: PState; numArg: cint): lua_Number {.iluaL.}
proc optnumber*(L: PState; nArg: cint; def: lua_Number): lua_Number {.iluaL.}
proc checkinteger*(L: PState; numArg: cint): lua_Integer {.iluaL.}
proc optinteger*(L: PState; nArg: cint; def: lua_Integer): lua_Integer {.iluaL.}
proc checkunsigned*(L: PState; numArg: cint): lua_Unsigned {.iluaL.}
proc optunsigned*(L: PState; numArg: cint; def: lua_Unsigned): lua_Unsigned {.iluaL.}
proc checkstack*(L: PState; sz: cint; msg: cstring) {.iluaL.}
proc checktype*(L: PState; narg: cint; t: cint) {.iluaL.}
proc checkany*(L: PState; narg: cint) {.iluaL.}
proc newmetatable*(L: PState; tname: cstring): cint {.iluaL.}
proc setmetatable*(L: PState; tname: cstring) {.iluaL.}
proc testudata*(L: PState; ud: cint; tname: cstring): pointer {.iluaL.}
proc checkudata*(L: PState; ud: cint; tname: cstring): pointer {.iluaL.}
proc where*(L: PState; lvl: cint) {.iluaL.}
proc error*(L: PState; fmt: cstring): cint {.varargs, iluaL.}
proc checkoption*(L: PState; narg: cint; def: cstring; lst: ptr cstring): cint {.iluaL.}
proc fileresult*(L: PState; stat: cint; fname: cstring): cint {.iluaL.}
proc execresult*(L: PState; stat: cint): cint {.iluaL.}
# pre-defined references 
const 
  LUA_NOREF* = (- 2)
  LUA_REFNIL* = (- 1)
proc `ref`*(L: PState; t: cint): cint {.iluaL.}
proc unref*(L: PState; t: cint; `ref`: cint) {.iluaL.}
proc loadfilex*(L: PState; filename: cstring; mode: cstring): cint {.iluaL.}
template luaL_loadfile*(L, f: expr): expr = 
  luaL_loadfilex(L, f, nil)

proc loadbufferx*(L: PState; buff: cstring; sz: csize; name: cstring; 
                  mode: cstring): cint {.iluaL.}
proc loadstring*(L: PState; s: cstring): cint {.iluaL.}
proc newstate*(): PState {.iluaL.}
proc len*(L: PState; idx: cint): cint {.iluaL.}
proc gsub*(L: PState; s: cstring; p: cstring; r: cstring): cstring {.iluaL.}
proc setfuncs*(L: PState; L2: ptr luaL_Reg; nup: cint) {.iluaL.}
proc getsubtable*(L: PState; idx: cint; fname: cstring): cint {.iluaL.}
proc traceback*(L: PState; L1: PState; msg: cstring; level: cint) {.iluaL.}
proc requiref*(L: PState; modname: cstring; openf: TCFunction; 
               glb: cint) {.iluaL.}
#
#* ===============================================================
#* some useful macros
#* ===============================================================
#
##define luaL_newlibtable(Lua,arr)	\
#  lua_createtable(Lua, 0, sizeof(arr)/sizeof(arr[0]) - 1)
##define luaL_newlib(L,l)	(luaL_newlibtable(L,l), luaL_setfuncs(L,l,0))
template luaL_argcheck*(L, cond, numarg, extramsg: expr): expr = 
  (cast[nil](((cond) or luaL_argerror(L, (numarg), (extramsg)))))

template luaL_checkstring*(L, n: expr): expr = 
  (luaL_checklstring(L, (n), nil))

template luaL_optstring*(L, n, d: expr): expr = 
  (luaL_optlstring(L, (n), (d), nil))

template luaL_checkint*(L, n: expr): expr = 
  (cast[cint](luaL_checkinteger(L, (n))))

template luaL_optint*(L, n, d: expr): expr = 
  (cast[cint](luaL_optinteger(L, (n), (d))))

template luaL_checklong*(L, n: expr): expr = 
  (cast[clong](luaL_checkinteger(L, (n))))

template luaL_optlong*(L, n, d: expr): expr = 
  (cast[clong](luaL_optinteger(L, (n), (d))))

template luaL_typename*(L, i: expr): expr = 
  lua_typename(L, lua_type(L, (i)))

template luaL_dofile*(L, fn: expr): expr = 
  (luaL_loadfile(L, fn) or lua_pcall(L, 0, LUA_MULTRET, 0))

template luaL_dostring*(L, s: expr): expr = 
  (luaL_loadstring(L, s) or lua_pcall(L, 0, LUA_MULTRET, 0))

template luaL_getmetatable*(L, n: expr): expr = 
  (lua_getfield(L, LUA_REGISTRYINDEX, (n)))

template luaL_opt*(L, f, n, d: expr): expr = 
  (if lua_isnoneornil(L, (n)): (d) else: f(L, (n)))

template luaL_loadbuffer*(L, s, sz, n: expr): expr = 
  luaL_loadbufferx(L, s, sz, n, nil)


#
#@@ TBufferSIZE is the buffer size used by the lauxlib buffer system.
#* CHANGE it if it uses too much C-stack space.
#
const 
  Lua_BufferSIZE* = 8192'i32 # BUFSIZ\
    ## COULD NOT FIND BUFSIZE ?? on my machine this is 8192
#
#* {======================================================
#* Generic Buffer manipulation
#* =======================================================
#
type 
  PBuffer* = ptr TBuffer
  TBuffer* {.pure, final.} = object 
    b*: cstring             # buffer address 
    size*: csize           # buffer size 
    n*: csize              # number of characters in buffer 
    L*: PState
    initb*: array[Lua_BufferSIZE, char] # initial buffer 
  
##define luaL_addchar(B,c) \
#  ((void)((B)->n < (B)->size || luaL_prepbuffsize((B), 1)), \
#   ((B)->b[(B)->n++] = (c)))
#
template luaL_addsize*(B, s: expr): expr = 
  (inc((B).n, (s)))

proc buffinit*(L: PState; B: PBuffer) {.iluaL.}
proc prepbuffsize*(B: PBuffer; sz: csize): cstring {.iluaL.}
proc addlstring*(B: PBuffer; s: cstring; l: csize) {.iluaL.}
proc addstring*(B: PBuffer; s: cstring) {.iluaL.}
proc addvalue*(B: PBuffer) {.iluaL.}
proc pushresult*(B: PBuffer) {.iluaL.}
proc pushresultsize*(B: PBuffer; sz: csize) {.iluaL.}
proc buffinitsize*(L: PState; B: PBuffer; sz: csize): cstring {.iluaL.}
template prepbuffer*(B: expr): expr = 
  prepbuffsize(B, TBufferSIZE)

# }====================================================== 
#
#* {======================================================
#* File handles for IO library
#* =======================================================
#
#
#* A file handle is a userdata with metatable 'LUA_FILEHANDLE' and
#* initial structure 'luaL_Stream' (it may contain other fields
#* after that initial structure).
#
const 
  LUA_FILEHANDLE* = "FILE*"
type 
  luaL_Stream* {.pure, final.} = object 
    f*: TFILE            # stream (NULL for incompletely created streams) 
    closef*: TCFunction  # to close stream (NULL for closed streams) 
  
# }====================================================== 
# compatibility with old module system 
when defined(LUA_COMPAT_MODULE): 
  proc pushmodule*(L: PState; modname: cstring; sizehint: cint){.iluaL.}
  proc openlib*(L: PState; libname: cstring; l: ptr luaL_Reg; nup: cint){.iluaL.}
  template luaL_register*(L, n, l: expr): expr = 
    (openlib(L, (n), (l), 0))






when isMainModule:
  #import lua52
  import strutils

  echo "Starting Lua"
  var L = newState()

  echo "Loading libraries"
  L.openLibs

  when defined (Lua_REPL): 
    import rdstdin
    echo "To leave the REPL, hit ^D, type !!!, or call quit()"
    
    var line: string = ""
    while readlineFromStdin ("> ", line):
    
      if line == "!!!": break
      
      let result = L.loadString(line).TThreadStatus
      if result == Thread_OK:
        let result =  L.pcall(0, LUA_MULTRET, 0).TThreadStatus
        case result
        of Thread_OK:     discard
        else:             echo result
      else:
        echo result
      
  else:
    proc testFunc (L: PState): cint {.cdecl.} =
      #foo
      echo "Hello thar"

    echo "Setting testFunc"
    L.pushCfunction testFunc
    L.setGlobal "testFunc"

    const LuaScript = "testFunc()"
    echo "Loading script: \"\"\"\L$1\L\"\"\"".format(luaScript)
    let result = L.loadString (luaScript) . TThreadStatus
    echo "return: ", result

    if result == Thread_OK:
      echo "Running script"
      let result = L.pcall (0, LUA_MULTRET, 0)

  echo "Closing Lua state"
  L.close


