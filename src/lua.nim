#*****************************************************************************
# *                                                                            *
# *  File:        lua.pas                                                      *
# *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
# *               Lavergne Thomas   (original translation to Pascal)           *
# *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
# *  Description: Basic Lua library                                            *
# *                                                                            *
# *****************************************************************************
#
#** $Id: lua.h,v 1.175 2003/03/18 12:31:39 roberto Exp $
#** Lua - An Extensible Extension Language
#** TeCGraf: Computer Graphics Technology Group, PUC-Rio, Brazil
#** http://www.lua.org   mailto:info@lua.org
#** See Copyright Notice at the end of this file
#
#
#** Updated to Lua 5.1.1 by Bram Kuijvenhoven (bram at kuijvenhoven dot net),
#**   Hexis BV (http://www.hexis.nl), the Netherlands
#** Notes:
#**    - Only tested with FPC (FreePascal Compiler)
#**    - Using LuaBinaries styled DLL/SO names, which include version names
#**    - LUA_YIELD was suffixed by '_' for avoiding name collision
#
#
#** Translated to pascal by Lavergne Thomas
#** Notes :
#**    - pointers type was prefixed with 'P'
#**    - lua_upvalueindex constant was transformed to function
#**    - Some compatibility function was isolated because with it you must have
#**      lualib.
#**    - LUA_VERSION was suffixed by '_' for avoiding name collision.
#** Bug reports :
#**    - thomas.lavergne@laposte.net
#**   In french or in english
#

when defined(useLuajit):
  when defined(MACOSX):
    const
      NAME* = "libluajit.dylib"
      LIB_NAME* = "libluajit.dylib"
  elif defined(UNIX):
    const
      NAME* = "libluajit.so(|.0)"
      LIB_NAME* = "libluajit.so(|.0)"
  else:
    const
      NAME* = "luajit.dll"
      LIB_NAME* = "luajit.dll"
else:
  when defined(MACOSX):
    const
      NAME* = "liblua(|5.1|5.0).dylib"
      LIB_NAME* = "liblua(|5.1|5.0).dylib"
  elif defined(UNIX):
    const
      NAME* = "liblua(|5.1|5.0).so(|.0)"
      LIB_NAME* = "liblua(|5.1|5.0).so(|.0)"
  else:
    const 
      NAME* = "lua(|5.1|5.0).dll"
      LIB_NAME* = "lua(|5.1|5.0).dll"

const 
  VERSION* = "Lua 5.1"
  RELEASE* = "Lua 5.1.1"
  VERSION_NUM* = 501
  COPYRIGHT* = "Copyright (C) 1994-2006 Lua.org, PUC-Rio"
  AUTHORS* = "R. Ierusalimschy, L. H. de Figueiredo & W. Celes"
  # option for multiple returns in `lua_pcall' and `lua_call' 
  MULTRET* = - 1              #
                              #** pseudo-indices
                              #
  REGISTRYINDEX* = - 10000
  ENVIRONINDEX* = - 10001
  GLOBALSINDEX* = - 10002

proc upvalueindex*(i: cint): cint
const                         # thread status; 0 is OK 
  constYIELD* = 1
  ERRRUN* = 2
  ERRSYNTAX* = 3
  ERRMEM* = 4
  ERRERR* = 5

type 
  PState* = pointer
  CFunction* = proc (state: PState): cint{.cdecl.}

#
#** functions that read/write blocks when loading/dumping Lua chunks
#

type 
  Reader* = proc (L: PState, ud: pointer, sz: ptr cint): cstring{.cdecl.}
  Writer* = proc (L: PState, p: pointer, sz: cint, ud: pointer): cint{.cdecl.}
  Alloc* = proc (ud, theptr: pointer, osize, nsize: cint){.cdecl.}

const 
  TNONE* = - 1
  TNIL* = 0
  TBOOLEAN* = 1
  TLIGHTUSERDATA* = 2
  TNUMBER* = 3
  TSTRING* = 4
  TTABLE* = 5
  TFUNCTION* = 6
  TUSERDATA* = 7
  TTHREAD* = 8                # minimum Lua stack available to a C function 
  MINSTACK* = 20

type                          # Type of Numbers in Lua 
  Number* = float
  Integer* = cint

{.pragma: ilua, importc: "lua_$1".}

{.push callConv: cdecl, dynlib: LibName.}
#{.push importc: "lua_$1".}

proc newstate*(f: Alloc, ud: pointer): PState {.ilua.}

proc close*(state: PState){.ilua.}
proc newthread*(state: PState): PState{.ilua.}
proc atpanic*(state: PState, panicf: CFunction): CFunction{.ilua.}

proc gettop*(state: PState): cint{.ilua.}
proc settop*(state: PState, idx: cint){.ilua.}
proc pushvalue*(state: PState, Idx: cint){.ilua.}
proc remove*(state: PState, idx: cint){.ilua.}
proc insert*(state: PState, idx: cint){.ilua.}
proc replace*(state: PState, idx: cint){.ilua.}
proc checkstack*(state: PState, sz: cint): cint{.ilua.}
proc xmove*(`from`, `to`: PState, n: cint){.ilua.}
proc isnumber*(state: PState, idx: cint): cint{.ilua.}
proc isstring*(state: PState, idx: cint): cint{.ilua.}
proc iscfunction*(state: PState, idx: cint): cint{.ilua.}
proc isuserdata*(state: PState, idx: cint): cint{.ilua.}
proc luatype*(state: PState, idx: cint): cint{.importc: "lua_type".}
proc typename*(state: PState, tp: cint): cstring{.ilua.}
proc equal*(state: PState, idx1, idx2: cint): cint{.ilua.}
proc rawequal*(state: PState, idx1, idx2: cint): cint{.ilua.}
proc lessthan*(state: PState, idx1, idx2: cint): cint{.ilua.}
proc tonumber*(state: PState, idx: cint): Number{.ilua.}
proc tointeger*(state: PState, idx: cint): Integer{.ilua.}
proc toboolean*(state: PState, idx: cint): cint{.ilua.}
proc tolstring*(state: PState, idx: cint, length: ptr cint): cstring{.ilua.}
proc objlen*(state: PState, idx: cint): cint{.ilua.}
proc tocfunction*(state: PState, idx: cint): CFunction{.ilua.}
proc touserdata*(state: PState, idx: cint): pointer{.ilua.}
proc tothread*(state: PState, idx: cint): PState{.ilua.}
proc topointer*(state: PState, idx: cint): pointer{.ilua.}
proc pushnil*(state: PState){.ilua.}
proc pushnumber*(state: PState, n: Number){.ilua.}
proc pushinteger*(state: PState, n: Integer){.ilua.}
proc pushlstring*(state: PState, s: cstring, len: cint){.ilua.}
proc pushstring*(state: PState, s: cstring){.ilua.}
proc pushvfstring*(state: PState, fmt: cstring, argp: pointer): cstring{.ilua.}
proc pushfstring*(state: PState, fmt: cstring): cstring{.varargs,ilua.}
proc pushcclosure*(state: PState, fn: CFunction, n: cint){.ilua.}
proc pushboolean*(state: PState, b: cint){.ilua.}
proc pushlightuserdata*(state: PState, p: pointer){.ilua.}
proc pushthread*(state: PState){.ilua.}
proc gettable*(state: PState, idx: cint){.ilua.}
proc getfield*(L: Pstate, idx: cint, k: cstring){.ilua.}
proc rawget*(state: PState, idx: cint){.ilua.}
proc rawgeti*(state: PState, idx, n: cint){.ilua.}
proc createtable*(state: PState, narr, nrec: cint){.ilua.}
proc newuserdata*(state: PState, sz: cint): pointer{.ilua.}
proc getmetatable*(state: PState, objindex: cint): cint{.ilua.}
proc getfenv*(state: PState, idx: cint){.ilua.}
proc settable*(state: PState, idx: cint){.ilua.}
proc setfield*(state: PState, idx: cint, k: cstring){.ilua.}
proc rawset*(state: PState, idx: cint){.ilua.}
proc rawseti*(state: PState, idx, n: cint){.ilua.}
proc setmetatable*(state: PState, objindex: cint): cint{.ilua.}
proc setfenv*(state: PState, idx: cint): cint{.ilua.}
proc call*(state: PState, nargs, nresults: cint){.ilua.}
proc pcall*(state: PState, nargs, nresults, errf: cint): cint{.ilua.}
proc cpcall*(state: PState, funca: CFunction, ud: pointer): cint{.ilua.}
proc load*(state: PState, reader: Reader, dt: pointer, chunkname: cstring): cint{.ilua.}
proc dump*(state: PState, writer: Writer, data: pointer): cint{.ilua.}
proc luayield*(state: PState, nresults: cint): cint{.importc: "lua_yield".}
proc resume*(state: PState, narg: cint): cint{.ilua.}
proc status*(state: PState): cint{.ilua.}
proc gc*(state: PState, what, data: cint): cint{.ilua.}
proc error*(state: PState): cint{.ilua.}
proc next*(state: PState, idx: cint): cint{.ilua.}
proc concat*(state: PState, n: cint){.ilua.}
proc getallocf*(state: PState, ud: ptr pointer): Alloc{.ilua.}
proc setallocf*(state: PState, f: Alloc, ud: pointer){.ilua.}
{.pop.}

#
#** Garbage-collection functions and options
#

const 
  GCSTOP* = 0
  GCRESTART* = 1
  GCCOLLECT* = 2
  GCCOUNT* = 3
  GCCOUNTB* = 4
  GCSTEP* = 5
  GCSETPAUSE* = 6
  GCSETSTEPMUL* = 7

#
#** ===============================================================
#** some useful macros
#** ===============================================================
#

proc pop*(state: PState, n: cint)
proc newtable*(state: Pstate)
proc register*(state: PState, n: cstring, f: CFunction)
proc pushcfunction*(state: PState, f: CFunction)
proc strlen*(state: Pstate, i: cint): cint
proc isfunction*(state: PState, n: cint): bool
proc istable*(state: PState, n: cint): bool
proc islightuserdata*(state: PState, n: cint): bool
proc isnil*(state: PState, n: cint): bool
proc isboolean*(state: PState, n: cint): bool
proc isthread*(state: PState, n: cint): bool
proc isnone*(state: PState, n: cint): bool
proc isnoneornil*(state: PState, n: cint): bool
proc pushliteral*(state: PState, s: cstring)
proc setglobal*(state: PState, s: cstring)
proc getglobal*(state: PState, s: cstring)
proc tostring*(state: PState, i: cint): cstring
#
#** compatibility macros and functions
#

proc getregistry*(state: PState)
proc getgccount*(state: PState): cint
type 
  Chunkreader* = Reader
  Chunkwriter* = Writer

#
#** ======================================================================
#** Debug API
#** ======================================================================
#

const 
  HOOKCALL* = 0
  HOOKRET* = 1
  HOOKLINE* = 2
  HOOKCOUNT* = 3
  HOOKTAILRET* = 4

const 
  MASKCALL* = 1 shl ord(HOOKCALL)
  MASKRET* = 1 shl ord(HOOKRET)
  MASKLINE* = 1 shl ord(HOOKLINE)
  MASKCOUNT* = 1 shl ord(HOOKCOUNT)

const 
  IDSIZE* = 60

type 
  TDebug*{.final.} = object    # activation record 
    event*: cint
    name*: cstring            # (n) 
    namewhat*: cstring        # (n) `global', `local', `field', `method' 
    what*: cstring            # (S) `Lua', `C', `main', `tail'
    source*: cstring          # (S) 
    currentline*: cint         # (l) 
    nups*: cint                # (u) number of upvalues 
    linedefined*: cint         # (S) 
    lastlinedefined*: cint     # (S) 
    short_src*: array[0.. <IDSIZE, char] # (S) \ 
                               # private part 
    i_ci*: cint                # active function 
  
  PDebug* = ptr TDebug
  Hook* = proc (state: PState, ar: PDebug){.cdecl.}

#
#** ======================================================================
#** Debug API
#** ======================================================================
#

{.push callConv: cdecl, dynlib: lua.LIB_NAME.}

proc getstack*(state: PState, level: cint, ar: PDebug): cint{.ilua.}
proc getinfo*(state: PState, what: cstring, ar: PDebug): cint{.ilua.}
proc getlocal*(state: PState, ar: PDebug, n: cint): cstring{.ilua.}
proc setlocal*(state: PState, ar: PDebug, n: cint): cstring{.ilua.}
proc getupvalue*(state: PState, funcindex: cint, n: cint): cstring{.ilua.}
proc setupvalue*(state: PState, funcindex: cint, n: cint): cstring{.ilua.}
proc sethook*(state: PState, funca: Hook, mask: cint, count: cint): cint{.ilua.}
proc gethook*(state: PState): Hook{.ilua.}
proc gethookmask*(state: PState): cint{.ilua.}
proc gethookcount*(state: PState): cint{.ilua.}

{.pop.}

# implementation

proc upvalueindex(i: cint): cint = 
  result = GLOBALSINDEX - i

proc pop(state: PState, n: cint) = 
  settop(state, - n - 1)

proc newtable(state: PState) = 
  createtable(state, 0, 0)

proc register(state: PState, n: cstring, f: CFunction) = 
  pushcfunction(state, f)
  setglobal(state, n)

proc pushcfunction(state: PState, f: CFunction) = 
  pushcclosure(state, f, 0)

proc strlen(state: PState, i: cint): cint = 
  result = objlen(state, i)

proc isfunction(state: PState, n: cint): bool = 
  result = luatype(state, n) == TFUNCTION

proc istable(state: PState, n: cint): bool = 
  result = luatype(state, n) == TTABLE

proc islightuserdata(state: PState, n: cint): bool = 
  result = luatype(state, n) == TLIGHTUSERDATA

proc isnil(state: PState, n: cint): bool = 
  result = luatype(state, n) == TNIL

proc isboolean(state: PState, n: cint): bool = 
  result = luatype(state, n) == TBOOLEAN

proc isthread(state: PState, n: cint): bool = 
  result = luatype(state, n) == TTHREAD

proc isnone(state: PState, n: cint): bool = 
  result = luatype(state, n) == TNONE

proc isnoneornil(state: PState, n: cint): bool = 
  result = luatype(state, n) <= 0

proc pushliteral(state: PState, s: cstring) = 
  pushlstring(state, s, s.len.cint)

proc setglobal(state: PState, s: cstring) = 
  setfield(state, GLOBALSINDEX, s)

proc getglobal(state: PState, s: cstring) = 
  getfield(state, GLOBALSINDEX, s)

proc tostring(state: PState, i: cint): cstring = 
  result = tolstring(state, i, nil)

proc getregistry(state: PState) = 
  pushvalue(state, REGISTRYINDEX)

proc getgccount(state: PState): cint = 
  result = gc(state, GCCOUNT, 0)


## -- lualib
#*****************************************************************************
# *                                                                            *
# *  File:        lualib.pas                                                   *
# *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
# *               Lavergne Thomas   (original translation to Pascal)           *
# *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
# *  Description: Standard Lua libraries                                       *
# *                                                                            *
# *****************************************************************************
#
#** $Id: lualib.h,v 1.28 2003/03/18 12:24:26 roberto Exp $
#** Lua standard libraries
#** See Copyright Notice in lua.h
#
#
#** Translated to pascal by Lavergne Thomas
#** Bug reports :
#**    - thomas.lavergne@laposte.net
#**   In french or in english
#

const 
  COLIBNAME* = "coroutine"
  TABLIBNAME* = "table"
  IOLIBNAME* = "io"
  OSLIBNAME* = "os"
  STRLINAME* = "string"
  MATHLIBNAME* = "math"
  DBLIBNAME* = "debug"
  LOADLIBNAME* = "package"

{.pragma: ilualib, importc: "lua$1".}

{.push callConv: cdecl, dynlib: lua.LIB_NAME.}
proc open_base*(state: PState): cint{.ilualib.}
proc open_table*(state: PState): cint{.ilualib.}
proc open_io*(state: PState): cint{.ilualib.}
proc open_string*(state: PState): cint{.ilualib.}
proc open_math*(state: PState): cint{.ilualib.}
proc open_debug*(state: PState): cint{.ilualib.}
proc open_package*(state: PState): cint{.ilualib.}
proc openlibs*(state: PState){.importc: "luaL_openlibs".}
{.pop.}

proc baselibopen*(state: PState): bool = 
  open_base(state) != 0'i32

proc tablibopen*(state: PState): bool = 
  open_table(state) != 0'i32

proc iolibopen*(state: PState): bool = 
  open_io(state) != 0'i32

proc strlibopen*(state: PState): bool = 
  open_string(state) != 0'i32

proc mathlibopen*(state: PState): bool = 
  open_math(state) != 0'i32

proc dblibopen*(state: PState): bool = 
  open_debug(state) != 0'i32



## -- lauxlib
#*****************************************************************************
# *                                                                            *
# *  File:        lauxlib.pas                                                  *
# *  Authors:     TeCGraf           (C headers + actual Lua libraries)         *
# *               Lavergne Thomas   (original translation to Pascal)           *
# *               Bram Kuijvenhoven (update to Lua 5.1.1 for FreePascal)       *
# *  Description: Lua auxiliary library                                        *
# *                                                                            *
# *****************************************************************************
#
#** $Id: lauxlib.h,v 1.59 2003/03/18 12:25:32 roberto Exp $
#** Auxiliary functions for building Lua libraries
#** See Copyright Notice in lua.h
#
#
#** Translated to pascal by Lavergne Thomas
#** Notes :
#**    - pointers type was prefixed with 'P'
#** Bug reports :
#**    - thomas.lavergne@laposte.net
#**   In french or in english
#

proc pushstring*(state: PState, s: string)
  # compatibilty macros
proc getn*(state: PState, n: cint): cint
  # calls lua_objlen
proc setn*(state: PState, t, n: cint)
  # does nothing!
type 
  Treg*{.final.} = object 
    name*: cstring
    `func`*: CFunction

  Preg* = ptr Treg


{.push callConv: cdecl, dynlib: lua.LIB_NAME.}
{.push importc: "luaL_$1".}

proc openlib*(state: PState, libname: cstring, lr: Preg, nup: cint)
proc register*(state: PState, libname: cstring, lr: Preg)

proc getmetafield*(state: PState, obj: cint, e: cstring): cint
proc callmeta*(state: PState, obj: cint, e: cstring): cint
proc typerror*(state: PState, narg: cint, tname: cstring): cint
proc argerror*(state: PState, numarg: cint, extramsg: cstring): cint
proc checklstring*(state: PState, numArg: cint, len: ptr int): cstring
proc optlstring*(state: PState, numArg: cint, def: cstring, len: ptr cint): cstring
proc checknumber*(state: PState, numArg: cint): Number
proc optnumber*(state: PState, nArg: cint, def: Number): Number
proc checkinteger*(state: PState, numArg: cint): Integer
proc optinteger*(state: PState, nArg: cint, def: Integer): Integer
proc checkstack*(state: PState, sz: cint, msg: cstring)
proc checktype*(state: PState, narg, t: cint)

proc checkany*(state: PState, narg: cint)
proc newmetatable*(state: PState, tname: cstring): cint

proc checkudata*(state: PState, ud: cint, tname: cstring): pointer
proc where*(state: PState, lvl: cint)
proc error*(state: PState, fmt: cstring): cint{.varargs.}
proc checkoption*(state: PState, narg: cint, def: cstring, lst: cstringArray): cint

proc unref*(state: PState, t, theref: cint)
proc loadfile*(state: PState, filename: cstring): cint
proc loadbuffer*(state: PState, buff: cstring, size: cint, name: cstring): cint
proc loadstring*(state: PState, s: cstring): cint
proc newstate*(): PState

{.pop.}
proc reference*(state: PState, t: cint): cint{.importc: "luaL_ref".}

{.pop.}

proc open*(): PState
  # compatibility; moved from unit lua to lauxlib because it needs luaL_newstate
  #
  #** ===============================================================
  #** some useful macros
  #** ===============================================================
  #
proc argcheck*(state: PState, cond: bool, numarg: cint, extramsg: cstring)
proc checkstring*(state: PState, n: cint): cstring
proc optstring*(state: PState, n: cint, d: cstring): cstring
proc checkint*(state: PState, n: cint): cint
proc checklong*(state: PState, n: cint): clong
proc optint*(state: PState, n: cint, d: float64): cint
proc optlong*(state: PState, n: cint, d: float64): clong
proc dofile*(state: PState, filename: cstring): cint
proc dostring*(state: PState, str: cstring): cint
proc getmetatable*(state: PState, tname: cstring)
  # not translated:
  # #define luaL_opt(L,f,n,d)  (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))
  #
  #** =======================================================
  #** Generic Buffer manipulation
  #** =======================================================
  #
const                         # note: this is just arbitrary, as it related to the BUFSIZ defined in stdio.h ...
  BUFFERSIZE* = 4096

type 
  Buffer*{.final.} = object 
    p*: cstring               # current position in buffer 
    lvl*: cint                 # number of strings in the stack (level) 
    L*: PState
    buffer*: array[0..BUFFERSIZE - 1, char] # warning: see note above about LUAL_BUFFERSIZE
  
  PBuffer* = ptr Buffer

proc addchar*(B: PBuffer, c: char)
  # warning: see note above about LUAL_BUFFERSIZE
  # compatibility only (alias for luaL_addchar) 
proc putchar*(B: PBuffer, c: char)
  # warning: see note above about LUAL_BUFFERSIZE
proc addsize*(B: PBuffer, n: cint)

{.push callConv: cdecl, dynlib: lua.LIB_NAME, importc: "luaL_$1".}
proc buffinit*(state: PState, B: PBuffer)
proc prepbuffer*(B: PBuffer): cstring
proc addlstring*(B: PBuffer, s: cstring, L: cint)
proc addstring*(B: PBuffer, s: cstring)
proc addvalue*(B: PBuffer)
proc pushresult*(B: PBuffer)
proc gsub*(state: PState, s, p, r: cstring): cstring
proc findtable*(state: PState, idx: cint, fname: cstring, szhint: cint): cstring
  # compatibility with ref system 
  # pre-defined references 
{.pop.}

const 
  NOREF* = - 2
  REFNIL* = - 1

proc unref*(state: PState, theref: cint)
proc getref*(state: PState, theref: cint)
  #
  #** Compatibility macros and functions
  #
# implementation

proc pushstring(state: PState, s: string) = 
  pushlstring(state, cstring(s), s.len.cint)

proc getn(state: PState, n: cint): cint = 
  result = objlen(state, n)

proc setn(state: PState, t, n: cint) = 
  # does nothing as this operation is deprecated
  nil

proc open(): PState = 
  result = newstate()

proc dofile(state: PState, filename: cstring): cint = 
  result = loadfile(state, filename)
  if result == 0: result = pcall(state, 0, MULTRET, 0)
  
proc dostring(state: PState, str: cstring): cint = 
  result = loadstring(state, str)
  if result == 0: result = pcall(state, 0, MULTRET, 0)
  
proc getmetatable(state: PState, tname: cstring) = 
  getfield(state, REGISTRYINDEX, tname)

proc argcheck(state: PState, cond: bool, numarg: cint, extramsg: cstring) = 
  if not cond: 
    discard argerror(state, numarg, extramsg)

proc checkstring(state: PState, n: cint): cstring = 
  result = checklstring(state, n, nil)

proc optstring(state: PState, n: cint, d: cstring): cstring = 
  result = optlstring(state, n, d, nil)

proc checkint(state: PState, n: cint): cint = 
  result = cint(checknumber(state, n))

proc checklong(state: PState, n: cint): clong = 
  result = int32(toInt(checknumber(state, n)))

proc optint(state: PState, n: cint, d: float64): cint = 
  result = optnumber(state, n, d).cint

proc optlong(state: PState, n: cint, d: float64): clong = 
  result = int32(toInt(optnumber(state, n, d)))

proc addchar(B: PBuffer, c: char) = 
  if cast[int](addr((B.p))) < (cast[int](addr((B.buffer[0]))) + BUFFERSIZE): 
    discard prepbuffer(B)
  B.p[1] = c
  B.p = cast[cstring](cast[int](B.p) + 1)

proc putchar(B: PBuffer, c: char) = 
  addchar(B, c)

proc addsize(B: PBuffer, n: cint) = 
  B.p = cast[cstring](cast[int](B.p) + n)

proc unref(state: PState, theref: cint) = 
  unref(state, REGISTRYINDEX, theref)

proc getref(state: PState, theref: cint) = 
  rawgeti(state, REGISTRYINDEX, theref)


