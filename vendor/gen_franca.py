import sokol.bindgen.gen_ir as gen_ir
import os, shutil, sys

import sokol.bindgen.gen_util as util

module_names = {
    'slog_':    'log',
    'sg_':      'gfx',
    'sapp_':    'app',
    'stm_':     'time',
    'saudio_':  'audio',
    'sgl_':     'gl',
    'sdtx_':    'debugtext',
    'sshape_':  'shape',
    'sglue_':   'glue',
    'sfetch_':  'fetch',
    'simgui_':  'imgui',
}

c_source_paths = {
    'slog_':    '../sokol_log.h',
    'sg_':      '../sokol_gfx.h',
    'sapp_':    '../sokol_app.h',
    'stm_':     '../sokol_time.h',
    'saudio_':  '../sokol_audio.h',
    'sgl_':     '../sokol_gl.h',
    'sdtx_':    '../sokol_debugtext.h',
    'sshape_':  '../sokol_shape.h',
    'sglue_':   '../sokol_glue.h',
    'sfetch_':  '../sokol_fetch.h',
    'simgui_':  '../sokol_imgui.h',
}

ignores = [
    'sdtx_printf',
    'sdtx_vprintf',
]

prim_types = {
    'int':          'i32',
    'bool':         'bool',
    'char':         'u8',
    'int8_t':       'i8',
    'uint8_t':      'u8',
    'int16_t':      'i16',
    'uint16_t':     'u16',
    'int32_t':      'i32',
    'uint32_t':     'u32',
    'int64_t':      'i64',
    'uint64_t':     'u64',
    'float':        'f32',
    'double':       'f64',
    'uintptr_t':    'usize',
    'intptr_t':     'isize',
    'size_t':       'usize'
}

prim_defaults = {
    'int':          '0',
    'bool':         'false',
    'int8_t':       '0.trunc()',
    'uint8_t':      '0.trunc()',
    'int16_t':      '0.trunc()',
    'uint16_t':     '0.trunc()',
    'int32_t':      '0.intcast()',
    'uint32_t':     '0.trunc()',
    'int64_t':      '0',
    'uint64_t':     '0.bitcast()',
    'float':        '0.0',
    'double':       '0.0',
    'uintptr_t':    '0.bitcast()',
    'intptr_t':     '0',
    'size_t':       '0'
}


struct_types = []
enum_types = []
enum_items = {}
out_lines = ''

def reset_globals():
    global struct_types
    global enum_types
    global enum_items
    global out_lines
    struct_types = []
    enum_types = []
    enum_items = {}
    out_lines = ''

def l(s):
    global out_lines
    out_lines += s + '\n'

def as_prim_type(s):
    return prim_types[s]

# prefix_bla_blub(_t) => PrefixBlaBlub
def as_struct_type(s, prefix):
    parts = s.lower().split('_')
    outp = '' # if s.startswith(prefix) else f'{parts[0]}.'
    # for part in parts[1:]:
    for part in parts:
        # ignore '_t' type postfix
        if (part != 't'):
            outp += part.capitalize()
    return outp

# prefix_bla_blub(_t) => PrefixBlaBlub
def as_enum_type(s, prefix):
    parts = s.lower().split('_')
    outp = '' # if s.startswith(prefix) else f'{parts[0]}.'
    # for part in parts[1:]:
    for part in parts:
        if (part != 't'):
            outp += part.capitalize()
    
    return outp

def check_override(name, default=None):
    if default is None:
        return name
    else:
        return default

def check_ignore(name):
    return name in ignores

# PREFIX_ENUM_BLA => Bla, _PREFIX_ENUM_BLA => Bla
def as_enum_item_name(s):
    outp = s.lstrip('_')
    parts = outp.split('_')[2:]
    outp = '_'.join(parts)
    if outp[0].isdigit():
        outp = '_' + outp
    return outp

def enum_default_item(enum_name):
    return enum_items[enum_name][0]

def is_prim_type(s):
    return s in prim_types

def is_struct_type(s):
    return s in struct_types

def is_enum_type(s):
    return s in enum_types

def is_const_prim_ptr(s):
    for prim_type in prim_types:
        if s == f"const {prim_type} *":
            return True
    return False

def is_prim_ptr(s):
    for prim_type in prim_types:
        if s == f"{prim_type} *":
            return True
    return False

def is_const_struct_ptr(s):
    for struct_type in struct_types:
        if s == f"const {struct_type} *":
            return True
    return False

def type_default_value(s):
    return prim_defaults[s]

def as_c_arg_type(arg_type, prefix):
    if arg_type == "void":
        return "Unit"
    elif is_prim_type(arg_type):
        return as_prim_type(arg_type)
    elif is_struct_type(arg_type):
        return as_struct_type(arg_type, prefix)
    elif is_enum_type(arg_type):
        return as_enum_type(arg_type, prefix)
    elif util.is_void_ptr(arg_type):
        return "rawptr"
    elif util.is_const_void_ptr(arg_type):
        return "rawptr"
    elif util.is_string_ptr(arg_type):
        return "CStr"
    elif is_const_struct_ptr(arg_type):
        return f"*{as_struct_type(util.extract_ptr_type(arg_type), prefix)}"
    elif is_prim_ptr(arg_type):
        return f"*{as_prim_type(util.extract_ptr_type(arg_type))}"
    elif is_const_prim_ptr(arg_type):
        return f"*{as_prim_type(util.extract_ptr_type(arg_type))}"
    else:
        sys.exit(f"Error as_c_arg_type(): {arg_type}")

def as_arg_type(arg_prefix, arg_type, prefix):
    # NOTE: if arg_prefix is None, the result is used as return value
    pre = "" if arg_prefix is None else arg_prefix
    if arg_type == "void":
        if arg_prefix is None:
            return "Unit"
        else:
            return ""
    elif is_prim_type(arg_type):
        return pre + as_prim_type(arg_type)
    elif is_struct_type(arg_type):
        return pre + as_struct_type(arg_type, prefix)
    elif is_enum_type(arg_type):
        return pre + as_enum_type(arg_type, prefix)
    elif util.is_void_ptr(arg_type):
        return pre + "?*anyopaque"
    elif util.is_const_void_ptr(arg_type):
        return pre + "?*const anyopaque"
    elif util.is_string_ptr(arg_type):
        return pre + "[:0]const u8"
    elif is_const_struct_ptr(arg_type):
        # not a bug, pass const structs by value
        # (franca) TODO: this would probably be fine? my backend would do an extra copy but the calling convention is the same. 
        return pre + f"*{as_struct_type(util.extract_ptr_type(arg_type), prefix)}"
    elif is_prim_ptr(arg_type):
        return pre + f"*{as_prim_type(util.extract_ptr_type(arg_type))}"
    elif is_const_prim_ptr(arg_type):
        return pre + f"*{as_prim_type(util.extract_ptr_type(arg_type))}"
    else:
        sys.exit(f"ERROR as_arg_type(): {arg_type}")

def is_string(type):
    return type == "[:0]const u8"

# get C-style arguments of a function pointer as string
def funcptr_args_c(field_type, prefix):
    tokens = field_type[field_type.index('(*)')+4:-1].split(',')
    s = ""
    for token in tokens:
        arg_type = token.strip()
        if s != "":
            s += ", "
        c_arg = as_c_arg_type(arg_type, prefix)
        if c_arg == "void":
            return ""
        else:
            s += c_arg
    return s

# get C-style result of a function pointer as string
def funcptr_result_c(field_type):
    res_type = field_type[:field_type.index('(*)')].strip()
    if res_type == 'void':
        return 'Unit'
    elif is_prim_type(res_type):
        return as_prim_type(res_type)
    elif util.is_const_void_ptr(res_type):
        return 'rawptr'
    elif util.is_void_ptr(res_type):
        return 'rawptr'
    else:
        sys.exit(f"ERROR funcptr_result_c(): {field_type}")

def funcdecl_args_c(decl, prefix):
    s = ""
    func_name = decl['name']
    for param_decl in decl['params']:
        if s != "":
            s += ", "
        param_name = param_decl['name']
        param_type = check_override(f'{func_name}.{param_name}', default=param_decl['type'])
        s += param_name + ": " + as_c_arg_type(param_type, prefix)
    return s

def funcdecl_result_c(decl, prefix):
    func_name = decl['name']
    decl_type = decl['type']
    result_type = check_override(f'{func_name}.RESULT', default=decl_type[:decl_type.index('(')].strip())
    return as_c_arg_type(result_type, prefix)

def gen_struct(decl, prefix):
    struct_name = check_override(decl['name'])
    type = as_struct_type(struct_name, prefix)
    l(f"{type} :: @struct(")
    for field in decl['fields']:
        field_name = check_override(field['name'])
        field_type = check_override(f'{struct_name}.{field_name}', default=field['type'])
        if is_prim_type(field_type):
            l(f"    {field_name}: {as_prim_type(field_type)},")
        elif is_struct_type(field_type):
            l(f"    {field_name}: {as_struct_type(field_type, prefix)},")
        elif is_enum_type(field_type):
            l(f"    {field_name}: {as_enum_type(field_type, prefix)},")
        elif util.is_string_ptr(field_type):
            l(f"    {field_name}: CStr,")
        elif util.is_const_void_ptr(field_type):
            l(f"    {field_name}: rawptr,")
        elif util.is_void_ptr(field_type):
            l(f"    {field_name}: rawptr,")
        elif is_const_prim_ptr(field_type):
            l(f"    {field_name}: *{as_prim_type(util.extract_ptr_type(field_type))},")
        elif util.is_func_ptr(field_type):
            l(f"    {field_name}: @FnPtr({funcptr_args_c(field_type, prefix)}) {funcptr_result_c(field_type)},")
        elif util.is_1d_array_type(field_type):
            array_type = util.extract_array_type(field_type)
            array_sizes = util.extract_array_sizes(field_type)
            if is_prim_type(array_type) or is_struct_type(array_type):
                if is_prim_type(array_type):
                    type = as_prim_type(array_type)
                elif is_struct_type(array_type):
                    type = as_struct_type(array_type, prefix)
                elif is_enum_type(array_type):
                    type = as_enum_type(array_type, prefix)
                else:
                    sys.exit(f"ERROR gen_struct is_1d_array_type: {array_type}")
                t0 = f"Array({type}, {array_sizes[0]})"
                l(f"    {field_name}: {t0},")
            elif util.is_const_void_ptr(array_type):
                l(f"    {field_name}: Array(rawptr, {array_sizes[0]}),")
            else:
                sys.exit(f"ERROR gen_struct: array {field_name}: {field_type} => {array_type} [{array_sizes[0]}]")
        elif util.is_2d_array_type(field_type):
            array_type = util.extract_array_type(field_type)
            array_sizes = util.extract_array_sizes(field_type)
            if is_prim_type(array_type):
                type = as_prim_type(array_type)
            elif is_struct_type(array_type):
                type = as_struct_type(array_type, prefix)
            else:
                sys.exit(f"ERROR gen_struct is_2d_array_type: {array_type}")
            t0 = f"Array(Array({type}, {array_sizes[1]}), {array_sizes[0]})"
            l(f"    {field_name}: {t0},")
        else:
            sys.exit(f"ERROR gen_struct: {field_name}: {field_type};")
    l(");")

def gen_consts(decl, prefix):
    for item in decl['items']:
        item_name = check_override(item['name'])
        l(f"{util.as_lower_snake_case(item_name, prefix)} :: {item['value']};")

def gen_enum(decl, prefix):
    enum_name = check_override(decl['name'])
    l(f"{as_enum_type(enum_name, prefix)} :: @enum(i32) (")
    for item in decl['items']:
        item_name = as_enum_item_name(check_override(item['name']))
        if item_name != "FORCE_U32":
            if 'value' in item:
                l(f"    {item_name} = {item['value']},")
            else:
                l(f"    {item_name},")
    l(");")

def gen_func_c(decl, prefix):
    l(f"fn {decl['name']}({funcdecl_args_c(decl, prefix)}) {funcdecl_result_c(decl, prefix)} #import(\"sokol\");")

def pre_parse(inp):
    global struct_types
    global enum_types
    for decl in inp['decls']:
        kind = decl['kind']
        if kind == 'struct':
            struct_types.append(decl['name'])
        elif kind == 'enum':
            enum_name = decl['name']
            enum_types.append(enum_name)
            enum_items[enum_name] = []
            for item in decl['items']:
                enum_items[enum_name].append(as_enum_item_name(item['name']))

def gen_module(inp, dep_prefixes):
    l('// machine generated, do not edit')
    l('')
    pre_parse(inp)
    prefix = inp['prefix']
    for decl in inp['decls']:
        if not decl['is_dep']:
            kind = decl['kind']
            if kind == 'consts':
                gen_consts(decl, prefix)
            elif not check_ignore(decl['name']):
                if kind == 'struct':
                    gen_struct(decl, prefix)
                elif kind == 'enum':
                    gen_enum(decl, prefix)
                elif kind == 'func':
                    gen_func_c(decl, prefix)

def prepare():
    if not os.path.isdir("sokol-zig"):
        print("don't forget to 'git clone https://github.com/floooh/sokol-zig'")
        exit(1)
    print('=== Generating Franca bindings:')

full_output = ""
def gen(c_header_path, c_prefix, dep_c_prefixes):
    global full_output
    if not c_prefix in module_names:
        print(f' >> warning: skipping generation for {c_prefix} prefix...')
        return
    module_name = module_names[c_prefix]
    print(f'  {c_header_path} => {module_name}')
    reset_globals()
    # (franca) still using the zig c files cause they know which includes you need
    c_source_path = f"sokol-zig/src/sokol/c/sokol_{module_name}.c"
    ir = gen_ir.gen(c_header_path, c_source_path, module_name, c_prefix, dep_c_prefixes)
    gen_module(ir, dep_c_prefixes)
    os.remove(ir['module'] + ".json")
    full_output += "//! " + ir['module'] + "\n"
    full_output += out_lines

tasks = [
    [ 'sokol/sokol_log.h',            'slog_',     [] ],
    [ 'sokol/sokol_gfx.h',            'sg_',       [] ],
    [ 'sokol/sokol_app.h',            'sapp_',     [] ],
    [ 'sokol/sokol_glue.h',           'sglue_',    ['sg_'] ],
    [ 'sokol/sokol_time.h',           'stm_',      [] ],
    [ 'sokol/sokol_audio.h',          'saudio_',   [] ],
    [ 'sokol/util/sokol_gl.h',        'sgl_',      ['sg_'] ],
    [ 'sokol/util/sokol_debugtext.h', 'sdtx_',     ['sg_'] ],
    [ 'sokol/util/sokol_shape.h',     'sshape_',   ['sg_'] ],
]

if __name__ == "__main__":
    prepare()
    for task in tasks:
        [c_header_path, main_prefix, dep_prefixes] = task
        gen(c_header_path, main_prefix, dep_prefixes)
    
    
    output_path = f"sokol.fr"
    with open(output_path, 'w', newline='\n') as f_outp:
        f_outp.write(full_output)

"""
zlib/libpng license

Copyright (c) 2018 Andre Weissflog

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the
use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in a
    product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not
    be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
"""
