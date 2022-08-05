import lldb
import lldb.formatters.Logger
import re
import os
from functools import partial

"""
Convenient for debug
command script import texmacs.py
"""


class texmacs_array_SyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        logger = lldb.formatters.Logger.Logger()
        self.valobj = valobj
        self.internal_dict = internal_dict

    def num_children(self):
        logger = lldb.formatters.Logger.Logger()
        data_len = self.valobj.GetValueForExpressionPath('.rep.n')
        return data_len.GetValueAsUnsigned(0)

    def get_child_at_index(self, index):
        logger = lldb.formatters.Logger.Logger()
        logger >> "Retrieving child " + str(index)
        if index < 0:
            return None
        if index >= self.num_children():
            return None
        try:
            return self.valobj.GetValueForExpressionPath('.rep.a' + '[' + str(index) + ']')
        except:
            return None


def texmacs_array_SummaryProvider(valobj, internal_dict, options):
    data_len = valobj.GetNonSyntheticValue().GetChildMemberWithName('n').signed
    return f'size={data_len}'


def texmacs_hashmap_SummaryProvider(valobj, internal_dict, options):
    val = valobj.GetNonSyntheticValue()
    size = val.GetValueForExpressionPath('.rep.size').signed
    n = val.GetValueForExpressionPath('.rep.n').signed
    max_val = val.GetValueForExpressionPath('.rep.max').signed
    return f'size={size},n={n},max={max_val}'


class texmacs_hashmap_SyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.internal_dict = internal_dict

    def num_children(self):
        count = 3
        val = self.valobj
        n = val.GetValueForExpressionPath('.rep.n').signed
        return count + n

    def get_child_at_index(self, index):
        val = self.valobj
        if index == 0:
            return val.GetValueForExpressionPath('.rep.size')
        if index == 1:
            return val.GetValueForExpressionPath('.rep.n')
        if index == 2:
            return val.GetValueForExpressionPath('.rep.max')
        return val.GetValueForExpressionPath('.rep.a' + '[' + str(index - 3) + ']')


class texmacs_list_SyntheticChildrenProvider:
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.internal_dict = internal_dict

    def num_children(self):
        count = 0
        rep = self.valobj.GetChildMemberWithName('rep')
        while rep.unsigned != 0:
            count += 1
            next_node = rep.GetChildMemberWithName('next')
            rep = next_node.GetChildMemberWithName('rep')
        return count

    def get_child_at_index(self, index):
        logger = lldb.formatters.Logger.Logger()
        logger >> "Retrieving child " + str(index)
        count = 0
        if index < 0:
            return None
        if index >= self.num_children():
            return None
        try:
            rep = self.valobj.GetChildMemberWithName('rep')
            if index == 0:
                return rep.GetChildMemberWithName('item')
            while count < index:
                next_node = rep.GetChildMemberWithName('next')
                rep = next_node.GetChildMemberWithName('rep')
                count += 1
            return rep.GetChildMemberWithName('item')
        except:
            return None


def texmacs_path_SummaryProvider(valobj, internal_dict):
    val_list = []
    rep = valobj.GetNonSyntheticValue().GetChildMemberWithName('rep')
    while rep.unsigned != 0:
        val = rep.GetChildMemberWithName('item').signed
        val_list.append(str(val))
        next_node = rep.GetChildMemberWithName('next')
        rep = next_node.GetChildMemberWithName('rep')
    return f'size={len(val_list)} [' + ', '.join(val_list) + ']'


def texmacs_rectangle_SummaryProvider(valobj, internal_dict):
    rep = valobj.GetChildMemberWithName('rep')
    x1 = rep.GetChildMemberWithName('x1').signed
    y1 = rep.GetChildMemberWithName('y1').signed
    x2 = rep.GetChildMemberWithName('x2').signed
    y2 = rep.GetChildMemberWithName('y2').signed

    return f"({x1}, {y1}) ({x2}, {y2})"


def texmacs_xkerning_SummaryProvider(valobj, internal_dict, options):
    padding = valobj.GetValueForExpressionPath('.rep.padding').signed
    left = valobj.GetValueForExpressionPath('.rep.left').signed
    right = valobj.GetValueForExpressionPath('.rep.right').signed
    return f'padding={padding}, left={left}, right={right}'


def texmacs_simple_pencil_rep_SummaryProvider(valobj, internal_dict, options):
    col = valobj.GetValueForExpressionPath('.c').unsigned
    w = valobj.GetValueForExpressionPath('.w').signed
    a = (col >> 24) & 255
    r = (col >> 16) & 255
    g = (col >> 8) & 255
    b = col & 255
    return f'c=({r},{g},{b},{a}), w={w}'


def texmacs_qt_proxy_renderer_rep_SummaryProvider(valobj, internal_dict, options):
    w = valobj.GetValueForExpressionPath('.w').signed
    h = valobj.GetValueForExpressionPath('.h').signed
    return f'w={w}, h={h}'


def texmacs_basic_renderer_rep_SummaryProvider(valobj, internal_dict, options):
    w = valobj.GetValueForExpressionPath('.w').signed
    h = valobj.GetValueForExpressionPath('.h').signed
    return f'w={w}, h={h}'


def texmacs_renderer_rep_SummaryProvider(valobj, internal_dict, options):
    cx1 = valobj.GetValueForExpressionPath('.cx1').signed
    cy1 = valobj.GetValueForExpressionPath('.cy1').signed
    cx2 = valobj.GetValueForExpressionPath('.cx2').signed
    cy2 = valobj.GetValueForExpressionPath('.cy2').signed
    return f'({cx1},{cy1})({cx2},{cy2})'


def texmacs_box_rep_SummaryProvider(valobj, internal_dict, options):
    rep = valobj
    x1 = rep.GetChildMemberWithName('x1').signed
    y1 = rep.GetChildMemberWithName('y1').signed
    x2 = rep.GetChildMemberWithName('x2').signed
    y2 = rep.GetChildMemberWithName('y2').signed

    return f"({x1}, {y1}) ({x2}, {y2})"


def texmacs_no_brush_rep_SummaryProvider(valobj, internal_dict, options):
    return "no_brush_rep"


def texmacs_brush_SummaryProvider(valobj, internal_dict, options):
    return valobj.GetChildMemberWithName('rep')


def texmacs_basic_character_SummaryProvider(valobj, internal_dict, options):
    unicode_val = valobj.GetChildMemberWithName('rep').GetChildMemberWithName('c').signed
    return chr(unicode_val)


def texmacs_font_SummaryProvider(valobj, internal_dict, options):
    return valobj.GetValueForExpressionPath('.rep.res_name')


def texmacs_coord2_SummaryProvider(valobj, internal_dict, options):
    x1 = valobj.GetValueForExpressionPath('.x1').signed
    x2 = valobj.GetValueForExpressionPath('.x2').signed
    return f'{x1},{x2}'


def texmacs_language_SummaryProvider(valobj, internal_dict, options):
    lan_name = valobj.GetValueForExpressionPath('.rep.lan_name')
    return lan_name


def texmacs_space_SummaryProvider(valobj, internal_dict, options):
    min_val = valobj.GetValueForExpressionPath('.rep.min').signed
    def_val = valobj.GetValueForExpressionPath('.rep.def').signed
    max_val = valobj.GetValueForExpressionPath('.rep.max').signed
    return f'min={min_val},def={def_val},max={max_val}'


def texmacs_vpenalty_SummaryProvider(valobj, internal_dict, options):
    pen_val = valobj.GetValueForExpressionPath('.rep.pen').signed
    exc_val = valobj.GetValueForExpressionPath('.rep.exc').signed
    return f'pen={pen_val},exc={exc_val}'


def texmacs_cursor_SummaryProvider(valobj, internal_dict, options):
    ox_val = valobj.GetValueForExpressionPath('.rep.ox').signed
    oy_val = valobj.GetValueForExpressionPath('.rep.oy').signed
    delta_val = valobj.GetValueForExpressionPath('.rep.delta').signed
    y1_val = valobj.GetValueForExpressionPath('.rep.y1').signed
    y2_val = valobj.GetValueForExpressionPath('.rep.y2').signed
    return f'({ox_val},{oy_val}) delta={delta_val},y1={y1_val},y2={y2_val}'


def texmacs_string_SummaryProvider(valobj, internal_dict, options):
    n = valobj.GetValueForExpressionPath('.rep.n').signed
    if n == 0:
        return ""
    s = ""
    for i in range(n):
        val = valobj.GetValueForExpressionPath('.rep.a' + '[' + str(i) + ']').unsigned
        s += chr(val)
    return s


def texmacs_tree_SummaryProvider(valobj, internal_dict, options):
    try:
        val = valobj.GetValueForExpressionPath('.rep')
        op = valobj.GetValueForExpressionPath('.rep.op')
        if op.signed == 0:
            # op == STRING
            return val.GetValueForExpressionPath('.label')
        else:
            return op
    except:
        return 'BUG'


def texmacs_page_item_SummaryProvider(valobj, internal_dict, options):
    type_val = valobj.GetValueForExpressionPath('.rep.type').signed
    type_list = [
        "PAGE_LINE_ITEM   ",
        "PAGE_HIDDEN_ITEM ",
        "PAGE_CONTROL_ITEM",
        "PAGE_NOTE_ITEM   ",
    ]
    if type_val > len(type_list):
        type_str = "UNK TYPE"
    else:
        type_str = f'type={type_list[type_val].strip()}'
    return type_str


def texmacs_line_item_SummaryProvider(valobj, internal_dict, options):
    type_val = valobj.GetValueForExpressionPath('.rep.type').signed
    op_type_val = valobj.GetValueForExpressionPath('.rep.op_type').signed
    penalty_val = valobj.GetValueForExpressionPath('.rep.penalty').signed
    type_list = [
        "OBSOLETE_ITEM      ",
        "STD_ITEM           ",
        "MARKER_ITEM        ",
        "STRING_ITEM        ",
        "LEFT_BRACKET_ITEM  ",
        "MIDDLE_BRACKET_ITEM",
        "RIGHT_BRACKET_ITEM ",
        "CONTROL_ITEM       ",
        "FLOAT_ITEM         ",
        "NOTE_LINE_ITEM     ",
        "NOTE_PAGE_ITEM     ",
    ]
    op_type_list = [
        "OP_UNKNOWN        ",
        "OP_TEXT           ",
        "OP_SKIP           ",
        "OP_SYMBOL         ",
        "OP_UNARY          ",
        "OP_BINARY         ",
        "OP_N_ARY          ",
        "OP_PREFIX         ",
        "OP_POSTFIX        ",
        "OP_INFIX          ",
        "OP_PREFIX_INFIX   ",
        "OP_APPLY          ",
        "OP_SEPARATOR      ",
        "OP_OPENING_BRACKET",
        "OP_MIDDLE_BRACKET ",
        "OP_CLOSING_BRACKET",
        "OP_BIG            ",
        "OP_TOTAL          ",
    ]
    penalty_dict = {
        '10000': 'HYPH_STD',
        "1000000": "HYPH_PANIC",
        "100000000": "HYPH_INVALID"
    }
    if type_val > len(type_list):
        type_str = "UNK TYPE"
    else:
        type_str = f'type={type_list[type_val].strip()}'
    if op_type_val > len(op_type_list):
        op_type_str = 'UNK OP_TYPE'
    else:
        op_type_str = f'op_type={op_type_list[op_type_val].strip()}'
    if str(penalty_val) in penalty_dict.keys():
        penalty_str = f'penalty={penalty_dict[str(penalty_val)]}'
    else:
        penalty_str = f'UNK penalty: {penalty_val}'
    return ','.join([type_str, op_type_str, penalty_str])


def texmacs_text_property_SummaryProvider(valobj, internal_dict, options):
    type_val = valobj.GetValueForExpressionPath('.type').signed
    type_list = [
        "TP_NORMAL         ",
        "TP_HYPH           ",
        "TP_THIN_SPACE     ",
        "TP_SPACE          ",
        "TP_DSPACE         ",
        "TP_NB_THIN_SPACE  ",
        "TP_NB_SPACE       ",
        "TP_NB_DSPACE      ",
        "TP_PERIOD         ",
        "TP_CJK_NORMAL     ",
        "TP_CJK_NO_BREAK   ",
        "TP_CJK_PERIOD     ",
        "TP_CJK_WIDE_PERIOD",
        "TP_OPERATOR       ",
        "TP_SHORTOP        ",
        "TP_OTHER          ",
    ]
    if type_val > len(type_list):
        return "UNK TYPE"
    else:
        return f'type={type_list[type_val]}'


def texmacs_modification_SummaryProvider(valobj, internal_dict, options):
    k = valobj.GetValueForExpressionPath('.rep.k').signed
    k_list = [
        "MOD_ASSIGN     ",
        "MOD_INSERT     ",
        "MOD_REMOVE     ",
        "MOD_SPLIT      ",
        "MOD_JOIN       ",
        "MOD_ASSIGN_NODE",
        "MOD_INSERT_NODE",
        "MOD_REMOVE_NODE",
        "MOD_SET_CURSOR ",
    ]
    if k > len(k_list):
        return "UNK MOD"
    else:
        return k_list[k]


def texmacs_hashentry_SummaryProvider(valobj, internal_dict, options):
    try:
        key = valobj.GetValueForExpressionPath('.key')
        im = valobj.GetValueForExpressionPath('.im')
        return f'"{key}" map to "{im}"'
    except:
        return 'BUG occur'


def __lldb_init_module(debugger, dict):
    debugger.HandleCommand(
        'type summary add -x "^hashmap<.*,.*>$" -F texmacs.texmacs_hashmap_SummaryProvider -w texmacs')
    debugger.HandleCommand(
        """type synthetic add -x "^hashmap<.*,.*>$" --python-class texmacs.texmacs_hashmap_SyntheticChildrenProvider -w texmacs""")
    debugger.HandleCommand(
        'type summary add -x "^hashentry<.*,.*>$" -F texmacs.texmacs_hashentry_SummaryProvider -w texmacs')
    debugger.HandleCommand(
        """type synthetic add ="colorhash" --python-class texmacs.texmacs_hashmap_SyntheticChildrenProvider -w texmacs""")

    debugger.HandleCommand(
        """type synthetic add -x "^array<.*>$" --python-class texmacs.texmacs_array_SyntheticChildrenProvider -w texmacs""")
    debugger.HandleCommand('type summary add -x "^array<.*>$" --summary-string "size=${svar%#}" -w texmacs')

    debugger.HandleCommand(
        """type synthetic add -x "^list<.*>$" --python-class texmacs.texmacs_list_SyntheticChildrenProvider -w texmacs""")
    debugger.HandleCommand('type summary add -x "^list<.*>$" --summary-string "size=${svar%#}" -w texmacs')

    debugger.HandleCommand('type summary add -x "rectangles" --summary-string "size=${svar%#}" -w texmacs')

    # Sometimes, texmacs string is not end with \0
    # So, this command is not suitalbe
    # debugger.HandleCommand('type summary add string --summary-string "${var.rep.a}"')
    # add another texmacs_string_SummaryProvider to print string

    type_list = [
        'tree',
        'modification',
        'string',
        'rectangle',
        'xkerning',
        'font',
        'simple_pencil_rep',
        'qt_proxy_renderer_rep',
        'basic_renderer_rep',
        'renderer_rep',
        'box_rep',
        'brush',
        'no_brush_rep',
        'path',
        'basic_character',
        'coord2',
        'language',
        'space',
        'text_property',
        'line_item',
        'page_item',
        'vpenalty',
        'cursor'
    ]
    for item in type_list:
        debugger.HandleCommand(f'type summary add "{item}" -F texmacs.texmacs_{item}_SummaryProvider -w texmacs')
    debugger.HandleCommand("type category enable texmacs")
