-------------------------------------------------------------------------------
--
-- MODULE      : glue_basic.lua
-- DESCRIPTION : Building basic glue
-- COPYRIGHT   : (C) 1999-2023  Joris van der Hoeven
--                   2023       jingkaimori
--
-- This software falls under the GNU general public license version 3 or later.
-- It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
-- in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

function main()
    return {
        binding_object = "",
        initializer_name = "initialize_glue_basic",
        glues = {
            {
                scm_name = "texmacs-version-release",
                cpp_name = "texmacs_version",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "version-before?",
                cpp_name = "version_inf",
                ret_type = "bool",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "updater-supported?",
                cpp_name = "updater_supported",
                ret_type = "bool"
            },
            {
                scm_name = "updater-running?",
                cpp_name = "updater_is_running",
                ret_type = "bool"
            },
            {
                scm_name = "updater-check-background",
                cpp_name = "updater_check_background",
                ret_type = "bool"
            },
            {
                scm_name = "updater-check-foreground",
                cpp_name = "updater_check_foreground",
                ret_type = "bool"
            },
            {
                scm_name = "updater-last-check",
                cpp_name = "updater_last_check",
                ret_type = "long"
            },
            {
                scm_name = "updater-set-interval",
                cpp_name = "updater_set_interval",
                ret_type = "bool",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "get-original-path",
                cpp_name = "get_original_path",
                ret_type = "string"
            },
            {
                scm_name = "os-win32?",
                cpp_name = "os_win32",
                ret_type = "bool"
            },
            {
                scm_name = "os-mingw?",
                cpp_name = "os_mingw",
                ret_type = "bool"
            },
            {
                scm_name = "os-macos?",
                cpp_name = "os_macos",
                ret_type = "bool"
            },
            {
                scm_name = "has-printing-cmd?",
                cpp_name = "has_printing_cmd",
                ret_type = "bool"
            },
            {
                scm_name = "x-gui?",
                cpp_name = "gui_is_x",
                ret_type = "bool"
            },
            {
                scm_name = "qt-gui?",
                cpp_name = "gui_is_qt",
                ret_type = "bool"
            },
            {
                scm_name = "gui-version",
                cpp_name = "gui_version",
                ret_type = "string"
            },
            {
                scm_name = "default-look-and-feel",
                cpp_name = "default_look_and_feel",
                ret_type = "string"
            },
            {
                scm_name = "default-chinese-font",
                cpp_name = "default_chinese_font_name",
                ret_type = "string"
            },
            {
                scm_name = "default-japanese-font",
                cpp_name = "default_japanese_font_name",
                ret_type = "string"
            },
            {
                scm_name = "default-korean-font",
                cpp_name = "default_korean_font_name",
                ret_type = "string"
            },
            {
                scm_name = "get-retina-factor",
                cpp_name = "get_retina_factor",
                ret_type = "int"
            },
            {
                scm_name = "get-retina-zoom",
                cpp_name = "get_retina_zoom",
                ret_type = "int"
            },
            {
                scm_name = "get-retina-icons",
                cpp_name = "get_retina_icons",
                ret_type = "int"
            },
            {
                scm_name = "get-retina-scale",
                cpp_name = "get_retina_scale",
                ret_type = "double"
            },
            {
                scm_name = "set-retina-factor",
                cpp_name = "set_retina_factor",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "set-retina-zoom",
                cpp_name = "set_retina_zoom",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "set-retina-icons",
                cpp_name = "set_retina_icons",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "set-retina-scale",
                cpp_name = "set_retina_scale",
                ret_type = "void",
                arg_list = {
                    "double"
                }
            },
            {
                scm_name = "tm-output",
                cpp_name = "tm_output",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tm-errput",
                cpp_name = "tm_errput",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "win32-display",
                cpp_name = "win32_display",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cpp-error",
                cpp_name = "cpp_error",
                ret_type = "void"
            },
            {
                scm_name = "supports-native-pdf?",
                cpp_name = "supports_native_pdf",
                ret_type = "bool"
            },
            {
                scm_name = "supports-ghostscript?",
                cpp_name = "supports_ghostscript",
                ret_type = "bool"
            },
            {
                scm_name = "rescue-mode?",
                cpp_name = "in_rescue_mode",
                ret_type = "bool"
            },
            {
                scm_name = "scheme-dialect",
                cpp_name = "scheme_dialect",
                ret_type = "string"
            },
            {
                scm_name = "get-texmacs-path",
                cpp_name = "get_texmacs_path",
                ret_type = "url"
            },
            {
                scm_name = "get-texmacs-home-path",
                cpp_name = "get_texmacs_home_path",
                ret_type = "url"
            },
            {
                scm_name = "get-user-login",
                cpp_name = "get_user_login",
                ret_type = "string"
            },
            {
                scm_name = "get-user-name",
                cpp_name = "get_user_name",
                ret_type = "string"
            },
            {
                scm_name = "plugin-list",
                cpp_name = "plugin_list",
                ret_type = "scheme_tree"
            },
            {
                scm_name = "set-fast-environments",
                cpp_name = "set_fast_environments",
                ret_type = "void",
                arg_list = {
                    "bool"
                }
            },
            {
                scm_name = "font-exists-in-tt?",
                cpp_name = "tt_font_exists",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "eval-system",
                cpp_name = "eval_system",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "var-eval-system",
                cpp_name = "var_eval_system",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "evaluate-system",
                cpp_name = "evaluate_system",
                ret_type = "array_string",
                arg_list = {
                    "array_string",
                    "array_int",
                    "array_string",
                    "array_int"
                }
            },
            {
                scm_name = "get-locale-language",
                cpp_name = "get_locale_language",
                ret_type = "string"
            },
            {
                scm_name = "get-locale-charset",
                cpp_name = "get_locale_charset",
                ret_type = "string"
            },
            {
                scm_name = "locale-to-language",
                cpp_name = "locale_to_language",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "language-to-locale",
                cpp_name = "language_to_locale",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "texmacs-time",
                cpp_name = "texmacs_time",
                ret_type = "int"
            },
            {
                scm_name = "pretty-time",
                cpp_name = "pretty_time",
                ret_type = "string",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "texmacs-memory",
                cpp_name = "mem_used",
                ret_type = "int"
            },
            {
                scm_name = "bench-print",
                cpp_name = "bench_print",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "bench-print-all",
                cpp_name = "bench_print",
                ret_type = "void"
            },
            {
                scm_name = "system-wait",
                cpp_name = "system_wait",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "get-show-kbd",
                cpp_name = "get_show_kbd",
                ret_type = "bool"
            },
            {
                scm_name = "set-show-kbd",
                cpp_name = "set_show_kbd",
                ret_type = "void",
                arg_list = {
                    "bool"
                }
            },
            {
                scm_name = "set-latex-command",
                cpp_name = "set_latex_command",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "set-bibtex-command",
                cpp_name = "set_bibtex_command",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "number-latex-errors",
                cpp_name = "number_latex_errors",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "number-latex-pages",
                cpp_name = "number_latex_pages",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "math-symbol-group",
                cpp_name = "math_symbol_group",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "math-group-members",
                cpp_name = "math_group_members",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "math-symbol-type",
                cpp_name = "math_symbol_type",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "object->command",
                cpp_name = "as_command",
                ret_type = "command",
                arg_list = {
                    "object"
                }
            },
            {
                scm_name = "exec-delayed",
                cpp_name = "exec_delayed",
                ret_type = "void",
                arg_list = {
                    "object"
                }
            },
            {
                scm_name = "exec-delayed-pause",
                cpp_name = "exec_delayed_pause",
                ret_type = "void",
                arg_list = {
                    "object"
                }
            },
            {
                scm_name = "protected-call",
                cpp_name = "protected_call",
                ret_type = "void",
                arg_list = {
                    "object"
                }
            },
            {
                scm_name = "notify-preferences-booted",
                cpp_name = "notify_preferences_booted",
                ret_type = "void"
            },
            {
                scm_name = "cpp-has-preference?",
                cpp_name = "has_user_preference",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cpp-get-preference",
                cpp_name = "get_user_preference",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "cpp-set-preference",
                cpp_name = "set_user_preference",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "cpp-reset-preference",
                cpp_name = "reset_user_preference",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "save-preferences",
                cpp_name = "save_user_preferences",
                ret_type = "void"
            },
            {
                scm_name = "get-default-printing-command",
                cpp_name = "get_printing_default",
                ret_type = "string"
            },
            {
                scm_name = "set-input-language",
                cpp_name = "set_input_language",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "get-input-language",
                cpp_name = "get_input_language",
                ret_type = "string"
            },
            {
                scm_name = "set-output-language",
                cpp_name = "gui_set_output_language",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "get-output-language",
                cpp_name = "get_output_language",
                ret_type = "string"
            },
            {
                scm_name = "translate",
                cpp_name = "translate",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "string-translate",
                cpp_name = "translate_as_is",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "translate-from-to",
                cpp_name = "translate",
                ret_type = "string",
                arg_list = {
                    "content",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "tree-translate",
                cpp_name = "tree_translate",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-translate-from-to",
                cpp_name = "tree_translate",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "force-load-translations",
                cpp_name = "force_load_dictionary",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "color",
                cpp_name = "named_color",
                ret_type = "int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "get-hex-color",
                cpp_name = "get_hex_color",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "named-color->xcolormap",
                cpp_name = "named_color_to_xcolormap",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "rgba->named-color",
                cpp_name = "named_rgb_color",
                ret_type = "string",
                arg_list = {
                    "array_int"
                }
            },
            {
                scm_name = "named-color->rgba",
                cpp_name = "get_named_rgb_color",
                ret_type = "array_int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "new-author",
                cpp_name = "new_author",
                ret_type = "double"
            },
            {
                scm_name = "set-author",
                cpp_name = "set_author",
                ret_type = "void",
                arg_list = {
                    "double"
                }
            },
            {
                scm_name = "get-author",
                cpp_name = "get_author",
                ret_type = "double"
            },
            {
                scm_name = "debug-set",
                cpp_name = "debug_set",
                ret_type = "void",
                arg_list = {
                    "string",
                    "bool"
                }
            },
            {
                scm_name = "debug-get",
                cpp_name = "debug_get",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "debug-message",
                cpp_name = "debug_message",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "get-debug-messages",
                cpp_name = "get_debug_messages",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "clear-debug-messages",
                cpp_name = "clear_debug_messages",
                ret_type = "void"
            },
            {
                scm_name = "cout-buffer",
                cpp_name = "cout_buffer",
                ret_type = "void"
            },
            {
                scm_name = "cout-unbuffer",
                cpp_name = "cout_unbuffer",
                ret_type = "string"
            },
            {
                scm_name = "mark-new",
                cpp_name = "new_marker",
                ret_type = "double"
            },
            {
                scm_name = "glyph-register",
                cpp_name = "register_glyph",
                ret_type = "void",
                arg_list = {
                    "string",
                    "array_array_array_double"
                }
            },
            {
                scm_name = "glyph-recognize",
                cpp_name = "recognize_glyph",
                ret_type = "string",
                arg_list = {
                    "array_array_array_double"
                }
            },
            {
                scm_name = "set-new-fonts",
                cpp_name = "set_new_fonts",
                ret_type = "void",
                arg_list = {
                    "bool"
                }
            },
            {
                scm_name = "new-fonts?",
                cpp_name = "get_new_fonts",
                ret_type = "bool"
            },
            {
                scm_name = "tmtm-eqnumber->nonumber",
                cpp_name = "eqnumber_to_nonumber",
                ret_type = "tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "busy-versioning?",
                cpp_name = "is_busy_versioning",
                ret_type = "bool"
            },
            {
                scm_name = "players-set-elapsed",
                cpp_name = "players_set_elapsed",
                ret_type = "void",
                arg_list = {
                    "tree",
                    "double"
                }
            },
            {
                scm_name = "players-set-speed",
                cpp_name = "players_set_speed",
                ret_type = "void",
                arg_list = {
                    "tree",
                    "double"
                }
            },
            {
                scm_name = "apply-effect",
                cpp_name = "apply_effect",
                ret_type = "void",
                arg_list = {
                    "content",
                    "array_url",
                    "url",
                    "int",
                    "int"
                }
            },
              
              -- routines for the font database
            {
                scm_name = "tt-exists?",
                cpp_name = "tt_font_exists",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tt-dump",
                cpp_name = "tt_dump",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "tt-font-name",
                cpp_name = "tt_font_name",
                ret_type = "scheme_tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "tt-analyze",
                cpp_name = "tt_analyze",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-database-build",
                cpp_name = "font_database_build",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "font-database-build-local",
                cpp_name = "font_database_build_local",
                ret_type = "void"
            },
            {
                scm_name = "font-database-extend-local",
                cpp_name = "font_database_extend_local",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "font-database-build-global",
                cpp_name = "font_database_build_global",
                ret_type = "void"
            },
            {
                scm_name = "font-database-build-characteristics",
                cpp_name = "font_database_build_characteristics",
                ret_type = "void",
                arg_list = {
                    "bool"
                }
            },
            {
                scm_name = "font-database-insert-global",
                cpp_name = "font_database_build_global",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "font-database-save-local-delta",
                cpp_name = "font_database_save_local_delta",
                ret_type = "void"
            },
            {
                scm_name = "font-database-load",
                cpp_name = "font_database_load",
                ret_type = "void"
            },
            {
                scm_name = "font-database-save",
                cpp_name = "font_database_save",
                ret_type = "void"
            },
            {
                scm_name = "font-database-filter",
                cpp_name = "font_database_filter",
                ret_type = "void"
            },
            {
                scm_name = "font-database-families",
                cpp_name = "font_database_families",
                ret_type = "array_string"
            },
            {
                scm_name = "font-database-delta-families",
                cpp_name = "font_database_delta_families",
                ret_type = "array_string"
            },
            {
                scm_name = "font-database-styles",
                cpp_name = "font_database_styles",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-database-search",
                cpp_name = "font_database_search",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "font-database-characteristics",
                cpp_name = "font_database_characteristics",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "font-database-substitutions",
                cpp_name = "font_database_substitutions",
                ret_type = "scheme_tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-family->master",
                cpp_name = "family_to_master",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-master->families",
                cpp_name = "master_to_families",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-master-features",
                cpp_name = "master_features",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-family-features",
                cpp_name = "family_features",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-family-strict-features",
                cpp_name = "family_strict_features",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-style-features",
                cpp_name = "style_features",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "font-guessed-features",
                cpp_name = "guessed_features",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "font-guessed-distance",
                cpp_name = "guessed_distance",
                ret_type = "double",
                arg_list = {
                    "string",
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "font-master-guessed-distance",
                cpp_name = "guessed_distance",
                ret_type = "double",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "font-family-guessed-features",
                cpp_name = "guessed_features",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "bool"
                }
            },
            {
                scm_name = "characteristic-distance",
                cpp_name = "characteristic_distance",
                ret_type = "double",
                arg_list = {
                    "array_string",
                    "array_string"
                }
            },
            {
                scm_name = "trace-distance",
                cpp_name = "trace_distance",
                ret_type = "double",
                arg_list = {
                    "string",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "logical-font-public",
                cpp_name = "logical_font",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "logical-font-exact",
                cpp_name = "logical_font_exact",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "logical-font-private",
                cpp_name = "logical_font",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "logical-font-family",
                cpp_name = "get_family",
                ret_type = "string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-variant",
                cpp_name = "get_variant",
                ret_type = "string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-series",
                cpp_name = "get_series",
                ret_type = "string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-shape",
                cpp_name = "get_shape",
                ret_type = "string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-search",
                cpp_name = "search_font",
                ret_type = "array_string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-search-exact",
                cpp_name = "search_font_exact",
                ret_type = "array_string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "search-font-families",
                cpp_name = "search_font_families",
                ret_type = "array_string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "search-font-styles",
                cpp_name = "search_font_styles",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-patch",
                cpp_name = "patch_font",
                ret_type = "array_string",
                arg_list = {
                    "array_string",
                    "array_string"
                }
            },
            {
                scm_name = "logical-font-substitute",
                cpp_name = "apply_substitutions",
                ret_type = "array_string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "font-family-main",
                cpp_name = "main_family",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            
              -- routines for images and animations
            {
                scm_name = "image->psdoc",
                cpp_name = "image_to_psdoc",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "anim-control-times",
                cpp_name = "get_control_times",
                ret_type = "array_double",
                arg_list = {
                    "content"
                }
            },
            
              -- routines for trees
            {
                scm_name = "tree->stree",
                cpp_name = "tree_to_scheme_tree",
                ret_type = "scheme_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "stree->tree",
                cpp_name = "scheme_tree_to_tree",
                ret_type = "tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "tree->string",
                cpp_name = "coerce_tree_string",
                ret_type = "string",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "string->tree",
                cpp_name = "coerce_string_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tm->tree",
                cpp_name = "tree",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-atomic?",
                cpp_name = "is_atomic",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-compound?",
                cpp_name = "is_compound",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-label",
                cpp_name = "L",
                ret_type = "tree_label",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-children",
                cpp_name = "A",
                ret_type = "array_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-arity",
                cpp_name = "N",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-child-ref",
                cpp_name = "tree_ref",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-child-set!",
                cpp_name = "tree_set",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-child-insert",
                cpp_name = "tree_child_insert",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-ip",
                cpp_name = "obtain_ip",
                ret_type = "path",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-active?",
                cpp_name = "tree_active",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-eq?",
                cpp_name = "strong_equal",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "tree"
                }
            },
            {
                scm_name = "subtree",
                cpp_name = "subtree",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "path"
                }
            },
            {
                scm_name = "tree-range",
                cpp_name = "tree_range",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "tree-copy",
                cpp_name = "copy",
                ret_type = "tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-append",
                cpp_name = "tree_append",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "tree"
                }
            },
            {
                scm_name = "tree-right-index",
                cpp_name = "right_index",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-label-extension?",
                cpp_name = "is_extension",
                ret_type = "bool",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-label-macro?",
                cpp_name = "is_macro",
                ret_type = "bool",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-label-parameter?",
                cpp_name = "is_parameter",
                ret_type = "bool",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-label-type",
                cpp_name = "get_tag_type",
                ret_type = "string",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tree-multi-paragraph?",
                cpp_name = "is_multi_paragraph",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-simplify",
                cpp_name = "simplify_correct",
                ret_type = "tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-minimal-arity",
                cpp_name = "minimal_arity",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-maximal-arity",
                cpp_name = "maximal_arity",
                ret_type = "int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-possible-arity?",
                cpp_name = "correct_arity",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-insert_point",
                cpp_name = "insert_point",
                ret_type = "int",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-is-dynamic?",
                cpp_name = "is_dynamic",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-accessible-child?",
                cpp_name = "is_accessible_child",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-accessible-children",
                cpp_name = "accessible_children",
                ret_type = "array_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-all-accessible?",
                cpp_name = "all_accessible",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-none-accessible?",
                cpp_name = "none_accessible",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-name",
                cpp_name = "get_name",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-long-name",
                cpp_name = "get_long_name",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-child-name",
                cpp_name = "get_child_name",
                ret_type = "string",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "tree-child-long-name",
                cpp_name = "get_child_long_name",
                ret_type = "string",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "tree-child-type",
                cpp_name = "get_child_type",
                ret_type = "string",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "tree-child-env*",
                cpp_name = "get_env_child",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-child-env",
                cpp_name = "get_env_child",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "tree-descendant-env*",
                cpp_name = "get_env_descendant",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "path",
                    "content"
                }
            },
            {
                scm_name = "tree-descendant-env",
                cpp_name = "get_env_descendant",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "path",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "tree-load-inclusion",
                cpp_name = "load_inclusion",
                ret_type = "tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "tree-as-string",
                cpp_name = "tree_as_string",
                ret_type = "string",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-extents",
                cpp_name = "tree_extents",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-empty?",
                cpp_name = "is_empty",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-multi-line?",
                cpp_name = "is_multi_line",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-is-buffer?",
                cpp_name = "admits_edit_observer",
                ret_type = "bool",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-search-sections",
                cpp_name = "search_sections",
                ret_type = "array_tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-search-tree",
                cpp_name = "search",
                ret_type = "array_path",
                arg_list = {
                    "content",
                    "content",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-search-tree-at",
                cpp_name = "search",
                ret_type = "array_path",
                arg_list = {
                    "content",
                    "content",
                    "path",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-spell",
                cpp_name = "spell",
                ret_type = "array_path",
                arg_list = {
                    "string",
                    "content",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-spell-at",
                cpp_name = "spell",
                ret_type = "array_path",
                arg_list = {
                    "string",
                    "content",
                    "path",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "tree-spell-selection",
                cpp_name = "spell",
                ret_type = "array_path",
                arg_list = {
                    "string",
                    "content",
                    "path",
                    "path",
                    "path",
                    "int"
                }
            },
            {
                scm_name = "previous-search-hit",
                cpp_name = "previous_search_hit",
                ret_type = "array_path",
                arg_list = {
                    "array_path",
                    "path",
                    "bool"
                }
            },
            {
                scm_name = "next-search-hit",
                cpp_name = "next_search_hit",
                ret_type = "array_path",
                arg_list = {
                    "array_path",
                    "path",
                    "bool"
                }
            },
            {
                scm_name = "navigate-search-hit",
                cpp_name = "navigate_search_hit",
                ret_type = "array_path",
                arg_list = {
                    "path",
                    "bool",
                    "bool",
                    "bool"
                }
            },
            {
                scm_name = "tag-minimal-arity",
                cpp_name = "minimal_arity",
                ret_type = "int",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tag-maximal-arity",
                cpp_name = "maximal_arity",
                ret_type = "int",
                arg_list = {
                    "tree_label"
                }
            },
            {
                scm_name = "tag-possible-arity?",
                cpp_name = "correct_arity",
                ret_type = "bool",
                arg_list = {
                    "tree_label",
                    "int"
                }
            },
            {
                scm_name = "set-access-mode",
                cpp_name = "set_access_mode",
                ret_type = "int",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "get-access-mode",
                cpp_name = "get_access_mode",
                ret_type = "int"
            },
            
            {
                scm_name = "tree-assign",
                cpp_name = "tree_assign",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "content"
                }
            },
            {
                scm_name = "tree-var-insert",
                cpp_name = "tree_insert",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-remove",
                cpp_name = "tree_remove",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "tree-split",
                cpp_name = "tree_split",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "tree-join",
                cpp_name = "tree_join",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            {
                scm_name = "tree-assign-node",
                cpp_name = "tree_assign_node",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "tree_label"
                }
            },
            {
                scm_name = "tree-insert-node",
                cpp_name = "tree_insert_node",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "tree-remove-node",
                cpp_name = "tree_remove_node",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "int"
                }
            },
            
            {
                scm_name = "cpp-tree-correct-node",
                cpp_name = "correct_node",
                ret_type = "void",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "cpp-tree-correct-downwards",
                cpp_name = "correct_downwards",
                ret_type = "void",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "cpp-tree-correct-upwards",
                cpp_name = "correct_upwards",
                ret_type = "void",
                arg_list = {
                    "tree"
                }
            },
            
              -- extra routines for content
            {
                scm_name = "concat-tokenize-math",
                cpp_name = "concat_tokenize",
                ret_type = "array_tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "concat-decompose",
                cpp_name = "concat_decompose",
                ret_type = "array_tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "concat-recompose",
                cpp_name = "concat_recompose",
                ret_type = "tree",
                arg_list = {
                    "array_tree"
                }
            },
            {
                scm_name = "with-like?",
                cpp_name = "is_with_like",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "with-same-type?",
                cpp_name = "with_same_type",
                ret_type = "bool",
                arg_list = {
                    "content",
                    "content"
                }
            },
            {
                scm_name = "with-similar-type?",
                cpp_name = "with_similar_type",
                ret_type = "bool",
                arg_list = {
                    "content",
                    "content"
                }
            },
            {
                scm_name = "with-correct",
                cpp_name = "with_correct",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "with-correct-superfluous",
                cpp_name = "superfluous_with_correct",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "invisible-correct-superfluous",
                cpp_name = "superfluous_invisible_correct",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "invisible-correct-missing",
                cpp_name = "missing_invisible_correct",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "int"
                }
            },
            {
                scm_name = "automatic-correct",
                cpp_name = "automatic_correct",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "string"
                }
            },
            {
                scm_name = "manual-correct",
                cpp_name = "manual_correct",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-upgrade-brackets",
                cpp_name = "upgrade_brackets",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "string"
                }
            },
            {
                scm_name = "tree-upgrade-big",
                cpp_name = "upgrade_big",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree-downgrade-brackets",
                cpp_name = "downgrade_brackets",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "bool",
                    "bool"
                }
            },
            {
                scm_name = "tree-downgrade-big",
                cpp_name = "downgrade_big",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "math-status-print",
                cpp_name = "math_status_print",
                ret_type = "void"
            },
            {
                scm_name = "math-status-reset",
                cpp_name = "math_status_reset",
                ret_type = "void"
            },
            {
                scm_name = "math-stats-compile",
                cpp_name = "compile_stats",
                ret_type = "void",
                arg_list = {
                    "string",
                    "content",
                    "string"
                }
            },
            {
                scm_name = "math-stats-occurrences",
                cpp_name = "number_occurrences",
                ret_type = "int",
                arg_list = {
                    "string",
                    "content"
                }
            },
            {
                scm_name = "math-stats-number-in-role",
                cpp_name = "number_in_role",
                ret_type = "int",
                arg_list = {
                    "string",
                    "content"
                }
            },
            
              -- paths
            {
                scm_name = "path-strip",
                cpp_name = "strip",
                ret_type = "path",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-inf?",
                cpp_name = "path_inf",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-inf-eq?",
                cpp_name = "path_inf_eq",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-less?",
                cpp_name = "path_less",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-less-eq?",
                cpp_name = "path_less_eq",
                ret_type = "bool",
                arg_list = {
                    "path",
                    "path"
                }
            },
            {
                scm_name = "path-start",
                cpp_name = "start",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-end",
                cpp_name = "end",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next",
                cpp_name = "next_valid",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous",
                cpp_name = "previous_valid",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next-word",
                cpp_name = "next_word",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-word",
                cpp_name = "previous_word",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next-node",
                cpp_name = "next_node",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-node",
                cpp_name = "previous_node",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-next-tag",
                cpp_name = "next_tag",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-previous-tag",
                cpp_name = "previous_tag",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-next-tag-same-argument",
                cpp_name = "next_tag_same_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-previous-tag-same-argument",
                cpp_name = "previous_tag_same_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path",
                    "scheme_tree"
                }
            },
            {
                scm_name = "path-next-argument",
                cpp_name = "next_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-argument",
                cpp_name = "previous_argument",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            {
                scm_name = "path-previous-section",
                cpp_name = "previous_section",
                ret_type = "path",
                arg_list = {
                    "content",
                    "path"
                }
            },
            
              -- modifications on trees
            {
                scm_name = "make-modification",
                cpp_name = "make_modification",
                ret_type = "modification",
                arg_list = {
                    "string",
                    "path",
                    "content"
                }
            },
            {
                scm_name = "modification-assign",
                cpp_name = "mod_assign",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "content"
                }
            },
            {
                scm_name = "modification-insert",
                cpp_name = "mod_insert",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "modification-remove",
                cpp_name = "mod_remove",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "modification-split",
                cpp_name = "mod_split",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "modification-join",
                cpp_name = "mod_join",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int"
                }
            },
            {
                scm_name = "modification-assign-node",
                cpp_name = "mod_assign_node",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "tree_label"
                }
            },
            {
                scm_name = "modification-insert-node",
                cpp_name = "mod_insert_node",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "modification-remove-node",
                cpp_name = "mod_remove_node",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int"
                }
            },
            {
                scm_name = "modification-set-cursor",
                cpp_name = "mod_set_cursor",
                ret_type = "modification",
                arg_list = {
                    "path",
                    "int",
                    "content"
                }
            },
            {
                scm_name = "modification-kind",
                cpp_name = "get_type",
                ret_type = "string",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-path",
                cpp_name = "get_path",
                ret_type = "path",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-tree",
                cpp_name = "get_tree",
                ret_type = "tree",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-root",
                cpp_name = "root",
                ret_type = "path",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-index",
                cpp_name = "index",
                ret_type = "int",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-argument",
                cpp_name = "argument",
                ret_type = "int",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-label",
                cpp_name = "L",
                ret_type = "tree_label",
                arg_list = {
                    "modification"
                }
            },
            
            {
                scm_name = "modification-copy",
                cpp_name = "copy",
                ret_type = "modification",
                arg_list = {
                    "modification"
                }
            },
            {
                scm_name = "modification-applicable?",
                cpp_name = "is_applicable",
                ret_type = "bool",
                arg_list = {
                    "content",
                    "modification"
                }
            },
            {
                scm_name = "modification-apply",
                cpp_name = "var_clean_apply",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "modification"
                }
            },
            {
                scm_name = "modification-inplace-apply",
                cpp_name = "var_apply",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "modification"
                }
            },
            {
                scm_name = "modification-invert",
                cpp_name = "invert",
                ret_type = "modification",
                arg_list = {
                    "modification",
                    "content"
                }
            },
            {
                scm_name = "modification-commute?",
                cpp_name = "commute",
                ret_type = "bool",
                arg_list = {
                    "modification",
                    "modification"
                }
            },
            {
                scm_name = "modification-can-pull?",
                cpp_name = "can_pull",
                ret_type = "bool",
                arg_list = {
                    "modification",
                    "modification"
                }
            },
            {
                scm_name = "modification-pull",
                cpp_name = "pull",
                ret_type = "modification",
                arg_list = {
                    "modification",
                    "modification"
                }
            },
            {
                scm_name = "modification-co-pull",
                cpp_name = "co_pull",
                ret_type = "modification",
                arg_list = {
                    "modification",
                    "modification"
                }
            },
            
              -- patches
            {
                scm_name = "patch-pair",
                cpp_name = "patch",
                ret_type = "patch",
                arg_list = {
                    "modification",
                    "modification"
                }
            },
            {
                scm_name = "patch-compound",
                cpp_name = "patch",
                ret_type = "patch",
                arg_list = {
                    "array_patch"
                }
            },
            {
                scm_name = "patch-branch",
                cpp_name = "branch_patch",
                ret_type = "patch",
                arg_list = {
                    "array_patch"
                }
            },
            {
                scm_name = "patch-birth",
                cpp_name = "patch",
                ret_type = "patch",
                arg_list = {
                    "double",
                    "bool"
                }
            },
            {
                scm_name = "patch-author",
                cpp_name = "patch",
                ret_type = "patch",
                arg_list = {
                    "double",
                    "patch"
                }
            },
            {
                scm_name = "patch-pair?",
                cpp_name = "is_modification",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-compound?",
                cpp_name = "is_compound",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-branch?",
                cpp_name = "is_branch",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-birth?",
                cpp_name = "is_birth",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-author?",
                cpp_name = "is_author",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-arity",
                cpp_name = "N",
                ret_type = "int",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-ref",
                cpp_name = "access",
                ret_type = "patch",
                arg_list = {
                    "patch",
                    "int"
                }
            },
            {
                scm_name = "patch-direct",
                cpp_name = "get_modification",
                ret_type = "modification",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-inverse",
                cpp_name = "get_inverse",
                ret_type = "modification",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-get-birth",
                cpp_name = "get_birth",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-get-author",
                cpp_name = "get_author",
                ret_type = "double",
                arg_list = {
                    "patch"
                }
            },
            
            {
                scm_name = "patch-copy",
                cpp_name = "copy",
                ret_type = "patch",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-applicable?",
                cpp_name = "is_applicable",
                ret_type = "bool",
                arg_list = {
                    "patch",
                    "content"
                }
            },
            {
                scm_name = "patch-apply",
                cpp_name = "var_clean_apply",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "patch"
                }
            },
            {
                scm_name = "patch-inplace-apply",
                cpp_name = "var_apply",
                ret_type = "tree",
                arg_list = {
                    "tree",
                    "patch"
                }
            },
            {
                scm_name = "patch-compactify",
                cpp_name = "compactify",
                ret_type = "patch",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-cursor-hint",
                cpp_name = "cursor_hint",
                ret_type = "path",
                arg_list = {
                    "patch",
                    "content"
                }
            },
            {
                scm_name = "patch-invert",
                cpp_name = "invert",
                ret_type = "patch",
                arg_list = {
                    "patch",
                    "content"
                }
            },
            {
                scm_name = "patch-commute?",
                cpp_name = "commute",
                ret_type = "bool",
                arg_list = {
                    "patch",
                    "patch"
                }
            },
            {
                scm_name = "patch-can-pull?",
                cpp_name = "can_pull",
                ret_type = "bool",
                arg_list = {
                    "patch",
                    "patch"
                }
            },
            {
                scm_name = "patch-pull",
                cpp_name = "pull",
                ret_type = "patch",
                arg_list = {
                    "patch",
                    "patch"
                }
            },
            {
                scm_name = "patch-co-pull",
                cpp_name = "co_pull",
                ret_type = "patch",
                arg_list = {
                    "patch",
                    "patch"
                }
            },
            {
                scm_name = "patch-remove-set-cursor",
                cpp_name = "remove_set_cursor",
                ret_type = "patch",
                arg_list = {
                    "patch"
                }
            },
            {
                scm_name = "patch-modifies?",
                cpp_name = "does_modify",
                ret_type = "bool",
                arg_list = {
                    "patch"
                }
            },
            
              -- links
            {
                scm_name = "tree->ids",
                cpp_name = "get_ids",
                ret_type = "list_string",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "id->trees",
                cpp_name = "get_trees",
                ret_type = "list_tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "vertex->links",
                cpp_name = "get_links",
                ret_type = "list_tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "tree->tree-pointer",
                cpp_name = "tree_pointer_new",
                ret_type = "observer",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "tree-pointer-detach",
                cpp_name = "tree_pointer_delete",
                ret_type = "void",
                arg_list = {
                    "observer"
                }
            },
            {
                scm_name = "tree-pointer->tree",
                cpp_name = "obtain_tree",
                ret_type = "tree",
                arg_list = {
                    "observer"
                }
            },
            {
                scm_name = "current-link-types",
                cpp_name = "all_link_types",
                ret_type = "list_string"
            },
            {
                scm_name = "get-locus-rendering",
                cpp_name = "get_locus_rendering",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "set-locus-rendering",
                cpp_name = "set_locus_rendering",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "declare-visited",
                cpp_name = "declare_visited",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "has-been-visited?",
                cpp_name = "has_been_visited",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            
            {
                scm_name = "graphics-set",
                cpp_name = "set_graphical_value",
                ret_type = "void",
                arg_list = {
                    "content",
                    "content"
                }
            },
            {
                scm_name = "graphics-has?",
                cpp_name = "has_graphical_value",
                ret_type = "bool",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "graphics-ref",
                cpp_name = "get_graphical_value",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "graphics-needs-update?",
                cpp_name = "graphics_needs_update",
                ret_type = "bool"
            },
            {
                scm_name = "graphics-notify-update",
                cpp_name = "graphics_notify_update",
                ret_type = "void",
                arg_list = {
                    "content"
                }
            },
            
              -- routines for strings
            {
                scm_name = "cpp-string-number?",
                cpp_name = "is_double",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-occurs?",
                cpp_name = "occurs",
                ret_type = "bool",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-count-occurrences",
                cpp_name = "count_occurrences",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-search-forwards",
                cpp_name = "search_forwards",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int",
                    "string"
                }
            },
            {
                scm_name = "string-search-backwards",
                cpp_name = "search_backwards",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int",
                    "string"
                }
            },
            {
                scm_name = "string-overlapping",
                cpp_name = "overlapping",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-replace",
                cpp_name = "replace",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-find-non-alpha",
                cpp_name = "find_non_alpha",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int",
                    "bool"
                }
            },
            {
                scm_name = "string-alpha?",
                cpp_name = "is_alpha",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-locase-alpha?",
                cpp_name = "is_locase_alpha",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "upcase-first",
                cpp_name = "upcase_first",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "locase-first",
                cpp_name = "locase_first",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "upcase-all",
                cpp_name = "upcase_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "locase-all",
                cpp_name = "locase_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-union",
                cpp_name = "string_union",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-minus",
                cpp_name = "string_minus",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "escape-generic",
                cpp_name = "escape_generic",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "escape-verbatim",
                cpp_name = "escape_verbatim",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "escape-shell",
                cpp_name = "escape_sh",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "escape-to-ascii",
                cpp_name = "cork_to_ascii",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "unescape-guile",
                cpp_name = "unescape_guile",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-quote",
                cpp_name = "scm_quote",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-unquote",
                cpp_name = "scm_unquote",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-trim-spaces-left",
                cpp_name = "trim_spaces_left",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-trim-spaces-right",
                cpp_name = "trim_spaces_right",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-trim-spaces",
                cpp_name = "trim_spaces",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "downgrade-math-letters",
                cpp_name = "downgrade_math_letters",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string-convert",
                cpp_name = "convert",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "encode-base64",
                cpp_name = "encode_base64",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "decode-base64",
                cpp_name = "decode_base64",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "sourcecode->cork",
                cpp_name = "sourcecode_to_cork",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cork->sourcecode",
                cpp_name = "cork_to_sourcecode",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "utf8->cork",
                cpp_name = "utf8_to_cork",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cork->utf8",
                cpp_name = "cork_to_utf8",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "utf8->t2a",
                cpp_name = "utf8_to_t2a",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "t2a->utf8",
                cpp_name = "t2a_to_utf8",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "utf8->html",
                cpp_name = "utf8_to_html",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "guess-wencoding",
                cpp_name = "guess_wencoding",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tm->xml-name",
                cpp_name = "tm_to_xml_name",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "old-tm->xml-cdata",
                cpp_name = "old_tm_to_xml_cdata",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tm->xml-cdata",
                cpp_name = "tm_to_xml_cdata",
                ret_type = "object",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "xml-name->tm",
                cpp_name = "xml_name_to_tm",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "old-xml-cdata->tm",
                cpp_name = "old_xml_cdata_to_tm",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "xml-unspace",
                cpp_name = "xml_unspace",
                ret_type = "string",
                arg_list = {
                    "string",
                    "bool",
                    "bool"
                }
            },
            {
                scm_name = "integer->hexadecimal",
                cpp_name = "as_hexadecimal",
                ret_type = "string",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "integer->padded-hexadecimal",
                cpp_name = "as_hexadecimal",
                ret_type = "string",
                arg_list = {
                    "int",
                    "int"
                }
            },
            {
                scm_name = "hexadecimal->integer",
                cpp_name = "from_hexadecimal",
                ret_type = "int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cpp-string-tokenize",
                cpp_name = "tokenize",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "cpp-string-recompose",
                cpp_name = "recompose",
                ret_type = "string",
                arg_list = {
                    "array_string",
                    "string"
                }
            },
            {
                scm_name = "string-differences",
                cpp_name = "differences",
                ret_type = "array_int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "string-distance",
                cpp_name = "distance",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
              
            -- Program bracket matching
            {
                scm_name = "find-left-bracket",
                cpp_name = "find_left_bracket",
                ret_type = "path",
                arg_list = {
                    "path",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "find-right-bracket",
                cpp_name = "find_right_bracket",
                ret_type = "path",
                arg_list = {
                    "path",
                    "string",
                    "string"
                }
            },
            
              -- routines for strings in the TeXmacs encoding
            {
                scm_name = "string->tmstring",
                cpp_name = "tm_encode",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring->string",
                cpp_name = "tm_decode",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-length",
                cpp_name = "tm_string_length",
                ret_type = "int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-ref",
                cpp_name = "tm_forward_access",
                ret_type = "string",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "tmstring-reverse-ref",
                cpp_name = "tm_backward_access",
                ret_type = "string",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "tmstring->list",
                cpp_name = "tm_tokenize",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "list->tmstring",
                cpp_name = "tm_recompose",
                ret_type = "string",
                arg_list = {
                    "array_string"
                }
            },
            {
                scm_name = "string-next",
                cpp_name = "tm_char_next",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "string-previous",
                cpp_name = "tm_char_previous",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "tmstring-split",
                cpp_name = "tm_string_split",
                ret_type = "array_string",
                arg_list = {
                    "string"
                }
            },
            
            {
                scm_name = "tmstring-translit",
                cpp_name = "uni_translit",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-locase-first",
                cpp_name = "uni_locase_first",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-upcase-first",
                cpp_name = "uni_upcase_first",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-locase-all",
                cpp_name = "uni_locase_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-upcase-all",
                cpp_name = "uni_upcase_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-unaccent-all",
                cpp_name = "uni_unaccent_all",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-letter?",
                cpp_name = "uni_is_letter",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tmstring-before?",
                cpp_name = "uni_before",
                ret_type = "bool",
                arg_list = {
                    "string",
                    "string"
                }
            },
            
              -- Spell checking
            {
                scm_name = "multi-spell-start",
                cpp_name = "spell_start",
                ret_type = "void"
            },
            {
                scm_name = "multi-spell-done",
                cpp_name = "spell_done",
                ret_type = "void"
            },
            {
                scm_name = "single-spell-start",
                cpp_name = "spell_start",
                ret_type = "string",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "single-spell-done",
                cpp_name = "spell_done",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "spell-check",
                cpp_name = "spell_check",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "spell-check?",
                cpp_name = "check_word",
                ret_type = "bool",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "spell-accept",
                cpp_name = "spell_accept",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "spell-var-accept",
                cpp_name = "spell_accept",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string",
                    "bool"
                }
            },
            {
                scm_name = "spell-insert",
                cpp_name = "spell_insert",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            
              -- Packrat grammar and parsing tools
            {
                scm_name = "packrat-define",
                cpp_name = "packrat_define",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string",
                    "tree"
                }
            },
            {
                scm_name = "packrat-property",
                cpp_name = "packrat_property",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "packrat-inherit",
                cpp_name = "packrat_inherit",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "packrat-parse",
                cpp_name = "packrat_parse",
                ret_type = "path",
                arg_list = {
                    "string",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "packrat-correct?",
                cpp_name = "packrat_correct",
                ret_type = "bool",
                arg_list = {
                    "string",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "packrat-context",
                cpp_name = "packrat_context",
                ret_type = "object",
                arg_list = {
                    "string",
                    "string",
                    "content",
                    "path"
                }
            },
            {
                scm_name = "syntax-read-preferences",
                cpp_name = "initialize_color_decodings",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
              
              -- further conversion routines for trees and strings
            {
                scm_name = "parse-texmacs",
                cpp_name = "texmacs_document_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "serialize-texmacs",
                cpp_name = "tree_to_texmacs",
                ret_type = "string",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "parse-texmacs-snippet",
                cpp_name = "texmacs_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "serialize-texmacs-snippet",
                cpp_name = "tree_to_texmacs",
                ret_type = "string",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "texmacs->stm",
                cpp_name = "tree_to_scheme",
                ret_type = "string",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "stm->texmacs",
                cpp_name = "scheme_document_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "stm-snippet->texmacs",
                cpp_name = "scheme_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "cpp-texmacs->verbatim",
                cpp_name = "tree_to_verbatim",
                ret_type = "string",
                arg_list = {
                    "tree",
                    "bool",
                    "string"
                }
            },
            {
                scm_name = "cpp-verbatim-snippet->texmacs",
                cpp_name = "verbatim_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "bool",
                    "string"
                }
            },
            {
                scm_name = "cpp-verbatim->texmacs",
                cpp_name = "verbatim_document_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "bool",
                    "string"
                }
            },
            {
                scm_name = "parse-latex",
                cpp_name = "parse_latex",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "parse-latex-document",
                cpp_name = "parse_latex_document",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "latex->texmacs",
                cpp_name = "latex_to_tree",
                ret_type = "tree",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "cpp-latex-document->texmacs",
                cpp_name = "latex_document_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "bool"
                }
            },
            {
                scm_name = "latex-class-document->texmacs",
                cpp_name = "latex_class_document_to_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "tracked-latex->texmacs",
                cpp_name = "tracked_latex_to_texmacs",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "bool"
                }
            },
            {
                scm_name = "conservative-texmacs->latex",
                cpp_name = "conservative_texmacs_to_latex",
                ret_type = "string",
                arg_list = {
                    "content",
                    "object"
                }
            },
            {
                scm_name = "tracked-texmacs->latex",
                cpp_name = "tracked_texmacs_to_latex",
                ret_type = "string",
                arg_list = {
                    "content",
                    "object"
                }
            },
            {
                scm_name = "conservative-latex->texmacs",
                cpp_name = "conservative_latex_to_texmacs",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "bool"
                }
            },
            {
                scm_name = "get-line-number",
                cpp_name = "get_line_number",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "get-column-number",
                cpp_name = "get_column_number",
                ret_type = "int",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "try-latex-export",
                cpp_name = "try_latex_export",
                ret_type = "tree",
                arg_list = {
                    "content",
                    "object",
                    "url",
                    "url"
                }
            },
            {
                scm_name = "parse-xml",
                cpp_name = "parse_xml",
                ret_type = "scheme_tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "parse-html",
                cpp_name = "parse_html",
                ret_type = "scheme_tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "parse-bib",
                cpp_name = "parse_bib",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "conservative-bib-import",
                cpp_name = "conservative_bib_import",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "content",
                    "string"
                }
            },
            {
                scm_name = "conservative-bib-export",
                cpp_name = "conservative_bib_export",
                ret_type = "string",
                arg_list = {
                    "content",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "clean-html",
                cpp_name = "clean_html",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "upgrade-tmml",
                cpp_name = "tmml_upgrade",
                ret_type = "tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "upgrade-mathml",
                cpp_name = "upgrade_mathml",
                ret_type = "tree",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "retrieve-mathjax",
                cpp_name = "retrieve_mathjax",
                ret_type = "tree",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "compute-keys-string",
                cpp_name = "compute_keys",
                ret_type = "array_string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "compute-keys-tree",
                cpp_name = "compute_keys",
                ret_type = "array_string",
                arg_list = {
                    "content",
                    "string"
                }
            },
            {
                scm_name = "compute-keys-url",
                cpp_name = "compute_keys",
                ret_type = "array_string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "compute-index-string",
                cpp_name = "compute_index",
                ret_type = "scheme_tree",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "compute-index-tree",
                cpp_name = "compute_index",
                ret_type = "scheme_tree",
                arg_list = {
                    "content",
                    "string"
                }
            },
            {
                scm_name = "compute-index-url",
                cpp_name = "compute_index",
                ret_type = "scheme_tree",
                arg_list = {
                    "url"
                }
            },
            
              -- routines for urls
            {
                scm_name = "url->url",
                cpp_name = "url",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "root->url",
                cpp_name = "url_root",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "string->url",
                cpp_name = "url",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url->string",
                cpp_name = "as_string",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url->stree",
                cpp_name = "as_tree",
                ret_type = "scheme_tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system->url",
                cpp_name = "url_system",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url->system",
                cpp_name = "as_system_string",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "unix->url",
                cpp_name = "url_unix",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url->unix",
                cpp_name = "as_unix_string",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-unix",
                cpp_name = "url",
                ret_type = "url",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "url-none",
                cpp_name = "url_none",
                ret_type = "url"
            },
            {
                scm_name = "url-any",
                cpp_name = "url_wildcard",
                ret_type = "url"
            },
            {
                scm_name = "url-wildcard",
                cpp_name = "url_wildcard",
                ret_type = "url",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "url-pwd",
                cpp_name = "url_pwd",
                ret_type = "url"
            },
            {
                scm_name = "url-parent",
                cpp_name = "url_parent",
                ret_type = "url"
            },
            {
                scm_name = "url-ancestor",
                cpp_name = "url_ancestor",
                ret_type = "url"
            },
            {
                scm_name = "url-append",
                cpp_name = "url_concat",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-or",
                cpp_name = "url_or",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-none?",
                cpp_name = "is_none",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted?",
                cpp_name = "is_rooted",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted-protocol?",
                cpp_name = "is_rooted",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-rooted-web?",
                cpp_name = "is_rooted_web",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted-tmfs?",
                cpp_name = "is_rooted_tmfs",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-rooted-tmfs-protocol?",
                cpp_name = "is_rooted_tmfs",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-root",
                cpp_name = "get_root",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-unroot",
                cpp_name = "unroot",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-atomic?",
                cpp_name = "is_atomic",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-concat?",
                cpp_name = "is_concat",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-or?",
                cpp_name = "is_or",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-ref",
                cpp_name = "url_ref",
                ret_type = "url",
                arg_list = {
                    "url",
                    "int"
                }
            },
            {
                scm_name = "url-head",
                cpp_name = "head",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-tail",
                cpp_name = "tail",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-format",
                cpp_name = "file_format",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-suffix",
                cpp_name = "suffix",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-basename",
                cpp_name = "basename",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-glue",
                cpp_name = "glue",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-unglue",
                cpp_name = "unglue",
                ret_type = "url",
                arg_list = {
                    "url",
                    "int"
                }
            },
            {
                scm_name = "url-relative",
                cpp_name = "relative",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-expand",
                cpp_name = "expand",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-factor",
                cpp_name = "factor",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-delta",
                cpp_name = "delta",
                ret_type = "url",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-secure?",
                cpp_name = "is_secure",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-descends?",
                cpp_name = "descends",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "url"
                }
            },
            
            {
                scm_name = "url-complete",
                cpp_name = "complete",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-resolve",
                cpp_name = "resolve",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-resolve-in-path",
                cpp_name = "resolve_in_path",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-resolve-pattern",
                cpp_name = "resolve_pattern",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-exists?",
                cpp_name = "exists",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-exists-in-path?",
                cpp_name = "exists_in_path",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-exists-in-tex?",
                cpp_name = "exists_in_tex",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-concretize*",
                cpp_name = "concretize_url",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-concretize",
                cpp_name = "concretize",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-sys-concretize",
                cpp_name = "sys_concretize",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-materialize",
                cpp_name = "materialize",
                ret_type = "string",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-test?",
                cpp_name = "is_of_type",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "url-regular?",
                cpp_name = "is_regular",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-directory?",
                cpp_name = "is_directory",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-link?",
                cpp_name = "is_symbolic_link",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-newer?",
                cpp_name = "is_newer",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "url-size",
                cpp_name = "file_size",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-last-modified",
                cpp_name = "last_modified",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-temp",
                cpp_name = "url_temp",
                ret_type = "url"
            },
            {
                scm_name = "url-scratch",
                cpp_name = "url_scratch",
                ret_type = "url",
                arg_list = {
                    "string",
                    "string",
                    "int"
                }
            },
            {
                scm_name = "url-scratch?",
                cpp_name = "is_scratch",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-cache-invalidate",
                cpp_name = "web_cache_invalidate",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "string-save",
                cpp_name = "string_save",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url"
                }
            },
            {
                scm_name = "string-load",
                cpp_name = "string_load",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "string-append-to-file",
                cpp_name = "string_append_to_file",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url"
                }
            },
            {
                scm_name = "system-move",
                cpp_name = "move",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "system-copy",
                cpp_name = "copy",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "system-remove",
                cpp_name = "remove",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-mkdir",
                cpp_name = "mkdir",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-rmdir",
                cpp_name = "rmdir",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "system-setenv",
                cpp_name = "set_env",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "system-search-score",
                cpp_name = "search_score",
                ret_type = "int",
                arg_list = {
                    "url",
                    "array_string"
                }
            },
            {
                scm_name = "system-1",
                cpp_name = "system",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url"
                }
            },
            {
                scm_name = "system-2",
                cpp_name = "system",
                ret_type = "void",
                arg_list = {
                    "string",
                    "url",
                    "url"
                }
            },
            {
                scm_name = "system-url->string",
                cpp_name = "sys_concretize",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "url-grep",
                cpp_name = "grep",
                ret_type = "url",
                arg_list = {
                    "string",
                    "url"
                }
            },
            {
                scm_name = "url-search-upwards",
                cpp_name = "search_file_upwards",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string",
                    "array_string"
                }
            },
            {
                scm_name = "picture-cache-reset",
                cpp_name = "picture_cache_reset",
                ret_type = "void"
            },
            {
                scm_name = "set-file-focus",
                cpp_name = "set_file_focus",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "get-file-focus",
                cpp_name = "get_file_focus",
                ret_type = "url"
            },
            
              -- Persistent data
            {
                scm_name = "persistent-set",
                cpp_name = "persistent_set",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "persistent-remove",
                cpp_name = "persistent_reset",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "persistent-has?",
                cpp_name = "persistent_contains",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "persistent-get",
                cpp_name = "persistent_get",
                ret_type = "string",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "persistent-file-name",
                cpp_name = "persistent_file_name",
                ret_type = "url",
                arg_list = {
                    "url",
                    "string"
                }
            },
            
              -- native TeXmacs databases
            {
                scm_name = "tmdb-keep-history",
                cpp_name = "keep_history",
                ret_type = "void",
                arg_list = {
                    "url",
                    "bool"
                }
            },
            {
                scm_name = "tmdb-set-field",
                cpp_name = "set_field",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "string",
                    "array_string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-get-field",
                cpp_name = "get_field",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-remove-field",
                cpp_name = "remove_field",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-get-attributes",
                cpp_name = "get_attributes",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-set-entry",
                cpp_name = "set_entry",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "scheme_tree",
                    "double"
                }
            },
            {
                scm_name = "tmdb-get-entry",
                cpp_name = "get_entry",
                ret_type = "scheme_tree",
                arg_list = {
                    "url",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-remove-entry",
                cpp_name = "remove_entry",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string",
                    "double"
                }
            },
            {
                scm_name = "tmdb-query",
                cpp_name = "query",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "scheme_tree",
                    "double",
                    "int"
                }
            },
            {
                scm_name = "tmdb-inspect-history",
                cpp_name = "inspect_history",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tmdb-get-completions",
                cpp_name = "get_completions",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tmdb-get-name-completions",
                cpp_name = "get_name_completions",
                ret_type = "array_string",
                arg_list = {
                    "url",
                    "string"
                }
            },
            
              -- TeXmacs servers and clients
            {
                scm_name = "server-start",
                cpp_name = "server_start",
                ret_type = "void"
            },
            {
                scm_name = "server-stop",
                cpp_name = "server_stop",
                ret_type = "void"
            },
            {
                scm_name = "server-read",
                cpp_name = "server_read",
                ret_type = "string",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "server-write",
                cpp_name = "server_write",
                ret_type = "void",
                arg_list = {
                    "int",
                    "string"
                }
            },
            {
                scm_name = "server-started?",
                cpp_name = "server_started",
                ret_type = "bool"
            },
            {
                scm_name = "client-start",
                cpp_name = "client_start",
                ret_type = "int",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "client-stop",
                cpp_name = "client_stop",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "client-read",
                cpp_name = "client_read",
                ret_type = "string",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "client-write",
                cpp_name = "client_write",
                ret_type = "void",
                arg_list = {
                    "int",
                    "string"
                }
            },
            {
                scm_name = "enter-secure-mode",
                cpp_name = "enter_secure_mode",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            
              -- connections to extern systems
            {
                scm_name = "connection-start",
                cpp_name = "connection_start",
                ret_type = "string",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "connection-status",
                cpp_name = "connection_status",
                ret_type = "int",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "connection-write-string",
                cpp_name = "connection_write",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "connection-write",
                cpp_name = "connection_write",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "connection-cmd",
                cpp_name = "connection_cmd",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "connection-eval",
                cpp_name = "connection_eval",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "string",
                    "content"
                }
            },
            {
                scm_name = "connection-interrupt",
                cpp_name = "connection_interrupt",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "connection-stop",
                cpp_name = "connection_stop",
                ret_type = "void",
                arg_list = {
                    "string",
                    "string"
                }
            },
            
              -- widgets
            {
                scm_name = "widget-printer",
                cpp_name = "printer_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "url"
                }
            },
            {
                scm_name = "widget-color-picker",
                cpp_name = "color_picker_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "bool",
                    "array_tree"
                }
            },
            {
                scm_name = "widget-extend",
                cpp_name = "extend_widget",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "array_widget"
                }
            },
            {
                scm_name = "widget-hmenu",
                cpp_name = "horizontal_menu",
                ret_type = "widget",
                arg_list = {
                    "array_widget"
                }
            },
            {
                scm_name = "widget-vmenu",
                cpp_name = "vertical_menu",
                ret_type = "widget",
                arg_list = {
                    "array_widget"
                }
            },
            {
                scm_name = "widget-tmenu",
                cpp_name = "tile_menu",
                ret_type = "widget",
                arg_list = {
                    "array_widget",
                    "int"
                }
            },
            {
                scm_name = "widget-minibar-menu",
                cpp_name = "minibar_menu",
                ret_type = "widget",
                arg_list = {
                    "array_widget"
                }
            },
            {
                scm_name = "widget-separator",
                cpp_name = "menu_separator",
                ret_type = "widget",
                arg_list = {
                    "bool"
                }
            },
            {
                scm_name = "widget-menu-group",
                cpp_name = "menu_group",
                ret_type = "widget",
                arg_list = {
                    "string",
                    "int"
                }
            },
            {
                scm_name = "widget-pulldown-button",
                cpp_name = "pulldown_button",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "promise_widget"
                }
            },
            {
                scm_name = "widget-pullright-button",
                cpp_name = "pullright_button",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "promise_widget"
                }
            },
            {
                scm_name = "widget-menu-button",
                cpp_name = "menu_button",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "command",
                    "string",
                    "string",
                    "int"
                }
            },
            {
                scm_name = "widget-toggle",
                cpp_name = "toggle_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "bool",
                    "int"
                }
            },
            {
                scm_name = "widget-balloon",
                cpp_name = "balloon_widget",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "widget"
                }
            },
            {
                scm_name = "widget-empty",
                cpp_name = "empty_widget",
                ret_type = "widget"
            },
            {
                scm_name = "widget-text",
                cpp_name = "text_widget",
                ret_type = "widget",
                arg_list = {
                    "string",
                    "int",
                    "int",
                    "bool"
                }
            },
            {
                scm_name = "widget-input",
                cpp_name = "input_text_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "string",
                    "array_string",
                    "int",
                    "string"
                }
            },
            {
                scm_name = "widget-enum",
                cpp_name = "enum_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "array_string",
                    "string",
                    "int",
                    "string"
                }
            },
            {
                scm_name = "widget-choice",
                cpp_name = "choice_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "array_string",
                    "string"
                }
            },
            {
                scm_name = "widget-choices",
                cpp_name = "choice_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "array_string",
                    "array_string"
                }
            },
            {
                scm_name = "widget-filtered-choice",
                cpp_name = "choice_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "array_string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "widget-tree-view",
                cpp_name = "tree_view_widget",
                ret_type = "widget",
                arg_list = {
                    "command",
                    "tree",
                    "tree"
                }
            },
            {
                scm_name = "widget-xpm",
                cpp_name = "xpm_widget",
                ret_type = "widget",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "widget-box",
                cpp_name = "box_widget",
                ret_type = "widget",
                arg_list = {
                    "scheme_tree",
                    "string",
                    "int",
                    "bool",
                    "bool"
                }
            },
            {
                scm_name = "widget-glue",
                cpp_name = "glue_widget",
                ret_type = "widget",
                arg_list = {
                    "bool",
                    "bool",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "widget-color",
                cpp_name = "glue_widget",
                ret_type = "widget",
                arg_list = {
                    "content",
                    "bool",
                    "bool",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "widget-hlist",
                cpp_name = "horizontal_list",
                ret_type = "widget",
                arg_list = {
                    "array_widget"
                }
            },
            {
                scm_name = "widget-vlist",
                cpp_name = "vertical_list",
                ret_type = "widget",
                arg_list = {
                    "array_widget"
                }
            },
            {
                scm_name = "widget-division",
                cpp_name = "division_widget",
                ret_type = "widget",
                arg_list = {
                    "string",
                    "widget"
                }
            },
            {
                scm_name = "widget-aligned",
                cpp_name = "aligned_widget",
                ret_type = "widget",
                arg_list = {
                    "array_widget",
                    "array_widget"
                }
            },
            {
                scm_name = "widget-tabs",
                cpp_name = "tabs_widget",
                ret_type = "widget",
                arg_list = {
                    "array_widget",
                    "array_widget"
                }
            },
            {
                scm_name = "widget-icon-tabs",
                cpp_name = "icon_tabs_widget",
                ret_type = "widget",
                arg_list = {
                    "array_url",
                    "array_widget",
                    "array_widget"
                }
            },
            {
                scm_name = "widget-scrollable",
                cpp_name = "user_canvas_widget",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "int"
                }
            },
            {
                scm_name = "widget-resize",
                cpp_name = "resize_widget",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "int",
                    "string",
                    "string",
                    "string",
                    "string",
                    "string",
                    "string",
                    "string",
                    "string"
                }
            },
            {
                scm_name = "widget-hsplit",
                cpp_name = "hsplit_widget",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "widget"
                }
            },
            {
                scm_name = "widget-vsplit",
                cpp_name = "vsplit_widget",
                ret_type = "widget",
                arg_list = {
                    "widget",
                    "widget"
                }
            },
            {
                scm_name = "widget-texmacs-output",
                cpp_name = "texmacs_output_widget",
                ret_type = "widget",
                arg_list = {
                    "content",
                    "content"
                }
            },
            {
                scm_name = "widget-texmacs-input",
                cpp_name = "texmacs_input_widget",
                ret_type = "widget",
                arg_list = {
                    "content",
                    "content",
                    "url"
                }
            },
            {
                scm_name = "widget-ink",
                cpp_name = "ink_widget",
                ret_type = "widget",
                arg_list = {
                    "command"
                }
            },
            {
                scm_name = "widget-refresh",
                cpp_name = "refresh_widget",
                ret_type = "widget",
                arg_list = {
                    "string",
                    "string"
                }
            },
            {
                scm_name = "widget-refreshable",
                cpp_name = "refreshable_widget",
                ret_type = "widget",
                arg_list = {
                    "object",
                    "string"
                }
            },
            {
                scm_name = "object->promise-widget",
                cpp_name = "as_promise_widget",
                ret_type = "promise_widget",
                arg_list = {
                    "object"
                }
            },
            {
                scm_name = "tree-bounding-rectangle",
                cpp_name = "get_bounding_rectangle",
                ret_type = "array_int",
                arg_list = {
                    "tree"
                }
            },
            {
                scm_name = "widget-size",
                cpp_name = "get_widget_size",
                ret_type = "array_int",
                arg_list = {
                    "widget"
                }
            },
            {
                scm_name = "texmacs-widget-size",
                cpp_name = "get_texmacs_widget_size",
                ret_type = "array_int",
                arg_list = {
                    "widget"
                }
            },
            {
                scm_name = "show-balloon",
                cpp_name = "show_help_balloon",
                ret_type = "void",
                arg_list = {
                    "widget",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "get-style-menu",
                cpp_name = "get_style_menu",
                ret_type = "object"
            },
            {
                scm_name = "hidden-package?",
                cpp_name = "hidden_package",
                ret_type = "bool",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "get-add-package-menu",
                cpp_name = "get_add_package_menu",
                ret_type = "object"
            },
            {
                scm_name = "get-remove-package-menu",
                cpp_name = "get_remove_package_menu",
                ret_type = "object"
            },
            {
                scm_name = "get-toggle-package-menu",
                cpp_name = "get_toggle_package_menu",
                ret_type = "object"
            },
            {
                scm_name = "refresh-now",
                cpp_name = "windows_refresh",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "get-screen-size",
                cpp_name = "get_screen_size",
                ret_type = "array_int"
            },
            
              -- buffers
            {
                scm_name = "buffer-list",
                cpp_name = "get_all_buffers",
                ret_type = "array_url"
            },
            {
                scm_name = "current-buffer-url",
                cpp_name = "get_current_buffer_safe",
                ret_type = "url"
            },
            {
                scm_name = "path-to-buffer",
                cpp_name = "path_to_buffer",
                ret_type = "url",
                arg_list = {
                    "path"
                }
            },
            {
                scm_name = "buffer-new",
                cpp_name = "make_new_buffer",
                ret_type = "url"
            },
            {
                scm_name = "buffer-rename",
                cpp_name = "rename_buffer",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "buffer-set",
                cpp_name = "set_buffer_tree",
                ret_type = "void",
                arg_list = {
                    "url",
                    "content"
                }
            },
            {
                scm_name = "buffer-get",
                cpp_name = "get_buffer_tree",
                ret_type = "tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-set-body",
                cpp_name = "set_buffer_body",
                ret_type = "void",
                arg_list = {
                    "url",
                    "content"
                }
            },
            {
                scm_name = "buffer-get-body",
                cpp_name = "get_buffer_body",
                ret_type = "tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-set-master",
                cpp_name = "set_master_buffer",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "buffer-get-master",
                cpp_name = "get_master_buffer",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-set-title",
                cpp_name = "set_title_buffer",
                ret_type = "void",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "buffer-get-title",
                cpp_name = "get_title_buffer",
                ret_type = "string",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-last-save",
                cpp_name = "get_last_save_buffer",
                ret_type = "int",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-last-visited",
                cpp_name = "last_visited",
                ret_type = "double",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-modified?",
                cpp_name = "buffer_modified",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-modified-since-autosave?",
                cpp_name = "buffer_modified_since_autosave",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-pretend-modified",
                cpp_name = "pretend_buffer_modified",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-pretend-saved",
                cpp_name = "pretend_buffer_saved",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-pretend-autosaved",
                cpp_name = "pretend_buffer_autosaved",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-attach-notifier",
                cpp_name = "attach_buffer_notifier",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-has-name?",
                cpp_name = "buffer_has_name",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-aux?",
                cpp_name = "is_aux_buffer",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-embedded?",
                cpp_name = "is_embedded_buffer",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-import",
                cpp_name = "buffer_import",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "url",
                    "string"
                }
            },
            {
                scm_name = "buffer-load",
                cpp_name = "buffer_load",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "buffer-export",
                cpp_name = "buffer_export",
                ret_type = "bool",
                arg_list = {
                    "url",
                    "url",
                    "string"
                }
            },
            {
                scm_name = "buffer-save",
                cpp_name = "buffer_save",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "tree-import-loaded",
                cpp_name = "import_loaded_tree",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tree-import",
                cpp_name = "import_tree",
                ret_type = "tree",
                arg_list = {
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tree-inclusion",
                cpp_name = "load_inclusion",
                ret_type = "tree",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "tree-export",
                cpp_name = "export_tree",
                ret_type = "bool",
                arg_list = {
                    "tree",
                    "url",
                    "string"
                }
            },
            {
                scm_name = "tree-load-style",
                cpp_name = "load_style_tree",
                ret_type = "tree",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "buffer-focus",
                cpp_name = "focus_on_buffer",
                ret_type = "bool",
                arg_list = {
                    "url"
                }
            },
            
            {
                scm_name = "view-list",
                cpp_name = "get_all_views",
                ret_type = "array_url"
            },
            {
                scm_name = "buffer->views",
                cpp_name = "buffer_to_views",
                ret_type = "array_url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "current-view-url",
                cpp_name = "get_current_view_safe",
                ret_type = "url"
            },
            {
                scm_name = "window->view",
                cpp_name = "window_to_view",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "view->buffer",
                cpp_name = "view_to_buffer",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "view->window-url",
                cpp_name = "view_to_window",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "view-new",
                cpp_name = "get_new_view",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "view-passive",
                cpp_name = "get_passive_view",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "view-recent",
                cpp_name = "get_recent_view",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "view-delete",
                cpp_name = "delete_view",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "window-set-view",
                cpp_name = "window_set_view",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url",
                    "bool"
                }
            },
            {
                scm_name = "switch-to-buffer",
                cpp_name = "switch_to_buffer",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "set-drd",
                cpp_name = "set_current_drd",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            
            {
                scm_name = "window-list",
                cpp_name = "windows_list",
                ret_type = "array_url"
            },
            {
                scm_name = "windows-number",
                cpp_name = "get_nr_windows",
                ret_type = "int"
            },
            {
                scm_name = "current-window",
                cpp_name = "get_current_window",
                ret_type = "url"
            },
            {
                scm_name = "buffer->windows",
                cpp_name = "buffer_to_windows",
                ret_type = "array_url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "window-to-buffer",
                cpp_name = "window_to_buffer",
                ret_type = "url",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "window-set-buffer",
                cpp_name = "window_set_buffer",
                ret_type = "void",
                arg_list = {
                    "url",
                    "url"
                }
            },
            {
                scm_name = "window-focus",
                cpp_name = "window_focus",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "switch-to-window",
                cpp_name = "switch_to_window",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            
            {
                scm_name = "new-buffer",
                cpp_name = "create_buffer",
                ret_type = "url"
            },
            {
                scm_name = "open-buffer-in-window",
                cpp_name = "new_buffer_in_new_window",
                ret_type = "url",
                arg_list = {
                    "url",
                    "content",
                    "content"
                }
            },
            {
                scm_name = "open-window",
                cpp_name = "open_window",
                ret_type = "url"
            },
            {
                scm_name = "open-window-geometry",
                cpp_name = "open_window",
                ret_type = "url",
                arg_list = {
                    "content"
                }
            },
            {
                scm_name = "clone-window",
                cpp_name = "clone_window",
                ret_type = "void"
            },
            {
                scm_name = "cpp-buffer-close",
                cpp_name = "kill_buffer",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "kill-window",
                cpp_name = "kill_window",
                ret_type = "void",
                arg_list = {
                    "url"
                }
            },
            {
                scm_name = "kill-current-window-and-buffer",
                cpp_name = "kill_current_window_and_buffer",
                ret_type = "void"
            },
            
            {
                scm_name = "project-attach",
                cpp_name = "project_attach",
                ret_type = "void",
                arg_list = {
                    "string"
                }
            },
            {
                scm_name = "project-detach",
                cpp_name = "project_attach",
                ret_type = "void"
            },
            {
                scm_name = "project-attached?",
                cpp_name = "project_attached",
                ret_type = "bool"
            },
            {
                scm_name = "project-get",
                cpp_name = "project_get",
                ret_type = "url"
            },
            
              -- transitional alternative windows; to be replaced by better solution
            {
                scm_name = "alt-window-handle",
                cpp_name = "window_handle",
                ret_type = "int"
            },
            {
                scm_name = "alt-window-create-quit",
                cpp_name = "window_create",
                ret_type = "void",
                arg_list = {
                    "int",
                    "widget",
                    "string",
                    "command"
                }
            },
            {
                scm_name = "alt-window-create-plain",
                cpp_name = "window_create_plain",
                ret_type = "void",
                arg_list = {
                    "int",
                    "widget",
                    "string"
                }
            },
            {
                scm_name = "alt-window-create-popup",
                cpp_name = "window_create_popup",
                ret_type = "void",
                arg_list = {
                    "int",
                    "widget",
                    "string"
                }
            },
            {
                scm_name = "alt-window-create-tooltip",
                cpp_name = "window_create_tooltip",
                ret_type = "void",
                arg_list = {
                    "int",
                    "widget",
                    "string"
                }
            },
            {
                scm_name = "alt-window-delete",
                cpp_name = "window_delete",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "alt-window-show",
                cpp_name = "window_show",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "alt-window-hide",
                cpp_name = "window_hide",
                ret_type = "void",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "alt-window-get-size",
                cpp_name = "window_get_size",
                ret_type = "scheme_tree",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "alt-window-set-size",
                cpp_name = "window_set_size",
                ret_type = "void",
                arg_list = {
                    "int",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "alt-window-get-position",
                cpp_name = "window_get_position",
                ret_type = "scheme_tree",
                arg_list = {
                    "int"
                }
            },
            {
                scm_name = "alt-window-set-position",
                cpp_name = "window_set_position",
                ret_type = "void",
                arg_list = {
                    "int",
                    "int",
                    "int"
                }
            },
            {
                scm_name = "alt-window-search",
                cpp_name = "window_search",
                ret_type = "path",
                arg_list = {
                    "url"
                }
            },
            
              -- routines for BibTeX
            {
                scm_name = "supports-bibtex?",
                cpp_name = "bibtex_present",
                ret_type = "bool"
            },
            {
                scm_name = "bibtex-run",
                cpp_name = "bibtex_run",
                ret_type = "tree",
                arg_list = {
                    "string",
                    "string",
                    "url",
                    "array_string"
                }
            },
            {
                scm_name = "bib-add-period",
                cpp_name = "bib_add_period",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-locase-first",
                cpp_name = "bib_locase_first",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-upcase-first",
                cpp_name = "bib_upcase_first",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-locase",
                cpp_name = "bib_locase",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-upcase",
                cpp_name = "bib_upcase",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-default-preserve-case",
                cpp_name = "bib_default_preserve_case",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-default-upcase-first",
                cpp_name = "bib_default_upcase_first",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-purify",
                cpp_name = "bib_purify",
                ret_type = "string",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-text-length",
                cpp_name = "bib_text_length",
                ret_type = "int",
                arg_list = {
                    "scheme_tree"
                }
            },
            {
                scm_name = "bib-prefix",
                cpp_name = "bib_prefix",
                ret_type = "string",
                arg_list = {
                    "scheme_tree",
                    "int"
                }
            },
            {
                scm_name = "bib-empty?",
                cpp_name = "bib_empty",
                ret_type = "bool",
                arg_list = {
                    "scheme_tree",
                    "string"
                }
            },
            {
                scm_name = "bib-field",
                cpp_name = "bib_field",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree",
                    "string"
                }
            },
            {
                scm_name = "bib-abbreviate",
                cpp_name = "bib_abbreviate",
                ret_type = "scheme_tree",
                arg_list = {
                    "scheme_tree",
                    "scheme_tree",
                    "scheme_tree"
                }
            },
        }
    }
end