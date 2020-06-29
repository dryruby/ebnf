# This file is automatically generated by ebnf version 2.0.0
# Derived from iso-ebnf.ebnf
module ISOEBNFMeta
  RULES = [
    EBNF::Rule.new(:syntax, nil, [:star, :syntax_rule]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:syntax_rule, nil, [:seq, :meta_identifier, :defining_symbol, :definitions_list, :terminator_symbol]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:definitions_list, nil, [:seq, :single_definition, :_definitions_list_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_definitions_list_1, ".1", [:star, :_definitions_list_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_definitions_list_2, ".2", [:seq, :definition_separator_symbol, :definitions_list]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:single_definition, nil, [:seq, :term, :_single_definition_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_single_definition_1, ".1", [:star, :_single_definition_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_single_definition_2, ".2", [:seq, ",", :term]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:term, nil, [:seq, :factor, :_term_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_term_1, ".1", [:opt, :_term_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_term_2, ".2", [:seq, "-", :exception]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:exception, nil, [:seq, :factor]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:factor, nil, [:seq, :_factor_1, :primary]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_factor_1, ".1", [:opt, :_factor_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_factor_2, ".2", [:seq, :integer, "*"]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:primary, nil, [:alt, :optional_sequence, :repeated_sequence, :special_sequence, :grouped_sequence, :meta_identifier, :terminal_string, :empty]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:optional_sequence, nil, [:seq, :start_option_symbol, :definitions_list, :end_option_symbol]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:repeated_sequence, nil, [:seq, :start_repeat_symbol, :definitions_list, :end_repeat_symbol]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:grouped_sequence, nil, [:seq, "(", :definitions_list, ")"]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:letter, nil, [:range, "a-zA-Z"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:decimal_digit, nil, [:range, "0-9"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:integer, nil, [:plus, :decimal_digit], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:meta_identifier, nil, [:seq, :letter, :_meta_identifier_1], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_meta_identifier_1, ".1", [:star, :meta_identifier_character]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:meta_identifier_character, nil, [:alt, :letter, :decimal_digit, "_"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:terminal_string, nil, [:alt, :_terminal_string_1, :_terminal_string_2], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_terminal_string_1, ".1", [:seq, "'", :_terminal_string_3, "'"]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_terminal_string_3, ".3", [:plus, :first_terminal_character]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_terminal_string_2, ".2", [:seq, "\"", :_terminal_string_4, "\""]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_terminal_string_4, ".4", [:plus, :second_terminal_character]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:first_terminal_character, nil, [:diff, :terminal_character, "'"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:second_terminal_character, nil, [:diff, :terminal_character, "\""], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:special_sequence, nil, [:seq, "?", :_special_sequence_1, "?"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_special_sequence_1, ".1", [:star, :special_sequence_character]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:special_sequence_character, nil, [:diff, :terminal_character, "?"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:terminal_character, nil, [:alt, :letter, :decimal_digit, :concatenate_symbol, :defining_symbol, :definition_separator_symbol, :end_comment_symbol, :end_group_symbol, :end_option_symbol, :end_repeat_symbol, :except_symbol, :first_quote_symbol, :repetition_symbol, :second_quote_symbol, :special_sequence_symbol, :start_comment_symbol, :start_group_symbol, :start_option_symbol, :start_repeat_symbol, :terminator_symbol, :other_character], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:other_character, nil, [:alt, :_other_character_1, "\\"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_other_character_1, ".1", [:range, ":+_%@&$<>^` ̃#x20#x23"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:gap_separator, nil, [:range, "#x9#xa#xb#xc#xd#x20"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_pass, nil, [:plus, :gap_separator], kind: :pass).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:empty, nil, [:seq, []], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:defining_symbol, nil, [:alt, "=", ":"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:definition_separator_symbol, nil, [:alt, "|", "/", "!"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:terminator_symbol, nil, [:alt, ";", "."], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:start_option_symbol, nil, [:alt, "[", "(/"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:end_option_symbol, nil, [:alt, "]", "/)"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:start_repeat_symbol, nil, [:alt, "{", "(:"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:end_repeat_symbol, nil, [:alt, "}", ":)"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:gap_free_symbol, nil, [:alt, :_gap_free_symbol_1, :terminal_string], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_gap_free_symbol_1, ".1", [:seq, :_gap_free_symbol_3, :terminal_character]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_gap_free_symbol_3, ".3", [:not, :_gap_free_symbol_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_gap_free_symbol_2, ".2", [:range, "'\""], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:repetition_symbol, nil, [:seq, "*"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:except_symbol, nil, [:seq, "-"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:concatenate_symbol, nil, [:seq, ","], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:first_quote_symbol, nil, [:seq, "'"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:second_quote_symbol, nil, [:seq, "\""], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:start_comment_symbol, nil, [:seq, "(*"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:end_comment_symbol, nil, [:seq, "*)"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:start_group_symbol, nil, [:seq, "("], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:end_group_symbol, nil, [:seq, ")"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:special_sequence_symbol, nil, [:seq, "?"], kind: :terminal).extend(EBNF::PEG::Rule),
  ]
end
