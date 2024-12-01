# This file is automatically generated by ebnf version 2.5.0
# Derived from etc/ebnf.ebnf
module EBNFMeta
  RULES = [
    EBNF::Rule.new(:ebnf, "1", [:star, :_ebnf_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_ebnf_1, "1.1", [:alt, :declaration, :rule]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:declaration, "2", [:alt, "@terminals", :pass]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:rule, "3", [:seq, :LHS, :expression]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:expression, "4", [:seq, :alt]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:alt, "5", [:seq, :seq, :_alt_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_alt_1, "5.1", [:star, :_alt_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_alt_2, "5.2", [:seq, "|", :seq]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:seq, "6", [:plus, :diff]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:diff, "7", [:seq, :postfix, :_diff_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_diff_1, "7.1", [:opt, :_diff_2]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_diff_2, "7.2", [:seq, "-", :postfix]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:postfix, "8", [:seq, :primary, :_postfix_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_postfix_1, "8.1", [:opt, :POSTFIX]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:primary, "9", [:alt, :HEX, :SYMBOL, :O_RANGE, :RANGE, :STRING1, :STRING2, :_primary_1]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_primary_1, "9.1", [:seq, "(", :expression, ")"]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:pass, "10", [:seq, "@pass", :expression]).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_terminals, nil, [:seq], kind: :terminals).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:LHS, "11", [:seq, :_LHS_1, :SYMBOL, :_LHS_2, "::="], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_LHS_1, "11.1", [:opt, :_LHS_3], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_LHS_3, "11.3", [:seq, "[", :SYMBOL, "]", :_LHS_4], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_LHS_4, "11.4", [:plus, " "], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_LHS_2, "11.2", [:star, " "], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:SYMBOL, "12", [:alt, :_SYMBOL_1, :O_SYMBOL], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_SYMBOL_1, "12.1", [:seq, "<", :O_SYMBOL, ">"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:O_SYMBOL, "12a", [:plus, :_O_SYMBOL_1], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_SYMBOL_1, "12a.1", [:alt, :_O_SYMBOL_2, :_O_SYMBOL_3, :_O_SYMBOL_4, "_", "."], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_SYMBOL_2, "12a.2", [:range, "a-z"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_SYMBOL_3, "12a.3", [:range, "A-Z"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_SYMBOL_4, "12a.4", [:range, "0-9"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:HEX, "13", [:seq, "#x", :_HEX_1], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_HEX_1, "13.1", [:plus, :_HEX_2], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_HEX_2, "13.2", [:alt, :_HEX_3, :_HEX_4, :_HEX_5], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_HEX_3, "13.3", [:range, "a-f"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_HEX_4, "13.4", [:range, "A-F"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_HEX_5, "13.5", [:range, "0-9"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:RANGE, "14", [:seq, "[", :_RANGE_1, :_RANGE_2, "]"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_RANGE_1, "14.1", [:plus, :_RANGE_3], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_RANGE_3, "14.3", [:alt, :_RANGE_4, :_RANGE_5, :R_CHAR, :HEX], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_RANGE_4, "14.4", [:seq, :R_CHAR, "-", :R_CHAR], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_RANGE_5, "14.5", [:seq, :HEX, "-", :HEX], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_RANGE_2, "14.2", [:opt, "-"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:O_RANGE, "15", [:seq, "[^", :_O_RANGE_1, :_O_RANGE_2, "]"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_RANGE_1, "15.1", [:plus, :_O_RANGE_3], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_RANGE_3, "15.3", [:alt, :_O_RANGE_4, :_O_RANGE_5, :R_CHAR, :HEX], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_RANGE_4, "15.4", [:seq, :R_CHAR, "-", :R_CHAR], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_RANGE_5, "15.5", [:seq, :HEX, "-", :HEX], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_O_RANGE_2, "15.2", [:opt, "-"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:STRING1, "16", [:seq, "\"", :_STRING1_1, "\""], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_STRING1_1, "16.1", [:star, :_STRING1_2], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_STRING1_2, "16.2", [:diff, :CHAR, "\""], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:STRING2, "17", [:seq, "'", :_STRING2_1, "'"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_STRING2_1, "17.1", [:star, :_STRING2_2], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_STRING2_2, "17.2", [:diff, :CHAR, "'"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:CHAR, "18", [:alt, :_CHAR_1, :_CHAR_2, :_CHAR_3, :_CHAR_4], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_CHAR_1, "18.1", [:range, "#x9#xA#xD"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_CHAR_2, "18.2", [:range, "#x20-#xD7FF"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_CHAR_3, "18.3", [:range, "#xE000-#xFFFD"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_CHAR_4, "18.4", [:range, "#x10000-#x10FFFF"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:R_CHAR, "19", [:diff, :CHAR, :_R_CHAR_1], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_R_CHAR_1, "19.1", [:alt, "]", "-", :HEX], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:POSTFIX, "20", [:range, "?*+"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:PASS, "21", [:alt, :_PASS_1, :_PASS_2, :_PASS_3, :_PASS_4], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_1, "21.1", [:range, "#x9#xA#xD#x20"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_2, "21.2", [:seq, :_PASS_5, :_PASS_6], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_5, "21.5", [:alt, :_PASS_7, "//"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_7, "21.7", [:diff, "#", "#x"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_6, "21.6", [:star, :_PASS_8], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_8, "21.8", [:range, "^#xA#xD"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_3, "21.3", [:seq, "/*", :_PASS_9, "*/"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_9, "21.9", [:star, :_PASS_10], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_10, "21.10", [:alt, :_PASS_11, :_PASS_12], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_11, "21.11", [:opt, :_PASS_13], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_13, "21.13", [:seq, "*", :_PASS_14], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_14, "21.14", [:range, "^/"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_12, "21.12", [:range, "^*"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_4, "21.4", [:seq, "(*", :_PASS_15, "*)"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_15, "21.15", [:star, :_PASS_16], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_16, "21.16", [:alt, :_PASS_17, :_PASS_18], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_17, "21.17", [:opt, :_PASS_19], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_19, "21.19", [:seq, "*", :_PASS_20], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_20, "21.20", [:range, "^)"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_PASS_18, "21.18", [:range, "^*"], kind: :terminal).extend(EBNF::PEG::Rule),
    EBNF::Rule.new(:_pass, nil, [:seq, :PASS], kind: :pass).extend(EBNF::PEG::Rule),
  ]
end

