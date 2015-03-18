# coding: utf-8
def normalize(obj)
  if obj.is_a?(String)
    obj.gsub(/\s+/m, ' ').
      gsub(/\s+\)/m, ')').
      gsub(/\(\s+/m, '(').
      strip
  else
    obj
  end
end

RSpec::Matchers.define :produce do |expected, info|
  match do |actual|
    expect(normalize(actual)).to eq normalize(expected)
  end
  
  failure_message do |actual|
    "Expected: #{normalize(expected)}\n" +
    "Actual  : #{normalize(actual)}\n" +
    "Raw     : #{expected}\n" +
    "Result  : #{actual}\n" +
    "Processing results:\n#{info.join("\n")}"
  end
end
