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

Info = Struct.new(:id, :logger, :action, :result, :format)

RSpec::Matchers.define :produce do |expected, info|
  match do |actual|
    @info = if info.is_a?(Logger)
      Info.new("", info)
    elsif info.is_a?(Hash)
      Info.new(info[:id], info[:logger], info[:action], info[:result])
    else
      Info.new(info)
    end
    expect(normalize(actual)).to eq normalize(expected)
  end
  
  failure_message do |actual|
    "Expected: #{normalize(expected)}\n" +
    "Actual  : #{normalize(actual)}\n" +
    "Raw     : #{expected}\n" +
    "Result  : #{actual}\n" +
    "Processing results:\n#{@info.logger}"
  end
end
