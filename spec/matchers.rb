# coding: utf-8
require 'json'
JSON_STATE = JSON::State.new(
   :indent       => "  ",
   :space        => " ",
   :space_before => "",
   :object_nl    => "\n",
   :array_nl     => "\n"
 )

RSpec::Matchers.define :produce do |expected, info|
  match do |actual|
    actual.should == expected
  end
  
  failure_message_for_should do |actual|
    "Expected: #{[Array, Hash].include?(expected.class) ? expected.to_json(JSON_STATE) : expected.inspect}\n" +
    "Actual  : #{[Array, Hash].include?(actual.class) ? actual.to_json(JSON_STATE) : actual.inspect}\n" +
    #(expected.is_a?(Hash) && actual.is_a?(Hash) ? "Diff: #{expected.diff(actual).to_json(JSON_STATE)}\n" : "") +
    "Processing results:\n#{info.join("\n")}"
  end
end
