test "simple_attribute_value_test" {
  let attr = AttributeValue::string("test")
  match attr {
    StringValue(s) => assert_eq(s, "test")
    _ => @test.fail("Test failed")
  }
}