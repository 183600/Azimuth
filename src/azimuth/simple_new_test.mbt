// 简单的新测试文件

///|
test "simple_new_add_test" {
  assert_eq(5, @azimuth.add(2, 3))
}

///|
test "simple_new_multiply_test" {
  assert_eq(6, @azimuth.multiply(2, 3))
}

///|
test "simple_new_greet_test" {
  assert_eq("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}