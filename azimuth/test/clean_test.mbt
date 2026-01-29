// 干净的测试用例
test "clean_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "clean_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "clean_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "clean_divide_test" {
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
}

test "clean_business_test" {
  let items = 25
  let capacity = 6
  let boxes = @azimuth.divide_with_ceil(items, capacity)
  @azimuth.assert_eq(5, boxes)
  
  let total_capacity = @azimuth.multiply(boxes, capacity)
  @azimuth.assert_eq(30, total_capacity)
}