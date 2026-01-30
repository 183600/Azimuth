// 用户添加的 MoonBit 测试用例
test "xyz_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "xyz_subtract_test" {
  let result = @azimuth.subtract(10, 3)
  @azimuth.assert_eq(7, result)
}

test "xyz_multiply_test" {
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "xyz_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

test "xyz_divide_with_ceil_test" {
  let result = @azimuth.divide_with_ceil(10, 3)
  @azimuth.assert_eq(4, result)
}

test "xyz_complex_calculation" {
  let step1 = @azimuth.multiply(3, 4)
  let step2 = @azimuth.add(step1, 5)
  let step3 = @azimuth.divide_with_ceil(step2, 2)
  @azimuth.assert_eq(9, step3)
}

test "xyz_business_scenario" {
  let items = 47
  let capacity = 12
  let boxes = @azimuth.divide_with_ceil(items, capacity)
  let total_capacity = @azimuth.multiply(boxes, capacity)
  let remaining = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(4, boxes)
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, remaining)
}

test "xyz_mathematical_properties" {
  let x = 7
  let y = 11
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  @azimuth.assert_eq(@azimuth.subtract(x, 0), x)
  @azimuth.assert_eq(@azimuth.multiply(x, 1), x)
}