// 用户添加的测试用例 - 2024年
test "user_test_basic_addition_2024" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "user_test_basic_multiplication_2024" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "user_test_basic_greeting_2024" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "user_test_divide_with_ceil_2024" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "user_test_subtract_2024" {
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

test "user_test_mathematical_properties_2024" {
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, 3)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3))
  )
}

test "user_test_complex_calculation_2024" {
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let offset = 3
  
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, offset)
  
  @azimuth.assert_eq(12, result)  // ceil(15*4/7) + 3 = 9 + 3 = 12
}

test "user_test_business_logic_2024" {
  let total_items = 47
  let box_capacity = 12
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, box_capacity)
  @azimuth.assert_eq(4, boxes_needed)
  
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(48, total_capacity)
  
  let wasted_space = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(1, wasted_space)
}

test "user_test_error_handling_2024" {
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
}

test "user_test_performance_2024" {
  let large_number = 10000
  
  let sum_result = @azimuth.add(large_number, large_number)
  let product_result = @azimuth.multiply(large_number, 100)
  let division_result = @azimuth.divide_with_ceil(product_result, large_number)
  
  @azimuth.assert_eq(20000, sum_result)
  @azimuth.assert_eq(1000000, product_result)
  @azimuth.assert_eq(100, division_result)
}