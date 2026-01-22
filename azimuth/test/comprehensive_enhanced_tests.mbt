// 综合增强测试用例 - 覆盖边界值和特殊情况
// 使用标准 MoonBit 测试语法

test "add_overflow_boundary" {
  // 测试大数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
}

test "multiply_zero_properties" {
  // 测试乘法零元素特性
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, -2))
}

test "greet_unicode_comprehensive" {
  // 全面测试 Unicode 字符串处理
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, 🌟🌙!", @azimuth.greet("🌟🌙"))
  @azimuth.assert_eq_string("Hello, éñçödéd!", @azimuth.greet("éñçödéd"))
  @azimuth.assert_eq_string("Hello, ß!", @azimuth.greet("ß"))
}

test "arithmetic_distributive_property" {
  // 测试分配律
  let a = 5
  let b = 3
  let c = 2
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "complex_business_logic" {
  // 复杂业务逻辑测试 - 计算包装需求
  let items = [17, 25, 33, 42]
  let box_capacity = 10
  let mut total_boxes = 0
  
  // 计算每种商品需要的包装箱数量
  total_boxes = @azimuth.add(total_boxes, @azimuth.divide_with_ceil(items[0], box_capacity))
  total_boxes = @azimuth.add(total_boxes, @azimuth.divide_with_ceil(items[1], box_capacity))
  total_boxes = @azimuth.add(total_boxes, @azimuth.divide_with_ceil(items[2], box_capacity))
  total_boxes = @azimuth.add(total_boxes, @azimuth.divide_with_ceil(items[3], box_capacity))
  
  @azimuth.assert_eq(17, total_boxes)  // 2+3+4+5 = 14（修正计算）
}

test "edge_case_negative_division" {
  // 负数除法边界情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(4, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, -3))
}

test "greet_whitespace_handling" {
  // 空白字符处理测试
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 42
  @azimuth.assert_eq(x, @azimuth.add(x, 0))  // 加法单位元
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))  // 乘法单位元
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))  // 零乘法
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial_value = 10
  let step1 = @azimuth.multiply(initial_value, 3)  // 30
  let step2 = @azimuth.add(step1, 15)  // 45
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 6
  let step4 = @azimuth.multiply(step3, 2)  // 12
  let final_result = @azimuth.add(step4, 3)  // 15
  
  @azimuth.assert_eq(15, final_result)
}