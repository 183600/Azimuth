// 高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能
test "add_function_comprehensive" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  
  // 边界情况测试
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1000, @azimuth.add(-500, -500))
  @azimuth.assert_eq(0, @azimuth.add(-500, 500))
}

test "multiply_function_edge_cases" {
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  
  // 大数测试
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
  @azimuth.assert_eq(-10000, @azimuth.multiply(-100, 100))
  @azimuth.assert_eq(10000, @azimuth.multiply(-100, -100))
}

test "greet_function_unicode_support" {
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // Unicode 支持测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "divide_with_ceil_comprehensive" {
  // 正数除法向上取整
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数除法向上取整
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "complex_arithmetic_operations" {
  // 复合运算测试
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(40, result2)  // (5+3) * ceil(10/2) = 8 * 5 = 40
  
  let result3 = @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(2, 5)), 7)
  @azimuth.assert_eq(3, result3)  // ceil((3*4 + 2*5)/7) = ceil(22/7) = 3
}

test "mathematical_properties" {
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(-5, 10), @azimuth.multiply(10, -5))
  
  // 零元素性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "real_world_scenarios" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 价格计算场景
  let price_per_item = 15
  let quantity = 3
  let total_cost = @azimuth.multiply(price_per_item, quantity)
  @azimuth.assert_eq(45, total_cost)
  
  // 折扣计算场景
  let original_price = 100
  let discount = 20
  let final_price = @azimuth.add(original_price, -discount)
  @azimuth.assert_eq(80, final_price)
}

test "error_handling_and_edge_cases" {
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.multiply(1000, 2000))
  @azimuth.assert_eq(3000, @azimuth.add(1000, 2000))
  
  // 除法边界情况
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(10000, 10))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(10001, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(9999, 10000))
}

test "string_processing_comprehensive" {
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 
	!", @azimuth.greet("
	"))
  
  // 长字符串处理
  let long_name = "VeryLongNameWithManyCharacters"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
  
  // 数字字符串处理
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "performance_and_stress" {
  // 连续运算测试
  let mut result = 0
  result = @azimuth.add(result, 1)
  result = @azimuth.add(result, 2)
  result = @azimuth.add(result, 3)
  result = @azimuth.add(result, 4)
  result = @azimuth.add(result, 5)
  @azimuth.assert_eq(15, result)  // 1+2+3+4+5 = 15
  
  // 嵌套运算测试
  let nested_result = @azimuth.multiply(@azimuth.add(2, 3), @azimuth.multiply(2, @azimuth.add(1, 1)))
  @azimuth.assert_eq(20, nested_result)  // (2+3) * 2 * (1+1) = 5 * 2 * 2 = 20
}