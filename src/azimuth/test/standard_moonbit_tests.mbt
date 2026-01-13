// 标准 MoonBit 测试用例

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(@azimuth.add(10, 20), 30)
  @azimuth.assert_eq(@azimuth.add(-5, 5), 0)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(@azimuth.multiply(3, 7), 21)
  @azimuth.assert_eq(@azimuth.multiply(-4, 5), -20)
  @azimuth.assert_eq(@azimuth.multiply(-6, -8), 48)
  @azimuth.assert_eq(@azimuth.multiply(0, 12345), 0)
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1), min_val)
  @azimuth.assert_eq(@azimuth.multiply(46341, 46341), max_val)
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
  @azimuth.assert_eq_string(@azimuth.greet("测试"), "Hello, 测试!")
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(result, 22)
}

test "real_world_calculation_scenario" {
  // 测试真实世界计算场景：购物车总价计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(total, 612)  // 199 * 3 + 15 = 597 + 15 = 612
}

test "string_processing_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  @azimuth.assert_eq_string(@azimuth.greet("John@Doe"), "Hello, John@Doe!")
  @azimuth.assert_eq_string(@azimuth.greet("user#123"), "Hello, user#123!")
  @azimuth.assert_eq_string(@azimuth.greet("test$%^&*"), "Hello, test$%^&*!")
}

test "sequential_operations_test" {
  // 测试连续运算操作
  let initial = 100
  
  // 连续加法
  let result1 = @azimuth.add(@azimuth.add(initial, 50), 25)
  @azimuth.assert_eq(result1, 175)
  
  // 连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(result2, 24)
  
  // 混合运算
  let result3 = @azimuth.add(@azimuth.multiply(5, 10), @azimuth.multiply(3, 4))
  @azimuth.assert_eq(result3, 62)  // 5*10 + 3*4 = 50 + 12 = 62
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(@azimuth.add(max_val, min_val), -1)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 1), min_val)
}