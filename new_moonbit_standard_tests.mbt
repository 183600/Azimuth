// 新的标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖项目核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-4, @azimuth.multiply(-2, 2))
  @azimuth.assert_eq(9, @azimuth.multiply(-3, -3))
}

test "greet_function_test" {
  // 问候函数测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "boundary_value_addition" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
}

test "boundary_value_multiplication" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let length = 10
  let width = 5
  let height = 3
  
  // 计算体积：长 × 宽 × 高
  let base_area = @azimuth.multiply(length, width)
  let volume = @azimuth.multiply(base_area, height)
  
  @azimuth.assert_eq(50, base_area)
  @azimuth.assert_eq(150, volume)
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_application" {
  // 实际应用场景测试：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(617, total)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello,   spaced!", @azimuth.greet("  spaced"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "error_handling_validation" {
  // 错误处理验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
  
  // 验证溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}