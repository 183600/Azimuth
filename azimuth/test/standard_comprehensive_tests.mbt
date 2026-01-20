// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的主要功能

test "add_basic_functionality" {
  // 测试加法基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试乘法基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_special_values" {
  // 测试乘法特殊值
  let min_val = -2147483648
  
  // 测试1和-1的情况
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_function_test" {
  // 测试问候函数
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法
  // 正数向上取整
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  
  // 负数向上取整
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  
  // 整除情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let a = 10
  let b = 5
  let c = -3
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-45, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(47, result2)
}

test "unicode_and_special_characters" {
  // 测试Unicode字符和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
}

test "negative_number_operations" {
  // 测试负数运算
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
  
  // 负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let shipping = 15
  
  let subtotal = @azimuth.add(item1, item2)
  let subtotal2 = @azimuth.add(subtotal, item3)
  let total = @azimuth.add(subtotal2, shipping)
  @azimuth.assert_eq(612, total)
  
  // 批量计算（单价 × 数量）
  let unit_price = 25
  let quantity = 8
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, order_total)
  
  // 页面分组计算（向上取整）
  let total_items = 35
  let items_per_page = 10
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(4, pages_needed)
}