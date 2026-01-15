// MoonBit 测试用例示例
// 测试项目的核心功能：add, multiply, greet 函数

test "example_basic_addition" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "example_basic_multiplication" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "example_string_greeting" {
  // 字符串问候测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "example_boundary_values" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "example_shopping_cart" {
  // 购物车计算实际应用场景
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  // 计算每种商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(total1, total2)
  
  // 验证总价：199*2 + 99*3 = 398 + 297 = 695
  assert_eq(695, cart_total)
}

test "example_temperature_conversion" {
  // 温度转换实际应用场景
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "example_complex_calculation" {
  // 复合计算测试
  // 计算：(a + b) × c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(38, result)
}

test "example_assert_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "example_area_calculations" {
  // 面积计算测试
  // 矩形面积：长 × 宽
  let length = 8
  let width = 6
  let rectangle_area = multiply(length, width)
  assert_eq(48, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 5
  let triangle_area = multiply(base, height) / 2
  assert_eq(25, triangle_area)
}

test "example_error_handling" {
  // 错误处理和溢出保护测试
  let max_val = 2147483647
  
  // 测试溢出保护
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 测试正常运算
  assert_true(add(5, 3) == 8)
  assert_true(multiply(4, 6) == 24)
}