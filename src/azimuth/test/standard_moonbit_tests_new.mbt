// 标准 MoonBit 测试用例
// 覆盖 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  let result1 = azimuth::add(2, 3)
  azimuth::assert_eq(5, result1)
  
  let result2 = azimuth::add(0, 0)
  azimuth::assert_eq(0, result2)
  
  let result3 = azimuth::add(-1, -2)
  azimuth::assert_eq(-3, result3)
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let result1 = azimuth::add(max_val, 1)
  azimuth::assert_eq(max_val, result1)
  
  let result2 = azimuth::add(min_val, -1)
  azimuth::assert_eq(min_val, result2)
  
  let result3 = azimuth::add(max_val, min_val)
  azimuth::assert_eq(-1, result3)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result1 = azimuth::multiply(2, 3)
  azimuth::assert_eq(6, result1)
  
  let result2 = azimuth::multiply(0, 5)
  azimuth::assert_eq(0, result2)
  
  let result3 = azimuth::multiply(2, -3)
  azimuth::assert_eq(-6, result3)
  
  let result4 = azimuth::multiply(-2, -3)
  azimuth::assert_eq(6, result4)
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的特殊情况
  let result1 = azimuth::multiply(5, 1)
  azimuth::assert_eq(5, result1)
  
  let result2 = azimuth::multiply(5, -1)
  azimuth::assert_eq(-5, result2)
  
  // 测试最小值的特殊处理
  let result3 = azimuth::multiply(min_val, -1)
  azimuth::assert_eq(min_val, result3)
  
  let result4 = azimuth::multiply(min_val, 2)
  azimuth::assert_eq(min_val, result4)
}

test "greet_functionality" {
  // 测试greet函数的基本功能
  let result1 = azimuth::greet("World")
  azimuth::assert_eq_string("Hello, World!", result1)
  
  let result2 = azimuth::greet("")
  azimuth::assert_eq_string("Hello, !", result2)
  
  let result3 = azimuth::greet("张三")
  azimuth::assert_eq_string("Hello, 张三!", result3)
}

test "greet_special_characters" {
  // 测试greet函数处理特殊字符
  let result1 = azimuth::greet("test@example.com")
  azimuth::assert_eq_string("Hello, test@example.com!", result1)
  
  let result2 = azimuth::greet("用户123")
  azimuth::assert_eq_string("Hello, 用户123!", result2)
  
  let result3 = azimuth::greet("🌍")
  azimuth::assert_eq_string("Hello, 🌍!", result3)
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  let result1 = azimuth::divide_with_ceil(10, 3)
  azimuth::assert_eq(3, result1)
  
  let result2 = azimuth::divide_with_ceil(6, 3)
  azimuth::assert_eq(2, result2)
  
  let result3 = azimuth::divide_with_ceil(11, 3)
  azimuth::assert_eq(4, result3)
  
  let result4 = azimuth::divide_with_ceil(-10, 3)
  azimuth::assert_eq(-3, result4)
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形的周长：2 * (长 + 宽)
  let length = 8
  let width = 5
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  azimuth::assert_eq(26, perimeter)
  
  // 计算三角形的面积：底 * 高 / 2
  let base = 10
  let height = 6
  let area = azimuth::divide_with_ceil(azimuth::multiply(base, height), 2)
  azimuth::assert_eq(30, area)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 5
  let c = 3
  
  // 测试加法交换律
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  azimuth::assert_eq(result1, result2)
  
  // 测试乘法交换律
  let result3 = azimuth::multiply(a, b)
  let result4 = azimuth::multiply(b, a)
  azimuth::assert_eq(result3, result4)
  
  // 测试分配律：a * (b + c) = a * b + a * c
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left, right)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  let cart_total = azimuth::add(total1, total2)
  
  azimuth::assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C * 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  azimuth::assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}