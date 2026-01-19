// Azimuth 综合测试用例
// 使用标准 MoonBit 测试语法

// 测试基本加法功能
test "add_basic_functionality" {
  // 测试正数相加
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
  
  // 测试负数相加
  assert_eq(-5, add(-2, -3))
  assert_eq(-10, add(-7, -3))
  
  // 测试正负数相加
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(5, -7))
  
  // 测试与零相加
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, add(0, 0))
}

// 测试边界值加法
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试极值相互作用
  assert_eq(-1, add(max_val, min_val))
}

// 测试基本乘法功能
test "multiply_basic_functionality" {
  // 测试正数相乘
  assert_eq(6, multiply(2, 3))
  assert_eq(15, multiply(3, 5))
  
  // 测试负数相乘
  assert_eq(6, multiply(-2, -3))
  assert_eq(-15, multiply(-3, 5))
  
  // 测试与零相乘
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  
  // 测试与一相乘
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
}

// 测试边界值乘法
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, multiply(max_val, 0))
  
  // 测试最小值边界
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
}

// 测试字符串拼接功能
test "greet_functionality" {
  // 测试基本字符串
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  
  // 测试Unicode字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

// 测试实际应用场景
test "real_world_scenarios" {
  // 购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(total1, total2)
  
  // 验证总价：199*2 + 99*3 = 398 + 297 = 695
  assert_eq(695, cart_total)
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C * 2 + 30
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
}

// 测试数学性质
test "mathematical_properties" {
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 3)), 
    add(multiply(a, b), multiply(a, 3))
  )
}

// 测试复合计算
test "complex_calculations" {
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let area = multiply(base, height) / 2
  assert_eq(24, area)
  
  // 计算平均值：(a + b + c) / 3
  let num1 = 90
  let num2 = 80
  let num3 = 85
  let sum = add(add(num1, num2), num3)
  let average = sum / 3
  assert_eq(85, average)
}