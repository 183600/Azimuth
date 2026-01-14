// 额外的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

// 测试 1: 基本算术运算验证
test "basic_arithmetic_validation" {
  // 测试加法基本运算
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(0, azimuth::add(-5, 5))
  azimuth::assert_eq(-8, azimuth::add(-3, -5))
  
  // 测试乘法基本运算
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
  azimuth::assert_eq(-12, azimuth::multiply(-3, 4))
  azimuth::assert_eq(0, azimuth::multiply(0, 100))
}

// 测试 2: 字符串处理功能
test "string_processing_features" {
  // 测试基本字符串拼接
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  
  // 测试包含特殊字符的字符串
  azimuth::assert_eq_string("Hello, @#$%!", azimuth::greet("@#$%"))
  azimuth::assert_eq_string("Hello, 12345!", azimuth::greet("12345"))
}

// 测试 3: 边界值处理测试
test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界处理
  azimuth::assert_eq(max_val, azimuth::add(max_val, 1))
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
  
  // 测试乘法边界处理
  azimuth::assert_eq(0, azimuth::multiply(max_val, 0))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
}

// 测试 4: 数学运算性质验证
test "mathematical_properties_verification" {
  let a = 12
  let b = 8
  
  // 测试加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 测试乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 测试分配律
  azimuth::assert_eq(
    azimuth::multiply(a, azimuth::add(b, 5)),
    azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, 5))
  )
}

// 测试 5: 实际应用场景 - 购物车计算
test "shopping_cart_calculation" {
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth::add(total1, total2)
  
  // 验证总价：299*2 + 199*3 = 598 + 597 = 1195
  azimuth::assert_eq(1195, cart_total)
}

// 测试 6: 温度转换计算
test "temperature_conversion_calculation" {
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius_temp = 20
  let fahrenheit_temp = azimuth::add(azimuth::multiply(celsius_temp, 2), 30)
  azimuth::assert_eq(70, fahrenheit_temp)
  
  // 测试极端温度
  let extreme_celsius = -40
  let extreme_fahrenheit = azimuth::add(azimuth::multiply(extreme_celsius, 2), 30)
  azimuth::assert_eq(-50, extreme_fahrenheit)
}

// 测试 7: 连续运算测试
test "sequential_operations_test" {
  // 测试连续加法
  let result1 = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  azimuth::assert_eq(10, result1)
  
  // 测试连续乘法
  let result2 = azimuth::multiply(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(26, result3)
}

// 测试 8: 面积和周长计算
test "area_perimeter_calculation" {
  // 计算矩形面积和周长
  let length = 10
  let width = 6
  
  // 面积 = 长 × 宽
  let area = azimuth::multiply(length, width)
  azimuth::assert_eq(60, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  azimuth::assert_eq(32, perimeter)
}

// 测试 9: 平均值计算
test "average_calculation" {
  // 计算三个数的平均值
  let num1 = 85
  let num2 = 90
  let num3 = 95
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(num1, num2), num3)
  // 计算平均值
  let average = sum / 3
  
  azimuth::assert_eq(90, average)
}

// 测试 10: 条件逻辑测试
test "conditional_logic_test" {
  // 测试基于比较结果的条件运算
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { 
    azimuth::add(x, y) 
  } else { 
    azimuth::multiply(x, y) 
  }
  
  // 由于 15 < 20 为真，所以应该计算 15 + 20 = 35
  azimuth::assert_eq(35, result)
}