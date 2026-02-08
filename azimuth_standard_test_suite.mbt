// 标准 MoonBit 测试用例
// 为 Azimuth 项目创建的高质量测试套件
// 包含 10 个测试用例，覆盖核心功能

test "azimuth_basic_arithmetic_operations" {
  // 测试 Azimuth 库的基本算术运算
  azimuth::assert_eq(8, azimuth::add(3, 5))
  azimuth::assert_eq(15, azimuth::multiply(3, 5))
  azimuth::assert_eq(-2, azimuth::subtract(3, 5))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(5, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
}

test "azimuth_division_with_ceil_precision" {
  // 测试向上取整除法的精度
  // 正数除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(4, 2))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100))
  
  // 边界情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 10))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 负数除法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(-10, -3))
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(10, -3))
}

test "azimuth_string_functionality" {
  // 测试字符串功能
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "azimuth_negative_number_operations" {
  // 测试负数运算
  azimuth::assert_eq(-2, azimuth::add(-5, 3))
  azimuth::assert_eq(2, azimuth::add(5, -3))
  azimuth::assert_eq(-8, azimuth::add(-5, -3))
  
  azimuth::assert_eq(-15, azimuth::multiply(-3, 5))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  
  azimuth::assert_eq(-8, azimuth::subtract(-3, 5))
  azimuth::assert_eq(8, azimuth::subtract(3, -5))
  azimuth::assert_eq(2, azimuth::subtract(-3, -5))
}

test "azimuth_mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 7
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 加法单位元
  azimuth::assert_eq(azimuth::add(x, 0), x)
  
  // 乘法单位元
  azimuth::assert_eq(azimuth::multiply(x, 1), x)
  
  // 减法逆运算
  azimuth::assert_eq(azimuth::add(azimuth::subtract(x, y), y), x)
}

test "azimuth_complex_calculation_scenario" {
  // 测试复杂计算场景
  // 模拟购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  // 计算小计
  let subtotal1 = azimuth::multiply(item1_price, item1_qty)
  let subtotal2 = azimuth::multiply(item2_price, item2_qty)
  let total = azimuth::add(subtotal1, subtotal2)
  
  // 计算税费（10%，向上取整）
  let tax = azimuth::divide_with_ceil(azimuth::multiply(total, 10), 100)
  let final_total = azimuth::add(total, tax)
  
  azimuth::assert_eq(75, subtotal1)
  azimuth::assert_eq(30, subtotal2)
  azimuth::assert_eq(105, total)
  azimuth::assert_eq(11, tax)
  azimuth::assert_eq(116, final_total)
}

test "azimuth_boundary_value_analysis" {
  // 测试边界值分析
  // 零值测试
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 100))
  azimuth::assert_eq(0, azimuth::subtract(0, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 10))
  
  // 单位值测试
  azimuth::assert_eq(1, azimuth::add(1, 0))
  azimuth::assert_eq(100, azimuth::multiply(1, 100))
  azimuth::assert_eq(1, azimuth::subtract(1, 0))
  azimuth::assert_eq(100, azimuth::divide_with_ceil(100, 1))
  
  // 负单位值测试
  azimuth::assert_eq(0, azimuth::add(-1, 1))
  azimuth::assert_eq(-100, azimuth::multiply(-1, 100))
  azimuth::assert_eq(1, azimuth::subtract(0, -1))
}

test "azimuth_large_number_operations" {
  // 测试大数运算
  let large_num = 100000
  
  azimuth::assert_eq(200000, azimuth::add(large_num, large_num))
  azimuth::assert_eq(10000000000, azimuth::multiply(large_num, large_num))
  azimuth::assert_eq(0, azimuth::subtract(large_num, large_num))
  azimuth::assert_eq(33334, azimuth::divide_with_ceil(large_num, 3))
  
  // 测试大数与负数的运算
  azimuth::assert_eq(99999, azimuth::add(large_num, -1))
  azimuth::assert_eq(-100000, azimuth::multiply(large_num, -1))
  azimuth::assert_eq(100001, azimuth::subtract(large_num, -1))
}

test "azimuth_fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算
  // 使用基本运算模拟递归计算
  
  // F(0) = 0, F(1) = 1
  let f0 = 0
  let f1 = 1
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = azimuth::add(f1, f0)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = azimuth::add(f2, f1)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = azimuth::add(f3, f2)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = azimuth::add(f4, f3)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = azimuth::add(f5, f4)
  
  azimuth::assert_eq(1, f2)
  azimuth::assert_eq(2, f3)
  azimuth::assert_eq(3, f4)
  azimuth::assert_eq(5, f5)
  azimuth::assert_eq(8, f6)
}

test "azimuth_temperature_conversion" {
  // 测试温度转换计算
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = azimuth::multiply(celsius, 9)
  let fahrenheit_step2 = azimuth::divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = azimuth::add(fahrenheit_step2, 32)
  
  // 华氏度转开尔文：K = (F - 32) × 5/9 + 273.15
  // 简化计算，使用整数近似
  let kelvin_step1 = azimuth::subtract(fahrenheit, 32)
  let kelvin_step2 = azimuth::multiply(kelvin_step1, 5)
  let kelvin_step3 = azimuth::divide_with_ceil(kelvin_step2, 9)
  let kelvin = azimuth::add(kelvin_step3, 273)
  
  azimuth::assert_eq(77, fahrenheit)
  azimuth::assert_eq(298, kelvin)
  
  // 验证转换的一致性（简化验证）
  let celsius_back = azimuth::divide_with_ceil(azimuth::multiply(azimuth::subtract(kelvin, 273), 9), 5)
  azimuth::assert_eq(25, celsius_back)
}