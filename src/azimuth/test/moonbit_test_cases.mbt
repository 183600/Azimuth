// MoonBit 标准测试用例
// 使用标准的 MoonBit 测试语法编写

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, @azimuth.add(3, 7))
  assert_eq(0, @azimuth.add(-5, 5))
  assert_eq(15, @azimuth.multiply(3, 5))
  assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(min_val, @azimuth.add(min_val, -1))
  assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, @azimuth.multiply(max_val, 0))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  
  // 冰点温度
  let celsius = 0
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  assert_eq(30, fahrenheit)
  
  // 室温
  let room_celsius = 22
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  assert_eq(74, room_fahrenheit)
  
  // 沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "financial_calculation_simple_interest" {
  // 测试简单利息计算
  let principal = 10000  // 本金
  let rate = 5           // 利率百分比
  let time = 3           // 时间（年）
  
  // 简单利息公式：利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  assert_eq(150000, interest)
  
  // 总金额 = 本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  assert_eq(160000, total_amount)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  
  // 矩形面积
  let length = 8
  let width = 6
  let rectangle_area = @azimuth.multiply(length, width)
  assert_eq(48, rectangle_area)
  
  // 三角形面积（底 × 高 ÷ 2）
  let base = 10
  let height = 8
  let triangle_area = @azimuth.multiply(base, height) / 2
  assert_eq(40, triangle_area)
  
  // 梯形面积（（上底 + 下底）× 高 ÷ 2）
  let top_base = 6
  let bottom_base = 10
  let trapezoid_height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(40, trapezoid_area)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  assert_eq(35, mean)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(55, -15)
  assert_eq(40, range)
}

test "complex_number_expressions" {
  // 测试复杂数字表达式
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)          // 15
  let diff_cd = @azimuth.add(c, -d)        // 12
  let product1 = @azimuth.multiply(sum_ab, diff_cd)  // 180
  let product2 = @azimuth.multiply(e, f)   // 21
  let result = @azimuth.add(product1, product2)     // 201
  
  assert_eq(201, result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_true(@azimuth.add(max_val, 100) == max_val)
  assert_true(@azimuth.multiply(max_val, 2) == max_val)
  assert_true(@azimuth.add(min_val, -100) == min_val)
  assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}