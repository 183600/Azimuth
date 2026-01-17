// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，包含9个测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.add(8, -3))
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val + 5, @azimuth.add(min_val, 5))
  @azimuth.assert_eq(min_val + 100, @azimuth.add(min_val, 100))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值与-1相乘的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  
  // 最小值与其他值相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 最大值溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1: (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(32, result1)
  
  // 场景2: a * b + c * d
  let x = 6
  let y = 7
  let z = 8
  let w = 9
  let product1 = @azimuth.multiply(x, y)
  let product2 = @azimuth.multiply(z, w)
  let result2 = @azimuth.add(product1, product2)
  @azimuth.assert_eq(114, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律: a * (b + c) = a * b + a * c
  let z = 5
  let sum = @azimuth.add(y, z)
  let left = @azimuth.multiply(x, sum)
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和边缘情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1: 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping_cost = 20
  let discount = 50
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, shipping_cost)
  let final_total = @azimuth.add(total_before_discount, -discount)
  @azimuth.assert_eq(607, final_total)
  
  // 场景2: 计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter_sum = @azimuth.add(length, width)
  let perimeter = @azimuth.multiply(perimeter_sum, 2)
  @azimuth.assert_eq(50, area)
  @azimuth.assert_eq(30, perimeter)
}