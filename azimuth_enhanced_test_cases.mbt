// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，包含9个测试用例

// 导入必要的模块
use azimuth

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(3, 7))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(8, -3))
  assert_eq(7, add(7, 0))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(0, -3))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 最小值特殊处理
  assert_eq(min_val + 5, add(min_val, 5))
  assert_eq(min_val + 100, add(min_val, 100))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(7, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(-7, multiply(7, -1))
}

test "multiply_function_overflow_protection" {
  // 测试乘法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最小值与-1相乘的特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  
  // 最小值与其他值相乘
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
  assert_eq(0, multiply(min_val, 0))
  
  // 最大值溢出测试
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1: (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let sum = add(a, b)
  let result1 = multiply(sum, c)
  assert_eq(32, result1)
  
  // 场景2: a * b + c * d
  let x = 6
  let y = 7
  let z = 8
  let w = 9
  let product1 = multiply(x, y)
  let product2 = multiply(z, w)
  let result2 = add(product1, product2)
  assert_eq(114, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律: a * (b + c) = a * b + a * c
  let z = 5
  let sum = add(y, z)
  let left = multiply(x, sum)
  let right = add(multiply(x, y), multiply(x, z))
  assert_eq(left, right)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和边缘情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1: 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping_cost = 20
  let discount = 50
  
  let subtotal = multiply(item_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  assert_eq(607, final_total)
  
  // 场景2: 计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = multiply(length, width)
  let perimeter_sum = add(length, width)
  let perimeter = multiply(perimeter_sum, 2)
  assert_eq(50, area)
  assert_eq(30, perimeter)
}