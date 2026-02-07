// 标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 导入必要的模块
@testable

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-3, 2), -1)
  assert_eq(add(50, 50), 100)
  assert_eq(add(-50, -50), -100)
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(multiply(2, 3), 6)
  assert_eq(multiply(5, 0), 0)
  assert_eq(multiply(-3, 5), -15)
  assert_eq(multiply(-3, -5), 15)
  assert_eq(multiply(100, 100), 10000)
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 5), 1)
  assert_eq(divide_with_ceil(0, 10), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(-1, 2), -1)
  assert_eq(divide_with_ceil(-1, -2), 1)
}

test "greet_function_basic" {
  // 问候函数基本测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(15, 15), 0)
  assert_eq(subtract(10, -10), 20)
  assert_eq(subtract(-10, 10), -20)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "edge_cases_boundary_values" {
  // 边界值测试
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 负一测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(divide_with_ceil(-5, 1), -5)
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let x = 10
  let y = 3
  let z = 2
  
  // (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(result1, 26)
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(result2, 32)
  
  // (x - y) / z (向上取整)
  let result3 = divide_with_ceil(subtract(x, y), z)
  assert_eq(result3, 4)
}

test "real_world_scenario" {
  // 真实世界场景测试：购物车计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let item1_subtotal = multiply(item1_price, item1_quantity)
  let item2_subtotal = multiply(item2_price, item2_quantity)
  
  // 计算总价
  let total = add(item1_subtotal, item2_subtotal)
  
  // 计算税费（10%向上取整）
  let tax = divide_with_ceil(multiply(total, 10), 100)
  
  // 计算最终总价
  let grand_total = add(total, tax)
  
  assert_eq(item1_subtotal, 75)
  assert_eq(item2_subtotal, 30)
  assert_eq(total, 105)
  assert_eq(tax, 11)
  assert_eq(grand_total, 116)
}