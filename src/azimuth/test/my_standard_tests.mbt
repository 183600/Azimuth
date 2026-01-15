// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的综合测试

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum)
  
  // 测试更复杂的等差数列：10 + 20 + 30 + 40 = 100
  let complex_sum = add(add(add(10, 20), 30), 40)
  assert_eq(100, complex_sum)
}

test "geometric_progression" {
  // 测试等比数列：2 × 2 × 2 × 2 = 16
  let product = multiply(multiply(multiply(2, 2), 2), 2)
  assert_eq(16, product)
  
  // 测试混合符号的等比数列：-2 × 2 × -2 × 2 = 16
  let mixed_product = multiply(multiply(multiply(-2, 2), -2), 2)
  assert_eq(16, mixed_product)
}

test "string_processing_variations" {
  // 测试不同类型的字符串处理
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}

test "boundary_value_combinations" {
  // 测试边界值的组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试边界值附近的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质：交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "real_world_calculations" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  // 验证总价：199×2 + 299×1 + 20 = 398 + 299 + 20 = 717
  assert_eq(717, final_total)
}

test "error_handling_validation" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
}

test "algorithmic_complexity" {
  // 测试算法复杂度计算：阶乘和幂运算
  // 计算 5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial)
  
  // 计算 2^6 = 2 × 2 × 2 × 2 × 2 × 2 = 64
  let power = multiply(multiply(multiply(multiply(multiply(2, 2), 2), 2), 2), 2)
  assert_eq(64, power)
}

test "data_structure_simulation" {
  // 模拟数据结构操作：数组索引计算
  let row = 3
  let col = 4
  let width = 10
  
  // 计算二维数组在一维数组中的索引：index = row × width + col
  let index = add(multiply(row, width), col)
  assert_eq(34, index)
  
  // 测试边界索引
  let first_index = add(multiply(0, width), 0)
  let last_index = add(multiply(9, width), 9)
  assert_eq(0, first_index)
  assert_eq(99, last_index)
}

test "comprehensive_integration" {
  // 综合集成测试：多个函数的组合使用
  let base_value = 100
  let multiplier = 3
  let increment = 50
  let name = "MoonBit"
  
  // 计算最终值：(base_value × multiplier) + increment
  let calculated_value = add(multiply(base_value, multiplier), increment)
  assert_eq(350, calculated_value)
  
  // 生成最终消息
  let final_message = greet(name)
  assert_eq_string("Hello, MoonBit!", final_message)
  
  // 验证计算结果的属性
  assert_true(calculated_value > base_value)
  assert_true(calculated_value % 10 == 0)
}