// 新增的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含10个测试用例

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(-5, 0))
}

test "greet_with_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let x = 10
  let y = 20
  let z = 30
  
  // 计算 (x + y) * z - x * y
  let result = add(multiply(add(x, y), z), multiply(multiply(x, y), -1))
  assert_eq(700, result)
}

test "boundary_value_operations" {
  // 测试边界值操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 5)), add(multiply(a, b), multiply(a, 5)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = multiply(2, add(length, width))
  assert_eq(50, perimeter)
  
  // 计算折扣价格：原价 - 折扣
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, multiply(discount, -1))
  assert_eq(160, final_price)
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
}

test "conditional_calculations" {
  // 测试条件计算
  let temperature = 25
  let is_hot = temperature > 20
  
  // 根据温度决定活动
  let activity = if is_hot { 
    "游泳" 
  } else { 
    "跑步" 
  }
  
  // 测试条件逻辑
  assert_true(is_hot)
  assert_eq_string("Hello, 游泳!", greet(activity))
  
  // 测试相反情况
  let cold_temperature = 15
  let is_cold = cold_temperature > 20
  let cold_activity = if is_cold { 
    "游泳" 
  } else { 
    "跑步" 
  }
  
  assert_false(is_cold)
  assert_eq_string("Hello, 跑步!", greet(cold_activity))
}