// 标准 Azimuth MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法
// 测试覆盖核心功能和各种边界情况

test "add_function_basic_cases" {
  // 加法函数基本测试用例
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界测试用例
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "subtract_function_various_scenarios" {
  // 减法函数各种场景测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "greet_function_string_operations" {
  // 问候函数字符串操作测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(26, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(32, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(4, result3)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 7
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 负值测试
  assert_eq(-5, add(0, -5))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-1, divide_with_ceil(-5, 5))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 大数运算
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  assert_eq(3334, divide_with_ceil(large_num, 3))
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景测试 - 购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  assert_eq(100, subtotal)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(10, tax)
  
  // 计算总价
  let total = add(subtotal, tax)
  assert_eq(110, total)
  
  // 计算每箱包装数量（假设每箱最多6个）
  let boxes_needed = divide_with_ceil(quantity, 6)
  assert_eq(1, boxes_needed)
}