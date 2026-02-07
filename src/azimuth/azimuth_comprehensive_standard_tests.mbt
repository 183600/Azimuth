// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(10, 8))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_arithmetic" {
  // 负数算术运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-13, subtract(-5, 8))
  assert_eq(13, subtract(8, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17
  let y = 23
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
  
  // 单位元性质
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值边界
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 单位值边界
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 100), 100)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(100, 1), 100)
  
  // 负单位边界
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 100), -100)
  assert_eq(subtract(0, -1), 1)
  assert_eq(divide_with_ceil(-100, 1), -100)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 100)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 101)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 1)
  
  // a + b * c
  let result4 = add(a, multiply(b, c))
  assert_eq(result4, 52)
}

test "precision_and_rounding" {
  // 精度和舍入测试
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(2, divide_with_ceil(100, 99))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(34, divide_with_ceil(101, 3))
  assert_eq(34, divide_with_ceil(102, 3))
}

test "large_number_operations" {
  // 大数运算测试
  let large_num = 100000
  let larger_num = 1000000
  
  assert_eq(200000, add(large_num, large_num))
  assert_eq(1100000, add(large_num, larger_num))
  assert_eq(20000000000, multiply(large_num, large_num))
  assert_eq(100000000000, multiply(large_num, larger_num))
  assert_eq(900000, subtract(large_num, 100000))
  assert_eq(33334, divide_with_ceil(large_num, 3))
  assert_eq(333334, divide_with_ceil(larger_num, 3))
}

test "real_world_application_calculations" {
  // 真实世界应用计算测试
  // 购物车计算场景
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  let tax_rate = 10
  
  // 计算小计
  let item1_subtotal = multiply(item1_price, item1_quantity)
  let item2_subtotal = multiply(item2_price, item2_quantity)
  let items_total = add(item1_subtotal, item2_subtotal)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(items_total, tax_rate), 100)
  let grand_total = add(items_total, tax_amount)
  
  assert_eq(75, item1_subtotal)
  assert_eq(30, item2_subtotal)
  assert_eq(105, items_total)
  assert_eq(11, tax_amount)
  assert_eq(116, grand_total)
}

test "edge_case_error_handling" {
  // 边界情况错误处理测试
  // 测试各种边界和错误情况
  
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 极值测试
  let max_val = 1000000
  let min_val = -1000000
  
  assert_eq(2000000, add(max_val, max_val))
  assert_eq(-2000000, add(min_val, min_val))
  assert_eq(0, add(max_val, min_val))
  
  assert_eq(1000000000000, multiply(max_val, max_val))
  assert_eq(1000000000000, multiply(min_val, min_val))
  assert_eq(-1000000000000, multiply(max_val, min_val))
  
  // 精度边界
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(-1, 1000000))
  assert_eq(-1, divide_with_ceil(1, -1000000))
}