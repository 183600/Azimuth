// 标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法编写的高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(-1, subtract(2, 3))
  assert_eq(1, divide_with_ceil(2, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "negative_number_calculations" {
  // 负数计算测试
  assert_eq(-1, add(-2, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-5, subtract(-2, 3))
  assert_eq(-1, divide_with_ceil(-2, 3))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let initial = 100
  let step1 = multiply(initial, 2)
  let step2 = add(step1, 50)
  let step3 = subtract(step2, 30)
  let result = divide_with_ceil(step3, 3)
  
  assert_eq(200, step1)
  assert_eq(250, step2)
  assert_eq(220, step3)
  assert_eq(74, result)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 单位元性质
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 25
  let quantity = 8
  let discount = 15
  let tax_rate = 10
  
  let subtotal = multiply(unit_price, quantity)
  let after_discount = subtract(subtotal, discount)
  let tax_amount = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  let total = add(after_discount, tax_amount)
  
  assert_eq(200, subtotal)
  assert_eq(185, after_discount)
  assert_eq(19, tax_amount)
  assert_eq(204, total)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(0, subtract(0, 0))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(10, multiply(1, 10))
  assert_eq(1, subtract(1, 0))
  assert_eq(10, divide_with_ceil(10, 1))
}

test "data_processing_pipeline" {
  // 数据处理流水线测试
  let data_points = 5
  let base_value = 10
  let increment = 3
  
  // 计算序列值
  let value1 = base_value
  let value2 = add(value1, increment)
  let value3 = add(value2, increment)
  let value4 = add(value3, increment)
  let value5 = add(value4, increment)
  
  // 计算总和
  let total = add(add(add(add(value1, value2), value3), value4), value5)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(total, data_points)
  
  assert_eq(10, value1)
  assert_eq(13, value2)
  assert_eq(16, value3)
  assert_eq(19, value4)
  assert_eq(22, value5)
  assert_eq(80, total)
  assert_eq(16, average)
}

test "error_handling_validation" {
  // 错误处理验证测试
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
}