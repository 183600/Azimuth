// 额外的标准 MoonBit 测试用例
// 这些测试用例使用标准的 MoonBit 测试语法，覆盖核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(10, add(-5, 15))
}

test "add_function_boundary_values" {
  // 测试 add 函数的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_function_special_cases" {
  // 测试 multiply 函数的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的特殊处理
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_application_test" {
  // 实际应用测试：计算商品总价
  let unit_price = 199
  let quantity = 3
  let discount = 50
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 应用折扣
  let discounted_total = add(subtotal, -discount)
  // 计算税费
  let tax = multiply(discounted_total, tax_rate) / 100
  // 计算最终总价
  let final_total = add(discounted_total, tax)
  
  assert_eq(624, final_total)
}

test "edge_cases_and_error_handling" {
  // 边界情况和错误处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保边界值运算不会导致未定义行为
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
}