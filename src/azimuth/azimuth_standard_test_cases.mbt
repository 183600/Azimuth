// Azimuth 项目标准测试用例
// 包含10个高质量的 MoonBit 测试用例

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(100, 200), 300)
  assert_eq(add(-50, -30), -80)
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的综合情况
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(-3, -4), 12)
  assert_eq(multiply(10, 10), 100)
}

test "subtract_function_edge_cases" {
  // 测试 subtract 函数的边界情况
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(-5, -10), 5)
  assert_eq(subtract(100, 100), 0)
}

test "divide_with_ceil_function_various_cases" {
  // 测试 divide_with_ceil 函数的各种情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_different_inputs" {
  // 测试 greet 函数的不同输入
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 30)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 52)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 3)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负一
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "practical_calculation_scenario" {
  // 实际计算场景测试
  // 模拟购物车计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算各商品小计
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  
  // 计算总计
  let subtotal = add(item1_total, item2_total)
  
  // 计算税费（10%，向上取整）
  let tax = divide_with_ceil(multiply(subtotal, 10), 100)
  
  // 计算总价
  let total = add(subtotal, tax)
  
  assert_eq(item1_total, 75)
  assert_eq(item2_total, 30)
  assert_eq(subtotal, 105)
  assert_eq(tax, 11)
  assert_eq(total, 116)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 测试除零处理
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 0), 0)
  
  // 测试大数运算
  let large_num = 100000
  assert_eq(add(large_num, large_num), 200000)
  assert_eq(multiply(large_num, 2), 200000)
  assert_eq(subtract(large_num, 50000), 50000)
  assert_eq(divide_with_ceil(large_num, 3), 33334)
}