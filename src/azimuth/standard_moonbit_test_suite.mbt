// 标准 MoonBit 测试套件
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_cases" {
  // 加法函数基本测试用例
  assert_eq(add(1, 1), 2)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(100, 200), 300)
  assert_eq(add(-50, -30), -80)
}

test "multiply_function_properties" {
  // 乘法函数性质测试
  assert_eq(multiply(1, 5), 5)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(-1, 10), -10)
  assert_eq(multiply(-2, -3), 6)
  assert_eq(multiply(10, 10), 100)
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(15, 5), 3)
}

test "subtract_function_edge_cases" {
  // 减法函数边界情况测试
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(-5, -10), 5)
  assert_eq(subtract(100, 0), 100)
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  assert_eq(multiply(add(a, b), c), 16)
  
  // a * b + c
  assert_eq(add(multiply(a, b), c), 17)
  
  // a - b / c (向上取整)
  assert_eq(subtract(a, divide_with_ceil(b, c)), 3)
}

test "mathematical_commutative_laws" {
  // 数学交换律验证测试
  let x = 7
  let y = 11
  
  // 加法交换律: x + y = y + x
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律: x * y = y * x
  assert_eq(multiply(x, y), multiply(y, x))
}

test "boundary_value_analysis_comprehensive" {
  // 边界值分析综合测试
  // 零值测试
  assert_eq(add(0, 5), 5)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 5), -5)
  assert_eq(divide_with_ceil(0, 5), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  assert_eq(add(-3, 7), 4)
  assert_eq(add(3, -7), -4)
  assert_eq(add(-3, -7), -10)
  
  assert_eq(multiply(-3, 7), -21)
  assert_eq(multiply(3, -7), -21)
  assert_eq(multiply(-3, -7), 21)
  
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景测试
  // 购物车计算：3个商品，价格分别为25、15、10元
  let item1_price = 25
  let item2_price = 15
  let item3_price = 10
  
  // 计算总价
  let subtotal = add(add(item1_price, item2_price), item3_price)
  
  // 计算税费（10%，向上取整）
  let tax = divide_with_ceil(multiply(subtotal, 10), 100)
  
  // 计算最终总价
  let total = add(subtotal, tax)
  
  assert_eq(subtotal, 50)
  assert_eq(tax, 5)
  assert_eq(total, 55)
}