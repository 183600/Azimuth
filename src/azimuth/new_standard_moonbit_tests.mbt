// 标准 MoonBit 测试用例集合
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(add(1, 1), 2)
  assert_eq(add(0, 5), 5)
  assert_eq(add(-3, 7), 4)
  assert_eq(add(100, 200), 300)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(2, 3), 6)
  assert_eq(multiply(0, 10), 0)
  assert_eq(multiply(-4, 5), -20)
  assert_eq(multiply(7, 8), 56)
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 5), 0)
  assert_eq(subtract(2, 8), -6)
  assert_eq(subtract(100, 50), 50)
}

test "division_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "greet_function_basic_cases" {
  // 测试问候函数的基本情况
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet(""), "Hello, !")
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  assert_eq(multiply(add(a, b), c), 16)
  
  // a * b + c
  assert_eq(add(multiply(a, b), c), 17)
  
  // (a - b) / c (向上取整)
  assert_eq(divide_with_ceil(subtract(a, b), c), 1)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 1), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  
  // 除零边界情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(4, -6), -24)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, 7), -17)
  assert_eq(subtract(10, -7), 17)
}

test "mathematical_properties_verification" {
  // 验证数学性质
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
  
  // 减法的逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "practical_calculation_scenarios" {
  // 实际计算场景测试
  // 计算购物总价：商品价格 × 数量 + 税费
  let item_price = 25
  let quantity = 4
  let tax = 15
  let total_cost = add(multiply(item_price, quantity), tax)
  assert_eq(total_cost, 115)
  
  // 计算平均分（向上取整）
  let scores_sum = 187
  let num_students = 8
  let average_score = divide_with_ceil(scores_sum, num_students)
  assert_eq(average_score, 24)
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 100
  let discount = 20
  let discounted_price = subtract(original_price, discount)
  assert_eq(discounted_price, 80)
}